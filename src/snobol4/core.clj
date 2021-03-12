(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z])
  (:require [clojure.edn :as edn])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (:require [instaparse.core :as insta :refer [defparser]]); Clojure
  (:require [clojure.tools.trace :refer :all])
; (:require [instaparse.core :as insta :refer-macros [defparser]]); ClojureScript
)
;---------------------------------------------------------------------------------------------------
(defn bug [x] (println (type x) " " x) x)
(defn re-quote [& ss]
  (str "#'" (string/replace (apply str ss) #"(\\|')" #(str \\ (second %1))) "'"))
(def label #"([^ \t\r\n+-]|\+)+")
(def white  #"([ \t]+|\n\+|\n\.)")
(def grammar
  (str "
  stmt      ::=  label? (<_> body? <__> goto?)? <__> <eos?>
  body      ::=  invoking | matching | replacing | assigning
  invoking  ::=  subject
  matching  ::=  subject <_> pattern
  replacing ::=  subject <_> pattern <_ '='> replace
  assigning ::=  subject <_ '='> replace
 <subject>  ::=  uop
 <pattern>  ::=  (<'?' _>)? and
 <replace>  ::=  (<_> expr)?
  goto      ::=  <__ ':' __>
                 ( branch
                 | sbranch (<__> fbranch)?
                 | fbranch (<__> sbranch)?
                 )
  branch    ::=  target
  sbranch   ::=  <'S'> target
  fbranch   ::=  <'F'> target
 <target>   ::=  <'('> expr <')'>
  comment   ::=  <'*'> #'.*' <eol>
  control   ::=  <'-'> #'[^;\\n]*' <eos>
  eos       ::=  '\\n' | ';'
  eol       ::=  '\\n'

 <expr>     ::=  <__> asn <__>
  asn       ::=  mch | mch  <_  '='  _>  asn
  mch       ::=  and | and  <_  '?'  _>  and (<_ '=' _> and)?
  and       ::=  alt | and  <_  '&'  _>  alt
  alt       ::=  cat | cat (<_  '|'  _>  cat)+
  cat       ::=  at  | at  (<_>          at)+
  at        ::=  sum | at   <_  '@'  _>  sum
  sum       ::=  hsh | sum  <_> '+' <_>  hsh
                     | sum  <_> '-' <_>  hsh
  hsh       ::=  div | hsh  <_  '#'  _>  div
  div       ::=  mul | div  <_  '/'  _>  mul
  mul       ::=  pct | mul  <_  '*'  _>  pct
  pct       ::=  xp  | pct  <_  '%'  _>  xp
  xp        ::=  cap | cap  <_> '^' <_>  xp
                     | cap  <_> '!' <_>  xp
                     | cap  <_> '**' <_> xp
  cap       ::=  ttl | ttl  <_> '$' <_>  cap
                     | ttl  <_> '.' <_>  cap
  ttl       ::=  uop | ttl  <_  '~'  _>  uop
  uop       ::=  ndx | '@' uop | '~' uop | '?' uop | '&' uop | '+' uop
                     | '-' uop | '*' uop | '$' uop | '.' uop | '!' uop
                     | '%' uop | '/' uop | '#' uop | '=' uop | '|' uop
  ndx       ::=  itm | ndx <'<'> lst <'>'> | ndx <'['> lst <']'>
  <itm>     ::=  I | R | S | N | grp | cnd | inv
  <grp>     ::=  <'('> expr <')'>
  cnd       ::=  <'('> expr <','> lst <')'>
  inv       ::=  N <'()'>  |  N <'('> lst <')'>
  <lst>     ::=  expr? | expr (<','> expr?)+
  <_>       ::=  <white+>
  <__>      ::=  <white*>
  I         ::=  #'[0-9]+'
  R         ::=  #'[0-9]+\\.[0-9]+'
  S         ::=  #'\"([^\"]|\\x3B)*\"'
              |  #'\\'([^\\']|\\x3B)*\\''
  N         ::=  #'[A-Za-z][A-Z_a-z0-9\\.\\-]*'
  label     ::=  #'[^ \\t\\r\\n+-.*][^ \\t\\r\\n]*'
  white     ::=  #'[ \\t]'
"))
;---------------------------------------------------------------------------------------------------
(defn coder [ast stmtno]
  (insta/transform
    { :comment   (fn comment     [cmt]   [:comment cmt])
      :control   (fn control     [ctl]   [:control ctl])
      ;--------------------------------------------------------------------------
      :stmt      (fn stmt [& ss]
                   (let [{L :label B :body G :goto} (apply conj ss)]
                     (apply vector ss)
                     {(if L L stmtno) [B G]}
                 ))
      :label     (fn label       [L]     {:label (if (re-find #"^[0-9A-Z_a-z]+$" L) (keyword L) (str L))})
      :body      (fn body        [B]     {:body  B})
      :invoking  (fn invoking    [S    ] S)
      :matching  (fn matching    [S P  ] (list '? S P))
      :replacing (fn replacing  ([S P  ] (list '?= S P 'epsilon))
                                ([S P R] (list '?= S P R)))
      :assigning (fn assigning  ([S    ] (list '= S 'epsilon))
                                ([S R  ] (list '= S R)))
      :goto      (fn goto        [& gs]  {:goto (reduce
																																											(fn [bs b]
																																												(let [key (first b) tgt (second b)]
																																													(assoc bs key
																																														(if (symbol? tgt) (keyword tgt) tgt)))) {} gs)
                                         })
      :branch    (fn branch      [L]     [:G L])
      :sbranch   (fn sbranch     [L]     [:S L])
      :fbranch   (fn fbranch     [L]     [:F L])
      ;--------------------------------------------------------------------------
      :expr      (fn expr        [x] x)
      :asn       (fn asn        ([x] x) ([x     y]  (list '= x y)))
      :mch       (fn mch        ([x] x) ([x  y   ]  (list '? x y))
                                        ([x  y  z]  (list '?= x y z)))
      :and       (fn and        ([x] x) ([x     y]  (list '& x y)))
      :alt       (fn alt        ([x] x) ([x  & ys]  (apply list '| x ys)))
      :cat       (fn cat        ([x] x) ([x  & ys]  (apply vector x ys)))
      :at        (fn at         ([x] x) ([x     y]  (list 'at x y)))
      :sum       (fn sum        ([x] x) ([x  op y]  (list (symbol op) x y)))
      :hsh       (fn hsh        ([x] x) ([x     y]  (list 'hash x y)))
      :div       (fn div        ([x] x) ([x     y]  (list '/ x y)))
      :mul       (fn mul        ([x] x) ([x     y]  (list '* x y)))
      :pct       (fn pct        ([x] x) ([x     y]  (list '% x y)))
      :xp        (fn xp         ([x] x) ([x  op y]  (list (symbol op) x y)))
      :cap       (fn cap        ([x] x) ([x  op y]  (list (symbol op) x y)))
      :ttl       (fn ttl        ([x] x) ([x     y]  (list 'tilde x y)))
      :uop       (fn uop        ([x] x) ([   op y]  (case op
                                                      "@" (list 'at y)
                                                      "#" (list 'hash y)
                                                      "~" (list 'tilde y)
                                                          (list (symbol op) y))))
      :ndx       (fn ndx        ([n] n) ([n  & xs]  (apply list n xs)))
      :cnd       (fn cnd        ([x] x) ([x  & ys]  (apply vector 'comma x ys)))
      :inv       (fn inv         [f  & xs]          (apply list f xs))
      :N         (fn N           [n]                (symbol n))
      :S         (fn S           [s]                (subs s 1 (- (count s) 1)))
      :I         edn/read-string
      :R         edn/read-string
    } ast))
;---------------------------------------------------------------------------------------------------
(def parse-program    )
(def parse-command    (insta/parser grammar :start :command))
(def parse-statement  (insta/parser grammar :start :stmt :total true))
(def parse-expression (insta/parser grammar :start :expr))

(defn files [directories]
  (reduce (fn [files directory]
    (reduce (fn [files file]
      (let [filenm (str file)]
        (if (re-find #"^.+\.(sno|spt|inc|SNO|SPT|INC)$" filenm)
          (conj files filenm) files)))
      files
      (file-seq (io/file directory))))
    []
    directories))

(def dirs ["./src/sno" "./src/inc" "./src/test ./src/rinky"])
(def out
  (fn [item]
      (binding [pp/*print-right-margin* 120, pp/*print-miser-width* 100]
        (pp/pprint item))))
;---------------------------------------------------------------------------------------------------
(def stmtno (atom 0))
(defn compile-stmt [cmd]
   (let [stmt1 (string/replace cmd #"[ \t]*\r?\n[+.][ \t]*" " ")
		       stmt2 (string/replace stmt1 #"\r?\n$" "")
		       stmtno (swap! stmtno inc)
		       ast (parse-statement stmt2)
		       code (coder ast stmtno)]
		   (if (and (map? code) (:reason code))
		     (let [line   (:line code)
		           column (:column code)
		           text   (:text code)
		           error  {stmtno [(list 'ERROR line column)]}]; text
		       (out error))
		     (out code))))
;---------------------------------------------------------------------------------------------------
(def SNO [
  "START"
  " BD = ('BE' | 'B') ('AR' | 'A') ('DS' | 'D')"
  " 'BEARDS' ? BD"
  " 'BEARD'  ? BD"
  " 'BEADS'  ? BD"
  " 'BEAD'   ? BD"
  " 'BARDS'  ? BD"
  " 'BARD'   ? BD"
  " 'BADS'   ? BD"
  " 'BAD'    ? BD"
  " 'BATS'   ? BD"
  " BR  = ('B' | 'R') ('E' | 'EA') ('D' | 'DS')"
  " 'BED'   ? BR"
  " 'BEDS'  ? BR"
  " 'BEAD'  ? BR"
  " 'BEADS' ? BR"
  " 'RED'   ? BR"
  " 'REDS'  ? BR"
  " 'READ'  ? BR"
  " 'READS' ? BR"
  "END"
])
;---------------------------------------------------------------------------------------------------
(defn re-cat [& regexs] (re-pattern (apply str regexs)))
(def  eol     #"[\n]")
(def  eos     #"[;\n]")
(def  skip    #"[^\n]*")
(def  fill    #"[^;\n]*")
(def  komment (re-cat #"[*]" skip eol))
(def  control (re-cat #"[-]" fill eos))
(def  kode    (re-cat #"[^;\n.+*-]" fill "(" #"\n[.+]" fill ")*" eos))
(def  block   (re-cat komment "|" control "|" kode "|" eol))
(defn doit []
  (case 1
    1 (doseq [s SNO] (compile-stmt s))
    2 (doseq [filenm (files dirs)]
        (let [program (slurp filenm)]
          (println ";------------------------------------------------------ " filenm)
          (doseq [command (re-seq block program)]
            (let [cmd (first command)]
                (cond
                  (nil? cmd) nil
                  (re-find #"^\*" cmd) nil
                  (re-find #"^\-" cmd) nil
                  true (compile-stmt cmd))))))
    3 (doseq [filenm (files dirs)]
        (with-open [rdr (io/reader filenm)]
          (doseq [line (line-seq rdr)]
            (let [ast (parse-command line) code (coder ast)]
              (println code)))))
  ))
;---------------------------------------------------------------------------------------------------
(defn ZIP [E]
  (loop [E (z/zipper #(or (list? %) (vector? %)) rest nil E) depth 0 direction :down]
    (out (z/node E))
    (case direction
    :down  (if (z/branch? E)
                    (recur (z/down E) (inc depth) :down)
                    (if (= E (z/rightmost E))
                          (if (= depth 0)
                        (z/root E)
                        (recur (z/up E) (dec depth) :right))
                          (recur (z/right E) depth :down)))
     :right (if (= E (z/rightmost E))
                      (if (= depth 0)
                        (z/root E)
                        (recur (z/up E) (dec depth) :right))
                      (recur (z/right E) depth :down)))))
;---------------------------------------------------------------------------------------------------
;(defn assign [n new] (alter-var-root n (fn [old] new)))
;(def x 10)
;(println x)
;(assign #'x 20)
;(println x)
;(def ^:dynamic x 10)
;(defn tryit [] x)
;(println x)
;(println (binding [x 20] x))
;(println (binding [x 20] (tryit)))
;(println x)
;---------------------------------------------------------------------------------------------------
;(import '[clojure.lang Var])
;(Var/create 42); root binding
;(let [V (.setDynamic (Var/create 0))]
;  (do (var-get V); 0
;      (with-bindings {V 42} (var-get V)))); 42
;(the-ns 'snobol4.core)
;(ns-map 'snobol4.core)
;(ns-aliases 'snobol4.string)
;(ns-publics 'snobol4.core)
;(set! symbol expr)
;---------------------------------------------------------------------------------------------------
(defn reference [N]
  (if-let [ns-name (namespace N)]
    (when-let [ns-ref
								      (or (get (ns-aliases *ns*) (symbol ns-name))
								          (find-ns (symbol ns-name)))]
      (get (ns-publics ns-ref) (symbol (name N))))
    (get (ns-map *ns*) (symbol (name N)))))
(defn $$ [N] (if-let [V (reference N)] (var-get V) "")); (var-get (eval (list 'var N)))
;---------------------------------------------------------------------------------------------------
(declare RUN)
(declare dq path part)
(declare Roman n)
(defn dq [_path] (binding [dq "" path _path part ""] (RUN :dq) dq));DEFINE('dq(path)part')
(defn Roman-binding [_n] (binding [Roman "" n _n] (RUN :Roman) Roman))
(defn Roman-save-restore [_n]
 '(let [_Roman ($$ 'Roman) __n ($$ 'n)]
    (def Roman "")
    (def n _n)
    (RUN :Roman)
    (def n __n)
    (let [__Roman Roman]
      (def Roman _Roman)
      __Roman)))

;---------------------------------------------------------------------------------------------------
(def LABELS-Roman {3 :Roman 6 :RomanEnd 8 :END})
(def STMTNOS-Roman {:Roman 3 :RomanEnd 6 :END 8})
(def CODE-Roman {
1         ['(DEFINE "Roman(n)units")]
2         ['(= romanXlat "0,1I,2II,3III,4IV,5V,6VI,7VII,8VIII,9IX,") {:G :RomanEnd}]
:Roman    ['(?= n [(RPOS 1) (. (LEN 1) units)]) {:F :RETURN}]
4         ['(? romanXlat [units (. (BREAK ",") units)]) {:F :FRETURN}]
5         ['(= Roman [(REPLACE (Roman n) "IVXLCDM" "XLCDM**") units]) {:S :RETURN, :F :FRETURN}]
:RomanEnd []
7         ['(Roman "MMXXI")]
:END      []
})
;---------------------------------------------------------------------------------------------------
(def LABELS {1 :START 21 :END})
(def STMTNOS {:START 1 :END 21})
(def CODE {
:START    []
2         ['(= BD [(| "BE" "B") (| "AR" "A") (| "DS" "D")])]
3         ['(? "BEARDS" BD)]
4         ['(? "BEARD" BD)]
5         ['(? "BEADS" BD)]
6         ['(? "BEAD" BD)]
7         ['(? "BARDS" BD)]
8         ['(? "BARD" BD)]
9         ['(? "BADS" BD)]
10        ['(? "BAD" BD)]
11        ['(? "BATS" BD)]
12        ['(= BR [(| "B" "R") (| "E" "EA") (| "D" "DS")])]
13        ['(? "BED" BR)]
14        ['(? "BEDS" BR)]
15        ['(? "BEAD" BR)]
16        ['(? "BEADS" BR)]
17        ['(? "RED" BR)]
18        ['(? "REDS" BR)]
19        ['(? "READ" BR)]
20        ['(? "READS" BR)]
:END      []
})
;---------------------------------------------------------------------------------------------------
(defn ALT     [& ps])
(defn SEQ     [& ps])
(defn |       [& Ps]   (apply list 'ALT Ps))
(defn $       [P & Ns] (apply list '=$ P Ns)) (defn =$    [p n])
(defn .       [P & Ns] (apply list '=. P Ns)) (defn =.    [p n])
(defn LEN     [I]      (list 'Len I))
(defn POS     [I]      (list 'Pos I))         (defn Pos   [i])
(defn RPOS    [I]      (list 'RPos I))        (defn RPos  [i])
(defn BREAK   [S]      (list 'Break S))       (defn Break [s])
(defn ?       [S P])
(defn ?=      ([N P] "") ([N P R] ""))
(defn REPLACE [S1 S2 S3] "")
(defn DEFINE  [proto] "")
;---------------------------------------------------------------------------------------------------
(defn Len$ [s len]; lazy
  (if (<= len 0) s
    (if (not (seq s)) nil
		    (lazy-seq
		      (cons
		        (first s)
		        (Len$ (rest s) (dec len)))))))
(defn Len [s length]; eager
  (loop [s s len length]
		  (if (<= len 0) s; Success
		    (if (not (seq s)) nil; Failure
		      (recur (rest s) (dec len))))))

(declare EVAL)
(deftrace INVOKE [op & args]
  (case op
    |        (apply | args)
    $        (apply $ args)
    .        (apply . args)
    LEN      (LEN (first args))
    POS      (POS (first args))
    RPOS     (RPOS (first args))
    BREAK    (BREAK (first args))
    ?        (let [[s p] args] (? (str s) p))
    =        (let [[N r] args]
               (if-not (list? r)         ; (apply 'def n r)
                 (eval (list 'def N r))  ; (eval (list 'def n r))
                 (do                     ; (eval (read-string (str "(def " n " '" r ")")))
                   (eval (list 'def N))  ; use (load-string "(...) (...)") for multiple
                   (alter-var-root (trace (reference N)) (fn [oldr] r))
                 )
               ) r)
    ?=       (let [[n p R] args, r (EVAL R)]
               (eval (trace (list 'def n r))) r)
    DEFINE   (let [[proto] args]
               (let [spec (apply vector (re-seq #"[0-9A-Z_a-z]+" proto))]
                 (let [[n & params] spec, f (symbol n)]
                   (eval (trace (list 'defn f ['& 'args] ""))) "")))
    REPLACE  (let [[s1 s2 s3] args] (REPLACE s1 s2 s3))
    Roman    "";(apply Roman args)
  )
)
;---------------------------------------------------------------------------------------------------
(deftrace EVAL [E]
  (when E
      (cond
        (nil? E) E
        (float? E) E
        (string? E) E
        (integer? E) E
        (symbol? E) ($$ E)
        (vector? E) (apply list 'SEQ (map EVAL E))
        (list? E)
          (let [[op & parms] E]
            (cond
              (= op '.)  (let [[P N]   parms] (INVOKE '. (EVAL P) N))
              (= op '$)  (let [[P N]   parms] (INVOKE '$ (EVAL P) N))
              (= op '=)  (let [[N R]   parms] (INVOKE '= N (EVAL R)))
              (= op '?=) (let [[N P R] parms] (INVOKE '?= N (EVAL P) R))
              true       (let [args (apply vector (map EVAL parms))]
                           (apply INVOKE op args))
          ))
        nil nil
      ))
)
;---------------------------------------------------------------------------------------------------
;(deftype Address [no label])
;(defmethod key Address [a] (if (.label a) (.label a) (.no a)))
;(->Address at); construct via factory
;(Address. at); construct
;---------------------------------------------------------------------------------------------------
;(definline skey [address] (let [[no label] address] (if label label no)))
(defn skey [address] (let [[no label] address] (if label label no)))
(defn saddr [at]  (cond (keyword? at) [(STMTNOS at) at]
											 								    (string?  at) [(STMTNOS at) at]
																				    (integer? at) [at (LABELS at)]))
(deftrace RUN [at]
  (loop [current       (saddr at)]
      (if-let [key     (skey current)]
		      (if-let [stmt  (CODE key)]
		        (let [ferst  (first stmt)
		              seqond (second stmt)
		              goto   (if (map? ferst) ferst seqond)
		              body   (if (map? ferst) seqond ferst)]
            (if (EVAL body)
              (if (contains? goto :G)   (recur (saddr (:G goto)))
                (if (contains? goto :S) (recur (saddr (:S goto)))
                                        (recur (saddr (inc (current 0))))))
              (if (contains? goto :G)   (recur (saddr (:G goto)))
                (if (contains? goto :F) (recur (saddr (:F goto)))
                                        (recur (saddr (inc (current 0))))))
            )
		        )))))
;---------------------------------------------------------------------------------------------------
(defn -main "SNOBOL4/Clojure." [& args] (RUN 1))