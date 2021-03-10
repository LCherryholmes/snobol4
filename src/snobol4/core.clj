(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z])
  (:require [clojure.edn :as edn])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (:require [instaparse.core :as insta :refer [defparser]]); Clojure
; (:require [instaparse.core :as insta :refer-macros [defparser]]); ClojureScript
)

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
  white     ::=  #'[ \\t]'  (*(' ' | '\\t')+ | '\\n+' | '\\n.'*)
"))

(defn coder [ast stmtno]
; (pp/pprint ast)
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
      :alt       (fn alt        ([x] x) ([x  & ys]  (apply vector '| x ys)))
      :cat       (fn cat        ([x] x) ([x  & ys]  (apply vector    x ys)))
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

(defn re-cat [& regexs] (re-pattern (apply str regexs)))
(def eol     #"[\n]")
(def eos     #"[;\n]")
(def skip    #"[^\n]*")
(def fill    #"[^;\n]*")
(def komment (re-cat #"[*]" skip eol))
(def control (re-cat #"[-]" fill eos))
(def kode    (re-cat #"[^;\n.+*-]" fill "(" #"\n[.+]" fill ")*" eos))
(def block   (re-cat komment "|" control "|" kode "|" eol))
(def dirs ["./src/sno" "./src/inc" "./src/test ./src/rinky"])
(def SNO [])
(def stmtno (atom 0))
(defn pprint [item]
  (binding [pp/*print-right-margin* 120, pp/*print-miser-width* 100]
    (pp/pprint item))
(defn doit []
  (doseq [filenm (files dirs)]
    (println ";------------------------------------------------------ " filenm)
    (case 2
      1 (doseq [s SNO]
          (doseq [b (re-seq block s)]
            (println b)))
      2 (let [program (slurp filenm)]
          (doseq [command (re-seq block program)]
            (let [cmd (first command)]
		            (cond
		              (nil? cmd) nil
		              (re-find #"^\*" cmd) nil
		              (re-find #"^\-" cmd) nil
		              true (let [stmt (string/replace
		                                (string/replace cmd
		                                  #"[ \t]*\r\n[+.][ \t]*" " ")
		                                #"\r\n$" "")
		                         stmtno (swap! stmtno inc)
		                         ast (parse-statement stmt)
		                         code (coder ast stmtno)]
		                     (if (and (map? code) (:reason code))
		                       (let [line   (:line code)
		                             column (:column code)
		                             text   (:text code)
		                             error  {stmtno [(list 'ERROR line column)]}]; text
		                         (pprint error))
		                       (pprint code)))
		  		               )))))
      3 (with-open [rdr (io/reader filenm)]
          (doseq [line (line-seq rdr)]
            (let [ast (parse-command line) code (coder ast)]
              (println code))
          ))
    )))

(defn -main "SNOBOL4 statement parser." [& args] (doit))