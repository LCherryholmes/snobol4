(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z :refer [zipper root node down up right left branch? rightmost leftmost]])
  (:require [clojure.edn :as edn])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (:require [clojure.tools.trace :refer :all])
  (:require [criterium.core :as criterium :refer :all])
  (:require [instaparse.core :as insta :refer [defparser]]); Clojure
  (:refer-clojure :exclude [= + - * / num])
; (:require [instaparse.core :as insta :refer-macros [defparser]]); ClojureScript
)
;---------------------------------------------------------------------------------------------------
(defn equal         [x y] (clojure.core/= x y))
(defn not-equal     [x y] (clojure.core/not= x y))
(defn err           [x]   (clojure.core/- -1 x))
(defn add          ([x]   (clojure.core/+ x))  ([x y] (clojure.core/+ x y)))
(defn subtract     ([x]   (clojure.core/- x))  ([x y] (clojure.core/- x y)))
(defn multiply     ([x]   (clojure.core/* x))  ([x y] (clojure.core/* x y)))
(defn divide        [x y] (clojure.core// x y))
(declare CODE)
(declare RUN)
(declare EVAL); accepts string or unevaluated expression
(declare INVOKE)
(declare MATCH)
(declare ALT)
(declare SEQ)
;---------------------------------------------------------------------------------------------------
(defn bug [x] (println (type x) " " x) x)
(defn re-quote [& ss]
  (str "#'" (string/replace (apply str ss) #"(\\|')" #(str \\ (second %1))) "'"))
(def label #"[0-9A-Za-z][^ \t\r\n]*")
(def white  #"([ \t]+|\n\+|\n\.)")
(def grammar
" stmt      ::=  label? body? goto? <__> <eos?>
  label     ::=  #'[0-9A-Za-z][^ \\t\\r\\n]*'
  body      ::=  <_> (invoking | matching | replacing | assigning)
  goto      ::=  <_ ':' __> (jmp  |  sjmp (<__> fjmp)?  |  fjmp (<__> sjmp)?)
  invoking  ::=  subject
  matching  ::=  subject <_> pattern
  replacing ::=  subject <_> pattern <_ '='> replace
  assigning ::=  subject <_ '='> replace
 <subject>  ::=  uop
 <pattern>  ::=  (<'?' _>)? and
 <replace>  ::=  (<_> expr)?
  jmp    ::=  target
  sjmp   ::=  <'S'> target
  fjmp   ::=  <'F'> target
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
  S         ::=  #'\"([^\"]|\\x3B)*\"'  |  #'\\'([^\\']|\\x3B)*\\''
  N         ::=  #'[A-Za-z][A-Z_a-z0-9\\.]*'
  white     ::=  #'[ \\t]'
")
;---------------------------------------------------------------------------------------------------
(defn coder [ast stmtno]
  (insta/transform
    { :comment   (fn comment     [cmt]   [:comment cmt])
      :control   (fn control     [ctl]   [:control ctl])
      ;--------------------------------------------------------------------------
      :stmt      (fn stmt        [& ss]  (let [{L :label B :body G :goto} (apply conj ss)]
                                           (apply vector ss)
                                           {(if L L stmtno) [B G]}))
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
                                                     (if (symbol? tgt) (keyword tgt) tgt)))) {} gs)})
      :jmp       (fn jmp         [L]     [:G L])
      :sjmp      (fn sjmp        [L]     [:S L])
      :fjmp      (fn fjmp        [L]     [:F L])
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
      :hsh       (fn hsh        ([x] x) ([x     y]  (list 'sharp x y)))
      :div       (fn div        ([x] x) ([x     y]  (list '/ x y)))
      :mul       (fn mul        ([x] x) ([x     y]  (list '* x y)))
      :pct       (fn pct        ([x] x) ([x     y]  (list '% x y)))
      :xp        (fn xp         ([x] x) ([x  op y]  (list (symbol op) x y)))
      :cap       (fn cap        ([x] x) ([x  op y]  (list (symbol op) x y)))
      :ttl       (fn ttl        ([x] x) ([x     y]  (list 'tilde x y)))
      :uop       (fn uop        ([x] x) ([   op y]  (case op
                                                      "@" (list 'at y)
                                                      "#" (list 'sharp y)
                                                      "~" (list 'tilde y)
                                                          (list (symbol op) y))))
      :ndx       (fn ndx        ([n] n) ([n  & xs]  (apply list n xs)))
      :cnd       (fn cnd        ([x] x) ([x  & ys]  (apply vector 'comma x ys)))
      :inv       (fn inv         [f  & xs]          (apply list f xs))
      :N         (fn N           [n]                (symbol n))
      :S         (fn S           [s]                (subs s 1 (subtract (count s) 1)))
      :I         edn/read-string
      :R         edn/read-string
    } ast))
;---------------------------------------------------------------------------------------------------
(def parse-command    (insta/parser grammar :start :command))
(def parse-statement  (insta/parser grammar :start :stmt :total true))
(def parse-expression (insta/parser grammar :start :expr))
;---------------------------------------------------------------------------------------------------
(def stmtno (atom 0))
(def out
  (fn [item]
      (binding [pp/*print-right-margin* 120, pp/*print-miser-width* 100]
        (pp/pprint item))))
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
(def  ε          "")
(def  η          ##NaN)
(def  &ALPHABET  (atom (apply vector (map #(char %) (range 256)))))
(def  &ANCHOR    (atom 0))
(def  &DUMP      (atom 0)); 1, 2, and 3 levels
(def  &ERRLIMIT  (atom 0))
(def  &ERRTEXT   (atom ε))
(def  &ERRTYPE   (atom 0))
(def  &FTRACE    (atom 0))
(def  &FULLLSCAN (atom 0))
(def  &LASTNO    (atom 0))
(def  &LCASE     (atom "abcdefghijklmnopqrstuvwxyz"))
(def  &MAXLNGTH  (atom 4194304))
(def  &PROFILE   (atom 0))
(def  &TRACE     (atom 0))
(def  &TRIM      (atom 0))
(def  &STCOUNT   (atom 0))
(def  &STLIMIT   (atom 2147483647))
(def  &UCASE     (atom "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
;---------------------------------------------------------------------------------------------------
; Arrays and Tables
(defn ARRAY      [A] ε)
(defn ITEM       [])
(defn PROTOTYPE  [])
(defn SORT       [])
(defn RSORT      [A])
(defn TABLE      [T] ε)
;---------------------------------------------------------------------------------------------------
; Function control
(defn APPLY      [])
(defn ARG        [])
(defn DEFINE     [])
(defn LOAD       [])
(defn LOCAL      [])
(defn OPSYN      [])
(defn UNLOAD     [])
;---------------------------------------------------------------------------------------------------
; Input/output
(def  INPUT$     (atom ε))
(def  OUTPUT$    (atom ε))
(def  TERMINAL$  (atom ε))
(defn BACKSPACE  [] ε)
(defn DETACH     [] ε)
(defn EJECT      [] ε)
(defn ENDFILE    [] ε)
(defn INPUT      [] ε)
(defn OUTPUT     [] ε)
(defn REWIND     [] ε)
(defn SET        [] ε)
;---------------------------------------------------------------------------------------------------
; Memory
(defn CLEAR      [] ε)
(defn COLLECT    [] ε)
(defn DUMP       [] ε)
;---------------------------------------------------------------------------------------------------
; Miscellaneous
(defn CHAR       [] ε)
(defn CONVERT    [] ε)
(defn CONVERT    [] ε)
(defn DATATYPE   [])
(defn DATE       [])
(defn SIZE       [s] 0)
(defn TIME       [])
;---------------------------------------------------------------------------------------------------
; Conversions
(defn     num    [x] (cond
                       (double? x) x
                       (integer? x) (.doubleValue x)
                       true (try (Double/parseDouble x)
                              (catch NumberFormatException E ##NaN))))
(defn     ncvt   [x] (list 'num x)); `(try (Integer. ~x) (catch Exception E (try (Float. ~x) (catch Exception E #Nan))))
(defn     scvt   [x] (list 'str x))
(defmacro numcvt [x] `(ncvt ~x)); `(try (Integer. ~x) (catch Exception E (try (Float. ~x) (catch Exception E #Nan))))
(defmacro strcvt [x] `(str ~x))
;---------------------------------------------------------------------------------------------------
; Operators
(defn assign        [n x]     nil)
(defn annihilate    [x]       nil)
(defn match-replace [n s p]   nil)
(defn keyword-value [n]       nil)
(defn dollar-value  [n]       nil)
(defn dot-name      [n]      `(if (list? ~n) ~n (list 'identity ~n)))
(defn $=            [p n])
(defn .=            [p n])
(defn negate        [p]      `(if (nil? ~p) ε nil))
(defmacro vectorize [x]      `(if (vector? ~x) ~x (vector ~x)))
(defn flatten-1     [Ω]       (mapcat #(if (sequential? %) % [%]) Ω))
(defn flatten-one   [op & Ω]  (apply list (reduce
                                (fn [Φ ω] (reduce
                                  (fn [Σ σ] (conj Σ σ))
                                  Φ ω))
                                [op] (vectorize Ω))))
(defn     x-2 [op x y]        (list op x y))
(defn     x-3 [op x y z]      (list op x y z))
(defn     x-n [op x y z & Ω]  (flatten-1 (conj Ω z y x op)))
(defmacro n-1 [op x]          (list op (numcvt x)))
(defmacro n-2 [op x y]        (list op (numcvt x) (numcvt y)))
(defmacro n-3 [op x y z]      (list op (numcvt x) (numcvt y)))
(defmacro n-n [op x y z & Ω]  (flatten-1 (map ncvt (conj Ω z y x op))))
(defmacro uneval [x]         `(if (list? ~x) ~x (list 'identity ~x)))
;---- ----- -------------------------------------------- ------- -- ----- ----------------------------------------------
(defn =     ([x]        ##NaN)                   ; unary            programable
            ([n x]      (assign n x)))           ; binary   0 right assignment
(defn ?     ([x]        (annihilate x))          ; unary            interrogation value annihilation
            ([s p]      (MATCH (seq s) 0 p))     ; binary   1 right match pattern
            ([n s p]    (match-replace n s p)))  ; tertiary 1 right match pattern then replace
(defn ?=    ([n s p]    (match-replace n s p)))  ; tertiary 1 right match pattern then replace
(defn &     ([n]        (keyword-value n))       ; unary            keyword
            ([x y]      ##NaN))                  ; binary   2 left  programable
(defn |     ([x]        ##NaN)                   ; unary            programable
            ([x y]      (x-2 'ALT x y))          ; binary   3 right pattern, alternation
            ([x y & zs] (x-n 'ALT x y zs)))      ; multi    3 right pattern, alternation
(defn at    ([n]        (list 'cursor n))        ; unary            pattern, assign cursor position
            ([x y]      ##NaN))                  ; binary   4 right programable
(defn +     ([x]        (n-1 add x))             ; unary            addition
            ([x y]      (n-2 add x y))           ; binary   6 left  addition
            ([x y & zs] (n-n add x y zs)))       ; multi    6 left  addition
(defn -     ([x]        (n-1 subtract x))        ; unary            subtraction
            ([x y]      (n-2 subtract x y))      ; binary   6 left  subtraction
            ([x y & zs] (n-n subtract x y zs)))  ; multi    6 left  subtraction
(defn sharp ([x]        ##NaN)                   ; unary            programable
            ([x y]      ##NaN))                  ; binary   7 left  programable
(defn /     ([x]        ##NaN)                   ; unary            programable
            ([x y]      (n-2 divide x y)))       ; binary   8 left  division
(defn *     ([x]        (uneval x))              ; unary            defer evaluation, unevaluated expression
            ([x y]      (n-2 multiply x y))      ; binary   9 left  multiplication
            ([x y & zs] (n-n multiply x y zs)))  ; multi    9 left  multiplication
(defn %     ([x]        ##NaN)                   ; unary            programable
            ([x y]      ##NaN))                  ; binary  10 left  programable
(defn !     ([x]        ##NaN)                   ; unary            programable
            ([x y]      (n-2 Math/pow x y)))     ; binary  11 right exponentiation
(defn **    ([x y]      (n-2 Math/pow x y)))     ; binary  11 right exponentiation
(defn $     ([n]        (dollar-value n))        ; unary            indirection
            ([x y]      (x-2 $= x y))            ; binary  12 left  immediate assignment
            ([x y & zs] (x-n $= x y zs)))        ; multi   12 left  immediate assignment
(defn .     ([x]        (dot-name x))            ; unary            name
            ([x y]      (x-2 .= x y))            ; binary  12 left  conditional assignment
            ([x y & zs] (x-n .= x y zs)))        ; multi   12 left  conditional assignment
(defn tilde ([x]        (list 'negate x))        ; unary            pattern, negates failure or success
            ([x y]      ##NaN))                  ; binary  13 left  programable
;---------------------------------------------------------------------------------------------------
; Comparison
(defmacro INTEGER [x])
(defn primitive
      [func default missing cvt condition]
      (list 'defn func
        (list []             missing)
        (list ['x]           (list 'if (condition (cvt 'x) default) ε))
        (list ['x 'y '& '_]  (list 'if (condition (cvt 'x) (cvt 'y)) ε))))
(eval (primitive 'EQ     0   ε ncvt     #(list 'clojure.core/= %1 %2))); Numeric comparison
(eval (primitive 'NE     0 nil ncvt     #(list 'not= %1 %2)))
(eval (primitive 'LE     0   ε ncvt     #(list '<=   %1 %2)))
(eval (primitive 'LT     0 nil ncvt     #(list '<    %1 %2)))
(eval (primitive 'GE     0   ε ncvt     #(list '>=   %1 %2)))
(eval (primitive 'GT     0 nil ncvt     #(list '>    %1 %2)))
(eval (primitive 'IDENT  ε   ε identity #(list 'identical? %1 %2))); Object comparison
(eval (primitive 'DIFFER ε nil identity #(list 'not  (list 'identical? %1 %2))))
(eval (primitive 'LEQ    ε   ε scvt     #(list 'clojure.core/= %1 %2))); String comparison
(eval (primitive 'LNE    ε nil scvt     #(list 'not= %1 %2)))
(eval (primitive 'LLE    ε   ε scvt     #(list '<=   %1 %2)))
(eval (primitive 'LLT    ε nil scvt     #(list '<    %1 %2)))
(eval (primitive 'LGE    ε   ε scvt     #(list '>=   %1 %2)))
(eval (primitive 'LGT    ε nil scvt     #(list '>    %1 %2)))
;---- ----- -------------------------------------------- ------- -- ----- ----------------------------------------------
; Numeric
(defmacro SIN    []  `(defn SIN  [x] (Math/sin  ~(numcvt 'x))))
(defmacro COS    []  `(defn COS  [x] (Math/cos  ~(numcvt 'x))))
(defmacro TAN    []  `(defn TAN  [x] (Math/tan  ~(numcvt 'x))))
(defmacro ASIN   []  `(defn ASIN [x] (Math/asin ~(numcvt 'x))))
(defmacro ACOS   []  `(defn ACOS [x] (Math/acos ~(numcvt 'x))))
(defmacro ATAN   []  `(defn ATAN [x] (Math/atan ~(numcvt 'x))))
(defmacro EXP    []  `(defn EXP  [x] (Math/exp  ~(numcvt 'x))))
(defmacro LN     []  `(defn LN   [x] (Math/log  ~(numcvt 'x))))
(defmacro SQRT   []  `(defn SQRT [x] (Math/sqrt ~(numcvt 'x))))
(defmacro REMDR  []  `(defn REMDR [x y] (clojure.core/rem ~(numcvt 'x) ~(numcvt 'y))))
(defmacro CHOP   []  `(defn CHOP [x] (let [_x ~(numcvt 'x)] (if (< _x 0.0) (Math/ceil _x) (Math/floor _x)))))
;---------------------------------------------------------------------------------------------------
; Pattern match
(defn ANY        [S]      (list 'ANY$     S))
(defn ARBNO      [P]      (list 'ARBNO!   P))
(defn BREAK      [S]      (list 'BREAK$   S))
(defn BREAKX     [S]      (list 'BREAKX$  S))
(defn FENCE     ([]       (list 'FENCE!    )); FENCE pattern variable
                ([P]      (list 'FENCE!  P))); FENCE pattern function
(defn LEN        [I]      (list 'LEN#     I))
(defn NOTANY     [S]      (list 'NOTANY$  S))
(defn POS        [I]      (list 'POS#     I))
(defn RPOS       [I]      (list 'RPOS#    I))
(defn RTAB       [I]      (list 'RTAB#    I))
(defn SPAN       [S]      (list 'SPAN$    S))
(defn TAB        [I]      (list 'TAB#     I))
(def  ARB                 (list 'ARB!      ))
(def  BAL                 (list 'BAL!      ))
(def  REM                 (list 'REM!      ))
(def  ABORT               (list 'ABORT!    ))
(def  FAIL                (list 'FAIL!     ))
(def  SUCCEED             (list 'SUCCEED!  ))
;---------------------------------------------------------------------------------------------------
; Program control
(defn EXIT       []); string or integer argument
(defn HOST       [])
(defn SETEXIT    [])
(defn STOPTR     [])
(defn TRACE      [])
;---------------------------------------------------------------------------------------------------
; Program-defined datatype
(defn DATA       [])
(defn FIELD      [])
(defn DATATYPE   [])
;---------------------------------------------------------------------------------------------------
; Synthesis (string, pattern, and object)
(defn DUPL       [x i]); using string concat or pattern sequence
(defn LPAD       [])
(defn REPLACE    [s1 s2 s3] "")
(defn REVERSE    [])
(defn RPAD       [])
(defn SUBSTR     [])
(defn TRIM       [])
(defn COPY       [x]); Object creation
;---------------------------------------------------------------------------------------------------
(defn LEN$$ [s len] (if (<= len 0) s (if (not (seq s)) nil (lazy-seq (cons (first s) (LEN$$ (rest s) (dec len)))))))
;---------------------------------------------------------------------------------------------------
(defn reference [N]
  (comment)
  (if-let [ns-name (namespace N)]
    (do (comment "ns-name: " ns-name " " N)
      (when-let [ns-ref (or (get (ns-aliases *ns*) (symbol ns-name))
                            (find-ns (symbol ns-name)))]
        (do (comment "ns-ref: " ns-ref)
          (get (ns-publics ns-ref) (symbol (name N))))))
    (do (comment "*ns*: " *ns* " " N)
      (if-let [var-ref (get (ns-map *ns*) (symbol (name N)))] var-ref
        (get (ns-map (find-ns (symbol "snobol4.core"))) (symbol (name N)))))))
(defn $$ [N] (if-let [V (reference N)] (var-get V) ε)); (var-get (eval (list 'var N)))
;---------------------------------------------------------------------------------------------------
(declare dq path part)
(declare Roman n)
(defn dq [_path] (binding [dq ε path _path part ε] (RUN :dq) dq));DEFINE('dq(path)part')
(defn Roman-binding [_n] (binding [Roman ε n _n] (RUN :Roman) Roman))
(defn Roman-save-restore [_n]
 '(let [_Roman ($$ 'Roman) __n ($$ 'n)]
    (def Roman ε)
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
2         ['(= BD [(| "BE" "BO" "B") (| "AR" "A") (| "DS" "D")])]
3         ['(? "BEARDS" BD)]
4         ['(? "BEARD" BD)]
5         ['(? "BEADS" BD)]
6         ['(? "BEAD" BD)]
7         ['(? "BARDS" BD)]
8         ['(? "BARD" BD)]
9         ['(? "BADS" BD)]
10        ['(? "BAD" BD)]
11        ['(? "BATS" BD)]
12        ['(= BR [(| "B" "F" "L" "R") (| "E" "EA") (| "D" "DS")])]
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
; Scanners
(deftrace ARB!     [Σ Δ Π]   [nil (err Δ)])
(deftrace BAL!     [Σ Δ Π]   [nil (err Δ)])
(deftrace ARBNO!   [Σ Δ Π]   [nil (err Δ)])
(deftrace FENCE!   [Σ Δ Π]   [nil (err Δ)])
(deftrace FENCE!!  [Σ Δ Π]   [nil (err Δ)])
(deftrace BREAKX$  [Σ Δ Π]   [nil (err Δ)])
(deftrace ABORT!   [Σ Δ Π]   [nil (err Δ)])
(deftrace SUCCEED! [Σ Δ Π]   [Σ Δ])
(deftrace FAIL!    [Σ Δ Π]   [Σ (err Δ)])
(deftrace POS#     [Σ Δ Π]   (if (equal Δ Π)         [Σ Δ]       [Σ (err Δ)]))
(deftrace RPOS#    [Σ Δ Π]   (if (equal (count Σ) Π) [Σ Δ]       [Σ (err Δ)]))
(deftrace ANY$     [Σ Δ Π]   (if (not (seq Σ))       [Σ (err Δ)] (if     (contains? Π (first Σ)) [(rest Σ) (inc Δ)] [Σ (err Δ)])))
(deftrace NOTANY$  [Σ Δ Π]   (if (not (seq Σ))       [Σ (err Δ)] (if-not (contains? Π (first Σ)) [(rest Σ) (inc Δ)] [Σ (err Δ)])))
(deftrace REM!     [Σ Δ Π]   (loop [σ Σ δ Δ    ] (if (not (seq σ))    [σ δ] (recur (rest σ) (inc δ)))))
(deftrace LIT$     [Σ Δ Π]   (loop [σ Σ δ Δ π Π]
                               (if (not (seq π))     [σ δ]
                                  (if (not (seq σ))  [σ (err δ)]
                                    (if (not-equal (first σ) (first π)) [σ (err δ)]
                                      (recur (rest σ) (inc δ) (rest π)))))))
(deftrace LEN#     [Σ Δ Π]   (loop [σ Σ δ Δ    ] (if (>= δ (add Δ Π)) [σ δ] (if (not (seq σ)) [σ (err δ)] (recur (rest σ) (inc δ))))))
(deftrace TAB#     [Σ Δ Π]   (loop [σ Σ δ Δ    ] (if (>= δ Π)         [σ δ] (if (not (seq σ)) [σ (err δ)] (recur (rest σ) (inc δ))))))
(deftrace RTAB#    [Σ Δ Π]   (loop [σ Σ δ Δ    ] (if (>= (count σ) Π) [σ δ] (if (not (seq σ)) [σ (err δ)] (recur (rest σ) (inc δ))))))
(deftrace SPAN$    [Σ Δ Π]   (loop [σ Σ δ Δ    ] (if (and (not (seq σ)) (identical? σ Σ)) [σ (err δ)] (if (and (not (contains? Π (first Σ))) (identical? σ Σ)) [σ (err δ)] (recur (rest σ) (inc δ))))))
(deftrace BREAK$   [Σ Δ Π]   (loop [σ Σ δ Δ    ] (if (not (seq σ))    [σ (err δ)] (if (contains? Π (first Σ)) [σ δ] (recur (rest σ) (inc δ))))))
(deftrace ALT      [Σ Δ & Π] (loop [        π Π]
                               (if (not (seq π)) [Σ (err Δ)]
                                 (let [[σ δ] (MATCH Σ Δ (first π))]
                                   (if (>= δ 0) [σ δ]
                                     (recur (rest π)))))))
(deftrace SEQ      [Σ Δ & Π] (loop [σ Σ δ Δ π Π]
                               (if (not (seq π)) [σ δ]
                                  (let [[σ δ] (MATCH σ δ (first π))]
                                    (if (< δ 0) [σ δ]
                                      (recur σ δ (rest π)))))))
(comment  MATCH    [Σ Δ Π]   (cond
                               (string? Π) (LIT$ Σ Δ Π)
                               (seq? Π) (let [[λ & π] Π, λ ($$ λ)] (apply λ Σ Δ π))))
;---------------------------------------------------------------------------------------------------
(defn     BRANCH?   [π] (or (list? π) (seq? π) (vector? π)))
(defn     pro-down  [ζ] (down ζ))
(defn     re-down   [ζ] (rightmost (down ζ)))
(defn     right-end [ζ] (equal ζ (rightmost ζ)))
(defn     left-end  [ζ] (equal ζ (leftmost ζ)))
(defn     MATCH     [Σ Δ Π]
  (loop [ζ (zipper BRANCH? rest nil Π), direction :proceed-down, Σ Σ, Δ Δ, Ψ 0, Ω []]
    (out Ω)
    (case direction
    :proceed-down
      (if (branch? ζ)     (recur (pro-down ζ) :proceed-down  Σ Δ (inc Ψ) (conj Ω [Δ Ψ]))
        (if (right-end ζ)
          (if (equal Ψ 0) (recur (re-down  ζ) :receed-down   Σ Δ (inc Ψ) (pop  Ω))
                          (recur (up       ζ) :proceed-right Σ Δ (dec Ψ) (conj Ω [Δ Ψ])))
                          (recur (right    ζ) :proceed-down  Σ Δ      Ψ  (conj Ω [Δ Ψ]))))
     :proceed-right
       (if (right-end ζ)
         (if (equal Ψ 0)  (recur (re-down  ζ) :receed-down   Σ Δ (inc Ψ) (pop  Ω))
                          (recur (up       ζ) :proceed-right Σ Δ (dec Ψ) (conj Ω [Δ Ψ])))
                          (recur (right    ζ) :proceed-down  Σ Δ      Ψ  (conj Ω [Δ Ψ])))
     :receed-down
      (if (branch? ζ)     (recur (re-down  ζ) :receed-down   Σ Δ (inc Ψ) (pop  Ω))
        (if (left-end ζ)
          (if (equal Ψ 0)        (root     ζ)
                          (recur (up       ζ) :receed-left   Σ Δ (dec Ψ) (pop  Ω)))
                          (recur (left     ζ) :receed-down   Σ Δ      Ψ  (pop  Ω))))
     :receed-left
       (if (left-end ζ)
         (if (equal Ψ 0)         (root     ζ)
                          (recur (up       ζ) :receed-left   Σ Δ (dec Ψ) (pop  Ω)))
                          (recur (left     ζ) :receed-down   Σ Δ      Ψ  (pop  Ω))))))
;---------------------------------------------------------------------------------------------------
(defn INVOKE [op & args]
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
                   (eval (trace (list 'defn f ['& 'args] ε))) ε)))
    REPLACE  (let [[s1 s2 s3] args] (REPLACE s1 s2 s3))
    EQ       (EQ (first args) (second args))
    Roman    ε;(apply Roman args)
  )
)
;---------------------------------------------------------------------------------------------------
(defn EVAL [E]
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
            (clojure.core/= op '.)  (let [[P N]   parms] (INVOKE '. (EVAL P) N))
            (clojure.core/= op '$)  (let [[P N]   parms] (INVOKE '$ (EVAL P) N))
            (clojure.core/= op '=)  (let [[N R]   parms] (INVOKE '= N (EVAL R)))
            (clojure.core/= op '?=) (let [[N P R] parms] (INVOKE '?= N (EVAL P) R))
            true (let [args (apply vector (map EVAL parms))]
                   (apply INVOKE op args))
        ))
      nil nil)))
;---------------------------------------------------------------------------------------------------
(defn skey [address]   (let [[no label] address] (if label label no)))
(defn saddr [at] (cond (keyword? at) [(STMTNOS at) at]
                       (string?  at) [(STMTNOS at) at]
                       (integer? at) [at (LABELS at)]))
(defn              RUN [at]
  (loop [      current (saddr at)]
    (if-let [      key (skey current)]
      (if-let [   stmt (CODE key)]
        (let [   ferst (first stmt)
                seqond (second stmt)
                  goto (if (map? ferst) ferst seqond)
                  body (if (map? ferst) seqond ferst)]
                       (if (EVAL body)
                         (if (contains? goto :G)   (recur (saddr (:G goto)))
                           (if (contains? goto :S) (recur (saddr (:S goto)))
                                                   (recur (saddr (inc (current 0))))))
                         (if (contains? goto :G)   (recur (saddr (:G goto)))
                           (if (contains? goto :F) (recur (saddr (:F goto)))
                                                   (recur (saddr (inc (current 0))))))
                       ))))))
;---------------------------------------------------------------------------------------------------
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
;---------------------------------------------------------------------------------------------------
(def SNO [
"copy OUTPUT = INPUT :S(copy)F(END)"
" n = 0 ;copy OUTPUT = INPUT :F(done) ; n = n + 1 :(copy) ;done OUTPUT = \"Program copied \" n \" lines.\" ;END"
" &TRIM = 1 ;nextl chars = chars + SIZE(INPUT) :F(done); lines = lines + 1 :(nextl) ;done OUTPUT = chars \" characters, \" +lines \" lines read.\" ;END"
" &TRIM = 1
  TERMINAL = 'Enter test lines, terminate with EOF'
* Read input line, convert lower case to upper.
loop s = REPLACE(TERMINAL, &LCASE, &UCASE) :F(END)
* Check for palindrome:
  TERMINAL = IDENT(s, REVERSE(s)) 'Palindrome!' :S(loop)
  TERMINAL = 'No, try again.' :(loop)
END
"
  " A = (X ? Y) (Q ? P)"
  " IDENT(,, TERMINAL = 'Sneaky!')"
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
            (let [ast (parse-command line) code (coder ast 1)]
              (println code)))))
  ))
;---------------------------------------------------------------------------------------------------
(defn -main "SNOBOL4/Clojure." [& args] (RUN 1))
