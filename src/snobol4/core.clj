(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z :refer [zipper root node down up right left branch? rightmost leftmost]])
  (:require [clojure.edn :as edn])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (:require [clojure.tools.trace :refer :all])
  (:require [criterium.core :as criterium :refer :all])
  (:require [instaparse.core :as insta :refer [defparser]]); :refer-macros [defparser]]); ClojureScript
  (:refer-clojure :exclude [= + - * / num])
)
;---------------------------------------------------------------------------------------------------
(defn add       ([x y] (clojure.core/+ x y)) ([x] (clojure.core/+ x)))
(defn subtract  ([x y] (clojure.core/- x y)) ([x] (clojure.core/- x)))
(defn multiply  ([x y] (clojure.core/* x y)) ([x] (clojure.core/* x)))
(defn divide     [x y] (clojure.core// x y))
(defn equal      [x y] (clojure.core/= x y))
(defn not-equal  [x y] (clojure.core/not= x y))
(defn out       [item] (binding [pp/*print-right-margin* 120, pp/*print-miser-width* 100] (pp/pprint item)) item)
;---------------------------------------------------------------------------------------------------
(declare RUN)
(declare EVAL!)
(declare INVOKE)
(declare MATCH)
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
  jmp       ::=  target
  sjmp      ::=  <'S'> target
  fjmp      ::=  <'F'> target
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
(defn emitter [ast]
  (insta/transform
    { :comment   (fn comment     [cmt]   [:comment cmt])
      :control   (fn control     [ctl]   [:control ctl])
      ;--------------------------------------------------------------------------
      :stmt      (fn stmt        [& ss]  (apply conj ss))
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
(defn assign        [n x]       nil)
(defn annihilate    [x]         nil)
(defn match-replace [n s p]     nil)
(defn keyword-value [n]         nil)
(defn dollar-value  [n]         nil)
(defn dot-name      [n]        `(if (list? ~n) ~n (list 'identity ~n)))
(defn $=            [p n])
(defn .=            [p n])
(defn negate        [p]        `(if (nil? ~p) ε nil))
(defn x-2           [op x y]    (list op x y))
(defn x-n           [op x y Ω]  (apply list (conj Ω y x op)))
(defn n-1           [op x]      (list op (numcvt x)))
(defn n-2           [op x y]    (list op (numcvt x) (numcvt y)))
(defn n-n           [op x y Ω]  (apply list op (map ncvt (conj Ω y x))))
(defmacro uneval [x]           `(if (list? ~x) ~x (list 'identity ~x)))
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
            ([x y]      (n-2 'Math/pow x y)))    ; binary  11 right exponentiation
(defn **    ([x y]      (n-2 'Math/pow x y)))    ; binary  11 right exponentiation
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
(eval (primitive 'EQ     0   ε ncvt     #(list 'equal %1 %2))); Numeric comparison
(eval (primitive 'NE     0 nil ncvt     #(list 'not=  %1 %2)))
(eval (primitive 'LE     0   ε ncvt     #(list '<=    %1 %2)))
(eval (primitive 'LT     0 nil ncvt     #(list '<     %1 %2)))
(eval (primitive 'GE     0   ε ncvt     #(list '>=    %1 %2)))
(eval (primitive 'GT     0 nil ncvt     #(list '>     %1 %2)))
(eval (primitive 'LEQ    ε   ε scvt     #(list 'equal %1 %2))); String comparison
(eval (primitive 'LNE    ε nil scvt     #(list 'not=  %1 %2)))
(eval (primitive 'LLE    ε   ε scvt     #(list '<=    %1 %2)))
(eval (primitive 'LLT    ε nil scvt     #(list '<     %1 %2)))
(eval (primitive 'LGE    ε   ε scvt     #(list '>=    %1 %2)))
(eval (primitive 'LGT    ε nil scvt     #(list '>     %1 %2)))
(eval (primitive 'IDENT  ε   ε identity #(list 'identical? %1 %2))); Object comparison
(eval (primitive 'DIFFER ε nil identity #(list 'not   (list 'identical? %1 %2))))
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
(defn charset    [S]      (reduce #(conj %1 %2) #{} S))
(defn ANY        [S]      (list 'ANY$     (charset S)))
(defn BREAK      [S]      (list 'BREAK$   (charset S)))
(defn BREAKX     [S]      (list 'BREAKX$  (charset S)))
(defn NOTANY     [S]      (list 'NOTANY$  (charset S)))
(defn SPAN       [S]      (list 'SPAN$    (charset S)))
(defn ARBNO      [P]      (list 'ARBNO!   P))
(defn FENCE     ([]       (list 'FENCE!    )); FENCE pattern variable
                ([P]      (list 'FENCE!   P))); FENCE pattern function
(defn LEN        [I]      (list 'LEN#     I))
(defn POS        [I]      (list 'POS#     I))
(defn RPOS       [I]      (list 'RPOS#    I))
(defn RTAB       [I]      (list 'RTAB#    I))
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
  (if-let [ns-name (namespace N)]
    (do (comment "ns-name: " ns-name " " N)
      (when-let [ns-ref (or (get (ns-aliases *ns*) (symbol ns-name))
                            (find-ns (symbol ns-name)))]
        (do (comment "ns-ref: " ns-ref)
          (get (ns-publics ns-ref) (symbol (name N))))))
    (do (comment N)
      (if-let [user-ref  (get (ns-map *ns*)                         (symbol (name N)))] user-ref
        (if-let [sno-ref (get (ns-map (find-ns 'snobol4.core))      (symbol (name N)))] sno-ref
                         (get (ns-map (find-ns 'snobol4.core-test)) (symbol (name N))))))))
(defn $$ [N] (if-let [V (reference N)] (var-get V) ε)); (var-get (eval (list 'var N)))
;---------------------------------------------------------------------------------------------------
; Scanners
(defn err      [Σ Δ]     [Σ (clojure.core/- -1 Δ)])
(defn ARB!     [Σ Δ Π]   (err nil Δ))
(defn BAL!     [Σ Δ Π]   (err nil Δ))
(defn ARBNO!   [Σ Δ Π]   (err nil Δ))
(defn FENCE!   [Σ Δ Π]   (err nil Δ))
(defn FENCE!!  [Σ Δ Π]   (err nil Δ))
(defn BREAKX$  [Σ Δ Π]   (err nil Δ))
(defn ABORT!   [Σ Δ Π]   (err nil Δ))
(defn SUCCEED! [Σ Δ Π]   [Σ Δ])
(defn FAIL!    [Σ Δ Π]   (err Σ Δ))
(defn POS#     [Σ Δ Π]   (if (equal Δ Π)         [Σ Δ] (err Σ Δ)))
(defn RPOS#    [Σ Δ Π]   (if (equal (count Σ) Π) [Σ Δ] (err Σ Δ)))
(defn ANY$     [Σ Δ Π]   (if (not (seq Σ))       (err Σ Δ) (if     (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn NOTANY$  [Σ Δ Π]   (if (not (seq Σ))       (err Σ Δ) (if-not (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn REM!     [Σ Δ Π]   (loop [σ Σ δ Δ    ]     (if (not (seq σ))    [σ δ] (recur (rest σ) (inc δ)))))
(defn LIT$     [Σ Δ Π]   (loop [σ Σ δ Δ π Π]     (if (not (seq π))    [σ δ]
                                                    (if (not (seq σ)) (err σ δ)
                                                      (if (not-equal (first σ) (first π)) (err σ δ)
                                                        (recur (rest σ) (inc δ) (rest π)))))))
(defn LEN#     [Σ Δ Π]   (loop [σ Σ δ Δ    ]     (if (>= δ (add Δ Π)) [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ))))))
(defn TAB#     [Σ Δ Π]   (loop [σ Σ δ Δ    ]     (if (>= δ Π)         [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ))))))
(defn RTAB#    [Σ Δ Π]   (loop [σ Σ δ Δ    ]     (if (>= (count σ) Π) [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ))))))
(defn SPAN$    [Σ Δ Π]   (loop [σ Σ δ Δ    ]     (if (not (contains? Π (first σ)))
                                                   (if (not-equal δ Δ) [σ δ] (err σ δ))
                                                   (recur (rest σ) (inc δ)))))
(defn BREAK$   [Σ Δ Π]   (loop [σ Σ δ Δ    ]     (if (not (seq σ)) (err σ δ)
                                                   (if (contains? Π (first σ)) [σ δ]
                                                     (recur (rest σ) (inc δ))))))
(defn ALT      [Σ Δ & Π] (loop [        π Π]     (if (not (seq π)) (err Σ Δ)
                                                   (let [[σ δ] (MATCH Σ Δ (first π))]
                                                     (if (>= δ 0) [σ δ]
                                                       (recur (rest π)))))))
(defn SEQ      [Σ Δ & Π] (loop [σ Σ δ Δ π Π]     (if (not (seq π)) [σ δ]
                                                    (let [[σ δ] (MATCH σ δ (first π))]
                                                      (if (< δ 0) [σ δ]
                                                        (recur σ δ (rest π)))))))
(defn MATCH!   [Σ Δ Π]   (cond (string? Π) (LIT$ Σ Δ Π)
                               (seq? Π) (let [[λ & π] Π, λ ($$ λ)] (apply λ Σ Δ π))))
;===================================================================================================
(defn top  [Ψ]   (last Ψ)); using vector stack, make "first" if ever using list stack
(defn pull [Ψ]   (if Ψ (if-not (empty? Ψ) (pop Ψ)))); protected pop, top is top for list or vector
(defn push [Ψ ζ] (if Ψ (conj Ψ ζ))); ZETA, zipper
(defn 🡡 [Ω]     (top Ω))
(defn 🡥 [Ω ζ]   (push Ω ζ))
(defn 🡧 [Ω]     (pull Ω))
(defn 🡧🡡 [Ω]    (top (pull Ω)))
(defn 🡧🡥 [Ω ζ]  (push (pull Ω) ζ))
(defn 🡧🡧 [Ω]    (pull (pull Ω)))
(comment Ω⭳ Ω⭱ Ω↥ Ω↧ Ω⭶ Ω⭸ Ω⭷ Ω⭹)
;---------------------------------------------------------------------------------------------------
(defn ζΣ   [ζ]      (if ζ (ζ 0))); SIGMA, Subject, String Start, Sequence of characters
(defn ζΔ   [ζ]      (if ζ (ζ 1))); DELTA, start position (Difference from start)
(defn ζσ   [ζ]      (if ζ (ζ 2))); sigma, subject, string end
(defn ζδ   [ζ]      (if ζ (ζ 3))); delta, end position
(defn ζΠ   [ζ]      (if ζ (ζ 4))); PI, Pattern Internal
(defn ζφ   [ζ]      (if ζ (ζ 5))); phi, part internal, pattern-piece iteration
(defn ζΨ   [ζ]      (if ζ (ζ 6))); psi, parent stack internal
(defn ζα   [ζ]      (<= (ζφ ζ) 0)); alpha, is the beginning?
(defn ζω   [ζ]      (>= (ζφ ζ) (count (ζΠ ζ)))); omega, is the end?
(defn ζλ   [ζ]      (cond; lamda, operation, function
                      (nil?        ζ) nil
                      (nil?    (ζΠ ζ)) nil
                      (string? (ζΠ ζ)) 'LIT$
                      (list?   (ζΠ ζ)) (first (ζΠ ζ))
                      (seq?    (ζΠ ζ)) (first (ζΠ ζ))
                      true     (out ["lamda? " (type (ζΠ ζ)) (ζΠ ζ)])))
(defn ζ↓   [ζ]      (let [[Σ Δ _ _ Π φ Ψ] ζ] [Σ Δ Σ Δ (nth Π φ) 1 (🡥 Ψ ζ)])); call down
(defn ζ↑  ([ζ σ δ]  (let [[Σ Δ _ _ _ _ Ψ] ζ] [Σ Δ σ δ (ζΠ (🡡 Ψ)) (ζφ (🡡 Ψ)) (🡧 Ψ)])); return up scan
          ([ζ]      (let [[Σ Δ σ δ _ _ Ψ] ζ] [Σ Δ σ δ (ζΠ (🡡 Ψ)) (ζφ (🡡 Ψ)) (🡧 Ψ)]))); retun up result
(defn ζ→   [ζ]      (let [[_ _ σ δ Π φ Ψ] ζ] [σ δ σ δ Π (inc φ) Ψ])); proceed right
(defn ζ←   [ζ]      (let [[Σ Δ _ _ Π φ Ψ] ζ] [Σ Δ Σ Δ Π (inc φ) Ψ])); receed left
;---------------------------------------------------------------------------------------------------
(defn MATCH [Σ Δ Π]
  (loop [action :proceed, ζ [Σ Δ ε ε Π 1 []] Ω []]
    (let [λ (ζλ ζ)]
      (println (format "%-8s %2s %2s %-5s %2s %-10s %2s %-10s %s %s"
        action (count (ζΨ ζ)) (count Ω) λ (ζΔ ζ) (apply str (ζΣ ζ)) (ζδ ζ) (apply str (ζσ ζ)) (ζφ ζ) (ζΠ ζ)))
      (case λ
        nil  (do (println)
                 (case action (:proceed :succeed) true (:recede :fail) false))
        ALT      (case action ;---------------------------------------------------------------------
                   :proceed
                     (if (ζω ζ)    (recur :recede  (🡧🡡 Ω) (🡧🡧 Ω))   ; no more alternatives, also, :fail (ζ↑ ζ) (🡧 Ω)
                                   (recur :proceed (ζ↓ ζ) (🡥 Ω ζ)))   ; try alternate
                   :recede         (recur :proceed (ζ← ζ) Ω)          ; try next alternate, keep left
                   :succeed        (recur :succeed (ζ↑ ζ) (🡧🡥 Ω ζ))  ; generator suspend (return) match
                   :fail           (recur :recede  (🡡 Ω) (🡧 Ω)))     ; generator reentry, try next
        SEQ      (case action ;---------------------------------------------------------------------
                   :proceed
                     (if (ζω ζ)    (recur :succeed (ζ↑ ζ) Ω)          ; no more subsequents, succeed
                                   (recur :proceed (ζ↓ ζ) Ω))         ; try subsequent
                   :succeed        (recur :proceed (ζ→ ζ) Ω)          ; try next subsequent, go right
                   :fail           (recur :recede  (🡡 Ω) (🡧 Ω)))     ; generator reentry, backtrack
        LIT$      (case action ;---------------------------------------------------------------------
                   :proceed
                   (let [[Σ Δ _ _ Π] ζ
                             [σ δ] (LIT$ Σ Δ Π)]                      ; scan literal
                     (if (>= δ 0)  (recur :succeed (ζ↑ ζ σ δ) Ω)      ; return match
                                   (recur :fail    (ζ↑ ζ Σ Δ) Ω))))   ; signal failure
       ;--------------------------------------------------------------------------------------------
       (ANY$ NOTANY$ SPAN$ BREAK$ BREAKX$ POS# RPOS#)
                 (case action
                   :proceed
                   (let [[Σ Δ _ _ Π] ζ
                             [σ δ] (($$ λ) Σ Δ (second Π))]           ; scan with primitive pattern
                     (if (>= δ 0)  (recur :succeed (ζ↑ ζ σ δ) Ω)      ; return match
                                   (recur :fail    (ζ↑ ζ Σ Δ) Ω))))   ; signal failure
      ; --------------------------------------------------------------------------------------------
        FAIL!                      (recur :recede  (🡡 Ω) (🡧 Ω))      ; signal failure, backtrack
        SUCCEED! (let [[Σ Δ] ζ]    (recur :succeed (ζ↑ ζ Σ Δ) Ω))     ; return epsilon match
        ARB!     nil
        BAL!     nil
        ARBNO!   nil
        ABORT!   nil
      ))))
;===================================================================================================
(def  eol               #"[\n]")
(def  eos               #"[;\n]")
(def  skip              #"[^\n]*")
(def  fill              #"[^;\n]*")
(defn re-cat [& rexes]  (re-pattern (apply str rexes)))
(def  komment           (re-cat #"[*]" skip eol))
(def  control           (re-cat #"[-]" fill eos))
(def  kode              (re-cat #"[^;\n.+*-]" fill "(" #"\n[.+]" fill ")*" eos))
(def  block             (re-cat komment "|" control "|" kode "|" eol))
(def  general-control-1 #"^-(ERRORS|EXECUTE|FAIL|OPTIMIZE|NOERRORS|NOEXECUTE|NOFAIL|NOOPTIMIZE)")
(def  general-control-2 #"^-(CASE|COPY|INCLUDE|IN)")
(def  listing-control-1 #"^-(EJECT|LIST|NOLIST|PRINT|NOPRINT|SINGLE|DOUBLE)")
(def  listing-control-2 #"^-(LINE|SPACE|STITL|TITLE)")
(def  parse-command     (insta/parser grammar :start :command))
(def  parse-statement   (insta/parser grammar :start :stmt :total true))
(def  parse-expression  (insta/parser grammar :start :expr))
(defn ERROR [info]      (list 'ERROR (:line info) (:column info) (:text info)))
;===================================================================================================
(defn INVOKE [op & args]
  (case op
    |        (apply | args)
    $        (apply $ args)
    .        (apply . args)
    LEN      (LEN    (first args))
    POS      (POS    (first args))
    RPOS     (RPOS   (first args))
    ANY      (ANY    (first args))
    BREAK    (BREAK  (first args))
    BREAKX   (BREAKX (first args))
    NOTANY   (NOTANY (first args))
    SPAN     (SPAN   (first args))
    FENCE    (first args)
    EQ       (EQ (first args) (second args))
    NE       (NE (first args) (second args))
    FAIL     FAIL
    ?        (let [[s p] args] (? (str s) p))
    =        (let [[N r] args]
               (if-not (list? r)         ; (apply 'def n r)
                 (eval (list 'def N r))  ; (eval (list 'def n r))
                 (do                     ; (eval (read-string (str "(def " n " '" r ")")))
                   (eval (list 'def N))  ; use (load-string "(...) (...)") for multiple
                   (alter-var-root (trace (reference N)) (fn [oldr] r))
                 )
               ) r)
    ?=       (let [[n p R] args, r (EVAL! R)]
               (eval (trace (list 'def n r))) r)
    DEFINE   (let [[proto] args]
               (let [spec (apply vector (re-seq #"[0-9A-Z_a-z]+" proto))]
                 (let [[n & params] spec, f (symbol n)]
                   (eval (trace (list 'defn f ['& 'args] ε))) ε)))
    REPLACE  (let [[s1 s2 s3] args] (REPLACE s1 s2 s3))
    Roman    ε;(apply Roman args)
))
;---------------------------------------------------------------------------------------------------
(deftrace EVAL [X] (cond (string? X) (EVAL! (first (emitter (parse-expression X)))), true (EVAL! X)))
(deftrace EVAL! [E]; Needs to handle failure
  (when E
    (cond
          (nil? E) E
        (float? E) E
       (string? E) E
      (integer? E) E
       (symbol? E) ($$ E)
       (vector? E) (apply list 'SEQ (map EVAL! E)); Needs to handle NULL concatentaion logic
         (list? E) (let [[op & parms] E]
                     (cond
                       (equal op '.)  (let [[P N]   parms] (INVOKE '. (EVAL! P) N))
                       (equal op '$)  (let [[P N]   parms] (INVOKE '$ (EVAL! P) N))
                       (equal op '=)  (let [[N R]   parms] (INVOKE '= N (EVAL! R)))
                       (equal op '?=) (let [[N P R] parms] (INVOKE '?= N (EVAL! P) R))
                       true (let [args (apply vector (map EVAL! parms))]
                              (apply INVOKE op args))))
             true  "Yikes! What is E?")))
;---------------------------------------------------------------------------------------------------
(def  eol               #"[\n]")
(def  eos               #"[;\n]")
(def  skip              #"[^\n]*")
(def  fill              #"[^;\n]*")
(defn re-cat [& rexes]  (re-pattern (apply str rexes)))
(def  komment           (re-cat #"[*]" skip eol))
(def  control           (re-cat #"[-]" fill eos))
(def  kode              (re-cat #"[^;\n.+*-]" fill "(" #"\n[.+]" fill ")*" eos))
(def  block             (re-cat komment "|" control "|" kode "|" eol))
(def  general-control-1 #"^-(ERRORS|EXECUTE|FAIL|OPTIMIZE|NOERRORS|NOEXECUTE|NOFAIL|NOOPTIMIZE)")
(def  general-control-2 #"^-(CASE|COPY|INCLUDE|IN)")
(def  listing-control-1 #"^-(EJECT|LIST|NOLIST|PRINT|NOPRINT|SINGLE|DOUBLE)")
(def  listing-control-2 #"^-(LINE|SPACE|STITL|TITLE)")
(def  parse-command     (insta/parser grammar :start :command))
(def  parse-statement   (insta/parser grammar :start :stmt :total true))
(def  parse-expression  (insta/parser grammar :start :expr))
(defn ERROR [info]      (list 'ERROR (:line info) (:column info) (:text info)))
;---------------------------------------------------------------------------------------------------
(defmacro comment? [command] (list 're-find #"^\*" command))
(defmacro control? [command] (list 're-find #"^\-" command))
(defn        CODE! [S]
  (let             [blocks (re-seq block (str S "\n"))]
    (loop          [block blocks NO 1 CODES {} NOS {} LABELS {}]
      (let         [command (first (first block))]
        (cond
              (nil? command) [CODES NOS LABELS]
          (comment? command) (recur (rest block) NO CODES NOS LABELS)
          (control? command) (recur (rest block) NO CODES NOS LABELS)
               true (let [ stmt (string/replace command #"[ \t]*\r?\n[+.][ \t]*" " ")
                           stmt (string/replace stmt #"\r?\n$" "")
                            ast (parse-statement stmt)
                           code (emitter ast)]
                      (if (and (map? code) (:reason code))
                        (recur (rest block) (inc NO) (assoc CODES NO (ERROR code)) NOS LABELS)
                        (let [label  (:label code)
                              body   (:body code)
                              goto   (:goto code)
                              key    (if label label NO)
                              code   (reduce #(conj %1 %2) [] [body goto])
                              nos    (if (keyword? key) (assoc NOS key NO) NOS)
                              labels (if (keyword? key) (assoc LABELS NO key) LABELS)
                              codes  (assoc CODES key code)]
                          (recur (rest block) (inc NO) codes nos labels)))))))))
;---------------------------------------------------------------------------------------------------
(def   STNO   (atom 0))
(def  <STNO>  (atom {}))
(def  <LABL>  (atom {}))
(def  <CODE>  (atom {}))
(defn  CODE   [S] (let [C (CODE! S) start (inc @STNO) codes (C 0) nos (C 1) labels (C 2)]
                    (loop [NO 1]
                      (if (> NO (count codes))
                        (if (and (@<LABL> start) (@<CODE> (@<LABL> start)))
                          (@<LABL> start)
                          (if (@<CODE> start) start))
                        (do
                          (swap! STNO inc)
                          (if-let [label (labels NO)]
                            (do
                              (swap! <CODE> #(assoc % label (codes label)))
                              (swap! <LABL> #(assoc % @STNO label))
                              (swap! <STNO> #(assoc % label @STNO)))
                            (swap! <CODE> #(assoc % @STNO (codes NO))))
                          (recur (inc NO)))))))
;---------------------------------------------------------------------------------------------------
(defn                  RUN [at]
  (letfn [
    (skey [address]   (let [[no label] address] (if label label no)))
    (saddr [at]      (cond (keyword? at) [(@<STNO> at) at]
                           (string?  at) [(@<STNO> at) at]
                           (integer? at) [at (@<LABL> at)]))]
      (loop [      current (saddr at)]
        (if-let [      key (skey current)]
          (if-let [   stmt (@<CODE> key)]
            (let [   ferst (first stmt)
                    seqond (second stmt)
                      body (if (map? ferst) seqond ferst)
                      goto (if (map? ferst) ferst seqond)]
                           (if (EVAL! body)
                             (if (contains? goto :G)   (recur (saddr (:G goto)))
                               (if (contains? goto :S) (recur (saddr (:S goto)))
                                                       (recur (saddr (inc (current 0))))))
                             (if (contains? goto :G)   (recur (saddr (:G goto)))
                               (if (contains? goto :F) (recur (saddr (:F goto)))
                                                       (recur (saddr (inc (current 0)))))))))))))
;---------------------------------------------------------------------------------------------------
(defn -main "SNOBOL4/Clojure." [& args])