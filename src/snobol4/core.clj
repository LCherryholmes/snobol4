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
  stmt      ::=  label? (<_> body? <__> branch?)? <__> <eos?>
  <body>    ::=  invoking | matching | replacing | assigning
  invoking  ::=  subject
  matching  ::=  subject <_> pattern
  replacing ::=  subject <_> pattern <_ '='> replace
  assigning ::=  subject <_ '='> replace
  <subject> ::=  uop
  <pattern> ::=  (<'?' _>)? and
  <replace> ::=  (<_> expr)?
  branch    ::=  <__ ':' __> ( goto | sgoto (<__> fgoto)? | fgoto (<__> sgoto)? )
  goto      ::=  target
  sgoto     ::=  <'S'> target
  fgoto     ::=  <'F'> target
  <target>  ::=  <'('> expr <')'>
  comment   ::=  <'*'> #'.*' <eol>
  control   ::=  <'-'> #'[^;\\n]*' <eos>
  eos       ::=  '\\n' | ';'
  eol       ::=  '\\n'

  <expr>    ::=  <__> asn <__>
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
  label     ::=  #'[^ \\t\\r\\n+-.*][^ \\t\\r\\n]*'
  white     ::=  #'[ \\t]'  (*(' ' | '\\t')+ | '\\n+' | '\\n.'*)
  <_>       ::=  <white+>
  <__>      ::=  <white*>
  I         ::=  #'[0-9]+'
  R         ::=  #'[0-9]+\\.[0-9]+'
  S         ::=  #'\"([^\"]|\\x3B)*\"'
              |  #'\\'([^\\']|\\x3B)*\\''
  N         ::=  #'[A-Za-z][A-Z_a-z0-9\\.\\-]*'
"))

(defn coder [ast]
; (pp/pprint ast)
  (insta/transform
    { :comment   (fn comment     [cmt]              [:comment cmt])
      :control   (fn control     [ctl]              [:control ctl])
      ;--------------------------------------------------------------------------
      :stmt      (fn stmt        [& parts]          (apply vector parts)
                              ; ([     ]            {                     })
                              ; ([L    ]            { L []                })
                              ; ([L B  ]            { L [B]               })
                              ; ([L B [G1 L1]]      { L [B {G1 L1}]       })
                              ; ([L B [G1 L1]
                              ;       [G2 L2]]      { L [B {G1 L1 G2 L2}] })
                 )
      :label     (fn label       [L]                (keyword L))
      :invoking  (fn invoking    [S    ]            S)
      :matching  (fn matching    [S P  ]            (list '? S P))
      :replacing (fn replacing  ([S P  ]            (list '? S P 'epsilon))
                                ([S P R]            (list '? S P R)))
      :assigning (fn assigning  ([S    ]            (list '= S 'epsilon))
                                ([S R  ]            (list '= S R)))
      :branch    (fn branch      [& gs]             (reduce
                                                      #(assoc %1
                                                        (first %2)
                                                        (second %2)) {} gs))
      :goto      (fn goto        [L]                [:G (keyword L)])
      :sgoto     (fn sgoto       [L]                [:S (keyword L)])
      :fgoto     (fn fgoto       [L]                [:F (keyword L)])
      :expr      (fn expr        [x] x);----------------------------------------
      :asn       (fn asn        ([x] x) ([x     y]  (list '= x y)))
      :mch       (fn mch        ([x] x) ([x  y   ]  (list '? x y))
                                        ([x  y  z]  (list '? x y z)))
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
      :uop       (fn uop        ([x] x) ([   op y]  (list (symbol op) y)))
      :ndx       (fn ndx        ([n] n) ([n  & xs]  (apply list n xs)))
      :cnd       (fn cnd        ([x] x) ([x  & ys]  (apply vector 'comma x ys)))
      :inv       (fn inv         [f  & xs]          (apply list f xs))
      :N         (fn N           [n]                (symbol n))
      :I         edn/read-string
      :R         edn/read-string
      :S         (fn S           [s]                (subs s 1 (- (count s) 1)))
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
(def dirs ["./src/sno" "./src/inc" "./src/test"])
(def SNO [])
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
		              true (let [stmt0 (string/replace cmd #"[ \t]*\r\n[+.][ \t]*" " ")
		                         stmt1 (string/replace stmt0 #"\r\n$" "")
		                         ast (parse-statement stmt1)
		                         code (coder ast)]
		                     (when (map? code)
		                       (let [line   (:line code)
		                             column (:column code)
		                             text   (:text code)]
		                         (println line " " column " " stmt1)
                         ; (pp/pprint stmt1)
		    		                 ; (pp/pprint code)
		    		               ))
		  		               )
		  		        ))))
      3 (with-open [rdr (io/reader filenm)]
          (doseq [line (line-seq rdr)]
            (let [ast (parse-command line) code (coder ast)]
              (println code))
          ))
    )))

(defn -main "SNOBOL4 statement parser." [& args] (doit))