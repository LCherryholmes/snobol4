(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z])
  (:require [clojure.string :as string])
  (:require [clojure.pprint :as pp])
  (:require [clojure.edn :as edn])
  (:require [instaparse.core :as insta :refer [defparser]]); Clojure
; (:require [instaparse.core :as insta :refer-macros [defparser]]); ClojureScript
)

(def grammar "
  stmt      ::=  label? (<_> body? branch?)? <__>
  <body>    ::=  invoking | matching | replacing | assigning
	 invoking  ::=  subject
  matching  ::=  subject <_> pattern
  replacing ::=  subject <_> pattern <_ '='> replace
  assigning ::=  subject <_ '='> replace
  <subject> ::=  uop
  <pattern> ::=  (<'?' _>)? and
  <replace> ::=  (<_> expr)?
  <branch>  ::=  <__ ':' __> ( goto | succ (<__> fail)? | fail (<__> succ)? )
  goto      ::=  target
  succ      ::=  <'S'> target
  fail      ::=  <'F'> target
  <target>  ::=  <'('> expr <')'>
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
  <lst>     ::=  expr | expr (<','> expr)+
  label     ::=  #'[^\\s]+'
  <__>      ::=  <#'[ \\t]*'>
  <_>       ::=  <#'[ \\t]+'>
  N         ::=  #'[A-Za-z][A-Z_a-z0-9\\.\\-]*'
  I         ::=  #'[0-9]+'
  R         ::=  #'[0-9]+\\.[0-9]+'
  S         ::=  #'\"[^\"]*\"'  |  #\"'[^']*'\"
")

(defn bug [x] (println (type x) " " x) x)
(defn coder [ast]
  (insta/transform
    {
     	:stmt      (fn ([     ]               {                   })
     	               ([L    ]               { L []              })
     	               ([L B  ]               { L [B]             })
     	               ([L B [G1 L1]]         { L [B G1 L1]       })
     	               ([L B [G1 L1] [G2 L2]] { L [B G1 L1 G2 L2] }))
      :label     (fn  [L]     (keyword L))
     	:invoking  (fn  [S    ] S)
     	:matching  (fn  [S P  ] (list '? S P))
     	:replacing (fn ([S P  ] (list '? S P 'epsilon))
     	               ([S P R] (list '? S P R)))
     	:assigning (fn ([S    ] (list '= S 'epsilon))
     	               ([S R  ] (list '= S R)))
      :goto      (fn  [L]     [:G (keyword L)])
      :succ      (fn  [L]     [:S (keyword L)])
      :fail      (fn  [L]     [:F (keyword L)])
     	:expr      (fn  [x] x);-------------------------------
      :asn       (fn ([x] x) ([x    y] (list '= x y)))
      :mch       (fn ([x    ] x)
                     ([x y  ] (list '? x y))
                     ([x y z] (list '? x y z)))
      :and       (fn ([x] x) ([x    y] (list '& x y)))
      :alt       (fn ([x] x) ([x & ys] (apply vector '| x ys)))
      :cat       (fn ([x] x) ([x & ys] (apply vector    x ys)))
      :at        (fn ([x] x) ([x    y] (list 'at x y)))
      :sum       (fn ([x] x) ([x op y] (list (symbol op) x y)))
      :hsh       (fn ([x] x) ([x    y] (list 'hash x y)))
      :div       (fn ([x] x) ([x    y] (list '/ x y)))
      :mul       (fn ([x] x) ([x    y] (list '* x y)))
      :pct       (fn ([x] x) ([x    y] (list '% x y)))
      :xp        (fn ([x] x) ([x op y] (list (symbol op) x y)))
      :cap       (fn ([x] x) ([x op y] (list (symbol op) x y)))
      :ttl       (fn ([x] x) ([x    y] (list 'tilde x y)))
      :uop       (fn ([x] x) ([  op y] (list (symbol op) y)))
      :ndx       (fn ([n] n) ([n & xs] (apply list n xs)))
      :cnd       (fn ([x] x) ([x & ys] (apply vector 'comma x ys)))
      :inv       (fn          [F & xs] (apply list F xs))
      :N         (fn  [n] (symbol n))
      :I         edn/read-string
      :R         edn/read-string
      :S         edn/read-string
    } ast))

(def parse-statement (insta/parser grammar :start :stmt))
(def parse-expression (insta/parser grammar :start :expr))
(def SNO [
  ""
  " "
  "L"
  "L "
  "L X = 10 :(G)"
  "L S P :S(S)"
  "L E = (1 + 2) * 10 :F(F)"
  "L :S(S)F(F)"
  " S"
  " S "
  "L S"
  "L S ="
  "L S = E"
  "L S P"
  "L S P ="
  "L S P = R :F(F)S(S)"
  "L (S ? P = R) :F(F)S(S)"
])

(defn doit []
		(doseq [S SNO]
		  (let [ast (parse-statement S) code (coder ast)]
		    (println (format "%-30s " S) code))))

(defn -main "SNOBOL4 statement parser." [& args] (doit))
