(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z])
  (:require [clojure.string :as string])
  (:require [clojure.pprint :as pp])
  (:require [clojure.edn :as edn])
  (:require [instaparse.core :as insta :refer [defparser]]); Clojure
; (:require [instaparse.core :as insta :refer-macros [defparser]]); ClojureScript
)

;(insta/parser (clojure.java.io/resource "myparser.bnf"))
(defparser p "S = 1*'a'" :input-format :abnf :output-format :enlive)
(time (def p (insta/parser "S = A B; A = 'a'+; B = 'b'+")))
(time (defparser p         "S = A B; A = 'a'+; B = 'b'+"))
(def ambiguous (insta/parser "S = A A; A = 'a'*;"))
(println (insta/parse ambiguous "aaaaaa"))
(println (insta/parses ambiguous "aaaaaa"))

(def SNO [
;  ""
;  "0"
;  "1"
;  "0.5"
;  "''"
;  "'a'"
;  "'ab'"
;  "'abc'"
   "A"
   "A[0]"
   "A[x]"
   "A[x, y]"
   "A[x, y][z]"
   "S"
   "S T"
   "S T U"
   "S T U V"
   "S T U V W"
   "N"
   "N | O"
   "N | O | P"
   "N | O | P | Q"
   "N | O | P | Q | R"
   "A B | R A | C A | D A | B R A"
;  "S = E"
;  "S ? P"
;  "S ? P = E"
;  "F()"
   "F(x)"
   "F(x y)"
   "F(x, y)"
   "F(x y z)"
   "F(x, y z)"
   "F(x, y, z)"
   "F(w, x, y, z)"
   "F(w, x y, z)"
   "F(w, x, y z)"
   "F(w x, y z)"
   "F(w, x y z)"
   "F(w x y z)"
;  "F (x)"
;  "F (x, y)"
;  "F (x, y, z)"
;  "F (x, y, z, p1, p2, p3)"
;  "F G(x)"
   "F(x) G(y)"
   "F(x, y) G(z)"
   "F(x, y, z, p1, p2, p3) G(z)"
   "SPAN(digits)"
   "
(SPAN(digits)
  ('.' FENCE(SPAN(digits) | epsilon) | epsilon)
  ('E' | 'e')
  ('+' | '-' | epsilon)
  SPAN(digits)
| SPAN(digits) '.' FENCE(SPAN(digits) | epsilon)
)"
])

(def snobol4-compile
  (insta/parser "
     expr ::= sno ;
				<sno> ::= <__> asn <__> ;
			 	 asn ::= mch | mch  <_  '='  _>  asn ;
 				 mch ::= and | mch  <_  '?'  _>  and ;
 				 and ::= alt | and  <_  '&'  _>  alt ;
 				 alt ::= cat | cat (<_  '|'  _>  cat)+ ;
 				 cat ::= at  | at  (<_>          at)+ ;
 				 at  ::= sum | at   <_  '@'  _>  sum ;
 				 sum ::= hsh | sum  <_> '+' <_>  hsh
				              | sum  <_> '-' <_>  hsh ;
	 			 hsh ::= div | hsh  <_  '#'  _>  div ;
	 			 div ::= mul | div  <_  '/'  _>  mul ;
	 			 mul ::= pct | mul  <_  '*'  _>  pct ;
	 			 pct ::= xp  | pct  <_  '%'  _>  xp ;
	 			 xp  ::= cap | cap  <_> '^' <_>  xp
				              | cap  <_> '!' <_>  xp
				              | cap  <_> '**' <_> xp ;
	 			 cap ::= ttl | ttl  <_> '$' <_>  cap
				              | ttl  <_> '.' <_>  cap ;
	 			 ttl ::= uop | ttl  <_  '~'  _>  uop ;
	 			 uop ::= ndx | '@' uop | '~' uop | '?' uop | '&' uop | '+' uop
  				            | '-' uop | '*' uop | '$' uop | '.' uop | '!' uop
		  		            | '%' uop | '/' uop | '#' uop | '=' uop | '|' uop ;
	 			 ndx ::= itm | ndx <'<'> lst <'>'> | ndx <'['> lst <']'> ;
				<itm> ::= I | R | S | N | grp | cnd | inv
    <grp> ::= <'('> sno <')'>
      cnd ::= <'('> sno <','> lst <')'> ;
      inv ::= F <'('       ')'>
            | F <'('> lst <')'>
 		 <lst> ::= sno | lst <','> sno ;
 		  <__> ::= <#'[ \\t\\n]*'> ;
		 	  <_> ::= <#'[ \\t\\n]+'> ;
 		    I  ::= #'[0-9]+' ;
 			   R  ::= #'[0-9]+\\.[0-9]+' ;
 	 	   S  ::= #'\"[^\"]*\"' | #\"'[^']*'\" ;
  		   F  ::= #'[A-Za-z][A-Z_a-z0-9\\.\\-]*' ;
 			   N  ::= #'[A-Za-z][A-Z_a-z0-9\\.\\-]*' ;
")) ;  :partial :true :start :rule-name  :total true

(defn bug [x] (println (type x) " " x) x)
(defn doit []
		(doseq [S SNO]
		  (def ast (snobol4-compile S))
		; (pp/pprint ast)
		  (def code
				  (insta/transform
				    { :expr (fn  [x] (if-not (list? x) x
				    	                  (if (<= (count x) 1) x x)))
				    ,	:sno  (fn  [x] x)
				    , :asn  (fn ([x] x) ([x y]    ['=           x y]))
				    , :mch  (fn ([x] x) ([x y]    ['?           x y]))
				    , :and  (fn ([x] x) ([x y]    ['&           x y]))
				    , :alt  (fn ([x] x) ([x & ys] (apply vector '| x ys)))
				    , :cat  (fn ([x] x) ([x & ys] (apply vector x ys)))
				    , :at   (fn ([x] x) ([x y]    [:at          x y]))
				    , :sum  (fn ([x] x) ([x op y] [(symbol op)  x y]))
				    , :hsh  (fn ([x] x) ([x y]    [:hash        x y]))
				    , :div  (fn ([x] x) ([x y]    ['/           x y]))
				    , :mul  (fn ([x] x) ([x y]    ['*           x y]))
				    , :pct  (fn ([x] x) ([x y]    ['%           x y]))
				    , :xp   (fn ([x] x) ([x op y] ['**          x y]))
				    , :cap  (fn ([x] x) ([x op y] [(symbol op)  x y]))
				    , :ttl  (fn ([x] x) ([x y]    [:tilde       x y]))
				    , :uop  (fn ([x] x) ([op x]   [(symbol op)  x  ]))
				    , :ndx  (fn ([n] n)
				                ([n x] (list n x))
				                ([n x y] (list n x y))
				                ([n x y z] (list n x y z)))
				    , :cnd  (fn ([x] x)
				                ([x & ys] (apply vector :condition x ys))); \,
				    , :inv  (fn ([F] (list F))
				                ([F & parms]
				                  (apply list
				                    (reduce (fn [args xs]
		                        (reduce (fn [arg x]
		                          (conj arg x))
		                          args
		                          xs))
		                        [F]
		                        (if (vector? parms) parms (vector parms))))))
				    , :I    edn/read-string
				    , :R    edn/read-string
				    , :S    edn/read-string
				    , :F    (fn  [n] (symbol n)); 'x
				    , :N    (fn  [n] (symbol n)); 'x
				    } ast); :number (comp clojure.edn/read-string str)
		  )
		  (println (format "%-30s " S) code)
))

(defn -main "SNOBOL4 statement parser." [& args] (doit))
(-main)