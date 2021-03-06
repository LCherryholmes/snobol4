(ns snobol4.core
  (:gen-class)
  (:require [clojure.zip :as z])
  (:require [clojure.string :as string])
  (:require [clojure.pprint :as pp])
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
   "S"
   "S = E"
   "S ? P"
   "S ? P = E"
   "F(X)"
   "F (X)"
   "FN(X, Y)"
   "SPAN(digits)"
   "(SPAN(digits)
      ('.' FENCE(SPAN(digits) | epsilon) | epsilon)
      ('E' | 'e')
      ('+' | '-' | epsilon)
      SPAN(digits)
    | SPAN(digits) '.' FENCE(SPAN(digits) | epsilon)
    )"
])

(def snobol4-compile
  (insta/parser "
				<sno> ::= __ asn __ ;
				 asn  ::= mch | mch _ '=' _ asn ;
				 mch  ::= and | mch _ '?' _ and ;
				 and  ::= or  | and _ '&' _ or ;
				 or   ::= cat | cat _ '|' _ or ;
				 cat  ::= at  | at     _    cat ;
				 at   ::= sum | at  _ '@' _ sum ;
				 sum  ::= hsh | sum _ '+' _ hsh | sum _ '-' _ hsh ;
				 hsh  ::= div | hsh _ '#' _ div ;
				 div  ::= mul | div _ '/' _ mul ;
				 mul  ::= pct | mul _ '*' _ pct ;
				 pct  ::= xp  | pct _ '%' _ xp ;
				 xp   ::= cap | cap _ '^' _ xp  | cap _ '!' _ xp | cap _ '**' _ xp ;
				 cap  ::= ttl | ttl _ '$' _ cap | ttl _ '.' _ cap ;
				 ttl  ::= uop | ttl _ '~' _ uop ;
				 uop  ::= ndx | '@' uop | '~' uop | '?' uop | '&' uop | '+' uop
  				            | '-' uop | '*' uop | '$' uop | '.' uop | '!' uop
		  		            | '%' uop | '/' uop | '#' uop | '=' uop | '|' uop ;
				 ndx  ::= itm | ndx <'<'> lst <'>'> | ndx <'['> lst <']'> ;
				<itm> ::= I | R | S | N | grp | cnd | inv
    <grp> ::= <'('> sno <')'>
     cnd  ::= <'('> sno ',' lst <')'> ;
     inv  ::= F <'('> lst <')'>
				<lst> ::= sno | lst ',' sno ;
		   <__> ::= <#'[ \\t\\n]*'> ;
		 	  <_> ::= <#'[ \\t\\n]+'> ;
 		   <I> ::= #'[0-9]+' ;
 			  <R> ::= #'[0-9]+\\.[0-9]+' ;
 			  <N> ::= #'[A-Za-z][A-Z_a-z\\.\\-]*' ;
  		  <F> ::= #'[A-Za-z][A-Z_a-z\\.\\-]*' ;
 	 	  <S> ::= #'\"[^\"]*\"' | #\"'[^']*'\" ;
" :total true)) ; :partial :true :start :rule-name

(defn bug [x] (println x) x)
(doseq [S SNO]
  (println S)
  (def ast (snobol4-compile S))
  (pp/pprint ast)
; (def code
		; (insta/transform
		;   { :asn  (fn ([x] (bug x)) ([x y] (list '=  (bug x) (bug y))))
		;   , :mch  (fn ([x] (bug x)) ([x y] (list '?  (bug x) (bug y))))
		;   , :and  (fn ([x] (bug x)) ([x y] (list '&  (bug x) (bug y))))
		;   , :or   (fn ([x] (bug x)) ([x y] (list '|  (bug x) (bug y))))
		;   , :at   (fn ([x] (bug x)) ([x y] (list \@  (bug x) (bug y))))
		;   , :sum  (fn ([x] (bug x)) ([x y] (list '+  (bug x) (bug y))))
		;   , :hsh  (fn ([x] (bug x)) ([x y] (list \#  (bug x) (bug y))))
		;   , :div  (fn ([x] (bug x)) ([x y] (list '/  (bug x) (bug y))))
		;   , :mul  (fn ([x] (bug x)) ([x y] (list '*  (bug x) (bug y))))
		;   , :pct  (fn ([x] (bug x)) ([x y] (list '%  (bug x) (bug y))))
		;   , :xp   (fn ([x] (bug x)) ([x y] (list '** (bug x) (bug y))))
		;   , :cap  (fn ([x] (bug x)) ([x y] (list '.  (bug x) (bug y))))
		;   , :ttl  (fn ([x] (bug x)) ([x y] (list '~  (bug x) (bug y))))
		;   , :ndx  (fn ([x] (bug x)) ([x y] (list '[] (bug x) (bug y))))
		;   , :cnd  (fn ([x] (bug x)) ([x y] (list \,  (bug x) (bug y))))
		;   , :inv  (fn ([x] (bug x)) ([x y] (list 'x  (bug y))))
		;   , :uop  (fn ([x] (bug x)) ([x y] (list 'x  (bug y))))
		;   , :cat  (fn ([x] (bug x)) ([x y] (list (bug x) (bug y))))
		;   } ast); :number (comp clojure.edn/read-string str)
; )
; (pp/pprint code)
)

(defn -main
  "SNOBOL4 statement parser."
  [& args]
  (println "SNOBOL4 statement parser."))
