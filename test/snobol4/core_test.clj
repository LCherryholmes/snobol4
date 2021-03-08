(ns snobol4.core-test
  (:require [clojure.test :refer :all]
            [snobol4.core :refer :all]))


(is (= 4 (+ 2 2)))
(is (thrown? ArithmeticException (/ 1 0)))
(is (thrown-with-msg? ArithmeticException #"Divide by zero" (/ 1 0)))


(def SNO [
   ""
   "0"
   "1"
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
;  "
;(SPAN(digits)
;  ('.' FENCE(SPAN(digits) | epsilon) | epsilon)
;  ('E' | 'e')
;  ('+' | '-' | epsilon)
;  SPAN(digits)
;| SPAN(digits) '.' FENCE(SPAN(digits) | epsilon)
;)"
])



(deftest christmas-tree
   (is (= "" ))
)

(deftest compiling-expression
  (testing "FIXME, I fail." (is (= 0 1)))
)

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

; :partial :true
; :start :rule-name
; :total true(def expression-parser
;(insta/parser (clojure.java.io/resource "myparser.bnf"))
;(defparser p "S = 1*'a'" :input-format :abnf :output-format :enlive)
;(time (def p (insta/parser "S = A B; A = 'a'+; B = 'b'+")))
;(time (defparser p         "S = A B; A = 'a'+; B = 'b'+"))
;(def ambiguous (insta/parser "S = A A; A = 'a'*;"))
;(println (insta/parse ambiguous "aaaaaa"))
;(println (insta/parses ambiguous "aaaaaa"))

(slurp "/tmp/test.txt")
(use 'clojure.java.io)
(with-open [rdr (reader "/tmp/test.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
(with-open [wrtr (writer "/tmp/test.txt")]
  (.write wrtr "Line to be written"))
(spit "/tmp/test.txt" "Line to be written")
(use 'clojure.java.io)
(with-open [wrtr (writer "/tmp/test.txt" :append true)]
  (.write wrtr "Line to be appended"))
(spit "/tmp/test.txt" "Line to be written" :append true)
(reader (file "/tmp/test.txt"))
(writer (file "tmp/test.txt"))
(System/getProperty "user.dir")
(def directory (clojure.java.io/file "/path/to/directory"))
(def files (file-seq directory))
(take 10 files)
