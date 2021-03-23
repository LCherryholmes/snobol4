(ns snobol4.core-test
  (:require [clojure.test :refer :all]
            [snobol4.core :refer :all :exclude [= + - * / num]]))

;(use-fixtures :each fixture1 fixture2 ...); :once

(deftest match-1
  (let [ROOT "car"
         PAT '[ROOT (| "s" "es" "")]]
      (is (EVAL '(? "car" PAT)))
      (is (EVAL '(? "cars" PAT)))
      (is (EVAL '(? "cares" PAT)))
  ))

(deftest match-2
  (let [N 1
        P '(| [(EQ N 1) "fox"]
              [(EQ N 2) "wolf"])]
    (is      (EVAL '(? "fox" P)))
    (is (not (EVAL '(? "wolf" P))))))

(deftest match-3
  (let [BD (EVAL '[(POS 0) (| "BE" "BO" "B") (| "AR" "A") (| "DS" "D") (RPOS 0)])]
    (is (? "BEARDS" BD))
    (is (? "BEARD"  BD))
    (is (? "BEADS"  BD))
    (is (? "BEAD"   BD))
    (is (? "BARDS"  BD))
    (is (? "BARD"   BD))
    (is (? "BADS"   BD))
    (is (? "BAD"    BD))
    (is (not (? "BATS"   BD)))))

(deftest match-4
  (let [BR (EVAL '[(POS 0) (| "B" "F" "L" "R") (| "E" "EA") (| "D" "DS") (RPOS 0)])]
    (is (? "BED"    BR))
    (is (? "BEDS"   BR))
    (is (? "BEAD"   BR))
    (is (? "BEADS"  BR))
    (is (? "RED"    BR))
    (is (? "REDS"   BR))
    (is (? "READ"   BR))
    (is (? "READS"  BR))
    (is (? "READS"  BR))
    (is (? "LEAD"   BR))
    (is (not (? "LEADER" BR)))))

(deftest match-5
  (let [A (EVAL '[(POS 0) (ANY "BFLR") (SPAN "EA") "D" (RPOS 0)])]
    (is (? "BED"    A))
    (is (? "FAD"    A))
    (is (? "LEED"   A))
    (is (? "READ"   A))
    (is (? "RAD"    A))
    (is (not (? "IED" A)))
    (is (not (? "JED" A)))
    (is (not (? "BID" A)))))

(deftest match-real
  (def digits "0123456789")
  (def epsilon "")
  (let [real (EVAL (str
         "POS(0)"
        " SPAN(digits)"
        " (  ('.' FENCE(SPAN(digits) | epsilon) | epsilon)"
        "    ('E' | 'e')"
        "    ('+' | '-' | epsilon)"
        "    SPAN(digits)"
        " |  '.' FENCE(SPAN(digits) | epsilon)"
         ")"
        " RPOS(0)"))]
  ; (is (? "1." real))
  ; (is (? "1.6" real))
  ; (is (? "1.61" real))
  ; (is (? "1.6E2" real))
  ; (is (? "1.6e-1" real))
  ; (is (? "1.61e+2" real))
  ; (is (? "1.618e+3" real))
    (is (? "1.618e+10" real))
  ; (is (not (? "1" real)))
  ; (is (not (? "1.6E" real)))
  ; (is (not (? "1.6e" real)))
  ; (is (not (? "1.6E-" real)))
  ; (is (not (? "1.6e-" real)))
  ; (is (not (? "1.6E+" real)))
  ; (is (not (? "1.6e+" real)))
  ))

(deftest match-define
  (def digits "0123456789")
; (def &LCASE "abcdefghijklmnopqrstuvwxyz")
; (def &UCASE "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (def epsilon "")
  (let [identifier (EVAL (str
         "POS(0)"
        " ANY(&UCASE &LCASE)"
        " FENCE(SPAN(digits '-.' &UCASE '_' &LCASE) | epsilon)"
        " RPOS(0)"))]
    (is (? "" identifier))
    (is (? "v" identifier))
    (is (? "id" identifier))
    (is (? "ID19" identifier))
    (is (? "match-define" identifier))
    (is (? "v_pat.name" identifier))
  ))

;  "DATA('tree(t,v,n,c)')"
(deftest datatype-test
  (is (= (DATATYPE "no") "STRING"))
  (is (= (DATATYPE 2019) "INTEGER"))
  (is (= (DATATYPE 1.62) "REAL"))
  (is (= (DATATYPE (ARRAY "0:9")) "ARRAY"))
  (is (= (DATATYPE (TABLE)) "TABLE"))
; (is (= (DATATYPE (LEN 10)) "PATTERN"))
; (is (= (DATATYPE (POS 0))  "PATTERN"))
; (is (= (DATATYPE (RPOS 0)) "PATTERN"))
  (is (= (DATATYPE [:LEN 10]) "PATTERN"))
  (is (= (DATATYPE ["Hello, " (SPAN @&LCASE)]) "PATTERN"))
  (is (= (DATATYPE 'epsilon) "NAME"))
  (is (= (DATATYPE (snobol4.core.NAME. 'epsilon)) "NAME"))
  (is (= (DATATYPE (list '* (list 'EQ 0 0))) "EXPRESSION"))
  (is (= (DATATYPE '(+ 1 2)) "EXPRESSION"))
  (is (= (DATATYPE (CODE "what year = 2021")) "CODE"))
  (is (= (DATATYPE #{}) "SET"))
  (is (= (DATATYPE (SET)) "SET"))
  (is (= (DATATYPE #"^[A-Z]+$") "REGEX"))
  (is (= (DATA "tree(t,v,n,c)") ε))
; (ns *ns* (:import [snobol4.core tree]))
; (is (= (DATATYPE (tree. \+ nil 2 [1 2])) "tree"))
; (is (= (DATATYPE (->tree \+ nil 2 [1 2])) "tree"))
; (is (= (DATATYPE (new tree \+ nil 2 [1 2])) "tree"))
)

(deftest match
  (match-1) (match-2) (match-3) (match-4)
  (match-5) (match-real) (match-define)
  (datatype-test))

(defn test-ns-hook [] (datatype-test))