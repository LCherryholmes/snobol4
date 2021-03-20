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

(deftest match
  (match-1) (match-2) (match-3) (match-4))

(defn test-ns-hook [] (match-3) (match-4))