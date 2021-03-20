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

(deftest match (match-1) (match-2))

(defn test-ns-hook []
  (match))