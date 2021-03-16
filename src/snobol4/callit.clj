(defn reference [N]
  (if-let [ns-name (namespace N)]
    (when-let [ns-ref (or (get (ns-aliases *ns*) (symbol ns-name))
                          (find-ns (symbol ns-name)))]
      (get (ns-publics ns-ref) (symbol (name N))))
    (get (ns-map *ns*) (symbol (name N)))))

(defn $$ [N] (if-let [V (reference N)] (var-get V) "")); (var-get (eval (list 'var N)))

(defmacro vectorize [x]      `(if (vector? ~x) ~x (vector ~x)))
(defn flatten-1     [Ω]       (mapcat #(if (sequential? %) % [%]) Ω))
(defn flatten-one   [op & Ω]  (apply list (reduce
                                (fn [Φ ω] (reduce
                                  (fn [Σ σ] (conj Σ σ))
                                  Φ ω))
                                [op] (vectorize Ω))))

(defn ALT [subj pos & patterns] (println "ALT" subj pos patterns))
(defn SEQ [subj pos & patterns] (println "SEQ" subj pos patterns))

(def pattern '(SEQ "Literal" (ALT "BE" "BO" "B") (ALT "AR" "A") (ALT "DS" "D")))
(def op (first pattern))
(def args (rest pattern))

;(SEQ "S" 1 args)
;(apply SEQ "S" 2 args)
;(apply ($$ 'SEQ) "S" 3 args)
;(eval (list 'SEQ "S" 4 args))
;(eval (list 'SEQ "S" 5 (quote args)))
;(println (flatten-1 args))
;(println (flatten-one pattern))
;(println (flatten-1 (list 'SEQ "S" 6 args)))

(println (conj (map #(list 'quote %1) (rest pattern)) 7 "S" (first pattern)))
(println (reverse	(reduce #(conj %1 (list 'quote %2)) (list 8 "S" 'SEQ) args)))
(println (apply list (reduce #(conj %1 (list 'quote %2)) ['SEQ "S" 9] args)))

(apply SEQ "S" 10 args)
(apply SEQ "S" 11 (map #(list 'quote %1) args))
(apply ($$ 'SEQ) "S" 12 (map #(list 'quote %1) args))
