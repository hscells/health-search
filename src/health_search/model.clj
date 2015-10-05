(ns health-search.model
  (:require [clojure.edn :as edn]))

(defn prob
  "calculate the probability of n gram x in terms n"
  ([x n]
  (float (/ (count (filter #{x} n)) (count n))))
  ([x y n]
  (float (/ (+ (count (filter #{x} n)) (count (filter #{y} n))) (count n)))))

(defn pmi
  "implementation of Pointwise Mututal Information"
  [a b n]
  (cond
    (zero? (* (prob a n) (prob b n))) 0
    :else
    (Math/log (/ (prob a b n) (* (prob a n) (prob b n))))))

(defn emim
  "implementation of Expected Mutual Information Measure"
  [a b n]
  (* (prob a b n) (pmi a b n)))

(def inputs
  "read the properties from the model.edn file and make them available in the program"
  (edn/read-string (slurp "model.edn")))
