(ns paip.chapter01.core
  (:require [clojure.math.numeric-tower :refer [expt]]))

;;;; 1. Introduction to Lisp

;; Exercise 1.1
;; TODO

;; Exercise 1.2
(defn power [x n]
  (cond (zero? n) 1
        (even? n) (expt (power x (/ n 2)) 2)
        :else (* x (power x (dec n)))))

;; Exercise 1.3
(def atom? (complement sequential?))

(defn count-atoms [expr]
  (cond (atom? expr) 1
        (empty? expr) 0
        :else (+ (count-atoms (first expr))
                 (count-atoms (rest expr)))))

;; Exercise 1.4
(defn count-anywhere [x expr]
  (cond (= x expr) 1
        (atom? expr) 0
        :else (+ (count-anywhere x (first expr))
                 (count-anywhere x (next expr)))))

;; Exercise 1.5
(defn dot-product [xs ys]
  (apply + (map * xs ys)))
