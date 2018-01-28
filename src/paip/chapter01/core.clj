(ns paip.chapter01.core
  (:require [clojure.math.numeric-tower :refer [expt]]))

;;;; 1. Introduction to Lisp

;; Exercise 1.2
(defn power [x n]
  (cond (zero? n) 1
        (even? n) (expt (power x (/ n 2)) 2)
        :else (* x (power x (dec n)))))
