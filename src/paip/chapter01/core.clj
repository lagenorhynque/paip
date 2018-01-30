(ns paip.chapter01.core
  (:require [clojure.math.numeric-tower :refer [expt]]))

;;;; 1. Introduction to Lisp

(defn last-name
  "Select the last name from a name represented as a list."
  [name]
  (last name))

(defn first-name
  "Select the first name from a name represented as a list."
  [name]
  (first name))

(def titles
  "A list of titles that can appear at the start of a name."
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General))

(defn first-name'
  "Select the first name from a name represented as a list."
  [name]
  (if (some #(= % (first name)) titles)
    (first-name' (rest name))
    (first name)))

(defn mappend
  "Apply fn to each element of list and append the results."
  [f coll]
  (apply concat (map f coll)))

(defn self-and-double [x]
  (list x (+ x x)))

(defn number-and-negation
  "If x is a number, return a list of x and -x."
  [x]
  (when (number? x)
    (list x (- x))))

(defn numbers-and-negations
  "Given a list, return only the numbers and their negations."
  [input]
  (mappend number-and-negation input))

(defn mappend'
  "Apply fn to each element of list and append the results."
  [f coll]
  (if (empty? coll)
    ()
    (concat (f (first coll))
            (mappend' f (rest coll)))))

;; Exercise 1.1
(def suffixes
  '(JD MD DO PharmD Sr. Jr.))

(defn last-name'
  "Select the last name from a name represented as a list."
  [name]
  (if (some #(= % (last name)) suffixes)
    (last-name' (butlast name))
    (last name)))

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
