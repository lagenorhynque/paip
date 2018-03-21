(ns paip.chapter01.core
  (:require [clojure.math.numeric-tower :refer [expt]]))

;;;; 1. Introduction to Lisp

(+ 2 2)

(+ 1 2 3 4 5 6 7 8 9 10)

(- (+ 9000 900 90 9) (+ 5000 500 50 5))

;;; 1.1

(concat '(Pat Kim) '(Robin Sandy))

'(Pat Kim)

'John
'(John Q Public)
'2
2
'(+ 2 2)
(+ 2 2)
;; John
;; (John Q Public)

(concat '(Pat Kim) (list '(John Q Public) 'Sandy))
(count (concat '(Pat Kim) (list '(John Q Public) 'Sandy)))

;;; 1.2

(def p '(John Q Public))
p
(def x 10)
(+ x x)
(+ x (count p))

;;; 1.3

;;; 1.4

p
(first p)
(rest p)
(second p)
(first (nnext p))
(fnext (nnext p))
(count p)

(def x' '((first element) 2 (element 3) ((4)) 5))
(count x')
(first x')
(second x')
(first (nnext x'))
(fnext (nnext x'))
(first (fnext (nnext x')))
(first (nnext (nnext x')))
(first x')
(second (first x'))

p
(cons 'Mr p)
(cons (first p) (rest p))
(def town (list 'Anytown 'USA))
(list p 'of town 'may 'have 'already 'won!)
(concat p '(of) town '(may have already won!))
p

(last p)

;;; 1.5

(defn last-name
  "Select the last name from a name represented as a list."
  [name]
  (last name))

(last-name p)
(last-name '(Rear Admiral Grace Murray Hopper))
(last-name '(Rex Morgan MD))
(last-name '(Spot))
(last-name '(Aristotle))

(defn first-name
  "Select the first name from a name represented as a list."
  [name]
  (first name))

p
(first-name p)
(first-name '(Wilma Flintstone))
(def names '((John Q Public) (Malcolm X)
                             (Admiral Grace Murray Hopper) (Spot)
                             (Aristotle) (A A Milne) (Z Z Top)
                             (Sir Larry Olivier) (Miss Scarlet)))
(first-name (first names))

;;; 1.6

(map last-name names)

(map - '(1 2 3 4))
(map + '(1 2 3 4) '(10 20 30 40))

(map first-name names)

(def titles
  "A list of titles that can appear at the start of a name."
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General))

(defn first-name'
  "Select the first name from a name represented as a list."
  [name]
  (if (some #(= % (first name)) titles)
    (first-name' (rest name))
    (first name)))

(map first-name' names)
(first-name' '(Madam Major General Paula Jones))

;;; 1.7

(defn mappend
  "Apply fn to each element of list and append the results."
  [f coll]
  ;; (apply concat (map f coll))
  (mapcat f coll))

(apply + '(1 2 3 4))

(apply concat '((1 2 3) (a b c)))

(defn self-and-double [x] (list x (+ x x)))
(self-and-double 3)
(apply self-and-double '(3))

(map self-and-double '(1 10 300))
(mappend self-and-double '(1 10 300))

(defn number-and-negation
  "If x is a number, return a list of x and -x."
  [x]
  (when (number? x)
    (list x (- x))))

(defn numbers-and-negations
  "Given a list, return only the numbers and their negations."
  [input]
  (mappend number-and-negation input))

(numbers-and-negations '(testing 1 2 3 test))

(defn mappend'
  "Apply fn to each element of list and append the results."
  [f coll]
  (if (empty? coll)
    ()
    (concat (f (first coll))
            (mappend' f (rest coll)))))

(+ 2 3)
(apply + '(2 3))
;; (+ '(2 3))

((fn [x] (+ x 2)) 4)

(map (fn [x] (+ x x))
     '(1 2 3 4 5))
(mappend (fn [l] (list l (reverse l)))
         '((1 2 3) (a b c)))

;;; 1.8

"a string"
(count "a string")
(count "")

;;; 1.9

'John
(def p' 'John)
(defn twice [x] (+ x x))
(if (= 2 3) (throw (RuntimeException.)) (+ 5 6))

(+ 2 3)
(- (+ 90 9) (+ 50 5 (count '(Pat Kim))))

42
-273.15
"a string"

(+ (* 3 4) (* 5 6))

;;; 1.10

;; Exercise 1.1
(def suffixes
  '(JD MD DO PharmD Sr. Jr.))

(defn last-name'
  "Select the last name from a name represented as a list."
  [name]
  (let [lname (last name)]
    (if (some #(= % lname) suffixes)
      (last-name' (butlast name))
      lname)))

;; Exercise 1.2
(defn power [x n]
  (cond
    (zero? n) 1
    (even? n) (expt (power x (/ n 2)) 2)
    :else (* x (power x (dec n)))))

;; Exercise 1.3
(def atom? (complement sequential?))

(defn count-atoms [expr]
  (cond
    (atom? expr) 1
    (empty? expr) 0
    :else (+ (count-atoms (first expr))
             (count-atoms (rest expr)))))

;; Exercise 1.4
(defn count-anywhere [x expr]
  (cond
    (= x expr) 1
    (atom? expr) 0
    :else (+ (count-anywhere x (first expr))
             (count-anywhere x (next expr)))))

;; Exercise 1.5
(defn dot-product [xs ys]
  (apply + (map * xs ys)))
