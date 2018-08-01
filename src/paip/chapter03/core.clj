(ns paip.chapter03.core
  (:require [clojure.math.numeric-tower :refer [abs expt round sqrt]]))

;;;; 3. Overview of Lisp

;;; 3.1

;;; 3.2

;; (defn 関数名 "任意のドキュメンテーション文字列" [パラメータ...] 本体...)
;; (defmacro マクロ名 "任意のドキュメンテーション文字列" [パラメータ...] 本体...)

;; (defonce 変数名 値)
;; (def 変数名 "任意のドキュメンテーション文字列" 値)
;; (def ^:const 変数名 "任意のドキュメンテーション文字列" 値)

;; (defrecord レコード名 [フィールド...])

(defrecord Name [first middle last])
(defn make-name [& {:keys [first middle last]
                    :or {middle nil}}]
  (->Name first middle last))

(def b (make-name :first 'Barney :last 'Rubble))
(:first b)
(:middle b)
(:last b)
(instance? Name b)
(instance? Name 'Barney)
(def b' (assoc b :middle 'Q))
b'

;; (cond
;;   テスト 結果
;;   テスト 結果
;;   ...)

;; (when test a b c)
;; (if test (do a b c))
;; (cond test (do a b c))

;; (when-not test x y)
;; (if (not test) (do x y))
;; (cond (not test) (do x y))

;; (and a b c)
;; (if a (if b c))
;; (cond a (cond b c))

;; (or a b c)
;; (if a a (if b b c))
;; (cond a a b b c c)

;; (case a
;;   b c
;;   x)
;; (if (= a b) c x)
;; (cond (= a b) c
;;       :else x)

(defn tax-bracket
  "Determine what percent tax should be paid for this income."
  [income]
  (cond
    (< income 10000.0) 0.0
    (< income 30000.0) 0.2
    (< income 50000.0) 0.25
    (< income 70000.0) 0.3
    :else              0.35))

;; (case 式
;;   照合 結果
;;   ...)

;; (let [変数 値 ...]
;;   本体...)

(let [x 40
      y (+ 1 1)]
  (+ x y))

;; ((fn [変数...]
;;    本体...)
;;  値...)

((fn [x y]
   (+ x y))
 40
 (+ 1 1))

(let [x 6
      y (* x x)]
  (+ x y))

;; Exercise 3.1
((fn [x]
   ((fn [y]
      (+ x y))
    (* x x)))
 6)

;; (conj list x)
;; (cons x list)

;; (pop list)
;; (rest list)

;; (peek list)
;; (first list)

(defrecord Player [score wins])
(defn make-player [& {:keys [score wins]
                      :or {score 0
                           wins 0}}]
  (->Player score wins))

(defn determine-winner
  "Increment the WINS for the player with highest score."
  [players]
  (-> (sort-by :score > players)
      vec
      (update-in [0 :wins] inc)))

;; (doseq [変数 シーケンス] 本体...)

(defn length1 [lst]
  (let [len (volatile! 0)]
    (doseq [element lst]
      (vswap! len inc))
    @len))

(defn length2 [lst]
  (let [len (volatile! 0)]
    (run! (fn [element]
            (vswap! len inc))
          lst)
    @len))

;; (dotimes [変数 数] 本体...)

;; (loop [変数 初期値...]
;;   本体...)

(defn length3 [lst]
  (loop [len 0
         l lst]
    (if (empty? l)
      len
      (recur (inc len)
             (rest l)))))

(defn length7 [lst]
  (count lst))

(map - '(1 2 3))
(map + '(1 2) '(10 20))
(map + '(1 2) '(10 20) '(100 200))

(remove #{1} '(1 2 3 2 1 0 -1))
(remove #(= (Math/abs %) 1) '(1 2 3 2 1 0 -1))
(remove #(> % 1) '(1 2 3 2 1 0 -1))

(remove odd? '(1 2 3 2 1 0 -1))
(filter odd? '(1 2 3 2 1 0 -1))
(some #(when (even? %) %) '(1 2 3 2 1 0 -1))

(def x '(a b c))
(def y '(1 2 3))

(every? odd? y)
(some odd? y)
(map - y)
#_(run! println y)

(->> y
     (some #{2})
     boolean)
(->> x
     (filter #(= % 'b))
     count)
(some #{2} y)
(->> x
     (map-indexed vector)
     (some (fn [[idx itm]] (when (= itm 'a) idx))))
(reduce + y)
(remove #{2} y)
(replace {2 4} y)

(defn length9 [lst]
  (if (empty? lst)
    0
    (+ 1 (length9 (rest lst)))))

(defn length10-aux [sublist len-so-far]
  (if (empty? sublist)
    len-so-far
    (recur (rest sublist) (inc len-so-far))))

(defn length10 [lst]
  (length10-aux lst 0))

(defn length11
  ([lst]
   (length11 lst 0))
  ([lst len-so-far]
   (if (empty? lst)
     len-so-far
     (recur (rest lst) (inc len-so-far)))))

(defn length12 [the-list]
  (letfn [(length13 [lst len-so-far]
            (if (empty? lst)
              len-so-far
              (recur (rest lst) (inc len-so-far))))]
    (length13 the-list 0)))

;; (letfn [(関数名 [パラメータ...] 関数本体)...]
;;   本体)

(defn product
  "Multiply all the numbers together to compute their product."
  [numbers]
  (reduce (fn [prod n]
            (if (zero? n)
              (reduced 0)
              (* n prod)))
          1
          numbers))

;; (while テスト 本体...)

;; (loop []
;;   (when テスト
;;     本体
;;     (recur)))

(defmacro my-while
  "Repeat body while test is true."
  [test & body]
  (list 'loop []
        (concat (list 'when test)
                body
                (list '(recur)))))

(macroexpand-1
 '(my-while (< @i 10)
            (println (* @i @i))
            (swap! i inc)))

(def i (atom 7))

#_(my-while (< @i 10)
            (println (* @i @i))
            (swap! i inc))

(defmacro my-while'
  "Repeat body while test is true."
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(def test1 '(a test))
`(this is ~test1)
`(this is ~@test1)
`(this is ~@test1 -- this is only ~@test1)

;;; 3.3

(first x)
(second x)
(nth x 0)
(rest x)
(last x)
(count x)
(reverse x)
(cons 0 y)
(concat x y)
(list x y)
(list* 1 2 x)
(empty? nil)
(empty? ())
(empty? x)
(list? x)
(list? 3)
(sequential? x)
(sequential? nil)
(sequential? ())
(= x x)
(= x y)
(sort > y)
(subvec (vec x) 1 2)

;; Exercise 3.2
;; list*

;; Exercise 3.3
(declare pr-next)

(defn dprint
  "Print an expression in dotted pair notation."
  [x]
  (if (not (sequential? x))
    (print x)
    (do (print "(")
        (dprint (first x))
        (pr-next (next x))
        (print ")"))))

(defn pr-next [x]
  (print " . ")
  (dprint x))

;; Exercise 3.4
;; n/a

;;; 3.4

;;; 3.5

;;; 3.6

(def table
  {'AL 'Alabama
   'AK 'Alaska
   'AZ 'Arizona
   'AR 'Arkansas})

(get table 'AK)
(get table 'TX)

;;; 3.7

(clojure.walk/postwalk-replace '{old new} '(old ((very old))))
(clojure.walk/postwalk-replace '{old new} 'old)

(defn english->french [words]
  (clojure.walk/postwalk-replace '{are vas
                                   book livre
                                   friend ami
                                   hello bonjour
                                   how comment
                                   my mon
                                   red rouge
                                   you tu
                                   today aujourd'hui}
                                 words))

(english->french '(hello my friend - how are you today ?))

;;; 3.8

(+ 4 2)
(- 4 2)
(* 4 2)
(/ 4 2)
(> 100 99)
(= 100 100)
(< 99 100)
(rand-int 100)
(expt 4 2)
(Math/sin Math/PI)
(Math/asin 0)
(min 2 3 4)
(abs -3)
(sqrt 4)
(round 4.1)
(rem 11 5)

;;; 3.9

(def r '#{a b c d})
(def s '#{c d e})

(clojure.set/intersection r s)
(clojure.set/union r s)
(clojure.set/difference r s)
(contains? r 'd)
(clojure.set/subset? s r)
(conj s 'b)
(conj s 'c)

(clojure.set/intersection '#{a b c d} '#{a b e})
(Long/toBinaryString (bit-and 2r11110 2r11001))

;;; 3.10

;; Exercise 3.5
(defprotocol INode
  (node-name [this])
  (node-yes [this])
  (node-no [this])
  (set-yes! [this yes-node])
  (set-no! [this no-node]))

(deftype Node [name
               ^:volatile-mutable yes
               ^:volatile-mutable no]
  INode
  (node-name [_] name)
  (node-yes [_] yes)
  (node-no [_] no)
  (set-yes! [this yes-node]
    (set! yes yes-node))
  (set-no! [this no-node]
    (set! no no-node))
  Object
  (toString [_]
    (str "(node :name " name ")")))

(defn make-node [& {:keys [name yes no]
                    :or {yes nil
                         no nil}}]
  (Node. name yes no))

(def db
  (make-node :name "animal"
             :yes (make-node :name "mammal")
             :no (make-node :name "vegetable"
                            :no (make-node :name "mineral"))))

(defn give-up []
  (print "I give up - what is it? ")
  (flush)
  (let [answer (read)]
    (println answer)
    (make-node :name answer)))

(defn questions
  ([] (questions db))
  ([node]
   (printf "Is it a %s? " (node-name node))
   (flush)
   (let [yes-no (read)]
     (println yes-no)
     (case (clojure.string/lower-case yes-no)
       ("y" "yes") (if (node-yes node)
                     (questions (node-yes node))
                     (set-yes! node (give-up)))
       ("n" "no") (if (node-no node)
                    (questions (node-no node))
                    (set-no! node (give-up)))
       "it" (println "Aha!")
       (do (println "Reply with YES, NO, or IT if I have guessed it.")
           (questions node))))))

;;; 3.11
\c
42
3.14159
42
123456789N
expt
'expt
nil
:key
'(a b c)
'[a b c]
"abc"

(type 123)
(instance? Long 123)
(instance? Number 123)
(instance? Integer 123)
(instance? Integer 123.0)
(isa? Long Number)

2/3

;;; 3.12

#_(clojure.pprint/cl-format true "hello, world")

#_(clojure.pprint/cl-format true "~&~a plus ~s is ~f" "two" "two" 4)

#_(let [numbers (range 1 (inc 5))]
    (clojure.pprint/cl-format true "~&~{~r~^ plus ~} is ~@r"
                              numbers (apply + numbers)))

;;; 3.13

(clojure.repl/apropos "string")

#_(clojure.repl/source char-escape-string)

#_(clojure.repl/doc first)

;;; 3.14

(defn average [numbers]
  (if (empty? numbers)
    (throw (ex-info "Average of the empty list is undefined." {:numbers numbers}))
    (/ (apply + numbers)
       (count numbers))))

(defn sqr
  "Multiply x by itself."
  [x]
  (assert (number? x))
  (* x x))

(defn f [n] (dotimes [_ n] nil))
#_(time (f 10000))

;;; 3.15

(+ 1 2 3 4)
(apply + '(1 2 3 4))
(apply + 1 2 '(3 4))
(eval '(+ 1 2 3 4))

;;; 3.16

(map (fn [x] (+ x x)) '(1 3 10))

(defn adder
  "Return a function that adds c to its argument."
  [c]
  (fn [x] (+ x c)))

(map (adder 3) '(1 3 10))
(map (adder 10) '(1 3 10))

(defn bank-account
  "Open a bank account starting with the given balance."
  [balance]
  (let [b (atom balance)]
    (fn [action amount]
      (case action
        :deposit (swap! b + amount)
        :withdraw (swap! b - amount)))))

(def my-account (bank-account 500.0))
(def your-account (bank-account 250.0))
(my-account :withdraw 75.0)
(your-account :deposit 250.0)
(your-account :withdraw 100.0)
(my-account :withdraw 25.0)

;;; 3.17

(defonce ^:dynamic *counter* 0)

(defn report []
  (printf "Counter = %d " *counter*))

#_(report)

#_(binding [*counter* 100]
    (report))

#_(report)

;; (set! 変数 値)

;; Exercise 3.6
(def a 'global-a)
(defonce ^:dynamic *b* 'global-b)

(defn fun [] *b*)

(let [a 'local-a]
  (binding [*b* 'local-b]
    (list a *b* (fun) paip.chapter03.core/a paip.chapter03.core/*b*)))
;; => (local-a local-b local-b global-a local-b)

;;; 3.18

;;; 3.19

(defn problem
  "Ask a math problem, read a reply, and say if it is correct."
  [x op y]
  (printf "How much is %d %s %d? " x op y)
  (flush)
  (let [input (read)]
    (println input)
    (if (= input ((resolve op) x y))
      (println "Correct!")
      (println "Sorry, that's not right."))))

(defn math-quiz
  "Ask the user a series of math problems."
  [op range n]
  (dotimes [i n]
    (problem (rand-int range) op (rand-int range))))

(defn math-quiz'
  "Ask the user a series of math problems."
  ([]
   (math-quiz' '+ 100 10))
  ([op]
   (math-quiz' op 100 10))
  ([op range]
   (math-quiz' op range 10))
  ([op range n]
   (dotimes [i n]
     (problem (rand-int range) op (rand-int range)))))

(defn math-quiz''
  "Ask the user a series of math problems."
  [& {:keys [op range n]
      :or {op '+
           range 100
           n 10}}]
  (dotimes [i n]
    (problem (rand-int range) op (rand-int range))))

:xyz
(defn f' [&xyz] (+ &xyz &xyz))
(f' 3)
;; (defn f' [:xyz] (+ :xyz :xyz))
(defn g [& {:keys [x y]}] (list x y))
(let [ks '(:x :y :z)]
  (g (second ks) 1 (first ks) 2))

(defn find-all
  "Find all those elements of sequence that match item,
  according to the keywords."
  [item sequence & {:keys [test test-not]
                    :or {test =}}]
  (if test-not
    (remove #(test-not % item) sequence)
    (filter #(test % item) sequence)))

;; Exercise 3.7
;; TODO

;; Exercise 3.8
;; n/a

;;; 3.20

;;; 3.21

;; Exercise 3.9
(defn length-r [lst]
  (reduce (fn [n _] (inc n))
          0
          lst))

;; Exercise 3.10
;; n/a

;; Exercise 3.11
;; n/a

;; Exercise 3.12
#_(clojure.pprint/cl-format true "~@(~{~a~^ ~}.~)" '(this is a test))
