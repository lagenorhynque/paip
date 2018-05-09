(ns paip.chapter02.core)

;;;; 2. A Simple Lisp Program

;;; 2.1

;;; 2.2

(declare one-of)

(defn Article []
  (one-of '(the a)))
(defn Noun []
  (one-of '(man ball woman table)))
(defn Verb []
  (one-of '(hit took saw liked)))
(defn noun-phrase []
  (concat (Article) (Noun)))
(defn verb-phrase []
  (concat (Verb) (noun-phrase)))
(defn sentence []
  (concat (noun-phrase) (verb-phrase)))

(defn one-of
  "Pick one element of set, and make a list of it."
  [set]
  (list (rand-nth set)))

(sentence)
(noun-phrase)
(verb-phrase)

(declare Adj PP)

(defn Adj* []
  (if (zero? (rand-int 2))
    ()
    (concat (Adj) (Adj*))))

(defn PP* []
  (if (rand-nth '(true false))
    (concat (PP) (PP*))))

(defn Adj [] (one-of '(big little blue green adiabatic)))
(defn Prep [] (one-of '(to in by with on)))
(defn noun-phrase' [] (concat (Article) (Adj*) (Noun) (PP*)))
(defn PP [] (concat (Prep) (noun-phrase')))

(defn sentence' []
  (concat (noun-phrase') (verb-phrase)))

;;; 2.3

(def simple-grammar
  "A grammar for a trivial subset of English."
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)))

(def grammar
  "The grammar used by generate. Initially, this is
  simple-grammar, but we can switch to other grammars."
  (atom simple-grammar))

(defn assoc' [item alist]
  (some #(when (= (first %) item) %) alist))

(assoc' 'Noun @grammar)

(defn rule-lhs
  "The left-hand side of a rule."
  [rule]
  (first rule))

(defn rule-rhs
  "The right-hand side of a rule."
  [rule]
  (nnext rule))

(defn rewrites
  "Return a list of the possible rewrites for this category."
  [category]
  (rule-rhs (assoc' category @grammar)))

(defn generate
  "Generate a random sentence or phrase."
  [phrase]
  (cond
    (list? phrase) (mapcat generate phrase)
    (rewrites phrase) (generate (rand-nth (rewrites phrase)))
    :else (list phrase)))

(generate 'sentence)
(generate 'noun-phrase)
(generate 'verb-phrase)

(defn generate'
  "Generate a random sentence or phrase."
  [phrase]
  (if (list? phrase)
    (mapcat generate' phrase)
    (if-let [choices (rewrites phrase)]
      (generate' (rand-nth choices))
      (list phrase))))

;; Exercise 2.1
(defn generate''
  "Generate a random sentence or phrase."
  [phrase]
  (let [choices (atom nil)]
    (cond
      (list? phrase) (mapcat generate'' phrase)
      (reset! choices (rewrites phrase)) (generate'' (rand-nth @choices))
      :else (list phrase))))

;; Exercise 2.2
(defn non-terminal? [phrase]
  (boolean (rewrites phrase)))

(defn generate'''
  "Generate a random sentence or phrase."
  [phrase]
  (cond
    (list? phrase) (mapcat generate''' phrase)
    (non-terminal? phrase) (generate''' (rand-nth (rewrites phrase)))
    :else (list phrase)))

;;; 2.4

;;; 2.5

(def bigger-grammar
  ""
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(reset! grammar bigger-grammar)

(generate 'sentence)

;;; 2.6

(defn generate-tree
  "Generate a random sentence or phrase,
  with a complete parse tree."
  [phrase]
  (cond
    (list? phrase) (map generate-tree phrase)
    (rewrites phrase) (cons phrase
                            (generate-tree (rand-nth (rewrites phrase))))
    :else (list phrase)))

(generate-tree 'sentence)

(defn combine-all
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((a 1) (b 1) (a 2) (b 2))."
  [xs ys]
  (mapcat (fn [y]
            (map (fn [x] (concat x y)) xs))
          ys))

(reset! grammar simple-grammar)

(defn generate-all
  "Generate a list of all possible expansions of this phrase."
  [phrase]
  (cond
    (nil? phrase) (list nil)
    (list? phrase) (combine-all (generate-all (first phrase))
                                (generate-all (next phrase)))
    (rewrites phrase) (mapcat generate-all (rewrites phrase))
    :else (list (list phrase))))

(generate-all 'Article)
(generate-all 'Noun)
(generate-all 'noun-phrase)
(count (generate-all 'sentence))

;;; 2.7

;; Exercise 2.3
(def simple-japanese-grammar
  '((sentence -> (subject predicate))
    (subject -> (noun-phrase ParticleForSubject))
    (predicate -> (noun-phrase ParticleForObject Verb))
    (noun-phrase -> (NounModifier Noun))
    (NounModifier -> その ある)
    (Noun -> 男 ボール 女 テーブル)
    (ParticleForSubject -> が)
    (ParticleForObject -> を)
    (Verb -> 打った 取った 見た 好んだ)))

(reset! grammar simple-japanese-grammar)

(generate 'sentence)

;; Exercise 2.4
(defn cross-product [f xs ys]
  (mapcat (fn [y]
            (map (fn [x] (f x y)) xs))
          ys))

(defn combine-all' [xs ys]
  (cross-product concat xs ys))
