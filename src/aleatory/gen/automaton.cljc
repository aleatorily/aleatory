(ns aleatory.gen.automaton
  "A uniform random generator of path for deterministic
  finite-state automata. This can be used to generate
  words of regular languages quasi-uniformly at random.

  The generator can be preprocessed for a given length `n`
  of random paths. The preprocessing requires roughly
  quadratic time and space, so large values for `n` should
  be avoided. A single preprocessing is enough for generating
   an arbitrary number of random paths of the same length,
   in linear-time.
  
  Remark: since we use long-to-double division operations the
  uniformity is only achieved for relatively small
  path lenghts (on not-so-small automata)."

  (:require #?(:clj [aleatory.utils :as u]
               :cljs [aleatory.utils :as u :refer-macros [except except-info]])
            [aleatory.prng.source :as prng]
            #?(:clj [aleatory.gen :as g]
               :cljs [aleatory.gen :as g :refer [Generator]]))
  
  #?(:clj (:import [aleatory.gen Generator])))

;; TODO : put in separate test file
(def aut-ex {::start :q0
             :q0 {:a :q1}
             :q1 {:b :q2
                  :c :q3}
             :q2 {:b :q2
                  :a :q4}
             :q3 {:a :q4}
             :q4 {:a :q1
                  ::final true}})

(defn init-weights [aut]
  [(reduce (fn [ws [q trans]]
             (if (::final trans)
               (assoc ws q 1)
               ws)) {} aut)])

(init-weights aut-ex)

(defn preprocess [aut n]
  (loop [i 1, weights (init-weights aut)]
    (if (<= i n)
      (let [pweights (nth weights (dec i)) ;; previous weights
            iweights
            (loop [qs (keys aut), iweights {}]
              (if (seq qs)
                (if (= ::start (first qs))
                  (recur (rest qs) iweights)
                  (let [qweight (reduce + 0 (map #(get pweights % 0) (vals (get aut (first qs)))))]
                    (if (zero? qweight)
                      (recur (rest qs) iweights)
                      (recur (rest qs) (assoc iweights (first qs) qweight)))))
                ;; all states processed
                iweights))]
                
        (recur (inc i) (assoc weights i iweights)))
      ;; at the end
      weights)))

(preprocess aut-ex 0)
(preprocess aut-ex 1)
(preprocess aut-ex 2)
(preprocess aut-ex 3)
(preprocess aut-ex 10)

(defn trans-probas [ts qweight nweights]
  (reduce (fn [probas [lbl q']]
            (conj probas [lbl q' (/ (double (get nweights q' 0))
                                    qweight)])) [] ts))

(trans-probas (get aut-ex :q2) (get (nth (preprocess aut-ex 10) 5) :q2)
              (nth (preprocess aut-ex 10) 4))


(defn choice [prng choices]
  (let [[x prng'] (prng/next-real prng)]
    (loop [choices choices, x x, nb (count choices)]
      (let [[lbl q' proba] (first choices)]
        (if (or (<= nb 1) (<= x proba))
          [lbl q' prng']
          (recur (rest choices) (- x proba) (dec nb)))))))

(choice (prng/make-random 424242) [[:a :q2 0.33] [:b :q3 0.33] [:c :q4 0.33]])


(defn generate-path
  "Generates a path of lengths `n` in automaton `aut`."
  [prng aut weights n]
  (loop [q (::start aut), prng prng, n n, path []]
    (if (zero? n)
      [path prng]
      (let [ts (get aut q)
            qweight (get (nth weights n) q 0.0)]
        (if (zero? qweight)
          ;; no path
          [[] prng]
          (let [nweights (nth weights (dec n))
                probas (trans-probas ts qweight nweights)
                [lbl q' prng'] (choice prng probas)]
            (recur q' prng' (dec n) (conj path lbl))))))))

(generate-path (prng/make-random 424242) aut-ex (preprocess aut-ex 3) 3)
(generate-path (prng/make-random) aut-ex (preprocess aut-ex 3) 3)

(generate-path (prng/make-random 424242) aut-ex (preprocess aut-ex 10) 10)
(generate-path (prng/make-random) aut-ex (preprocess aut-ex 10) 10)


