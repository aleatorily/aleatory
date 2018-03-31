(ns aleatory.gen.automaton
  "A uniform random generator of path for deterministic
  finite-state automata. This can be used to generate
  words of regular languages uniformly at random."

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
               (assoc ws q (bigint 1))
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
                  (let [qweight (reduce +' 0N (map #(get pweights % 0N) (vals (get aut (first qs)))))]
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




