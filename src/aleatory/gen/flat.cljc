(ns aleatory.gen.flat
  "Flat generators are implemented in this namespace.
  
  A flat generator describes a class of  \"simple\" containers of atomic
  objects. The size of a flat container is the number of its elements."
  
  (:require [aleatory.prng.source :as prng]
            #?(:clj [aleatory.gen :as g]
               :cljs [aleatory.gen :as g :refer [Generator]])
            [aleatory.gen.atomic :as atomic])

  #?(:clj (:import [aleatory.gen Generator])))

;;{
;; ## Simple vectors
;;}

(defrecord SimpleVector [atom])

(def simple-vector-descr
  {:generator ::unif-real
   :props #{:uniform :flat :sized}
   :params {:atom "The atomic generator for the vector elements."}
   :doc "A generator for vectors of atomic values described by the `atom` parameter.
The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
 a timeout is reached).
This generator also preserves uniformity."})

(defn ^{:doc (:doc simple-vector-descr)}
  simple-vector [atom]
  (->SimpleVector atom))

(defn gen-simple-vector [atom-gen src ctx]
  (loop [ctx ctx, src src, v []]
    (if (zero? (:fuel ctx))
      [(g/gen-object v :size (count v)) src ctx]
      (let [[obj src' ctx' :as ret] (g/generate atom-gen src ctx)]
        (cond
          ;; wrong fuel consumption
          (not= (:fuel ctx') (dec (:fuel ctx)))
          (throw (ex-info "Wrong non-atomic fuel consumption." {:pre-ctx ctx
                                                                :post-ctx ctx}))
          ;; object not generated
          (g/no-object? obj) ret
          
          ;; ok to continue
          :else
          (recur ctx' src' (conj v (:data obj))))))))

(extend-type SimpleVector
  g/Generator
  (generate [gen src ctx] (gen-simple-vector (:atom gen) src ctx))
  (describe [gen] (assoc simple-vector-descr
                         :elements (g/describe (:atom gen)))))

(g/generate (simple-vector (aleatory.gen.atomic/unif-boolean)) (prng/make-random 424242) {:fuel 10})

;;{
;; ## Simple strings
;;}

(defrecord SimpleString [char-gen])

(def simple-string-descr
  {:generator ::unif-real
   :props #{:uniform :flat :sized}
   :params {:char-gen "The underlying character generator."}
   :doc "A generator for simple strings over the specified character generator. 
The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
 a timeout is reached). This generator is uniform."})

(defn simple-string
  "A generator for simple strings over the specified character alphabets `alphas`, cf. [[atomic/unif-char]]. 
  The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
 a timeout is reached). This generator is uniform."
  [& alphas]
  (->SimpleString (apply atomic/unif-char alphas)))

(defn gen-simple-string [char-gen src ctx]
  (g/gen-fmap-data (partial apply str) (gen-simple-vector char-gen src ctx)))

(extend-type SimpleString
  g/Generator
  (generate [gen src ctx] (gen-simple-string (:char-gen gen) src ctx))
  (describe [gen] (assoc simple-string-descr
                         :elements (g/describe (:char-gen gen)))))

(g/generate (simple-string \a \c [\e \h] [\A \D]) (prng/make-random 424242) {:fuel 10})

;;; TODO: uniform strings of bounded length ?
;;; cf. stack overflow question
;;; https://stackoverflow.com/questions/3066707/how-do-i-generate-a-random-string-of-up-to-a-certain-length