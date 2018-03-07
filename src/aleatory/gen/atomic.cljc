(ns aleatory.gen.atomic
  "Basic atomic generators are implemented in this namespace.
  
  An atomic generator describes a class of  \"simple\" objects without an internal structure 
  (or at least not a complex ones). 
  This covers the basic data-types such as booleans, numbers, strings, etc. 
  An atomic generator is (structurally) of size 1, hence the name."
  
  (:require [aleatory.prng.source :as prng]
            #?(:clj [aleatory.gen :as g]
               :cljs [aleatory.gen :as g :refer [Generator]]))

  #?(:clj (:import [aleatory.gen Generator])))

;;{
;; ## Generation of constants
;;
;; This is the simplest class of "randomly" generated
;; values ... always drawn with probability 1.0
;;}

(defrecord Const [const])

(extend-type Const
  g/Generator
  (generate [gen src ctx]
    [(g/gen-object (:const gen)) src ctx])
  (describe [gen]
    {:generator ::const
     :props #{:uniform :atomic}
     :params {:const "The constant value to generate."}
     :doc "Draw the constant `:const` unconditionally."}))

(g/generate (->Const 42) (prng/make-random 424242) {})

;;{
;; ## Generation of booleans
;;
;; We begin with the simplest example of generating
;; booleans, i.e. coin flips.
;;}

(defrecord UnifBoolean [])

(defn next-unif-boolean [src]
  (prng/next-bool src))

(extend-type UnifBoolean
  g/Generator
  (generate [gen src ctx]
    (let [[b src'] (next-unif-boolean src)]
      [(g/gen-object b) src' ctx]))
  (describe [gen]
    {:generator ::unif-boolean
     :props #{:uniform :atomic}
     :params {}
     :doc "Draw a boolean (`true` or `false`) uniformly at random."}))

(g/generate (->UnifBoolean) (prng/make-random 424242) {})

(defrecord NonUnifBoolean [ptrue])

(defn next-non-unif-boolean [src ptrue]
  "Generate a non-uniform boolean from random source `src`
with probability `ptrue` of drawing value `true`, hence
 `false` with probability `(- 1.0 ptrue)`."
  (let [[x src'] (prng/next-real src)]
    [(<= x ptrue) src']))

(extend-type NonUnifBoolean
  g/Generator
  (generate [gen src ctx]
    (let [[b src'] (next-non-unif-boolean src (:ptrue gen))]
      [(g/gen-object b) src' ctx]))
  (describe [gen]
    {:generator ::non-unif-boolean
     :props #{:non-uniform :atomic}
     :params {:ptrue "The probability of drawing the `true` value."}
     :doc "Draw a boolean (`true` or `false`) with probability `:ptrue` at random."}))

(g/generate (->NonUnifBoolean 0.9) (prng/make-random 424242) {})


