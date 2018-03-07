(ns aleatory.gen.atomic
  "Basic atomic generators are implemented in this namespace.
  
  An atomic generator describes a class of  \"simple\" objects without an internal structure 
  (or at least not a complex ones). 
  This covers the basic data-types such as booleans, numbers, strings, etc. 
  An atomic generator is (structurally) of size 1, hence the name."
  
  (:require [aleatory.prng.source :as prng]))


;;{
;; ## Generation of booleans
;;
;; We begin with the simplest example of generating
;; booleans, i.e. coin flips.
;;}

(defn next-unif-boolean [src]
  (prng/next-bool src))

(defn next-non-unif-boolean [src ptrue]
  "Generate a non-uniform boolean from random source `src`
with probability `ptrue` of drawing value `true`, hence
 `false` with probability `(- 1.0 ptrue)`."
  (let [[x src'] (prng/next-real src)]
    [(<= x ptrue) src']))


