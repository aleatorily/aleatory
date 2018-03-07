(ns aleatory.gen
  "The common functionalities of random generators
  are defined in this namespace.")

(defrecord GenObject [data size])

(defn gen-object [data & {:keys [size]
                          :or {size 1}}]
  (->GenObject data size))

(defprotocol Generator
  "The generic protocol for generators."
  (generate [gen src ctx]
    "Generates an object using the generator `gen` from
 random source `src` and generation context `ctx`.
This returns a triple `[obj src' ctx']` of the generated object
as a [[GenObject]] record together with the next random source
state and updated context.")
  (describe [gen]
    "Returns a description of the generator as a map,
 which can be used to recreate the generator, and also
analyze its properties"))

