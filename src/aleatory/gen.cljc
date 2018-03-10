(ns aleatory.gen
  "The common functionalities of random generators
  are defined in this namespace.")

(defrecord GenObject [data size])

(defn no-object
  "Generate a failed [[GenObject]], i.e. the failure of generating
  an object with `:data` the `reason` of the failure, and of `:size` zero.

  The currently valid reasons are:

  - `:aleatory.gen/not-enough-fuel` if there is not enough fuel left for generating the object
  - `:aleatory.gen/timeout` if the generation process is taking \"too long\".

  For timeout, the unit of time is the generation of a single number from the underlying PRNG.
  "
  [reason]
  (->GenObject reason 0))

(def not-enough-fuel ::not-enough-fuel)

(defn gen-object [data & {:keys [size]
                          :or {size nil}}]
  (->GenObject data size))

(defn no-object? [obj]
  (zero? (:size obj)))

(defprotocol Generator
  "The generic protocol for generators."
  (generate [gen ctx]
    "Generates an object using the generator `gen` from
 generation context `ctx`.
This returns a pair `[obj ctx']` of the generated object
as a [[GenObject]] record together with the next random source
state and updated context.")
  (describe [gen]
    "Returns a description of the generator as a map,
 which can be used to recreate the generator, and also
analyze its properties."))

(defn generator? [x]
  (instance? Generator x))

(defn gen-fmap [f [obj ctx]]
  (if (no-object? obj)
    [obj ctx]
    (f obj ctx)))

(defn gen-fmap-data [f ret]
  (gen-fmap (fn [obj ctx] [(assoc obj :data (f (:data obj))) ctx]) ret))



