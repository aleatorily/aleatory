(ns aleatory.gen
  "The common functionalities of random generators
  are defined in this namespace."

  (:require [aleatory.prng.source :as prng]))

(defrecord GenObject [data metadata])

(defn no-object
  "Generate a failed [[GenObject]], i.e. the failure of generating
  an object with `:data` the reason of the failure and as `:metadata` some further
  information map.

  The currently valid reasons are:
  
  - `:aleatory.gen/size-too-small` if the target size is too small for generating the object.
  - `:aleatory.gen/timeout` if the generation process is taking \"too long\".

  For timeout, the unit of time is the generation of a single number from the underlying PRNG.
  "
  [reason info]
  ;; TODO add some checks
  (->GenObject reason (assoc info ::no-object true)))

(defn no-object? [obj]
  (get (:metadata obj) ::no-object false))

(defn gen-object [data info]
  ;; TODO add some checks
  (->GenObject data info))

(defprotocol Generator
  "The generic protocol for generators."
  (describe [gen]
    "Returns a description of the generator as a map,
 which can be used to recreate the generator, and also
analyze its properties.")
  (prepare-gen-context [gen ctx]
    "Checks the provided context for specific generator requirements.
The returned information is a pair `[ok res]` with `ok` a boolean flag
which is `true` is the context is accepted. The result `res` is in this
case a potentially updated version of `ctx`. In case of an error (e.g. a
 requirement not satisfied), the flag is `false` and the result is an
explanation string for the failure.")
  (sample [gen ctx]
    "Generates an object using the generator `gen` from
 generation context `ctx`.
This returns a pair `[obj ctx']` of the generated object
as a [[GenObject]] record together with the next random source
state and updated context."))

(defn generator? [x]
  (instance? Generator x))

(defn samples [gen nb-samples ctx]
  (if (zero? nb-samples)
    ()
    (lazy-seq (let [[obj ctx'] (sample gen ctx)]
                (if (no-object? obj)
                  (list [obj ctx'])
                  (cons [obj ctx'] (samples (dec nb-samples) ctx')))))))


(defn gen-fmap [f [obj ctx]]
  (if (no-object? obj)
    [obj ctx]
    (f obj ctx)))

(defn gen-fmap-data [f ret]
  (gen-fmap (fn [obj ctx] [(assoc obj :data (f (:data obj))) ctx]) ret))


;; this somewhat fuzzy parameterization is
;; somewhat non-idiomatic, but it makes the
;; main generate function quite user-friendly.
(declare parse-gen-opts)
(declare parse-gen-positional)
(declare prepare-context)

(defn generate [gen & opts]
  (let [[args ctx] (parse-gen-opts opts)
        positional (parse-gen-positional args)
        [ok ctx'] (prepare-context gen ctx)]
    (when (not ok)
      (throw (ex-info "Cannot generate object: bad generator context"
                      (assoc ctx' :context ctx))))
    (if-let [nb-samples (get positional :nb-samples)]
      (samples nb-samples gen ctx')
      (sample gen ctx'))))

(defn parse-gen-opts [opts]
  (loop [opts opts, args [], ctx {}]
    (if (seq opts)
      (let [arg (first opts)]
        (if (keyword? arg)
          (if (seq opts)
            (recur (rest (rest opts)) args (assoc ctx arg (second opts)))
            (throw (ex-info "Missing value for option" {:option arg})))
          ;; not a keyword? positional
          (recur (rest opts) (conj args arg) ctx)))
      ;; no more opts
      [args ctx])))

(defn parse-gen-positional [args]
  (if (seq args)
    (if (seq (rest args))
      (throw (ex-info "Too many positional arguments"{:args args}))
      (let [nb-samples (first args)]
        (if (and (integer? nb-samples) (>= nb-samples 1))
          {:nb-samples nb-samples}
          (throw (ex-info "First (optional) positional argument should be
a strictly positive integer (number of samples)"{:argument nb-samples})))))
    ;; no argument
    {}))

;; this can be redefined locally if needed
;; (dynamic var)
(def ^:dynamic *reseed-function* prng/random-seed)

(defn prepare-context [gen ctx]
  (let [[ok ctx] (if-let [seed (get ctx :seed)]
                   (if (integer? seed)
                     [true (assoc ctx :reseed false)]
                     [false {:message "Context option :seed should be an integer." :seed seed}])
                   ;; no seed
                   (if-let [src (get ctx :source)]
                     [true (assoc ctx
                                  :seed (:seed src)
                                  :reseed false)]
                     [true (assoc ctx
                                  :seed (*reseed-function*)
                                  :reseed true)]))]
    (if (not ok)
      [ok ctx]
      (let [[ok ctx] (if-let [src (get ctx :source)]
                       (if (prng/random-source? src)
                         [true ctx]
                         [false {:message "Context option :source should be a RandomSource" :source src}])
                       [true (assoc ctx :source (prng/make-random (:seed ctx)))])]
        (if (not ok)
          [ok ctx]
          (prepare-gen-context gen ctx))))))
