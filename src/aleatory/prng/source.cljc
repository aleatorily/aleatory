(ns aleatory.prng.source
  "Definition of a PRNG source.

  We use pseudo-random number generators (PRNGs) as random
  sources for generating more complex things. The PRNG protocol
   allows to use different algorithms with different characteristics.
  "
  (:require  #?(:clj [aleatory.prng.mersenne :as m]
                :cljs [aleatory.prng.mersenne :as m :refer [MTState]]))
             
  #?(:clj (:import [aleatory.prng.mersenne MTState]))
  
  )

;; We fix the word size for the random generation
(def ^:const WORD-SIZE 32)

(defprotocol PRNG
  "The common protocol for pseudo-random number generators.
   on PRNGs. We expect a 32bit value represented as a long."
  (next-uint32-impl [prng] "Fetch the next 32bits number which is returned together with the next state of the PRNG, as a pair."))

(extend-type MTState
  PRNG
  (next-uint32-impl [prng] (m/next-num prng)))

(def ^:dynamic *default-prng* m/mk-mtstate-by-val)


(defrecord RandomSource [prng seed count last index])

(defn random-source? [t]
  (instance? RandomSource t))

;; XXX: negative values are not taken into account
(defn random-seed []
  #?(:clj (rand-int Integer/MAX_VALUE)
     :cljs (rand-int 2147483647)))

(defn make-random
  "Create a new (pseudo-) random source with optional `seed`.

  An optional `prng` (pseudo-random number generator) backend
  can be given. This `prng` must understand the [[PRNG]] protocol.
  The default `prng` is given by the dynamic variable [[*default-prng*]]"
  ([] (make-random *default-prng* (random-seed)))
  ([seed] (make-random *default-prng* seed))
  ([prng seed] (let [prng (prng seed)]
                 (->RandomSource prng seed 0 nil 0))))


(defn- consume [rnd]
  (let [[num prng] (next-uint32-impl (:prng rnd))]
    (->RandomSource prng (:seed rnd) (inc (:count rnd)) num (dec WORD-SIZE))))

;; (consume (make-random 424242))

(defn integer-bounds [min max x]
  (+ (mod x (- max min)) min))

(integer-bounds 10 100 25043)

(defn next-uint32
  "Consumes a uniform 32 bits number from the random source `rnd`.
  Returns a pair of this number and the next state of the source."
  ([rnd min max] (let [[num rnd'] (next-uint32 rnd)]
                   [(integer-bounds min max num) rnd']))
  ([rnd max] (next-uint32 rnd 0 max))
  ([rnd]
  (let [rnd' (consume rnd)]
    [(:last rnd') (assoc rnd' :last nil)])))

(defn next-bool
  "Consumes a uniform random boolean from
  the random source `rnd`"
  [rnd]
  (if (nil? (:last rnd))
    (let [rnd' (consume rnd)]
      [(bit-test (:last rnd') (:index rnd')) (update rnd' :index dec)])
    ;; at least one bit in last
    [(bit-test (:last rnd) (:index rnd)) (if (zero? (:index rnd))
                                           (assoc rnd :last nil)
                                           (update rnd :index dec))]))

(next-bool (make-random 424242))
(next-bool (second (next-bool (make-random 424242))))

(declare next-real)
(defn next-bernoulli [src ptrue]
  "Generate a non-uniform boolean from random source `src` using
Bernoulli distribution. The probability of drawing value `true`
is `ptrue`, hence `false` is with probability `(- 1.0 ptrue)`."
  (let [[x src'] (next-real src)]
    [(<= x ptrue) src']))

(defn uint32->int
  "Converts an unsigned 32 bits integer (represented as a long) to a signed 32 bits integer."
  [x] (unchecked-int x))

(uint32->int 0x00000000)
(uint32->int 0x80000000)
(uint32->int 0x7FFFFFFF)
(uint32->int 0xFFFFFFFF)


(defn next-long
  "Produces a uniform signed 64 bits long integer from the random source `rnd`."
  ([rnd min max] (let [[num rnd'] (next-long rnd)]
                   [(integer-bounds min max num) rnd']))
  ([rnd max] (next-long rnd 0 max))
  ([rnd] (let [[num1 rnd'] (next-uint32 rnd)
               [num2 rnd''] (next-uint32 rnd')]
           [(bit-or (bit-shift-left num1 32) num2) rnd''])))

(next-long (make-random 424242))
(next-long (make-random 424242) -300 300)
(next-long (make-random 424242)  300)

(defn next-int
  "Produces a uniform signed 32 bits integer from the random source `rnd`."
  ([rnd min max] (let [[num rnd'] (next-int rnd)]
                   [(integer-bounds min max num) rnd']))
  ([rnd max] (next-int rnd 0 max))
  ([rnd] (let [[num rnd'] (next-uint32 rnd)]
          [(uint32->int num) rnd'])))

(next-int (make-random 424242))
(next-int (make-random 424242) -30 30)
(next-int (make-random 424242) 30)

(defn uint32->real
  "Converts an unsigned 32 bits integer (represented as a long) to a signed 32 bits integer."
  [x] (* x (/ 1.0 4294967296.0)))

(defn next-real
  "Produces a uniform \"real\" number between 0.0 (inclusive) and 1.0 (exclusive),
   represented as a `double` value."
  [rnd] (let [[num rnd'] (next-uint32 rnd)]
          [(uint32->real num) rnd']))

(next-real (make-random 424242))

(defn rand-uint32-seq
  "Generates a sequence of random (32 bits) numbers from the given `rnd` random source."
  ([rnd min max] (map (partial integer-bounds min max) (rand-uint32-seq rnd)))
  ([rnd max] (rand-uint32-seq rnd 0 max))
  ([rnd]
   (lazy-seq
    (let [[num rnd'] (next-uint32 rnd)]
      (cons num (rand-uint32-seq rnd'))))))

(defn rand-long-seq
  "Generates a sequence of random (64bits) integers from the given `rnd` random source."
  ([rnd min max] (map (partial integer-bounds min max) (rand-long-seq rnd)))
  ([rnd max] (rand-long-seq rnd 0 max))
  ([rnd]
   (lazy-seq (let [[num rnd'] (next-long rnd)]
               (cons num (rand-long-seq rnd'))))))

(take 20 (rand-long-seq (make-random 424242)))

(filter #(or (< % 10)
             (>= % 90)) (take 10000 (rand-long-seq (make-random 424242) 10 90)))

(filter #(>= % 90) (take 10000 (rand-long-seq (make-random 424242) 90)))

(defn rand-int-seq
  "Generates a sequence of random (32bits) integers from the given `rnd` random source."
  ([rnd min max] (map (partial integer-bounds min max) (rand-int-seq rnd)))
  ([rnd max] (rand-int-seq rnd 0 max))
  ([rnd]
   (map uint32->int (rand-uint32-seq rnd))))

(take 20 (rand-int-seq (make-random 424242)))

(filter #(or (< % -30)
             (>= % 30))
        (take 10000 (rand-int-seq (make-random 424242) -30 30)))

(take 10 (map type (rand-int-seq (make-random 424242))))


