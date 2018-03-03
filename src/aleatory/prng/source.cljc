(ns aleatory.prng.source
  "Definition of a PRNG source.

  We use pseudo-random number generators (PRNGs) as random
  sources for generating more complex things. The PRNG protocol
   allows to use different algorithms with different characteristics.
  "
  (:require [aleatory.prng.mersenne :as m])

  (:require #?(:clj [aleatory.prng.mersenne :as m]
               :cljs [aleatory.prng.mersenne :as m :refer [MTState]]))
  #?(:clj
     (:import [aleatory.prng.mersenne MTState]))

  
  )

(defprotocol PRNG
  "The common protocol for pseudo-random number generators.
   on PRNGs"
  (num-size [prng] "Get the bit-size of the number generated at each step (most often 32 bits).
This should be between 1 and 64.")
  (next-num [prng] "Fetch the next number which is returned together with the next state of the PRNG, as a pair."))

(extend-type MTState
  PRNG
  (num-size [prng] 32)
  (next-num [prng] (m/next-num prng)))

(def ^:dynamic *default-prng* m/mk-mtstate-by-val)


(defrecord RandomSource [prng seed count last index])

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

(make-random)

(defn produce [rnd]
  (let [[num prng] (next-num (:prng rnd))]
    (->RandomSource prng (:seed rnd) (inc (:count rnd)) num (dec (num-size prng)))))

(produce (make-random 424242))

(defn next-bool
  "Produces a uniform random boolean from
  the random source `rnd`"
  [rnd]
  (if (nil? (:last rnd))
    (let [rnd' (produce rnd)]
      [(bit-test (:last rnd') (:index rnd')) (update rnd' :index dec)])
    ;; at least one bit in last
    [(bit-test (:last rnd) (:index rnd)) (if (zero? (:index rnd))
                                           (assoc rnd :last nil)
                                           (update rnd :index dec))]))

(next-bool (make-random 424242))
(next-bool (second (next-bool (make-random 424242))))



(defn next-int
  "Produces a uniform signed 32 bits integer from the random source `rnd`."
  [rnd] (let [rnd' (produce rnd)
              num (:last rnd')
              rnd'' (assoc rnd :last nil)]
          [(unchecked-int (bit-and num 0xFFFFFFFF)) rnd'']))

(next-int (make-random 424242))

(bit-test 2r0011 0)
