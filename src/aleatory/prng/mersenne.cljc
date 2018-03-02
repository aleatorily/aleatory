(ns aleatory.prng.mersenne
  "This is yet another implementation of the Mersenne twister MT19936
  for 32-bits Pseudo-Random Number Generation (PRNG).
  
  as described at: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html

  The implementation here is designed for:

  - immutability: side-effects are eskewed resulting in slower (but not ridiculous) speed

  - portability: the random sequences should be the same in Clojure and Clojurescript

  Apart from these, it is almost a literal port.

  Note the usual disclaimer: this PRNG is not cryptographically secure, and the
  implementation has not been demonstrated correct, so use it at your own
  aleatory risk!

  ")

#?(:clj (set! *warn-on-reflection* true))


;; Remark: we perform computations with long in standard Clojure even if ints
;; are enough... But unchecked coercions are cumbersome to use, and speed
;; should be comparable on 64 bits machines

;;; Period parameters
(def N 624)
(def M 397)
;; Constant vector A
(def MATRIX_A 0x9908b0df)
;; Most significant w-r bits
(def UPPER_MASK 0x80000000)
;; Least significant r bits
(def LOWER_MASK 0x7fffffff)

;; useful aliases
(definline >>> [x n]
  `(unsigned-bit-shift-right ~x ~n))

(definline << [x n]
  `(bit-shift-left ~x ~n))

(definline ++ [a b]
  `(+ ~a ~b) ; maybe go with unchecked-ints at some point...
  )

(definline ** [a b]
  `(* ~a ~b))


;;; Mersenne tiwster state
(defrecord MTState [mt mti])

;;; Seeding

(def UPPER_MASK_SEED 0xffff0000)
(def LOWER_MASK_SEED 0x0000ffff)
(def MULTIPLIER_SEED 1812433253)

(defn mk-mtstate-by-val [seed]
  (let [init (>>> seed 0)]
    (loop [mti 1, prev init, mt [init]]
      (if (< mti N)
        (let [s (bit-xor prev (>>> prev 30))
              new (-> s
                      (** MULTIPLIER_SEED)
                      (++ mti)
                      (bit-and 0xFFFFFFFF))]
          (recur (inc mti) new (conj mt new)))
        ;; end
        (->MTState mt N)))))

(mk-mtstate-by-val 32)

;; TODO : initialization by vector/array of values (requires a counted thing)
(defn mk-mtstate-by-vals [seeds])


