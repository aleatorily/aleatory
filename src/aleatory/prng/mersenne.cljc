(ns aleatory.prng.mersenne
  "This is yet another implementation of the Mersenne twister MT19937
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

;; Keeping only 32 bits
(def MASK_32BITS 0xFFFFFFFF)

;; useful aliases
(definline >>> [x n]
  `(unsigned-bit-shift-right ~x ~n))

(definline << [x n]
  `(bit-shift-left ~x ~n))

;;; Mersenne tiwster state
(defrecord MTState [mt index])

#?(:clj (defmethod print-method MTState [st ^java.io.Writer writer]
          (.write writer "<<MTState>>")))

;;; Seeding

(def MULTIPLIER_SEED 1812433253)

(defn mk-mtstate-by-val [seed]
  (loop [mti 1, prev seed, mt [seed]]
    (if (< mti N)
      (let [s (bit-xor prev (>>> prev 30))
            new (-> s
                    (* MULTIPLIER_SEED)
                    (+ mti)
                    (bit-and MASK_32BITS))]
        (recur (inc mti) new (conj mt new)))
      ;; end
      (->MTState mt N))))

(mk-mtstate-by-val 424242)

;; TODO : initialization by vector/array of values (requires a counted thing)
;; (defn mk-mtstate-by-vals [seeds])


(defn twister [state]
  (loop [i 0, mt (:mt state)]
    (if (< i N)
      (let [s (-> (nth mt i)
                  (bit-and UPPER_MASK)
                  (+ (bit-and (nth mt (mod (inc i) N))
                              LOWER_MASK))
                  (bit-and MASK_32BITS))
            mti (bit-xor (nth mt (mod (+ i M) N)) (>>> s 1))
            mti' (if (odd? s)
                   (bit-xor mti MATRIX_A)
                   mti)]
        (recur (inc i) (assoc mt i mti')))
      ;; end
      (->MTState mt 0))))

;; (last (:mt (twister (mk-mtstate-by-val 424242))))

(defn next-num [state]
  (let [state (if (>= (:index state) N)
                (twister state)
                state)
        x (nth (:mt state) (:index state))
        x (bit-xor x (>>> x 11))
        x (bit-xor x (bit-and (<< x 7) 2636928640))
        x (bit-xor x (bit-and (<< x 15) 4022730752))
        x (bit-xor x (>>> x 18))
        x (bit-and x MASK_32BITS)]
    [x (update state :index inc)]))

;; (next-num (second (next-num (mk-mtstate-by-val 424242))))


(defn num->real [num]
  (* num (/ 1.0 4294967295.0)))

;; (num->real (first (next-num (mk-mtstate-by-val 424242))))
