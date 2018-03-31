(ns aleatory.gen.atomic
  "Basic atomic generators are implemented in this namespace.
  
  An atomic generator describes a class of  \"simple\" objects without an internal structure 
  (or at least not a complex ones). 
  This covers the basic data-types such as booleans, numbers, strings, etc. 
  An atomic generator is (structurally) of size 1, hence the name."
  
  (:require #?(:clj [aleatory.utils :as u]
               :cljs [aleatory.utils :as u :refer-macros [except except-info]])
            [aleatory.prng.source :as prng]
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

(def const-descr
  {:generator ::const
     :props #{:uniform :atomic}
     :params {:const "The constant value to generate."}
     :doc "Draw the constant `:const` unconditionally."})

(defn const
  "A generator for constant `c`."
  [c]
  (->Const c))

(defn const-nil
  "A generator for the `nil` value."
  []
  (->Const nil))

(defn prepare-atomic-context [ctx]
  (if-let [size (get ctx :size)]
    (if (and (integer? size)
             (>= size 0))
      [true ctx]
      [false {:message "The :size field of context should be a positive integer"
              :size size}])
    ;; we generate size 1 by default (sized generator)
    [true (assoc ctx :size 1)]))

(defn atomic-gen [val ctx]
  (if (> (:size ctx) 0)
    [(g/gen-object val {:size 1}) (update ctx :size dec)]
    [(g/no-object :g/size-too-small {:target-size (:size ctx)
                                     :min-size 1}) ctx]))

(extend-type Const
  g/Generator
  (describe [_] const-descr)
  (prepare-gen-context [_ ctx] (prepare-atomic-context ctx))
  (sample [gen ctx]
    (atomic-gen (:const gen) ctx)))

(g/generate (->Const 42) :size 1 :seed 424242)
(g/generate (->Const 42) :seed 424242)
(g/generate (->Const 42) :size 1)


;;{
;; ## Generation of booleans
;;
;; We begin with the simplest example of generating
;; booleans, i.e. coin flips.
;;}

(defrecord UnifBoolean [])

(def unif-boolean-descr
  {:generator ::unif-boolean
   :props #{:uniform :atomic}
   :params {}
   :doc "Draw a boolean (`true` or `false`) uniformly at random."})

(defn unif-boolean
  "A generator for a uniform boolean, i.e. a *coin flip*."
  []
  (->UnifBoolean))

(extend-type UnifBoolean
  g/Generator
  (prepare-gen-context [_ ctx] (prepare-atomic-context ctx))
  (sample [gen ctx]
    (let [[b src'] (prng/next-bool (:source ctx))]
      (atomic-gen b (assoc ctx :source src'))))
  (describe [_] unif-boolean-descr))

(g/generate (->UnifBoolean)  :size 1 :seed 424242)

(defrecord BernoulliBoolean [ptrue])

(def bernoulli-boolean-descr
  {:generator ::bernoulqli-boolean
   :props #{:non-uniform :atomic}
   :params {:ptrue "The probability of drawing the `true` value."}
   :doc "Draw a boolean (`true` or `false`) with probability `:ptrue` at random."})

(defn bernoulli-boolean
  "A generator for a non-uniform boolean, following the *Bernoulli distribution*.
  The value `true` is drawn with probabilty `ptrue` (a value between 0.0 inclusive
   and 1.0 exclusive)."
  [ptrue]
  (->BernoulliBoolean ptrue))

(extend-type BernoulliBoolean
  g/Generator
  (prepare-gen-context [_ ctx] (prepare-atomic-context ctx))
  (sample [gen ctx]
    (let [[b src'] (prng/next-bernoulli (:source ctx) (:ptrue gen))]
      (atomic-gen b (assoc ctx :source src'))))
  (describe [gen] bernoulli-boolean-descr))

(g/generate (->BernoulliBoolean 0.9) :size 1 :seed 424242)

;;{
;; ## Generation of numbers
;;
;;}

(defrecord UnifReal [])

(def unif-real-descr
  {:generator ::unif-real
   :props #{:uniform :atomic}
   :params {}
   :doc "Draw a uniform real number (32 bits floating point value) 
between 0.0 (inclusive) and 1.0 (exclusive)."})

(defn unif-real
  "A generator for a inform real (32 bits floating point) number
   between 0.0 (inclusive) and 1.0 (exclusive)."
  []
  (->UnifReal))

(extend-type UnifReal
  g/Generator
  (prepare-gen-context [_ ctx] (prepare-atomic-context ctx))
  (sample [gen ctx]
    (let [[x src'] (prng/next-real (:source ctx))]
      (atomic-gen x (assoc ctx :source src'))))
  (describe [gen] unif-real-descr))

(g/generate (->UnifReal) :size 1 :seed 424242)

(defrecord UnifInt [min max])

(def unif-int-descr
  {:generator ::unif-int
   :props #{:uniform :atomic}
   :params {:min :max}
   :doc "Draw a uniform integer number (32 bits signed) 
between :min (inclusive, defaults to 0) and :max (exclusive)."})

(defn unif-int
  "Draw a uniform integer number (32 bits signed) 
  between `min` (inclusive, defaults to 0) and `max` (exclusive)."
  ([max] (unif-int 0 max))
  ([min max] (->UnifInt min max)))

(extend-type UnifInt
  g/Generator
  (prepare-gen-context [_ ctx] (prepare-atomic-context ctx))
  (sample [gen ctx]
    (let [[x src'] (prng/next-int (:source ctx) (:min gen) (:max gen))]
      (atomic-gen x (assoc ctx :source src'))))
  (describe [gen] unif-int-descr))

(g/generate (->UnifInt 10 30) :size 1 :seed 424242)

;;{
;; ## Generation of characters

;; This is based on the notion of an alphabet
;;}

(defn alpha-literal
  "Describes the alphabet composed of a single character literal `c`."
  [c]
  {:chars (sorted-set c)
   :ranges []})

(defn alpha-set
  "Describes the alphabet composed of a set of character literals `cs`."
  [cs]
  {:chars cs
   :ranges []})

(defn alpha-range
  "Describes the alphabet composed of an interval of characters
  from `cmin` to `cmax`, both included."
  [cmin cmax]
  (cond
    (u/< cmax cmin) (throw (ex-info "Maximum character should be greated that minimum." {:cmin cmin
                                                                                         :cmax cmax}))
    (u/= cmin cmax) (alpha-literal cmin)
    :else
    {:chars (sorted-set)
     :ranges [[cmin cmax]]}))

(declare char-in-range?)

(defn known-char? [alpha c]
  (or (contains? (:chars alpha) c)
      (some #(char-in-range? (first %) (second %) c) (:ranges alpha))
      false))

(defn char-in-range? [cmin cmax c]
  (u/<= cmin c cmax))

(char-in-range? \a \z \e)
(char-in-range? \a \z \A)

(known-char? {:chars (sorted-set \b \c)
              :ranges [[\e \g]]} \d)
(known-char? {:chars (sorted-set \b \c)
              :ranges [[\e \g]]} \b)
(known-char? {:chars (sorted-set \b \c)
              :ranges [[\e \g]]} \f)

(defn alpha-insert-char [as c]
  (if (known-char? as c)
    (throw (ex-info "Character already in alphabet." {:alphabet as
                                                      :char c}))
    (update as :chars #(conj % c))))

;; (u/except (alpha-insert-char (alpha-literal \a) \a) :bang!) 
(alpha-insert-char (alpha-literal \a) \b)
;; (u/except (alpha-insert-char (alpha-range \a \z) \m) :bang!) 
(alpha-insert-char (alpha-range \a \z) \M) 

(defn alpha-insert-set [as cs]
  (reduce alpha-insert-char as cs))

(alpha-insert-set (alpha-literal \a) (sorted-set \b \c \d))
;; (u/except-info (alpha-insert-set (alpha-literal \a) (sorted-set \b \c \d \a)))
(alpha-insert-set (alpha-range \a \z) (sorted-set \B \C \D))
;; (u/except-info (alpha-insert-set (alpha-range \a \z) (sorted-set \B \c \D)))

(defn range-in-alpha? [alpha cmin cmax]
  (or (some #(char-in-range? cmin cmax %) (:chars alpha))
      (some (fn [[cmin' cmax']]
              (or (u/<= cmin' cmin cmax')
                  (u/<= cmin' cmax cmax'))) (:ranges alpha))
      false))

(range-in-alpha? (alpha-range \a \z) \c \p)
(range-in-alpha? (alpha-range \A \Z) \C \a)
(range-in-alpha? (alpha-range \a \z) \c \C)
(range-in-alpha? (alpha-range \a \z) \A \Z)

(defn alpha-insert-range [as cmin cmax]
  (if (range-in-alpha? as cmin cmax)
    (throw (ex-info "Some characters in range already in alphabet." {:alphabet as
                                                                     :range [cmin cmax]}))
    (update as :ranges #(conj % [cmin cmax]))))

;; XXX: we could normalize but that's not really useful
(alpha-insert-range (alpha-literal \a) \b \z)
;; (u/except-info (alpha-insert-range (alpha-range \a \z) \c \p))

(defn alpha-union-impl
  [alpha alphas]
  (if (seq alphas)
    (as-> alpha $
      (alpha-insert-set $ (:chars (first alphas)))
      (reduce (fn [as [cmin cmax]]
                (alpha-insert-range as cmin cmax)) $ (:ranges (first alphas)))
      (recur $ (rest alphas)))
    ;; the end
    alpha))

(defn alpha-union
  "Builds the union of the alphabets `as1` ... `asn`."
  ([as1 as2 & more] (alpha-union-impl as1 (cons as2 more))))

(alpha-union (alpha-literal \a)
             (alpha-literal \e))

(alpha-union (alpha-literal \a)
             (alpha-range \e \z))

;; (u/except-info (alpha-union (alpha-literal \f)
;;                             (alpha-range \e \z)))

(defrecord UnifChar [alpha])

(def unif-char-descr
  {:generator ::unif-char
   :props #{:uniform :atomic}
   :params {:alpha "The alphabet used for generation."}
   :doc "Draw a uniform character in the given alphabet `alpha`."})

(defn alpha-build [alpha]
  (cond
    ;; literal
    (char? alpha)
    (alpha-literal alpha)
    ;; range
    (and (u/tuple? 2 alpha)
         (char? (first alpha))
         (char? (second alpha)))
    (alpha-range (first alpha) (second alpha))
    ;; set (other sequences)
    (and (sequential? alpha)
         (every? char? alpha))
    (alpha-set (into sorted-set alpha))
    :else
    (throw (ex-info "Not a correct alphabet descriptor." {:wrong-alphabet alpha}))))

(defn unif-char
  "Draw a uniform character in the given alphabets `alphas`."
  [& alphas]
  (->UnifChar (apply alpha-union (map alpha-build alphas))))

(defn alpha-indices [alpha]
  (loop [ranges (:ranges alpha), total (dec (count (:chars alpha))), iranges []]
    (if (seq ranges)
      (let [[cmin cmax] (first ranges)
            irange (inc (- (int cmax) (int cmin)))]
        (recur (rest ranges) (+ total irange) (conj iranges [(inc total) (+ total irange)])))
      [total (dec (count (:chars alpha))) iranges])))

(alpha-indices (alpha-union
                (alpha-literal \a)
                (alpha-literal \b)
                (alpha-literal \c)
                (alpha-range \e \g)
                (alpha-range \A \D)))

(defn select-char [ind iset iranges chars ranges]
  (if (<= ind iset)
    (u/seq-nth ind chars)
    (loop [i 0]
      (let [[cmin cmax] (nth ranges i)
            [imin imax] (nth iranges i)]
        (if (<= ind imax)
          (let [ind-in-range (- ind imin)
                char-val (+ (int cmin) ind-in-range)]
            (char char-val))
          (recur (inc i)))))))

(select-char 0 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 1 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 2 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 3 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 4 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 5 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 6 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 7 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 8 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
(select-char 9 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]])
;; (u/except (select-char 10 2 [[3 5] [6 9]] (sorted-set \a \b \c) [[\e \g] [\A \D]]) :bang!)


(defn gen-unif-char [gen ctx]
  (let [[imax iset iranges] (alpha-indices (:alpha gen))
        [ind src'] (prng/next-int (:source ctx) imax)
        ch (select-char ind iset iranges (:chars (:alpha gen)) (:ranges (:alpha gen)))]
    (atomic-gen ch (assoc ctx :source src'))))

(gen-unif-char (unif-char \a \b \c)  {:size 1
                                      :source (prng/make-random 424242)})

(gen-unif-char (unif-char \a \b \c [\e \g]) {:size 1
                                             :source (prng/make-random 424242)})

(extend-type UnifChar
  g/Generator
  (prepare-gen-context [_ ctx] (prepare-atomic-context ctx))  
  (sample [gen ctx] (gen-unif-char gen ctx))
  (describe [gen] unif-char-descr))

(g/generate (unif-char \a \b \c) :size 1 :seed 424242)

