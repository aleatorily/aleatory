(ns aleatory.gen.boltzmann
  "This namespace implements a fast generator for
  uniform random trees defined by a tree grammar."

  (:require [aleatory.utils :as u]
            [aleatory.prng.source :as prng]
            #?(:clj [aleatory.gen :as g]
               :cljs [aleatory.gen :as g :refer [Generator]])
            [aleatory.gen.atomic :as atomic])

  #?(:clj (:import [aleatory.gen Generator])))

;;{
;; ## Tree grammars
;;
;; }

(defn bintree-grammar
  "A grammar for binary trees, with node labels
  generated by the generator `label-gen`.
  The optional `build-node` function takes 3 argbuments,
   the label, the left subtree and the right subtree,
   and build an internal node. It defaults to [[vector]]."
  ([label-gen] (bintree-grammar label-gen vector))
  ([label-gen build-node]
   {:bintree [:either :tip :node]
    :tip [:const 0 nil]
    :node [:build 1 build-node :label :bintree :bintree]
    :label [:inner label-gen]}))

(defn gentree-grammar
  "A grammar for general trees, with node labels
  generated by the generator `label-gen`.
  The optional `build-node` function takes 2 arguments,
   the label and the forest attached to the node (represented as a list).
  It defaults to [[cons]]."
  ([label-gen] (gentree-grammar label-gen cons))
  ([label-gen build-node]
   {:gentree [:build 1 build-node :label :forest]
    :label [:inner label-gen]
    :forest [:either :gnil :gcons]
    :gnil [:const 0 ()]
    :gcons [:build 0 cons :gentree :forest]}))

(def ott-grammar
  "This is an example grammar for \"one-two-three\" trees."
  {:ottree [:either :one :two :three]
   :one [:const 1 :one]
   :two [:build 2 (fn [t1 t2] [:two t1 t2]) :ottree :ottree]
   :three [:build 3 (fn [t1 t2 t3] [:three t1 t2 t3]) :ottree :ottree :ottree]})

;;{
;; ## Evaluation of functional equation
;;
;; }

(declare eval-elem)

(defn eval-const [size z]
  (if (zero? size)
    1.0
    (Math/pow z size)))

(eval-const 0 2.0)
(eval-const 4 2.0)

(defn eval-either [branches z prev]
  (apply + (map #(eval-elem % z prev) branches)))

(defn eval-build [size elems z prev]
  (* (eval-const size z)
     (apply * (map #(eval-elem % z prev) elems))))

(defn eval-elem [elem z prev]
  (if (and (vector? elem) (seq elem))
    (case (first elem)
      (:either :+ :choice :or) (eval-either (rest elem) z prev)
      (:const :value) (eval-const (second elem) z)
      (:build :node) (eval-build (second elem) (rest (rest elem)) z prev)
      ;; else don't know ...
      1.0)
    ;; recursive reference ? 1.0 by default
    (get prev elem 1.0)))

(eval-elem [:const 0 nil] 0.5 {})
(eval-elem [:const 5 nil] 2.0 {})
(eval-elem [:either :one :two] 0.5 {:one 1.0, :two 2.0})
(eval-elem [:build 3 identity 42 :two :three] 2.0 {:two 2.0 :three 3.0})
(eval-elem [:inner identity] 0.5 {})

(defn eval-grammar [grammar z prev]
  (u/mapkv (fn [ref elem] [ref (eval-elem elem z prev)]) grammar))


;;{
;; ## Singularity Oracle
;;
;; We use the simple newton iteration for testing
;; convergence. A faster iteration could be tried
;; but it's a compile-time performance issue so that's
;; not so critical.
;;
;; }


;; newton iteration
(declare iter)
;; divergence (we went too far)
(declare diverge?)

;; Mr Oracle, please find the singularity
(defn oracle [class zmin zmax eps-iter eps-div]
  ;; (println "[search] zmin=" zmin "zmax=" zmax)
  (if (< (- zmax zmin) eps-iter)
    [zmin (iter class zmin eps-div)]
    (let [z (/ (+ zmin zmax)
               2.0)
          v (iter class z eps-div)]
      ;; (println "  => z=" z "v=" v)
      (if (diverge? v eps-div)
        (recur class zmin z eps-iter eps-div)
        (recur class z zmax eps-iter eps-div)))))

;; distance between vectors v1 and v2
(defn norm [v1 v2]
  (reduce-kv (fn [norm elem y1]
               (let [y2 (get v2 elem)
                     y (Math/abs (- y1 y2))]
                 (if (> y norm) y norm))) 0.0 v1))

(norm {:a 0.1 :b 0.3} {:a 0.2 :b -0.2})

;; iteration until distance is less than `eps` (thank you Mr. Newton)
(defn iter [gram z eps]
  (loop [v1 (u/mapkv (fn [k _] [k 0.0]) gram)]
    (let [v2 (eval-grammar gram z v1)]
      ;; (println "[iter] v2=" v2 "norm=" (norm v1 v2))
      (if (<= (norm v1 v2) eps)
        v2
        (recur v2)))))

;; vector has diverged wrt. eps?
(defn diverge? [v eps]
  (u/some-kv (fn [_ w] (or (< w 0.0) (> w (/ 1.0 eps)))) v))


(oracle ott-grammar 0.0 1.0 0.00001 0.00000001)

(oracle (gentree-grammar identity) 0.0 1.0 0.001 0.000001)

(oracle (bintree-grammar identity) 0.0 1.0 0.001 0.000001)


;;{
;; ## Weighted grammars
;;
;; This is an interesting pre-computation.
;;
;; }


(defn weighted-args [args z weights]
  (let [eargs (mapv (fn [arg] [arg (eval-elem arg z weights)]) args)
        total (apply + (map second eargs))]
    (loop [eargs eargs, acc 0.0, wargs []]
      (if (seq eargs)
        (let [[arg weight] (first eargs)
              acc' (+ acc weight)]
          (recur (rest eargs) acc' (conj wargs [arg (/ acc' total)])))
        ;; no more arg
        wargs))))

(defn weighted-elem [elem z weights]
  (if (and (vector? elem) (seq elem))
    (case (first elem)
      (:either :+ :choice :or)
      (vec (cons ::either (weighted-args (rest elem) z weights)))
      (:const :value) (vec (cons ::const (rest elem)))
      (:build :node) (vec (cons ::build (rest elem)))
      (:inner) (vec (cons ::inner (rest elem)))
      ;; otherwise
      elem)
    elem))

(defn weighted-grammar [class z weights]
  (u/mapkv (fn [ref elem] [ref (weighted-elem elem z weights)]) class))


(defn compile-grammar [grammar eps-iter eps-div]
  (let [[z v] (oracle grammar 0.0 1.0 eps-iter eps-div)
        wgram (weighted-grammar grammar z v)]
    [z v wgram]))

(compile-grammar (bintree-grammar identity) 0.00001 0.000001)

(compile-grammar (gentree-grammar identity) 0.00001 0.000001)

(compile-grammar ott-grammar 0.00001 0.000001)

(defn choose [src choices]
  (let [[x src'] (prng/next-real src)]
    (some (fn [[elem proba]]
            (and (<= x proba) [elem src']))
          choices)))

(choose (prng/make-random 424242) [[:one 0.5] [:two 0.8] [:three 1.0]])

;;{
;; ## Generation of size
;;
;; }

;; Remark: tail recursive
(defn gensize [src wgram maxsize elem]
  (loop [src src, elem elem, size 0, cont '()]
    (cond
      ;; tree is too big
      (>= size maxsize) [-1 src]
      ;; non-tail recursion
      (= elem ::recur) (if (seq cont)
                         (recur src (first cont) size (rest cont))
                         [size src])
      ;; maybe a command
      (and (vector? elem) (seq elem))
      (case (first elem)
        ::either (let [[elem' src'] (choose src (rest elem))]
                   (recur src' elem' size cont))
        ::const (recur src ::recur (+ size (second elem)) cont)
        ::build (let [[_ n _ fst & args] elem]
                  (recur src fst (+ size n) (concat args cont)))
        ;; else don't know ...
        (recur src ::recur size cont))
      :else (if-let [nelem (get wgram elem)]
              (recur src nelem size cont)
              ;; We skip all the elements we don't know about
              (recur src ::recur size cont)))))

(let [[z v wgram] (compile-grammar ott-grammar 0.00001 0.000001)]
  (gensize (prng/make-random 24242) wgram 1000 :ottree))

(defn gensizes [src wgram maxsize elem]
  (let [[size src'] (gensize src wgram maxsize elem)]
    (if (> size 0)
      (lazy-seq (cons [size src] (gensizes src' wgram maxsize elem)))
      (recur src' wgram maxsize elem))))

(let [[z v wgram] (compile-grammar ott-grammar 0.00001 0.000001)]
  (take 20 (map first (gensizes (prng/make-random 24242) wgram 1000 :ottree))))

;;{
;; ## Tree generation
;;
;;}

;; Remark: this function is tail rec, and that was not a walk in the park

(defn gentree-const [csize cvalue size cont]
  (let [[[buildvec buildcont] & cont'] cont]
    [::recur (+ size csize) (cons [(conj buildvec cvalue) buildcont] cont')]))

(defn gentree-data [value size cont]
  (if (seq cont)
    (let [[[buildargs buildcont] & cont'] cont]
      [::recur size (cons [(conj buildargs value) buildcont] cont')])
    (throw (ex-info "Arbitrary data outside of build node" {:data value}))))

(defn gentree-build [bsize bfun bargs size cont]
  [::recur (+ size bsize) (cons [[] [:build bfun bargs]] cont)])

(defn gentree-make [buildargs bfun bargs size cont]
  ;; precondition: cont is non-empty
  (if (seq bargs)
    [(first bargs) size (cons [buildargs [::build bfun (rest bargs)]] cont)]
    ;; no more arguments
    (if (seq cont)
      (let [[[buildargs' buildcont'] & cont'] cont]
        [::recur size (cons [(conj buildargs' (apply bfun buildargs)) buildcont'] cont')])
      [[::result (apply bfun buildargs)] size  cont])))

(defn gentree [ctx wgram elem]
  (loop [ctx ctx, elem elem, size 0, cont '()]
    (cond
      ;; non-tail call
      (= elem ::recur)
      (if (seq cont)
        (let [[[buildargs [_ bfun bargs]] & cont'] cont
              [elem'' size'' cont''] (gentree-make buildargs bfun bargs size cont')]
          (recur ctx elem'' size'' cont''))
        ;; end of recursion
        [size elem ctx])
      ;; recursion
      (contains? wgram elem)
      (recur ctx (get wgram elem) size cont)
      ;; maybe a command
      (and (vector? elem) (seq elem))
      (case (first elem)
        ::result
        [size (second elem) ctx]
        ::either
        (let [[elem' src'] (choose (:source ctx) (rest elem))]
          (recur (assoc ctx :source src') elem' size cont))
        ::const
        (let [[_ csize cvalue] elem]
          (if (seq cont)
            (let [[elem' size' cont'] (gentree-const csize cvalue size cont)]
              (recur ctx elem' size' cont'))
            ;; direct const result
            [csize cvalue ctx]))
        ::build
        (let [[_ bsize bfun & bargs] elem
              [elem' size' cont'] (gentree-build bsize bfun bargs size cont)]
          (recur ctx elem' size' cont'))
        ::inner
        (let [[label labelgen] (if (= (count elem) 3)
                                 [(second elem) (nth elem 2)]
                                 [:inner (second elem)])
              inner-ctx (get ctx label)
              [obj inner-ctx'] (g/sample labelgen inner-ctx)
              ctx' (assoc ctx label (assoc inner-ctx' :size (:size inner-ctx)))]
          (if (g/no-object? obj)
            [0 obj ctx']
            (if (seq cont)
              (let [[elem' size' cont'] (gentree-const 0 (:data obj) size cont)]
                (recur ctx' elem' size' cont'))
              [size (:data obj) ctx'])))
        ;; else don't know ...
        (let [[elem' size' cont'] (gentree-data elem size cont)]
          (recur ctx elem' size' cont')))
      :else ;; arbitrary element
      (let [[elem' size' cont'] (gentree-data elem size cont)]
        (recur ctx elem' size' cont')))))


(let [[z v wgram] (compile-grammar ott-grammar 0.00001 0.000001)]
  (gentree {:source (prng/make-random 24242)} wgram :ottree))

(defn boltzmann [ctx wgram sing elem min-size max-size]
  (let [[size src'] (first (filter (fn [[size _]] (>= size min-size))
                                   (gensizes (:source ctx) wgram max-size elem)))
        [size' tree ctx'] (gentree (assoc ctx :source src') wgram elem)]
    [size tree ctx']))

(defrecord TreeGenerator [wgrammar root weights sing])

(def treegen-descr
  {:generator ::treegen
   :props #{:uniform :deep :controlled}
   :params {:grammar "The tree grammar used for generation."}
   :doc "A generator for tree structures based on boltzmann sampling. The structure is described by the `:grammar` parameter.
The generator is `:controlled` and thus provides some control over the obtained size. In practice it will consume up-to the allowed `size` value. This generator is uniform (as long as the label generators are also uniform)."})

;; by default 10% approximation of size
(def ^:dynamic *min-size-ratio* 0.1)

(defn prepare-size-context [ctx]
  (let [size (get ctx :size)
        min-size (get ctx :min-size)
        ratio-size (get ctx :ratio-size)
        max-size (get ctx :max-size)
        size (or size max-size)
        max-size (or max-size size)
        min-size (or min-size (and max-size (* max-size (or ratio-size *min-size-ratio*))))]
    (if (nil? max-size)
      [false {:message "Missing :size or :max-size in context."
              :context ctx}]
      [true (assoc ctx
                   :size size
                   :min-size min-size
                   :max-size max-size)])))

(defn prepare-treegen-context [gen ctx]
  (let [[ok ctx] (prepare-size-context ctx)]
    (if (not ok)
      [ok ctx]
      (let [inner-gens (reduce-kv (fn [gens _ elem]
                                    (if (and (vector? elem)
                                             (= (first elem) ::inner))
                                      (if (= (count elem) 3)
                                        (assoc gens (second elem) (nth elem 2))
                                        (assoc gens :inner (second elem)))
                                      gens)) {} (:wgrammar gen))
            [ok ctx'] (reduce-kv (fn [[_ ctx] label gen]
                                   (if-let [lblctx (get ctx label)]
                                     (let [[ok ctx'] (g/prepare-context gen lblctx)]
                                       (if (not ok)
                                         (reduced [ok ctx'])
                                         [true (assoc ctx label ctx')]))
                                     ;; no label
                                     [false {:message "Missing label for inner generator in context"
                                             :label label
                                             :context ctx}])) [true ctx] inner-gens)]
        [ok ctx']))))

(extend-type TreeGenerator
  g/Generator
  (prepare-gen-context [gen ctx] (prepare-treegen-context gen ctx))
  (sample [gen ctx]
    ;; (println "ctx=" ctx)
    (let [[size tree ctx'] (boltzmann ctx
                                      (:wgrammar gen)
                                      (:singularity gen)
                                      (:root gen)
                                      (:min-size ctx)
                                      (:max-size ctx))]
      (cond
        (g/no-object? tree) [tree ctx']
        (< size 0) [(g/no-object ::size-not-found {:message "Boltzmann sampling failure."}) ctx']
        :else
        [(g/gen-object tree {:size size}) ctx'])))
  (describe [gen] treegen-descr))

(defn treegen [grammar root eps-iter eps-div]
  (let [[sing weights wgrammar] (compile-grammar grammar eps-iter eps-div)]
    (->TreeGenerator wgrammar root weights sing)))

(first
 (g/generate (treegen ott-grammar :ottree 0.001 0.0001)
             :min-size 10 :max-size 40 :seed 424242))
;; (g/generate (treegen ott-grammar :ottree 0.001 0.0001) :size 40 :seed 424242)

(first
 (g/generate (treegen (bintree-grammar (atomic/unif-int 10 30))
                      :bintree 0.001 0.00001)
             :min-size 10 :max-size 40
             :seed 424242 :inner {:seed 393939 :size 1}))


(first
 (g/generate (treegen (gentree-grammar (atomic/unif-boolean))
                      :gentree 0.001 0.00001)
             :min-size 10 :max-size 40
             :seed 424242 :inner {:seed 393939 :size 1}))

