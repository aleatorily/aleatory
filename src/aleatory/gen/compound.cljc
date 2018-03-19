
(ns aleatory.gen.compound
  "Compound generators are implemented in this namespace.
  
  A compound generator describes a class  containers containing
  random-generated objects. The size of a compound container is the sum
  of the sizes of its elements."
  
  (:require [aleatory.prng.source :as prng]
            #?(:clj [aleatory.gen :as g]
               :cljs [aleatory.gen :as g :refer [Generator]])
            [aleatory.gen.atomic :as atomic])

  #?(:clj (:import [aleatory.gen Generator])))

;;{
;; ## Vectors
;;}

(defrecord Vector [elem])

(def vector-descr
  {:generator ::vector
   :props #{:uniform :compound :sized}
   :params {:elem "The generator for elements."}
   :doc "A generator for vectors of random objects described by the `elem` parameter.
The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
 a timeout is reached).
This generator also preserves uniformity."})

(defn vector-gen
  "A generator for vectors of atomic values described by the `elem` parameter.
  The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
  a timeout is reached).
  This generator also preserves uniformity."
  [elem]
  (->Vector elem))

(defn prepare-vector-context [gen ctx]
  (let [[ok ctx] (if-let [size (get ctx :size)]
                   (if (and (integer? size)
                            (>= size 0))
                     [true ctx]
                     [false {:message "The :size field of context should be a positive integer"
                             :size size}])
                   [false {:message "Missing :size field in context." :ctx ctx}])]
    (if (not ok)
      [ok ctx]
      (let [[ok ctx] (if-let [elem-ctx (get ctx :elem)]
                       (if-let [size (get elem-ctx :size)]
                         (if (and (integer? size)
                                  (>= size 0))
                           [true ctx]
                           [false {:message "The :size field of element context should be a positive integer"
                                   :size size}])
                         [false {:message "Missing :size field in element context." :elem-ctx elem-ctx}])
                       [false {:message "Missing :elem field in context (compound generation)." :ctx ctx}])]
        (if (not ok)
          [ok ctx]
          (let [elem-ctx (:elem ctx)
                elem-ctx (if (or (get elem-ctx :source)
                                 (get elem-ctx :seed)
                                 (get elem-ctx :reseed))
                           elem-ctx
                           (assoc elem-ctx
                                  :source (:source ctx)
                                  :seed (:seed ctx)))
                [ok elem-ctx] (g/prepare-context (:elem gen) elem-ctx)]
            (if (not ok)
              [ok elem-ctx]
              [ok (assoc ctx :elem elem-ctx)])))))))

(defn generate-vector [elem-gen ctx]
  (loop [ctx ctx, v []]
    (if (zero? (:size ctx))
      [(g/gen-object v {:size (count v)}) ctx]
      (let [[obj elem-ctx' :as ret] (g/sample elem-gen (:elem ctx))]
        (if (g/no-object? obj)
          [obj (assoc ctx :elem elem-ctx')]
          (recur (assoc ctx
                        :size (dec (:size ctx))
                        :elem (assoc elem-ctx' :size (:size (:elem ctx))))
                 (conj v (:data obj))))))))

(extend-type Vector
  g/Generator
  (prepare-gen-context [gen ctx] (prepare-vector-context gen ctx))
  (sample [gen ctx] (generate-vector (:elem gen) ctx))
  (describe [gen] (assoc vector-descr
                         :elements (g/describe (:elem gen)))))

(g/generate (vector-gen (aleatory.gen.atomic/unif-boolean))
            :size 10 :seed 424242 :elem {:size 1 :seed 424242})

(g/generate (vector-gen (aleatory.gen.atomic/unif-int 10 50))
            :size 10 :seed 424242 :elem {:size 1 :seed 424242})

(g/generate (vector-gen (aleatory.gen.atomic/unif-int 10 50))
            :size 10 :seed 424242 :elem {:size 1})

(g/generate (vector-gen (aleatory.gen.atomic/unif-int 10 50))
            :size 10 :seed 424242 :elem {:size 1 :reseed true})


(g/generate (vector-gen (vector-gen (aleatory.gen.atomic/unif-int 10 50)))
            :size 5 :seed 424242 :elem {:size 5 :seed 393939 :elem {:size 1 :seed 12345}})

(g/generate (vector-gen (vector-gen (aleatory.gen.atomic/unif-int 10 50)))
            :size 5 :elem {:size 5 :elem {:size 1}})



;;{
;; ## Tuples
;;}

;; (defrecord Tuple [elems])

;; (def tuple-descr
;;   {:generator ::tuple
;;    :props #{:uniform :compound :sized}
;;    :params {:elems "An array of generator contexts for the tuple elements."}
;;    :doc "A generator for tuple of random objects described by the `elems` parameter.
;; The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
;;  a timeout is reached).
;; This generator also preserves uniformity."})

;; (defn tuple-gen
;;   "A generator for tuples of elements described by the `elems` parameters.
;;   The generator is `:sized` and thus consumes, for its element count, the whole remaining fuel size (unless
;;   a timeout is reached).
;;   This generator also preserves uniformity."
;;   [& elems]
;;   (->Vector elems))

;; (defn prepare-tuple-element-context [gen ctx elem-gen elem-ctx]
;;   (let [elem-ctx (if (or (get elem-ctx :source)
;;                          (get elem-ctx :seed)
;;                          (get elem-ctx :reseed))
;;                    elem-ctx
;;                    (assoc elem-ctx
;;                           :source (:source ctx)
;;                           :seed (:seed ctx)))]
;;     (g/prepare-context elem-gen elem-ctx)))

;; (defn prepare-tuple-context [gen ctx]
;;   (let [[ok elems-ctx]
;;         (if-let [elems-ctx (get ctx :elems)]
;;           (reduce (fn [[ok elems-ctx] [ok' elem-ctx]]
;;                     (if (not ok')
;;                       (reduced [ok' elem-ctx])
;;                       [ok (conj elems-ctx elem-ctx)]))
;;                   [true []]
;;                   (map #(prepare-tuple-element-context gen ctx %1 %2) (:elems gen) elems-ctx))
;;           [false {:message "Missing :elems field in context (tuple generation)." :ctx ctx}])]
;;     (if (not ok)
;;       [ok ctx]
;;       [ok (assoc ctx :elems elems-ctx)])))

;; (defn generate-tuple [elems-gen ctx]
;;   (loop [ctx ctx, elems-gen elems-gen, idx 0, tup []]
;;     (if (seq elems-gen)
;;       (let [elem-gen (first elems-gen)]
;;         (let [[obj elem-ctx' :as ret] (g/sample elem-gen (nth (:elems ctx) idx))
;;               elems-ctx' (assoc (:elems ctx) idx elem-ctx')]
;;           (if (g/no-object? obj)
;;             [obj (assoc ctx :elem elems-ctx')]
;;             (recur (assoc ctx
;;                           :elems (assoc ctx :elems elem-ctx'))
;;                    (rest elems-gen)
;;                    (inc idx)
;;                    (conj tup (:data obj))))))
;;       ;; end of tuple
;;       [(g/gen-object tup {:size (count tup)}) ctx])))

;; (extend-type Vector
;;   g/Generator
;;   (prepare-gen-context [gen ctx] (prepare-compound-context gen ctx))
;;   (sample [gen ctx] (generate-vector (:elem gen) ctx))
;;   (describe [gen] (assoc vector-descr
;;                          :elements (g/describe (:elem gen)))))

;; (g/generate (vector-gen (aleatory.gen.atomic/unif-boolean))
;;             :size 10 :seed 424242 :elem {:size 1 :seed 424242})

;; (g/generate (vector-gen (aleatory.gen.atomic/unif-int 10 50))
;;             :size 10 :seed 424242 :elem {:size 1 :seed 424242})

;; (g/generate (vector-gen (aleatory.gen.atomic/unif-int 10 50))
;;             :size 10 :seed 424242 :elem {:size 1})

;; (g/generate (vector-gen (aleatory.gen.atomic/unif-int 10 50))
;;             :size 10 :seed 424242 :elem {:size 1 :reseed true})


;; (g/generate (vector-gen (vector-gen (aleatory.gen.atomic/unif-int 10 50)))
;;             :size 5 :seed 424242 :elem {:size 5 :seed 393939 :elem {:size 1 :seed 12345}})

;; (g/generate (vector-gen (vector-gen (aleatory.gen.atomic/unif-int 10 50)))
;;             :size 5 :elem {:size 5 :elem {:size 1}})


