(ns aleatory.utils
  "Miscellaneous utilies.

  Remark: the functions in this namespace should not be imported undecorated."

  (:refer-clojure :exclude [= < > <= >=]))


(defn mapkv
  "Apply the binary function `f` to the `[k, v]` key-value
  pairs of the map `m`."
  [f m]
  (into {} (map (fn [[k v]] (f k v)) m)))

(defn some-kv [pred m]
  (reduce-kv (fn [res k v]
               (let [r (pred k v)]
                 (if r
                   (reduced r)
                   res))) nil m))

(defn tuple? [n v]
  (and (vector? v)
       (clojure.core/= (count v) n)))

(defn seq-nth [n s]
  (first (drop n s)))

(seq-nth 2 '(:a :b :c :d :e))

(defn all-compares? [expected? c1 c2 cs]
  (let [cmp (compare c1 c2)]
    (cond
      (not (expected? cmp)) false
      (seq cs) (recur expected? c2 (first cs) (rest cs))
      :else true)))

;; (all-compares? neg? \a \b '(\c \d \e))
;; => true

;; (all-compares? neg? \a \b '(\c \e \d))
;; => false

(defn = [c1 c2 & cs]
  (all-compares? zero? c1 c2 cs))

;; (= 3 (+ 2 1) (dec 4) (/ 9 3))
;; => true

(defn < [c1 c2 & cs]
  (all-compares? neg? c1 c2 cs))

(defn <= [c1 c2 & cs]
  (all-compares? #(or (zero? %) (neg? %)) c1 c2 cs))

(defn > [c1 c2 & cs]
  (all-compares? pos? c1 c2 cs))

(defn >= [c1 c2 & cs]
  (all-compares? #(or (zero? %) (pos? %)) c1 c2 cs))

#?(:clj (defmacro except [expr catch-form]
          `(try ~expr
                (catch clojure.lang.ExceptionInfo ~'_ ~catch-form)))
   :cljs (defmacro except [expr catch-form]
          `(try ~expr
                (catch ExceptionInfo ~'_ ~catch-form))))

;; (macroexpand-1 '(except (/ 2 0) 42))
;; => (try (/ 2 0) (catch java.lang.Exception _ 42))

;; (except (/ 2 2) 42)
;; => 1

;; (except (/ 2 0) 42)
;; => 42


#?(:clj (defmacro except-info [expr]
          `(try ~expr
                (catch clojure.lang.ExceptionInfo e# (assoc (ex-data e#) :message (.getMessage e#)))))
   :cljs (defmacro except-info [expr catch-form]
          `(try ~expr
                (catch ExceptionInfo e# (assoc (ex-data e#) :message (.getMessage e#))))))

;; (except-info (throw (ex-info "the message" {:the :data})))
;; => {:the :data, :message "the message"}
