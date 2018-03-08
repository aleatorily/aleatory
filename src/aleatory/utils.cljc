(ns aleatory.utils
  "Miscellaneous utilies.

  Remark: the functions in this namespace should not be imported undecorated."

  (:refer-clojure :exclude [= < > <= >=])
  )

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
                (catch Exception ~'_ ~catch-form)))
   :cljs (defmacro except [expr catch-form]
          `(try ~expr
                (catch js/Object ~'_ ~catch-form))))

;; (macroexpand-1 '(except (/ 2 0) 42))
;; => (try (/ 2 0) (catch java.lang.Exception _ 42))

;; (except (/ 2 2) 42)
;; => 1

;; (except (/ 2 0) 42)
;; => 42


#?(:clj (defmacro except-info [expr]
          `(try ~expr
                (catch Exception e# (assoc (ex-data e#) :message (.getMessage e#)))))
   :cljs (defmacro except-info [expr catch-form]
          `(try ~expr
                (catch js/Object e# (assoc (ex-data e#) :message (.getMessage e#))))))

;; (except-info (throw (ex-info "the message" {:the :data})))
;; => {:the :data, :message "the message"}
