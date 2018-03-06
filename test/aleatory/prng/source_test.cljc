(ns aleatory.prng.source-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer [is deftest testing]])
            
            [aleatory.prng.source :refer [make-random
                                          integer-bounds
                                          next-uint32
                                          rand-uint32-seq]])

  )



(deftest test-make-random
  (is (= (:count (make-random))
         0))

  (is (= (:last (make-random))
         nil))

  (is (= (:index (make-random))
         0))

  (is (= (:seed (make-random 424242))
         424242)))

(deftest test-integer-bounds
  (is (= (integer-bounds 10 100 25043)
         33)))

(deftest test-uint32
  (is (= (first (next-uint32 (make-random 424242)))
         2133873575))

  (is (= (first (next-uint32 (second (next-uint32 (make-random 424242)))))
         722872296))

  (is (= (first (next-uint32 (make-random 424242) 10 100))
         45))

  (is (= (first (next-uint32 (make-random 424242) 90))
         35))

  (is (= (:count (second (next-uint32 (make-random 424242))))
         1))

  (is (= (:count (second (next-uint32 (second (next-uint32 (make-random 424242))))))
         2))

  (is (= (:index (second (next-uint32 (make-random 424242))))
         31))

  (is (= (:last (second (next-uint32 (make-random 424242))))
         nil))

  )

(deftest test-rand-uint32-seq

  (is (= (take 20 (rand-uint32-seq (make-random 424242)))
         '(2133873575 722872296 3214212552 125208362 1379361719 2135620276 1809286372 2366187820 3257434320 2034407037 648070172 905400172 1169129833 4291933519 203481632 5547170 519434368 2601812636 2951870883 3440755965))))




