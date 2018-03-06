(ns aleatory.prng.source-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer [is deftest testing]])
            
            [aleatory.prng.source :refer [make-random]])

  )



(deftest test-make-random
  (is (= (:count (make-random)) 0))

  (is (= (:last (make-random)) nil))

  (is (= (:index (make-random)) 0))

  (is (= (:seed (make-random 424242)) 424242)))

;; (deftest )




