(ns deepfns.core-test
  (:require #?@(:clj [[clojure.test :refer :all]]
                :cljs [[cljs.test :as test :refer [test-var] :refer-macros [is]]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [clojure.test.check.clojure-test #?@(:clj [:refer [defspec]]
                                                 :cljs [:refer-macros [defspec]])]
            [deepfns.core :refer :all]))

;; assertions:
;; 1. identity
;; 2. associativity/functional composition
;; 3. validate all nested types are unchanged
;; TODO: type validation on all nested types
(defspec deepfmap-spec
  (prop/for-all [nums (gen/one-of [(gen/set gen/int)
                                   (gen/list gen/int)
                                   (gen/vector gen/int)
                                   (gen/map gen/keyword gen/int)])
                 anything (gen/vector gen/any 1 5)
                 f (gen/elements [* +])]
    (let [f1 (partial f 1)
          f2 (partial f 2)
          out-identity (deepfmap identity anything)
          out-single1 (deepfmap (comp f2 f1) nums)
          out-single2 (deepfmap (comp f1 f2) nums)]
      (= out-identity anything) ; 1
      (= out-single1 out-single2) ; 2
      ;; for now we're just checking the top level type
      (every? #(= (type nums) (type %)) ; 3
        [out-single1 out-single2]))))

;; Here are some test cases for nested type checking (3)
(deftest deepfmap-types-single
  (are [expected f args]
      (is (= expected (deepfmap f args)))
    nil identity nil

    [2] inc [1]

    #{1 '(2) [3]} inc #{0 '(1) [2]}

    {:a 1 :b 2} inc {:a 0 :b 1}

    {:foo [#{[1]}]} inc {:foo [#{[0]}]}))

;; and to cover the variadic version of deepfmap
(deftest deepfmap-types-multi
  (are [expected f arg1 arg2 arg3]
      (is (= expected (deepfmap f arg1 arg2 arg3)))
    nil identity nil nil nil

    [[1] [2] [3]] inc [0] [1] [2]

    [#{1 '(2) [3]} #{5 '(6) [7]} #{8 '(9) [10]}]
    inc #{0 '(1) [2]} #{4 '(5) [6]} #{7 '(8) [9]}

    [{:foo [#{[1]}] :b 2} {:bar[#{[3]}] :k 4} {:bazz [#{[5]}] :z 6}]
    inc {:foo [#{[0]}] :b 1} {:bar[#{[2]}] :k 3} {:bazz [#{[4]}] :z 5}))
