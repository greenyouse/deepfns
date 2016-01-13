(ns deepfns.core-test
  (:require #?@(:clj [[clojure.test :refer :all]]
                :cljs [[cljs.test :as test :refer [test-var] :refer-macros [is]]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [clojure.test.check.clojure-test #?@(:clj [:refer [defspec]]
                                                 :cljs [:refer-macros [defspec]])]
            [deepfns.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fmap

;; assertions:
;; 1. identity
;; 2. associativity/functional composition
;; 3. validate all nested types are unchanged
;; TODO: type validation on all nested types
(defn repeat-rand [n x]
  (let [cnt (rand-nth (range 2 n))]
    (repeat cnt x)))

(defspec deepfmap-identity
  (prop/for-all [any-single gen/any]
    (let [out-single (deepfmap identity any-single)
          args-multi (map vector (repeat-rand 6 any-single))
          out-multi (apply (partial deepfmap identity) args-multi)]
      (and
        (is (= any-single out-single)
          (format "Single identity failed for deepfmap: input - %s, output - %s" any-single out-single))
        (is (= args-multi out-multi)
          (format "Variadic identity failed for deepfmap: input - %s, output - %s" args-multi out-multi))))))

(def nums
  "Generates some arbitrarily nested datastructure full of ints"
  (gen/recursive-gen
    (fn [n]
      (gen/one-of [(gen/set n)
                   (gen/list n)
                   (gen/vector n)
                   (gen/map gen/keyword n)]))
    gen/int))

(defspec deepfmap-fc
  (prop/for-all [f (gen/elements [* +])
                 n nums]
    (let [args (repeat-rand 6 n)
          f1 (partial f 10)
          f2 (partial f 20)
          out1 (apply (partial deepfmap (comp f1 f2)) args)
          out2 (apply (partial deepfmap (comp f2 f1)) args)]
      (and
        (is (= out1 out2)
          (format "fc failed for deepfmap"))))))

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
    [[nil] [nil] [nil]] identity [nil] [nil] [nil]

    ['(1) '(2) '(3)] identity '(1) '(2) '(3)

    [[1] [2] [3]] inc [0] [1] [2]

    [#{1 '(2) [3]} #{5 '(6) [7]} #{8 '(9) [10]}]
    inc #{0 '(1) [2]} #{4 '(5) [6]} #{7 '(8) [9]}

    {:foo [#{[1]}] :b 2 :bar [#{[3]}] :k 4 :bazz [#{[5]}] :z 6}
    inc {:foo [#{[0]}] :b 1} {:bar[#{[2]}] :k 3} {:bazz [#{[4]}] :z 5}))
