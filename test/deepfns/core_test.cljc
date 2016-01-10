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
;; TODO: get multiple validation working, tricky because every
;;  generated element needs to have the same structure but different vals
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
