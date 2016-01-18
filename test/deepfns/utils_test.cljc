(ns deepfns.utils-test
  (:require #?@(:clj [[clojure.test :refer :all]]
                :cljs [[cljs.test :as test :refer [test-var] :refer-macros [is deftest]]])
            [deepfns.utils :refer :all]))

(deftest save-meta-roundtrip
  (are [expected arg]
      (and
        (is (= arg (save-meta arg arg)))
        (is (= expected (meta (save-meta arg arg)))))
    nil {}

    {:foo true} ^{:foo true} {}

    {:foo true} ^{:foo true} []

    {:foo true} (with-meta '() {:foo true})

    {:fizz true :buzz true}
    ^{:fizz true :buzz true} {}))

(deftest save-metas-rountrip
  (are [expected arg1 arg2 arg3]
      (let [as [arg1 arg2 arg3]]
        (and
          (is (= as (save-metas as as)))
          (is (= expected (meta (save-metas as as))))))
    nil {} {} {}

    {:foo true} ^{:foo true} {} {} {}

    {:foo true :bar true :fizz true}
    ^{:foo true} {} ^{:bar true} {} ^{:fizz true} {}

    {:foo 2}
    ^{:foo true} {} ^{:foo 1} {} ^{:foo 2} {}

    {:foo true :bar true :fizz true}
    ^{:foo true} [] ^{:bar true} [] ^{:fizz true} []

    {:foo true :bar true :fizz true}
    (with-meta '() {:foo true})
    (with-meta '() {:bar true})
    (with-meta '() {:fizz true})))
