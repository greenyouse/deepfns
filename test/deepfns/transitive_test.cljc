(ns deepfns.transitive-test
  (:require #?@(:clj [[clojure.test :refer :all]]
                :cljs [[cljs.test :as test :refer [test-var]
                        :refer-macros [are is deftest]]])
            [deepfns.transitive :refer [=> eq> p> if> when> map>
                                        reduce1> reduce2> filter>]]))

(deftest =>-test
  (are [expected fs m]
      (is (= expected ((apply => fs) m)))
    nil [:foo] nil

    1 [:foo] {:foo 1}

    2 [:foo :bar] {:foo {:bar 2}}

    2 [:foo inc] {:foo 1}

    "fizz" [:foo :bar] {:foo {:bar "fizz" :fizz 1} :buzz 2}))

(deftest eq>-test
  (are [expected expr1 expr2 m]
      (is (= expected ((eq> expr1 expr2) m)))
      true nil nil {}

      true {:foo :bar} {:foo 1} {:bar 1}

      false {:fizz :bar} {:foo 1} {:bar 1}

      true {:foo :bar} {:foo :bar} {:bar 1}

      true [:foo :bar] [:bar] {:bar 1}))

(deftest if>-test
  (are [expected pred then else m]
      (is (= expected ((if> pred then else) m)))
      nil nil nil nil {}

      false nil true false {}

      true {:foo :bar} true false {:bar 1}

      true (eq> {:foo :bar} {:foo 1}) true false {:bar 1}

      {:woot 1} (eq> {:foo :bar} {:foo 1})
      {:woot :bar} "bad" {:bar 1}

      "bad" (eq> {:foo :bar} {:foo 1})
      {:woot :bar} "bad" {:bar 2}))

(deftest when>-test
  (are [expected pred then m]
      (is (= expected ((when> pred then) m)))
    nil nil nil {}

    nil nil true {}

    true {:foo :bar} true {:bar 1}

    true (eq> {:foo :bar} {:foo 1}) true {:bar 1}

    {:woot 1} (eq> {:foo :bar} {:foo 1}) {:woot :bar} {:bar 1}

    nil (eq> {:foo :bar} {:foo 1}) {:woot :bar} {:bar 2}))

(deftest map>-test
  (are [expected f expr1 expr2 m]
      (is (= expected ((map> f expr1 expr2) m)))
    [] + nil nil {}

    [] + [:foo] [:bar] {}

    [] + [:foo] [:bar] {:foo 1}

    [3] + [:foo] [:bar] {:foo 1 :bar 2}

    [2] + [:foo] [:foo] {:foo 1 :bar 2}

    [2] + [:foo :bar] [:foo] {:foo 1 :bar 2}

    [2 3] + [:foo :bar] [:foo :foo] {:foo 1 :bar 2}))

(deftest reduce1>-test
  (are [expected f expr1 expr2 m]
      (is (= expected ((reduce1> f expr1 expr2) m)))
    nil + nil nil {}

    nil + [:foo] [:bar] {}

    1 + [:foo] [:bar] {:foo 1}

    3 + [:foo] [:bar] {:foo 1 :bar 2}

    4 + [:foo :bar] [:foo] {:foo 1 :bar 2}

    5 + [:foo :bar] [:foo :foo] {:foo 1 :bar 2}))

(deftest reduce2>-test
  (are [expected f expr1 expr2 m]
      (is (= expected ((reduce2> f 10 expr1 expr2) m)))
    nil + nil nil {}

    nil + [:foo] [:bar] {}

    11 + [:foo] [:bar] {:foo 1}

    13 + [:foo] [:bar] {:foo 1 :bar 2}

    14 + [:foo :bar] [:foo] {:foo 1 :bar 2}

    15 + [:foo :bar] [:foo :foo] {:foo 1 :bar 2}))

(deftest filter>-test
  (are [expected pred expr1 expr2 m]
      (is (= expected ((filter> pred expr1 expr2) m)))
    nil nil nil nil {}

    nil nil [:foo] [:bar] {}

    nil even? [:foo] [:bar] {:bazz (range 5)}

    [0 2 4] even? [:foo] [:bar] {:foo (range 5)}

    [0 2 4 0 2 4] even? [:foo] [:foo] {:foo (range 5)}

    [0 2 4 6] even? [:foo] [:bar] {:foo (range 5) :bar 6}

    [0 2 4 6 6] even? [:foo] [:bar :bar] {:foo (range 5) :bar 6}))
