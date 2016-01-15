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
          out-multi (deepfmap identity args-multi)]
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
          out1 (deepfmap (comp f1 f2) args)
          out2 (deepfmap (comp f2 f1) args)]
      (and
        (is (= out1 out2)
          (format "fc failed for deepfmap"))))))

;; Here are some test cases for nested type checking (3)
(deftest deepfmap-types-single
  (are [expected f args]
      (is (= expected (deepfmap f args)))
    nil identity nil

    [2] inc [1]

    [[2] [3] [4]] inc (lazy-seq [[1] [2] [3]])

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fapply

;; TODO: abstract these tests better to cut down on the redundant code

;; assertions:
;; 1. identity
;; 2. functional composition
;; 3. homomorphism
;; 4. interchange

;; covers the identity law (1)
(defspec deepfapply-identity
  (prop/for-all [set-args (gen/set nums {:min-elements 1})
                 vec-args (gen/vector nums 1 5)
                 lst-args (gen/not-empty (gen/list nums))
                 seq-args (gen/not-empty
                            (gen/fmap (fn [n]
                                        (seq n))
                              nums))
                 map-args (gen/map (gen/return :a) nums {:min-elements 1})]
    (let [set-out (deepfapply (pure set-args identity) set-args)
          vec-out (deepfapply (pure vec-args identity) vec-args)
          lst-out (deepfapply (pure lst-args identity) lst-args)
          seq-out (deepfapply (pure seq-args identity) seq-args)
          map-out (deepfapply (pure map-args identity)  map-args)]
      (and
        (is (= set-args set-out)) "Set output differs"
        (is (= vec-args vec-out) "Vector output differs")
        (is (= lst-args lst-out) "List output differs")
        (is (= seq-args seq-out) "Seq output differs")
        (is (= map-args map-out) "Map output differs")))))

(defn build-fn [[m] f]
  (cond
    (set? m) #{f}
    (vector? m) [f]
    (list? m) (list f)
    (seq? m) (seq [f])
    (map? m) {:a f}))

(def gen-coll-ints
  "Generates vector of collections of ints for testing"
  (gen/one-of
    [(gen/vector (gen/map (gen/return :a) gen/int {:min-elements 1}) 1 5)
     (gen/vector (gen/vector gen/int 1) 1 5)
     (gen/vector (gen/fmap list gen/int) 1 5)
     (gen/vector (gen/set gen/int {:min-elements 1
                                   :max-elements 1}) 1 5)
     (gen/vector (gen/fmap (fn [n]
                             (seq [n])) gen/int)
       1 5)]))

;; check for functional composition (2)
;; Just checking top level for now, nested is covered by unit tests
(defspec deepfapply-composition
  (prop/for-all [fs (gen/elements [+ *])
                 arg gen-coll-ints]
    (let [f1 (build-fn arg (partial fs 5))
          f2 (build-fn arg (partial fs 10))
          out1 (->> arg
                 (apply deepfapply f1)
                 (deepfapply f2))
          out2 (->> arg
                 (apply deepfapply f2)
                 (deepfapply f1))]
      (true?
        (is (= out1 out2)
          (format "deefapply composition output not equal: out1 - %s, out2 - %s" out1 out2))))))

(defn expected-val [[m :as coll] f]
  (cond
    (or (set? m)
        (vector? m)
        (list? m)
        (seq? m)) (conj (empty m) (reduce f (mapcat identity coll)))
    :else
    {:a (reduce f (flatten (map vals coll)))}))

;; test for homomorphism (3)
(defspec deepfapply-homomorphism
  (prop/for-all [f (gen/elements [* +])
                 args gen-coll-ints]
    (let [out (apply deepfapply (build-fn args f) args)
          expected (expected-val args f)]
      (true?
        (is (= out expected)
          (format "deepfapply homomorphism not equal: op - %s, args - %s" f args))))))

;; TODO: get some coverage for variadic interchange
;; test for interchange (4)
(defspec deepfapply-interchange
  (prop/for-all [args gen-coll-ints]
    (let [x (first args)
          f (pure (empty x) identity)
          out1 (deepfapply (pure (empty x) #(% x)) f)
          out2 (deepfapply f (pure (empty x) x))]
      (and
        (is (= out1 out2) "deepfapply interchange not equal")))))

(deftest deepfapply-types-single
  (are [expected f arg]
      (is (= expected (deepfapply f arg)))
    [nil] [identity] [nil]

    #{[1] [2] [3]} #{[inc]} #{[0] [1] [2]}

    {:a 2 :b 2} {:a inc :b identity} {:a 1 :b 2}

    '({:a 2 :b 2} {:a 4 :b 4})
    '({:a inc :b identity}) '({:a 1 :b 2} {:a 3 :b 4})

    [2 3 4 0 1 2] [inc dec] [1 2 3]

    [{:a [2 2] :b 2} {:a [4 4] :b '(4)}]
    [{:a [inc inc]}] [{:a [1] :b 2} {:a [3] :b '(4)}]))

(deftest deepfapply-types-multi
  (are [expected f arg1 arg2 arg3]
      (is (= expected (deepfapply f arg1 arg2 arg3)))
    [6] [+] [1] [2] [3]

    #{'([6])} #{'([+])} #{'([1])} #{'([2])} #{'([3])}

    #{[12 18]} #{[(partial * 2) (partial * 3)]} #{[1]} #{[2]} #{[3]}

    {:a 6 :b 4}
    {:a +} {:a 1} {:a 2} {:a 3 :b 4}

    [{:a #{18} :b {:c {:d [28]}}}]
    [{:a #{*} :b {:c {:d [*]}}}]
    [{:a #{1 2}}] [{:a #{3} :b {:c {:d [4 5]}}}] [{:a #{6} :b {:c {:d [7]}}}]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pure + deeppure

(deftest pure-types
  (are [expected coll x]
      (is (= expected (pure coll x)))

    nil nil nil
    [nil] [] nil
    '(nil) '() nil
    {nil nil} {} nil

    [[1]] [] [1]
    '("foo") '() "foo"
    {nil {:a 1}} {} {:a 1}

    [1] [[1]] 1
    '("foo") '(["bar"]) "foo"
    {nil '(1)} {"foo" "bar"} '(1)
    {nil 1} {:a ["foo"]} 1))

(deftest deeppure-types
  (are [expected coll x]
      (is (= expected (deeppure coll x)))
    nil nil nil
    [nil] [] nil
    '(nil) '() nil
    {nil nil} {} nil

    [[1]] [] [1]
    '("foo") '() "foo"
    {nil {:a 1}} {} {:a 1}

    [[1]] [[1]] 1
    '(["foo"]) '(["bar"]) "foo"
    {"foo" '(1)} {"foo" "bar"} '(1)

    [1 [1] 1] ["foo" [] "bar"] 1

    (list 1 {:b [{nil 1} 1 (list 1)]} 1)
    (list "foo" {:b [{} "bar" (list "fizz")]} "buzz") 1

    {:a "test" :b ["test" '("test")]}
    {:a 10 :b [1 '()]} "test"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filterapply

(deftest filterapply-types-single
  (are [expected f arg1]
      (is (= expected (filterapply f arg1)))
    [nil] [identity] [nil]

    #{[1] [2] [3]} #{[inc]} #{[0] [1] [2]}

    {:a 2 :b 2} {:a inc :b identity} {:a 1 :b 2}

    '({:a 2 :b 2} {:a 4 :b 4})
    '({:a inc :b identity}) '({:a 1 :b 2} {:a 3 :b 4})

    [2 3 4 0 1 2] [inc dec] [1 2 3]

    [{:a [2 2] :b 2} {:a [4 4] :b '(4)}]
    [{:a [inc inc]}] [{:a [1] :b 2} {:a [3] :b '(4)}]

    {:a 2} {:a inc} {:a 1 :b 2 :c 3}

    {:a 2 :b [2 0] :c {:d 5}}
    {:a inc :b [inc dec] :c {:d identity}} {:a 1 :b [1] :c {:d 5}}))

(deftest filterapply-types-multi
  (are [expected f arg1 arg2 arg3]
      (is (= expected (filterapply f arg1 arg2 arg3)))
    [6] [+] [1] [2] [3]

    #{'([6])} #{'([+])} #{'([1])} #{'([2])} #{'([3])}

    #{[12 18]} #{[(partial * 2) (partial * 3)]} #{[1]} #{[2]} #{[3]}

    {:a 6}
    {:a +} {:a 1} {:a 2} {:a 3 :b 4}

    [{:a #{18} :b {:c {:d [28]}}}]
    [{:a #{*} :b {:c {:d [*]}}}]
    [{:a #{1 2}}] [{:a #{3} :b {:c {:d [4 5]}}}] [{:a #{6} :b {:c {:d [7]}}}]

    {:a 6 :b [6 6] :c {:d [1 2 3]}}
    {:a + :b [+ *] :c {:d vector}}
    {:a 1 :b [1] :c {:d 1} :e 1}
    {:a 2 :b [2] :c {:d 2 :f 2}}
    {:a 3 :b [3] :c {:d 3}}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transitive

;; TODO: Write some laws for transitive behavior
(deftest transitive-types-single
  (are [expected trav arg]
      (is (= expected ((transitive trav) arg)))
    nil nil {:foo "bar"}

    {:foo "bar"} {:foo "bar"} {:fizz "buzz"}

    {:foo {:bar 2} :fizz {:buzz 2}}
    {:foo {:bar 2} :fizz {:buzz 2}}
    {:foo "bar"}

    {:foo {:bar 2}} {:foo {:bar inc}} 1

    [1 [2 {:fizz 3}]]
    [:foo [:bar {:fizz :buzz}]]
    {:foo 1 :bar 2 :buzz 3}

    {:foo {:bar 1} :fizz 2 :buzz [3 "foo"]}
    {:foo {:bar :1} :fizz :2 :buzz [:3 "foo"]}
    {:1 1 :2 2 :3 3}

    {"foo" 1} {"foo" :bar} {:bar 1}))
