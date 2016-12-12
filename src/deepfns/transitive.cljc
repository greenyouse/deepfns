(ns deepfns.transitive
  "Functions that combine with transitives for more powerful data
  transformations"
  (:require [clojure.string :as s]
            [deepfns.core :as d]
            #?(:clj [deepfns.utils :refer [binary-op]])
            #?(:cljs [deepfns.utils :refer-macros [binary-op]])))

(defn =>
  "Like the threading macro, some->, this threads data through successive
  functions as long as the result is not nil."
  [& fs]
  (fn [m]
    (loop [[f & fs] fs acc m]
      (if (nil? f)
        acc
        (cond
          (or (sequential? f) (set? f))
          (if-some [vs (map #(get acc %) f)]
            (let [[f2 & fs] fs]
              (if (nil? f2)
                (filter (complement nil?) vs)
                (recur fs (apply f2 vs)))))

          :else
          (if-some [v (f acc)]
            (recur fs v)))))))

(def p>
  "An alias for clojure.core/partial"
  partial)

(defn and>
  "A transitive form of and where ts is any number of transitives. Outputs all
  the nested values when successful."
  [& ts]
  (fn [m]
    (reduce (fn [_ t]
              (let [out (d/<=> t m)]
                (if out
                  out
                  (reduced out))))
      {} ts)))

(defn or>
  "A transitive form of or where ts is any number of transitives."
  [& ts]
  (fn [m]
    (reduce (fn [_ t]
              (when-some [matched-t (d/<=> t m)]
                (reduced matched-t)))
      {} ts)))

(defn default>
  "Similar to or>, it takes some transitives ts and returns
  the first matching one. If none are found then it returns
  the default value."
  [default & ts]
  (fn [m]
    (if-some [or-found ((apply or> ts) m)]
      or-found
      default)))

(binary-op eq> =)
(binary-op not-eq> not=)
(binary-op gt> >)
(binary-op lt> <)
(binary-op gte> >=)
(binary-op lte> <=)

(defn- nil-string [& coll]
  "Convert any nil values to empty strings"
  (map #(if (nil? %) "" %) coll))

#?(:clj
   (defn format>
     "A transitive that formats a string with transitive keyword
  arguments. Any unmatched transitive keywords will be converted
  to empty strings.

  example:
  ((format> \"%s.%s\" :one :two)
   {:one \"foo\" :two \"bar\"})

  => \"foo.bar\""
     [s & ks]
     {:pre [(not-empty ks)]}
     (=> ks
       nil-string
       (partial apply format s))))

(defn str>
  "A transitive version of str that can take transitive keywords
  and strings to compose a larger string."
  [& ks]
  (let [formatter (reduce #(if (string? %2)
                             (str % %2)
                             (str % "%s"))
                    "" ks)
        keywords (filter keyword? ks)]
    (if (empty? keywords)
      (constantly (s/join "" ks))
      (apply format> formatter keywords))))

(defn if>
  "Like if but for transitives. Takes a predicate and returns the then
  or else clause."
  ([pred then]
   (fn [m]
     (let [pred-t (d/<=> pred m)
           then-t (d/<=> then m)]
       (if pred-t
         then-t))))
  ([pred then else]
   (fn [m]
     (let [pred-t (d/<=> pred m)
           then-t (d/<=> then m)
           else-t (d/<=> else m)]
       (if pred-t
         then-t
         else-t)))))

(defn when>
  "A transitive form of when. Takes a predicate and returns the
  transitive clause when successful."
  [pred then]
  (fn [m]
    (let [pred-t (d/<=> pred m)
          then-t (d/<=> then m)]
      (when pred-t
        then-t))))

(defn- to-transitive [exprs]
  (map d/<=> exprs))

(defn- to-val [exprs m]
  (map #(% m) exprs))

(defn map> [f & exprs]
  (let [ts (to-transitive exprs)]
    (fn [m]
      (->> (to-val ts m)
           (apply (partial map f))))))

(defn- trans-vals [exprs m]
  (->> (to-val exprs m)
       (flatten)
       (remove nil?)))

(defn reduce1>
  "A transitive form of reduce where exprs are collections of transitive
  expressions. Here's an example:

  ((reduce1> + [:foo :foo :bar] [:foo]) {:foo 1})

  => 3"
  ([f & exprs]
   (let [ts (to-transitive exprs)]
     (fn [m]
       (let [xs (trans-vals ts m)]
         (when (not (empty? xs))
           (reduce f xs)))))))

(defn reduce2>
  "A transitive form of reduce that uses an initial seed value."
  ([f seed & exprs]
   (let [ts (to-transitive exprs)]
     (fn [m]
       (let [xs (trans-vals ts m)]
         (when (not (empty? xs))
           (reduce f seed xs)))))))

(defn filter>
  "A transitive form of filter where exprs are collections of transitive
  expressions."
  [pred & exprs]
  (let [ts (to-transitive exprs)]
    (fn [m]
      (let [xs (trans-vals ts m)]
        (when (not (empty? xs))
          (filter pred xs))))))

(defn es>
  "Escapes expressions from being evaluated by a transitive

  ((<=> {:foo (es> [:bar :baz])
   {:bar 10})

  => {:foo [:bar :baz]}"
  [& exprs]
  (apply constantly exprs))
