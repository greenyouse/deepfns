(ns deepfns.transitive
  "Functions that combine with transitives for more powerful data
  transformations"
  (:require [deepfns.core :as d]))

(defn =>
  "Like the threading macro, some->, this threads data through successive
  functions as long as the result is not nil."
  [& fs]
  (fn [m]
    (loop [fs fs acc m]
      (if (empty? fs)
        acc
        (if-some [v ((first fs) acc)]
          (recur (rest fs) v))))))

(def p>
  "An alias for clojure.core/partial"
  partial)

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

;; I know eq is usually for checking memory equality... I just like
;; having => for threading and this seemed like a good second choice
;; I'd take suggestions for a new name too
(defn eq>
  "A transative form of = where both arguments are transitive
  expressions"
  [x-expr y-expr]
  (let [x-t (d/<=> x-expr)
        y-t (d/<=> y-expr)]
    (fn [m]
      (= (x-t m) (y-t m)))))

(defn if>
  "Like if but for transitives. Takes a predicate and returns the then
  or else clause."
  ([pred then]
   (fn [m]
     (let [pred-t (d/<=> pred)
           then-t (d/<=> then)]
       (if (pred-t m)
         (then-t m)))))
  ([pred then else]
   (fn [m]
     (let [pred-t (d/<=> pred)
           then-t (d/<=> then)
           else-t (d/<=> else)]
       (if (pred-t m)
         (then-t m)
         (else-t m))))))

(defn when>
  "A transitive form of when. Takes a predicate and returns the
  transitive clause when successful."
  [pred then]
  (fn [m]
    (let [pred-t (d/<=> pred)
          then-t (d/<=> then)]
      (when (pred-t m)
        (then-t m)))))

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
