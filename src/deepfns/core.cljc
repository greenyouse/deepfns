(ns deepfns.core
  "General, category theory functions plus a few new ones."
  (:require [deepfns.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fmap

(defn- group-vals [ms k]
  (let [matches (map #(find % k) ms)]
    (when-not (every? nil? matches)
      (vals (remove nil? matches)))))

(defn- apply-key [handler]
  (fn [f ms k]
    [k (apply (partial handler f)
         (group-vals ms k))]))

(declare deepfmap)

(defn- fmap-seq [f m]
  (map (partial deepfmap f) m))

(defn- fmap-vec [f m]
  (mapv (partial deepfmap f) m))

(defn- fmap-lst [f m]
  (into '() (map (partial deepfmap f) m)))

(defn- fmap-set [f m]
  (into #{} (map (partial deepfmap f) m)))

(defn- fmap-map
  ([f m]
   (reduce (fn [acc [k v]]
             (assoc acc k (deepfmap f v)))
     {} m))
  ([f m ms]
   (let [mcoll (cons m ms)]
     (reduce-kv (fn [acc mk mv]
                  ;; find all the matching keys in mcoll
                  (when-let [keys (into #{} (flatten (map keys mcoll)))]
                    (let [apply-key (apply-key deepfmap)]
                      (into {}
                        ;; eval the nested maps and bind them to the output
                        (map (partial apply-key f mcoll) keys)))))
       m m))))

(defn deepfmap
  "Like fmap but it will recursively evalute all nested collections.

  If you're not already familiar, fmap is basically like map but it
  guarantees that the type of the collection that's passed in as an
  argument will remain unchanged when it comes out as a result.

  See this Haskell page for more info:
  https://en.wikibooks.org/wiki/Haskell/The_Functor_class

  (deepfmap inc {:a 1 :b {:c 2}})

  => {:a 2 :b {:c 3}}"
  ([f]
   (fn [m & ms]
     (if ms
       (apply (partial deepfmap f m) ms)
       (deepfmap f m))))
  ([f m]
   (cond
     ;; map deepfmap over all the entries until it thunks out
     (list? m) (u/save-meta (reverse (fmap-lst f m)) m)
     (vector? m) (u/save-meta (fmap-vec f m) m)
     (seq? m) (u/save-meta (fmap-seq f m) m)
     (set? m) (u/save-meta (fmap-set f m) m)
     ;; map deepfmap over all the vals (nested too)
     (associative? m) (u/save-meta (fmap-map f m) m)
     ;; two base cases here:
     ;; compose fns
     (fn? m) (comp f m)
     :else
     ;; thunk on list atoms like string/keyword/number/etc.
     (f m)))
  ([f m & ms]
   ;; same thing here but we'll mutually walk ms
   (let [mcoll (cons m ms)]
     (cond
       (list? m) (u/save-metas (map (partial fmap-lst f) mcoll) mcoll)
       (vector? m) (u/save-metas (map (partial fmap-vec f) mcoll) mcoll)
       (seq? m) (u/save-metas (map (partial fmap-seq f) mcoll) mcoll)
       (set? m) (u/save-metas (map (partial fmap-set f) mcoll) mcoll)
       (associative? m) (u/save-metas (fmap-map f m ms) mcoll)
       (fn? m) (map #(comp f %) mcoll)
       :else
       (apply f mcoll)))))

(def <-$>
  "An alias for deepfmap"
  deepfmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fapply + pure + filterapply

(defn- ss-fapply
  "An fapply helper fn for sequential/set types. It uses a handler fn
  fn-name as a callback for evaluating the nested data."
  [fn-name]
  (fn
    ([t fs m]
     (-> (apply t (mapcat #(map (partial fn-name %) m) fs))
       (u/save-meta m)))
    ([t fs m ms mcoll]
     (->  (apply t (mapcat #(apply map (partial fn-name %) m ms) fs))
       (u/save-metas mcoll)))))

(defn- map-fapply
  "Similar to ss-fapply, this is a helper fn for the map type. It also uses
  fn-name as a callback fn to recurse with. The seed variable is for what
  the applicative will reduce into. With deepfapply we'll want to save all
  values but with filterapply it will get rid of values by using an empty
  map as the seed."
  [fn-name seed]
  (fn
    ([fs m]
     (reduce-kv (fn [acc fk f]
                  (if-let [[_ mv] (find m fk)]
                    (assoc acc fk (fn-name f mv))
                    acc))
       (or seed m) fs))
    ([fs m ms]
     (let [mcoll (cons m ms)
           full-seed (apply merge (reverse mcoll))]
       (reduce-kv (fn [acc fk f]
                    ;; find all the matching keys in mcoll
                    (if-let [vals (group-vals mcoll fk)]
                      (assoc acc fk
                        ;; eval the nested maps and bind them to the output
                        (apply (partial fn-name f) vals))
                      acc))
         (or seed full-seed) fs)))))

(defn- fapply-handler [fn-name ss-handler map-handler]
  (fn
    ([fs]
     (fn [m & ms]
       (if ms
         (apply (partial fn-name fs m) ms)
         (fn-name fs m))))
    ([fs m]
     (cond
       ;; map the fn for sequential/set (ss) types
       ;; symbols in quoted lists can be evaluated in clj but not cljs
       (list? fs) (ss-handler list #?(:clj (map eval fs)
                                      :cljs fs)
                    m)
       (seq? fs) (ss-handler #(identity %&) fs m)
       (vector? fs) (ss-handler vector fs m)
       (set? fs) (ss-handler #(set %&) fs m)
       ;; match keys for maps and if found apply the fn (otherwise
       ;;  just leave it)
       (map? fs) (u/save-meta (map-handler fs m) m)
       ;; base cases:
       ;; either use the fn
       (fn? fs) (fs m)
       :else
       ;; or wrap list atoms in a constantly
       ((constantly fs) m)))
    ([fs m & ms]
     (let [mcoll (cons m ms)]
       (cond
         ;; mapcat all the results for sequential/set types
         (list? fs) (ss-handler list #?(:clj (map eval fs)
                                        :cljs fs)
                      m ms mcoll)
         (seq? fs) (ss-handler #(identity %&) fs m ms mcoll)
         (vector? fs) (ss-handler vector fs m ms mcoll)
         (set? fs) (ss-handler #(set %&) fs m ms mcoll)
         ;; match all keys for maps and apply the fn if ther was a match
         (map? fs) (u/save-metas (map-handler fs m ms) mcoll)
         ;; apply the fn to the args
         (fn? fs) (apply fs mcoll)
         :else
         ;; list atoms should be constants
         (map (constantly fs) mcoll))))))


;; FIXME: When fs is a list, the list cannot be quoted.
;; there's no eval in cljs so whatever functions are in a
;; quoted list will stay symbols (hard to work around this)
(defn deepfapply
  "Similar to fapply but recursively evaluates all the arguments.

   This basically applies a function that's wrapped in the same type
   as the arguments and returns a collection of the that type.

   See this Haskell page for more info:
   https://en.wikibooks.org/wiki/Haskell/Applicative_Functors#Application_in_functors

   ex:
   (deepfapply {:a +}
     {:a 1} {:a 1 :b 2})

   => {:a 2 :b 2}"
  [& args]
  (let [seq-fapply (ss-fapply deepfapply)
        map-fapply (map-fapply deepfapply nil)
        fapply (fapply-handler deepfapply  seq-fapply map-fapply)]
    (apply fapply args)))

(def <-*>
  "An alias for deepfapply"
  deepfapply)

(defn filterapply
  "The same as deepfapply but keys not in the applicative will not be
   propagated.

  (filterapply {:a +}
    {:a 1} {:a 1 :b 2})

  => {:a 2}

  NOTE: saves all metadata even if a collection gets filtered"
  [& args]
  (let [seq-fapply (ss-fapply filterapply)
        map-fapply (map-fapply filterapply {})
        fapply (fapply-handler filterapply seq-fapply map-fapply)]
    (apply fapply args)))

(declare deeppure)

(defn- coll-pure [t v]
  (into (empty t)
    (if (empty? t)
      (conj t v)
      (for [branch (map identity t)]
        (if  (not (coll? branch))
          v
          (deeppure branch v))))))

(defn- map-pure [t v]
  (into (empty t)
    (if (empty? t)
      (assoc t nil v)
      (map (fn [[k val :as branch]]
             (if-not (coll? branch)
               (into (empty t) [[k v]])
               (into (empty t) [[k (deeppure val v)]])))
        t))))

(defn deeppure
  "This is a recursive version of pure. It will replace all values in
   the given type with value. All keys on maps will also be preserved.

  (deeppure [1 []] 10)

  => [10 [10]]"
  ([m]
   (fn [value]
     (deeppure m value)))
  ([m value]
   (cond
     (map? m) (map-pure m value)
     (coll? m) (coll-pure m value)
     :else
     value)))

(defn pure
  "Basically pure will take any collection and insert some value into it.

  See this for more info:
  https://en.wikibooks.org/wiki/Haskell/Applicative_Functors#The_Applicative_class

  ex:
  (pure [1] 10)

  => [10]"
  ([m]
   (fn [value]
     (pure m value)))
  ([m value]
   (cond
     (map? m) (assoc (empty m) nil value)
     (coll? m) (conj (empty m) value))))


(declare zip)

(defn- zip-vec
  ([fs m]
   (into (empty m) (map deepfmap #?(:clj (eval (vec fs))
                                    :cljs fs) m)))
  ([fs m args]
   (into (empty m) (apply (partial map deepfmap #?(:clj (eval (vec fs))
                                                   :cljs fs) m) args))))

(letfn [(init-map [fs args]
         (into {}
           (map (partial filter #(not (contains? fs (key %))))
             args)))]
  (defn- zip-map [fs & args]
    (reduce-kv (fn [acc k v]
                 (if (fn? v)
                   (assoc acc k (apply (partial deepfmap v)
                                  (remove nil? (map k args))))
                   (assoc acc k (apply (partial zip v)
                                  (remove nil? (map k args))))))
      (init-map fs args) fs)))

(defn zip
  "Similar to zip-list but it handles nested data and maps too. Each
  function in the fs collection will be applied to the argument at
  that position (vectors + seqs) or the matching key (maps). The
  1-arity version will return an infinite seq of the item.

  Here is more info on zip-list:
  https://en.wikibooks.org/wiki/Haskell/Applicative_Functors#ZipList

  ex:
  (zip [+ -] [1 2 3] [4 5] [6 1])

  => [11 -4]

   (zip {:a inc :b dec}
     {:a 1 :b {:c 2 :d 3}})

  => {:a 2 :b {:c 1 :d 2}}

  NOTE: This is not compatible with sets, only lists, seqs, and maps."
  ([x]
   (repeat x))
  ([fs x]
   (cond
     (vector? fs) (u/save-meta (zip-vec fs x) x)
     (seq? fs) (u/save-meta (reverse (zip-vec fs x)) x)
     (map? fs) (u/save-meta (zip-map fs x) x)))
  ([fs x y]
   (cond
     (vector? fs) (u/save-metas (zip-vec fs x [y]) [x y])
     (seq? fs) (u/save-metas (reverse (zip-vec fs x [y])) [x y])
     (map? fs) (u/save-metas (zip-map fs x y) [x y])))
  ([fs x y & args]
   (let [more-args (cons y args)
         all-args (cons x (cons y args))]
     (cond
       (vector? fs) (-> (zip-vec fs x more-args)
                        (u/save-metas all-args))
       (seq? fs) (-> (reverse (zip-vec fs x more-args))
                     (u/save-metas all-args))
       (map? fs) (-> (apply (partial zip-map fs x y) args)
                     (u/save-metas all-args))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transitive

(declare transitive)

(defn- eval-entries
  ([seed fs m]
   (not-empty
     (reduce (fn [acc f]
               (f acc m))
       seed fs))))

(defn- apply-entries
  ([seed fs]
   (fn [m & ms]
     (if (not-empty ms)
       (map (partial eval-entries seed fs) (cons m ms))
       (eval-entries seed fs m)))))

(defn- transitive-lst
  ([f]
   (fn [result m]
     (if-let [v (f m)]
       (conj result v)
       result))))

(defn- transitive-map
  ([k f]
   (fn [result m]
     (if-some [v (f m)]
       (assoc result k v)
       result))))

(defn transitive
  "Takes a transitive and uses that to walk a datastructure, accumulating
  the results into the applicative as it goes.

  ex:
  ((transitive {:a :foo :b {:c :bar}})
    {:foo 1 :bar 2 :fizz 3})

  => {:a 1 :b {:c 2}}"
  ([f]
   (cond
     (map? f) (apply-entries {}
                  (map (fn [[k v]]
                         (transitive-map k (transitive v)))
                    f))
     (seq? f) (apply-entries '() (map (comp transitive-lst transitive) (reverse f)))
     (vector? f) (apply-entries []
                      (map (comp transitive-lst transitive) f))
     (set? f) (apply-entries #{}
                   (map (comp transitive-lst transitive) f))
     (fn? f) f
     (keyword? f) f
     :else
     (constantly f)))
  ([f m]
   ((transitive f) m))
  ([f m & ms]
   (apply (transitive f) (cons m ms))))

(def <=>
  "An alias for transitive"
  transitive)
