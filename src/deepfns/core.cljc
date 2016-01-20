(ns deepfns.core
  (:require [deepfns.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fmap

(declare deepfmap)

(defn- fseq [f m]
  (map (partial deepfmap f) m))

(defn- fvec [f m]
  (mapv (partial deepfmap f) m))

(defn- flst [f m]
  (into '() (map (partial deepfmap f) m)))

(defn- fset [f m]
  (into #{} (map (partial deepfmap f) m)))

(defn- group-vals [ms k]
  (let [matches (map #(find % k) ms)]
    (when-not (every? nil? matches)
      (vals (remove nil? matches)))))

(defn- apply-key [handler]
  (fn [f ms k]
    [k (apply (partial handler f)
         (group-vals ms k))]))

(defn- fassc
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
  "Like fmap but it will recursively evalute all nested collections"
  ([f]
   (fn [m & ms]
     (if ms
       (apply (partial deepfmap f m) ms)
       (deepfmap f m))))
  ([f m]
   (cond
     ;; map deepfmap over all the entries until it thunks out
     (list? m) (u/save-meta (reverse (flst f m)) m)
     (vector? m) (u/save-meta (fvec f m) m)
     (seq? m) (u/save-meta (fseq f m) m)
     (set? m) (u/save-meta (fset f m) m)
     ;; map deepfmap over all the vals (nested too)
     (associative? m) (u/save-meta (fassc f m) m)
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
       (list? m) (u/save-metas (map (partial flst f) mcoll) mcoll)
       (vector? m) (u/save-metas (map (partial fvec f) mcoll) mcoll)
       (seq? m) (u/save-metas (map (partial fseq f) mcoll) mcoll)
       (set? m) (u/save-metas (map (partial fset f) mcoll) mcoll)
       (associative? m) (u/save-metas (fassc f m ms) mcoll)
       (fn? m) (map #(comp f %) mcoll)
       :else
       (apply f mcoll)))))

(def <-$>
  "An alias for deepfmap"
  deepfmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fapply + pure + filterapply

(declare deepfapply)

(defn- lst-fapply
  ([fs m]
   (apply list
     ;; converts to fs to symbols
     (mapcat #(map (partial deepfapply (eval %)) m) fs)))
  ([fs m ms]
    (apply list
      (mapcat #(apply map (partial deepfapply (eval %)) m ms) fs))))

(defn- seq-fapply
  ([fs m]
   (mapcat #(map (partial deepfapply %) m) fs))
  ([fs m ms]
   (mapcat #(apply map (partial deepfapply %) m ms) fs)))

(defn- set-fapply
  ([fs m]
   (set
     (mapcat #(map (partial deepfapply %) m) fs)))
  ([fs m ms]
   (set
     (mapcat #(apply map (partial deepfapply %) m ms) fs))))

(defn- vec-fapply
  ([fs m]
   (apply vector
     (mapcat #(map (partial deepfapply %) m) fs)))
  ([fs m ms]
   (apply vector
     (mapcat #(apply map (partial deepfapply %) m ms) fs))))

(defn- assc-fapply
  ([fs m]
   (reduce-kv (fn [acc fk f]
                (if-let [[_ mv] (find m fk)]
                  (assoc acc fk (deepfapply f mv))
                  acc))
     m fs))
  ([fs m ms]
   (let [mcoll (cons m ms)
         seed (apply merge (reverse mcoll))]
     (reduce-kv (fn [acc fk f]
                  ;; find all the matching keys in mcoll
                  (if-let [vals (group-vals mcoll fk)]
                    (assoc acc fk
                      ;; eval the nested maps and bind them to the output
                      (apply (partial deepfapply f) vals))
                    acc))
       seed fs))))

(defn deepfapply
  "Similar to fapply but recursively evaluates all the arguments"
  ([fs]
   (fn [m & ms]
     (if ms
       (apply (partial deepfapply fs m) ms)
       (deepfapply fs m))))
  ([fs m]
   (cond
     ;; map the fn for sequential/set types
     (list? fs) (u/save-meta (lst-fapply fs m) m)
     (seq? fs) (u/save-meta (seq-fapply fs m) m)
     (vector? fs) (u/save-meta (vec-fapply fs m) m)
     (set? fs) (u/save-meta (set-fapply fs m) m)
     ;; match keys for maps and if found apply the fn (otherwise
     ;;  just leave it)
     (map? fs) (u/save-meta (assc-fapply fs m) m)
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
       (list? fs) (u/save-metas (lst-fapply fs m ms) mcoll)
       (seq? fs) (u/save-metas (seq-fapply fs m ms) mcoll)
       (vector? fs) (u/save-metas (vec-fapply fs m ms) mcoll)
       (set? fs) (u/save-metas (set-fapply fs m ms) mcoll)
       ;; match all keys for maps and apply the fn if ther was a match
       (map? fs) (u/save-metas (assc-fapply fs m ms) mcoll)
       ;; apply the fn to the args
       (fn? fs) (apply fs mcoll)
       :else
       ;; list atoms should be constants
       (map (constantly fs) mcoll)))))

(def <-*>
  "An alias for deepfapply"
  deepfapply)


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
   the given type with value. All keys on maps will also be preserved."
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
  ([m]
   (fn [value]
     (pure m value)))
  ([m value]
   (cond
     (map? m) (assoc (empty m) nil value)
     (coll? m) (conj (empty m) value))))


(declare filterapply)

(defn- assc-filterapply
  ([fs m]
   (reduce-kv (fn [acc fk f]
                (if-let [[_ mv] (find m fk)]
                  (assoc acc fk (filterapply f mv))
                  acc))
     ;; start with an empty map to filter vals not in applicative
     {} fs))
  ([fs m ms]
   (let [mcoll (cons m ms)]
     (reduce-kv (fn [acc fk f]
                  ;; find all the matching keys in mcoll
                  (if-let [vals (group-vals mcoll fk)]
                    (assoc acc fk
                      ;; eval the nested maps and bind them to the output
                      (apply (partial filterapply f) vals))
                    acc))
       ;; use the empty map here too
       {} fs))))

(defn filterapply
  "The same as deepfapply but keys not in the applicative will not be
  propagated.

  NOTE: saves all metadata even if a collection gets filtered"
  ([f]
   (fn [m & ms]
     (if ms
       (apply (partial filterapply f m) ms)
       (filterapply f m))))
  ([fs m]
   (cond
     (list? fs) (u/save-meta (lst-fapply fs m) m)
     (seq? fs) (u/save-meta (seq-fapply fs m) m)
     (vector? fs) (u/save-meta (vec-fapply fs m) m)
     (set? fs) (u/save-meta (set-fapply fs m) m)
     (map? fs) (u/save-meta (assc-filterapply fs m) m)
     (fn? fs) (fs m)
     :else
     ((constantly fs) m)))
  ([fs m & ms]
   (let [mcoll (cons m ms)]
     (cond
       (list? fs) (u/save-metas (lst-fapply fs m ms) mcoll)
       (seq? fs) (u/save-metas (seq-fapply fs m ms) mcoll)
       (vector? fs) (u/save-metas (vec-fapply fs m ms) mcoll)
       (set? fs) (u/save-metas (set-fapply fs m ms) mcoll)
       (map? fs) (u/save-metas (assc-filterapply fs m ms) mcoll)
       (fn? fs) (apply fs mcoll)
       :else
       (map (constantly fs) mcoll)))))


(declare zip)

(defn- vec-zip
  ([fs m]
   (into (empty m) (map deepfmap (eval (vec fs)) m)))
  ([fs m args]
   (into (empty m) (apply (partial map deepfmap (eval (vec fs)) m) args))))

(letfn [(init-map [fs args]
         (into {}
           (map (partial filter #(not (contains? fs (key %))))
             args)))]
  (defn- map-zip [fs & args]
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

  NOTE: This is not compatible with sets, only lists, seqs, and maps."
  ([x]
   (repeat x))
  ([fs x]
   (cond
     (vector? fs) (u/save-meta (vec-zip fs x) x)
     (seq? fs) (u/save-meta (reverse (vec-zip fs x)) x)
     (map? fs) (u/save-meta (map-zip fs x) x)))
  ([fs x y]
   (cond
     (vector? fs) (u/save-metas (vec-zip fs x [y]) [x y])
     (seq? fs) (u/save-metas (reverse (vec-zip fs x [y])) [x y])
     (map? fs) (u/save-metas (map-zip fs x y) [x y])))
  ([fs x y & args]
   (let [more-args (cons y args)
         all-args (cons x (cons y args))]
     (cond
       (vector? fs) (-> (vec-zip fs x more-args)
                        (u/save-metas all-args))
       (seq? fs) (-> (reverse (vec-zip fs x more-args))
                     (u/save-metas all-args))
       (map? fs) (-> (apply (partial map-zip fs x y) args)
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

(defn- lst-transitive
  ([f]
   (fn [result m]
     (if-let [v (f m)]
       (conj result v)
       result))))

(defn- assc-transitive
  ([k f]
   (fn [result m]
     (if-some [v (f m)]
       (assoc result k v)
       result))))

(defn transitive
  "Takes an applicative and uses that to walk a datastructure, accumulating
  the results into the applicative as it goes."
  ([f]
   (cond
     (map? f) (apply-entries {}
                  (map (fn [[k v]]
                         (assc-transitive k (transitive v)))
                    f))
     (seq? f) (apply-entries '()
                    (map (comp lst-transitive transitive) f))
     (vector? f) (apply-entries []
                      (map (comp lst-transitive transitive) f))
     (set? f) (apply-entries #{}
                   (map (comp lst-transitive transitive) f))
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
