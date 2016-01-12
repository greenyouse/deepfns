(ns deepfns.core)

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

(defn apply-key [handler]
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
       (deepfmap f m ms)
       (deepfmap f m))))
  ([f m]
   (cond
     ;; map deepfmap over all the entries until it thunks out
     (list? m) (flst f m)
     (vector? m) (fvec f m)
     (seq? m) (fseq f m)
     (set? m) (fset f m)
     ;; map deepfmap over all the vals (nested too)
     (associative? m) (fassc f m)
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
       (list? m) (map (partial flst f) mcoll)
       (vector? m) (map (partial fvec f) mcoll)
       (seq? m) (map (partial fseq f) mcoll)
       (set? m) (map (partial fset f) mcoll)
       (associative? m) (fassc f m ms)
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
  ([f]
   (fn [m & ms]
     (if ms
       (deepfapply f m ms)
       (deepfapply f m))))
  ([fs m]
   (cond
     ;; map the fn for sequential/set types
     (list? fs) (lst-fapply fs m)
     (seq? fs) (seq-fapply fs m)
     (vector? fs) (vec-fapply fs m)
     (set? fs) (set-fapply fs m)
     ;; match keys for maps and if found apply the fn (otherwise
     ;;  just leave it)
     (map? fs) (assc-fapply fs m)
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
       (list? fs) (lst-fapply fs m ms)
       (seq? fs) (seq-fapply fs m ms)
       (vector? fs) (vec-fapply fs m ms)
       (set? fs) (set-fapply fs m ms)
       ;; match all keys for maps and apply the fn if ther was a match
       (map? fs) (assc-fapply fs m ms)
       ;; apply the fn to the args
       (fn? fs) (apply fs mcoll)
       :else
       ;; list atoms should be constants
       (map (constantly fs) mcoll)))))

(def <-*>
  "An alias for deepfapply"
  deepfapply)


(declare pure)

(defn- coll-pure [t v]
  (into (empty t)
    (if (empty? t)
      (conj t v)
      (for [branch (map identity t)]
        (if  (not (coll? branch))
          v
          (pure branch v))))))

(defn- map-pure [t v]
  (into (empty t)
    (if (empty? t)
      (assoc t nil v)
      (map (fn [[k val :as branch]]
             (if-not (coll? branch)
               (into (empty t) [[k v]])
               (into (empty t) [[k (pure val v)]])))
        t))))

(defn deeppure
  "This is a recursive version of pure. It will replace all values in
   the given type with value. All keys on maps will also be preserved."
  ([m]
   (fn [value]
     (pure m value)))
  ([m value]
   (cond
     (map? m) (map-pure m value)
     (coll? m) (coll-pure m value)
     :else
     value)))


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
  propagated."
  ([f]
   (fn [m & ms]
     (if ms
       (filterapply f m ms)
       (filterapply f m))))
  ([fs m]
   (cond
     (list? fs) (lst-fapply fs m)
     (seq? fs) (seq-fapply fs m)
     (vector? fs) (vec-fapply fs m)
     (set? fs) (set-fapply fs m)
     (map? fs) (assc-filterapply fs m)
     (fn? fs) (fs m)
     :else
     ((constantly fs) m)))
  ([fs m & ms]
   (let [mcoll (cons m ms)]
     (cond
       (list? fs) (lst-fapply fs m ms)
       (seq? fs) (seq-fapply fs m ms)
       (vector? fs) (vec-fapply fs m ms)
       (set? fs) (set-fapply fs m ms)
       (map? fs) (assc-filterapply fs m ms)
       (fn? fs) (apply fs mcoll)
       :else
       (map (constantly fs) mcoll)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; traverse

(declare traverse)

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

(defn- lst-traverse
  ([f]
   (fn [result m]
     (if-let [v (f m)]
       (conj result v)
       result))))

(defn- assc-traverse
  ([k f]
   (fn [result m]
     (if-some [v (f m)]
       (assoc result k v)
       result))))

(defn traverse
  "Takes an applicative and uses that to traverse a datastructure, accumulating
  the results into the applicative as it goes."
  ([f]
   (cond
     (map? f) (apply-entries {}
                  (map (fn [[k v]]
                         (assc-traverse k (traverse v)))
                    f))
     (seq? f) (apply-entries '()
                    (map (comp lst-traverse traverse) f))
     (vector? f) (apply-entries []
                      (map (comp lst-traverse traverse) f))
     (set? f) (apply-entries #{}
                   (map (comp lst-traverse traverse) f))
     (fn? f) f
     (keyword? f) f
     :else
     (constantly f))))

(def <=>
  "An alias for traverse"
  traverse)
