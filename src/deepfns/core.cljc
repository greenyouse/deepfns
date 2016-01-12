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

(defn- fassc [f m]
  (reduce (fn [acc [k v]]
            (assoc acc k (deepfmap f v)))
    {} m))

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
   (let [ms (cons m ms)]
     (cond
       (list? m) (map (partial flst f) ms)
       (vector? m) (map (partial fvec f) ms)
       (seq? m) (map (partial fseq f) ms)
       (set? m) (map (partial fset f) ms)
       (associative? m) (map (partial fassc f) ms)
       (fn? m) (map #(comp f %) ms)
       :else
       (map f ms)))))

(def <-$>
  "An alias for deepfmap"
  deepfmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fapply + pure

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

(defn- group-keys [ms k]
  (vals (map #(find % k) ms)))

(defn- assc-fapply
  ([fs m]
   (reduce-kv (fn [acc fk f]
                (if-let [[_ mv] (find m fk)]
                  (assoc acc fk (deepfapply f mv))
                  acc))
     m fs))
  ([fs m ms]
   (let [mcoll (cons m ms)]
     (reduce-kv (fn [acc fk f]
                  ;; find all the matching keys in mcoll
                  (when-let [vals (group-keys mcoll fk)]
                    (assoc acc fk
                      ;; eval the nested maps and bind them to the output
                      (apply (partial deepfapply f) vals))))
       m fs))))

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
   the given type with val. All keys on maps will be preserved."
  ([m]
   (fn [value]
     (pure m value)))
  ([m value]
   (cond
     (map? m) (map-pure m value)
     (coll? m) (coll-pure m value)
     :else
     value)))
