(ns deepfns.core)

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
