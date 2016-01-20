(ns deepfns.utils
  "Internal, helper functions not meant for consumption")

(defn save-meta
  "Save metadata for an object"
  [expr x]
  (with-meta expr (meta x)))

(defn save-metas
  "Save metadata for a collection of objects"
  ([expr xs]
   (let [m (->> (map meta xs)
                (remove nil?)
                (apply merge))]
     (with-meta expr m))))
