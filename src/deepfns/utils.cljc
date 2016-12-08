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

(defmacro binary-op [name op]
  (let [doc-string# (str "A transative form of" `~op "where both arguments are transitive
 expressions")]
    `(defn ~name ~doc-string# [~'x-expr ~'y-expr]
       (let [~'x-t (d/<=> ~'x-expr)
             ~'y-t (d/<=> ~'y-expr)]
         (fn [~'m]
           (~op (~'x-t ~'m) (~'y-t ~'m)))))))
