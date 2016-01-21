(ns test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [deepfns.core-test]
            [deepfns.transitive-test]
            [deepfns.utils-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
        (run-tests
          'deepfns.core-test
          'deepfns.transitive-test
          'deepfns.utils-test))
    0
    1))
