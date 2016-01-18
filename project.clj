(defproject com.greenyouse/deepfns "0.1.0-SNAPSHOT"
  :description "Deeply nested fmap, fapply, and more!"
  :url "https://github.com/greenyouse/deepfns"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]]

  :profiles {:dev {:dependencies
                   [[org.clojure/test.check "0.9.0"]
                    [criterium "0.4.3"]]}})
