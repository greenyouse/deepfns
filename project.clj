(defproject com.greenyouse/deepfns "0.1.5"
  :description "Deeply nested fmap, fapply, and more!"
  :url "https://github.com/greenyouse/deepfns"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]]

  :profiles {:dev {:dependencies
                   [[org.clojure/test.check "0.9.0"]
                    [criterium "0.4.3"]]
                   :plugins
                   [[lein-codox "0.9.1"]
                    [lein-cljsbuild "1.1.2"]]}}

  :codox {:source-uri "https://github.com/greenyouse/deepfns/blob/master/{filepath}#L{line}"
          :include [deepfns.core deepfns.transitive deepfns.utils]}

  :cljsbuild {:builds {:test {:source-paths ["src" "test"]
                              :notify-command ["phantomjs"
                                               "phantom/unit-test.js"
                                               "phantom/unit-test.html"]
                              :compiler {:output-to "target/cljs/test.js"
                                         :optimizations :whitespace
                                         :pretty-print true}}}
              :test-commands {"unit-test" ["phantomjs"
                                           "phantom/unit-test.js"
                                           "phantom/unit-test.html"]}})
