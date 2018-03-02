(defproject aleatory "0.0.1-SNAPSHOT"
  :description "Generating random stuff, by chance..."
  :url "https://github.com/aleatorily/aleatory"
  :license {:name "3-Clauses BSD License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  ;; :dependencies []
  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          ;;:namespaces []
          }
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {
              :builds [{:source-paths ["src-cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]}
  :profiles {:dev {:dependencies
                   [[org.clojure/clojure "1.9.0"]
                    [org.clojure/clojurescript "1.9.946"]
                    [com.cemerick/piggieback "0.2.2"]
                    ]}
             :test {:dependencies [[org.clojure/clojure "1.9.0"]]}})

