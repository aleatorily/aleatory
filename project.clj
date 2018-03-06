(defproject aleatory "0.0.2-SNAPSHOT"
  :description "Generating random stuff, by chance..."
  :url "https://github.com/aleatorily/aleatory"
  :license {:name "3-Clauses BSD License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  ;; :dependencies []
  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          ;;:namespaces []
          }
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.8"]
            [lein-codox "0.10.3"]]
  :profiles {:dev {:dependencies
                   [[org.clojure/clojure "1.9.0"]
                    [org.clojure/clojurescript "1.9.946"]
                    [com.cemerick/piggieback "0.2.2"]
                    ]}
             :test {:dependencies [[org.clojure/clojure "1.9.0"]
                                   [org.clojure/clojurescript "1.9.946"]]}}
  :doo {:build "test"
        :alias {:default [:node]
                :browsers [:chrome :firefox]
                :all [:browsers :headless]}}
  
  :cljsbuild {:builds
              {:dev {:source-paths ["src"]
                     :jar true
                     :compiler {:output-to "resources/public/js/main-dev.js"
                                :optimizations :whitespace
                                :pretty-print true}}
               :test {:source-paths ["src" "test"]
                     :jar true
                      :compiler {:output-to "resources/public/js/all-tests.js"
                                 :target :nodejs
                                 :main aleatory.test-runner
                                 :optimizations :none
                                 :pretty-print true}}}}
  )

