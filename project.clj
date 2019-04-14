(defproject datview "0.0.1-alpha1-SNAPSHOT"
  :description "Effortlessly compose data visualizations and controls for Datomic and DataScript data"
  :url "http://github.com/metasoarous/datview"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-alpha6"]
                 [org.clojure/clojurescript "1.9.36"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 ;; datomic needed for reader literals
                 [com.datomic/datomic-free "0.9.5372" :exclusions [joda-time org.slf4j/slf4j-nop com.google.guava/guava]]
                 ;; Datsys things
                 [datspec "0.0.1-alpha1-SNAPSHOT"]
                 [datreactor "0.0.1-alpha1-SNAPSHOT"]
                 ;; Other stuff
                 [com.stuartsierra/component "0.3.1"]
                 [com.andrewmcveigh/cljs-time "0.5.0-alpha1"]
                 [testdouble/clojurescript.csv "0.2.0"]
                 [datascript "0.15.0"]
                 [posh "0.5.4"]
                 [reagent "0.6.0"]
                 [markdown-clj "0.9.89"]
                 [servant "0.1.5"]
                 ;; Not sure if this should just be a dev dep; It's kinda nice
                 [data-frisk-reagent "0.3.5"]
                 [re-com "0.9.0" :exclusions [cljsjs/react cljsjs/react-dom]]
                 [bidi "2.0.9"]
                 [io.rkn/conformity "0.4.0"] ;; should this be here?
                 [prismatic/plumbing "0.5.2"] ;; aren't using currently
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]] ;; used for table view
  ;;
  ;; ## Snipped from DataScript's
  ;; ============================
  ;;
  ;; The following was taken from DataScript's project.clj; may need to clean up a bit
  ;;
  ;; Leaving this out for now
  ;:global-vars {*warn-on-reflection* true}
  :cljsbuild {:builds [{:id "release"
                        :source-paths ["src"]
                        :assert false
                        :compiler {:output-to     "release-js/datview.bare.js"
                                   :optimizations :advanced
                                   :pretty-print  false
                                   :elide-asserts true
                                   :output-wrapper false
                                   :parallel-build true}}]}
                        ;:notify-command ["release-js/wrap_bare.sh"]
  :profiles {:dev {:source-paths ["bench/src" "test" "dev" "src"]
                   :plugins [[lein-cljsbuild "1.1.2"]
                             [lein-typed "0.3.5"]]
                   :cljsbuild {:builds [{:id "advanced"
                                         :source-paths ["src" "test"]
                                         :compiler {:output-to     "target/datview.js"
                                                    :optimizations :advanced
                                                    :source-map    "target/datview.js.map"
                                                    :pretty-print  true
                                                    :recompile-dependents false
                                                    :parallel-build true}}
                                        {:id "none"
                                         :source-paths ["src" "test" "dev"]
                                         :compiler {:main          datview.test
                                                    :output-to     "target/datview.js"
                                                    :output-dir    "target/none"
                                                    :optimizations :none
                                                    :source-map    true
                                                    :recompile-dependents false
                                                    :parallel-build true}}]}}}
  :clean-targets ^{:protect false} ["target"
                                    "release-js/datview.bare.js"
                                    "release-js/datview.js"]
  ;;
  ;; ## Back to from extraction...
  ;; =============================
  ;;
  ;; Once we're ready
  ;:core.typed {:check []
               ;:check-cljs []}
  ;;
  ;; Not sure if we need these either
  :resource-paths ["resources" "resources-index/prod"]
  :target-path "target/%s"
  :aliases {"package"
            ["with-profile" "prod" "do"
             "clean" ["cljsbuild" "once"]]})


