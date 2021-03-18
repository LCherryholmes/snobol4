(defproject snobol4 "0.1.0"
  :description "SNOBOL4 system."
  :url ""
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
    [org.clojure/clojure "1.10.1"]
    [org.clojure/tools.trace "0.7.11"]
  ; [org.clojure/math.numeric-tower "0.0.4"]
  ; [aysylu/loom "1.0.2"]
    [instaparse "1.4.10"]
    [criterium "0.4.6"]]
  :main ^:skip-aot snobol4.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
