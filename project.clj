(defproject dvlopt/dsim
            "0.0.0-alpha0"

  :description  " ? "
  :url          " ? "
  :license      {:name "Eclipse Public License"
                 :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [dvlopt/void         "0.0.1"]]
  :profiles     {:dev {:source-paths ["dev"]
                       :main         user
                       :dependencies [[criterium              "0.4.4"]
                                      [org.clojure/test.check "0.10.0-alpha3"]]
                       :plugins      [[lein-codox      "0.10.5"]
                                      [venantius/ultra "0.5.2"]]
                       :codox        {:output-path  "doc/auto"
                                      :source-paths ["src"]}
                       :global-vars  {*warn-on-reflection* true}}})
