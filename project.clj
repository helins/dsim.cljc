(defproject dvlopt/dsim
            "0.0.0-alpha0"

  :description  "Idiomatic and purely functional discrete event simulation"
  :url          "https://github.com/dvlopt/dsim.clj"
  :license      {:name "Eclipse Public License"
                 :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles     {:dev {:source-paths ["dev"]
                       :main         user
                       :dependencies [[criterium              "0.4.4"]
                                      [org.clojure/test.check "0.10.0-alpha3"]]
                       :plugins      [[lein-codox      "0.10.5"]
                                      [venantius/ultra "0.5.2"]]
                       :codox        {:output-path  "doc/auto"
                                      :source-paths ["src"]}
                       :global-vars  {*warn-on-reflection* true}}})
