(defproject trateg "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1-RC1"]
                 [medley "1.2.0"]
                 [cheshire "5.8.1"]
                 [org.clojure/data.csv "0.1.4"]
                 [net.cgrand/xforms "0.18.2"]
                 [org.ta4j/ta4j-core "0.12"]]
  :repl-options {:init-ns trateg.core}
  :source-paths ["src" "dev"])
