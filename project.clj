(defproject one-function-to-rule-them-all "1.0.0-SNAPSHOT"
  :main one-function-to-rule-them-all
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [iloveponies.tests/one-function-to-rule-them-all "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
