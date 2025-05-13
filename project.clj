(defproject clojure-json-parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.json "2.5.1"]]
  :repl-options {:init-ns clojure-json-parser.core}
  :plugins [[cider/cider-nrepl "0.55.7"]]
  :cljfmt {:load-config-file? true})
