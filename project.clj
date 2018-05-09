(defproject paip "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :profiles {:dev {:plugins [[jonase/eastwood "0.2.5"]
                             [lein-eftest "0.4.1"]
                             [lein-cljfmt "0.5.7"]]
                   :aliases {"lint" ^{:doc "Execute cljfmt check and eastwood."}
                             ["do" ["cljfmt" "check"] ["eastwood"]]}}})
