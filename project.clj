(defproject minesweeper-clone "0.1.0-SNAPSHOT"
  :description "A Minesweeper clone"
  :url "http://http://karstendick.github.io/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]
                             [lein-kibit "0.0.8"]]}})
