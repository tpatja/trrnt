(defproject trrnt "0.1.0-SNAPSHOT"
  :description "BitTorrent client"
  :url "http://github.com/tpatja/trrnt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main trrnt.core
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.395"]
                 [aleph "0.4.1"]
                 [manifold "0.1.5"]
                 [gloss "0.2.6"]
                 [byte-streams "0.2.2"]
                 [cheshire "5.6.3"]])
