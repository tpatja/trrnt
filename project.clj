(defproject trrnt "0.1.0-SNAPSHOT"
  :description "BitTorrent client"
  :url "http://github.com/tpatja/trrnt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main trrnt.core
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [aleph "0.4.0"]
                 [manifold "0.1.0"]
                 [gloss "0.2.5"]
                 [byte-streams "0.2.0"]
                 [cheshire "5.4.0"]])
