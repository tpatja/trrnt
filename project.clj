(defproject trrnt "0.1.0-SNAPSHOT"
  :description "BitTorrent client"
  :url "http://github.com/tpatja/trrnt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main trrnt.core
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [gloss "0.2.5"]
                 [byte-streams "0.2.0"]])
