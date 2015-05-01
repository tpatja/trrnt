(ns trrnt.core
  (:use clojure.java.io)
  (:require [trrnt.utils :refer :all]
            [trrnt.bencode :as b]
            [trrnt.tracker :as t]
            [trrnt.pwp :as pwp]
            [clojure.core.async :as a])
  (:gen-class))


(defn info-hash
  [torrent-dict]
  (sha1 (b/encode (torrent-dict "info"))))

(defn torrent-size
  [info-dict]
  (let [pieces (info-dict "pieces")
        piece-len (info-dict "piece length")
        n-pieces (/ (count pieces) 20)]
    (if
        (contains? info-dict "length") (do
                                         (println "length exists")
                                         (info-dict "length"))
        (reduce + (map #(% "length") (info-dict "files"))))))


(defn announce-torrent-test
  [fname]
  (let [d (b/decode (input-stream fname))
        trackers (if (contains? d "announce-list")
                   (flatten (d "announce-list"))
                   (conj ()  (d "announce")))
        size (torrent-size (d "info"))
        hash (info-hash d)]
    (a/go
      (let [c (t/<announce trackers hash :started size)
            ]
        (dotimes [_ 12]
          (let [peers (a/<! c)]
            (println (str "got: " peers))
            (doseq [p peers]
                        (a/go
                          (pwp/start (:ip p)
                                     (:port p)
                                     (String. hash "ISO-8859-1")))))))
      (t/<announce trackers hash :stopped size))
    hash))

(defn -main []
  (println "main"))
