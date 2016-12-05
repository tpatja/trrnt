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

(defn torrent-trackers
  [d]
  (if (contains? d "announce-list")
                   (flatten (d "announce-list"))
                   (conj ()  (d "announce"))))

(defn start-torrent-test
  [fname]
  (let [d (b/decode (input-stream fname))
        trackers (torrent-trackers d)
        size (torrent-size (d "info"))
        hash (String. (info-hash d) "ISO-8859-1")]
    (a/go
      (let [c (t/<announce trackers hash :started size)]
        (dotimes [_ 12] ;; for some reason one channel take does not give all contents
          (let [[tracker peers] (a/<! c)]
            (println (str "got: " peers " from " tracker))
            (doseq [p peers]
              (pwp/start (:ip p) (:port p) hash))))
        (t/<announce trackers hash :stopped size)))
    hash))

(defn -main []
  (println "main"))
