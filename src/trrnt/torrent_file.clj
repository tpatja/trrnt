(ns trrnt.torrent-file
  (:require
   [trrnt.bencode :as b]
   [trrnt.utils :refer [sha1 hexify]]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn torrent-infohash
  "sha-1 digest of torrent's info map (20-byte) as a byte-array"
  [torrent-map]
  (sha1 (b/encode (torrent-map "info"))))

(defn torrent-infohash-str
  "sha-1 digest of torrent's info map (hex string)"
  [torrent-map]
  (hexify (torrent-infohash torrent-map)))

(defn torrent-size
  "Total size of torrent's downloadable contents in bytes"
  [info-map]
  (let [pieces (info-map "pieces")
        piece-len (info-map "piece length")
        n-pieces (/ (count pieces) 20)]
    (if
     (contains? info-map "length") (do
                                     (println "top-level length exists")
                                     (info-map "length"))
     (reduce + (map #(% "length") (info-map "files"))))))

(defn torrent-trackers
  [d]
  (if (contains? d "announce-list")
    (flatten (d "announce-list"))
    (vector (d "announce"))))

(defn decode-torrent-file
  [filename]
  (b/decode (io/input-stream filename)))
