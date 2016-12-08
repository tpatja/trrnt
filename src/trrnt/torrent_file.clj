(ns trrnt.torrent-file
  (:require
   [trrnt.bencode :as b]
   [trrnt.utils :refer [sha1]]
   [clojure.java.io :as io]))

(defn torrent-infohash
  "sha-1 digest of torrent's info map (20-byte)"
  [torrent-map]
  (sha1 (b/encode (torrent-map "info"))))

(defn torrent-infohash-str
  "sha-1 digest of torrent's info map (hex string)"
  [torrent-map]
  (let [data-bytes (torrent-infohash torrent-map)]
      (apply str
         (map
          #(.substring
            (Integer/toString
             (+ (bit-and % 0xff) 0x100) 16) 1)
          data-bytes))))

(defn torrent-size
  "Total size of torrent's downloadable contents in bytes"
  [info-map]
  (let [pieces (info-map "pieces")
        piece-len (info-map "piece length")
        n-pieces (/ (count pieces) 20)]
     (count pieces)
    #_(if
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
