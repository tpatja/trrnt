(ns trrnt.core
  (:use clojure.java.io)
  (:import (java.net URI URLEncoder)
           (java.security MessageDigest)
           (java.io ByteArrayInputStream))
  (:require [trrnt.bencode :as b]
            [trrnt.tracker :as t]
            [clojure.string :as s]
            [clojure.core.async
             :as a
             :refer [>! <!  go chan]]))

(defn udp-tracker?
  [url]
  (try 
    (let [u (URI. url)
          protocol (.getScheme u)]
      (= protocol "udp"))
    (catch Exception e false)))

(defn sha1
  [x]
  (let [d (MessageDigest/getInstance "SHA1")]
    (.digest d x)))

(defn hexstring
  [bytes]
  (apply str (map #(format "%02x" %) bytes)))

(defn udp-tracker-target [url]
  (let [u (URI. url)
        host (.getHost u)
        port (.getPort u)]
    [host port]))

(defn scrape-torrent-udp
  [host port info-hash]
  (t/scrape-udp host port info-hash))

;; (defn scrape-torrent-http
;;   [url info-hash]
;;   (tracker/scrape-http url info-hash))


(defn info-hash
  [torrent-dict]
  (println "info-hash")
  (sha1 (b/encode (torrent-dict "info"))))

(defn get-torrent-info-hash
  [fname]
  (let [d (b/decode (input-stream fname))]
    (info-hash d)))

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
        udp-trackers (filter udp-tracker? trackers)
        http-trackers (filter (comp not udp-tracker?) trackers)
        size (torrent-size (d "info"))
        hash (info-hash d)
        announce (fn [url] (t/announce-http url hash "started" size))]
    (println (d "announce-list"))
    (println http-trackers)
    (println (str  "from udp "  (t/announce-udp "open.demonii.com" 1337 (String.  hash "ISO-8859-1") size)))
    (go
      (let [c (t/<parallel-announce-http http-trackers hash "started" size)]
        (println (str "got " (<! c)))))))


(defn scrape-torrent-test
  [fname]
  (let [d (b/decode (input-stream fname))
        trackers (flatten (d "announce-list"))
        udp-trackers (filter udp-tracker? trackers)
        http-trackers (filter (comp not udp-tracker?) trackers)
        udp-tracker-targets (map udp-tracker-target
                                 (conj  udp-trackers
                                        "udp://open.demonii.com:1337/announce"))
        hash (String.  (info-hash d) "ISO-8859-1")]
    (println trackers)
    (println "torrent-size:" (torrent-size (d "info")))
    ;;(map #(t/scrape-http % hash) http-trackers)
    ;; (println (str "scraping " (hexstring hash) " from UDP trackers " (s/join ", " udp-trackers)))
    ;; (println udp-tracker-targets)
    (scrape-torrent-udp "open.demonii.com" 1337 hash)
    ;; (println udp-trackers)
    ;; (println (s/join " " (map #(class (first %)) udp-tracker-targets)))
    ;; (map #(scrape-torrent-udp (first %) (second %) hash) udp-tracker-targets)
    ;;    (hexstring info-hash)
    ))
