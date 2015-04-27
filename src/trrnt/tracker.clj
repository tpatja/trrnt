(ns trrnt.tracker
  (:use clojure.java.io)
  (:import (java.io ByteArrayOutputStream DataOutputStream)
           (java.net InetSocketAddress InetAddress DatagramPacket DatagramSocket))
  (:require [trrnt.bencode :as b]
            [gloss.core :refer :all]
            [gloss.io :refer :all]
            [aleph.http :as http]
            [byte-streams :as bs]
            [clojure.string :as s]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def udp-frames
  {:connect-req (ordered-map :conn-id :uint64-be
                             :action :uint32-be
                             :transaction-id :uint32-be)
   
   :connect-resp (ordered-map :action :uint32-be
                              :transaction-id :uint32-be
                              :conn-id :uint64-be)
   
   :scrape-req (ordered-map :conn-id :uint64-be
                            :action :uint32-be
                            :transaction-id :uint32-be
                            :info-hash (string :ascii :length 20))

   :scrape-resp (ordered-map :action :uint32-be
                             :transaction-id :uint32-be
                             :seeders :uint32-be
                             :completed :uint32-be
                             :leechers :uint32-be)
   
   :error-resp (ordered-map :action :uint32-be
                            :transaction-id :uint32-be
                            :error (string :ascii))})

(defn- rnd-transaction-id
  []
  (rand-int (/ (Integer/MAX_VALUE) 2)))

(defn mk-connect-input []
  (encode (udp-frames :connect-req) {:conn-id 0x41727101980
                                     :action 0
                                     :transaction-id (rnd-transaction-id)}))

(defn mk-scrape-req [connection-id info-hash]
  (encode (udp-frames :scrape-req) {:conn-id connection-id
                                    :action 2
                                    :transaction-id (rnd-transaction-id)
                                    :info-hash info-hash}))

(defn announce-url->scrape-url
  [url]
  (s/replace url "announce" "scrape"))

(defn udp-request
  "Send connect request to UDP tracker. On success, return connection-id"
  [host port data data-len recv-len]
  (println (str "udp-request " host " " port))
  (try
    (let [s (DatagramSocket.)
          addr (InetSocketAddress. host port)
          recv-addr (InetSocketAddress. "localhost" (.getLocalPort s))
          packet (DatagramPacket. data data-len addr)
          recv-packet (DatagramPacket. (byte-array recv-len) recv-len recv-addr)]
      (println (str "local port "  (.getLocalPort s)))
      (.setSoTimeout s 3000)
      (println "sending")
      (.send s packet)
      (Thread/sleep 200)
      (println "receiving")
      (.receive s recv-packet)
      (.getData recv-packet))
    (catch Exception e
      (println (str "exception: " e))  nil)))

(defn connect-udp-tracker
  "Send connect request to UDP tracker. On success, return connection-id"
  [host port]
  (let [req (bs/to-byte-array (mk-connect-input))
        resp (udp-request host port req (count req) 16)]
    (when resp
      (let [resp-map (decode (udp-frames :connect-resp) resp)
            req-map (decode (udp-frames :connect-req) req)]
        (if (and
             (= (req-map :transaction-id (resp-map :transaction-id)))
             (= (resp-map :action) 0))
          (resp-map :conn-id) 
          nil)))))

(defn scrape-udp
  "scrape UDP tracker for given info-hash"
  ([host port info-hash]
   (scrape-udp host port (connect-udp-tracker host port) info-hash))
  ([host port connection-id info-hash]
   (when connection-id
     (println (str "connected to " host " with id " connection-id))
     (let [req (bs/to-byte-array (mk-scrape-req connection-id info-hash))
           resp (udp-request host port req (count req) 20)]
       (when resp
         (let [resp-map (decode (udp-frames :scrape-resp) resp)
               req-map (decode (udp-frames :scrape-req) req)]
           (if
               (= (req-map :transaction-id)
                  (resp-map :transaction-id))
             (select-keys resp-map [:leechers :completed :seeders])
             nil)))))))

(defn scrape-http
  ;; looks like scrape over HTTP is not really used anymore
  [url info-hash]
  (println "scrape-http")
  (try
    (let [scrape-url (announce-url->scrape-url url)
          resp @(http/get url {:query-params {:info_hash info-hash}})]
      (bs/to-string (resp :body)))
    (catch Exception e
      (println (str "exception: " e)))))


(defn rand-string [characters n]
  (->> (fn [] (rand-nth characters))
       repeatedly
       (take n)
       (apply str)))

(defn gen-peer-id []
  (rand-string (map char (range (int \a) (inc (int \z)))) 20))

(defn should-escape
  [ch]
  (not (or (and (>= ch 48) (<= ch 57)) ;; 0-9
           (and (>= ch 97) (<= ch 122));; a-z
           (and (>= ch 65) (<= ch 90)) ;; A-Z
           (and (>= ch 45) (<= ch 46)) ;; -.
           (= 95 ch)                   ;; _
           (= 126 ch))))               ;; ~

(defn hash->urlparam
  [hash]
  (apply str (map (fn [ch] (if (should-escape ch)
                             (str "%" (format "%02X" ch))
                             (char  ch)))
                  hash)))

(defn parse-compact-peers
  [s]
  (reduce (fn[list peer]
            (let [[port-msb port-lsb] (take-last 2 (map int peer))
                  str-ip (apply str (interpose \. (map int (take 4 peer))))]
              (conj list
                    {:ip (InetAddress/getByName str-ip)
                     :port (bit-or
                            (bit-shift-left port-msb 8) (bit-and port-lsb 0xff))})))
          [] (partition 6 s)))


(defn announce-http
  "HTTP announce request for given event"
  [announce-url info-hash event left]
  (println (str "announce-http " event))
  (let [peer-id (gen-peer-id)
        encoded-hash (hash->urlparam info-hash)
        url (.concat announce-url (str "?info_hash=" encoded-hash))
        res @(http/get url {:query-params
                            {:peer_id peer-id
                             :port 6881
                             :uploaded 0
                             :downloaded 0
                             :left left
                             :compact 1
                             :no_peer_id 0
                             :event event}})
        decoded-resp (b/decode (input-stream (:body res)))]
    (update-in decoded-resp ["peers"] parse-compact-peers)))

(defn <parallel-announce-http
  [urls info-hash event left]
  (let [c (chan)]
    (doseq [url urls]
      (println (str "announce " url))
      (go
        (let [res (announce-http url info-hash event left)]
          (when res
            (>! c res)))))
    c))


(defn <parallel-scrape-udp
  "Scrapes given hash on given UDP trackers. 
  Returns a channel for results"
  [trackers info-hash]
  (let [c (chan)]
    (doseq [[host port] trackers]
      (println (str "scraping " host " " port))
      (go
        (let [res (scrape-udp host port info-hash)]
          (println "res " res)
          (when res
            (>! c [[host port info-hash] res])))))
    c))
