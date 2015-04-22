(ns trrnt.tracker
  (:import (java.io ByteArrayOutputStream DataOutputStream)
           (java.net InetSocketAddress DatagramPacket DatagramSocket))
  (:require [gloss.core :refer :all]
            [gloss.io :refer :all]
            [byte-streams :as bs]
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
