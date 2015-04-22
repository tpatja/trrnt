(ns trrnt.tracker2
  (:import (java.io ByteArrayOutputStream DataOutputStream))
  (:import (java.net InetSocketAddress DatagramPacket DatagramSocket))
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

(defn scrape
  ([host port info-hash]
   (scrape host port (connect-udp-tracker host port) info-hash))
  ([host port connection-id info-hash]
   (when connection-id
     (println (str "connected to " host " with id " connection-id))
     (let [req (mk-scrape-req connection-id info-hash)
           req-arr (bs/to-byte-array req)
           resp (udp-request host port req-arr (count req-arr) 20)
           resp-map (decode (udp-frames :scrape-resp) resp)]
       ;; (println "req " req)
       [[host port] resp-map]))))

(defn <parallel-scrape-test
  "Scrapes given hash on given UDP trackers. 
   Returns a channel for results"
  [trackers info-hash]
  (let [c (chan)]
    ;;(go (println (str "from chan: " (<! c))))
    (doseq [[host port] trackers]
      (println (str host " " port))
      (go (>! c (scrape host port info-hash))))
    c))
