(ns trrnt.tracker
  (:import (java.io ByteArrayOutputStream DataOutputStream)
           (java.net InetSocketAddress InetAddress DatagramPacket DatagramSocket URI))
  (:require [trrnt.utils :as utils]
            [trrnt.bencode :as b]
            [gloss.core :refer :all]
            [gloss.io :refer :all]
            [aleph.http :as http]
            [byte-streams :as bs]
            [clojure.string :as string]
            [clojure.core.async :refer [go chan >!]]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defonce usable-public-udp-trackers (atom []))

(def udp-frames
  {:connect-req (ordered-map :conn-id :uint64
                             :action :uint32
                             :transaction-id :uint32)

   :connect-resp (ordered-map :action :uint32
                              :transaction-id :uint32
                              :conn-id :uint64)

   :announce-req (ordered-map :conn-id :uint64
                              :action :uint32
                              :transaction-id :uint32
                              :info-hash (finite-block 20)
                              :peer-id (finite-block 20)
                              :downloaded :uint64
                              :left :uint64
                              :uploaded :uint64
                              :event :uint32
                              :ip :uint32
                              :key :uint32
                              :num-want :int32
                              :port :uint16)

   :announce-resp-beginning (ordered-map :action :uint32
                                         :transaction-id :uint32
                                         :interval :uint32
                                         :leechers :uint32
                                         :seeders :uint32)

   :scrape-req (ordered-map :conn-id :uint64
                            :action :uint32
                            :transaction-id :uint32
                            :info-hash (finite-block 20))

   :scrape-resp (ordered-map :action :uint32
                             :transaction-id :uint32
                             :seeders :uint32
                             :completed :uint32
                             :leechers :uint32)

   :error-resp (ordered-map :action :uint32
                            :transaction-id :uint32
                            :error (string :ascii))})

(def udp-tracker-actions {:connect   0
                          :announce  1
                          :scrape    2
                          :error     3})

(def udp-tracker-events {:none      0
                         :completed 1
                         :started   2
                         :stopped   3})

(defn- rnd-transaction-id
  []
  (rand-int (/ (Integer/MAX_VALUE) 2)))

(defn mk-connect-input []
  (encode (udp-frames :connect-req) {:conn-id 0x41727101980
                                     :action (udp-tracker-actions :connect)
                                     :transaction-id (rnd-transaction-id)}))

(defn mk-announce-req [conn-id info-hash peer-id event left port]
  (encode (udp-frames :announce-req) {:conn-id conn-id
                                      :action (udp-tracker-actions :announce)
                                      :transaction-id (rnd-transaction-id)
                                      :info-hash info-hash
                                      :peer-id peer-id
                                      :downloaded 0
                                      :left left
                                      :uploaded 0
                                      :event (udp-tracker-events event)
                                      :ip 0
                                      :key 0
                                      :num-want -1
                                      :port port}))

(defn mk-scrape-req [conn-id info-hash]
  (encode (udp-frames :scrape-req) {:conn-id conn-id
                                    :action (udp-tracker-actions :scrape)
                                    :transaction-id (rnd-transaction-id)
                                    :info-hash info-hash}))

(defn announce-url->scrape-url
  [url]
  (string/replace url "announce" "scrape"))

(defn udp-request
  [host port data data-len max-recv-len]
  (try
    (let [s (DatagramSocket.)
          addr (InetSocketAddress. host port)
          recv-addr (InetSocketAddress. "localhost" (.getLocalPort s))
          packet (DatagramPacket. data data-len addr)
          recv-packet (DatagramPacket. (byte-array max-recv-len) max-recv-len recv-addr)]
      (println (str "local port "  (.getLocalPort s)))
      (.setSoTimeout s 5000)
      (.send s packet)
      (.receive s recv-packet)
      (byte-array (take (.getLength recv-packet) (.getData recv-packet))))
    (catch Exception e
      (println (str "udp-request exception: " e))  nil)))

(defn connect-udp-tracker
  "Send connect request to UDP tracker. On success, return connection-id"
  [host port]
  (println "connect-udp-tracker" host port)
  (let [req  (bs/to-byte-array (mk-connect-input))
        resp (udp-request host port req (count req) 16)]
    (if resp
      (let [resp-map (decode (udp-frames :connect-resp) resp)
            req-map (decode (udp-frames :connect-req) req)]
        (when (and
               (= (req-map :transaction-id)
                  (resp-map :transaction-id))
               (zero? (resp-map :action)))
          (resp-map :conn-id)))
      (do (swap! usable-public-udp-trackers #(remove #{[host port]} %))
          nil))))

(defn gen-peer-id []
  (utils/rand-string (map char (range (int \a) (inc (int \z)))) 20))

(defn should-escape
  [ch]
  (not (or (and (>= ch 48) (<= ch 57)) ;; 0-9
           (and (>= ch 97) (<= ch 122));; a-z
           (and (>= ch 65) (<= ch 90)) ;; A-Z
           (and (>= ch 45) (<= ch 46)) ;; -.
           (= 95 ch)                   ;; _
           (= 126 ch))))               ;; ~

(defn hash->urlparam
  "URL encode given sha1 (bytes)"
  [hash]
  (string/join (map (fn [ch] (if (should-escape ch)
                              (str "%" (format "%02X" ch))
                              (char  ch)))
                    hash)))


(defn ip-addr-str
  "String format IPV4 address from given 32-bit integer"
  [x]
  (-> x
      str
      InetAddress/getByName
      .getHostAddress))

(defn parse-compact-peers
  "Parse vector of ip + port maps from given byte-array with compact peer list (BEP-23) data"
  [ba]
  (let [decoded (decode
                 (repeated (ordered-map :ip :int32 :port :uint16) :prefix :none)
                 ba)]
    (vec (map #(update-in % [:ip] ip-addr-str)
              decoded))))

(defn parse-compact-peers-http
  [s]
  (println "parse-compact-peers-http" (count s))
  (parse-compact-peers (.getBytes s "ISO-8859-1")))


(defn ipv6-addr-str
  [data]
  (string/join ":" (map #(format "%x" %)
                        (decode (repeated :uint16 :prefix :none) data))))

(defn parse-compact-peers6-http
  [s]
  (println "parse-compact-peers6" (count s))
  (let [ba (.getBytes s "ISO-8859-1")
        decoded (decode
                 (repeated (ordered-map :ip6 (finite-block 16) :port :uint16) :prefix :none)
                 ba)]
    (vec
     (map #(update-in % [:ip6] ipv6-addr-str) decoded))))




(defn announce-udp
  "Announce given event to UDP tracker."
  ([host port info-hash event left]
   (let [connection-id (connect-udp-tracker host port)]
     (when connection-id
       (announce-udp host port connection-id info-hash event left))))
  ([host port connection-id info-hash event left]
   (println "announce-udp " event)
   (when connection-id
     (println (str "connected to " host " with id " connection-id))
     (let [peer-id (gen-peer-id)
           announce-req ()
           req (bs/to-byte-array (mk-announce-req connection-id
                                                  info-hash
                                                  peer-id
                                                  event
                                                  left
                                                  6881))
           resp (udp-request host port req (count req) 2048)
           [resp-beginning resp-end] (utils/split-ba resp 20)
           resp-map (decode (udp-frames :announce-resp-beginning) resp-beginning)
           peers (parse-compact-peers resp-end)]
       (assoc resp-map "peers" peers)))))

(defn announce-http
  "Announce given event to HTTP tracker"
  [announce-url info-hash event left]
  (println "announce-http" event announce-url)
  (let [peer-id (gen-peer-id)
        encoded-hash (hash->urlparam info-hash)
        url (.concat announce-url (str "?info_hash=" encoded-hash))
        res @(http/get url {:query-params
                            {:peer_id peer-id
                             :port 6881
                             :uploaded 0
                             :downloaded 0
                             :left left
                             :compact "1"
                             :no_peer_id 0
                             :event (name event)}})
        decoded-resp (b/decode (io/input-stream (:body res)))]
    (-> decoded-resp
        (update-in ["peers"] parse-compact-peers-http)
        (update-in ["peers6"] parse-compact-peers6-http))))

(defn udp-tracker?
  [url]
  (try
    (let [u (URI. url)
          protocol (.getScheme u)]
      (= protocol "udp"))
    (catch Exception e
      (println "udp-tracker? exception" e)
      false)))

(defn udp-tracker-target [url]
  (let [u (URI. url)
        host (.getHost u)
        port (.getPort u)]
    [host port]))

(defn announce
  [tracker info-hash event left]
  (println "announce " tracker (udp-tracker? tracker))
  (try
    (if (udp-tracker? tracker)
      (announce-udp tracker info-hash event left)
      (announce-http tracker info-hash event left))
    (catch Exception e
      (println "announce exception (tracker " tracker ") " e))))

(defn <announce
  "Announce given event for given trackers. Return core.async channel yielding list of peers"
  [trackers info-hash event left]
  (println "<announce " event trackers)
  (let [c (chan)]
    (doseq [t trackers]
      (go
        (let [r (announce t info-hash event left)]
          (when r
            ;; TODO: somehow make sure no duplicates get put into channel
            (>! c [t (r "peers")])))))
    c))

(defn scrape-udp
  "scrape UDP tracker for given info-hash"
  ([host port info-hash]
   (let [connection-id (connect-udp-tracker host port)]
     (when connection-id
       (scrape-udp host port connection-id info-hash))))
  ([host port connection-id info-hash]
   (when connection-id
     (println (str "connected to " host " with id " connection-id))
     (let [req (bs/to-byte-array (mk-scrape-req connection-id info-hash))
           resp (udp-request host port req (count req) 20)]
       (if resp
         (let [resp-map (decode (udp-frames :scrape-resp) resp)
               req-map (decode (udp-frames :scrape-req) req)]
           (when (= (req-map :transaction-id)
                    (resp-map :transaction-id))
             (select-keys resp-map [:leechers :completed :seeders])))
         (swap! usable-public-udp-trackers #(remove #{[host port]} %)))))))

(defn scrape-http
  ;; looks like scrape over HTTP is not really used anymore
  [url info-hash]
  (println "scrape-http, url:" url)
  (try
    (let [scrape-url (announce-url->scrape-url url)
          resp @(http/get url {:query-params {:info_hash info-hash}})]
      (bs/to-string (resp :body)))
    (catch Exception e
      (println (str "scrape-http exception: " e)))))


(defn <scrape
  [trackers info-hash]
  (let [c (chan)]
    (doseq [t trackers]
      (println (str "scraping " t))
      (go
        (if (udp-tracker? t)
          (let [[host port] t
                res (scrape-udp host port info-hash)])
          (let [res (scrape-http t info-hash)]))
        ))))


(defn <scrape-udp
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


(defn get-public-udp-trackers []
  (if (empty? @usable-public-udp-trackers)
    (reset! usable-public-udp-trackers
            (->>
             "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_udp.txt"
             slurp
             string/split-lines
             (filter not-empty)
             (map udp-tracker-target)))
    @usable-public-udp-trackers))

(defn scrape-public-udp-trackers
  "Scrape public UDP trackers for give infohash."
  [info-hash]
  (for [[host port] (get-public-udp-trackers)]
    {host (scrape-udp host port info-hash)}))
