(ns trrnt.pwp
  "Implements peer wire protocol (PWP)"
  (:require [trrnt.client-names :refer :all]
            [gloss.core :as g]
            [gloss.io :as io]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [aleph.tcp :as tcp]
            [byte-streams :as bs]
            [clojure.core.async :as a]
            [clojure.string :as string]))

(defonce peer-id (str "-RR0001-" (string/join (repeatedly 12 #(rand-int 10)))))

(g/defcodec message-id (g/enum :ubyte
                               :choke
                               :unchoke
                               :interested
                               :not-interested
                               :have
                               :bitfield
                               :request
                               :piece
                               :cancel
                               :port))

(defn message-codecs []
  (let [codec-defs   [{:type :choke}
                      {:type :unchoke}
                      {:type :interested}
                      {:type :not-interested}
                      {:type :have
                       :index :uint32}
                      {:type :bitfield
                       :bitfield (g/repeated :ubyte :prefix :none)}
                      {:type :request
                       :index :uint32
                       :offset :uint32
                       :length :uint32}
                      {:type :piece
                       :index :uint32
                       :offset :uint32
                       :block (g/repeated :ubyte :prefix :none)}
                      {:type :cancel
                       :index :uint32
                       :offset :uint32
                       :length :uint32}
                      {:type :port
                       :listen-port :uint16}]
        codec-keys (map :type codec-defs)]
    (zipmap codec-keys (map g/compile-frame codec-defs))))

(def codecs
  {:handshake (g/ordered-map :protocol-name (g/finite-frame :ubyte (g/string :iso-8859-1 :length 19))
                             :reserved :uint64
                             :info-hash (g/string :iso-8859-1 :length 20)
                             :peer-id (g/string :iso-8859-1 :length 20))
   :pwp-message (g/finite-frame :uint32
                                (g/header message-id (message-codecs) :type))})

(defn mk-handshake [info-hash]
  (io/encode (codecs :handshake) {:dummy 19
                                  :protocol-name "BitTorrent protocol"
                                  :reserved 0
                                  :info-hash info-hash
                                  :peer-id peer-id}))

(defn peer-client-name
  "Return client name and version given a peer ID. Assumes peer ID uses BEP-20 convention"
  [peer-id]
  (let [match (re-find #"^\-([A-Z][A-Z])(\p{ASCII}+)\-" peer-id)]
    (if match
      (let [client-id (second match)
            client-ver (string/join "." (nth match 2))]
        (str (known-bt-client-names client-id) " " client-ver))
      "unknown client")))

(defn verify-handshake [hs info-hash]
  (and
   (= (:protocol-name hs) "BitTorrent protocol")
   (= (:info-hash hs) info-hash)))

(defn handshake
  "Open TCP connection to peer, send+receive handshake. Return decoded peer handshake
   and stream"
  [addr port info-hash]
  (let [hs (mk-handshake info-hash)
        c @(d/timeout! (tcp/client {:host addr
                                    :port port}) 3000 nil)]
    (when c
      (println "TCP connect ok" addr)
      (s/put! c hs)
      (let [bb-stream (s/map io/to-byte-buffer c)
            stream (io/decode-stream-headers bb-stream
                                             [(codecs :handshake)])
            handshake (first @(s/take! stream))]
        [handshake stream c]))))

(defn send-pwp-message [stream msg]
  (s/put! stream (io/encode (codecs :pwp-message) msg)))

(defn handle-message [msg]
  (println "handle-message")
  (when msg
    (let [type (:type msg)]
      (case type
        :have (println "have" msg)
        :bitfield (println "bitfield")
        nil))))

(defn start [addr port info-hash]
  (println (str "pwp start " addr " " port))
  (let [[peer-handshake in c] (handshake addr port info-hash)]
    (when (verify-handshake peer-handshake info-hash)
      (println (str  "verified handshake with " addr ":" port
                     " (using " (peer-client-name (:peer-id peer-handshake)) ")"))
      (let [s (io/decode-stream in (codecs :pwp-message))]
        (dotimes [_ 20]
          (handle-message @(s/take! s)))
        #_(a/go
            (println "sends")
            (map #(send-pwp-message c %)
                 [{:type :bitfield :bitfield [0]}
                  {:type :unchoke}
                  {:type :interested}]))
        c))))
