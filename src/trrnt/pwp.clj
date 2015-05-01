(ns trrnt.pwp
  "Implements peer wire protocol (PWP)"
  (:require [trrnt.client-names :refer :all]
            [gloss.core :as g]
            [gloss.io :as io]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [aleph.tcp :as tcp]
            [byte-streams :as bs]))


(defonce peer-id (str "-RR0001-" (apply str (take 12 (repeatedly #(rand-int 10))))))

(def frames
  {:handshake  (g/ordered-map :protocol-name (g/finite-frame :ubyte (g/string :iso-8859-1 :length 19))
                              :reserved :uint64
                              :info-hash (g/string :iso-8859-1 :length 20)
                              :peer-id (g/string :iso-8859-1 :length 20))
;;   :pwp (g/finite-frame :uint32 )
   })

(defn mk-handshake [info-hash]
  (io/encode (frames :handshake) {:dummy 19
                               :protocol-name "BitTorrent protocol"
                               :reserved 0
                               :info-hash info-hash
                               :peer-id peer-id}))

(defn peer-client-name [peer-id]
  "Return client name and version given a peer ID. Assumes peer ID uses BEP-20 convention"
  (let [match (re-find #"^\-([A-Z][A-Z])(\p{ASCII}+)\-" peer-id)]
    (if match
      (let [client-id (second match)
            client-ver (apply str (interpose "." (nth match 2)))]
        (str (known-bt-client-names client-id) " " client-ver))
      "unknown client")))


(defn wrap-duplex-stream
  [protocol s]
  (let [out (s/stream)]
    (s/connect
      (s/map #(io/encode protocol %) out)
      s)
    (s/splice
      out
      (io/decode-stream s protocol))))

(defn verify-handshake [hs info-hash]
  (and
   (= (:protocol-name hs) "BitTorrent protocol")
   (= (:info-hash hs) info-hash)))


(defn handshake [addr port info-hash]
  "Open TCP connection to peer, send+receive handshake. Return decoded peer handshake
   and stream"
  (let [hs (mk-handshake info-hash)
        c @(d/timeout! (tcp/client {:host addr
                                    :port port}) 3000 nil)]
    (when c
      (println "TCP connect ok" addr)
      (s/put! c hs)
      (let [bb-stream (s/map io/to-byte-buffer c)
            stream (io/decode-stream-headers bb-stream
                                             [(frames :handshake)])
            handshake (first @(s/take! stream))]
        [handshake stream]))))


(defn start [addr port info-hash]
  (println (str "pwp start " addr " " port))
  (let [[peer-handshake stream] (handshake addr port info-hash)]
    (when (verify-handshake peer-handshake info-hash)
      (println (str  "verified handshake with " addr ":" port
                     " (using " (peer-client-name (:peer-id peer-handshake)) ")"))
      ;; peer can be used
      )))
