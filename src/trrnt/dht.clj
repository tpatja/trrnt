(ns trrnt.dht
  (:require [trrnt.bencode :as b]))

(defn- encode-dht-message [transaction-id message-type data-map]
  (b/encode (merge  {"t" transaction-id
                     "y" message-type}
                    data-map)))

(defn- encode-dht-q [transaction-id query-type query-args-map]
  (encode-dht-message transaction-id "q" (merge {"q" query-type}
                                                {"a" query-args-map})))

(defn encode-ping-q [transaction-id querying-node-id]
  (encode-dht-q transaction-id
                "ping"
                {"id" querying-node-id}))

(defn encode-find-node-q [transaction-id querying-node-id target-node-id]
  (encode-dht-q transaction-id
                "find_node"
                {"id" querying-node-id
                 "target" target-node-id}))

(defn encode-get-peers-q [transaction-id querying-node-id infohash]
  (encode-dht-q transaction-id
                "get_peers"
                {"id" querying-node-id
                 "info_hash" infohash}))

(defn- decode-message-data
  [encoded]
  (let [decoded (b/decode encoded)
        response? (nil? (get decoded "a"))]
    (if response?
      (decoded "r")
      (decoded "a"))))

(defn decode-find-node
  "Decode a map with node-id, target-node-id and nodes keys from find_node query or response"
  [encoded]
  (let [message-data (decode-message-data encoded)]
    {:node-id (get message-data "id")
     :target (get message-data "target")
     :nodes (get message-data "nodes")}))

(defn decode-ping
  "Decode a map with node-id key from ping query or response"
  [encoded]
  (let [message-data (decode-message-data encoded)]
    {:node-id (get message-data "id")}))

(defn decode-error
  "Decode a map with error-code and error-message from given
  stream with bencoded error"
  [encoded]
  (let [decoded (b/decode encoded)
        [error-code error-message] (decoded "e")]
    {:error-code error-code
     :error-message error-message}))
