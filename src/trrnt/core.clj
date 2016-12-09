(ns trrnt.core
  (:require [trrnt.utils :refer :all]
            [trrnt.bencode :as b]
            [trrnt.tracker :as tracker]
            [trrnt.pwp :as pwp]
            [trrnt.torrent-file :as tf]
            [clojure.core.async :refer [go <!]]
            [clojure.java.io :as io])
  (:gen-class))

(defn start-torrent-test
  [filename]
  (let [torrent-map (tf/decode-torrent-file filename)
        trackers (tf/torrent-trackers torrent-map)
        size (tf/torrent-size (torrent-map "info"))
        hash (String. (tf/torrent-infohash torrent-map) "ISO-8859-1")]
    (go
      (let [c (tracker/<announce trackers hash :started size)]
        (dotimes [_ 12] ;; TODO: for some reason one channel take does not give all contents
          (let [[tracker peers] (<! c)]
            (println (str "got: " peers " from " tracker))
            (doseq [p peers]
              (pwp/start (:ip p) (:port p) hash))))
        (tracker/<announce trackers hash :stopped size)))
    hash))

(defn -main []
  (when (seq *command-line-args*)
    (start-torrent-test (first *command-line-args*))))
