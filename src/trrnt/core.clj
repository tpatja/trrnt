(ns trrnt.core
  (:require [trrnt.utils :refer :all]
            [trrnt.bencode :as b]
            [trrnt.tracker :as tracker]
            [trrnt.pwp :as pwp]
            [trrnt.torrent-file :as torrent-file]
            [clojure.core.async :refer [go <!]]
            [clojure.java.io :as io])
  (:gen-class))

(defn start-torrent-test
  [filename]
  (let [torrent-map (torrent-file/decode-torrent-file filename)
        trackers (torrent-trackers torrent-map)
        size (torrent-size (torrent-map "info"))
        hash (String. (info-hash torrent-map) "ISO-8859-1")]
    (go
      (let [c (tracker/<announce trackers hash :started size)]
        (dotimes [_ 12] ;; TODO: for some reason one channel take does not give all contents
          (let [[tracker peers] (a/<! c)]
            (println (str "got: " peers " from " tracker))
            (doseq [p peers]
              (pwp/start (:ip p) (:port p) hash))))
        (tracker/<announce trackers hash :stopped size)))
    hash))

(defn -main []
  (when (not (empty? *command-line-args*))
    (start-torrent-test (first *command-line-args*))))
