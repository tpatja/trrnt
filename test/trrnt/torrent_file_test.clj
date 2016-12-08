(ns trrnt.torrent-file-test
  (:require [trrnt.torrent-file :as sut]
            [clojure.test :refer :all]))

(def torrent-map (sut/decode-torrent-file "test_data/nld.torrent"))

(deftest test-torrent-decode
  (is (every? #(contains? torrent-map %)
              ["info" "url-list" "announce-list" "locale" "created by"
               "creation date" "title" "comment"])))

(deftest test-trackers
  (is (not (empty? (sut/torrent-trackers torrent-map)))))

(deftest test-infohash
  (is (= (sut/torrent-infohash-str torrent-map)
         "15c1673eaafb48fc20c7556609807d790469790a")))
