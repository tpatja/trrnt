(ns trrnt.torrent-file-test
  (:require [trrnt.torrent-file :as sut]
            [clojure.test :refer :all]))

(def torrent-map (sut/decode-torrent-file "test_data/coursera-statistics.torrent"))

(deftest test-torrent-decode
  (is (every? #(contains? torrent-map %)
              ["info" "url-list" "announce-list" "magnet-info"]))
  (is (= "coursera-statistics-making-sense-of-data"
         (get (torrent-map "info") "name"))))

(deftest test-trackers
  (is (not (empty? (sut/torrent-trackers torrent-map)))))

(deftest test-infohash
  (let [infohash-str (sut/torrent-infohash-str torrent-map)]
    (is (= (count infohash-str) 40))
    (is (= infohash-str
           "a0cbaf3e03e0893085b6fbdc97cb6220896dddf2"))))
