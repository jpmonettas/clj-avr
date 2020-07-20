(ns clj-avr.hex-loader
  "File loader for Intel HEX file format
  https://en.wikipedia.org/wiki/Intel_HEX"
  (:require [clj-avr.utils :as utils]
            [clojure.string :as str]))

(defn valid-checksum? [ln-bytes]
  ;; TODO: implement this
  true)

(defn parse-line
  "Parse hex line.
  Give a byte sequence in this format (without the spaces) :

  :10 00 10 00 0C 94 6E 00 0C 94 6E 00 0C 94 6E 00 0C 94 6E 00 A8
   ^  ^___^ ^  ^___________________ data ____________________^ ^
   |    |   |                                                  |
   |    |   record type                                        checksum
   |    address
   count

  Returns a map with {:type :address :data}
  "

  [ln-str]
  (let [[cnt addr1 addr2 rt & data-and-chk :as ln-bytes] (->> ln-str
                                                              rest  ;; discard :
                                                              (partition 2)
                                                              (map #(apply str %))
                                                              (map utils/hex->byte))]

    (when-not (valid-checksum? ln-bytes)
      (throw (ex-info "Invalid checksum" {:ln-bytes ln-bytes})))

    (case (utils/int-val rt)
      0  {:type :data
          :address (bit-or (bit-shift-left addr1 8) addr2) ;; pack both into a int
          :data (butlast data-and-chk)}
      1 {:type :eof}
      2 {:type :ext-seg-addr}
      3 {:type :ext-lin-addr}
      4 {:type :start-lin-addr})))

(defn load-hex
  "Load a Intel HEX file from path"
  [path]
  (->> (utils/slurp-file path)
       (str/split-lines)
       (map parse-line)))

(defn print-hex-data [parsed-line-bytes]
  (doseq [{:keys [type address data]} parsed-line-bytes]
    (when (= type :data)
      (println (format "%016x - %s" address (->> data
                                              (map #(format "%02x" %))
                                              (str/join " ")))))))

(comment

  (print-hex-data (load-hex "./resources/Blink.ino.hex"))

  (hex-line->bytes ":100000000C945C000C946E000C946E000C946E00CA")

  )
