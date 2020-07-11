(ns clj-avr.utils
  (:require [clojure.string :as str]))

(defn hex->byte [hex-str]
  (char (Integer/parseInt hex-str 16)))

(defn unhexify [hex-str]
  (->> hex-str
       (partition 2)
       (map hex->byte)))

(defn slurp-file [path]
  (slurp path))
