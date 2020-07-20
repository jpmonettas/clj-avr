(ns clj-avr.utils
  (:require [clojure.string :as str]))



(defn slurp-file [path]
  (slurp path))

(defn n-to-binary-str [x]
  (Long/toBinaryString x))

(defn hex->byte [hex-str]
  (Integer/parseInt hex-str 16))

(defn bits->byte [s]
  (Integer/parseInt s 2))

(defn bits->word [s]
  (Integer/parseInt s 2))

(defn bits->dword [s]
  (Long/parseLong s 2))

(defn parse-int [s]
  (Integer/parseInt s))

(defn int-val [x]
  (int x))
