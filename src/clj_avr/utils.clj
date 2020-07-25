(ns clj-avr.utils
  (:require [clojure.string :as str]))



(defn slurp-file [path]
  (slurp path))

(defn n-to-binary-str [x]
  (Long/toBinaryString x))

(defn hex->byte [hex-str]
  (Integer/parseInt hex-str 16))

(defn twos-complement [bit-str]
  (let [N (count bit-str)
        com (- (long (Math/pow 2 N)) (Long/parseUnsignedLong bit-str 2))
        r-bit-str (Long/toBinaryString com)]
    (str (apply str (repeat (- N (count r-bit-str)) "0")) r-bit-str)))

(defn bits->dword
  "Signed bits string to dword (as a long). Assuming bits string is two's complement encoded."
  [[f :as s]]
  (* (Long/parseLong (twos-complement s) 2) (if (= \1 f) -1 0)))

(defn ubits->dword
  "Unsigned bits string to dword (as a long)"
  [s]
  (Long/parseUnsignedLong s 2))

(defn parse-int [s]
  (Integer/parseInt s))

(defn int-val [x]
  (int x))

(defn max-long-value []
  (Long/MAX_VALUE))

(defn xor [a b]
  (or (and a (not b))
      (and (not a) b)))

(defn word-low [w]
  (bit-and w 0xff))

(defn word-high [w]
  (bit-shift-right (bit-and w 0xff00) 8))

(defn word [h l]
  (bit-or
   (bit-and l 0xff)
   (bit-shift-left (bit-and h 0xff) 8)))
