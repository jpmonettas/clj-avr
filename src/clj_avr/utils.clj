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
  ;; TODO: implement this in a faster way (without string manipulation)

  (-> (Long/parseLong (if (= \1 f) (twos-complement s) s)  2)
      (bit-and (Long/parseLong (apply str (repeat (count s) "1")) 2))
      (* (if (= \1 f) -1 1))))

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

(defn bit-red
  "Bit reduce bits operations.
  Bit-ops is like [[val1 bit1] :and [val2 bit2] :or ..] where
  valn is a number and bitn is a bit position inside valn."
  [bit-ops]
  (let [resolve-bits (fn [x]
                       (if (vector? x)
                         (let [[a b c] x]
                           (if (= :! a)
                             (not (bit-test b c))
                             (bit-test a b)))
                         x))
        bit-ops' (map resolve-bits bit-ops)]
    (reduce (fn [r [opk v]]
              (let [op (case opk
                         :and #(and %1 %2)
                         :or  #(or %1 %2))]
                (op r v)))
            (first bit-ops')
            (partition 2 (rest bit-ops')))))
