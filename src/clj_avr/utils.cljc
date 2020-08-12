(ns clj-avr.utils
  #?(:clj  (:require [clojure.string :as str])
     :cljs (:require [clojure.string :as str]
                     [goog.string :as gstring])))

(def max-long-value #?(:clj  (Long/MAX_VALUE)
                       :cljs js/Number.MAX_VALUE))

(defn format [& args]
  #?(:clj  (apply clojure.core/format args)
     :cljs (apply gstring/format args)))

(defn padded-hex [n pad]
  #?(:clj  (format (str "%0" pad "x") n)
     :cljs (let [hex (.toString n 16)]
             (str (apply str (repeat (- pad (count hex)) "0"))
                  hex))))

(defn char-code [c]
  #?(:clj  (int c)
     :cljs (.charCodeAt (str c) 0)))

(defn parse-int
  ([s] (parse-int s 10))
  ([s r]
   #?(:clj  (Integer/parseInt s r)
      :cljs (js/parseInt s r))))

(defn parse-long
  ([s] (parse-long s 10))
  ([s r]
   #?(:clj  (Long/parseLong s r)
      :cljs (js/parseInt s r))))

(defn pow [x y]
  #?(:clj  (Math/pow x y)
     :cljs (js/Math.pow x y)))

(defn n-to-binary-str [n]
  #?(:clj  (Long/toBinaryString n)
     :cljs (-> n
               (unsigned-bit-shift-right 0)
               (.toString 2))))

(defn parse-unsigned-long [s r]
  #?(:clj  (Long/parseUnsignedLong s r))
  #?(:cljs (-> (js/parseInt s r)
               (unsigned-bit-shift-right 0))))

(defn hex->byte [hex-str]
  (parse-int hex-str 16))

(defn twos-complement [bit-str]
  (let [N (count bit-str)
        com (- (long (pow 2 N)) (parse-unsigned-long bit-str 2))
        r-bit-str (n-to-binary-str com)]
    (str (apply str (repeat (- N (count r-bit-str)) "0")) r-bit-str)))

(defn ubits->dword
  "Unsigned bits string to dword (as a long)"
  [s]
  (parse-unsigned-long s 2))

(defn bits->dword
  "Signed bits string to dword (as a long). Assuming bits string is two's complement encoded."
  [[f :as s]]
  ;; TODO: implement this in a faster way (without string manipulation)

  (-> (parse-long (if (= \1 f) (twos-complement s) s)  2)
      (bit-and (parse-long (apply str (repeat (count s) "1")) 2))
      (* (if (= \1 f) -1 1))))

(defn int-val [x]
  (int x))

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
