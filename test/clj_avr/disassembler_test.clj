(ns clj-avr.disassembler-test
  (:require [clj-avr.disassembler :as da]
            [clj-avr.hex-loader :as hex-loader]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def gcc-compiler "/home/jmonetta/non-rep-software/arduino-1.8.12/hardware/tools/avr/bin/avr-objdump")
(def gcc-args ["-z" "--no-show-raw-insn" "--prefix-addresses" "-m" "avr" "-D"])
(def hex-examples ["./resources/blink.hex"
                   "./resources/factorial.hex"])

(deftest disassembler-integration-test
  (doseq [hex-file hex-examples]
    (testing (format "Disassembling %s with avr-objdump and comparing the outputs" hex-file)
     (let [normalize-line (fn [l]
                            (-> l
                                (str/replace #";.*" "")
                                (str/replace #"[\t\s]+" " ")
                                (str/replace #"[\s]+$" "")
                                (str/lower-case )))
           avr-gcc-lines (->> (apply clojure.java.shell/sh (-> [gcc-compiler]
                                                               (into gcc-args)
                                                               (into [hex-file])))
                              :out
                              (str/split-lines)
                              (filter #(str/starts-with? % "0x"))
                              (map normalize-line))
           clj-avr-lines (->> (with-out-str (-> (hex-loader/load-hex hex-file)
                                                da/disassemble-hex
                                                da/print-disassemble))
                              str/split-lines
                              (map normalize-line))]
       (doseq [[gcc-l our-l] (map vector avr-gcc-lines clj-avr-lines)]
         (is (= gcc-l our-l) "Our disassembly doesn't match avr-gcc one"))))))
