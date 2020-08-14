(ns clj-avr.emulator-test
  (:require [clj-avr.emulator :as emu]
            [clj-avr.hex-loader :as hex-loader]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest emulator-factorial-integration-test
  (testing "Running the compiled factorial example should end with 120 in r24"
    (is (= (-> (emu/empty-emu)
               (emu/load-prog (hex-loader/parse-hex (slurp "./resources/factorial.hex")))
               (emu/run)
               (emu/get-reg-by-addr 24))
           120)
        "R24 should contain 120 after running the factorial example")))
