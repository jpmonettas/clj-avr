(ns clj-avr.emulator
  (:require [clj-avr.disassembler :as da]
            [clj-avr.hex-loader :as hex-loader]))

(def ^:dynamic *emu-debug* #{})

;; -----------------------------
;; | Program Memory            |
;; |---------------------------| 0x0000
;; |                           |
;; | Application Flash Section |
;; |                           |
;; |---------------------------|
;; | Boot Flash Section        |
;; ----------------------------- 0x3FFF

;; --------------------
;; | Data Memory      |
;; |------------------+
;; |     32 Regs      | 0x0000 - 0x001F
;; |   64 I/O Regs    | 0x0020 - 0x005f (EEPROM addr, data, etc regs) (GPIOR0, GPIOR1, GPIOR2)
;; | 160 Ext I/O Regs | 0x0060 - 0x00FF
;; |                  |
;; |                  |
;; |   Internal SRAM  |
;; |    (1048 x 8)    |
;; |                  |
;; |                  |
;; |------------------| <- SPH:SPL
;; |       stack      |
;; -------------------- RAMEND

;; Special registries
;; -----------------------------------------
;; X-register | R27 (0x1B) H | R26 (0x1A) L |
;; Y-register | R27 (0x1D) H | R26 (0x1C) L |
;; Z-register | R27 (0x1F) H | R26 (0x1E) L |

(def io-reg->address #(- % 0x20))

;; basic mem
(defn basic-memory [size-in-bytes]
  {:data (into [] (repeat size-in-bytes 0))
   :max-addr-written 0})

(defn read-byte [mem addr]
  (get-in mem [:data addr]))

(defn write-byte [mem addr byte]
  (-> mem
      (assoc-in [:data addr] byte)
      (update :max-addr-written max addr)))

(defn mem-used [mem]
  (:max-addr-written mem))

;;--------------------------------

(defn read-mem-byte [{:keys [regs data-mem] :as emu} addr]
  (if (<= addr 0xFF)
    ;; registers are memory mapped from 0x00 to 0xFF
    (get regs addr)

    (read-byte data-mem addr)))

(defn read-mem-dword [{:keys [regs data-mem] :as emu} addr]
  (bit-or ;; TODO: add endianness to name
   (bit-shift-left (long (read-mem-byte emu addr))        3)
   (bit-shift-left (long (read-mem-byte emu (+ addr 1)))  2)
   (bit-shift-left (long (read-mem-byte emu (+ addr 2)))  1)
   (bit-shift-left (long (read-mem-byte emu (+ addr 3)))  0)))


(defn empty-registers []
  (->> (range 0xff)
       (map #(vector % 0) )
       (into {:pc 0})))

(defn set-reg-by-addr [{:keys [regs] :as emu} addr byte-val]
  ;; address and reg number are the same thing
  (->> emu
       (update :regs assoc addr byte-val)))

(defn set-reg-by-io-reg [emu io-reg byte-val]
  (set-reg-by-addr emu (io-reg->address io-reg) byte-val))

(defn empty-emu []
  {:data-mem (basic-memory (* 2 1024))  ;; 4k memory
   :prog-mem (basic-memory (* 32 1024)) ;; 32k prog memory
   :regs (empty-registers)
   :status :running})

(defmulti step-inst (fn [emu inst] (:op inst)))

#_(defmethod step-inst :jmp [{:keys [memory registers] :as emu} {:keys [] :as inst}]
  emu)

(defmethod step-inst :nop [emu _]
  emu)

(defmethod step-inst :default [emu inst]
  (println (format "%s instruction not implemented." (:op inst)))
  emu)

(defn maybe-print-emu-debug [emu-prev inst emu-next]
  (when (or (= *emu-debug* :all)
            (and (set? *emu-debug*)
                 (contains? *emu-debug* (:op inst))))
    (println "Just executed " inst)))

(defn step [{:keys [prog-mem regs] :as emu}]
  (let [pc (:pc regs)
        next-inst (->> emu
                       :prog-mem
                       :data
                       (drop pc)
                       da/read-dword
                       da/next-opcode)
        emu-next (step-inst emu next-inst)
        same-pc-reg? (= (-> emu :regs :pc)
                        (-> emu-next :regs :pc))]

    (maybe-print-emu-debug emu next-inst emu-next)

    (cond-> emu-next
      same-pc-reg?
      (update-in [:regs :pc] #(+ % (:op/bytes-cnt next-inst)))

      (> (-> emu-next :regs :pc) (mem-used prog-mem))
      (update :status :halt))))

(defn run [emu]
  (loop [e (assoc emu :status :running)]
    (if (= (:status e) :running)
      (recur (step e))
      e)))

(defn load-prog [emu hex]
  (reduce (fn [e {:keys [type address data]}]
            (if (= type :data)
              (second
               (reduce (fn [[addr e] b]
                         [(inc addr)
                          (update e :prog-mem write-byte addr b)])
                       [address e]
                       data))
              e))
   emu
   hex))

(comment

  (def emu-after-load (load-prog (assoc-in (empty-emu) [:regs :pc] 0x6c)
                                 (hex-loader/load-hex "./resources/Blink.ino.hex")))
  (run emu-after-load)
  (-> emu-after-load
      :prog-mem
      da/disassemble-bytes
      da/print-disassemble)
  )
