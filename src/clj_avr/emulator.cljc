(ns clj-avr.emulator
  (:require [clj-avr.disassembler :as da]
            [clj-avr.hex-loader :as hex-loader]
            [clj-avr.utils :as utils :refer [bit-red]]
            [clojure.string :as str]))

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
;; |------------------|
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
;; Y-register | R29 (0x1D) H | R28 (0x1C) L |
;; Z-register | R31 (0x1F) H | R30 (0x1E) L |

  ;; for atmega328p------------------
(def reg->addr

  {:sreg 0x3f
   :sph  0x3e
   :spl  0x3d
   :xh   0x1b
   :xl   0x1a
   :yh   0x1d
   :yl   0x1c
   :zh   0x1f
   :zl   0x1e
   })

(def ram-end (+ 32 64 160 ;; registers
              (* 2 1024) ;; 2k sdram memory
              ))

;; for atmega328p------------------


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

(declare set-reg-by-addr)

(defn write-mem-byte [{:keys [regs data-mem] :as emu} addr byte]
  (if (<= addr 0xFF)
    ;; registers are memory mapped from 0x00 to 0xFF
    (set-reg-by-addr emu addr byte)

    (update emu :data-mem write-byte addr byte)))

(def addr->reg (reduce-kv (fn [r k v] (assoc r v k)) {} reg->addr))

(defn empty-registers []
  (->> (range 0xff)
       (map (fn [r]
              [r (cond
                   (= r (reg->addr :sph)) (utils/word-high ram-end)
                   (= r (reg->addr :spl)) (utils/word-low  ram-end)
                   :else 0)]))
       (into {:pc 0})))

(def flag-bit (zipmap [:c :z :n :v :s :h :t :i] (range)))

(defn set-reg-by-addr [{:keys [regs] :as emu} addr byte-val]
  ;; address and reg number are the same thing
  (assoc-in emu [:regs addr] byte-val))

(defn get-reg-by-addr [{:keys [regs] :as emu} addr]
  ;; address and reg number are the same thing
  (get-in emu [:regs addr]))

(defn set-reg-by-io-reg [emu io-reg byte-val]
  (set-reg-by-addr emu (io-reg->address io-reg) byte-val))

(defn get-reg-by-io-reg [emu io-reg]
  (get-reg-by-addr emu (io-reg->address io-reg)))

(defn set-pc [emu addr]
  (assoc-in emu [:regs :pc] addr))

(defn flag-test [emu fk]
  (-> (get-reg-by-addr emu (reg->addr :sreg))
      (bit-test (flag-bit fk))))

(defn flag-set [emu fk v?]
  (set-reg-by-addr emu (reg->addr :sreg)
                   (let [r (get-reg-by-addr emu (reg->addr :sreg))]
                     (if v?
                       (bit-set   r (flag-bit fk))
                       (bit-clear r (flag-bit fk))))))

(defn sp-address [emu]
  (let [sph (get-in emu [:regs (reg->addr :sph)])
        spl (get-in emu [:regs (reg->addr :spl)])]
    (utils/word sph spl)))

(defn push-byte [emu b]
  (let [sp-addr (sp-address emu)
        sp-addr' (dec sp-addr)]
    (-> emu
        (write-mem-byte sp-addr b)
        (set-reg-by-addr (reg->addr :sph) (utils/word-high sp-addr'))
        (set-reg-by-addr (reg->addr :spl) (utils/word-low  sp-addr')))))

;; TODO: some of this and push-byte can probably be refactored
(defn pop-byte [emu]
  (let [sp-addr (sp-address emu)
        sp-addr' (inc sp-addr)]
    (-> emu
        (set-reg-by-addr (reg->addr :sph) (utils/word-high sp-addr'))
        (set-reg-by-addr (reg->addr :spl) (utils/word-low  sp-addr')))))

(defn peek-byte
  "Returns the byte at the top of the stack"
  [emu]
  (let [sp-addr (sp-address emu)]
    (read-mem-byte emu (+ sp-addr 1))))


(defn push-word
  "Push a word into the stack, first push MSB then LSB"
  [emu word]
  (-> emu
      (push-byte (utils/word-high word))
      (push-byte (utils/word-low  word))))

(defn peek-word
  "Peeks a word from the stack, the word will be [H: SP+2  L: SP+1]. Made to be used with push-word."
  [emu]
  (let [sp-addr (sp-address emu)]
   (utils/word (read-mem-byte emu (+ sp-addr 2))
               (read-mem-byte emu (+ sp-addr 1)))))

(defn pop-word
  "Discards the top two bytes in the stack"
  [emu]
  (-> emu
      (pop-byte)
      (pop-byte)))

(defn special-reg-addr [rk]
  (case rk
    :x [(reg->addr :xh) (reg->addr :xl)]
    :y [(reg->addr :yh) (reg->addr :yl)]
    :z [(reg->addr :zh) (reg->addr :zl)]))

(defn set-special-reg
  "Sets the special register X,Y or Z.
  rk should be :x, :y or :z and word represented as a long"
  [emu rk word]
  (let [[addr-high addr-low] (special-reg-addr rk)]
    (-> emu
        (set-reg-by-addr addr-low  (utils/word-low  word))
        (set-reg-by-addr addr-high (utils/word-high word)))))

(defn get-special-reg
  "Gets the special register X,Y or Z word. "
  [emu rk]
  (let [[addr-high addr-low] (special-reg-addr rk)]
    (utils/word
        (get-reg-by-addr emu addr-high)
        (get-reg-by-addr emu addr-low))))

(defn empty-emu []
  {:data-mem (basic-memory ram-end)
   :prog-mem (basic-memory (* 32 1024)) ;; 32k prog memory
   :regs (empty-registers)
   :status :running})

(defmulti step-inst (fn [emu inst] (:op inst)))

(defmethod step-inst :jmp [{:keys [data-mem prog-mem regs] :as emu} {:keys [addr] :as inst}]
  ;; NOTE: Why addresses in call and jump has to be shifted to the left I still couldn't figure out
  (set-pc emu (bit-shift-left addr 1)))

(defmethod step-inst :call [emu {:keys [addr] :as inst}]
  ;; NOTE: Why addresses in call and jump has to be shifted to the left I still couldn't figure out
  (let [pc (get-in emu [:regs :pc])]
    (-> emu
       (push-word (+ pc (:op/bytes-cnt inst))) ;; store the return address as pc+2
       (set-pc (bit-shift-left addr 1)))))

(defmethod step-inst :eor [emu {:keys [src-reg dst-reg] :as inst}]
  (let [rs (get-reg-by-addr emu src-reg)
        rd (get-reg-by-addr emu dst-reg)
        r (bit-xor rs rd)]
    (-> emu
        (set-reg-by-addr dst-reg r)
        (flag-set :v false)
        (flag-set :n (bit-test r 7))
        (flag-set :s (utils/xor (flag-test emu :n) (flag-test emu :v)))
        (flag-set :z (= r 0)))))

(defmethod step-inst :ldi [emu {:keys [dst-reg const] :as inst}]
  (-> emu
      (set-reg-by-addr dst-reg const)))

(defmethod step-inst :out [emu {:keys [src-reg io-reg] :as inst}]
  (let [rs (get-reg-by-addr emu src-reg)]
    (-> emu
        (set-reg-by-io-reg io-reg rs))))


(defmethod step-inst :ret [emu {:keys [] :as inst}]
  (let [ret-addr (peek-word emu)]
    (-> emu
        pop-word
        (set-pc ret-addr))))

(defmethod step-inst :cli [{:keys [data-mem prog-mem regs] :as emu} {:keys [] :as inst}]
  (flag-set emu :i false))

(defmethod step-inst :rjmp [{:keys [regs] :as emu} {:keys [const] :as inst}]
  (set-pc emu (+ (:pc regs) const 1)))

(defmethod step-inst :std [emu {:keys [y-reg+q src-reg z-reg+q] :as inst}]
  (let [pointer (cond
                  y-reg+q (+ y-reg+q (get-special-reg emu :y))
                  z-reg+q (+ z-reg+q (get-special-reg emu :z))
                  :else (throw (ex-info "Error when executing STD. y-reg+q or z-reg+q must be set" {:inst inst})))
        sr (get-reg-by-addr emu src-reg)]
    (-> emu
        (write-mem-byte pointer sr))))

(defmethod step-inst :add [emu {:keys [src-reg dst-reg]}]
  (let [rr (get-reg-by-addr emu src-reg)
        rd (get-reg-by-addr emu dst-reg)
        r  (+ rr rd)]
    (-> emu
        (flag-set :h (bit-red [[rd 3] :and [rr 3] :or [rr 3] :and [:! r 3] :or [:! r 3] :and [rd 3]]))
        (flag-set :s (utils/xor (flag-test emu :n) (flag-test emu :v)))
        (flag-set :v (bit-red [[rd 7] :and [rr 7] :and [:! r 7] :or [:! rd 7] :and [:! rr 7] :and [r 7]]))
        (flag-set :n (bit-test r 7))
        (flag-set :z (zero? r))
        (flag-set :c (bit-red [[rd 7] :and [rr 7] :or [rr 7] :and [:! r 7] :or [:! r 7] :and [rd 7]]))
        (set-reg-by-addr dst-reg r))))

(defmethod step-inst :in [emu {:keys [io-reg dst-reg] :as inst}]

  #dbg ^{:break/when (#{0x86 0x88} (-> emu :regs :pc))}
  (let [rd (get-reg-by-io-reg emu io-reg)]
    (-> emu
        (set-reg-by-addr dst-reg rd))))

(defmethod step-inst :movw [emu {:keys [src-reg dst-reg] :as inst}]
  (-> emu
      (set-reg-by-addr dst-reg       (get-reg-by-addr emu src-reg))
      (set-reg-by-addr (inc dst-reg) (get-reg-by-addr emu (inc src-reg)))))

(defmethod step-inst :push [emu {:keys [src-reg] :as inst}]
  (-> emu
      (push-byte (get-reg-by-addr emu src-reg))))

(defmethod step-inst :pop [emu {:keys [dst-reg] :as inst}]
  (-> emu
      (set-reg-by-addr dst-reg (peek-byte emu))
      (pop-byte)))

(defmethod step-inst :or [emu {:keys [dst-reg src-reg] :as inst}]
  (let [rr (get-reg-by-addr emu src-reg)
        rd (get-reg-by-addr emu dst-reg)
        r  (bit-or rr rd)]
    (-> emu
        (set-reg-by-addr dst-reg r)
        (flag-set :s (utils/xor (flag-test emu :n) (flag-test emu :v)))
        (flag-set :v false)
        (flag-set :n (bit-test r 7))
        (flag-set :z (zero? r)))))

(defmethod step-inst :mul [emu {:keys [src-reg dst-reg] :as inst}]
  (let [rr (get-reg-by-addr emu src-reg)
        rd (get-reg-by-addr emu dst-reg)
        r (* rr rd)]
    (-> emu
        (set-reg-by-addr 0 (utils/word-low r))
        (set-reg-by-addr 1 (utils/word-high r))
        (flag-set :c (bit-test r 15))
        (flag-set :z (zero? r)))))

(defmethod step-inst :sbiw [emu {:keys [dst-reg const] :as inst}]
  (let [rdl (get-reg-by-addr emu dst-reg)
        rdh (get-reg-by-addr emu (inc dst-reg))
        w (utils/word rdh rdl)
        r (- w const)]
    (-> emu
        (set-reg-by-addr dst-reg       (utils/word-high r))
        (set-reg-by-addr (inc dst-reg) (utils/word-low  r))
        (flag-set :s (utils/xor (flag-test emu :n) (flag-test emu :v)))
        (flag-set :v (bit-red [[:! r 15] :and [rdh 7]]))
        (flag-set :n (bit-test r 15))
        (flag-set :z (zero? r))
        (flag-set :c (bit-red [[r 15] :and [:! rdh 7]])))))

(defmethod step-inst :rcall [emu {:keys [const] :as inst}]
  (let [pc (get-in emu [:regs :pc])]
    (-> emu
        (push-word (+ pc (:op/bytes-cnt inst))) ;; store the return address as pc+1
        (set-pc (+ pc const (:op/bytes-cnt inst))))))

(defmethod step-inst :brne [emu {:keys [const] :as inst}]
  (let [pc (get-in emu [:regs :pc])]
    (cond-> emu
      (not (flag-test emu :z)) (set-pc (inc const)))))

(defmethod step-inst :ldd [emu {:keys [y-reg+q dst-reg z-reg+q] :as inst}]
  (let [pointer (cond
                  y-reg+q (+ y-reg+q (get-special-reg emu :y))
                  z-reg+q (+ z-reg+q (get-special-reg emu :z))
                  :else (throw (ex-info "Error when executing LDD. y-reg+q or z-reg+q must be set" {:inst inst})))
        sd (get-reg-by-addr emu dst-reg)]
    (-> emu
        (set-reg-by-addr dst-reg (read-mem-byte emu pointer)))))

(defmethod step-inst :.dw [emu _]
  ;; this is also just a nop, since we couldn't disassemble it
  emu)

(defmethod step-inst :nop [emu _]
  emu)

(defmethod step-inst :default [emu inst]
  (println (format "%s instruction not implemented." (:op inst)))
  emu)

(defn print-stack [emu]
  (let [sp-addr (sp-address emu)]
    (print "| ")
    (doseq [pointer (reverse (range sp-addr ram-end))]
      (print (format "%02x " (read-mem-byte emu (inc pointer)))))
    (println "<sp")))

(defn maybe-print-emu-debug [emu-prev inst emu-next]
  (when (or (= *emu-debug* :all)
            (and (set? *emu-debug*)
                 (contains? *emu-debug* (:op inst))))

    (da/print-disassemble [inst]) ;; print disassembled instruction
    (println  "   -" inst)        ;; print instruction map
    (doseq [[r v1] (:regs emu-prev)] ;; print registers that changed
      (let [v2 (get-in emu-next [:regs r])]
        (when (not= v1 v2)
          (if (= r (reg->addr :sreg))
            (do ;; if it is the flag register that changed print flag details
              (print "   - Flags changed : ")
              (doseq [[fk b] flag-bit]
                (let [fv1 (flag-test emu-prev fk)
                      fv2 (flag-test emu-next fk)]
                  (when (utils/xor fv1 fv2)
                    (print (format "%s: %s -> %s" fk fv1 fv2)))))
              (println))

            ;; print how the register changed
            (println (format "   - r%s: %02x -> %02x" (str (or (addr->reg r) r)) v1 v2))))))
    ;; print the stack
    (print "   -")
    (print-stack emu-next)))

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

(defn run
  ([emu] (run emu (utils/max-long-value)))
  ([emu max-iter]
   (loop [e (assoc emu :status :running)
          iter max-iter]
     (if (and (= (:status e) :running) (pos? iter))
       (recur (step e) (dec iter))
       (do
         (when (zero? iter) (println "Max iterations reached. Stopping."))
         e)))))

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

  (def emu-after-load (load-prog (empty-emu) (hex-loader/load-hex "./resources/factorial.hex")))

  (let [{:keys [data max-addr-written]} (:prog-mem emu-after-load)]
    (->> data
         (take (inc max-addr-written))
         da/disassemble-bytes
         da/print-disassemble))

  (binding [*emu-debug* :all]
    (run emu-after-load 14))


  )
