(ns clj-avr.disassembler
  (:require [clj-avr.hex-loader :as hex-loader]
            [clojure.string :as str]
            [clj-avr.utils :as utils]))


;; From http://ww1.microchip.com/downloads/en/DeviceDoc/AVR-Instruction-Set-Manual-DS40002198A.pdf

;; Small parsing language
;; ----------------------

;; all 0s and 1s should match for the target to be parsed
;; A-Z lower case vars groups are read as one signed number (represented as a long)
;; a-z upper case vars groups are read as one unsigned number (represented as a long)
;; mapping to a :keyword++N makes increments the parsed value by N and maps it to :keyword
;; using the [_ :keyword] mapping, maps :keyword to true

(def instructions-table
  [[:nop      {"0000000000000000" '[]}]
   [:.dw      {"00000000cccccccc" '[c :const]}]
   [:movw     {"00000001ddddrrrr" '[d :dst-reg r :src-reg]}]
   [:muls     {"00000010ddddrrrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:mulsu    {"000000110ddd0rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:fmul     {"000000110ddd1rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:fmuls    {"000000111ddd0rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:cp       {"000101rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:cpc      {"000001rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:sub      {"000110rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:sbc      {"000010rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:add      {"000011rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:adc      {"000111rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:cpse     {"000100rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:and      {"001000rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:eor      {"001001rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:or       {"001010rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:mov      {"001011rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:cpi      {"0011kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:subi     {"0101kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:sbci     {"0100kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:ori      {"0110kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:andi     {"0111kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:clc      {"1001010010001000" '[]}]
   [:clh      {"1001010011011000" '[]}]
   [:cli      {"1001010011111000" '[]}]
   [:cln      {"1001010010101000" '[]}]
   [:clr      {"001001dddddddddd" '[d :dst-reg]}]
   [:cls      {"1001010011001000" '[]}]
   [:clt      {"1001010011101000" '[]}]
   [:clv      {"1001010010111000" '[]}]
   [:clz      {"1001010010011000" '[]}]
   [:sec      {"1001010000001000" '[]}]
   [:seh      {"1001010001011000" '[]}]
   [:sei      {"1001010001111000" '[]}]
   [:sen      {"1001010000101000" '[]}]
   [:ses      {"1001010001001000" '[]}]
   [:set      {"1001010001101000" '[]}]
   [:sev      {"1001010000111000" '[]}]
   [:sez      {"1001010000011000" '[]}]

   [:ld       {"1001000ddddd1100" '[d :dst-reg _ :x-reg]
               "1001000ddddd1101" '[d :dst-reg _ :-x-reg]
               "1001000ddddd1110" '[d :dst-reg _ :x+-reg]

               "1000000ddddd1000" '[d :dst-reg _ :y-reg]
               "1001000ddddd1001" '[d :dst-reg _ :y+-reg]
               "1001000ddddd1010" '[d :dst-reg _ :-y-reg]

               "1000000ddddd0000" '[d :dst-reg _ :z-reg]
               "1001000ddddd0001" '[d :dst-reg _ :z+-reg]
               "1001000ddddd0010" '[d :dst-reg _ :-z-reg]}]

   [:ldd      {"10q0qq0ddddd0qqq" '[d :dst-reg q :z-reg+q]
               "10q0qq0ddddd1qqq" '[d :dst-reg q :y-reg+q]}]

   [:st       {"1001001rrrrr1100" '[_ :x-reg  r :src-reg]
               "1001001rrrrr1101" '[_ :x+-reg r :src-reg]
               "1001001rrrrr1110" '[_ :-x-reg r :src-reg]

               "1000001rrrrr1000" '[_ :y-reg   r :src-reg]
               "1001001rrrrr1001" '[_ :y+-reg  r :src-reg]
               "1001001rrrrr1010" '[_ :-y-reg  r :src-reg]

               "1000001rrrrr0000" '[_ :z-reg   r :src-reg]
               "1001001rrrrr0001" '[_ :z+-reg  r :src-reg]
               "1001001rrrrr0010" '[_ :-z-reg  r :src-reg]}]

   [:std      {"10q0qq1rrrrr1qqq" '[q :y-reg+q r :src-reg]
               "10q0qq1rrrrr0qqq" '[q :z-reg+q r :src-reg]}]

   [:lpm      {"1001010111001000" '[]
               "1001000ddddd0100" '[d :dst-reg _ :z-reg]
               "1001000ddddd0101" '[d :dst-reg _ :z+-reg]}]

   [:elpm     {"1001010111011000" '[]
               "1001000ddddd0110" '[d :dst-reg _ :z-reg]
               "1001000ddddd0111" '[d :dst-reg _ :z+-reg]}]

   [:xch      {"1001001ddddd0100" '[d :dst-reg]}]
   [:las      {"1001001ddddd0101" '[d :dst-reg]}]
   [:lac      {"1001001ddddd0110" '[d :dst-reg]}]
   [:lat      {"1001001ddddd0111" '[d :dst-reg]}]
   [:pop      {"1001000ddddd1111" '[d :dst-reg]}]
   [:push     {"1001001ddddd1111" '[d :src-reg]}]
   [:com      {"1001010ddddd0000" '[d :dst-reg]}]
   [:neg      {"1001010ddddd0001" '[d :dst-reg]}]
   [:swap     {"1001010ddddd0010" '[d :dst-reg]}]
   [:inc      {"1001010ddddd0011" '[d :dst-reg]}]
   [:asr      {"1001010ddddd0101" '[d :dst-reg]}]
   [:lsr      {"1001010ddddd0110" '[d :dst-reg]}]
   [:ror      {"1001010ddddd0111" '[d :dst-reg]}]
   [:ret      {"1001010100001000" '[]}]
   [:reti     {"1001010100011000" '[]}]
   [:sleep    {"1001010110001000" '[]}]
   [:break    {"1001010110011000" '[]}]
   [:wdr      {"1001010110101000" '[]}]
   [:spm      {"1001010111101000" '[]}]
   [:dec      {"1001010ddddd1010" '[d :dst-reg]}]
   [:des      {"10010100kkkk1011" '[k :const]}]
   [:cbi      {"10011000aaaaabbb" '[a :io-reg b :bit]}]
   [:sbi      {"10011010aaaaabbb" '[a :io-reg b :bit]}]
   [:sbic     {"10011001aaaaabbb" '[a :io-reg b :bit]}]
   [:sbis     {"10011011aaaaabbb" '[a :io-reg b :bit]}]
   [:in       {"10110aadddddaaaa" '[d :dst-reg a :io-reg]}]
   [:out      {"10111aarrrrraaaa" '[a :io-reg r :src-reg]}]
   [:rjmp     {"1100KKKKKKKKKKKK" '[K :const]}]
   [:ldi      {"1110kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:ser      {"11101111dddd1111" '[d :dst-reg++16]}]
   [:breq     {"111100KKKKKKK001" '[K :const]}]
   [:bld      {"1111100ddddd0bbb" '[d :dst-reg b :bit]}]
   [:bst      {"1111101ddddd0bbb" '[d :dst-reg b:bit]}]
   [:sbr      {"0110kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:sbrc     {"1111110rrrrr0bbb" '[r :src-reg b :bit]}]
   [:sbrs     {"1111111rrrrr0bbb" '[r :src-reg b :bit]}]
   [:brts     {"111100KKKKKKK110" '[K :const]}]
   [:brvc     {"111101KKKKKKK011" '[K :const]}]
   [:brpl     {"111101KKKKKKK010" '[K :const]}]
   [:brlt     {"111100KKKKKKK100" '[K :const]}]
   [:brhs     {"111100KKKKKKK101" '[K :const]}]
   [:fmulsu   {"000000111ddd1rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:brtc     {"111101KKKKKKK110" '[K :const]}]
   [:brie     {"111100KKKKKKK111" '[K :const]}]
   [:brlo     {"111100KKKKKKK000" '[K :const]}]
   [:brid     {"111101KKKKKKK111" '[K :const]}]
   [:brsh     {"111101KKKKKKK000" '[K :const]}]
   [:bset     {"100101000sss1000" '[s :bit]}]
   [:icall    {"1001010100001001" '[]}]
   [:brbs     {"111100KKKKKKKsss" '[s :bit K :const]}]
   [:mul      {"100111rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:rcall    {"1101KKKKKKKKKKKK" '[K :const]}]
   [:brhc     {"111101KKKKKKK101" '[K :const]}]
   [:eijmp    {"1001010000011001" '[]}]
   [:rol      {"000111dddddddddd" '[d :dst-reg]}]
   [:brcc     {"111101KKKKKKK000" '[K :const]}]
   [:brvs     {"111100KKKKKKK011" '[K :const]}]
   [:brcs     {"111100KKKKKKK000" '[K :const]}]
   [:brne     {"111101KKKKKKK001" '[K :const]}]
   [:brbc     {"111101KKKKKKKsss" '[s :bit K :const]}]
   [:brmi     {"111100KKKKKKK010" '[K :const]}]
   [:eicall   {"1001010100011001" '[]}]
   [:tst      {"001000dddddddddd" '[d :dst-reg]}]
   [:brge     {"111101KKKKKKK100" '[K :const]}]
   [:ijmp     {"1001010000001001" '[]}]
   [:bclr     {"100101001sss1000" '[s :bit]}]
   [:lsl      {"000011dddddddddd" '[d :dst-reg]}]
   [:lds      {"1001000ddddd0000kkkkkkkkkkkkkkkk" '[d :dst-reg++16 k :addr]}]
   [:sts      {"1001001ddddd0000kkkkkkkkkkkkkkkk" '[k :addr    d :dst-reg]}]
   [:jmp      {"1001010kkkkk110kkkkkkkkkkkkkkkkk" '[k :addr]}]
   [:call     {"1001010kkkkk111kkkkkkkkkkkkkkkkk" '[k :addr]}]

   ;; TODO: fix this
   ;; this three are weird so will be a little worng for now,
   ;; will require a more expressive binding lang to implement
   ;; or maybe just a hack, will see
   [:adiw     {"10010110kkddkkkk" '[d :dst-reg k :const]}]
   [:sbiw     {"10010111kkddkkkk" '[d :dst-reg k :const]}]
   [:cbr      {"0111kkkkddddkkkk" '[d :dst-reg++16 k :const]}] ;; 16-bit Opcode: (see ANDI with K complemented)

   ])


;; TODO: rename to read program dword, since it is specific
;; of how the program is stored in hex
(defn read-dword [data-bytes]
  (let [[b1 b2 b3 b4] data-bytes]
    (bit-or
     (bit-shift-left (or b2 0) 24)
     (bit-shift-left (or b1 0) 16)
     (bit-shift-left (or b4 0) 8)
     (or b3 0))))

(defn parse-dword
"If dword matches the pattern fixed part return
  pattern vars bit strings extracted from dword.

  Returns nil otherwise.

  Example :

     (parse-dword   10010100000011000000000001011100   ;; <= 2483814492
                  \"1101010kkkkk11ckkkkkkkkkkkkkkkkk\")

     => {k \"0000000000000001011100\"
         c \"0\"}
  "
[dword pattern]
(let [pad-dword-str (fn [s c] (str/replace (utils/format "%32s" s) " " c))
      dword-bin-str (pad-dword-str (utils/n-to-binary-str dword) "0")]
  (loop [[[dw-c p-c] & r ] (map vector dword-bin-str pattern)
         vars {}]
    (if-not p-c

      ;; when there is no more patter to read return the vars map
      vars

      ;; more to read
      (if (#{\0 \1} p-c)

        ;; if it is a 0 or a 1 check it is the same in pattern and value
        (when (= dw-c p-c)
          (recur r vars))

        ;; if it is a var, accumulate the value bit in the corresponding var
        (recur r (update vars p-c str dw-c)))))))

(defn- read-vars
  "Read vars bit strings into dword (longs). If the var is uppercase read it as signed long,
  otherwise reads it as unsigned long."
  [vars]
  (let [lowcase-char? #(<= (utils/char-code \a) (utils/char-code %) (utils/char-code \z))
        upcase-char?  #(<= (utils/char-code \A) (utils/char-code %) (utils/char-code \Z))]
    (reduce-kv (fn [r v bits-str]
                 (let [dword (cond
                               (lowcase-char? v) (utils/ubits->dword bits-str)
                               (upcase-char? v)  (utils/bits->dword bits-str)
                               :else (throw (ex-info "Error parsing vars. A var can only be a char in [a-zA-Z]" {:vars vars :fail v})))]
                   (assoc r (keyword (str v)) dword)))
               {}
               vars)))

(defn bind-vars
  "Reads the bit strings in bits-vars and binds them to bindings applying the small parser rules"
  [bits-vars bindings]

  (let [vars (read-vars bits-vars)
        bind-fn (fn [r [bind-sym bind-key]]
                  (let [increment (when-let [s (second (re-find #"\+\+(.+)" (name bind-key)))]
                                    (utils/parse-int s))
                        bk (if increment
                             (keyword (str/replace (name bind-key) #"\+\+.+" ""))
                             bind-key)]
                    (-> r
                        (assoc bk (if (= bind-sym '_)
                                    true
                                    (cond-> (get vars (keyword bind-sym))
                                      increment (+ increment))))
                        (update :op/args conj bk))))]
    (->> bindings
         (partition 2)
         (reduce bind-fn
                 {:op/args []}))))

(defn next-opcode
  "Finds and eturns the next-opcode in dword (represented as a long).
  Uses clj-avr.disassemble/instructions-table to identify and disassemble it.

  Returns a map representing a instruction like {:op :op/bytes :op/args __op-related-info__}"
  [dword]

  (let [;; the disassemble parser is not expresive enough for parsing adiw and sbiw dest-reg
        ;; so we are fixing it here
        fix-adiw-sbiw-dst-reg (fn [i] (update i :dst-reg (zipmap (range) [24 26 28 30])))
        fix-movw-regs (fn [i]
                        (let [fix (zipmap (range) (remove odd? (range 0 31)))]
                             (-> i (update :dst-reg fix) (update :src-reg fix))))
        inst-finder (fn [[mnem patterns]]
                      (when-let [match (some (fn [[p bindings]]
                                               (when-let [bits-vars (parse-dword dword p)]
                                                 (-> (bind-vars bits-vars bindings)
                                                     (assoc :op/bytes-cnt (quot (count p) 8)
                                                            :op/pattern p
                                                            :op/pattern-vars bits-vars))))
                                             patterns)]
                        (assoc match
                               :op mnem
                               :op/bytes (bit-shift-right dword (* 8 (- 4 (:op/bytes-cnt match)))))))
        inst (some inst-finder instructions-table)]

    (cond-> inst
      (#{:adiw :sbiw} (:op inst)) fix-adiw-sbiw-dst-reg
      (#{:movw} (:op inst))       fix-movw-regs)))

(defn disassemble-bytes
  "Disassemble a collection of bytes into a collection of instructions like:
  {:op :op/bytes :op/args __op-related-info__}"
  [bytes]
  (loop [bs bytes
         instructions []]
    (if (empty? bs)
      instructions

      (let [next-dword (read-dword bs)
            inst (next-opcode next-dword)]

        (when-not inst
          (println "Couldn't disassemble instruction" {:dword next-dword
                                                       :dword-bin (utils/n-to-binary-str next-dword)
                                                       :dword-hex (utils/padded-hex next-dword 0)}))

        (recur (drop (:op/bytes-cnt inst) bs)
               (conj instructions inst))))))

(defn add-instr-mem-addresses [from insts]
  (second
   (reduce (fn [[addr r] inst]
             [(+ addr (:op/bytes-cnt inst))
              (conj r (assoc inst :memory/address addr))])
           [from []]
           insts)))

(defn disassemble-hex
  "Disassembles a entire hex, as loaded by clj-avr.hex-loader/load-hex"
  [hex]
  (let [disasm-line (fn disasm-line [{:keys [address data]}]
                      (->> (disassemble-bytes data)
                           (add-instr-mem-addresses address)))]
    (->> hex
         (filter #(= (:type %) :data))
         (mapcat disasm-line))))

;;;;;;;;;;;;;;
;; Printing ;;
;;;;;;;;;;;;;;

(def rel-branch-ops #{:brne :rjmp :rcall})
(def long-address-ops #{:jmp :call})

(def reg-formatter (partial str "r"))
(def hex-formatter #(str "0x" (utils/padded-hex % 2)))
(def formatters
  {:bit     hex-formatter
   :addr    (fn [v] (if (> v 0) (hex-formatter v) (str v)))
   :const   hex-formatter

   :src-reg reg-formatter
   :dst-reg reg-formatter
   :io-reg  hex-formatter
   :reg     reg-formatter

   :x-reg  (constantly "X")
   :x+-reg (constantly "X+")
   :-x-reg (constantly "-X")

   :y-reg   (constantly "Y")
   :y+-reg  (constantly "Y+")
   :-y-reg  (constantly "-Y")
   :y-reg+q #(utils/format "Y+%d" %)

   :z-reg  (constantly "Z")
   :z+-reg  (constantly "Z+")
   :-z-reg (constantly "-Z")
   :z-reg+q #(utils/format "Z+%d" %)})

(defn disassemble-inst-str [{:keys [:memory/address :op/bytes :op :op/args] :as inst}]
  (with-out-str
   (when address (print (utils/format "0x%s " (utils/padded-hex address 8))))
   (print (utils/format "%s\t" (if op (name op) "<fail>")))
   (print (->> args
               (map (fn [a]
                      (let [arg-v (get inst a)]
                        (cond
                          ;; I don't understand this yet, but looking at other disassemblers the long address ones
                          ;; are shifted one bit to the left. Couldn't find any documentation.
                          (long-address-ops op) ((formatters a) (bit-shift-left arg-v 1))
                          (rel-branch-ops op)   (and arg-v (utils/format ".%s%d"  (if (neg? arg-v) "" "+") (* 2 arg-v)))
                          :else                 ((formatters a) arg-v)))))
               (str/join ", ")))
   (print "\t\n")))

(defn print-disassemble
  "Prints a disassemle to standard output."
  [disassemble]
  (doseq [inst disassemble]
    (print (disassemble-inst-str inst))))


;;;;;;;;;;
;; Main ;;
;;;;;;;;;;

#?(:clj
   (defn -main [& [hex-path]]
     (-> hex-path
         slurp
         hex-loader/parse-hex
         disassemble-hex
         print-disassemble)))

;;;;;;;;;;;;;;;;;;
;; Repl testing ;;
;;;;;;;;;;;;;;;;;;

(comment
  (def data-bytes '(12 148 92 0 12 148 110 0 12 148 110 0 12 148 110 0))
  (def first-dword (read-dword data-bytes))
  (utils/n-to-binary-str first-word)

  (utils/n-to-binary-str first-dword)
  (def jump-pat "1001010kkkkk11ckkkkkkkkkkkkkkkkk")

  (= (parse-dword first-dword "1001010kkkkk11ckkkkkkkkkkkkkkkkk") {\k "0000000000000001011100", \c "0"})

  (next-opcode first-dword)

  (-> (hex-loader/parse-hex (slurp "./resources/blink.hex"))
      disassemble-hex
      print-disassemble)
  (disassemble-hex (hex-loader/parse-hex (slurp "./resources/Blink.ino.hex")))

  )
