(ns clj-avr.disassembler
  (:require [clj-avr.hex-loader :as hex-loader]
            [clojure.string :as str]
            [clj-avr.utils :as utils]))


;; From http://ww1.microchip.com/downloads/en/DeviceDoc/AVR-Instruction-Set-Manual-DS40002198A.pdf
(def instructions-table
  [[:nop      {"0000000000000000" '[]}]
   [:.dw      {"00000000cccccccc" '[c :const]}]
   [:movw     {"00000001DDDDRRRR" '[D :dst-reg R :src-reg]}]
   [:muls     {"00000010ddddrrrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:mulsu    {"000000110ddd0rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:fmul     {"000000110ddd1rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:fmuls    {"000000111ddd0rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:cp       {"000101rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:cpc      {"000001rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:sub      {"000110rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:sbc      {"000010rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:add      {"000111rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:adc      {"000011rdddddrrrr" '[d :dst-reg r :src-reg]}]
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
   [:ser      {"11101111dddd1111" '[d :dst-reg++16]}]
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
   [:push     {"1001001ddddd1111" '[d :dst-reg]}]
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
   [:rjmp     {"1100kkkkkkkkkkkk" '[k :addr]}]
   [:ldi      {"1110kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:breq     {"111100kkkkkkk001" '[k :const]}]
   [:bld      {"1111100ddddd0bbb" '[d :dst-reg b :bit]}]
   [:bst      {"1111101ddddd0bbb" '[d :dst-reg b:bit]}]
   [:sbr      {"0110kkkkddddkkkk" '[d :dst-reg++16 k :const]}]
   [:sbrc     {"1111110rrrrr0bbb" '[r :src-reg b :bit]}]
   [:sbrs     {"1111111rrrrr0bbb" '[r :src-reg b :bit]}]
   [:brts     {"111100kkkkkkk110" '[k :const]}]
   [:brvc     {"111101kkkkkkk011" '[k :const]}]
   [:brpl     {"111101kkkkkkk010" '[k :const]}]
   [:brlt     {"111100kkkkkkk100" '[k :const]}]
   [:brhs     {"111100kkkkkkk101" '[k :const]}]
   [:fmulsu   {"000000111ddd1rrr" '[d :dst-reg++16 r :src-reg++16]}]
   [:brtc     {"111101kkkkkkk110" '[k :const]}]
   [:brie     {"111100kkkkkkk111" '[k :const]}]
   [:brlo     {"111100kkkkkkk000" '[k :const]}]
   [:brid     {"111101kkkkkkk111" '[k :const]}]
   [:brsh     {"111101kkkkkkk000" '[k :const]}]
   [:bset     {"100101000sss1000" '[s :bit]}]
   [:icall    {"1001010100001001" '[]}]
   [:brbs     {"111100kkkkkkksss" '[s :bit k :const]}]
   [:mul      {"100111rdddddrrrr" '[d :dst-reg r :src-reg]}]
   [:rcall    {"1101kkkkkkkkkkkk" '[k :const]}]
   [:brhc     {"111101kkkkkkk101" '[k :const]}]
   [:eijmp    {"1001010000011001" '[]}]
   [:rol      {"000111dddddddddd" '[d :dst-reg]}]
   [:brcc     {"111101kkkkkkk000" '[k :const]}]
   [:brvs     {"111100kkkkkkk011" '[k :const]}]
   [:brcs     {"111100kkkkkkk000" '[k :const]}]
   [:brne     {"111101kkkkkkk001" '[k :const]}]
   [:brbc     {"111101kkkkkkksss" '[s :bit k :const]}]
   [:brmi     {"111100kkkkkkk010" '[k :const]}]
   [:eicall   {"1001010100011001" '[]}]
   [:tst      {"001000dddddddddd" '[d :dst-reg]}]
   [:brge     {"111101kkkkkkk100" '[k :const]}]
   [:ijmp     {"1001010000001001" '[]}]
   [:bclr     {"100101001sss1000" '[s :bit]}]
   [:lsl      {"000011dddddddddd" '[d :dst-reg]}]
   [:lds      {"1001000ddddd0000kkkkkkkkkkkkkkkk" '[d :dst-reg++16 k :addr]}]
   [:sts      {"1001001ddddd0000kkkkkkkkkkkkkkkk" '[k :addr    d :dst-reg]}]
   [:jmp      {"1001010kkkkk110kkkkkkkkkkkkkkkkk" '[k :addr]}]
   [:call     {"1001010kkkkk111kkkkkkkkkkkkkkkkk" '[k :addr]}]
   ;; this three are weird so will be a little worng for now,
   ;; will require a more expressive binding lang to implement
   ;; or maybe just a hack, will see
   [:adiw     {"10010110kkppkkkk" '[]}]
   [:sbiw     {"10010111kkppkkkk" '[]}]
   [:cbr      {"0111kkkkddddkkkk" '[d :dst-reg++16 k :const]}] ;; 16-bit Opcode: (see ANDI with K complemented)

   ])

(def reg-formatter (partial str "R"))
(def hex-formatter (partial format "0x%04x"))
(def formatters
  {:bit     hex-formatter
   :addr    hex-formatter
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
   :y-reg+q #(format "Y+%d" %)

   :z-reg  (constantly "Z")
   :z+-reg  (constantly "Z+")
   :-z-reg (constantly "-Z")
   :z-reg+q #(format "Z+%d" %)})

(defn read-word [data-bytes]
(let [[b1 b2] data-bytes]
  (bit-or (bit-shift-left b2 8) b1)))

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
  pattern vars values extracted from dword.

  Returns nil otherwise.

  Example :

     (parse-dword   10010100000011000000000001011100   ;; <= 2483814492
                  \"1101010kkkkk11ckkkkkkkkkkkkkkkkk\")

     => {k 92, c 0}
  "
[dword pattern]
(let [pad-dword-str (fn [s c] (str/replace (format "%32s" s) " " c))
      dword-bin-str (pad-dword-str (utils/n-to-binary-str dword) "0")]
  (loop [[[dw-c p-c] & r ] (map vector dword-bin-str pattern)
         vars {}]
    (if-not p-c

      (reduce-kv (fn [r v bits-str]
                   (assoc r (keyword (str v)) (utils/bits->dword bits-str)))
                 {}
                 vars)

      (if (#{\0 \1} p-c)
        (when (= dw-c p-c)
          (recur r vars))

        (recur r (update vars p-c str dw-c)))))))

(defn bind-vars
  "Bind the vars returned by parse-dword using a binding form like the ones in the instructions-table"
  [vars bindings]
  (let [bind-fn (fn [r [bind-sym bind-key]]
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

  (let [inst-finder (fn [[mnem patterns]]
                      (when-let [match (some (fn [[p bindings]]
                                               (when-let [vars (parse-dword dword p)]
                                                 (-> (bind-vars vars bindings)
                                                     (assoc :op/bytes-cnt (quot (count p) 8)))))
                                             patterns)]
                        (assoc match
                               :op mnem
                               :op/bytes (bit-shift-right dword (* 8 (- 4 (:op/bytes-cnt match)))))))]
    (some inst-finder instructions-table)))

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
                                                       :dword-hex (format "%x" next-dword)}))

        (recur (drop (:op/bytes-cnt inst) bs)
               (conj instructions inst))))))

(defn disassemble-hex
  "Disassembles a entire hex, as loaded by clj-avr.hex-loader/load-hex"
  [hex]
  (let [disasm-line (fn disasm-line [{:keys [address data]}]
                      (let [disasm (disassemble-bytes data)]
                        (second
                         (reduce (fn [[addr r] inst]
                                   [(+ addr (:op/bytes-cnt inst))
                                    (conj r (assoc inst :memory/address addr))])
                                 [address []]
                                 disasm))))]
    (->> hex
         (filter #(= (:type %) :data))
         (mapcat disasm-line))))

(defn print-disassemble
  "Prints a disassemle to standard output."
  [disassemble]
  (doseq [{:keys [:memory/address :op/bytes :op :op/args] :as inst} disassemble]
    (when address (print (format "%x: \t" address)))
    (print (let [[b1 b2 b3 b4] (->> (format "%04x" bytes)
                                    (partition 2)
                                    (map #(apply str %)))]
             (format "%-16s"
                     (cond-> (str b2)
                       b1 (str " " b1)
                       b4 (str " " b4)
                       b3 (str " " b3)))))
    (print (format "%-8s" (if op (name op) "<fail>")))
    (print (->> args
                (map (fn [a] ((formatters a) (get inst a))))
                (str/join ", ")))
    (print "\n")))

(defn -main [& [hex-path]]
  (-> hex-path
      hex-loader/load-hex
      disassemble-hex
      print-disassemble))

(comment
  (def data-bytes '(12 148 92 0 12 148 110 0 12 148 110 0 12 148 110 0))
  (def first-word (read-word data-bytes))
  (def first-dword (read-dword data-bytes))
  (utils/n-to-binary-str first-word)

  (utils/n-to-binary-str first-dword)
  (def jump-pat "1001010kkkkk11ckkkkkkkkkkkkkkkkk")

  (utils/n-to-binary-str (make-mask (indexes #{\0 \1} "1001010kkkkk11ckkkkkkkkkkkkkkkkk")))

  (parse-pattern "1001010kkkkk11ckkkkkkkkkkkkkkkkk")
  (next-opcode first-dword)

  (-> (hex-loader/load-hex "./resources/Blink.ino.hex")
      disassemble-hex
      print-disassemble)
  (disassemble-hex (hex-loader/load-hex "./resources/Blink.ino.hex"))

  )
