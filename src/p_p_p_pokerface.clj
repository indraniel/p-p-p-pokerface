(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[snd _] card]
     (if (Character/isDigit snd)
       (Integer/valueOf (str snd))
       (replacements snd))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ rank-counts (set (vals (frequencies (map rank hand)))) ]
    (if (contains? rank-counts 2) true false )))

(defn three-of-a-kind? [hand]
  (let [ rank-counts (set (vals (frequencies (map rank hand)))) ]
    (if (contains? rank-counts 3) true false )))

(defn four-of-a-kind? [hand]
  (let [ rank-counts (set (vals (frequencies (map rank hand)))) ]
    (if (contains? rank-counts 4) true false )))

(defn flush? [hand]
  (let [ suit-counts (set (vals (frequencies (map suit hand)))) ]
    (if (contains? suit-counts 5) true false )))

(defn full-house? [hand]
  (let [ rank-counts (set (vals (frequencies (map rank hand)))) ]
    (if (and (contains? rank-counts 2) (contains? rank-counts 3)) true false )))


(defn two-pairs? [hand]
  (let [ rank-occurences (frequencies (vals (frequencies (map rank hand)))) 
         pair-occurences (if (contains? rank-occurences 2) (get rank-occurences 2) 0) ]
     (if (= 2 pair-occurences) true false)))


(defn straight? [hand]
  (let [ high-ace-ranks (sort (map rank hand))
         low-ace-ranks  (sort (replace {14 1} high-ace-ranks))
         range-hand     (fn [h] (range (apply min h) (+ 1 (apply max h)))) ]
    (if (or (= high-ace-ranks (range-hand high-ace-ranks))
            (= low-ace-ranks  (range-hand low-ace-ranks)))
        true
        false)))

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    true
    false))

(defn value [hand]
  (cond
     (straight-flush?  hand) 8
     (four-of-a-kind?  hand) 7
     (full-house?      hand) 6
     (flush?           hand) 5
     (straight?        hand) 4
     (three-of-a-kind? hand) 3
     (two-pairs?       hand) 2
     (pair?            hand) 1
     :else                   0))
