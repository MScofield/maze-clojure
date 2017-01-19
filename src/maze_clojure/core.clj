(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []
  (vec
    (for [row (range size)]
      (vec
        (for [col (range size)]
          {:row row, :col col, :visited? false, :bottom? true, :right? true, 
           :start? false, :end? false})))))
          
(defn possible-neighbors [rooms row col]
  (vec
    (filter
      (fn [room]
        (and room (= false (:visited? room))))
      [(get-in rooms [(dec row) col])
       (get-in rooms [(inc row) col])
       (get-in rooms [row (dec col)])
       (get-in rooms [row (inc col)])])))

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))
      
(defn first-end? [rooms row col]
  (< 1 (count (set (map :end? rooms)))))  

;;(defn mark-end [rooms row col]
;;  firstEnd (first-end? rooms row col)
;;  (if firstEnd
;;    (assoc-in rooms [row col :end?] true)
;;    rooms))
    
       
(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))
    
(declare create-maze)

(defn create-maze-loop [rooms old-row old-col new-row new-col]
  (let [new-rooms (tear-down-wall rooms old-row old-col new-row new-col)
        new-rooms (create-maze new-rooms new-row new-col)]
    (if (= rooms new-rooms)
      rooms
      (create-maze-loop new-rooms old-row old-col new-row new-col))))

(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visited?] true)
   ;;     rooms (if 
   ;;             (and 
   ;;                  (first-end? rooms row col)
   ;;                  (not random-neighbor rooms row col)
   ;;             (assoc-in rooms [row col :end?] true))
        next-room (random-neighbor rooms row col)]
    (if next-room
          (create-maze-loop rooms row col 
            (:row next-room)(:col next-room)) rooms)))
      ;;(if (first-end? rooms row col) (assoc-in rooms [row col :end?] true)))
    ;;rooms))

(defn -main []
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)
        rooms (assoc-in rooms [0 0 :start?] true)]
    (doseq [_ rooms]
      (print " _"))
    (println)
    (doseq [row rooms]
      (print "|")
      (doseq [room row]
        (if (:start? room)
          (print "o")
          (if (:end? room)
            (print "x")
            (if (:bottom? room)
              (print "_")
              (print " "))))
        (if (:right? room)
          (print "|")
          (print " ")))
      (println))))
