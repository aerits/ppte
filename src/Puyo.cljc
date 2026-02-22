(ns Puyo
  (:require
   [PuyoTypes :as pt]))

(defn create-board [rows cols val]
  (vec (map (fn [_] (vec (repeat cols val))) (range rows))))
(defn board-get [v [x y]]
  (-> v (get y) (get x)))
(defn board-get-type [v [x y]]
  (-> (board-get v [x y]) first))

(defn +v [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])
(defn create-puyo [x y color]
  [x y color])
(declare player_grounded? player_move-left player_move-right player_move-down board_place-player)
(defn create-player
  "x and y are position on grid
  rotation starts off as up
  returns a map {:blocks [puyo puyo] :rot :UP :pos [x y]}
  "
  [x y color1 color2]
  {:blocks [(create-puyo 0 -1 color1) (create-puyo 0 0 color2)] :pos [x (+ 1 y)]})
(defn vec_rotate [[x y]]
  [(* -1 y) x])
(defn puyo_rotate [[x y color] rots]
  (if (> rots 0)
    (recur (apply create-puyo (concat (vec_rotate [x y]) (list color))) (dec rots))
    [x y color]))
(defn player_rotate [p board ccw-rotations]
  (when p
    (let [new-player (assoc p :blocks (map #(puyo_rotate %  ccw-rotations) (:blocks p)))]
      (if (player_grounded? new-player board)
        (let [left-p (player_move-left new-player)
              right-p (player_move-right new-player)
              up-p (player_move-down new-player -1.01)
              up-left-p (player_move-left up-p)
              up-right-p (player_move-right up-p)
              fn-land-player (fn [p board] (second (board_place-player board (player_move-down p 2))))]
          (cond
            (not (player_grounded? up-p board)) (fn-land-player up-p board)
            (not (player_grounded? left-p board)) left-p
            (not (player_grounded? right-p board)) right-p
            (not (player_grounded? up-left-p board)) (fn-land-player up-left-p board)
            (not (player_grounded? up-right-p board)) (fn-land-player up-right-p board)
            :else p))
        new-player))))
;; (println (player_rotate (create-player 0 0 :pt/blue :pt/blue) 1))

(defn create-falling-block [x y color]
  {:blocks [(create-puyo 0 0 color)] :pos [x y] :yspeed 0})

(defn player_move-down [p dist]
  (when p (update p :pos update 1 + dist)))
(defn falling-block_move-down [block accel]
  (when block
    (-> (update block :yspeed + accel)
        (player_move-down (:yspeed block)))))
(defn falling-blocks_move-down [blocks accel]
  ;; (println blocks)
  (map #(falling-block_move-down % accel) blocks))
(defn garbage-puyo_create "returns falling-blocks"
  ([falling-blocks num]
   (garbage-puyo_create falling-blocks num 0))

  ([falling-blocks num layer]
   (cond
     (< num 6)
     (let [blocks (for [i (range 6)]
                    (create-falling-block i layer :pt/garbage))
           num-to-remove (- 6 num)]
       (loop [blocks (into #{} blocks)
              num-to-remove num-to-remove]
         (if (> num-to-remove 0)
           (recur (disj blocks (rand-nth (into [] blocks))) (dec num-to-remove))
           (concat falling-blocks (into [] blocks)))))

     (= num 6)
     (let [blocks (for [i (range 6)]
                    (create-falling-block i layer :pt/garbage))]
       (concat falling-blocks blocks))

     (> num 6)
     (->
      (garbage-puyo_create falling-blocks 6 layer)
      (recur (- num 6) (- layer 1))))))

(defn s+ "addition with max, never go over max" [x y max]
  (if (>= x max)
    max
    (+ x y)))
(defn s- "subtraction with min, don't go below min" [x y min]
  (if (<= x min)
    min
    (- x y)))
(defn l+ "wraparound add" [x y max]
  (if (>= x max)
    0
    (+ x y)))
(defn player_move-left [p]
  (when p (update p :pos update 0 s- 1 0)))
(defn player_move-right [p]
  (when p (update p :pos update 0 s+ 1 5)))
(defn player_move-checked [p fun board]
  (let [new-player (fun p)
        up-new-player (player_move-down new-player -0.5)]
    (if (player_grounded? new-player board)
      (if (player_grounded? up-new-player board)
        p
        (second (board_place-player board new-player)))
      new-player)))

(defn player_grounded? [player board]
  (when player

    (let [blocks (:blocks player)
          pos (:pos player)
          x (get pos 0)
          y (get pos 1)
          y (Math/floor y)
          y (+ y 1)
          block-pos (map (fn [block]
                           (let [bx (get block 0)
                                 by (get block 1)
                                 bx (+ bx x)
                                 by (+ by y)
                                 grid-point (board-get-type board [bx by])]
                             (list bx by grid-point)))
                         blocks)]
      (loop [b block-pos
             block (first b)]
        (if (and (seq b) (not (= (last block) :pt/empty)))
          true
          (if (seq b)

            (recur (rest b) (second b))
            nil))))))

(defn board_place-player "returns [board p] where p has the new position" [board p]
  ;; (println p)
  (let [p (loop [p p]
            ;; if y = -1, ur dead
            (if (> -1 (get (:pos p) 1))
              nil
              (if (player_grounded? (player_move-down p -1) board)
                (recur (player_move-down p -1))
                p)))

        blocks (:blocks p)
        pos (:pos p)
        x (get pos 0)
        y (get pos 1)
        y (Math/floor y)
        block-pos (map (fn [block]
                         (let [bx (get block 0)
                               by (get block 1)
                               block-type (get block 2)
                               bx (+ bx x)
                               by (+ by y)]
                           (list bx by block-type)))
                       blocks)]
    (when p
      (loop [b block-pos
             block (first b)
             board board]
        (if (seq b)
          (recur (rest b) (second b)
                 (assoc-in board [(second block) (first block)] [(last block)]))
          [board (assoc p :pos [x y])])))))
(defn board_place-grounded-falling-blocks "returns [board fallingblocks placedblocks]" [board blocks]
  (let [blocks (for [block blocks]
                 (if (player_grounded? block board)
                   [true block]
                   [nil block]))
        grounded-blocks (map (fn [[_ block]] block) (filter #(not (= nil (first %)))
                                                            blocks))
        new-blocks (map (fn [[_ block]] block) (filter #(= nil (first %)) blocks))
        [new-board placed] (reduce (fn [[board placedb] block]
                                     (let [[newboard placed] (board_place-player board block)]
                                       [newboard (conj placedb placed)]))
                                   [board []] grounded-blocks)]
    [new-board new-blocks placed]))

(defn dfs [board [x y] color visited]
  (if (and (= color (board-get-type board [x y])) (not (contains? visited [x y])))
    (let [dirs [[-1 0] [1 0] [0 1] [0 -1]]
          neighbors (map #(+v % [x y]) dirs)
          neighbors (filter #(= color (board-get-type board %)) neighbors)]
      (reduce #(dfs board %2 color %1) (conj visited [x y]) neighbors))
    visited))

(defn board_pop-puyos--find-garbage "returns [[x y c]]"
  ([board blocks-to-remove]
   (board_pop-puyos--find-garbage board blocks-to-remove []))
  ([board blocks-to-remove garbage-blocks]
   (if (seq blocks-to-remove)
     (let [dirs [[-1 0] [1 0] [0 1] [0 -1]]
           [bx by _bc] (first blocks-to-remove)
           blocks (for [[x y] dirs]
                    (if (= (board-get-type board (+v [x y] [bx by])) :pt/garbage)
                      (conj (+v [x y] [bx by]) :pt/garbage)
                      nil))
           blocks (filter #(not (nil? %)) blocks)]
       (board_pop-puyos--find-garbage board (rest blocks-to-remove) (concat garbage-blocks blocks)))
     garbage-blocks)))

(defn board_pop-puyos "returns [board [num-puyos-popped colors] popped-blocks], colors is hashset" [board]
  (let [rows (count board)
        cols (count (get board 0))
        blocks-to-remove
        (for [y (range rows) x (range cols)]
          (when ((into #{} pt/constructable) (board-get-type board [x y]))
            [x y (board-get-type board [x y]) (count (dfs board [x y] (board-get-type board [x y]) #{}))]))

        blocks-to-remove (filter #(not (= nil %)) blocks-to-remove)
        blocks-to-remove (filter #(>= (last %) 4) blocks-to-remove)
        blocks-to-remove (map (fn [[x y c _]] [x y c]) blocks-to-remove)
        blocks-to-remove (->> (concat blocks-to-remove (board_pop-puyos--find-garbage board blocks-to-remove))
                              (filter #(seq %)))
        popped-blocks (map (fn [[x y c]] (create-falling-block x y c)) blocks-to-remove)]
    (reduce
     (fn [[board puyos-popped# colors] [x y c]]
       [(assoc-in board [y x] [:pt/empty]) (inc puyos-popped#) (conj colors c) popped-blocks])
     [board 0 #{}]
     blocks-to-remove)))

(defn board_get-falling-puyos "returns [board [p1 p2 p3 ..]]" [board]
  (let [rows (count board)
        cols (count (get board 0))
        blocks-to-remove
        (for [y (range rows) x (range cols)]
          (when (not (= :pt/empty (board-get-type board [x y])))
            [x y (board-get-type board [x y]) (board-get-type board (+v [x y] [0 1]))]))

        blocks-to-remove (filter #(not (= nil %)) blocks-to-remove)
        blocks-to-remove (filter #(= (last %) :pt/empty) blocks-to-remove)
        falling (map (fn [[x y c]] (create-falling-block x y c)) blocks-to-remove)
        blocks-to-remove (map (fn [[x y _]] [x y]) blocks-to-remove)]
    [(reduce (fn [board [x y]] (assoc-in board [y x] [:pt/empty])) board blocks-to-remove)
     (vec falling)])) ;; same as a player

(defn fill-piece-queue "queue is a list" [queue len]
  (if (< (count queue) len)
    (recur (conj queue
                 (create-player 2 0 (rand-nth pt/constructable) (rand-nth pt/constructable)))
           2)
    queue))

(def score_chain-power-table [-1 0 8 16 32 64 96 128 160 192 224 256 288
                              320 352 384 416 448 480 512 544 576 608 640 672])
(def score_color-bonus-table [-1 0 3 6 12 24])
(def score_group-bonus-table [-1 -1 -1 -1 0 2 3 4 5 6 7 10])
(defn score_get-in-table [table i]
  (if (>= i (count table))
    (last table)
    (table i)))

(defn score_calculate
  "groups is vec of group sizes"
  [puyos-cleared chain# color# group#]
  (loop [total 0
         pc puyos-cleared
         chain# chain#
         color# color#
         group# group#]
    (if (empty? pc)
      total
      (recur
       (+ total (* 10 (first pc)
                   (let [n (+ (score_get-in-table score_chain-power-table (first chain#))
                              (score_get-in-table score_color-bonus-table (first color#))
                              (score_get-in-table score_group-bonus-table (first group#)))]
                     (cond
                       (> n 999) 999
                       (< n 1) 1
                       :else n))))
       (rest pc) (rest chain#) (rest color#) (rest group#)))))

(defn score_chain-score-update
  ([globalstate blocks]
   (if (> (count blocks) 0)
     (score_chain-score-update globalstate blocks true)
     (->
      (assoc globalstate :chain-score 0)
      (dissoc :chain-colors)
      (dissoc :chain-groups))))

  ([globalstate blocks _]
   (let [colors (reduce
                 (fn [colors {[[_x _y c]] :blocks}]
                   (conj colors c))
                 #{}
                 blocks)
         new-state
         (-> (update globalstate :chain-colors #(conj % [colors]))
             (update :chain-groups #(concat % [(count blocks)])))]
     (assoc new-state :chain-score
            (score_calculate
             (:chain-groups new-state)
             (map inc (range (:chain new-state)))
             (map count (:chain-colors new-state))
             (:chain-groups new-state))))))

(defn run-hook [globalstate hook & args]
  (reduce #(apply %2 %1 args) globalstate hook))

(defn state-is [state enum]
  (= (:process state) enum))
(defn state-fall-blocks [globalstate state hooks currentTime deltaTime minSpeed]
  (let [[board fallingblocks] (board_get-falling-puyos (:board globalstate))
        fallingblocks (apply conj (:falling-blocks globalstate) fallingblocks)
        fallingblocks (map (fn [block] (if (< (:yspeed block) minSpeed)
                                         (assoc block :yspeed minSpeed)
                                         block)) fallingblocks)
        fallingblocks (falling-blocks_move-down fallingblocks (* deltaTime 0.0007))

        [board fallingblocks placedblocks] (board_place-grounded-falling-blocks board fallingblocks)
        new-globalstate (-> globalstate
                            (run-hook (:hook-block-land hooks) placedblocks)
                            (assoc :board board)
                            (assoc :falling-blocks fallingblocks))]
    [new-globalstate (if (> (count fallingblocks) 0)
                       state
                       {:process :s/fall-fast :next-state [:s/pop currentTime 100]})]))
(defn state-update
  "state is {}, returns [globalstate state]
  hooks include :hook-block-land and :hook-block-pop
  "
  [globalstate state hooks currentTime deltaTime]
  (let [globalstate (update globalstate :piece-queue fill-piece-queue 2)]
    (if (contains? state :next-state)
     ;; change to next state
      (let [next-state (:next-state state)
            [next-state previousTime delta] next-state]
        (if (> (- currentTime previousTime) delta)
          [globalstate {:process next-state}]
          [globalstate state]))

      (cond
        (state-is state :s/dead)
        [globalstate state]

        (state-is state :s/new-player)
        [(-> (assoc globalstate :player (peek (:piece-queue globalstate)))
             (update :piece-queue pop)
             (assoc :chain 0))
         {:process :s/new-player :next-state [:s/player-fall currentTime 0]}]

        (state-is state :s/player-fall)
        (if (player_grounded? (:player globalstate) (:board globalstate))
          (let [[new-board placed-pos] (board_place-player (:board globalstate) (:player globalstate))
                placed-pos (if (:player/groundTime placed-pos) placed-pos (assoc placed-pos :player/groundTime currentTime))]
            (if new-board
             ;; if (currentTime - groundTime) > 1000, place player
             ;; else: stop player from falling through the ground
              (if (> (- currentTime (:player/groundTime placed-pos)) 1000)
                [(-> globalstate
                     (run-hook (:hook-block-land hooks) (list placed-pos))
                     (assoc :board new-board)
                     (assoc :player nil))
                 {:process :s/player-fall :next-state [:s/fall-slow currentTime 100]}]
                [(assoc globalstate :player placed-pos) state])

              [(assoc globalstate :player nil) {:process :s/player-fall :next-state [:s/dead currentTime 0]}]))
         ;; player is not on ground so let player move down
         ;; if player doesn't land on the ground, remove ground time
          (let [player (-> (:player globalstate)
                           (player_move-down (* 0.0005 deltaTime))
                           ((fn [player] (if (player_grounded? player (:board globalstate))
                                           player
                                           (dissoc player :player/groundTime)))))]
            [(assoc globalstate :player player) state]))

        (state-is state :s/fall-slow)
        (state-fall-blocks globalstate state hooks currentTime deltaTime 0)

        (state-is state :s/fall-fast)
        (state-fall-blocks globalstate state hooks currentTime deltaTime 0.3)

        (state-is state :s/pop)
        (let [[new-board puyos-popped# _colors popped] (board_pop-puyos (:board globalstate))
              popped? (> puyos-popped# 0)]
          [(-> (assoc globalstate :board new-board) (run-hook (:hook-block-pop hooks) popped))
           {:process :s/pop :next-state (if popped? [:s/fall-fast currentTime 700] [:s/new-player currentTime 0])}])))))

(defn create-globalstate
  ([queue]
   {:board (create-board 14 6 [:pt/empty])
    :player (create-player 2 0 :pt/red :pt/red)
    :particles {}
    :state-enum {:process :s/pop}
    :chain 0
    :piece-queue queue
    :das 131 ;; ms
    :keys {}
    :falling-blocks (garbage-puyo_create [] 10)})
  ([queue das]
   (-> (create-globalstate queue)
       (assoc :das das))))
