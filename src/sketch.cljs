(ns sketch
  (:require [goog.object :as g]
            [PuyoTypes :as pt]
            [Particle :as pcl]
            )
  #_{:clj-kondo/ignore [:unused-import]}
  (:import p5)
  (:require-macros [Macros :as m]))

;; TODO switch to drawing on a texture
;; TODO refactor into multiple files
;; TODO add lock delay
;; TODO add a menu

(defn getTime []
  (js/performance.now))
(defmacro with [f-begin f-end & body]
  (f-begin)
  `(~@body)
  (f-end))

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
              fn-land-player (fn [p board] (second (board_place-player board (player_move-down p 2))))
              ]
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
        (second (board_place-player board new-player) ))
      new-player)))

(defn player_grounded? [player board]
  (when player

    (let [blocks (:blocks player)
          pos (:pos player)
          x (get pos 0)
          y (get pos 1)
          y (js/ceil y)
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
        y (js/floor y)
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

(defn board_pop-puyos "returns [board [num-puyos-popped colors] popped-blocks], colors is hashset" [board]
  (let [rows (count board)
        cols (count (get board 0))
        blocks-to-remove
        (for [y (range rows) x (range cols)]
          (when (not (= :pt/empty (board-get-type board [x y])))
            [x y (board-get-type board [x y]) (count (dfs board [x y] (board-get-type board [x y]) #{}))]))

        blocks-to-remove (filter #(not (= nil %)) blocks-to-remove)
        blocks-to-remove (filter #(>= (last %) 4) blocks-to-remove)
        blocks-to-remove (map (fn [[x y c _]] [x y c]) blocks-to-remove)
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
     (vec falling) ;; same as a player
     ]))

(defonce state (atom
                {:board (create-board 12 6 [:pt/empty])
                 :player (create-player 2 0 :pt/red :pt/red)
                 :particles {}
                 :state-enum {:process :s/pop}
                 :chain 0
                 :piece-queue #queue []
                 :das 131 ;; ms
                 :textures {}
                 :fonts {}
                 :keys {}
                 :falling-blocks []}))
(def timer (atom {:timer/lastUpdate (getTime) :timer/dt 50}))
;; (def fb (js/createFramebuffer))
(defn draw-board
  "
  draw board v
  each element is drawn using the map handle {key function}
  each key in the map corresponds to an element in v
  "
  [v handle x y]
  (dotimes [row (count v)]
    (dotimes [col (count (get v 0))]
      (when (not (contains? (:particles @state) [col row]))
        (let [el (board-get-type v [col row])
            ;; right left up down
              dirs '([1 0] [-1 0] [0 -1] [0 1])
              surrounding (map (fn [dirvec]
                                 (let [dir (+v dirvec [col row])]
                                   (if (not (contains? (:particles @state) dir))
                                     (board-get-type v dir)
                                     (board-get-type v [200 200])))) dirs)
              f (get handle el)]
          (if f
          ;; (when (= el :pt/red)
          ;; (println surrounding)
          ;;   )
            (f col row x y surrounding)
            (println (str "Draw handle not implemented for " el))))))))

(defn draw-player
  [p handle x y]
  (when p
    (let [px (get (:pos p) 0)
          py (get (:pos p) 1)
          transparency (-> (- (getTime) (:player/groundTime p)) (/ 1000) (- 1) (* -1) (* 255))
          transparency (if (:player/groundTime p) transparency 255)]
      (doseq [blocks (:blocks p)]
        (let [bx (get blocks 0)
              by (get blocks 1)
              c (get blocks 2)
              f (get handle c)]
          (if f
            (do
              (js/tint 255 transparency)
            (f (+ bx px) (+ by py) x y)
              (js/noTint)
              )
            (throw (js/Error (str "Draw handle not implemented for " c)))))))))
(defn draw-falling-blocks
  [blocks handle x y]
  (when blocks
    (doseq [block blocks]
      ;; (println (first block ))
      (draw-player block handle x y))))
(defn draw-piece-queue
  [queue handle x y]
  (when queue
    (doseq [[idx player] (map-indexed vector queue)]
      (draw-player player handle x (+ (* 130 idx) y)))))

(defn draw-sprite
  ([texture [ix iy iw ih] [dx dy dw dh]]
   (js/image texture (- dx (/ dw 2)) (- dy (/ dh 2)) dw dh ix iy iw ih))
  ([texture [sw sh gap] [gridx gridy] [x y w h]]
   (draw-sprite texture [(-> gridx (* (+ gap sw))) (-> gridy (* (+ gap sh))) sw sh] [x y w h])))

(defn draw-puyo [[sx sy] [x y] [ofx ofy] w h]
  (draw-sprite
   (->> (:textures @state) (:puyos))
   [18 17 1] [sx sy] [(+ ofx x) (+ ofy y) w h]))

(def pt-to-grid {:pt/red [0 3]
                 :pt/green [0 7]
                 :pt/blue [0 11]
                 :pt/yellow [0 15]
                 :pt/purple [0 19]})
(defn create-puyo-animation-particle "grid pos is a list of [sx sy]" [grid-pos color loop? lifetime]
  (let [[colorx colory] (color pt-to-grid)
        grid-pos (map (fn [[x y t]] [(+ colorx x) (+ colory y) (if t t 255)]) grid-pos)]
    (pcl/create-particle
     (fn [pc [x y] [ofx ofy] w h] (let [current-frame (nth grid-pos (:frame pc))]
                                    (js/tint 255 (nth current-frame 2))
                                    (draw-puyo current-frame [(* x w) (* y h)] [ofx ofy] w h)
                                    (js/noTint)))
     (fn [pc] (update pc :frame (if loop? l+ s+) 1 (:max-frames pc)))
     {:frame 0 :max-frames (dec (count grid-pos)) :death (+ (getTime) lifetime)})))

(defn =i [& terms]
  (if (apply = terms) 1 0))
(defn puyo-drawer-generator [sx sy color]
  (fn [x y ofx ofy [right left up down]]
    (let [color-string (str (=i color up) (=i color down) (=i color left) (=i color right))
          check-colors (fn [int] (= int color-string))
          [sx-offset sy-offset] (cond (check-colors "0000") [0 0]
                                      (check-colors "0001") [1 -3]
                                      (check-colors "0010") [3 -3]
                                      (check-colors "0011") [2 -3]
                                      (check-colors "0100") [0 -3]
                                      (check-colors "0101") [1 -2]
                                      (check-colors "0110") [3 -2]
                                      (check-colors "0111") [2 -2]
                                      (check-colors "1000") [0 -1]
                                      (check-colors "1001") [1 0]
                                      (check-colors "1010") [3 0]
                                      (check-colors "1011") [2 0]
                                      (check-colors "1100") [0 -2]
                                      (check-colors "1101") [1 -1]
                                      (check-colors "1110") [3 -1]
                                      (check-colors "1111") [2 -1]
                                      :else (throw (js/Error. (str "Invalid state:" color-string))))]
      ;; (println color-string)
      (draw-puyo [(+ sx-offset sx) (+ sy-offset sy)] [(* x 50) (* y 50)] [ofx ofy] 54 51))))
(def puyo-draw-handle
  {:pt/empty (fn [x y ofx ofy] (js/fill "yellow") (js/circle (+ ofx (* x 50)) (+ ofy (* y 50)) 10))
   :pt/red (puyo-drawer-generator 0 3 :pt/red)
   :pt/green (puyo-drawer-generator 0 7 :pt/green)
   :pt/blue (puyo-drawer-generator 0 11 :pt/blue)
   :pt/yellow (puyo-drawer-generator 0 15 :pt/yellow)
   :pt/purple (puyo-drawer-generator 0 19 :pt/purple)})

;; (pt/assertHandlesAllTypes puyo-draw-handle)

(defn fill-piece-queue "queue is a list" [queue]
  (if (< (count queue) 7)
    (recur (fill-piece-queue (conj queue
                                   (create-player 2 0 (rand-nth (rest pt/enum)) (rand-nth (rest pt/enum))))))
    queue))

(defn player_input-handle [p keys das board]
  (let [dt (fn [time] (- (getTime) time))
        justPressed (fn [time] (< (dt time) (+ 0 js/deltaTime)))
        checkTime (fn [time] (or (justPressed time) (> (dt time) das)))]
    (reduce (fn [p [key time]]
              ;; (println key)
              (cond
                (and (checkTime time) (= key "ArrowLeft") (js/keyIsDown 37)) (player_move-checked p player_move-left board)
                (and (checkTime time) (= key "ArrowRight") (js/keyIsDown 39)) (player_move-checked p player_move-right board)
                (and (checkTime time) (= key " ") (js/keyIsDown 32)) (assoc (player_move-down p 10) :player/groundTime 0)
                (and (justPressed time) (= key "x")) (player_rotate p board 1)
                (and (justPressed time) (= key "z")) (player_rotate p board 3)
                (and (js/keyIsDown 40) (= key "ArrowDown")) (assoc (player_move-down p (/ 1 5)) :player/groundTime 0)
                (js/keyIsDown 13) (create-player 2 0 (rand-nth (rest pt/enum)) (rand-nth (rest pt/enum)))
                (js/keyIsDown 49) {:blocks [(create-puyo 0 0 (rand-nth (rest pt/enum)))] :pos [2 0]}
                :else p)) p keys)))

(defn create-anim-hook [particle-f]
  (fn [globalstate blocks]
     (update globalstate :particles
             #(conj
               %
               (reduce
                (fn [acc block]
                  (reduce (fn [acc block2]
                            (let [[x y c] block2
                                  [x y] (+v [x y] (:pos block))]
                              (assoc acc [x y] (particle-f c)))) acc (:blocks block)))
                {} blocks))))
  )

(defn frame-extend "returns v v v v for (frame-extend v 4)"[& vecs]
  ;; (println vecs)
  (->>
   (for [[a b] (partition 2 1 (concat vecs '(1)))]
    (if  (vector? a)
      (if (number? b)
        (repeat b a)
        (list a))
      (list nil)))
    (reduce #(concat %1 %2))
    (filter #(not (= % nil)))
    (vec)))

(def hook-block-land
  [(create-anim-hook #(create-puyo-animation-particle [[6 0] [7 0]] % true 200))])
(def hook-block-pop
  [(create-anim-hook #(create-puyo-animation-particle
                       (frame-extend [0 0] [0 0 90] [0 0] [0 0 90] [0 0] [0 0 90] [4 0] 4 [7 -1] [8 -1]) % false 700))
   (fn [globalstate blocks]
     (update globalstate :chain inc))])


(defn run-hook [globalstate hook & args]
  (reduce #(apply %2 %1 args) globalstate hook))

(def state-enum [:s/player-fall :s/pop :s/dead])

(defn state-is [state enum]
  (= (:process state) enum))
(defn state-fall-blocks [globalstate state currentTime deltaTime minSpeed]
  (let [[board fallingblocks] (board_get-falling-puyos (:board globalstate))
        fallingblocks (apply conj (:falling-blocks globalstate) fallingblocks)
        fallingblocks (map (fn [block] (if (< (:yspeed block) minSpeed)
                                         (assoc block :yspeed minSpeed)
                                         block)) fallingblocks)
        fallingblocks (falling-blocks_move-down fallingblocks (* deltaTime 0.0007 ))

        [board fallingblocks placedblocks] (board_place-grounded-falling-blocks board fallingblocks)
        new-globalstate (-> globalstate
                            (run-hook hook-block-land placedblocks)
                            (assoc :board board)
                            (assoc :falling-blocks fallingblocks))]
    [new-globalstate (if (> (count fallingblocks) 0)
                       state
                       {:process :s/fall-fast :next-state [:s/pop currentTime 100]})]))
(defn state-update "state is {}, returns [globalstate state]" [globalstate state currentTime deltaTime]
  (if (contains? state :next-state)
    ;; change to next state
    (let [next-state (:next-state state)
          [next-state previousTime delta] next-state]
      (if (> (- currentTime previousTime) delta)
        [globalstate {:process next-state}]
        [globalstate state]))

    (cond
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
             (if (> (- currentTime (:player/groundTime placed-pos) ) 1000)
               [(-> globalstate
                    (run-hook hook-block-land (list placed-pos))
                    (assoc :board new-board)
                    (assoc :player nil))
                {:process :s/player-fall :next-state [:s/fall-slow currentTime 100]}]
               [(assoc globalstate :player placed-pos) state]
               )
             
            [(assoc globalstate :player nil) {:process :s/player-fall :next-state [:s/dead currentTime 0]}]))
        ;; player is not on ground so let player move down
        ;; if player doesn't land on the ground, remove ground time
        (let [player (-> (:player globalstate)
                         (player_move-down (* 0.0005 deltaTime))
                         ((fn [player] (if (player_grounded? player (:board globalstate))
                                        player
                                        (dissoc player :player/groundTime))))
                         )]
          [(assoc globalstate :player player) state]) )

      (state-is state :s/fall-slow)
      (state-fall-blocks globalstate state currentTime deltaTime 0)

      (state-is state :s/fall-fast)
      (state-fall-blocks globalstate state currentTime deltaTime 0.3)

      (state-is state :s/pop)
      (let [[new-board puyos-popped# _colors popped] (board_pop-puyos (:board globalstate))
            popped? (> puyos-popped# 0)]
        [(-> (assoc globalstate :board new-board) (run-hook hook-block-pop popped))
         {:process :s/pop :next-state (if popped? [:s/fall-fast currentTime 700] [:s/new-player currentTime 0])}]))))

(defn preload []
  (swap! state update :textures assoc :puyos (js/loadImage "original-puyos.png"))
  (swap! state update :fonts assoc :roboto (js/loadFont "fonts/Roboto-Regular.ttf")))

(defn setup []
  (let [canvas (js/createCanvas js/window.innerWidth js/window.innerHeight "webgl")
        ;; canvas (js/_renderer)
        texture (.getTexture canvas (:puyos (:textures @state)))]
    (.setInterpolation texture js/NEAREST js/NEAREST))
  ;; (js/noSmooth)
  (js/textFont (:roboto (:fonts @state)))
  (js/pixelDensity 1)
  (js/noStroke))

(defn draw []
  (swap! state update :player player_input-handle (:keys @state) (:das @state) (:board @state))
  ;; (println @state)
  ;; (println (:falling-blocks @state))
  ;; (println (:keys @state))
  (let [[new-state new-state-enum] (state-update @state (:state-enum @state) (getTime) js/deltaTime)]
    (reset! state new-state)
    (swap! state assoc :state-enum new-state-enum))

  (swap! state update :piece-queue fill-piece-queue)

  (when (> (- (getTime) (:timer/updateTime @timer)) (:timer/dt @timer))
    (swap! timer assoc :timer/updateTime (getTime))
    (let [particles (:particles @state)
          new-particles (reduce
                         (fn [acc [pos particle]]
                           (if (> (:death particle) (getTime))
                             (conj acc {pos ((:on-update particle) particle)})
                             acc))
                         {}
                         particles)]
      (swap! state assoc :particles new-particles)))

  (js/background "gray")
  (js/fill "yellow")
  ;; (let [offx js/window.innerWidth
  ;;       offy js/window.innerHeight
  ;;       offx (/ offx 3)
  ;;       offy (/ offy 5)]
  (let [offx -200
        offy -300]
    (draw-board (:board @state)
                puyo-draw-handle
                offx offy)
    (doseq [[[x y] particle] (:particles @state)]
      ;; (println (str [x y] particle))
      ((:on-draw particle) particle [x y] [offx offy] 50 50))
    (draw-player (:player @state)
                 puyo-draw-handle
                 offx offy)
    (js/color "yellow")
    (js/text  (str "chain " (:chain @state) "fps: " (Math/round (js/frameRate))) -300 0)
    (draw-falling-blocks (:falling-blocks @state)
                         puyo-draw-handle
                         offx offy)
    (draw-piece-queue (:piece-queue @state)
                      puyo-draw-handle
                      (+ offx 200) offy)))

(defn windowResized []
  (js/resizeCanvas js/window.innerWidth js/window.innerHeight))

(defn keyPressed []
  (swap! state update :keys assoc js/key (getTime)))

(defn keyReleased []
  (println (str "releasing " js/key))
  (swap! state update :keys dissoc js/key))

(doto js/window
  (g/set "preload" preload)
  (g/set "setup" setup)
  (g/set "draw" draw)
  (g/set "windowResized" windowResized)
  (g/set "keyPressed" keyPressed))
  ;; (g/set "keyReleased" keyReleased))
