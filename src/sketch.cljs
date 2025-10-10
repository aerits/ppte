(ns sketch
  (:require [goog.object :as g]
            [goog.date :as gdate]
            [PuyoTypes :as pt])
  #_{:clj-kondo/ignore [:unused-import]}
  (:import p5))

;; TODO switch to drawing on a texture
;; TODO animations
;; TODO add lock delay
;; TODO add rotation
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
(declare player_grounded? player_move-left player_move-right player_move-down)
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
              up-p (player_move-down new-player -0.8)]
          (cond
            (not (player_grounded? up-p board) ) up-p
            (not (player_grounded? left-p board)) left-p
            (not (player_grounded? right-p board) ) right-p
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
  (println blocks)
  (map #(falling-block_move-down % accel) blocks))
(defn s+ [x y max]
  (if (>= x max)
    max
    (+ x y)))
(defn s- [x y min]
  (if (<= x min)
    0
    (- x y)))
(defn player_move-left [p]
  (when p (update p :pos update 0 s- 1 0)))
(defn player_move-right [p]
  (when p (update p :pos update 0 s+ 1 5)))
(defn player_move-checked [p fn board]
  (let [new-player (fn p)]
    (if ( player_grounded? new-player board)
      p
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

(defn board_place-player [board p]
  (println p)
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
          board)))))
(defn board_place-grounded-falling-blocks "returns [board blocks]" [board blocks]
  (let [blocks (for [block blocks]
                 (if (player_grounded? block board)
                   [true block]
                   [nil block]))
        grounded-blocks (map (fn [[_ block]] block) (filter #(not (= nil (first %)))
                                                            blocks))
        new-blocks (map (fn [[_ block]] block) (filter #(= nil (first %)) blocks))
        new-board (reduce #(board_place-player %1 %2) board grounded-blocks)]
    [new-board new-blocks]))

;; (defn puyo-check-connected [board vec color already-counted]
;;   (if (or (= color (board-get-type board vec)) (contains? already-counted vec))

;;     (let [dirs [[0 -1] [0 1] [1 0] [-1 0]]]
;;       (apply + 1 (for [dir dirs]
;;                    (puyo-check-connected board (+v vec dir) color (conj already-counted vec)))))

;;     0))

(defn dfs [board [x y] color visited]
  (if (and (= color (board-get-type board [x y])) (not (contains? visited [x y])))
    (let [dirs [[-1 0] [1 0] [0 1] [0 -1]]
          neighbors (map #(+v % [x y]) dirs)
          neighbors (filter #(= color (board-get-type board %)) neighbors)]
      (reduce #(dfs board %2 color %1) (conj visited [x y]) neighbors))
    visited))

(defn board_pop-puyos "returns [board [num-puyos-popped colors]], colors is hashset" [board]
  (let [rows (count board)
        cols (count (get board 0))
        blocks-to-remove
        (for [y (range rows) x (range cols)]
          (when (not (= :pt/empty (board-get-type board [x y])))
            [x y (board-get-type board [x y]) (count (dfs board [x y] (board-get-type board [x y]) #{}))]))

        blocks-to-remove (filter #(not (= nil %)) blocks-to-remove)
        blocks-to-remove (filter #(>= (last %) 4) blocks-to-remove)
        blocks-to-remove (map (fn [[x y c _]] [x y c]) blocks-to-remove)]
    (reduce
     (fn [[ board puyos-popped# colors] [x y c]] [(assoc-in board [y x] [:pt/empty]) (inc puyos-popped#) (conj colors c)])
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
                 :chain 0
                 :das 131 ;; ms
                 :textures {}
                 :fonts {}
                 :keys {}
                 :falling-blocks []}))
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
      (let [el (board-get-type v [col row])
            ;; right left up down
            dirs '([1 0] [-1 0] [0 -1] [0 1])
            surrounding (map (fn [dirvec] (board-get-type v (+v dirvec [col row]))) dirs)
            f (get handle el)]
        (if f
          ;; (when (= el :pt/red)
          ;; (println surrounding)
          ;;   )
          (f col row x y surrounding)
          (throw (js/Error (str "Draw handle not implemented for " el))))))))

(defn draw-player
  [p handle x y]
  (when p
    (let [px (get (:pos p) 0)
          py (get (:pos p) 1)]
      (doseq [blocks (:blocks p)]
        (let [bx (get blocks 0)
              by (get blocks 1)
              c (get blocks 2)
              f (get handle c)]
          (if f
            (f (+ bx px) (+ by py) x y)
            (throw (js/Error (str "Draw handle not implemented for " c)))))))))
(defn draw-falling-blocks
  [blocks handle x y]
  (when blocks
    (doseq [block blocks]
      ;; (println (first block ))
      (draw-player block handle x y))))

(defn draw-sprite
  ([texture [ix iy iw ih] [dx dy dw dh]]
   (js/image texture (- dx (/ dw 2)) (- dy (/ dh 2)) dw dh ix iy iw ih))
  ([texture [sw sh gap] [gridx gridy] [x y w h]]
   (draw-sprite texture [(-> gridx (* (+ gap sw))) (-> gridy (* (+ gap sh))) sw sh] [x y w h])))

(defn draw-puyo [[sx sy] [x y] [ofx ofy] w h]
  (draw-sprite
   (->> (:textures @state) (:puyos))
   [18 17 1] [sx sy] [(+ ofx x) (+ ofy y) w h]))

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

(defn player_input-handle [p keys das board]
  (let [dt (fn [time] (- (getTime) time))
        justPressed (fn [time] (< (dt time) (+ 0 js/deltaTime)))
        checkTime (fn [time] (or (justPressed time) (> (dt time) das)))]
    (reduce (fn [p [key time]]
              ;; (println key)
              (cond
                (and (checkTime time) (= key "ArrowLeft") (js/keyIsDown 37)) (player_move-checked p player_move-left board)
                (and (checkTime time) (= key "ArrowRight") (js/keyIsDown 39)) (player_move-checked p player_move-right board)
                (and (checkTime time) (= key " ") (js/keyIsDown 32)) (player_move-down p 10)
                (and (justPressed time) (= key "f")) (player_rotate p board 1)
                (and (justPressed time) (= key "d")) (player_rotate p board 3)
                (and (js/keyIsDown 40) (= key "ArrowDown")) (player_move-down p (/ 1 5))
                (js/keyIsDown 13) (create-player 2 0 (rand-nth (rest pt/enum)) (rand-nth (rest pt/enum)))
                (js/keyIsDown 49) {:blocks [(create-puyo 0 0 (rand-nth (rest pt/enum)))] :pos [2 0]}
                :else p)) p keys)))
    ;; (cond
    ;;   (and (checkTime (get keys "ArrowLeft")) (js/keyIsDown 37)) (player_move-left p)
    ;;   (and (checkTime (get keys "ArrowRight")) (js/keyIsDown 39)) (player_move-right p)
    ;;   (js/keyIsDown 40) (player_move-down p 0.5)
    ;;   (js/keyIsDown 32) (println "you pressed space")
    ;;   (js/keyIsDown 13) (create-player 2 0 (rand-nth (rest pt/enum )) (rand-nth (rest pt/enum )))
    ;;   :else p)))

(defn preload []
  (swap! state update :textures assoc :puyos (js/loadImage "original-puyos.png"))
  (swap! state update :fonts assoc :roboto (js/loadFont "fonts/Roboto-Regular.ttf")))

(defn setup []
  (let [canvas (js/createCanvas js/window.innerWidth js/window.innerHeight "webgl")
        canvas (js/_renderer)
        texture (.getTexture canvas (:puyos (:textures @state)))]
    (.setInterpolation texture js/NEAREST js/NEAREST))
  (js/noSmooth)
  (js/textFont (:roboto (:fonts @state)))
  (js/pixelDensity 1)
  (js/noStroke))

(defn draw []
  (swap! state update :player player_input-handle (:keys @state) (:das @state) (:board @state))
  ;; (println @state)
  ;; (println (:falling-blocks @state))
  ;; (println (:keys @state))
  (if (player_grounded? (:player @state) (:board @state))
    (let [new-board (board_place-player (:board @state) (:player @state))]
      (if new-board
        (do (swap! state assoc :board new-board)
            (swap! state assoc :player nil))
        (do (swap! state assoc :player nil)
            (println "you died"))))
    (swap! state update :player player_move-down (* 0.0005 js/deltaTime)))
  (when (not (:player @state))
    (let [[new-board puyos-popped# _] (board_pop-puyos (:board @state))]
      (swap! state assoc :board new-board)
      (when (> puyos-popped# 0)
        (swap! state update :chain inc)
        )
      )
    (let [[board blocks] (board_get-falling-puyos (:board @state))]
      (when (seq blocks)
        (swap! state assoc :board board)
        (println blocks)
        (swap! state assoc :falling-blocks (apply conj (:falling-blocks @state) blocks)))))

  (if (> (count (:falling-blocks @state)) 0)
    (do (swap! state update :falling-blocks falling-blocks_move-down 0.01)
        (let [[board blocks] (board_place-grounded-falling-blocks (:board @state) (:falling-blocks @state))]
          (swap! state assoc :board board)
          (swap! state assoc :falling-blocks blocks)))
    (when (not (:player @state))
      (swap! state assoc :chain 0)
      (swap! state assoc :player (create-player 2 0 (rand-nth (rest pt/enum)) (rand-nth (rest pt/enum))))))

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
    (draw-player (:player @state)
                 puyo-draw-handle
                 offx offy)
(js/color "yellow")
    (js/text  (str "chain " (:chain @state) ) -300 0)
    (draw-falling-blocks (:falling-blocks @state)
                         puyo-draw-handle
                         offx offy)))

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
