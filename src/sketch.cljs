(ns sketch
  (:require [goog.object :as g]
            [PuyoTypes :as pt]
            [Puyo :as p]
            [Particle :as pcl]
            [clojure.string :as str]
            [clojure.core.async :as a])
  #_{:clj-kondo/ignore [:unused-import]}
  (:import p5)
  (:require-macros [Macros :as m]))

;; TODO switch to drawing on a texture
;; TODO refactor into multiple files
;; TODO add lock delay
;; TODO add a menu

(defn getTime []
  (js/performance.now))

(defn request-to-keywords [req]
  (when req
    (into {} (for [[_ k v] (re-seq #"([^&=]+)=([^&]+)" req)]
               [(keyword k) v]))))
(defn keyword-map []
  (-> (str/split js/window.location.href #"\?")
      (second)
      (request-to-keywords)))

(defmacro with [f-begin f-end & body]
  (f-begin)
  `(~@body)
  (f-end))

(defonce state (atom
                (p/create-globalstate #queue [])))
(defonce resources
  (atom {:textures {}
         :fonts {}}))

(def timer (atom {:timer/lastUpdate (getTime) :timer/dt 50}))
;; (def fb (js/createFramebuffer))
(defn draw-board
  "
  draw board v
  each element is drawn using the map handle {key function}
  each key in the map corresponds to an element in v
  "
  [v handle x y]
  (dotimes [row (count v)] ;; 2 hidden rows
    (dotimes [col (count (get v 0))]
      (when (not (contains? (:particles @state) [col row]))
        (let [el (p/board-get-type v [col row])
            ;; right left up down
              dirs '([1 0] [-1 0] [0 -1] [0 1])
              surrounding (map (fn [dirvec]
                                 (let [dir (p/+v dirvec [col row])]
                                   (if (not (contains? (:particles @state) dir))
                                     (p/board-get-type v dir)
                                     (p/board-get-type v [200 200])))) dirs)
              f (get handle el)]
          (if f
          ;; (when (= el :pt/red)
          ;; (println surrounding)
          ;;   )
            (f col row x y surrounding)
            (println (str "Draw handle not implemented for " el))))))))

(defn draw-player
  ([p handle x y opacity]
   (when p
     (let [px (get (:pos p) 0)
           py (get (:pos p) 1)]
       (doseq [blocks (:blocks p)]
         (let [bx (get blocks 0)
               by (get blocks 1)
               c (get blocks 2)
               f (get handle c)]
           (if f
             (do
               (js/tint 255 opacity)
               (f (+ bx px) (+ by py) x y)
               (js/noTint))
             (throw (js/Error (str "Draw handle not implemented for " c)))))))))
  ([p handle x y]
   (let [transparency (-> (- (getTime) (:player/groundTime p)) (/ 1000) (- 1) (* -1) (* 255))
         transparency (if (:player/groundTime p) transparency 255)]
     (draw-player p handle x y transparency))))
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
   (->> (:textures @resources) (:puyos))
   [18 17 1] [sx sy] [(+ ofx x) (+ ofy y) w h]))

(def pt-to-grid {:pt/red [0 3]
                 :pt/green [0 7]
                 :pt/blue [0 11]
                 :pt/yellow [0 15]
                 :pt/purple [0 19]
                 :pt/garbage [23 0]})
(defn create-puyo-animation-particle2
  "
  (grid-pos-fn :pt/color -> (list [sx sy]))
  lifetime is amount of ms after it should die
  "
  [grid-pos-fn color loop? lifetime]
  (let [[colorx colory] (color pt-to-grid)
        grid-pos (grid-pos-fn color)
        grid-pos (map (fn [[x y t]] [(+ colorx x) (+ colory y) (if t t 255)]) grid-pos)]
    (pcl/create-particle
     (fn [pc [x y] [ofx ofy] w h] (let [current-frame (nth grid-pos (:frame pc))]
                                    (js/tint 255 (nth current-frame 2))
                                    (draw-puyo current-frame [(* x w) (* y h)] [ofx ofy] w h)
                                    (js/noTint)))
     (fn [pc] (update pc :frame (if loop? p/l+ p/s+) 1 (:max-frames pc)))
     {:frame 0 :max-frames (dec (count grid-pos)) :death (+ (getTime) lifetime)})))

(defn create-text-particle "grid pos is a list of [sx sy]" [draw-text [posx posy] fn-p5-style lifetime]
  (pcl/create-particle
   (fn [pc [x y] [ofx ofy] w h]
     (fn-p5-style
      (fn [] (js/text draw-text (+ ofx (* posx w)) (+ ofy (* posy h))))))
   (fn [pc] pc)
   {:death (+ (getTime) lifetime)}))

(defn =i [& terms]
  (if (apply = terms) 1 0))

(defn puyo-drawer-variants [check-colors color-string]
  (cond (check-colors "0000") [0 0]
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
        :else (throw (js/Error. (str "Invalid state:" color-string)))))

(defn puyo-drawer-generator [sx sy color variants-fn]
  (fn [x y ofx ofy [right left up down]]
    (let [color-string (str (=i color up) (=i color down) (=i color left) (=i color right))
          check-colors (fn [int] (= int color-string))
          [sx-offset sy-offset] (variants-fn check-colors color-string)]
      ;; (println color-string)
      (draw-puyo [(+ sx-offset sx) (+ sy-offset sy)] [(* x 50) (* y 50)] [ofx ofy] 54 51))))
(def puyo-draw-handle
  {:pt/empty (fn [x y ofx ofy] (js/fill "yellow") (js/circle (+ ofx (* x 50)) (+ ofy (* y 50)) 10))
   :pt/red (puyo-drawer-generator 0 3 :pt/red puyo-drawer-variants)
   :pt/green (puyo-drawer-generator 0 7 :pt/green puyo-drawer-variants)
   :pt/blue (puyo-drawer-generator 0 11 :pt/blue puyo-drawer-variants)
   :pt/yellow (puyo-drawer-generator 0 15 :pt/yellow puyo-drawer-variants)
   :pt/purple (puyo-drawer-generator 0 19 :pt/purple puyo-drawer-variants)
   :pt/garbage (puyo-drawer-generator 23 0 :pt/garbage (fn [_ _] [0 0]))})

;; (pt/assertHandlesAllTypes puyo-draw-handle)

(defn player_input-handle [p keys das board]
  (let [dt (fn [time] (- (getTime) time))
        justPressed (fn [time] (< (dt time) (+ 0 js/deltaTime)))
        checkTime (fn [time] (or (justPressed time) (> (dt time) das)))]
    (reduce (fn [p [key time]]
              ;; (println key)
              (cond
                (and (checkTime time) (= key "ArrowLeft") (js/keyIsDown 37)) (p/player_move-checked p p/player_move-left board)
                (and (checkTime time) (= key "ArrowRight") (js/keyIsDown 39)) (p/player_move-checked p p/player_move-right board)
                (and (checkTime time) (= key " ") (js/keyIsDown 32)) (assoc (p/player_move-down p 40) :player/groundTime -1000)
                (and (justPressed time) (= key "x")) (p/player_rotate p board 1)
                (and (justPressed time) (= key "z")) (p/player_rotate p board 3)
                (and (js/keyIsDown 40) (= key "ArrowDown")) (-> (p/player_move-down p (/ 1 5))
                                                                ((fn [p] (assoc p :player/groundTime
                                                                                (if (p/player_grounded? p board) -1000 nil)))))
                (js/keyIsDown 13) (p/create-player 2 0 (rand-nth pt/constructable) (rand-nth pt/constructable))
                (js/keyIsDown 49) {:blocks [(p/create-puyo 0 0 (rand-nth pt/enum))] :pos [2 0]}
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
                                 [x y] (p/+v [x y] (:pos block))]
                             (assoc acc [x y] (particle-f c))))
                         acc (:blocks block)))
               {} blocks)))))

(defn frame-extend "returns v v v v for (frame-extend v 4)" [& vecs]
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
  [(create-anim-hook #(create-puyo-animation-particle2
                       (fn [c]
                         (if (not= c :pt/garbage)
                           [[6 0] [7 0]]
                           [[0 0]]))
                       % true 200))])
(def hook-block-pop
  [(create-anim-hook #(create-puyo-animation-particle2
                       (fn [c]
                         (if (not= c :pt/garbage)
                           (frame-extend [0 0] [0 0 90] [0 0] [0 0 90] [0 0] [0 0 90] [4 0] 4 [7 -1] [8 -1])
                           [[0 0]]))
                       % false 700))
   (fn [globalstate blocks]
     (update globalstate :chain (if (> (count blocks) 0) inc identity)))
   p/score_chain-score-update
   (fn [globalstate blocks]
     (print "chain score:" (:chain-score globalstate))
     globalstate)
   (fn [globalstate blocks]
     (if blocks
       (let [[minx miny] (reduce
                          (fn [[minx miny] block]
                            (let [[x y] (:pos block)]
                              [(if (< x minx) x minx) (if (< y miny) y miny)]))
                          [999 999]
                          blocks)
             particle (create-text-particle
                       (str (:chain globalstate) "chain")
                       [(- minx 1) (- miny 1)]
                       (fn [fn-draw]
                         (js/fill "white")
                         (js/textSize 22)
                         (js/stroke "black")
                         (js/strokeWeight 10)
                         (fn-draw))
                       1000)]
         (assoc-in globalstate [:particles [-1 -1]] particle))
       globalstate))])

(def hooks {:hook-block-land hook-block-land
            :hook-block-pop hook-block-pop})

(defn preload []
  (swap! resources update :textures assoc :puyos (js/loadImage "original-puyos.png"))
  (swap! resources update :fonts assoc :roboto (js/loadFont "fonts/Roboto-Regular.ttf")))

(defn setup []
  (let [canvas (js/createCanvas js/window.innerWidth js/window.innerHeight "webgl")
        ;; canvas (js/_renderer)
        texture (.getTexture canvas (:puyos (:textures @resources)))]
    (.setInterpolation texture js/NEAREST js/NEAREST))
  ;; (js/noSmooth)
  (js/textFont (:roboto (:fonts @resources)))
  (js/pixelDensity 1)
  (js/noStroke))

(defn draw []
  ;; (println @state)
  ;; (println (:falling-blocks @state))
  ;; (println (:keys @state))
  ;; (println (:chain @state))
  (let [[new-state new-state-enum] (p/state-update @state (:state-enum @state) hooks (getTime) js/deltaTime)]
    (reset! state new-state)
    (swap! state assoc :state-enum new-state-enum))

  ;; (swap! state update :piece-queue p/fill-piece-queue 2)

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

  (swap! state update :player player_input-handle (:keys @state) (:das @state) (:board @state))

  ;; (println (:particles @state))

  (js/background "gray")
  (js/fill "yellow")
  ;; (let [offx js/window.innerWidth
  ;;       offy js/window.innerHeight
  ;;       offx (/ offx 3)
  ;;       offy (/ offy 5)]
  (let [offx -200
        offy -400]
    (draw-board (:board @state)
                puyo-draw-handle
                offx offy)
    (doseq [[[x y] particle] (:particles @state)]
      ;; (println (str [x y] particle))
      ((:on-draw particle) particle [x y] [offx offy] 50 50))

    ;; draw ghost piece
    (draw-player (last (p/board_place-player (:board @state) (p/player_move-down (:player @state) 20)))
                 puyo-draw-handle
                 offx offy
                 80)
    ;; draw actual player
    (draw-player (:player @state)
                 puyo-draw-handle
                 offx offy)

    ;; cover up hidden rows
    (js/stroke "gray")
    (js/fill "gray")
    (js/rect (- offx 25) (- offy 25) 300 100)
    (js/noStroke)

    ;; debug info
    (js/fill "yellow")
    (js/textSize 22)
    (js/text  (str "chain " (:chain @state) "fps: " (Math/round (js/frameRate))) -400 0)

    ;; misc
    (draw-falling-blocks (:falling-blocks @state)
                         puyo-draw-handle
                         offx offy)
    (draw-piece-queue (:piece-queue @state)
                      puyo-draw-handle
                      (+ offx 200) offy)
    (when-not (.hasFocus js/document)
      (println "no-focus")
      (js/fill "white")
      (js/textSize 40)
      (js/text "click to focus window" -200 0))))

(defn windowResized []
  (js/resizeCanvas js/window.innerWidth js/window.innerHeight))

(defn keyPressed []
  (swap! state update :keys assoc js/key (getTime)))

(defn keyReleased []
  (println (str "releasing " js/key))
  (swap! state update :keys dissoc js/key))

(defonce main-lock (atom 0))

(defn main-update []
  (let [iframe (js/document.getElementById "frame1")]
    (.contentWindow.postMessage iframe "bruh" js/window.location.origin)))

(defn main []
  (when (= @main-lock 0)
    (a/go-loop []
      (a/<! (a/timeout 1000))
      (main-update)
      (recur))))

(when (and (= @main-lock 0) (:play (keyword-map)))
  (js/window.addEventListener
   "message"
   (fn [message]
     (print "RECIEVED MESSAGE" message.data))))

(if (:play (keyword-map))
  (doto js/window
    (g/set "preload" preload)
    (g/set "setup" setup)
    (g/set "draw" draw)
    (g/set "windowResized" windowResized)
    (g/set "keyPressed" keyPressed))
  (main))
  ;; (g/set "keyReleased" keyReleased))

(swap! main-lock inc)
