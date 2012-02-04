(ns monet.canvas
  (:require [monet.core :as core]))

;;*********************************************
;; Canvas drawing functions
;;*********************************************

(defn get-context [canvas type]
  (. canvas (getContext (name type))))

(defn begin-path [ctx]
  (. ctx (beginPath))
  ctx)

(defn close-path [ctx]
  (. ctx (closePath))
  ctx)

(defn fill [ctx]
  (. ctx (fill))
  ctx)

(defn stroke [ctx]
  (. ctx (stroke))
  ctx)

(defn clear-rect [ctx {:keys [x y w h]}]
  (. ctx (clearRect x y w h))
  ctx)

(defn rect [ctx {:keys [x y w h]}]
  (begin-path ctx)
  (. ctx (rect x y w h))
  (close-path ctx)
  (fill ctx)
  ctx)

(defn circle [ctx {:keys [x y r]}]
  (begin-path ctx)
  (. ctx (arc x y r 0 (* (.-PI js/Math) 2) true))
  (close-path ctx)
  (fill ctx)
  ctx)

(defn text [ctx {:keys [text x y]}]
  (. ctx (fillText text x y)))

(defn fill-style [ctx color]
  (set! ctx.fillStyle color)
  ctx)

(defn stroke-style [ctx color]
  (set! ctx.strokeStyle color)
  ctx)

(defn stroke-width [ctx w]
  (set! ctx.lineWidth w)
  ctx)

(defn move-to [ctx x y]
  (. ctx (moveTo x y))
  ctx)

(defn line-to [ctx x y]
  (. ctx (lineTo x y))
  ctx)

(defn alpha [ctx a]
  (set! ctx.globalAlpha a)
  ctx)

(defn save [ctx]
  (. ctx (save))
  ctx)

(defn restore [ctx]
  (. ctx (restore))
  ctx)

;;*********************************************
;; Canvas Entities
;;*********************************************

(def entities (js-obj))
(def active (atom true))

(defn add-entity [k ent]
  (aset entities k ent))

(defn remove-entity [k]
  (js-delete entities k))

(defn get-entity [k]
  (:value (aget entities k)))

(defn update-entity [k func & extra]
  (let [cur (aget entities k)
        res (apply func cur extra)]
    (aset entities k res)))

(defn entity [v update draw]
  {:value v
   :draw draw
   :update update})

(defn update-loop []
  (when @active
    (let [ks (js-keys entities)
          cnt (alength ks)]
      (loop [i 0]
        (when (< i cnt)
          (let [k (aget ks i)
                {:keys [update value] :as ent} (aget entities k)]
            (when update
              (let [updated (update value)]
                (when (aget entities k)
                  (aset entities k (assoc ent :value updated)))))
            (recur (inc i)))))
      (js/setTimeout update-loop 10))))

(defn draw-loop [ctx width height]
  (clear-rect ctx {:x 0 :y 0 :w width :h height})
  (when @active
    (let [ks (js-keys entities)
          cnt (alength ks)]
      (loop [i 0]
        (when (< i cnt)
          (let [k (aget ks i)
                {:keys [draw value] :as ent} (aget entities k)]
            (when draw 
              (draw ctx value)) 
            (recur (inc i))))))
    (core/animation-frame #(draw-loop ctx width height))))

(defn init [canvas & [context-type]]
  (let [ct (or context-type "2d")
        width (.getAttribute canvas "width")
        height (.getAttribute canvas "height")
        ctx (get-context canvas ct)]
    (update-loop)
    (draw-loop ctx width height)))

(defn stop [] (reset! active false))
(defn start [] (reset! active true))
