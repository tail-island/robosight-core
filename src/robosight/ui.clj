(ns robosight.ui
  (:require (robosight [core :as robosight]))
  (:import  (javafx.geometry    VPos)
            (javafx.scene.paint Color)))

(def ^:private stroke-paints
  [(Color/rgb 199  36  58)
   (Color/rgb   0 134 171)
   (Color/rgb 116  65 153)])

(def ^:private fill-paints
  [(Color/rgb 236 172 181)
   (Color/rgb 201 232 241)
   (Color/rgb 229 215 238)])

(defn- x->javafx
  [x]
  (+ x (/ robosight/field-size-x 2)))

(defn- y->javafx
  [y]
  (+ (- y) (/ robosight/field-size-y 2)))

(defn- angle->javafx
  [angle]
  (* (/ (Math/atan2 (Math/sin (- angle)) (Math/cos (- angle))) Math/PI) 180))

(defn- draw-tank
  [graphics-context object team]
  (let [[center-x center-y radius direction broken?] (flatten ((juxt :center robosight/radius :direction robosight/broken?) object))
        paint-index (cond-> team
                      broken? ((constantly 2)))]
    (doto graphics-context
      (.setFill    (fill-paints paint-index))
      (.fillOval   (- (x->javafx center-x) radius)
                   (- (y->javafx center-y) radius)
                   (* radius 2)
                   (* radius 2))
      (.setStroke  (stroke-paints paint-index))
      (.strokeOval (- (x->javafx center-x) radius)
                   (- (y->javafx center-y) radius)
                   (* radius 2)
                   (* radius 2))
      (.strokeLine (+ (x->javafx center-x) (* (Math/cos direction) (* radius 0.5)))
                   (- (y->javafx center-y) (* (Math/sin direction) (* radius 0.5)))
                   (+ (x->javafx center-x) (* (Math/cos direction) radius))
                   (- (y->javafx center-y) (* (Math/sin direction) radius))))))

(defn- draw-tank-info
  [graphics-context object team]
  (let [[center-x center-y radius hp name broken?] (flatten ((juxt :center robosight/radius :hp :name robosight/broken?) object))
        paint-index (cond-> team
                      broken? ((constantly 2)))]
    (doto graphics-context
      (.setStroke       (stroke-paints paint-index))
      (.setTextBaseline VPos/TOP)
      (.strokeText      name
                        (+ (x->javafx center-x) radius 2)
                        (- (y->javafx center-y) radius))
      (.setTextBaseline VPos/BOTTOM)
      (.strokeText      (if broken?
                          "-"
                          (format "%.1f" hp))
                        (+ (x->javafx center-x) radius 2)
                        (+ (y->javafx center-y) radius)))))

(defn- draw-shell
  [graphics-context object]
  (let [[center-x center-y radius] (flatten ((juxt :center robosight/radius) object))]
    (doto graphics-context
      (.setFill  Color/BLACK)
      (.fillOval (- (x->javafx center-x) radius)
                 (- (y->javafx center-y) radius)
                 (* radius 2)
                 (* radius 2)))))

(defn draw
  [graphics-context objects]
  (doto graphics-context
    (.clearRect 0 0 robosight/field-size-x robosight/field-size-y))
  (doseq [[object team] (map vector (take 10 objects) (concat (repeat 5 0) (repeat 5 1)))]
    (draw-tank graphics-context object team))
  (doseq [[object team] (map vector (take 10 objects) (concat (repeat 5 0) (repeat 5 1)))]
    (draw-tank-info graphics-context object team))
  (doseq [object (drop 10 objects)]
    (draw-shell graphics-context object)))
