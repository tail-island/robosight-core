(ns robosight.core
  (:require (clojure.core [matrix        :as matrix])
            (clojure.data [json          :as json])
            (clojure.math [combinatorics :as combinatorics]))
  (:import  (java.util.concurrent TimeoutException TimeUnit)))

;; Comparing floating points utilities...

(def ^:private epsilon
  (Math/pow 10 -6))

(defn f=
  [x y]
  (let [d (Math/abs (- x y))]
    (or (<= d epsilon) (<= d (* (max (Math/abs x) (Math/abs y)) epsilon)))))

(def fnot=
  (complement f=))

(defn f<
  [x y]
  (and (< x y) (fnot= x y)))

(defn f<=
  [x y]
  (or (< x y) (f= x y)))

(def f>
  (complement f<=))

(def f>=
  (complement f<))

;; Constants.

(def max-turn
  300)

(def field-size-x
  640.0)

(def field-size-y
  480.0)

(def field-size
  [field-size-x field-size-y])

(def tank-radius
  20.0)

(def tank-density
  1.0)

(def shell-radius
  2.0)

(def shell-density
  5.0)

(def coefficient-of-restitution
  0.8)

(def damage-ratio
  0.1)

(def initial-hp
  1000.0)

(def shoot-interval
  20)

(def tank-impact-speed-min
  0.5)

(def tank-impact-speed-max
  1.0)

(def shell-impact-speed-min
  5.0)

(def shell-impact-speed-max
  10.0)

(def thinking-time
  500)

;; Data structure.

(def initial-state
  {:turn    0
   :objects [{:type :tank :center [-160.0  180.0] :direction (* Math/PI 0.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "a"}
             {:type :tank :center [-160.0  100.0] :direction (* Math/PI 0.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "b"}
             {:type :tank :center [-160.0   20.0] :direction (* Math/PI 0.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "c"}
             {:type :tank :center [-160.0  -60.0] :direction (* Math/PI 0.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "d"}
             {:type :tank :center [-160.0 -140.0] :direction (* Math/PI 0.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "e"}
             {:type :tank :center [ 160.0 -180.0] :direction (* Math/PI 1.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "v"}
             {:type :tank :center [ 160.0 -100.0] :direction (* Math/PI 1.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "w"}
             {:type :tank :center [ 160.0  -20.0] :direction (* Math/PI 1.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "x"}
             {:type :tank :center [ 160.0   60.0] :direction (* Math/PI 1.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "y"}
             {:type :tank :center [ 160.0  140.0] :direction (* Math/PI 1.0) :velocity [0.0 0.0] :hp initial-hp :can-shoot-after 0 :name "z"}]})

;; Utilities.

(defn radius
  [object]
  ({:tank tank-radius :shell shell-radius}
   (:type object)))

(defn density
  [object]
  ({:tank tank-density :shell shell-density}
   (:type object)))

(defn mass
  [object]
  (* Math/PI (Math/pow (radius object) 2) (density object)))

(defn kinetic-energy
  [object]
  (if object
    (/ (* (mass object) (Math/pow (matrix/length (:velocity object)) 2)) 2)
    0.0))

(defn tank?
  [object]
  (= (:type object) :tank))

(defn shell?
  [object]
  (= (:type object) :shell))

(defn broken?
  [object]
  (f<= (:hp object) 0.0))

(defn tanks-coll
  [state]
  (->> (:objects state)
       (partition 5)
       (take 2)))

(defn game-finished?
  [state]
  (or (> (:turn state) max-turn)
      (some #(every? broken? %) (tanks-coll state))))

(defn winner
  [state]
  (let [survivors-coll  (map #(remove broken? %) (tanks-coll state))
        survivor-counts (map count survivors-coll)]
    (cond
      (apply > survivor-counts) 0
      (apply < survivor-counts) 1
      :else                     (let [survivor-hp-totals (map #(reduce + (map :hp %)) survivors-coll)]
                                  (cond
                                    (apply f> survivor-hp-totals) 0
                                    (apply f< survivor-hp-totals) 1)))))

;; Action.

(defn- rotation
  [direction]
  [[(Math/cos direction) (Math/sin direction)] [(- (Math/sin direction)) (Math/cos direction)]])

(defn- forward
  [state object-index impact-speed]
  (update-in state [:objects object-index :velocity] #(matrix/add % (matrix/mmul [(max (min impact-speed tank-impact-speed-max) tank-impact-speed-min) 0.0]
                                                                                 (rotation (get-in state [:objects object-index :direction]))))))

(defn- turn-to
  [state object-index direction]
  (assoc-in state [:objects object-index :direction] direction))

(defn- shoot
  [state object-index impact-speed]
  (let [[center velocity direction radius can-shoot-after] ((juxt :center :velocity :direction radius :can-shoot-after) (get-in state [:objects object-index]))
        rotation      (rotation direction)
        shell-center  (matrix/add center (matrix/mmul [(+ radius shell-radius 0.1) 0.0] rotation))
        enough-space? (every? #(f> (matrix/length (matrix/sub shell-center (:center %))) (+ tank-radius shell-radius)) (:objects state))]
    (cond-> state
      (and (<= can-shoot-after 0) enough-space?) (-> (assoc-in  [:objects object-index :can-shoot-after] shoot-interval)
                                                     (update-in [:objects] #(conj % {:type     :shell
                                                                                     :center   shell-center
                                                                                     :velocity (matrix/add velocity (matrix/mmul [(max (min impact-speed shell-impact-speed-max) shell-impact-speed-min) 0.0]
                                                                                                                                 rotation))}))))))

(defn- action-fn
  [ins outs]
  (fn [state]
    (->> (map (fn [team in out friends-and-enemies]
                (doto in
                  (.println (json/write-str friends-and-enemies))
                  (.flush))
                (->> (let [read-line (future (.readLine out))]
                       (try
                         (.get read-line
                               (if (<= (:turn state) 1)
                                 10000  ; 初回はいろいろ時間がかかると思うので（Clojureで作ったサンプルの起動がすげー遅かった……）、制限時間は10秒にします。
                                 thinking-time)
                               TimeUnit/MILLISECONDS)
                         (catch TimeoutException ex
                           (.cancel read-line true)
                           (throw (ex-info "Timeout error." {:reason :timeout :timeout-team team})))))
                     (#(json/read-str % :key-fn keyword))
                     (map (fn [{:keys [function parameter]}]
                            (if function
                              (if-let [function (ns-resolve (symbol "robosight.core") (symbol function))]
                                (if (number? parameter)
                                  [function parameter])))))
                     (#(take 5 (concat % (repeat nil))))))
              (range 2)
              ins
              outs
              ((juxt identity reverse) (tanks-coll state)))
         (apply concat)
         (map-indexed cons)
         (reduce (fn [state [object-index function parameter]]
                   (cond-> state
                     (and ((complement broken?) (get-in state [:objects object-index])) function) (function object-index parameter)))
                 state))))

;; Uniform linear motion.

(defn- uniform-linear-motion'
  [objects now-time]
  (letfn [(linear-motion [objects duration]
            (->> objects
                 (keep-indexed (fn [index object]
                                 (if object
                                   [index (assoc object :center (matrix/add (:center object) (matrix/mul (:velocity object) duration)))])))
                 (reduce #(apply assoc %1 %2) objects)))
          (solve-quadratic-equation [a b c]
            (let [d (- (Math/pow b 2) (* 4 a c))]
              (if (>= d 0)
                ;; 二次方程式の階の公式そのままより、以下のほうが浮動小数点の誤差が少ないらしい。。。
                (let [op (if (>= b 0)
                           -
                           +)
                      x1 (/ (op (- b) (Math/sqrt d)) (* 2 a))
                      x2 (if (= x1 0)
                           x1
                           (/ c a x1))]
                  [x1 x2]))))
          (bounce-off-object-time' [object other]
            (if (and object other)
              (let [v0 (matrix/sub (:center other) (:center object))
                    v1 (matrix/sub (matrix/add (:center other) (:velocity other)) (matrix/add (:center object) (:velocity object)))]
                (if-not (matrix/equals v0 v1 epsilon)
                  (->> (solve-quadratic-equation (matrix/length-squared (matrix/sub v1 v0))
                                                 (* (matrix/dot v0 (matrix/sub v1 v0)) 2)
                                                 (- (matrix/length-squared v0)
                                                    (Math/pow (+ (radius other) (radius object)) 2)))
                       (filter #(f< 0.0 %))
                       (sort)
                       (first))))))
          (bounce-off-object-time [object other]
            (some->> (bounce-off-object-time' object other)
                     (#(if (f<= (+ now-time %) 1.0)
                         %))))
          (bounce-off-object-times [objects]
            (->> objects
                 (map-indexed vector)
                 (#(combinatorics/combinations % 2))  ; オブジェクトの数が少ないから、ナイーブな実装で大丈夫なはず。。。
                 (keep (fn [index-and-object-pair]
                         (if-let [bounce-time (apply bounce-off-object-time (map second index-and-object-pair))]
                           [bounce-time (map first index-and-object-pair)])))
                 (#(if-let [first-bounce-time (first (sort (map first %)))]
                     [first-bounce-time (keep (fn [[bounce-time index-pair]]
                                                (if (f= bounce-time first-bounce-time)
                                                  index-pair))
                                              %)]))))
          (bounce-off-object' [impacts objects index-pair]
            (->> index-pair
                 ((juxt identity reverse))
                 (map (fn [indice]
                        (let [[[c0 v0 m0] [c1 v1 m1]] (map (juxt :center :velocity mass) (map objects indice))
                              [i0 i1]                 (map #(or % [0.0 0.0]) (map impacts indice))
                              n                       (matrix/normalise (matrix/sub c1 c0))]
                          (matrix/sub (matrix/add (matrix/mul n
                                                              (matrix/dot (matrix/sub (matrix/add v1 i1) (matrix/add v0 i0)) n)
                                                              (/ m1 (+ m0 m1))
                                                              2.0)  ; 調整が繰り返されますので、とりあえずは完全弾性衝突で計算します。
                                                  (matrix/add v0 i0))
                                      v0))))
                 (map vector index-pair)
                 (reduce #(apply assoc %1 %2) impacts)))
          (bounce-off-object [objects index-pairs]
            (->> ((fn [impacts bounce-index-pairs]  ; TODO: 無限ループしないかチェックする。
                    (let [next-impacts (reduce #(bounce-off-object' %1 objects %2) impacts bounce-index-pairs)]
                      (if-let [next-bounce-index-pairs (seq (filter (fn [index-pair]
                                                                      (apply bounce-off-object-time'
                                                                        (map (fn [index] (update (objects index) :velocity #(matrix/add % (next-impacts index)))) index-pair)))
                                                                    index-pairs))]
                        (recur next-impacts next-bounce-index-pairs)
                        next-impacts)))
                  (vec (repeat (count objects) nil)) index-pairs)
                 (keep-indexed (fn [index impact]
                                 (if impact
                                   (if-let [object (objects index)]
                                     [index (if (tank? object)
                                              (update object :velocity #(matrix/add % (matrix/div (matrix/mul impact (+ 1.0 coefficient-of-restitution)) 2.0))))]))))  ; 反発係数は、ここで調整します。
                 (reduce #(apply assoc %1 %2) objects)))
          (bounce-off-wall-time [object]
            (->> (mapcat (fn [field-size center velocity]
                           (if (not= velocity 0.0)
                             (map #(/ (- (% (/ field-size 2)) center (% (radius object))) velocity) [+ -])))
                         field-size (:center object) (:velocity object))
                 (filter #(and (f< 0.0 %) (f<= (+ now-time %) 1.0)))
                 (sort)
                 (first)))
          (bounce-off-wall-times [objects]
            (->> objects
                 (keep-indexed (fn [index object]
                                 (if-let [bounce-time (bounce-off-wall-time object)]
                                   [bounce-time index])))
                 (#(if-let [first-bounce-time (first (sort (map first %)))]
                     [first-bounce-time (keep (fn [[bounce-time index]]
                                                (if (f= bounce-time first-bounce-time)
                                                  index))
                                              %)]))))
          (bounce-off-wall [objects indice]
            (->> indice
                 (keep (fn [index]
                         (if-let [object (objects index)]
                           [index (if (tank? object)
                                    (assoc object :velocity (map (fn [field-size center velocity]
                                                                   (cond->> velocity
                                                                     (some #(f= (- (% (/ field-size 2)) center (% (radius object))) 0.0) [+ -]) (* (- coefficient-of-restitution))))
                                                                 field-size (:center object) (:velocity object))))])))
                 (reduce #(apply assoc %1 %2) objects)))
          (damage [objects last-objects indice]  ; 同じタイミングで遠くで大事故が起きたときに影響を受けてしまいますけど、ごめんなさい、無視で。
            (if-let [indice (seq (filter objects indice))]
              (let [damage (* (/ (- (reduce + (map kinetic-energy last-objects))
                                    (reduce + (map kinetic-energy objects)))
                                 (count indice))
                              damage-ratio)]
                (reduce #(update-in %1 [%2 :hp] (fn [hp] (- hp damage))) objects indice))
              objects))
          (force-inside-wall [objects]  ; 浮動小数点計算の誤差ですり抜ける可能性があるので、せめて、強制的に壁の中に戻しておきます。
            (->> objects
                 (keep-indexed (fn [index object]
                                 (if object
                                   [index (assoc object :center (map (fn [field-size center velocity]
                                                                       (cond->> center
                                                                         (< (- center (radius object)) (- (/ field-size 2))) ((constantly (+ (- (/ field-size 2)) (radius object) 0.01)))
                                                                         (> (+ center (radius object)) (+ (/ field-size 2))) ((constantly (- (+ (/ field-size 2)) (radius object) 0.01)))))
                                                                     field-size (:center object) (:velocity object)))])))
                 (reduce #(apply assoc %1 %2) objects)))]
    (let [[bounce-off-object-time bounce-off-object-index-pairs] (bounce-off-object-times objects)
          [bounce-off-wall-time   bounce-off-wall-indice]        (bounce-off-wall-times   objects)]
      (if-let [bounce-time (first (sort (keep identity [bounce-off-object-time bounce-off-wall-time])))]
        (recur (-> objects
                   (linear-motion bounce-time)
                   (cond-> (and bounce-off-object-time (f= bounce-off-object-time bounce-time)) (#(-> %
                                                                                                      (bounce-off-object bounce-off-object-index-pairs)
                                                                                                      (damage % (distinct (flatten bounce-off-object-index-pairs))))))
                   (cond-> (and bounce-off-wall-time   (f= bounce-off-wall-time   bounce-time)) (#(-> %
                                                                                                      (bounce-off-wall bounce-off-wall-indice)
                                                                                                      (damage % bounce-off-wall-indice))))
                   (linear-motion epsilon)  ; 浮動小数点の計算誤差で物体が食い込むことがあるので、少しだけ時間を進めておきます。
                   (force-inside-wall))
               (+ now-time bounce-time epsilon))  ; 食い込み防止で時間を進めたので、忘れずに足しておきます。
        (-> objects
            (linear-motion (- 1.0 now-time))
            (force-inside-wall))))))

(defn- uniform-linear-motion
  [state]
  (assoc state :objects (uniform-linear-motion' (:objects state) 0.0)))

;; Main loop.

(defn- cooling-turret
  [state]
  (->> (:objects state)
       (keep-indexed #(if (tank? %2)
                        %1))
       (reduce #(update-in %1 [:objects %2 :can-shoot-after] dec) state)))

(defn- delete-vanished-objects
  [state]
  (update state :objects #(->> % (keep identity) (vec))))

(defn- inc-turn
  [state]
  (update state :turn inc))

(defn tick-fn
  [ins outs]
  (comp inc-turn delete-vanished-objects cooling-turret uniform-linear-motion (action-fn ins outs)))
