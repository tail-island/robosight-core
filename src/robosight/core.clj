(ns robosight.core
  (:require (clojure.core  [matrix        :as matrix])
            (clojure.data  [json          :as json])
            (clojure.math  [combinatorics :as combinatorics])
            (clojure.tools [logging       :as logging]))
  (:import  (java.util.concurrent TimeoutException TimeUnit)))

;; Comparing floating points utilities... See <https://tail-island.github.io/programming/2017/04/27/floating-point.html>.

(def ^:private epsilon
  (Math/pow 10 -6))

(defn- f=
  [x y]
  (let [d (Math/abs (- x y))]
    (or (<= d epsilon) (<= d (* (max (Math/abs x) (Math/abs y)) epsilon)))))

(def ^:private fnot=
  (complement f=))

(defn- f<
  [x y]
  (and (< x y) (fnot= x y)))

(defn- f<=
  [x y]
  (or (< x y) (f= x y)))

(def ^:private f>
  (complement f<=))

(def ^:private f>=
  (complement f<))

;; Options.

(def ^:dynamic *practice-mode*)

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
  "引数で指定されたオブジェクトの半径を返します。"
  [object]
  ({:tank tank-radius :shell shell-radius}
   (:type object)))

(defn density
  "引数で指定されたオブジェクトの密度を返します。"
  [object]
  ({:tank tank-density :shell shell-density}
   (:type object)))

(defn mass
  "引数で指定されたオブジェクトの質量を返します。"
  [object]
  (* Math/PI (Math/pow (radius object) 2) (density object)))

(defn kinetic-energy
  "引数で指定されたオブジェクトの運動エネルギーを返します。"
  [object]
  (if object
    (/ (* (mass object) (Math/pow (matrix/length (:velocity object)) 2)) 2)
    0.0))

(defn tank?
  "引数で指定されたオブジェクトが戦車であればtrue、そうでなければfalseを返します。"
  [object]
  (= (:type object) :tank))

(defn shell?
  "引数で指定されたオブジェクトが弾丸であればtrue、そうでなければfalseを返します。"
  [object]
  (= (:type object) :shell))

(defn broken?
  "引数で指定されたオブジェクトが破壊されていればtrue、そうでなければfalseを返します。"
  [object]
  (f<= (:hp object) 0.0))

(defn tanks-coll
  "状態（:objectsに戦車10両と弾丸複数のデータが入っている）から、Leftチームの戦車群とRightチームの戦車群を取得します。"
  [state]
  (->> (:objects state)
       (partition 5)
       (take 2)))

(defn game-finished?
  "試合終了であればtrue、そうでなければfalseを返します。"
  [state]
  (or (> (:turn state) max-turn)
      (some #(every? broken? %) (tanks-coll state))))

(defn winner
  "勝利チームを返します。Leftチームが勝利なら0、Rightチームが勝利なら1、引き分けならnilになります。"
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
  "2次元の回転行列を生成します。"
  [direction]
  [[(Math/cos direction) (Math/sin direction)] [(- (Math/sin direction)) (Math/cos direction)]])

(defn- forward
  "前進します。"
  [state object-index impact-speed]
  (update-in state [:objects object-index :velocity] #(matrix/add % (matrix/mmul [(max (min impact-speed tank-impact-speed-max) tank-impact-speed-min) 0.0]
                                                                                 (rotation (get-in state [:objects object-index :direction]))))))

(defn- turn-to
  "回転します。"
  [state object-index direction]
  (assoc-in state [:objects object-index :direction] direction))

(defn- shoot
  "砲撃します。"
  [state object-index impact-speed]
  (let [[center velocity direction radius can-shoot-after] ((juxt :center :velocity :direction radius :can-shoot-after) (get-in state [:objects object-index]))
        rotation      (rotation direction)
        shell-center  (matrix/add center (matrix/mmul [(+ radius shell-radius 0.1) 0.0] rotation))
        enough-space? (and (every? identity (map (fn [field-size shell-center]
                                                   (f< (+ shell-center shell-radius) (+ (/ field-size 2)))
                                                   (f> (- shell-center shell-radius) (- (/ field-size 2))))
                                                 field-size shell-center))
                           (every? #(f> (matrix/length (matrix/sub shell-center (:center %))) (+ tank-radius shell-radius)) (:objects state)))]
    (cond-> state
      (and (<= can-shoot-after 0) enough-space?) (-> (assoc-in  [:objects object-index :can-shoot-after] shoot-interval)
                                                     (update-in [:objects] #(conj % {:type     :shell
                                                                                     :center   shell-center
                                                                                     :velocity (matrix/add velocity (matrix/mmul [(max (min impact-speed shell-impact-speed-max) shell-impact-speed-min) 0.0]
                                                                                                                                 rotation))}))))))

(defn- action-fn
  "思考ルーチンの標準入力に状況を出力し、思考ルーチンの標準出力から戦車への指示を入力し、指示を実行します。"
  [ins outs]
  (fn [state]
    (->> (map (fn [team in out friends-and-enemies]
                ;; 思考ルーチンの標準入力に状況を出力します。
                (doto in
                  (.println (->> (json/write-str friends-and-enemies)
                                 (logging/spyf (format "team %d, stdin is '%%s'." team))))
                  (.flush))
                ;; 思考ルーチンの標準出力から戦車への指示を入力します。
                (->> (let [read-line (future (.readLine out))]
                       (if *practice-mode*
                         @read-line
                         (try
                           (.get read-line
                                 (if (<= (:turn state) 1)
                                   10000  ; 初回はいろいろ時間がかかると思うので（Clojureで作ったサンプルの起動がすげー遅かった……）、制限時間は10秒にします。
                                   thinking-time)
                                 TimeUnit/MILLISECONDS)
                           (catch TimeoutException ex
                             (.cancel read-line true)
                             (throw (ex-info "Timeout error." {:reason :timeout :timeout-team team}))))))
                     (logging/spyf (format "team %d, stdout is '%%s'." team))
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
         ;; 指示を実行します。
         (apply concat)
         (map-indexed cons)
         (reduce (fn [state [object-index function parameter]]
                   (cond-> state
                     (and ((complement broken?) (get-in state [:objects object-index])) function) (function object-index parameter)))
                 state))))

;; Uniform linear motion.

(defn- uniform-linear-motion'
  "等速直線運動します。衝突の瞬間まで等速直線運動し、衝突したオブジェクトを適切に反射させ、now-timeを進めて再帰呼出しします。"
  [objects now-time]
  (letfn [(linear-motion [objects duration]  ; 等速直線運動します。
            (->> objects
                 (keep-indexed (fn [index object]
                                 (if object
                                   [index (assoc object :center (matrix/add (:center object) (matrix/mul (:velocity object) duration)))])))
                 (reduce #(apply assoc %1 %2) objects)))
          (solve-quadratic-equation [a b c]  ; 二次方程式を解きます。
            (let [d (- (Math/pow b 2) (* 4 a c))]
              (if (>= d 0)
                ;; 二次方程式の階の公式そのままより、以下のほうが浮動小数点の誤差が少ないらしい。。。
                (let [op (if (>= b 0.0)
                           -
                           +)
                      x1 (/ (op (- b) (Math/sqrt d)) (* 2 a))
                      x2 (if (= x1 0.0)
                           x1
                           (/ c a x1))]
                  [x1 x2]))))
          (bounce-off-object-time' [object other]  ; 2つのオブジェクトが衝突する時刻を計算します（）。
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
          (bounce-off-object-time [object other]  ; 2つのオブジェクトがターン中に衝突する時刻を計算します。使用したアルゴリズムは<http://marupeke296.com/COL_3D_No9_GetSphereColliTimeAndPos.html>です。
            (some->> (bounce-off-object-time' object other)
                     (#(if (f<= (+ now-time %) 1.0)
                         %))))
          (bounce-off-object-times [objects]  ; オブジェクト群が衝突する時刻の集合をを計算します。
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
          (bounce-off-object' [impacts objects index-pair]  ; 衝突した2つのオブジェクトを反射させます。使用したアルゴリズムは<http://marupeke296.com/COL_Basic_No5_WallVector.html>です。
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
          (bounce-off-object [objects index-pairs]  ; 衝突したオブジェクト（複数組の可能性がある）を反射させます。オブジェクトが正しく反射するまで、繰り返して反射を実施していきます。
            (->> ((fn [impacts bounce-index-pairs]
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
          (bounce-off-wall-time [object]  ; オブジェクトが最初に壁見衝突する時刻を計算します。
            (->> (mapcat (fn [field-size center velocity]
                           (if (not= velocity 0.0)
                             (map #(/ (- (% (/ field-size 2)) center (% (radius object))) velocity) [+ -])))
                         field-size (:center object) (:velocity object))
                 (filter #(and (f< 0.0 %) (f<= (+ now-time %) 1.0)))
                 (sort)
                 (first)))
          (bounce-off-wall-times [objects]  ; オブジェクトが壁に衝突する時刻の集合を計算します。
            (->> objects
                 (keep-indexed (fn [index object]
                                 (if-let [bounce-time (bounce-off-wall-time object)]
                                   [bounce-time index])))
                 (#(if-let [first-bounce-time (first (sort (map first %)))]
                     [first-bounce-time (keep (fn [[bounce-time index]]
                                                (if (f= bounce-time first-bounce-time)
                                                  index))
                                              %)]))))
          (bounce-off-wall [objects indice]  ; オブジェクトを壁から反射させます。使用したアルゴリズムは<http://marupeke296.com/COL_Basic_No5_WallVector.html>です。
            (->> indice
                 (keep (fn [index]
                         (if-let [object (objects index)]
                           [index (if (tank? object)
                                    (assoc object :velocity (map (fn [field-size center velocity]
                                                                   (cond->> velocity
                                                                     (some #(f= (- (% (/ field-size 2)) center (% (radius object))) 0.0) [+ -]) (* (- coefficient-of-restitution))))
                                                                 field-size (:center object) (:velocity object))))])))
                 (reduce #(apply assoc %1 %2) objects)))
          (damage [objects last-objects indice]  ; ダメージを計算します。同じタイミングで遠くで大事故が起きたときに影響を受けてしまいますけど、ごめんなさい、無視で。
            (if-let [indice (seq (filter objects indice))]
              (let [damage (* (/ (- (reduce + (map kinetic-energy last-objects))
                                    (reduce + (map kinetic-energy objects)))
                                 (count indice))
                              damage-ratio)]
                (reduce #(update-in %1 [%2 :hp] (fn [hp] (- hp damage))) objects indice))
              objects))
          (force-inside-wall [objects]  ; 強制的に壁の中に戻します。浮動小数点計算の誤差ですり抜ける可能性があるので、せめて、強制的に壁の中に戻しておきます……。
            (->> objects
                 (keep-indexed (fn [index object]
                                 (if object
                                   [index (assoc object :center (map (fn [field-size center velocity]
                                                                       (cond->> center
                                                                         (< (- center (radius object)) (- (/ field-size 2))) ((constantly (+ (- (/ field-size 2)) (radius object) 0.01)))
                                                                         (> (+ center (radius object)) (+ (/ field-size 2))) ((constantly (- (+ (/ field-size 2)) (radius object) 0.01)))))
                                                                     field-size (:center object) (:velocity object)))])))
                 (reduce #(apply assoc %1 %2) objects)))]
    ;; オブジェクト同士が衝突する時刻と、オブジェクトが壁に衝突する時刻を取得します。
    (let [[bounce-off-object-time bounce-off-object-index-pairs] (bounce-off-object-times objects)
          [bounce-off-wall-time   bounce-off-wall-indice]        (bounce-off-wall-times   objects)]
      ;; 衝突して反射すると等速直線運動になりませんから、最初の衝突を取得して、反射させ、再帰呼出しします。衝突がなければ、次のターンまで等速直線運動させます。
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
               (+ now-time bounce-time epsilon))  ; 食い込み防止で時間を進めたので、忘れずに足しておきます
        (-> objects
            (linear-motion (- 1.0 now-time))
            (force-inside-wall))))))

(defn- uniform-linear-motion
  "ターンとターンの間の、等速直線運動を実行します。"
  [state]
  (assoc state :objects (uniform-linear-motion' (:objects state) 0.0)))

;; Main loop.

(defn- cooling-turret
  "砲撃後、砲塔を冷やします。"
  [state]
  (->> (:objects state)
       (keep-indexed #(if (tank? %2)
                        %1))
       (reduce #(update-in %1 [:objects %2 :can-shoot-after] dec) state)))

(defn- delete-vanished-objects
  "衝突で消えたオブジェクトを状態から削除します。"
  [state]
  (update state :objects #(->> % (keep identity) (vec))))

(defn- inc-turn
  [state]
  (update state :turn inc))

(defn tick-fn
  "ターンを実行する関数を返します。アクションを実行し、等速直線運動し、砲塔を冷やし、衝突で消えたオブジェクトを削除し、ターンを増加させます。"
  [ins outs]
  (comp inc-turn delete-vanished-objects cooling-turret uniform-linear-motion (action-fn ins outs)))
