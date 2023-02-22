(ns cobain-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(-main)

(def myvec [1 2 3])

(cons 1 myvec)


(peek [1 2 3 4 5 6])
(pop [1 2 3 4 5 6])

(repeat 2 3)
(into [] '(1 2 3)) ;; Untuk mengerate jadi vector
(subvec [325 46 56  7] 1 3)
(sort (set [1 5 5 35 3]))
(sort (hash-set [11 1 11 11  5 5 35 3]))
(hash-map 1 2 3 4)
([1 2 3 4] 0)
(myvec 5)
(count '(1 2 3 4))
(assoc {:a 1 :b 2} :c 3)
(merge {:a 1 :b 2} {:a 10 :c 3})
(dissoc {:a 1 :b 2} :a)
(def mmap {:a 1 :b 2 :c 3})
(keys mmap)
(vals mmap)

(shuffle myvec)

(defn kuadrat [x] (* x x))
(kuadrat 2)

(#(+ %1 %2) 2 3)

(def foo (fn [x] (fn [y] (+ x y))))

((foo 3) 5)

;; y = mx + c

(def pergaris (fn [m c] (fn [x] (+ (* m x) c))))

(def garis1 (pergaris 3 10))

;; y = ax2 + bx + c

(def pk (fn [a b c] (fn [x] (+ (* x x) (+ b x) c))))

(def garis1 (pk 1 2 3))

(garis1 2)
;;  ANJIR KE FIND PATTERN NYA ALHAMDULILLAH

(garis1 13)

(defn square [x] (* x x))

(def pk
  (fn [a b c]
    (fn [x]
      (+ (* a (square x))
         (+ b x)
         c))))

(def something (pk 1 -5 6))
(something 1)

(something 2)

(defn kecepatan [jarak waktu] (/ jarak waktu))

(kecepatan 20 2)

(defn keliling-persegi-panjang [p l] (+ (* 2 p) (* 2 l)))

(keliling-persegi-panjang 10 2)

(defn luas-segitiga [a t] (* a t 0.5))

(luas-segitiga 6 3)

(defn keliling-trapesium [a b t] (* 0.5 (* t (+ a b))))

(keliling-trapesium 8 5 4)

(defn kondisi?
  [x]
  (if (> x 0) "positif"
      (if (< x 0) "negatif" "nol")))

(defn kondisi1
  [x]
  (cond
    (= x 0) "nol"
    (> x 0) "positif"
    :else "negatif"))

(kondisi? 12)
(kondisi1 0)

(square 2)
;; Rumus ABC = -b +- akar b2-4ac / 2a

(defn kecap
  [a b c]
  (let [det (Math/sqrt (- (square b) (* 4 a c)))]
    [(/ (+ (- b) det) (* 2 a)) ;; spasi nya tuh di minus b be careful
     (/ (- (- b) det) (* 2 a))]))

;; Ada cara lain yang lebih menarik
(defn kecap1
  [a b c x?]
  (let [det (Math/sqrt (- (square b) (* 4 a c)))]
    (/ (+ (- b)
          (if (= x? 1)
            det
            (- det)))
       (* 2 a))))


(kecap 1 -5 6)
(kecap 98 -5 10)

(kecap1 1 -5 6 1)
(kecap1 1 -5 6 2)

;; Bisa nge define x nya setelah misalkan

(let [x 123] (+ x x))

(let [x 12 y 21] (+ x y))

(defn ngasal
  [x]
  (let [a (+ x x)]
    (let [b (* x a)]
      (+ a b))))

(ngasal 3)
(ngasal 6)
(ngasal 8)

(defn sayHello [nama] (str "Hello, " nama))
(sayHello "zaidan")

;; Faktorial

(defn faktorial
  [n]
  (if (<= n 1)
    1
    (* n (faktorial (dec n)))))

(faktorial 4)

(defn sum
  [xs]
  (if (empty? xs)
    0
    (+ (first xs) (sum (rest xs)))))

(sum (range 1 10))

((fn foo [x]
   (when (> x 0)
     (conj (foo (dec x)) x)))
 5)

(defn sum'
  [[x & xs]]
  (if x (+ x (sum' xs)) 0))

(sum' (range 1 10))

(defn drop'
  [n xs]
  (cond
    (zero? n) xs
    (empty? xs) '()
    :else (drop' (- n 1) (rest xs))))

(drop' 5 (range 1 10))


(defn product
  [[x & xs]]
  (if x
    (* x (product xs))
    1
    )
  )

(product (range 1 5))

(defn iter-prime
  [n i]
  (cond (= n i) true
        (zero? (rem n i)) false
        :else (iter-prime n (inc i))
        )
  )

(defn prime? 
  [n]
  (cond (<= n 1) false
        (= n 2) true
        :else (iter-prime n 2)
        )
  )

(prime? 101)
(filter prime? (range 15))

(defn prime?2
  [n]
  (let [iter (fn iter [i]
               (cond (= n i) true 
                     (zero? (rem n i)) false
                     :else (iter (inc i))
                     ))]
    (cond (<= n 1) false
          (= n 2) true
          :else (iter 2))
    )
  )

(prime?2 2)
(into [] (filter prime?2 (range 10)))

(defn prime?3
  [n]
  (let [iter (fn iter [i]
               (cond (= n i) true
                     (zero? (rem n i)) false
                     :else (iter (+ i 2)))
               )]
    (cond (<= n 1) false
          (= n 2) true
          (even? n) false
          :else (iter 3)
          )
    ) 
  )

(prime?3 9)
(filter prime?3 (range 20))

(defn prime?4 
  [n]
  (let [akar-n (Math/sqrt n)
        iter (fn iter [i]
               (cond (> i akar-n) true
                     (zero? (rem n i)) false
                     :else (iter (+ i 2))))]
    (cond (<= n 1) false
          (= n 2) true
          (even? n) false
          :else (iter 3)
          )
    )
  )

(prime?4 2)
(filter prime?4 (range 10))

(time(count (filter prime? (range 10000))))
(time(count (filter prime?2 (range 10000))))
(time(count (filter prime?3 (range 10000))))
(time(count (filter prime?4 (range 10000))))

(defn fak
  [i]
  (if (<= i 1)
    1
    (* i (fak (dec i)))
    ))
(fak 5)

(defn sum
  [[x & xs]]
  (if x (+ x (sum xs)) 0)
  )

(sum (range 10))

(defn sum'
  [lst]
  (loop [[x & xs] lst res 0]
    (if x
      (recur xs (+ res x))
      res)))

(sum' (range 10))

(defn jumlah-dari-0-n
  [n]
  (loop [i 0 res 0]
    (if (> i n)
      res
      (recur (+ i 1) (+ res i))
      )
    )
  )

(jumlah-dari-0-n 54)

(defn sum
  [[x & xs]]
  (if x (+ x (sum xs)) 0))

(sum (range 10))

(defn product 
  [[x & xs]]
  (if x (* x (product xs)) 1)
  )

(product (range 1 5))

(+)
(*)

(defn folding [f [x & xs]]
  (if x
    (f x (folding f xs))
    (f))
  )

(folding + (range 5))
(folding * (range 1 5))

(defn kuadratin [[x & xs]]
  (if x
    (cons (* x x) (kuadratin xs))
    )
  )

(kuadratin [1 2 3])

(defn kubikin [[x & xs]]
  (if x
    (cons (* x x x) (kubikin xs))
    )
  )

(kubikin [1 2 3])

(defn all-pangkat [f [x & xs]]
  (if x
    (cons (f x) (all-pangkat f xs))
    [])
  )

(all-pangkat #(+ % % %) [1 2 3])

(defn square [x] (* x x))
(defn quad [x] (* x (square x)))

(-> 123 
    (square)
    (quad)
    (- 10)
    )

(->> 123
     square
     quad
     (- 10)
     )

(-> 100
    (- 20))

(->> 100
     (- 20))

(->> [1 2 3 4 5 6]
     (map square)
     (reduce +)
     (+ 999)
     )

(->> (range 10)
     (map square)
     (reduce +)
     (+ 999)
     )

(map-indexed #(vector %1 %2) [5 4 3 2 1])
(map-indexed #(vector %1 %2) (range 10))

(keep #(if (odd? %) % nil) (range 10))
(keep #(when (odd? %) %) (range 10))

(->> (range 10)
     (keep #(when (odd? %) %)))

(map square (range 10))
(mapv square (range 10))
(filter even? (range 10))
(filterv even? (range 10))

(->> (range 1 101)
     (filter odd?)
     (map square)
     (reduce +)
     )

(let [a 123] (* 2 a))

(let [a 3 b 6]
  (* a b (let [x 10] x) )
  )

(defn komplek [a b c d]
  (let [i 10
        j 12
        k 13]
    [(* a b c d) (* i j k)]
    ))

(komplek 1 2 3 4)

(defn komplek2 [a b]
  (if (even? a)
    (let [x 10]
      (* a x b))
    (let [ y (* a b)]
      (if (even? y)
        (+ a b)
        (- a b))))
  )

(komplek2 5 4)

(defn komplek3 [a b]
  (* a
     (if (even? b) (+ b 1) b)
     (let [ x (* 2 a b)]
       (if (> x 10)
         (* a b x)
         (+ a b x)))
     ))

(komplek3 7 8)

(remove odd? (range 10))

(shuffle (range 10))

(sort (shuffle (range 10)))
(sort > (shuffle (range 10)))
(sort < (shuffle (range 10)))
(sort = (shuffle (range 10))) ;; di randomin lagi

(= (range 10)(sort (shuffle (range 10))))

(->> (range 10)
     (map #(vector (rem % 3) %))
     (sort-by first)
     )

(sort-by first [[1 2] [4 1] [ 5 6 ]])
(sort-by second [[1 2] [4 1] [ 5 6 ]])

(apply + [ 1 2 3 4 5])
(apply * [ 1 2 3 4 5])

(partition 3 (range 22))
(partition-by #(= 0 (rem % 3)) (range 22))

(take-while even? (range 10))
(take-while #(< % 5) (range 10))

(drop-while odd? (range 10))
(drop-while integer? (range 10))

(take-while integer? (range 10))
(take-while #(< % 5) (range 10))
(take-while #(< % 5) [1 2 3 4 43325 4535  52 34 3 2 1])

(group-by odd? (range 10))
(group-by #(rem % 7) (range 100))

(def memap (zipmap [:a :b :c :d :e :f :g]
                   (range 10)))
memap
(keys memap)
(vals memap)


(into [] (range 10))
(into '() (range 10))
(into #{} (range 10))

(into {} [1 2 3])

(identity 123)
(map #(vector (vals %)
              (keys %))
     memap)

(->> (range 1 101)
     (map #(* % %))
     (filter #(or (= 0 (rem % 19))
                 (= 0 (rem % 37))))
     )

(assoc memap :k 10)
(dissoc memap :a :c)

(merge memap {:h 10 :a 10})
(merge-with + memap {:a 9 :b 10})

(def komap {:a {:b 10}
            :b [1 2 3 4 5 6]
            :c {:a 123
                :b 321
                :c [1 2 3 4 5]}
            :d [123 321 234 {:k 10
                             :j 20}]
            })

(identity komap)
(get-in komap [:a])
(get-in komap [:a :b])
(get-in komap [:d 3 :k])

(update-in komap [:a] #(assoc % :c 20))
(update-in komap [:b] #(take 3 %))

(assoc-in komap [:a] 10)
(assoc-in komap [:a :b] 50)

(every? odd? (range 10))
(some odd? (range 10))

(defn prime?
  [n]
  (->> (range 2 (inc (int (Math/sqrt n))))
       (every? #(pos? (rem n %)))
       ))

(prime? 2)

(mapcat #(vector % (* % 2) (* % 3)) (range 10))

(->> (iterate inc 1)
     (take 10)
     )

(->> (iterate #(+ 2 %) 1)
     (take 10)
     )

(->> (iterate #(+ 2 %) 1)
     (take 10)
     (drop-while #(> % 100))
     (filter #(= 10 (rem % 8)))
     )

(->> (iterate rest (range 15))
     (take 10)
    ;;  (mapcat identity)
     (map #(reduce + %))
     )

(defn add [n]
  (fn [a] (+ n a))
  )

(def add10 (add 10))

(add10 20)

(def square #(* % %))
(def triad #(* % % %))

((comp square #(+ % 5)) 10)
((comp square [1 2 3]) 1)

((juxt square triad [1 2 3 4 5 6 7 8]) 2)

((juxt :a :b) komap)

(#(+ % 5) 10)

((partial + 5) 10)

((partial map #(* % %)) [1 2 3])
(def smap (partial map #(* % %)))
(smap (range 10))

(for [i (range 10)] i)
(for [i (range 5)
      j (range i 5)
      ]
  [i j]
  )

(defn foo-one [n]
  (for [i (range n)] (* i i))
  )

(foo-one 7)

(defn foo-two [n]
  (for [i (range n)
        j (range i n)
        :when (odd? i)
        ]
    [i j]
    )
  )

(for [i (range 10)
      :when (odd? i)]
  [i])

(foo-two 10)

(defn foo-three [n]
  (for [i (range n)
        j (range i n)
        :while (< i 5)
        :while (< (* i j) 50)
        ]
    [i j]
    )
  )

(foo-three 5)

(for [i (range 10)
      j (range i 10)
      :while(odd? (* i j))
      ]
  [i j]
  )

(apply + [1 2 3 4])
(reduce + '(1 2 3 4))
(apply + 1 2 3 [1 2 3])
(eval (concat [+ 1 2 3] [1 2 3]))

(defn foo [a-list]
  (reduce #(+ %1 %2) a-list))
(foo (range 10))

(reduce conj () [1 2 3 4])

(defn foo-too [i]
  (reduce (fn [a b] (cons b (reverse a))) [] i)
  )

(foo-too (range 10))

(take 5 (iterate #(+ 3 %) 1))

(println "sore")

(def my-var [123])

;; (reset! my-var 321)

;; (dotimes [i 10] (println "sore"))

(when true "sore")
(when true "sore" "siang")
(if true "sore" "siang")

;; (if false
;;   "keluar ini"
;;   (do (println "sore mbak!")
;;       (reset! my-var "something")
;;       "yang ini"
;;       ))
;; Gajelas dia error mulu

(doseq [i (range 10)] (print i))
(doseq [i (range 10)
        :when (even? i)
        ] (print i))

(defn sol1a [lim]
  (->> (range lim)
       (filter #(or (zero? (rem % 3))
                    (zero? (rem % 5))
                    ))
       (reduce +)
       time
       )
  )

(defn sol1b [lim]
  (time (- (+ (reduce + (range 3 lim 3))
        (apply + (range 5 lim 5))
        )
     (reduce + (range 15 lim 15))
     )
  ))

(defn sol1c [lim]
  (->> (for [i (range lim)
             :when (or (zero? (rem i 3))
                       (zero? (rem i 5)))] i)
       (reduce +)
       time
       )
  )

(defn sol1d [lim]
  (time (let [sum (atom 0)]
          (dotimes [i lim]
            (if (or (zero? (rem i 3))
                    (zero? (rem i 5)))
              (reset! sum (+ @sum i))))
          @sum
          )
         ))

(defn sol1e [lim]
  (time (loop [i 1 
               sum 0]
          (if (>= i lim)
            sum
            (if (or (zero? (rem i 3))
                    (zero? (rem i 5)))
              (recur (+ i 1) (+ sum i))
              (recur (+ i 1) sum)
              )) 
          ))
  )

(defn sol1f [lim]
  (->> (range lim)
       (keep #(when (or (zero? (rem % 3))
                        (zero? (rem % 5))) %))
       (reduce +) 
       time
       )
  )

(sol1a 1000)
(sol1b 1000)
(sol1c 1000)
(sol1d 1000)
(sol1e 1000)
(sol1f 1000)

(defn fibo [n]
  (if (<= n 1)
    1
    (+ (fibo (dec n)) (fibo (- n 2)))
    )
  )

(def mfibo 
  (memoize 
   (fn [n] 
     (if (<= n 1) 
       1 
       (+ (mfibo (dec n)) (mfibo (- n 2)))))) 
  )

(defn sol2a [lim]
  (->> (range)
       (map fibo)
       (take-while (partial > lim))
       (filter even?)
       (reduce +)
       time
       )
  )

(defn sol2b [lim]
  (->> (range)
       (map mfibo)
       (take-while (partial > lim))
       (filter even?)
       (reduce +)
       time
       )
  )

(sol2a 5)
(sol2b 5)

(defn lfibo [n]
  (->> (iterate #(let 
                  [[a b] %] 
                   [b (+ a b)])
                [1 1])
       (map first)
       (take n)
       )
  )

(lfibo 10)

;; BATASSSSSSSSSSSSSSSSSSSSS

(filter (clojure.string/upper-case))

;; ((fn [x y] (nth x y)) [:a :b :c] 0)

((fn [x y] (first (drop y x))) [:a :b :c] 0)


(partition-all 3 (range 5))

(filter (fn [x] true) (quote (:anything :goes :here)))

(let [not-a-symbol? (complement symbol?)]
  (map not-a-symbol? [:a 'b "c"]))

(repeat 100 :foo)

((fn [[a b c]] (str "First comes " a ", "
                    "then comes " b ", "
                    "then comes " c " with the baby carriage"))
 ["love" "marriage" "Clojure"])

;; Maunya pas ketemu angka ganjil diambil, terus kalo genap di drop.

(let [atomic-clock (atom 0)]
  (do
    (swap! atomic-clock + 1 2 3 4 5)
    @atomic-clock))

(let [atomic-clock (atom 0)]
  (do
    (compare-and-set! atomic-clock 0 :fin)
    @atomic-clock))

(= "123 Test Lane, Testerville, TX"
   (let [test-address {:street-address "123 Test Lane"
                       :city "Testerville"
                       :state "TX"}
         {:keys [street-address city state]} test-address]
     (str street-address ", " city ", " state)))

(= {
    :original-parts ["Stephen" "Hawking"] 
    :named-parts {
                  :first "Stephen" 
                  :last "Hawking"}} 
   (let [[first-name last-name :as full-name] ["Stephen" "Hawking"]]
     ["Stephen" "Hawking"]
     {:original-parts full-name :named-parts {:first first-name :last last-name}}
     ))

(let [[first-name last-name :as full-name] ["Stephen" "Hawking"]])

(= "Rich Hickey aka The Clojurer aka Go Time aka Macro Killah"
   (let [[first-name last-name & aliases]
         (list "Rich" "Hickey" "The Clojurer" "Go Time" "Macro Killah")]
     
     ))

(let [[first-name last-name & aliases]
      (list "Rich" "Hickey" "The Clojurer" "Go Time" "Macro Killah")]
  (str first-name " " last-name " aka " (first aliases) " aka " (second aliases) " aka " (last aliases))
  )

(let [test-address {:street-address "123 Test Lane"
                         :city "Testerville"
                         :state "TX"}
           {street-address :street-address, city :city, state :state} test-address]
  (str street-address ", " city ", " state)
  )

(let [test-address {:street-address "123 Test Lane"
                    :city "Testerville"
                    :state "TX"}]
  ( ["Test" "Testerson"] test-address))

(map inc (take 3 (drop 2 [2 5 4 1 3 6])) )
(->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc))

(clojure.set/subset? #{1 2} #{1 2})

(for [x (range 40) 
      :when (= 1 (rem x 4))] x)

(true?  (fn [x] (vals x) :a {:a nil :b 2}))
(false? (fn [x] (vals x) :b {:a nil :b 2}))

(seq "hello")

;; ((fn [x] (seq x ())) "HeLlO, WoRlD!")

;; ((map (fn [index] (* index index)) (filter odd? (range 10))) (for [index (range 10) :when 1] 1))

(= (mapcat (fn [x] (repeat 2 x)) [1 2 3]) '(1 1 2 2 3 3))

;; (= (mapcat (fn [x y] (repeat y x)) [1 2 3] 2) '(1 1 2 2 3 3))

;; (mapcat (fn [x] (repeat x (fn [y] (y)))) [1 2 3] 2)

((fn [s n] (mapcat (fn [z] (repeat n z)) s)) [1 2 3] 4)



(map (fn [x] (repeat 2 x)) [1 2 3] )

(mapcat (fn [x] (repeat 2 x)) [1 2 3] )

((fn [x]
   (apply str (reverse x))) "clojure")

(true? (= (fn [x]
            (apply str (reverse x)))
          "clojure"))

(true? (#(= (seq %) (reverse (seq %)))
        "clojure"))

(clojure.string/upper-case "zaidan")

(vector :a :b :c) 1

(conj (vector :a :b :c) 1)

()

(conj (list :a :b :c) 1)


(let [a 1 b [1 2 3 4]] (cons a b))

(let [b [1 2 3 4 5 6]] [(first b) (rest b)])

()

(vector nil nil)

(partition 3 [:a :b :c :d :e])

(take 2 [0 1 2 3 4 5])

(map nil? [:a :b nil :c :d])

(partition-all 3 (range 5))

(quote (:anything :goes :here))

(filter (fn [x] false) (quote (:anything :goes :here)))

(filter (fn [x] true) (quote (:anything :goes :here)))

(partition 3 5 (range 13))

(partition 3 3 [:this :are "my" "words"] (range 7))

(map (fn [index] (* index index)) (filter odd? (range 10))) 

(for [index (range 10)
      :when (odd? index)
      ] index)

(for [i (range 10)] i)

      ((fn [x] (seq x)) "abc")

(= (flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))   
(seq '((1 2) 3 [4 [5 6]]))
      
(reduce (fn [a b] (* a b)) [1 2 3 4])

(let [not-a-symbol? (complement symbol?)]
  (map not-a-symbol? [:a 'b "c"]))

(filter odd? (range 10))

(iterate inc 0)

(defn aja [[a b]] (str b a))

(aja [:foo :bar])

(map inc (take 3 (drop 2 [2 5 4 1 3 6])))

(some #{2 7 6} [5 6 7 8])

(defn sum-xs [xs]
  (reduce + xs))

((fn [xs]
   (reduce + xs)) [1 2 3])

(sum-xs [1 2 3])


(for [index (range 6)] index)

(#(apply str (re-seq #"[A-Z]+" %)) "HeLlO, WoRlD!")

(defn tulisan [x]
  (->> 
   (re-seq #"[A-Z]" x)
   (apply str)
   )
  )

(tulisan "HeLlO, WoRlD!") 

(def lala 123)
lala

(= (max 1 8 3 4) 8)
(max 1 8 3 4)

(def nomer [1 2 3 4])
(apply max nomer)

(defn product
  [[x & xs]]
  (if x
    (* x (product xs))
    1))

(product (range 1 5))

(defn drop'
  [n xs]
  (cond
    (zero? n) xs
    (empty? xs) '()
    :else (drop' (- n 1) (rest xs))))

(drop' 5 (range 1 10))

(defn maksimum
  [& x]
  (last (sort x)))

(maksimum 192819 8 9)

  ((defn coba [& x] (last (sort x))) 1 2 3 6656)

(apply +
  (nth [1 2 3] 0) (nth [:a :b :c] 0))

(first [:a :b :c])

(nth [1 2 3] 0)

(seq ["a" ["b"] "c"])

(partition 3 [1 2 3 4 5 6])

(merge (partition 1 [:a :b :c :d]))

(partition 2 [[1 2] [3 4] [5 6]])

(defn pecah [x vec]
  [(subvec vec 0 x) (subvec vec x)]
  )

(pecah 3 [1 2 3 4 5 6])  

(= [10 20 30] 
   (map (fn [x] (* x 10)) 
        (filter (fn [x] (< x 4)) [1 2 3 4 5 6 7 8])))

(map (fn [x] (* x 10) (filter (fn [x] (if (< x 4) 
                                        x
                                        #(* % %))
                                        )) [1 2 3 4 5 6 7 8]))

(for [index (range 10)
      :when (if (odd? index) (* index index))] 
  (if (odd? index) (* index index)))


(let [test-address {:street-address "123 Test Lane"
                           :city "Testerville"
                           :state "TX"}]
         ({:vals [:street-address]} test-address))

;; (= (__ 3) '(1 1 2))

(fn [x])

((fn [a b] (nil? (a b 0))) :a {:a nil :b 2})
(((fn [a vec] (zipmap vec (repeat a)) 0 [:a :b :c])))

(range 1 4)

((fn [a b] (for [i (range a b)]
  i)) 3 6)

(defn panjangin [a b]
  (take (- b a) (iterate inc a))
  )

(panjangin 1 4)
(panjangin 5 8)

(interpose 0 [1 2 3])
(partition 1 [1 2 3])
(partition 1 ["one" "two" "three"])

(flatten (partition 2 (interleave [:a :b :c] [1 2 3])))
;; coba pake mapcat


;; (#{2 3})

;; BATAS
;; BATAS
;; BATAS
;; BATAS

(defn build-fib
  [x]
  (conj x (+ (last x) (nth x (- (count x) 2)))))

(build-fib 5)

(defn fib
  ([num] (fib [0 1] num))
  ([fib-list num]
   (if (<= (count fib-list) num)
     (recur (build-fib fib-list) num)
     fib-list)))

(defn fib-num
  [num]
  (last (fib num)))

(fib 6)
(fib-num 6)


;; (defn fib-z [x]
;;   (case x
;;     0 0
;;     1 1
;;     (+ (fib (- x 1)) (fib (- x 2)))))

;; (fib-z 10))

;; (defn fib-seq [x]
;;   ([num] (fib-seq [0 1] num))
;;   ([fib-list num])
;;    (if (<= (count fib-list) num)
;;     (recur (fib fib-list) num) 
;;      fib-list)))

;;  (defn fib-sum [num]
;;    [num]
;;    (last (fib-seq num)))







