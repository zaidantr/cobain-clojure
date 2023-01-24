(ns cobain-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(-main)

(def myvec [1 2 3])

(cons 1 myvec)

(defn build-fib
  [x]
  (conj x (+ (last x) (nth x (- (count x) 2))))
  )

(peek [1 2 3 4 5 6 ])
(pop [1 2 3 4 5 6 ])

(repeat 2 3)
(into [] '(1 2 3)) ;; Untuk mengerate jadi vector
(subvec [325 46 56  7] 1 3)
(sort (set [1 5 5 35 3  ]))
(sort (hash-set [11 1 11 11  5 5 35 3  ]))
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

(defn luas-segitiga [a t] (* (* a t) (0.5)))

(luas-segitiga 6 2)

;; (defn fib
;;   ([num] (fib [0 1] num))
;;   ([fib-list num]
;; (if (<= (count fib-list) num)
;;     (recur (build-fib fib-list) num)
;;     fib-list)))

;; (defn fib-num
;;   [num]
;;   (last (fib num))
;;   )

;; (fib 15)
;; (fib-num 6)


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







