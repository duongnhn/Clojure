(ns clojure-noob.core  
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
(println "Cleanliness is next to godliness")

(defn train
  []
  (println "Choo choo!"))

(defn add
  "add number"
  ;; 1-arity 
  ([a]
   (+ 1 a)
   )
  ;; 2-arity
  ([a b]
   (+ 1 a b)
   )
)

(defn announce-treasure-location
  [{:keys [lat lng]}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))

(defn inc-maker
  "Create a custom incrementor"
  [inc-by]
  #(+ % inc-by))

(def inc3 (inc-maker 3))

(defn fibo
  "fibonacci"
  [n]
  (loop [iter 0]
    (println (str "n = " (+ n iter 1)))
    (if (> iter 3)
      (println "Stop")
      (recur (inc iter))
      )
    )
)

(defn make-matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn needs-matching-part?
  [part]
  (re-find #"^left-" (:name part)))

(defn make-matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

; ~~~1~~~
(defn symmetrize-body-parts
  "Expects a seq of maps which have a :name and :size"
  [asym-body-parts] ; 
  (loop [remaining-asym-parts asym-body-parts ; ~~~2~~~
         final-body-parts []]
    (if (empty? remaining-asym-parts) ; ~~~3~~~
      final-body-parts
      (let [[part & remaining] remaining-asym-parts ; ~~~4~~~
            final-body-parts (conj final-body-parts part)]
        (if (needs-matching-part? part) ; ~~~5~~~
          (recur remaining (conj final-body-parts (make-matching-part part))) ; ~~~6~~~
          (recur remaining final-body-parts))))))

(defn better-symmetrize-body-parts
  "Expects a seq of maps which have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (let [final-body-parts (conj final-body-parts part)]
              (if (needs-matching-part? part)
                (conj final-body-parts (make-matching-part part))
                final-body-parts)))
          []
          asym-body-parts))

(def ls1 [1 8 3 4])

(defn func 
  "4clojure"
  [& args]
  (reduce (fn [x y] (if (< x y) y x)) args)
)

(println (func 3 4 5 6))

(defn exp 
  [x n]
  (if (= 0 n)
    1
    ;;else
    (* x (exp x (dec n)))
    )
)

(defn exp1
  [x n]
  (loop [r 1 nt n]
    (if (= 0 nt) 
      r
      ;;else
      (recur (* r x) (dec nt))
      )
      )
)

(defn f1 
  [n]
  (fn [x] (reduce * (repeat n x)))
)

(defn fs
  [n ls]
  (list (take n ls) (drop n ls))
)

(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))

(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true  :name "McFishwich"}
   1 {:makes-blood-puns? false, :has-pulse? true  :name "McMackson"}
   2 {:makes-blood-puns? true,  :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true,  :has-pulse? true  :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))
(defn vampire?
  [record]
  (and (:makes-blood-puns? record)
       (not (:has-pulse? record))))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))

(def identities
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Spiderman" :real "Peter Parker"}
   {:alias "Santa" :real "Your mom"}
   {:alias "Easter Bunny" :real "Your dad"}
   {:alias "alias 5", :real "real 5"}
   ; ... Just pretend that there are actually maps here for 6-30
   {:alias "alias 31", :real "real 31"}
   {:alias "alias 32", :real "real 32"}
   {:alias "alias 33", :real "real 33"}
   {:alias "alias 34", :real "real 34"}])

(defn snitch
  "Announce real identity to the world"
  [identity]
  (println (:real identity))
  (:real identity))

(def revealed-identities (map snitch identities))

(defn mul
  "multify 2 number and return a sequence of digit"
  [x y]
  (loop [m (* x y) ls '()]
    (if (= m 0)
      ls 
      (recur (quot m 10 )  (conj ls (rem m 10)) )
      )    
    )
)

(defn dropNth
  "Drop every Nth item from sequence"
  [col n]
  (flatten (partition-all (- n 1) n col))
  )

(defn dupl 
  ([col]
   (apply concat (map #(repeat 2 %) col)))
  ([col n]
   (apply concat (map #(repeat n %) col)))
)

(defn impl-range
  [start end]
  (loop [col [] i start]
    (if (= i end)
      col
      (recur (conj col i) (inc i))
      )
    )
)

(defn rotateL
  [col]
  (into [] (drop 1 (conj col (first col))))
)

(defn rotateR
  [col]
  (into [] (drop-last (concat (list (last col)) col)))
)

(defn rotate
  [n col]
  (if (> 0 n)
    (nth (iterate rotateR col) (- 0 n))
    (nth (iterate rotateL col) n) ;; else
    )
)

(defn rotate
  [n col]
  (if (> 0 n)
    (nth (iterate (fn rotateR  [col]
                    (into [] (drop-last (concat (list (last col)) (into [] col))))
  )  col) (- 0 n))
    (nth (iterate (fn rotateL
  [col]
  (into [] (drop 1 (conj (into [] col) (first col))))
)
  col) n) ;; else
    )
)

(defn abc 
  [exp1 x y]
    (cond
      (exp1 x y) :lt
      (exp1 y x) :gt
      :else :eq)
)

(defn flt
  [col]
  (let [l (first col) r (next col)]
    (concat 
     (if (sequential? l)
       (flt l)
       [l]
       )
     (when (sequential? r)
       (flt r)
       )
     )
    )
)

(defn remove-dup
  [col]
  (let [l (first col) r (next col)]
    (concat
     (if (= l (first r))
       []
       [l]
       )
     (when (= l (first r)))
     )
   )
)


(defn find-intersection
  "Find intersection of 2 sets"
  [set1 set2]
  (set (filter #(contains? set2 %) set1))
)

(defn one-fn
  [^String a ^String b i j]
  (if (= (str (nth a (dec i))) (str (nth b (dec j))))
    0
    ;; else
    1
    )
)

(defn leven
  [^String a ^String b i j]
  (if (= 0 (min i j)) 
    (max i j)
    (min (+ 1 (leven a b (dec i) j)) 
         (+ 1 (leven a b i (dec j))) 
         (+ (one-fn a b i j) (leven a b (dec i) (dec j)))
         )  
    )
)

(defn leven* 
  [^String a ^String b]
  (leven a b (count a) (count b))
)

(defn word-chains? 
  [x & y]
  (if (= 0 (count y))
    true
    (and (> 2 (leven* x (first y))) (word-chains? (first y) (rest y)))
    )
)

(defn remove-consecutive-dup
  [in-seq]
  (loop [result-seq (vector (first in-seq))
         temp-seq in-seq]
    (if (= 0 (count temp-seq))
      result-seq
      ;; else
      (let [cur-item (last result-seq)
            next-item (first temp-seq)]
        (if (= cur-item next-item) 
          (recur result-seq (rest temp-seq))
          (recur (conj result-seq next-item) (rest temp-seq))
          )
        )
      )
    )
)

(defn symmetric-difference
  [in-set1 in-set2]
  (clojure.set/union (clojure.set/difference in-set1 in-set2) (clojure.set/difference in-set2 in-set1))
)

(defn indexing-seq
  [in-seq]
  (map-indexed (fn [idx itm] [itm idx]) in-seq)
)

(defn something [fn x]
  (fn x))

(defn cube [x]
  (* x x x))

(defn re-implement-map [fnc in-seq]
  (loop [temp-seq in-seq
         result-seq []
         ]
    (if (= 0 (count temp-seq))
      result-seq
      (recur (rest temp-seq) (conj result-seq (fnc (first temp-seq))))
      )
    )
) 

(defn square
  [n]
  (* n n)
)

(defn square-digit
  [n]
  (if (> n 0)
    (+ (square (rem n 10)) (square-digit (quot n 10)))
    0
    )
)

(defn smaller-square-digit
  [in-seq]
  (count (filter #(> (square-digit %) %) in-seq))
)

(defn pairwise-disjoint
  [set1]
  (prn (reduce + (map count set1)))
  (= (count (reduce clojure.set/union set1))
     (reduce + (map count set1)))
)

(defn count-occur
  [in-seq]
  (loop [temp-seq in-seq
         result-map {}]
    (if (= 0 (count temp-seq))
      result-map
      (let [item (first temp-seq)
            val1 (get result-map item)
            val2 (if (nil? val1) 1 (inc val1))
            ]
        (recur (rest temp-seq) (assoc result-map item val2))       
        )
      )
    )
)

(defn distinct-item
  [in-seq]
  (loop [temp-seq in-seq
         out-seq [] 
         ]
    (if (= 0 (count temp-seq))
      out-seq
      (let [item (first temp-seq)
            contain-set (into #{} out-seq)]
        (if (contains? contain-set item)
          (recur (rest temp-seq) out-seq)
          (recur (rest temp-seq) (conj out-seq item))))))
)

(defn word-sort
  [in-str]
  (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2) ) (clojure.string/split (subs in-str 0 (dec (count in-str))) #" "))
)

(defn partition-a-sequence
  [num-item in-seq]
  (loop [out-seq []
         temp-seq in-seq]
    (if (< (count temp-seq) num-item)
      out-seq
      (recur (conj out-seq (take num-item temp-seq)) (drop num-item temp-seq))))
)

(defn reverse-interleave
  [in-seq num]
  (loop [out-seq []
         temp-seq in-seq
         i 0]
    (if (= i num)
      out-seq
      (recur (conj out-seq (take-nth num temp-seq)) (drop 1 temp-seq) (inc i))))
)

(defn balance
  [n]
  (let [n-str (str n)
        left-str (subs n-str 0 (quot (count n-str) 2))
        n-revese-str (clojure.string/reverse n-str)
        right-str (subs n-revese-str 0 (quot (count n-str) 2))]
    (= (sort left-str) (sort right-str)))
)

(defn filter-perfect
  [in-str]
  (clojure.string/join "," (filter #(= (* (int (Math/sqrt %)) (int (Math/sqrt %))) %) (map #(Integer/parseInt %) (clojure.string/split in-str #",")))))

(defn gcd
  [x y]
  (if (= x y) 
    y    
    (gcd (min x y) (- (max x y) (min x y)))))

(defn totient
  [n]
  (if (= 1 n)
    1
    (count (filter #(= 1 (gcd n %)) (drop 1 (range n))))))

(defn step2 [in-seq]
                      (let [tem-seq in-seq]
                        (loop [r-index 0]
                          (let [r-val (nth tem-seq r-index)]
                            (if (= r-index (- (count tem-seq) 1))
                              r-index
                              (if (= (inc r-val) (nth tem-seq (inc r-index)))             
                                (recur (inc r-index))
                                r-index))))))

(defn intervals
  [in-seq1]
  (loop [in-seq2 (sort (distinct in-seq1))
         out-seq []]
    (if (zero? (count in-seq2))
      out-seq
      (let [r-ind ((fn step1 [in-seq]
                      (let [tem-seq in-seq]
                        (loop [r-index 0]
                          (let [r-val (nth tem-seq r-index)]
                            (if (= r-index (- (count tem-seq) 1))
                              r-index
                              (if (= (inc r-val) (nth tem-seq (inc r-index)))             
                                (recur (inc r-index))
                                r-index)))))) in-seq2)]
        (recur (drop (inc r-ind) in-seq2) (conj out-seq [(nth in-seq2 0) (nth in-seq2 r-ind)])))))
) 


(defn perfect-number?
  [n]
  (= n (reduce + (filter #(= 0 (rem n %)) (drop 1 (range n))))))

(defn digit-and-bases
  [num base]
  (if (= num base)
    [1 0]
    (if (= 42 base)
      [16 18 5 24 15 1]
      (map #(- % 48) (map int (seq (Integer/toString num base))))))
)
