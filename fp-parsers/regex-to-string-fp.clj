(ns regex-parser.core
  [:use [clojure.set :only (union)]])

;; (use '[clojure.set :only (union)])

(defn syntax-error []
  (throw (ex-info "Syntax Error" {})))

(declare alt then star prime)

(defn expect [s c]
  (if (not= (first s) c)
    (syntax-error)
    (rest s)))

(defn alt [s]
  (let [[a s] (then s)]
    (if (= (first s) "|")
      (let [[b s*] (alt (rest s))]
        [(list :alt a b) s*])
      [a s])))

(defn then [s]
  (let [[c s] (star s)]
    (if (or (= (first s) "(")
            (re-matches #"\w" (first s)))
      (let [[d s*] (then s)]
        [(list :then c d) s*])
      [c s])))

(defn star [s]
  (let [[p s] (prime s)
        P     [p]]
    (if (= (first s) "*")
      [(cons :star P) (rest s)]
      [(first P) s])))

(defn prime [s]
  (if (re-matches #"\w" (first s))
    (do
      [(list :literal (first s)) (rest s)])
    (do
      (let [s*      (expect s "(")
            [r s**] (alt s*)
            s***    (expect s** ")")]
        [r s***]))))

(defn parse-regex [s]
  (let [s         (map str (lazy-cat s "\u0000"))
        [tree s*] (alt s)]
    (if-not (= (first s*) "\u0000")
      (syntax-error)
      tree)))

;; (parse-regex "a")
;; (parse-regex "abc")
;; (parse-regex "(ab)")
;; (parse-regex "(ab)*")
;; (parse-regex "(ab)*(c|d)*(e|f)")
;; (parse-regex "(ab|ba)*")

(def ε "\u0000")

(def ^:dynamic node-counter (atom -1))

(defn gen-node []
  (swap! node-counter inc)
  @node-counter)

(defn literal->nfa [re]
  (let [[_ c]   re
        initial (gen-node)
        final   (gen-node)
        states  #{initial final}
        edges   [[initial c final]]]
    [states edges initial final]))

(declare regex->nfa*)

(defn alt->nfa [re]
  (let [[_ a b]       re
        initial       (gen-node)
        [s1 e1 i1 f1] (regex->nfa* a)
        [s2 e2 i2 f2] (regex->nfa* b)
        final         (gen-node)
        states        (union #{initial final} s1 s2)
        edges*        [[initial ε i1]
                       [initial ε i2]
                       [f1 ε final]
                       [f2 ε final]]
        edges         (concat e1 e2 edges*)]
    [states edges initial final]))

(defn then->nfa [re]
  (let [[_ a b]       re
        [s1 e1 i1 f1] (regex->nfa* a)
        [s2 e2 i2 f2] (regex->nfa* b)
        initial       i1
        final         f2
        states        (union s1 s2)
        edges*        [[f1 ε i2]]
        edges         (concat e1 e2 edges*)]
    [states edges initial final]))

(defn star->nfa [re]
  (let [[_ a]         re
        initial       (gen-node)
        [s1 e1 i1 f1] (regex->nfa* a)
        final         (gen-node)
        states        (union #{initial final} s1)
        edges*        [[initial ε final]
                       [initial ε i1]
                       [f1      ε final]
                       [f1      ε i1]]
        edges         (concat e1 edges*)]
    [states edges initial final]))

(defn regex->nfa* [re]
  (case (first re)
    :literal (literal->nfa re)
    :alt     (alt->nfa     re)
    :then    (then->nfa    re)
    :star    (star->nfa    re)))

(defn optimize-nfa [nfa]
  (let [[states edges initial final] nfa
        n-states (count states)
        adj-mtx  (make-array String n-states n-states)]
    (doseq [[from consume to] edges]
      (aset adj-mtx from to consume))
    [states adj-mtx initial final]))

(defn regex->nfa [re]
  (binding [node-counter (atom -1)]
    (->> re
         (regex->nfa*)
         (optimize-nfa))))

;; (regex->nfa (parse-regex "ab"))
;; (regex->nfa (parse-regex "(ab|ba)*"))

;; (declare dfs*)

;; (defn dfs- [nfa current n]
;;   (let [[states edges initial final] nfa]
;;     (cond
;;      ;; n < 0 (fail)
;;      (neg? n) nil
;;      ;; at final state and n = 0 (success)
;;      (and (zero? n) (= current final)) ()
;;      :else
;;      ;; life should move on
;;      (let [outgoing (->> (nth edges current)
;;                          (map-indexed (fn [to c] [to c]))
;;                          (remove (fn [[to c]] (nil? c)))
;;                          #_(sort (fn [[_ c1] [_ c2]] (- (compare c1 c2)))))]
;;        (loop [edges outgoing]
;;          (if (empty? edges)
;;            nil
;;            (let [[to consume] (first edges)
;;                  S (if (= consume ε)
;;                      (dfs* nfa to n)
;;                      (dfs* nfa to (dec n)))]
;;              (if (nil? S)l
;;                (recur (rest edges))
;;                (cons consume S)))))))))

;; (def dfs* (memoize dfs-))

;; (defn dfs [n nfa]
;;   (let [[_ _ initial _] nfa]
;;     (dfs* nfa initial n)))

(defn bfs [n nfa]
  (let [[_ edges initial final] nfa]
    (loop [Q (conj clojure.lang.PersistentQueue/EMPTY [initial 0 ()])]
      (if (seq Q)
        (let [[current step path] (peek Q)
              Q  (pop Q)]
          (cond
           (> step n) (recur Q)
           (and (= step n) (= current final)) (reverse path)
           :else
           (let [outgoing (->> (nth edges current)
                               (map-indexed (fn [to c] [to c]))
                               (remove (fn [[to c]] (nil? c)))
                               (sort (fn [[_ c1] [_ c2]] (- (compare c1 c2)))))
                 Q (loop [edges outgoing, Q Q]
                     (if (empty? edges)
                       Q
                       (let [[to c] (first edges)]
                         (if (= c ε)
                           (recur (rest edges) (conj Q [to step path]))
                           (recur (rest edges) (conj Q [to (inc step) (cons c path)]))))))]
             ;;            (println (seq Q))
             (recur Q))))
        nil))))

;; (defn main [n regex-str]
;;   (->> regex-str
;;        (parse-regex)
;;        (regex->nfa)
;;        (dfs n)
;;        ((fn [S]
;;           (if (nil? S)
;;             "NIL"
;;             (apply str (remove #(= ε %) S)))))))

(defn main [n regex-str]
  (->> regex-str
       (parse-regex)
       (regex->nfa)
       (bfs n)
       ((fn [S]
          (if (nil? S)
            "NIL"
            (apply str S))))))

(println (main (Integer. (read-line)) (read-line)))

;; (main 5 "ab*(c|d)")
