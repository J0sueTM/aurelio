(ns aurelio.grammar)

(defn build-char-p
  [^Character c1]
  (fn [^Character c2]
    (= (int c1) (int c2))))

(defn build-string-p
  [^String s1]
  (fn [^String s2]
    (= s1 s2)))

(defn build-or-p
  [& predicates]
  (if (empty? predicates)
    (throw (ex-message "`or` predicate has no inner predicates."))
    (fn [v]
      (some #(% v) predicates))))

(defn build-range-p
  [start end]
  (fn [c]
    (<= (int start) (int c) (int end))))

(defn build-seq*-p
  [predicate]
  (fn [vals]
    (every? #(predicate %) vals)))

(def predicate-builders
  {:or build-or-p
   :range build-range-p
   :seq* build-seq*-p
   :seq+ build-seq*-p
   :opt build-seq*-p})

(defn sort-sym-refs
  [exprs]
  (reduce
   (fn [acc expr]
     (->> (flatten expr)
          ;; retrieve symbol references
          (filter #(and (keyword? %) (nil? (get predicate-builders %))))
          ;; count symrefs by parent
          (reduce #(update-in %1 [%2] (fnil inc 0)) {})
          ;; count all unique symrefs
          (reduce-kv
           (fn [sym-acc wsym wsym-count]
             (update-in sym-acc [wsym] (fnil (partial + wsym-count) 0)))
           acc)
          (sort-by val >)
          (into {})))
   {} exprs))

(defn order-syms-by-dep
  [grammar]
  (let [sym-refs (sort-sym-refs (vals grammar))]
    (->> (sort-by
          #(get sym-refs (key %) 0) >
          grammar)
         (into {}))))

(comment
  (order-syms-by-dep
   {:a [:or [:seq+ :d] :e]
    :b [\c [:seq* :e] :d :d]
    :e [\a]
    :d [\n]})
  ;; => {:d [\n], :e [\a], :a [:or [:seq+ :d] :e], :b [\c [:seq* :e] :d :d]}
  )

;; (defn build-vec
;;   [id expr]
;;   (if-let [?keyword (first expr)]
;;     (if-let [?op-builder (get predicate-builders ?keyword)]
;;       #(apply (apply ?op-builder (second expr)) %)
;;       ?keyword)
;;     (throw (ex-message (str "identifier `" id
;;                             "` has an empty vector as expression")))))
;;  
;; (defn build-identifier
;;   [id expr grammar]
;;   (if-let [built-id (get-in grammar id)]
;;     built-id
;;     (->> (cond
;;            (char? expr) (build-char-p expr)
;;            (vector? expr) (id build-vec expr))
;;          (assoc grammar id))))

(def base-grammar
  {:ws [\space]
   :nl [\n]
   :digit [:range \0 \9]
   :number [[:seq+ :digit]
            [:opt \.]
            [:opt [:seq+ :digit]]]})

(defn build
  [user-grammar]
  (let [grammar (order-syms-by-dep (merge base-grammar user-grammar))]
    ;; TODO: implement remaining grammar builders
    (identity grammar)))
