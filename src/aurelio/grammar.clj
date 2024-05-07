(ns aurelio.grammar)

(defn build-char-p
  "Returns a predicate function that compares given character
  `c1` against any other character."
  [^Character c1]
  (fn [^Character c2]
    (= (int c1) (int c2))))

(defn build-str-p
  "Returns a predicate function that compares given string
  `s1` agains any other string."
  [^String s1]
  (fn [^String s2]
    (= s1 s2)))

(defn build-or-p
  "Returns a predicate function that tests given list of
  `predicates` against any expression."
  ([]
   (throw (Exception. "`or` predicate has no inner predicates.")))
  ([& predicates]
   (fn [expr]
     ((complement not-any?) #(% expr) predicates))))

(defn build-range-p
  "Returns a predicate function that tests whether any other
  character exists inbetween `start` and `end`.

  `NOTE`: Currently, Aurelio does not support UTF-8 or any other
  encoding format, only ASCII. Please open a PR incase you need
  that working ASAP."
  [start end]
  (fn [c]
    (<= (int start) (int c) (int end))))

(defn build-seq*-p
  "Returns a predicate function that tests given `predicate` against
  a sequence of expressions that `MAY` be null or empty."
  [predicate]
  (fn [exprs]
    (every? #(predicate %) (filter some? (seq exprs)))))

(defn build-seq+-p
  "Returns a predicate function that tests given `predicate` against
  a sequence of expressions that `CANNOT` be `nil` or empty."
  [predicate]
  (fn [exprs]
    (and (some? (seq exprs)) ((build-seq*-p predicate) exprs))))

(defn build-opt-p
  "Returns a predicate function that tests given `predicate` against
  any expression, as long as it is not `nil`."
  [predicate]
  (fn [& expr]
    (or (empty? expr) (predicate (first expr)))))

;; TODO: improve this, too weak
(defn build-useq-p
  "Returns a predicate function that tests if all non-optional expressions
  match uniquely (independent from order) each given `predicate`."
  ([]
   (throw (Exception. "`useq` predicate has no predicates.")))
  ([& predicates]
   (fn [exprs]
     ;; compare non-optionals with predicate count
     (if (not= (count (remove #(when (seq? %)
                                 (not= (first %) :opt))
                              exprs))
               (count predicates))
       (throw (Exception. "expression count != predicate count"))
       (let [preds-or-p (apply build-or-p predicates)]
         (map #(preds-or-p %) exprs))))))

;; ((build-useq-p (build-char-p \c) (build-char-p \t)) [\t \t])

(def predicate-builders
  {:or build-or-p
   :range build-range-p
   :seq* build-seq*-p
   :seq+ build-seq+-p
   :opt build-seq*-p
   :useq build-useq-p})

(defn- sort-sym-refs
  "Given a list of `expressions` (the vals for a grammar table),
  separates the symbol references and reordes them by reference count.

  A symbol reference is a `keyword` that is not a builtin predicate, a
  a reference to a symbol created by the user.

  `NOTE`: This algorithm should be refactored in the future. We're currently
  ignoring circular dependencies. Also, it looks complex :(
  "
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

(comment
  (sort-sym-refs [[:a :b [:or :d :d]]
                  [:b [:seq+ :d :d]]])
  ;; => {:d 4, :b 2, :a 1}
  )

(defn order-syms-by-dep
  "Given a `grammar`, reorders its symbols (i.e. `keywords`) based
  on their reference count by other symbol's expressions.

  See also: `sort-sym-refs`"
  [grammar]
  (let [sym-refs (sort-sym-refs (vals grammar))]
    (->> grammar
         (sort-by #(get sym-refs (key %) 0) >)
         (into {}))))

(defn build-expr-p
  "Builds a list of predicates, corresponding to each expression in given `expressions`."
  [expr]
  (if (empty? expr)
    (throw (Exception. "an empty expression is invalid."))
    (let [pred-keyword (first expr)
          pred-builder (get predicate-builders pred-keyword)]
      (if (and (keyword? pred-keyword) (some? pred-builder))
        (try
          (->> (build-expr-p (rest expr))
               (apply pred-builder))
          (catch Exception e
            (throw (Exception. (str "failed to build predicate `"
                                    pred-keyword "`\n"
                                    (.getMessage e))))))
        (map
         (fn [inner-expr]
           (cond-> inner-expr
             (char? inner-expr) build-char-p
             (string? inner-expr) build-str-p
             (keyword? inner-expr) identity
             (vector? inner-expr) build-expr-p))
         expr)))))

(def base-grammar
  {:ws [\space]
   :nl [\n]
   :empty [""]})

(defn build
  [user-grammar]
  (let [grammar (order-syms-by-dep (merge base-grammar user-grammar))]
    (reduce-kv
     (fn [gr sym expr]
       (assoc gr sym (build-expr-p expr)))
     {} grammar)))
