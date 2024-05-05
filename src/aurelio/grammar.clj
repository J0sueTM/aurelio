(ns aurelio.grammar)

(defn build-char-p
  "Returns a predicate function that can compare given character
  `c1` against any other character."
  [^Character c1]
  (fn [^Character c2]
    (= (int c1) (int c2))))

^:rct/test
(comment
  ((build-char-p \c) \c)
  ;; => true

  ((build-char-p \c) \t)
  ;; => false
  )

(defn build-or-p
  "Returns a predicate function that runs given list of
  `predicates` against any other value, returning true if
  any of them returns true as well."
  ([]
   (throw (Exception. "`or` predicate has no inner predicates.")))
  ([& predicates]
   (fn [v]
     ((complement not-any?) #(% v) predicates))))

^:rct/test
(comment
  ((build-or-p
    (build-char-p \c)
    (build-char-p \t))
   \c)
  ;; => true

  ((build-or-p
    (build-char-p \c)
    (build-char-p \t))
   \v)
  ;; => false

  (try
    (build-or-p)
    (catch Exception e
      (.getMessage e)))
  ;; => "`or` predicate has no inner predicates."
  )

(defn build-range-p
  "Returns a predicate function that tests whether any other
  character exists inbetween `start` and `end`.

  `NOTE`: Currently, Aurelio does not support UTF-8 or any other
  encoding format, only ASCII. Please open a PR incase you need
  that working ASAP."
  [start end]
  (fn [c]
    (<= (int start) (int c) (int end))))

^:rct/test
(comment
  ((build-range-p \a \z) \n)
  ;; => true

  ((build-range-p \0 \9) \3)
  ;; => true

  ((build-range-p \a \z) \5)
  ;; => false
  )

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

(defn- sort-sym-refs
  "Given a list of `expressions` (the vals for a grammar table),
  separates the symbol references and reordes them by reference count.

  A symbol reference is a `keyword` that is not a builtin predicate, a
  a reference to a symbol created by the user.

  `NOTE`: This algorithm should be refactored in the future. We're currently
  ignoring circular dependencies. Also, it looks complex :("
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

^:rct/test
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

^:rct/test
(comment
  (order-syms-by-dep
   {:a [:or [:seq+ :d] :e]
    :b [\c [:seq* :e] :d :d]
    :e [\a]
    :d [\n]})
  ;; => {:d [\n], :e [\a], :a [:or [:seq+ :d] :e], :b [\c [:seq* :e] :d :d]}
  )

(defn build-expr-p
  "Builds a list of predicates, corresponding to each expression in given `expressions`."
  [expr]
  (if (empty? expr)
    (throw (Exception. "an empty expression is invalid."))
    (let [pred-keyword (first expr)
          pred-builder (get predicate-builders pred-keyword)]
      (if (and (keyword? pred-keyword) (some? pred-builder))
        (try
          (->> (rest expr)
               build-expr-p
               (apply pred-builder))
          (catch Exception e
            (throw (Exception. (str "failed to build predicate `" pred-keyword "`\n"
                                    (.getMessage e))))))
        (map
         (fn [inner-expr]
           (cond-> inner-expr
             (char? inner-expr) build-char-p
             (keyword? inner-expr) identity
             (vector? inner-expr) build-expr-p))
         expr)))))

^:rct/test
(comment
  (let [[space-p c-p] (build-expr-p [\space \c])]
    [(space-p \space)
     (space-p \c)
     (c-p \c)])
  ;; => [true false true]

  (build-expr-p [:hello :world])
  ;; => (:hello :world)

  (let [c-or-t-p (build-expr-p [:or \c \t])]
    [(c-or-t-p \c)
     (c-or-t-p \t)
     (c-or-t-p \n)])
  ;; => [true true false]

  (let [[or-abc-p n-p] (build-expr-p [[:or \a \b \c] \n])]
    [(or-abc-p \a)
     (or-abc-p \b)
     (or-abc-p \c)
     (or-abc-p \n)
     (n-p \n)
     (n-p \a)])
  ;; => [true true true false true false]

  (try
    (build-expr-p [])
    (catch Exception e
      (.getMessage e)))
  ;; => "an empty expression is invalid."
  )

(def base-grammar
  {:ws [\space]
   :nl [\n]
   :char [:or
          [:range \a \z]
          [:range \A \Z]]
   :string [:seq+ :char]
   :digit [:range \0 \9]
   :number [[:seq+ :digit]
            [:opt [\. [:seq+ :digit]]]]})

(defn build
  [user-grammar]
  (let [grammar (order-syms-by-dep (merge base-grammar user-grammar))]
    (reduce-kv
     (fn [gr sym expr]
       (assoc gr sym (build-expr-p expr)))
     {} grammar)))

^:rct/test
(comment
  (build {})
  ;; =>>
  {:digit fn?
   :char fn?
   :ws '(fn?)
   :nl '(fn?)
   :string fn?
   :number '(fn? fn? fn?)}
  ;;
  )
