(ns aurelio.grammar-test
  (:require
   [aurelio.grammar :as g]
   [clojure.test :as t]))

(t/deftest build-char-p-test
  [(t/is ((g/build-char-p \c) \c))
   (t/is (not ((g/build-char-p \c) \t)))])

(t/deftest build-str-p-test
  [(t/is ((g/build-str-p "hello") "hello"))
   (t/is (not ((g/build-str-p "hello") "bye")))])

(t/deftest build-or-p-test
  (let [or-ct-p (g/build-or-p
                 (g/build-char-p \c)
                 (g/build-char-p \t))]
    [(t/is (or-ct-p \c))
     (t/is (or-ct-p \t))
     (t/is (not (or-ct-p \v)))]))

(t/deftest build-range-p-test
  (let [digit-p (g/build-range-p \0 \9)
        lowercase-p (g/build-range-p \a \z)]
    [(t/is (digit-p \1))
     (t/is (not (digit-p \a)))
     (t/is (lowercase-p \t))
     (t/is (not (lowercase-p \9)))]
    (let [alphanum-p (g/build-or-p
                      digit-p
                      lowercase-p
                      (g/build-range-p \A \Z))]
      [(t/is (alphanum-p \t))
       (t/is (alphanum-p \U))
       (t/is (alphanum-p \7))
       (t/is (not (alphanum-p \newline)))])))

(t/deftest order-syms-by-dep-test
  (t/are [x y] (= (g/order-syms-by-dep x) y)

    {:a [:b :b]
     :b [\a]}
    {:b [\a]
     :a [:b :b]}

    {:a [:or [:seq+ :d] :e]
     :b [\c [:seq* :e] :d :d]
     :e [\a]
     :d [\n]}
    {:d [\n]
     :e [\a]
     :a [:or [:seq+ :d] :e]
     :b [\c [:seq* :e] :d :d]}))

(t/deftest build-expr-p-test
  (t/testing "char expr"
    (let [[space-p c-p] (g/build-expr-p [\space \c])]
      [(t/is (space-p \space))
       (t/is (c-p \c))]))
  (t/testing "keyword expr"
    (t/is (= (g/build-expr-p [:hello :world])
             '(:hello :world))))
  (t/testing "`or` expr"
    (let [[or-p n-p] (g/build-expr-p [[:or \a \b \c] \n])]
      [(t/is (or-p \a))
       (t/is (or-p \b))
       (t/is (or-p \c))
       (t/is (not (or-p \n)))
       (t/is (n-p \n))
       (t/is (not (n-p \a)))])))

(t/deftest build-seq-p-test
  (let [alphachar-p (g/build-or-p
                     (g/build-range-p \a \z)
                     (g/build-range-p \A \Z))]
    (t/testing "seq*"
      (let [alphastr*-p (g/build-seq*-p alphachar-p)]
        [(t/is (alphastr*-p []))
         (t/is (alphastr*-p nil))
         (t/is (alphastr*-p [\h \e \l \l \o]))
         (t/is (not (alphastr*-p [\h \3 \l \l \o])))]))
    (t/testing "seq+"
      (let [alphastr+-p (g/build-seq+-p alphachar-p)]
        [(t/is (alphastr+-p [\h \e \l \l \o]))
         (t/is (not (alphastr+-p [\3])))]))))

(t/deftest build-opt-p-test
  (let [c-p (g/build-opt-p (g/build-char-p \c))]
    [(t/is (c-p))
     (t/is (c-p \c))
     (t/is (not (c-p \t)))]))

;; TODO
#_(t/deftest build
    (t/testing "empty user grammar"
      (t/is (= (g/build {})
               {:digit fn?
                :char fn?
                :ws '(fn?)
                :nl '(fn?)
                :string fn?
                :number '(fn? fn? fn?)}))))
