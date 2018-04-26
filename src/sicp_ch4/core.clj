;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns sicp-ch4.core
  (:gen-class)
  (:refer-clojure :exclude [eval apply true? false?]))

(def apply-in-underlying-clojure clojure.core/apply)

(declare eval)

(defn true? [x]
  (not (= x false)))

(defn false? [x]
  (= x false))

(defn tagged-list? [exp tag]
  (if (seq? exp)
    (= (first exp) tag)
    false))

(defn self-evaluating? [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation [exp]
  (second exp))

(defn variable? [exp]
  (symbol? exp))

(defn assignment? [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (second exp))

(defn assignment-value [exp]
  (nth exp 2))

;; procedure

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))

(defn compound-procedure? [p]
  (tagged-list? p 'procedure))

(defn procedure-parameters [p]
  (second p))

(defn procedure-body [p]
  (nth p 2))

(defn procedure-environment [p]
  (nth p 3))

;; lambda

(defn lambda? [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (second exp))

(defn lambda-body [exp]
  (rest (rest exp)))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))

;; procedure

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation [proc]
  (second proc))

(def primitive-procedures
  (list (list 'car first)
        (list 'cdr rest)
        (list '+ +)
        (list '- -)
        (list 'mod mod)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'reverse reverse)
        (list 'cons cons)
        (list 'null? nil?)))

(def primitive-procedure-names (map first primitive-procedures))

(def primitive-procedure-objects (map #(list 'primitive (second %)) primitive-procedures))

(defn apply-primitive-procedure [proc args]
  (apply-in-underlying-clojure (primitive-implementation proc) args))

;; environment
;; '({a 1 b 2} {a 2} {c 3})

(defn enclosing-environment [env]
  (rest env))

(defn first-frame [env]
  (first env))

(def the-empty-environment (atom '()))

(defn make-frame [variables values]
  (clojure.core/apply hash-map (interleave variables values)))

(defn frame-variables [frame]
  (keys frame))

(defn frame-values [frame]
  (vals frame))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (atom (cons (make-frame vars vals) @base-env))
    (if (< (count vars) (count vals))
      (throw (ex-info "Too many arguments supplied" {:vars vars :vals vals}))
      (throw (ex-info "Too few arguments supplied" {:vars vars :vals vals})))))

(defn lookup-variable-value [var env]
  (if-let [rst (get (clojure.core/apply merge (reverse @env)) var)]
    rst
    (throw (ex-info "Unbound variable" {:var var}))))

(defn set-variable-value! [var val env]
  (swap! env #(loop [env %
                     rs '()]
                (if (seq env)
                  (let [current-frame (first-frame env)]
                    (if (contains? current-frame var)
                      (concat rs (list (assoc current-frame var val)) (rest env))
                      (recur (enclosing-environment env) (concat rs (list current-frame)))))
                  rs))))

(defn define-variable! [var val env]
  (swap! env #(clojure.core/apply list (assoc-in (vec %) [0 var] val))))

(defn setup-environment []
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; application

(defn application? [exp]
  (seq? exp))

(defn operator [exp]
  (first exp))

(defn operands [exp]
  (rest exp))

(defn first-operand [ops]
  (first ops))

(defn rest-operands [ops]
  (rest ops))

(defn no-operands? [ops]
  (empty? ops))

;; if

(defn if? [exp]
  (tagged-list? exp 'if))

(defn if-predicate [exp]
  (second exp))

(defn if-consequent [exp]
  (nth exp 2))

(defn if-alternative [exp]
  (if-not (empty? (rest (rest (rest exp))))
    (first (rest (rest (rest exp))))
    'false))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;; define

(defn definition? [exp]
  (tagged-list? exp 'define))

(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (first (second exp))))

(defn definition-value [exp]
  (if (symbol? (second exp))
    (nth exp 2)
    (make-lambda (rest (second exp))
                 (rest (rest exp)))))

;; cond

(defn cond? [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses [exp]
  (rest exp))

(defn cond-predicate [clause]
  (first clause))
(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn cond-actions [clause]
  (rest clause))

;; begin

(defn begin? [exp]
  (tagged-list? exp 'begin))

(defn begin-actions [exp]
  (rest exp))

(defn last-exp? [seq]
  (empty? (rest seq)))

(defn first-exp [seq]
  (first seq))

(defn rest-exps [seq]
  (rest seq))

(defn make-begin [seq]
  (cons 'begin seq))

(defn sequence->exp [seq]
  (cond (empty? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false
    (let [first (first clauses)
          rst (rest clauses)]
      (if (cond-else-clause? first)
        (if (empty? rst)
          (sequence->exp (cond-actions first))
          (throw (ex-info "ELSE clause isn't last -- COND->IF"
                          {:clauses clauses})))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rst))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

(defn list-of-values [exps env]
  (println "list of values:" exps)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn eval-if [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
  (println "eval-seq:" exps)
  (cond (last-exp? exps) (eval (first-exp exps) env)
        :else (do (eval (first-exp exps) env)
                  (eval-sequence (rest-exps exps) env))))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(defn apply [proc args]
  (println "apply:" (take 2 proc))
  (cond
    (primitive-procedure? proc) (apply-primitive-procedure proc args)
    (compound-procedure? proc) (eval-sequence (procedure-body proc)
                                              (extend-environment (procedure-parameters proc)
                                                                  args
                                                                  (procedure-environment proc)))
    :else (throw (ex-info "Unknown procedure type -- APPLY" {:proc proc}))))

(declare analyze)

(defn analyze-self-evaluating [exp]
  (println "analyze self-evaluating:" exp)
  (fn [env]
    exp))

(defn analyze-quoted [exp]
  (println "analyze qouted:" exp)
  (let [qval (text-of-quotation exp)]
    (fn [env]
      qval)))

(defn analyze-variable [exp]
  (println "analyze variable:" exp)
  (fn [env]
    (lookup-variable-value exp env)))

(defn analyze-assignment [exp]
  (println "analyze assignment:" exp)
  (let [var (assignment-variable exp)
        vproc (analyze (assignment-value exp))]
    (fn [env]
      (set-variable-value! var (vproc env) env)
      'ok)))

(defn analyze-definition [exp]
  (println "analyze definition:" exp)
  (let [var (definition-variable exp)
        vproc (analyze (definition-value exp))]
    (fn [env]
      (define-variable! var (vproc env) env)
      'ok)))

(defn analyze-if [exp]
  (println "analyze if:" exp)
  (let [pproc (analyze (if-predicate exp))
        cproc (analyze (if-consequent exp))
        aproc (analyze (if-alternative exp))]
    (fn [env]
      (if (true? (pproc env))
        (cproc env)
        (aproc env)))))

(defn analyze-sequence [exps]
  (println "analyze sequence:" exps)
  (let [sequentially (fn [proc1 proc2]
                       (fn [env]
                         (proc1 env)
                         (proc2 env)))
        procs (map analyze exps)]
    (if (empty? procs)
      (throw (ex-info "Empty sequence -- ANALYZE" {}))
      (loop [first-proc (first procs)
             rest-procs (rest procs)]
        (if (empty? rest-procs)
          first-proc
          (recur (sequentially first-proc (first rest-procs))
                 (rest rest-procs)))))))

(defn analyze-lambda [exp]
  (println "analyze lambda:" exp)
  (let [vars (lambda-parameters exp)
        bproc (analyze-sequence (lambda-body exp))]
    (fn [env]
      (make-procedure vars bproc env))))

(defn execute-application [proc args]
  (cond (primitive-procedure? proc) (apply-primitive-procedure proc args)
        (compound-procedure? proc) ((procedure-body proc)
                                    (extend-environment (procedure-parameters proc)
                                                        args
                                                        (procedure-environment proc)))
        :else (ex-info "Unknown procedure type -- EXECUTE-APPLICATION" {:proc proc})))

(defn analyze-application [exp]
  (println "analyze application:" exp)
  (let [fproc (analyze (operator exp))
        aprocs (map analyze (operands exp))]
    (fn [env]
      (execute-application (fproc env) (map #(% env) aprocs)))))

(defn analyze [exp]
  (cond
    (self-evaluating? exp) (analyze-self-evaluating exp)
    (quoted? exp) (analyze-quoted exp)
    (variable? exp) (analyze-variable exp)
    (assignment? exp) (analyze-assignment exp)
    (definition? exp) (analyze-definition exp)
    (if? exp) (analyze-if exp)
    (lambda? exp) (analyze-lambda exp)
    (begin? exp) (analyze-sequence (begin-actions exp))
    (cond? exp) (do
                  (println "analyze cond:" exp)
                  (analyze (cond->if exp)))
    (application? exp) (analyze-application exp)
    :else (throw (ex-info "Unknown expression type -- ANALYZE" {:exp exp}))))

(defn eval [exp env]
  ((analyze exp) env))

(def env (setup-environment))

(def code '(begin
            (define (fizzbuzz i result)
              (if (< i 11)
                (fizzbuzz (+ i 1) (cons (cond
                                          ((= (mod i 3) 0) "Fizz")
                                          ((= (mod i 5) 0) "Buzz")
                                          (else i)) result))
                (reverse result)))
            (fizzbuzz 1 (quote ()))))

(defn -main []
  (set! *print-length* 30)
  (println (eval code env)))
