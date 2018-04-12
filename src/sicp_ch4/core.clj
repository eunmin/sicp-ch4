(ns sicp-ch4.core
  (:refer-clojure :exclude [eval apply true? false?]))

(def apply-in-underlying-clojure clojure.core/apply)

(declare eval)
(declare self-evaluating?)
(declare variable?)
(declare lookup-variable-value)
(declare quoted?)
(declare text-of-quotation)
(declare assignment?)
(declare eval-assingment)
(declare definition?)
(declare eval-definition)
(declare if?)
(declare eval-if)
(declare lambda?)
(declare make-procedure)
(declare lambda-parameters)
(declare lambda-body)
(declare begin?)
(declare eval-sequence)
(declare begin-actions)
(declare cond?)
(declare cond->if)
(declare application?)
(declare apply)
(declare operator)
(declare list-of-values)
(declare operands)
(declare primitive-procedure?)
(declare apply-primitive-procedure)
(declare compound-procedure?)
(declare procedure-body)
(declare extend-environment)
(declare procedure-paramenters)
(declare procedure-environment)
(declare no-operands?)
(declare first-operand)
(declare rest-operands)
(declare if-predicate)
(declare if-consequent)
(declare if-alternative)
(declare last-exp?)
(declare first-exp)
(declare rest-exps)
(declare set-variable-value!)
(declare assignment-variable)
(declare assignment-value)
(declare define-variable!)
(declare definition-value)
(declare definition-variable)
(declare tagged-list?)
(declare make-lambda)
(declare make-begin)
(declare cond-predicate)
(declare expand-clauses)
(declare primitive-procedure-names)
(declare primitive-procedure-objects)
(declare true?)

(defn eval [exp env]
  (cond
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assingment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env)
    (begin? exp) (eval-sequence (begin-actions exp) env)
    (cond? exp) (eval (cond->if exp) env)
    (application? exp) (apply (eval (operator exp) env)
                              (list-of-values (operands exp) env))
    :else (throw (ex-info "Unknown expression type -- EVAL" {:exp exp}))))

(defn apply [proc args]
  (cond
    (primitive-procedure? proc) (apply-primitive-procedure proc args)
    (compound-procedure? proc) (eval-sequence (procedure-body proc)
                                              (extend-environment (procedure-paramenters proc)
                                                                  args
                                                                  (procedure-environment proc)))
    :else (throw (ex-info "Unknown procedure type -- APPLY" {:proc proc}))))

(defn list-of-values [exps env]
  (if (no-operands? exps)
    []
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn eval-if [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
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

(defn self-evaluating? [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation [exp]
  (second exp))

(defn tagged-list? [exp tag]
  (if (list? exp)
    (= (first exp) tag)
    false))

(defn variable? [exp]
  (symbol? exp))

(defn assignment? [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (second exp))

(defn assignment-value [exp]
  (nth exp 2))

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

;; lambda

(defn lambda? [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (second exp))

(defn lambda-body [exp]
  (rest (rest exp)))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))

;; if

(defn if? [exp]
  (tagged-list? exp 'if))

(defn if-predicate [exp]
  (second exp))

(defn if-consequent [exp]
  (nth exp 2))

(defn if-alternative [exp]
  (if-not (nil? (rest (rest (rest exp))))
    (first (rest (rest (rest exp))))
    'false))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;; begin

(defn begin? [exp]
  (tagged-list? exp 'begin))

(defn begin-actions [exp]
  (rest exp))

(defn last-exp? [seq]
  (nil? (rest seq)))

(defn first-exp [seq]
  (first seq))

(defn rest-exps [seq]
  (rest seq))

(defn sequence->exp [seq]
  (cond (nil? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn make-begin [seq]
  (cons 'begin seq))

;; application

(defn application? [exp]
  (list? exp))

(defn operator [exp]
  (first exp))

(defn operands [exp]
  (rest exp))

(defn no-operands? [ops]
  (nil? ops))

(defn first-operand [ops]
  (first ops))

(defn rest-operands [ops]
  (rest ops))

;; cond

(defn cond? [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses [exp]
  (rest exp))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn cond-predicate [clause]
  (first clause))

(defn cond-actions [clause]
  (rest clause))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

(defn expand-clauses [clauses]
  (if (nil? clauses)
    'false
    (let [first (first clauses)
          rst (rest clauses)]
      (if (cond-else-clause? first)
        (if (nil? rst)
          (sequence->exp (cond-actions first))
          (throw (ex-info "ELSE clause isn't last -- COND->IF"
                          {:clauses clauses})))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rst))))))

(defn true? [x]
  (not (= x false)))

(defn false? [x]
  (= x false))

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

;; environment
;; '({a 1 b 2} {a 2} {c 3})

(defn enclosing-environment [env]
  (rest env))

(defn first-frame [env]
  (first env))

(def the-empty-environment '())

(defn make-frame [variables values]
  (apply hash-map (interleave variables values)))

(defn frame-variables [frame]
  (keys frame))

(defn frame-values [frame]
  (vals frame))

(defn add-binding-to-frame! [var val frame]
  (assoc frame var val))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (throw (ex-info "Too many arguments supplied" {:vars vars :vals vals}))
      (throw (ex-info "Too few arguments supplied" {:vars vars :vals vals})))))

(defn lookup-variable-value [var env]
  (get (clojure.core/apply merge (reverse @env)) var))

(defn set-variable-value! [var val env]
  ()
  #_(define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  #_(env-loop env))

(defn define-variable! [var val env]
  #_(let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (add-binding-to-frame! var val frame))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))

(defn setup-environment []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

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
        (list 'cons cons)
        (list 'null? nil?)))

(def primitive-procedure-names (map first primitive-procedures))

(def primitive-procedure-objects (map #(list 'primitive (second %)) primitive-procedures))

(defn apply-primitive-procedure [proc args]
  (apply-in-underlying-clojure (primitive-implementation proc) args))
