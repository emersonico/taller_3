#lang eopl
;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <un-programa (exp)>
;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;                  ::= <"\"" <texto> "\"">
;;                      <texto-lit (txt)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <(<expresion> <primitiva-binaria> <expresion>)>
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <primitiva-unaria> (<expresion>)>
;;                      <primapp-un-exp (prim-unaria exp)>
;; <primitiva-binaria> :=  + (primitiva-suma)
;;                     :=  ~ (primitiva-resta)
;;                     :=  / (primitiva-div)
;;                     :=  * (primitiva-multi)
;;                     :=  concat (primitiva-concat)
;;<primitiva-unaria>   :=  longitud (primitiva-longitud)
;;                     :=  add1 (primitiva-add1)
;;                     :=  sub1 (primitiva-sub1)
;******************************************************************************************

;=======================
; ESPECIFICACIÓN LÉXICA
;=======================

(define scanner-spec-simple-interpreter
  '((white-sp
      (whitespace) skip)

    (comentario
      ("//" (arbno (not #\newline))) skip)

    ;; Cadenas de texto entre comillas
    (string
      ("\"" (arbno (not #\")) "\"") string)

    ;; Identificadores que comienzan con @ (por ejemplo: @x, @edad)
    (identificador
      ("@" letter (arbno (or letter digit "-" "_"))) symbol)

    ;; Números enteros positivos
    (numero
      (digit (arbno digit)) number)

    ;; Números enteros negativos
    (numero
      ("-" digit (arbno digit)) number)

    ;; Números decimales positivos
    (numero
      (digit (arbno digit) "." digit (arbno digit)) number)

    ;; Números decimales negativos
    (numero
      ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))




;========================
; ESPECIFICACIÓN SINTÁCTICA
;========================

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)              ; Un programa consiste en una expresión

    (expresion (numero) numero-lit)                 ; Expresión numérica literal
    (expresion (string) texto-lit)                  ; Expresión de texto literal
    (expresion (identificador) var-exp)             ; Expresión de variable

    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp) ; Aplicación de primitiva binaria
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp) ; Aplicación de primitiva unaria

    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp) ;CONDICIONALES

    (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp) ; Esto permtite declarar una o varias varibales locales anres del cuerpo

    ;Declarar procedimientos
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc") procedimiento-exp) ;

    ;Evaluacion de procesimiendos
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)

    ;Recursividad
    (expresion ("funcionRec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) 
               "haga" expresion "finRec")
               recursivo-exp)

    
    (primitiva-binaria ("+") primitiva-suma)        ; Primitiva de suma
    (primitiva-binaria ("~") primitiva-resta)       ; Primitiva de resta
    (primitiva-binaria ("*") primitiva-multi)       ; Primitiva de multiplicación
    (primitiva-binaria ("/") primitiva-div)         ; Primitiva de división
    (primitiva-binaria ("concat") primitiva-concat) ; Primitiva de concatenación

    (primitiva-unaria ("longitud") primitiva-longitud) ; Primitiva de longitud de cadena
    (primitiva-unaria ("add1") primitiva-add1)      ; Primitiva de incremento
    (primitiva-unaria ("sub1") primitiva-sub1)      ; Primitiva de decremento
  ))

;========================
; CREACIÓN DE DATATYPES
;========================
; Genera automáticamente los tipos de datos para la sintaxis abstracta del lenguaje
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;========================
; PARSER Y SCANNER
;========================

; scan&parse: string -> programa
; Convierte una cadena de texto en un árbol de sintaxis abstracta del programa
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

; just-scan: string -> lista-de-tokens  
; Realiza solo el análisis léxico de una cadena, devolviendo una lista de tokens
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;========================
; AMBIENTE (ENVIRONMENT)
;========================

; define-datatype environment: Define la estructura de datos para los ambientes
(define-datatype environment environment?
  (empty-env)                                       ; Ambiente vacío

  (extended-env                                     ; Ambiente extendido
   (vars (list-of symbol?))                         ; Lista de identificadores
   (vals list?)                                     ; Lista de valores correspondientes
   (env environment?))                              ; Ambiente padre

  (recursively-extended-env                         ; Ambiente para funciones recursivas
   (proc-names (list-of symbol?))                   ; Nombres de los procedimientos
   (idss (list-of (list-of symbol?)))               ; Parámetros de cada procedimiento
   (bodies (list-of expresion?))                    ; Cuerpo de cada procedimiento
   (env environment?)))                             ; Ambiente base

; apply-env: environment × symbol -> value
; Busca el valor asociado a un identificador en el ambiente
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "Variable no definida: ~s" search-var))

      (extended-env (vars vals old-env)
        (let ((pos (list-find-position search-var vars)))
          (if (number? pos)
              (list-ref vals pos)
              (apply-env old-env search-var))))

      (recursively-extended-env (proc-names idss bodies old-env)
        (let ((pos (list-find-position search-var proc-names)))
          (if (number? pos)
              ;; devuelve una cerradura (función recursiva)
              (cerradura (list-ref idss pos)
                         (list-ref bodies pos)
                         env)
              (apply-env old-env search-var)))))))


; extend-env-recursively: crea un ambiente recursivo
(define extend-env-recursively
  (lambda (proc-names idss bodies env)
    (recursively-extended-env proc-names idss bodies env)))

; init-env: -> environment
; Crea el ambiente inicial con algunas variables predefinidas
(define init-env
  (lambda ()
    (extended-env
     '(@a @b @c @d @e)                             ; Identificadores requeridos por el ejercicio
     (list 1 2 3 "hola" "FLP")                     ; Valores correspondientes requeridos
     (empty-env))))                                 ; Ambiente base vacío


;; buscar-variable: symbol × environment → value
;; Busca un identificador en el ambiente y retorna su valor
;; Si no encuentra la variable, genera un error descriptivo
(define buscar-variable
  (lambda (id env)
    (apply-env env id)))

;========================
; Función auxiliar para evaluar listas de expresiones
;========================

(define eval-rands
  (lambda (rands env)
    (if (null? rands)
        '()
        (cons (eval-expresion (car rands) env)
              (eval-rands (cdr rands) env)))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else
       (let ((r (list-index pred (cdr ls))))
         (if (number? r) (+ r 1) #f))))))


;; ========================================
;; Tipo de dato para representar procedimientos (cerraduras)
;; ========================================
(define-datatype procVal procVal?
  (cerradura
    (lista-ID (list-of symbol?))   ; Parámetros
    (exp expresion?)               ; Cuerpo del procedimiento
    (amb environment?)))           ; Ambiente donde se definió


(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (params cuerpo env)
        (if (= (length params) (length args))
            (eval-expresion cuerpo (extended-env params args env))
            (eopl:error 'apply-procedure
                        "Número incorrecto de argumentos: esperaba ~s, recibió ~s"
                        (length params) (length args)))))))



;========================
; FUNCIÓN PRINCIPAL
;========================

; eval-program: programa -> value
; Función principal que evalúa un programa completo
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)                           ; Un programa tiene un cuerpo (expresión)
                   (eval-expresion body (init-env)))))) ; Evalúa la expresión en el ambiente inicial

;========================
; EVALUADOR DE EXPRESIONES
;========================

; eval-expresion: expresion × environment -> value
; Evalúa una expresión en un ambiente dado y retorna su valor
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)                        ; Los números se evalúan a sí mismos
      (texto-lit (txt) txt)                         ; Los textos se evalúan a sí mismos
      (var-exp (id)                                 ; Las variables se buscan en el ambiente
        (buscar-variable id env))
      
      (primapp-bin-exp (exp1 prim-binaria exp2)     ; Aplicación de primitiva binaria
        (let ((v1 (eval-expresion exp1 env))        ; Evalúa el primer operando
              (v2 (eval-expresion exp2 env)))       ; Evalúa el segundo operando
          (apply-primitiva-binaria prim-binaria v1 v2))) ; Aplica la primitiva
      
      (primapp-un-exp (prim-unaria exp1)            ; Aplicación de primitiva unaria
        (let ((v (eval-expresion exp1 env)))        ; Evalúa el operando
          (apply-primitiva-unaria prim-unaria v)))  ; Aplica la primitiva


      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (eval-expresion test-exp env)) ; Verifica la condición
            (eval-expresion true-exp env) ; Si es verdadera, evalúa la rama "entonces"
            (eval-expresion false-exp env))) ; Si es falsa, evalúa la rama "sino"

      (variableLocal-exp (ids exps cuerpo)
        (let ((args (eval-rands exps env))) ;evalua las variables locales
          (eval-expresion cuerpo 
                          (extended-env ids args env)))) ;crea un nuevo ambiente extendido y evalua el cuerpo

      ;declara procedimientos
      ;Esto no ejecuta el cuerpo, solo crea la cerradura (el valor del procedimiento).
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env))


      ;evaluacion de procedimiento
      (app-exp (rator rands)
        (let ((proc (eval-expresion rator env))
            (args (eval-rands rands env)))
         (if (procVal? proc)
            (apply-procedure proc args)
            (eopl:error 'eval-expresion
                    "Intento de aplicar algo que no es un procedimiento: ~s" proc))))


      ;expresión recursiva (letrec / funcionRec)
      (recursivo-exp (proc-names idss cuerpos letrec-body)
        (eval-expresion
          letrec-body
          (extend-env-recursively proc-names idss cuerpos env)))


      

      (else (eopl:error 'eval-expresion "Expresión no implementada: ~s" exp)))))


;========================
; PRIMITIVAS BINARIAS
;========================

; apply-primitiva-binaria: primitiva-binaria × value × value -> value
; Aplica una primitiva binaria a dos valores y retorna el resultado
(define apply-primitiva-binaria
  (lambda (prim a b)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ a b))                   ; Suma aritmética
      (primitiva-resta () (- a b))                  ; Resta aritmética
      (primitiva-multi () (* a b))                  ; Multiplicación aritmética
      (primitiva-div ()                             ; División aritmética con verificación
        (if (zero? b)
            (eopl:error 'apply-primitiva-binaria "División por cero")
            (/ a b)))
      (primitiva-concat ()                          ; Concatenación de cadenas
        (if (and (string? a) (string? b))
            (string-append a b)
            (eopl:error 'apply-primitiva-binaria "concat espera dos cadenas, recibió: ~s y ~s" a b))))))

;========================
; PRIMITIVAS UNARIAS
;========================

; apply-primitiva-unaria: primitiva-unaria × value -> value
; Aplica una primitiva unaria a un valor y retorna el resultado
(define apply-primitiva-unaria
  (lambda (prim v)
    (cases primitiva-unaria prim
      (primitiva-longitud ()                        ; Longitud de cadena
        (if (string? v)
            (string-length v)
            (eopl:error 'apply-primitiva-unaria "longitud espera una cadena, recibió: ~s" v)))
      (primitiva-add1 ()                            ; Incremento en 1
        (if (number? v)
            (+ v 1)
            (eopl:error 'apply-primitiva-unaria "add1 espera un número, recibió: ~s" v)))
      (primitiva-sub1 ()                            ; Decremento en 1
        (if (number? v)
            (- v 1)
            (eopl:error 'apply-primitiva-unaria "sub1 espera un número, recibió: ~s" v))))))


;========================
; BOOLEANOS 
;========================

(define valor-verdad?
  (lambda (val)
    (cond
      [(number? val) (not (zero? val))]    ; 0 → falso, otros números → verdadero
      [else #t])))                         ; No números → siempre verdadero

;========================
; INTERFAZ DEL INTÉRPRETE
;========================
; interpretador: -> void
; Inicia un bucle de lectura-evaluación-impresión (REPL) para el lenguaje
(define interpretador
  (sllgen:make-rep-loop
   "--> "                                           ; Prompt para el usuario
   (lambda (pgm) (eval-program pgm))               ; Función que evalúa el programa
   (sllgen:make-stream-parser                      ; Parser para entrada en stream
    scanner-spec-simple-interpreter
    grammar-simple-interpreter)))

;========================
; INICIAR INTÉRPRETE
;========================
(interpretador)