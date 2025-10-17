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
  '((white-sp (whitespace) skip)                    ; Omite espacios en blanco
    (comentario ("//" (arbno (not #\newline))) skip) ; Omite comentarios de una línea
    ;; texto entre comillas dobles: "hola mundo"
    (string ("\"" (arbno (not #\")) "\"") string)   ; Reconoce cadenas entre comillas
    ;; identificadores que comienzan con @
    (identificador ("@" (arbno (or letter digit "-" "_"))) symbol) ; Reconoce variables que empiezan con @
    ;; números: enteros y decimales, positivos o negativos
    (numero ("-"? digit (arbno digit) ("." digit (arbno digit))?) number) ; Reconoce números con/sin signo y decimales
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
   (vars list?)                                     ; Lista de identificadores
   (vals list?)                                     ; Lista de valores correspondientes
   (env environment?)))                             ; Ambiente padre

; apply-env: environment × symbol -> value
; Busca el valor asociado a un identificador en el ambiente
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()                                 ; Si el ambiente está vacío
        (eopl:error 'apply-env "Variable no definida: ~s" search-var))
      (extended-env (vars vals old-env)             ; Si el ambiente está extendido
        (letrec 
            ((loop (lambda (vars vals)              ; Función auxiliar para buscar en las listas
                     (cond
                       ((null? vars)                ; Si no se encuentra en este nivel
                        (apply-env old-env search-var)) ; Buscar en el ambiente padre
                       ((eqv? (car vars) search-var) ; Si se encuentra la variable
                        (car vals))
                       (else                        ; Continuar buscando
                        (loop (cdr vars) (cdr vals)))))))
          (loop vars vals))))))

; init-env: -> environment
; Crea el ambiente inicial con algunas variables predefinidas
(define init-env
  (lambda ()
    (extended-env
     '(@x @y @z @mensaje @pi)                       ; Identificadores predefinidos
     (list 10 20 30 "Hola mundo" 3.1416)           ; Valores correspondientes
     (empty-env))))                                 ; Ambiente base vacío


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
        (apply-env env id))
      
      (primapp-bin-exp (exp1 prim-binaria exp2)     ; Aplicación de primitiva binaria
        (let ((v1 (eval-expresion exp1 env))        ; Evalúa el primer operando
              (v2 (eval-expresion exp2 env)))       ; Evalúa el segundo operando
          (apply-primitiva-binaria prim-binaria v1 v2))) ; Aplica la primitiva
      
      (primapp-un-exp (prim-unaria exp1)            ; Aplicación de primitiva unaria
        (let ((v (eval-expresion exp1 env)))        ; Evalúa el operando
          (apply-primitiva-unaria prim-unaria v)))  ; Aplica la primitiva
      (else (eopl:error 'eval-expresion "Expresión no implementada: ~s" exp)))))
