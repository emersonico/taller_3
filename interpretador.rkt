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