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