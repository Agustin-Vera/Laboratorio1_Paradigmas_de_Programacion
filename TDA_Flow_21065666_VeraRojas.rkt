#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA Flow
;######################################################################################
; ID(int) X name-msg(str) x Option


;######################################################################################
;        Constructor
;######################################################################################
;
(define new-flow (lambda (id name-msg option)
    (list id name-msg option)))


;######################################################################################
;        Selectores
;######################################################################################

(define get-flow-id car)
(define get-flow-name-msg cadr)
(define get-flow-options caddr)


;######################################################################################
;        Modificadores
;######################################################################################

;Retorna la lista de options con la opcion agregada a esta
(define add-option-to-options (lambda (flow option)
    (cons option (get-flow-options flow))))


;add-unique-options
;Agrega uno o más option sin repetir un option ya agregado, compara el code, que puede verse como su id
(define add-unique-options (lambda (options)
    (cond ((null? options) options)
          (else (cons (car options) 
                (add-unique-options (filter (lambda (option) (not (equal-option-code? (car options) option))) options))))
    )))




(provide (all-defined-out))
