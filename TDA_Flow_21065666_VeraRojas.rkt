#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA Flow
;######################################################################################
; ID(int) X name-msg(str) x Option


;######################################################################################
;        Selectores
;######################################################################################





;######################################################################################
;        Modificadores
;######################################################################################

;add-unique-options
;Agrega uno o más option sin repetir un option ya agregado, compara el code, que puede verse como su id
(define add-unique-options (lambda (options)
    (cond ((null? options) options)
          (else (cons (car options) 
                (add-unique-options (filter (lambda (option) (not (equal-option-code? (car options) option))) options))))
    )))

(provide (all-defined-out))
