#lang racket

(require "TDA_Flow_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA ChatBot
;######################################################################################
; id(int) X name(str) X welcomeMessage(str) startFlowID(int) X flows



;######################################################################################
;        Modificadores
;######################################################################################

;Agrega los flujos que sean unicos, no se repiten, en base a su id
(define add-unique-flows (lambda (flows)
    (cond ((null? flows) flows)
          (else (cons (car flows) 
                (add-unique-flows (filter (lambda (flow) (not (equal-flow-id? (car flows) flow))) flows))))
    )))

(provide (all-defined-out))