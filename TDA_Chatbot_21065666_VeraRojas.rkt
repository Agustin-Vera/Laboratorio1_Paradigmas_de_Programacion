#lang racket

(require "TDA_Flow_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA ChatBot
;######################################################################################
; id(int) X name(str) X welcomeMessage(str) startFlowID(int) X flows

;######################################################################################
;        Constructor
;######################################################################################
(define new-chatbot (lambda (id name welcomeMessage startFlowID flows)
    (list id name welcomeMessage startFlowID flows)))


;######################################################################################
;        Pertenencias
;######################################################################################

(define equal-chatbot-id? (lambda (chatbot new-chatbot)
    (cond ((null? chatbot) #f)
          ((= (get-chatbot-id chatbot) (get-chatbot-id new-chatbot)) #t)
          (else #f))))



;######################################################################################
;        Selectores
;######################################################################################
(define get-chatbot-id car)
(define get-chatbot-name cadr)
(define get-chatbot-welcomeMsg caddr)
(define get-chatbot-startFlowID cadddr)
(define get-chatbot-flows (lambda (flow) (cadddr (cdr flow))))

;No recursivo
(define get-chatbot-by-id (lambda (chatbots id)
    (car (filter (lambda (chatbot) (equal? (get-chatbot-id chatbot) id)) chatbots))))


;######################################################################################
;        Modificadores
;######################################################################################

;Recursion de cola
(define add-flow-to-flows (lambda (flow-list flow)
    (if (null? flow-list)
        (list flow)
        (cons (car flow-list) (add-flow-to-flows (cdr flow-list) flow)))))


;Agrega los flujos que sean unicos, no se repiten, en base a su id
(define add-unique-flows (lambda (flows)
    (cond ((null? flows) flows)
          (else (cons (car flows) 
                (add-unique-flows (filter (lambda (flow) (not (equal-flow-id? (car flows) flow))) flows))))
    )))



(provide (all-defined-out))