#lang racket

(require "TDA_Chatbot_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA System
;######################################################################################
; name(str) X initialChatbotCodeLink(int) X Chatbots X Users X chatHistorys X currentChatbotID(int) 
; X currentFlowID(int) X date(string)




;######################################################################################
;        Selectores
;######################################################################################

(define get-initial-flow-id-by-initialChatbotCodeLink (lambda (chatbots chatbot-id)
    (get-chatbot-startFlowID (get-chatbot-by-id chatbots chatbot-id))))


;######################################################################################
;        Modificadores
;######################################################################################

(define add-unique-chatbots (lambda (chatbots)
    (cond ((null? chatbots) chatbots)
          (else (cons (car chatbots) 
                (add-unique-chatbots (filter (lambda (chatbot) (not (equal-chatbot-id? (car chatbots) chatbot))) chatbots))))
    )))




(provide (all-defined-out))