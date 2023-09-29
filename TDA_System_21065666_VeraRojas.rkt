#lang racket

(require "TDA_Chatbot_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA System
;######################################################################################
; name(str) X initialChatbotCodeLink(int) X Chatbots X Users X chatHistorys X currentChatbotID(int) 
; X currentFlowID(int) X date(string)

;######################################################################################
;        Constructor
;######################################################################################
;
(define new-system (lambda (name initialChatbotCodeLink chatbots users chatHistory currentChatbotID currentFlowID date)
    (list name initialChatbotCodeLink chatbots users chatHistory currentChatbotID currentFlowID date)))


;######################################################################################
;        Selectores
;######################################################################################
;
(define get-system-name car)
(define get-system-initial-chatbot-code-link cadr)
(define get-system-chatbots caddr)
(define get-system-users cadddr)
(define get-system-chat-history (lambda (system) (cadddr (cdr system))))
(define get-system-current-chatbotID (lambda (system) (cadddr (cdr (cdr system)))))
(define get-system-current-flowID (lambda (system) (cadddr (cdr (cdr (cdr system))))))
(define get-system-date (lambda (system) (cadddr (cdr (cdr (cdr (cdr system)))))))



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

;Retorna la lista de options con la opcion agregada a esta
(define add-chatbot-to-chatbots (lambda (system chatbot)
    (cons chatbot (get-system-chatbots system))))

;######################################################################################
;        Otras funciones
;######################################################################################

;Introduce el current flowID en caso de ser el chatbot inicial
(define add-system-current-flow-id (lambda (system chatbot)
    (cond ((= (get-chatbot-id chatbot) (get-system-initial-chatbot-code-link system)) (get-chatbot-startFlowID chatbot))
          (else (get-system-current-flowID system)))))


(provide (all-defined-out))