#lang racket

(require "TDA_Flow_21065666_VeraRojas.rkt")
(require "TDA_Option_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA ChatBot
;######################################################################################
; id(int) X name(str) X welcomeMessage(str) X startFlowID(int) X flows

;######################################################################################
;        Constructor
;######################################################################################

;Descripcion de la funcion: Crea un chatbot 
;Dominio: id(int) X name(str) X welcomeMessage(str) X startFlowID(int) X flows
;Recorrido: chatbot
;Tipo de recursion: N/A
(define new-chatbot (lambda (id name welcomeMessage startFlowID flows)
    (list id name welcomeMessage startFlowID flows)))


;######################################################################################
;        Pertenencias
;######################################################################################

;Descripcion de la funcion: Verica si un chatbot es igual a otro comparando sus ID
;Dominio: chatbot X new-chatbot(chatbot)
;Recorrido: boolean
;Tipo de recursion: N/A
(define equal-chatbot-id? (lambda (chatbot new-chatbot)
    (cond ((null? chatbot) #f)
          ((= (get-chatbot-id chatbot) (get-chatbot-id new-chatbot)) #t)
          (else #f))))


;Descripcion de la funcion: Verifica si un chatbot nuevo existe dentro de una lista de chatbots
;Dominio: chatbots X new-chatbot(chatbot)
;Recorrido: boolean
;Tipo de recursion: N/A
(define chatbot-exist? (lambda (chatbots new-chatbot) 
    (if (equal? (null? (filter (lambda (x) (equal? x #t)) 
                        (map (lambda (chatbot) (equal-chatbot-id? chatbot new-chatbot)) chatbots))) #f)
        #t
        #f)))


;######################################################################################
;        Selectores
;######################################################################################

;Obtienen un elemento dado un chatbot
;Poseen nombres representativos
;Dominio: chatbot
;Recorrido: lo que su nombre indique
;Tipo de recursion: N/A
(define get-chatbot-id car)
(define get-chatbot-name cadr)
(define get-chatbot-welcomeMsg caddr)
(define get-chatbot-startFlowID cadddr)
(define get-chatbot-flows (lambda (flow) (cadddr (cdr flow))))


;Descripcion de la funcion: Obtiene un chatbot dado su ID desde una lista de chatbots 
;Dominio: chatbots X id(int)
;Recorrido: chatbot
;Tipo de recursion: N/A
(define get-chatbot-by-id (lambda (chatbots id)
    (car (filter (lambda (chatbot) (equal? (get-chatbot-id chatbot) id)) chatbots))))


;Descripcion de la funcion: Obtiene un chatbot dado su ID desde una lista de chatbots 
;Dominio: chatbots X id(int)
;Recorrido: chatbot
;Tipo de recursion: Recursion de cola
(define get-chatbot-by-id-rec (lambda (chatbots id)
    (cond ((null? chatbots) chatbots)
          ((equal? (get-chatbot-id (car chatbots)) id) (car chatbots))
          (else (get-chatbot-by-id (cdr chatbots) id)))))


;Descripcion de la funcion: Obtiene un chatbot dado un message
;Dominio: chatbots X current-chartbotID(int) X current-flowID(int) X message(string)
;Recorrido: chatbot
;Tipo de recursion: Recursion de cola
(define get-chatbot-by-message (lambda (chatbots current-chartbotID current-flowID message)
    (get-chatbot-by-id chatbots
                       (get-option-ChatbotCodeLink (get-option-by-message-norec (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id chatbots current-chartbotID)) current-flowID)) message)))))

;######################################################################################
;        Modificadores
;######################################################################################

;Descripcion de la funcion: Agrega un flow a una lista de flows 
;Dominio: flow-list X flow
;Recorrido: flow-list
;Tipo de recursion: Recursion de cola
(define add-flow-to-flows (lambda (flow-list flow)
    (if (null? flow-list)
        (list flow)
        (cons (car flow-list) (add-flow-to-flows (cdr flow-list) flow)))))


;Descripcion de la funcion: Agrega los flujos que sean unicos, no se repiten, en base a su ID (agrega una ocurrencia de cada flow)
;Dominio: flows
;Recorrido: flows
;Tipo de recursion: N/A 
(define add-unique-flows (lambda (flows)
    (cond ((null? flows) flows)
          (else (cons (car flows) 
                (add-unique-flows (filter (lambda (flow) (not (equal-flow-id? (car flows) flow))) flows)))))))


(provide (all-defined-out))