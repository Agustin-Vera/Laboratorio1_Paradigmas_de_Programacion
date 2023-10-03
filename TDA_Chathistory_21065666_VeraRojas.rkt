#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")
(require "TDA_Chatbot_21065666_VeraRojas.rkt")
(require "TDA_User_21065666_VeraRojas.rkt")
(require "TDA_Flow_21065666_VeraRojas.rkt")
(require racket/date)

;######################################################################################
;        TDA ChatHistory
;######################################################################################
; user-name(str) X History(str)

;######################################################################################
;        Constructor
;######################################################################################

;Descripcion de la funcion: Crea un chatHistory 
;Dominio: user-name(string)
;Recorrido: chatHistory
;Tipo de recursion: N/A
(define chatHistory (lambda (user-name)
    (list user-name "")))


;Descripcion de la funcion: Crea un chatHistory 
;Dominio: user-name(string) X history(string)
;Recorrido: chatHistory
;Tipo de recursion: N/A
(define new-chatHistory (lambda (user-name history)
    (list user-name history)))


;######################################################################################
;        Selectores
;######################################################################################
(define get-chatHistory-user car)
(define get-chatHistory cadr)

;######################################################################################
;        Modificadores
;######################################################################################

;Descripcion de la funcion: Crea la nueva interaccion entre el user y un chatbot
;Dominio: user X user-message(string) X chatbot X flow-id(int)
;Recorrido: string
;Tipo de recursion: N/A
(define make-chat-message (lambda (user user-message chatbot flow-id)
    (string-append (make-user-message user user-message) (make-system-message chatbot flow-id))))


;Descripcion de la funcion: Crea el mensaje de entrada del usuario
;Dominio: user X message(string)
;Recorrido: string
;Tipo de recursion: N/A
(define make-user-message (lambda (user message)
    (cond ((string? message) (string-append make-date " - " (get-user-name user) ": " message "\n"))
          (else (string-append make-date " - " (get-user-name user) ": " (number->string message) "\n")))))


;Descripcion de la funcion: Crea el mensaje respuesta del chatbot
;Dominio: chatbot X flow-id(int)
;Recorrido: string
;Tipo de recursion: N/A
(define make-system-message (lambda (chatbot flow-id)
    (string-append make-date " - " (get-chatbot-name chatbot) ": " (get-flow-name-msg (get-flow-by-id-rec (get-chatbot-flows chatbot) flow-id)) "\n"
                   (options-to-string-rec (get-flow-options (get-flow-by-id-rec (get-chatbot-flows chatbot) flow-id))))))


;Descripcion de la funcion: Actuliza el chatHistory de un usuario, agregando una interacion nueva
;Dominio: user X user-message(string) X chatHistorys X chatbot X flow-id(int) chatbots
;Recorrido: chatHistors
;Tipo de recursion: Recursion de cola
;Documentación del cuerpo: Si el chatHisotry en cuestion posee el mismo user-name que el user de entrada (usuario iniciado)
;                               Se debe crear la interacion entre usuario y chatbots
;                               Dado el user-message se busca la option con el current-chatbot y el current-flow, teniendo la option se
;                               obtiene el chatbot y flow al que esa option apunta, con ello se puede crear la interaccion en make-chat-message
(define update-chatHistory (lambda (user user-message chatHistorys chatbot flow-id chatbots)
    (cond ((null? chatHistorys) chatHistorys)
          ((string-ci=? (get-user-name user) (get-chatHistory-user (car chatHistorys))) 
          (cons (new-chatHistory (get-user-name user) 
                                 (string-append (get-chatHistory (car chatHistorys)) 
                                                (make-chat-message user user-message (get-chatbot-by-id-rec chatbots (get-option-ChatbotCodeLink (get-option-by-message (get-flow-options (get-flow-by-id-rec (get-chatbot-flows chatbot) flow-id)) (transform-message user-message)))) 
                                                                   (get-option-InitialFlowCodeLink (get-option-by-message (get-flow-options (get-flow-by-id-rec (get-chatbot-flows chatbot) flow-id))  (transform-message user-message) )))))
                (update-chatHistory user user-message (cdr chatHistorys) chatbot flow-id chatbots)))
          (else (cons (car chatHistorys) (update-chatHistory user user-message (cdr chatHistorys) chatbot flow-id chatbots))))))


;Descripcion de la funcion: Actuliza el chatHistory de un usuario, agregando una interacion nueva, el chatbot repite la respuesta anteorior debido a una entrada no valida
;                           No valida hace referencia que se ingreso un code inexistente o una keyword inexistente
;Dominio: user X user-message(string) X chatHistorys X chatbot X flow-id(int) chatbots
;Recorrido: string
;Tipo de recursion: Recursion de cola
;Documentación del cuerpo: Si el chatHisotry en cuestion posee el mismo user-name que el user de entrada (usuario iniciado)
;                          Repite el mensaje del current-chatbot y current-flow
(define repeat-message-chatHistory (lambda (user user-message chatHistorys chatbot flow-id) 
    (cond ((null? chatHistorys) chatHistorys)
          ((string-ci=? (get-user-name user) (get-chatHistory-user (car chatHistorys))) 
          (cons (new-chatHistory (get-user-name user)
                                 (string-append (get-chatHistory (car chatHistorys))
                                                (make-chat-message user user-message chatbot flow-id)))
                (repeat-message-chatHistory user user-message (cdr chatHistorys) chatbot flow-id)))
          (else (cons (car chatHistorys) (repeat-message-chatHistory user user-message (cdr chatHistorys) chatbot flow-id))))))

;######################################################################################
;        Otras funciones
;######################################################################################

;Descripcion de la funcion: Le da el formato dia/mes/anio al string de la fecha 
;Dominio: day(string) X month(string) X year(string)
;Recorrido: string
;Tipo de recursion: N/A
(define date-format (lambda (day month year)
    (string-append day "/" month "/" year)))


;Descripcion de la funcion: Obtiene la fecha del dia de hoy
;Dominio: N/A
;Recorrido: string
;Tipo de recursion: N/A
(define make-date
  (date-format (number->string (date-day (current-date)))
               (number->string (date-month (current-date)))
               (number->string (date-year (current-date)))))


;Descripcion de la funcion: Transforma un mensaje en formato string a numero
;Dominio: user-message(string)
;Recorrido: int or string
;Tipo de recursion: N/A
 (define transform-message (lambda (user-message)
    (if (number? (string->number user-message))
        (string->number user-message)
        user-message))) 


(provide (all-defined-out))