#lang racket

(require "TDA_Chatbot_21065666_VeraRojas.rkt")
(require "TDA_User_21065666_VeraRojas.rkt")
(require "TDA_Chathistory_21065666_VeraRojas.rkt")
(require "TDA_Flow_21065666_VeraRojas.rkt")
(require "TDA_Option_21065666_VeraRojas.rkt")


;######################################################################################
;        TDA System
;######################################################################################
; name(str) X initialChatbotCodeLink(int) X Chatbots X Users X chatHistorys X currentChatbotID(int) X currentFlowID(int) X date(string)

;######################################################################################
;        Constructor
;######################################################################################

;Descripcion de la funcion: Crea un system
;Dominio: name(str) X initialChatbotCodeLink(int) X chatbots X users X chatHistorys X currentChatbotID(int) X currentFlowID(int) X date(string)
;Recorrido: system
;Tipo de recursion: N/A
(define new-system (lambda (name initialChatbotCodeLink chatbots users chatHistorys currentChatbotID currentFlowID date)
    (list name initialChatbotCodeLink chatbots users chatHistorys currentChatbotID currentFlowID date)))


;######################################################################################
;        Pertenencias
;######################################################################################

;Descripcion de la funcion: Verifica si existe algun usuario iniciado
;Dominio: system
;Recorrido: bool
;Tipo de recursion: N/A 
(define logged-users? (lambda (system) 
    (logged-user? (get-system-users system))))


;######################################################################################
;        Selectores
;######################################################################################

;Obtienen un elemento dado un system
;Poseen nombres representativos
;Dominio: system
;Recorrido: lo que su nombre indique
;Tipo de recursion: N/A
(define get-system-name car)
(define get-system-initial-chatbot-code-link cadr)
(define get-system-chatbots caddr)
(define get-system-users cadddr)
(define get-system-chat-history (lambda (system) (cadddr (cdr system))))
(define get-system-current-chatbotID (lambda (system) (cadddr (cdr (cdr system)))))
(define get-system-current-flowID (lambda (system) (cadddr (cdr (cdr (cdr system))))))
(define get-system-date (lambda (system) (cadddr (cdr (cdr (cdr (cdr system)))))))


;Descripcion de la funcion: Obtiene el flowID inicial de un chatbot dado su ID
;Dominio: chatbots X chatbot-id(int)
;Recorrido: int
;Tipo de recursion: N/A
(define get-initial-flow-id-by-initialChatbotCodeLink (lambda (chatbots chatbot-id)
    (get-chatbot-startFlowID (get-chatbot-by-id chatbots chatbot-id))))


;Descripcion de la funcion: Obtiene al usuario iniciado en el system
;Dominio: system
;Recorrido: user
;Tipo de recursion: N/A 
(define get-system-user-logged (lambda (system)
    (get-logged-user (get-system-users system))))

;######################################################################################
;        Modificadores
;######################################################################################

;Descripcion de la funcion: Dada una lista de chatbots guarda solo las primeras ocurrencias de estos, comparando sus ID
;Dominio: chatbots
;Recorrido: chatbots
;Tipo de recursion: Recursion de cola
(define add-unique-chatbots (lambda (chatbots)
    (cond ((null? chatbots) chatbots)
          (else (cons (car chatbots) 
                (add-unique-chatbots (filter (lambda (chatbot) (not (equal-chatbot-id? (car chatbots) chatbot))) chatbots)))))))


;Descripcion de la funcion: Agrega un chatbot a una lista de chatbots
;Dominio: system X chatbot
;Recorrido: chatbots
;Tipo de recursion: N/A
(define add-chatbot-to-chatbots (lambda (system chatbot)
    (cons chatbot (get-system-chatbots system))))


;Descripcion de la funcion: Agrega un chatHistory a una lista de chatHistorys
;Dominio: user(string) X chatHistory-list
;Recorrido: chatHistory-list
;Tipo de recursion: N/A
(define add-chatHistory-to-system (lambda (user chatHistory-list)
    (cons (chatHistory user) chatHistory-list)))


;Descripcion de la funcion: Agrega un usuario a una lista de usuarios
;Dominio: system X user
;Recorrido: users
;Tipo de recursion: N/A
(define add-user-to-users (lambda (system user)
    (cons user (get-system-users system))))


;Descripcion de la funcion: Cierra sesion del usuario iniciado
;Dominio: users
;Recorrido: users
;Tipo de recursion: Recursion de cola 
(define logout-user (lambda (users)
    (cond ((null? users) users)
          (else (cons (logout (car users)) (logout-user (cdr users)))))))


;Descripcion de la funcion: Actualiza el current-flowID del system al startFlowID del chatbot inicial
;Dominio: system 
;Recorrido: int
;Tipo de recursion: N/A 
(define set-system-startFlowID (lambda (system)
    (cond ((null? (get-system-chatbots system)) (get-system-current-flowID system))
          (else (get-chatbot-startFlowID (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system)))))))


;Descripcion de la funcion: Obtiene el ID del chatbot al cual apunta la simulacion (la entrada del usuario y el link de la option)
;Dominio: system X user-message(string) 
;Recorrido: int
;Tipo de recursion: N/A 
(define change-system-current-chatbot-id (lambda (system user-message)
    (if (message-is-valid? (get-flow-options (get-flow-by-id-rec (get-chatbot-flows (get-chatbot-by-id-rec (get-system-chatbots system) (get-system-current-chatbotID system)))
                                            (get-system-current-flowID system))) user-message)
        (get-option-ChatbotCodeLink (get-option-by-message 
                                    (get-flow-options (get-flow-by-id-rec (get-chatbot-flows 
                                    (get-chatbot-by-id-rec (get-system-chatbots system) (get-system-current-chatbotID system))) (get-system-current-flowID system))) (transform-message user-message)))
        (get-system-current-chatbotID system))))


;Descripcion de la funcion: Obtiene el ID del flow al cual apunta la simulacion (la entrada del usuario y el link de la option)
;Dominio: system X user-message(string) 
;Recorrido: int
;Tipo de recursion: N/A 
(define change-system-current-flow-id (lambda (system user-message)
    (if (message-is-valid? (get-flow-options (get-flow-by-id-rec (get-chatbot-flows (get-chatbot-by-id-rec (get-system-chatbots system) (get-system-current-chatbotID system)))
                                            (get-system-current-flowID system))) user-message)
        (get-option-InitialFlowCodeLink (get-option-by-message 
                                        (get-flow-options (get-flow-by-id-rec (get-chatbot-flows 
                                        (get-chatbot-by-id-rec (get-system-chatbots system) (get-system-current-chatbotID system))) (get-system-current-flowID system))) (transform-message user-message)))
        (get-system-current-flowID system))))

;######################################################################################
;        Otras funciones
;######################################################################################

;Descripcion de la funcion: Obtiene el current flowID en caso de ser el chatbot inicial
;Dominio: system X chatbot
;Recorrido: int
;Tipo de recursion: N/A 
(define add-system-current-flow-id (lambda (system chatbot)
    (cond ((= (get-chatbot-id chatbot) (get-system-initial-chatbot-code-link system)) (get-chatbot-startFlowID chatbot))
          (else (get-system-current-flowID system)))))


;Descripcion de la funcion: Agrega una interaccion de un usuario con un chatbot al chatHistory de un usuario
;Dominio: system X message(string) X current-chatbot-id(int) X current-flow-id(int)
;Recorrido: chatHistorys
;Tipo de recursion: Recursion de cola
(define new-interaction-rec (lambda (system message current-chatbot-id current-flow-id)
    (if (message-is-valid? (get-flow-options (get-flow-by-id-rec (get-chatbot-flows (get-chatbot-by-id-rec (get-system-chatbots system) current-chatbot-id))
                           current-flow-id)) message)

            (update-chatHistory (get-logged-user (get-system-users system)) message
                                (get-system-chat-history system)
                                (get-chatbot-by-id-rec (get-system-chatbots system) current-chatbot-id)
                                current-flow-id
                                (get-system-chatbots system))
                                
            (repeat-message-chatHistory (get-logged-user (get-system-users system)) message
                                        (get-system-chat-history system)
                                        (get-chatbot-by-id-rec (get-system-chatbots system) current-chatbot-id)
                                        current-flow-id))))


;Descripcion de la funcion: Agrega una interaccion de un usuario con un chatbot al chatHistory de un usuario
;Dominio: system X message(string)
;Recorrido: chatHistorys
;Tipo de recursion: N/A
(define new-interaction-norec (lambda (system message)
    (if (message-is-valid? (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system)))
                           (get-system-current-flowID system))) message)
        
        (update-chatHistory-norec (get-system-chat-history system) (get-user-name (get-logged-user (get-system-users system))) 
                                  (make-chat-message (get-logged-user (get-system-users system)) message 
                                                     (get-chatbot-by-message (get-system-chatbots system) (get-system-current-chatbotID system) (get-system-current-flowID system) message)
                                                     (get-option-InitialFlowCodeLink (get-option-by-message-norec (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system))) (get-system-current-flowID system))) message)))) 
                                                     
        (repeat-chatHistory-norec (get-system-user-logged system) (get-system-chat-history system)
                                  (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system))
                                  (get-system-current-flowID system)
                                  message))))


;Descripcion de la funcion: Cambia el current chatbotID dado la option-code o keyword (user-message) ingresado, obtiene el chatbotCodeLink de la option
;Dominio: system X user-message(string)
;Recorrido: int
;Tipo de recursion: N/A
(define change-system-current-chatbot-id-norec (lambda (system user-message)
    (if (message-is-valid? (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system)))
                           (get-system-current-flowID system))) user-message)
        
        (get-option-ChatbotCodeLink (get-option-by-message-norec (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system))) 
                                                                                                   (get-system-current-flowID system))) user-message))
        (get-system-current-chatbotID system))))


;Descripcion de la funcion: Cambia el current flowID dado la option-code o keyword (user-message) ingresado, obtiene el InitialFlowCodeLink de la option
;Dominio: system X user-message(string)
;Recorrido: int
;Tipo de recursion: N/A
(define change-system-current-flow-id-norec (lambda (system user-message)
    (if (message-is-valid? (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system)))
                           (get-system-current-flowID system))) user-message)
        
        (get-option-InitialFlowCodeLink (get-option-by-message-norec (get-flow-options (get-flow-by-id (get-chatbot-flows (get-chatbot-by-id (get-system-chatbots system) (get-system-current-chatbotID system))) 
                                                                                                   (get-system-current-flowID system))) user-message))
        (get-system-current-flowID system))))

(provide (all-defined-out))