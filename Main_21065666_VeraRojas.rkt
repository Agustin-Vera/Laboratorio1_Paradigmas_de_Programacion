#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")
(require "TDA_Flow_21065666_VeraRojas.rkt")
(require "TDA_Chatbot_21065666_VeraRojas.rkt")
(require "TDA_System_21065666_VeraRojas.rkt")
(require "TDA_User_21065666_VeraRojas.rkt")
(require "TDA_Chathistory_21065666_VeraRojas.rkt")

;######################################################################################
;        RFN - TDA Algo - Algo
;######################################################################################
;Descripcion de la funcion: 
;Dominio: 
;Recorrido: 
;Tipo de recursion: 


;######################################################################################
;        RF2 - TDA Option - Constructor
;######################################################################################
;Descripcion de la funcion: Crea un TDA option
;Dominio: code(int) X message(string) X ChatbotCodeLink(int) X FlowCodeLink(int) X Keyword*
;Recorrido: option
;Tipo de recursion: N/A
(define option (lambda (code message ChatbotCodeLink InitialFlowCodeLink  . Keyword)
    (list code message ChatbotCodeLink InitialFlowCodeLink  (transformar-a-minusculas Keyword))))


;######################################################################################
;        RF3 - TDA Flow - Constructor
;######################################################################################
;Descripcion de la funcion: Crea un TDA flow
;Dominio: name(string) X option*
;Recorrido: flow
;Tipo de recursion: N/A
(define flow (lambda (id name-msg . option)
    (list id name-msg (add-unique-options option))))


;######################################################################################
;        RF4 - TDA Flow - Modificador
;######################################################################################
;Descripcion de la funcion: Agrega una opcion a un flujo
;Dominio: flow X option
;Recorrido: flow
;Tipo de recursion: N/A 
(define flow-add-option (lambda (flow option)
    (if (not (option-exist? (get-flow-options flow) option))
        (new-flow (get-flow-id flow)
                  (get-flow-name-msg flow)
                  (add-option-to-options flow option))
        flow)))  


;######################################################################################
;        RF5 - TDA Chatbot - Constructor
;######################################################################################
;Descripcion de la funcion: Crea un TDA chatbot
;Dominio: chatbotID(int) X name(string) X welcomeMessage(string) X flows
;Recorrido: chatbot
;Tipo de recursion: N/A
(define chatbot (lambda (chatbotID name welcomeMessage startFlowID . flows)
    (list chatbotID name welcomeMessage startFlowID (add-unique-flows flows))))


;######################################################################################
;        RF6 - TDA Chatbot - Modificador
;######################################################################################
;Descripcion de la funcion: Agrega un unico flujo a un chatbot 
;Dominio: chatbot X flow
;Recorrido: chatbot
;Tipo de recursion: Recursion de cola
(define chatbot-add-flow (lambda (chatbot flow)
    (if (not (flow-exist? (get-chatbot-flows chatbot) flow))
        (new-chatbot (get-chatbot-id chatbot)
                     (get-chatbot-name chatbot)
                     (get-chatbot-welcomeMsg chatbot)
                     (get-chatbot-startFlowID chatbot)
                     (add-flow-to-flows (get-chatbot-flows chatbot) flow))
        chatbot)))
    

;######################################################################################
;        RF7 - TDA System - Constructor
;######################################################################################
;Descripcion de la funcion: Crea un TDA system
;Dominio: name(string) X initialChatbotCodeLink(int) X chatbot*
;Recorrido: System
;Tipo de recursion: N/A
(define system (lambda (name initialChatbotCodeLink . chatbot)
    (cond ((null? chatbot) (list name initialChatbotCodeLink (add-unique-chatbots chatbot) null null
                                 initialChatbotCodeLink null make-date))
          (else (list name initialChatbotCodeLink (add-unique-chatbots chatbot) null null 
                      initialChatbotCodeLink
                      (get-initial-flow-id-by-initialChatbotCodeLink chatbot initialChatbotCodeLink)
                      make-date)))))


;######################################################################################
;        RF8 - TDA System - Modificador
;######################################################################################
;Descripcion de la funcion: Agrega un chatbot un system 
;Dominio: system X chatbot
;Recorrido: system
;Tipo de recursion: N/A
(define system-add-chatbot (lambda (system chatbot)
    (if (not (chatbot-exist? (get-system-chatbots system) chatbot))
        (new-system (get-system-name system)
                    (get-system-initial-chatbot-code-link system)
                    (add-chatbot-to-chatbots system chatbot)
                    (get-system-users system)
                    (get-system-chat-history system)
                    (get-system-current-chatbotID system)
                    (add-system-current-flow-id system chatbot)
                    (get-system-date system))
        system)))


;######################################################################################
;        RF9 - TDA System - Modificador
;######################################################################################
;Descripcion de la funcion: Agrega un usuario a un system
;Dominio: system X user(string)
;Recorrido: system
;Tipo de recursion: N/A
(define system-add-user (lambda (system user)
    (if (not (user-exist? (get-system-users system) user))
        (new-system (get-system-name system)
                    (get-system-initial-chatbot-code-link system)
                    (get-system-chatbots system)
                    (add-user-to-users system (new-user user #f))
                    (add-chatHistory-to-system user (get-system-chat-history system))
                    (get-system-current-chatbotID system)
                    (get-system-current-flowID system)
                    (get-system-date system))
        system)))


;######################################################################################
;        RF10 - TDA System
;######################################################################################
;Descripcion de la funcion: Inicia la sesion de un usuario existente dentro de un system
;Dominio: system X user(string)
;Recorrido: system
;Tipo de recursion: N/A 
(define system-login (lambda (system user)
    (if (and (user-exist? (get-system-users system) user) (not (logged-users? system)))
        (new-system (get-system-name system)
                    (get-system-initial-chatbot-code-link system)
                    (get-system-chatbots system)
                    (login-user (get-system-users system) user)
                    (get-system-chat-history system)
                    (get-system-current-chatbotID system)
                    (get-system-current-flowID system)
                    (get-system-date system))
       system)))


;######################################################################################
;        RF11 - TDA System
;######################################################################################
;Descripcion de la funcion: Cierra sesion del usuario iniciado
;Dominio: system
;Recorrido: system
;Tipo de recursion: N/A 
(define system-logout (lambda (system)
    (if (logged-users? system) 
        (new-system (get-system-name system)
                    (get-system-initial-chatbot-code-link system)
                    (get-system-chatbots system)
                    (logout-user (get-system-users system))
                    (get-system-chat-history system)
                    (get-system-initial-chatbot-code-link system)
                    (set-system-startFlowID system)
                    (get-system-date system))
        system)))


;######################################################################################
;        RF12
;######################################################################################
;Descripcion de la funcion: Permite a un usuario interactuar con un chatbot
;Dominio: system X message(string)
;Recorrido: system
;Tipo de recursion: Recursion de cola 
(define system-talk-rec (lambda (system message)
    (if (logged-users? system)
        (new-system (get-system-name system)
                    (get-system-initial-chatbot-code-link system)
                    (get-system-chatbots system)
                    (get-system-users system)
                    (new-interaction-rec system message (get-system-current-chatbotID system) (get-system-current-flowID system))
                    (change-system-current-chatbot-id system message)
                    (change-system-current-flow-id system message)
                    (get-system-date system))
        system)))


;######################################################################################
;        RF13
;######################################################################################
;Descripcion de la funcion: Permite a un usuario interactuar con un chatbot
;Dominio: system X message(string)
;Recorrido: system
;Tipo de recursion: N/A
(define system-talk-norec (lambda (system message)
    (if (logged-users? system)
        (new-system (get-system-name system)
                    (get-system-initial-chatbot-code-link system)
                    (get-system-chatbots system)
                    (get-system-users system)
                    (new-interaction-norec system message)
                    (change-system-current-chatbot-id-norec system message)
                    (change-system-current-flow-id-norec system message)
                    (get-system-date system))
        system)))


;######################################################################################
;        RF14
;######################################################################################
;Descripcion de la funcion: Obtiene una sintesis de las interacciones de un usuario y el sistema
;Dominio: system X usuario(string)
;Recorrido: string
;Tipo de recursion: N/A
(define system-synthesis (lambda (system usuario)  
    (get-chatHistory (car (filter (lambda (chatHistory) (string-ci=? usuario (get-chatHistory-user chatHistory))) (get-system-chat-history system))))))


(provide (all-defined-out))