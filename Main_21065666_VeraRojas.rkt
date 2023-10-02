#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")
(require "TDA_Flow_21065666_VeraRojas.rkt")
(require "TDA_Chatbot_21065666_VeraRojas.rkt")
(require "TDA_System_21065666_VeraRojas.rkt")
(require "TDA_User_21065666_VeraRojas.rkt")

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


;creando opciones
;opción 1 vinculada al chatbot 2 con su flujo 4 (asumiendo su existencia) en sistema
(define op1 (option  1 "1) Viajar" 2 4 "viajar" "turistear" "conocer"))
op1
;opción 1 vinculada al chatbot 4 con su flujo 3 (asumiendo su existencia) en sistema
(define op2 (option  2 "2) Estudiar" 4 3 "aprender" "perfeccionarme"))
op2

;######################################################################################
;        RF3 - TDA Flow - Constructor
;######################################################################################
;Descripcion de la funcion: Crea un TDA flow
;Dominio: name(string) X option*
;Recorrido: flow
;Tipo de recursion: N/A
(define flow (lambda (id name-msg . option)
    (list id name-msg (add-unique-options option))))

(define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
f10


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

(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
f11


;######################################################################################
;        RF5 - TDA Chatbot - Constructor
;######################################################################################
;Descripcion de la funcion: Crea un TDA chatbot
;Dominio: chatbotID(int) X name(string) X welcomeMessage(string) X flows
;Recorrido: chatbot
;Tipo de recursion: N/A
(define chatbot (lambda (chatbotID name welcomeMessage startFlowID . flows)
    (list chatbotID name welcomeMessage startFlowID (add-unique-flows flows))))

(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
cb0


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
    
(define cb1 (chatbot-add-flow cb0 f10)) 
cb1


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

;creando la un nuevo sistema de chatbots con nombre “NewSystem”
(define s0 (system "NewSystem" 0))
s0
;alternativamente podría usarse:
(define s11 (system "NewSystem"  0 cb1))
s11


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

(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
s1


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


(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
(define s5 (system-add-user s4 "user3"))
s5


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

(define s6 (system-login s5 "user8")) ;user8 no existe. No inicia sesión
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))  ;no permite iniciar sesión a user2, pues user1 ya inició sesión
s8


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

(define s9 (system-logout s8))
s9