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
;        Script de Pruebas Numero 2
;######################################################################################

;Ejemplo de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
(define op1 (option  1 "1) Viajar" 1 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2 y op1
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada            
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
;Chatbot1
(define op3 (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York"))
(define op4 (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel"))
(define op5 (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine"))
(define op6 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op7 (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park"))
(define op8 (option 2 "2) Museos" 1 2 "Museo"))
(define op9 (option 3 "3) Ningún otro atractivo" 1 3 "Museo"))
(define op10 (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir")) 
(define op11 (option 1 "1) Solo" 1 3 "Solo")) 
(define op12 (option 2 "2) En pareja" 1 3 "Pareja"))
(define op13 (option 3 "3) En familia" 1 3 "Familia"))
(define op14 (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos"))
(define op15 (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino"))
(define f20 (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f22 (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
(define cb1 (chatbot 1 "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f20 f21 f22))
;Chatbot2
(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))

(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define cb2 (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
;Sistema
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2"))
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8"))
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))
;las siguientes interacciones deben funcionar de igual manera con system-talk-rec  o system-talk-norec 
(define s11 (system-talk-rec s10 "hola"))
(define s12 (system-talk-rec s11 "1"))
(define s13 (system-talk-rec s12 "1"))
(define s14 (system-talk-rec s13 "Museo"))
(define s15 (system-talk-rec s14 "1"))
(define s16 (system-talk-rec s15 "3"))
(define s17 (system-talk-rec s16 "5"))

;(display (get-chatHistory (cadr (get-system-chat-history s17))))
;s17

(define s11-norec (system-talk-norec s10 "hola"))
(define s12-norec (system-talk-norec s11 "1"))
(define s13-norec (system-talk-norec s12 "1"))
(define s14-norec (system-talk-norec s13 "Museo"))
(define s15-norec (system-talk-norec s14 "1"))
(define s16-norec (system-talk-norec s15 "3"))
(define s17-norec (system-talk-norec s16 "5"))
(display (get-chatHistory (cadr (get-system-chat-history s17-norec))))
;s17-norec
