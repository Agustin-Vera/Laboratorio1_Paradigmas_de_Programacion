#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")
(require "TDA_Flow_21065666_VeraRojas.rkt")
(require "TDA_Chatbot_21065666_VeraRojas.rkt")

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