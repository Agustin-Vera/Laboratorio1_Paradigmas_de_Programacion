#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")

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