#lang racket


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


(provide (all-defined-out))