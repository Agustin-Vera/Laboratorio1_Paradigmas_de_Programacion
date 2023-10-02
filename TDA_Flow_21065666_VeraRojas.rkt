#lang racket

(require "TDA_Option_21065666_VeraRojas.rkt")

;######################################################################################
;        TDA Flow
;######################################################################################
; ID(int) X name-msg(str) x Options


;######################################################################################
;        Constructor
;######################################################################################

;Descripcion de la funcion: Crea un flow
;Dominio: id(int) X name-msg(string) X options
;Recorrido: flow
;Tipo de recursion: N/A 
(define new-flow (lambda (id name-msg options)
    (list id name-msg options)))


;######################################################################################
;        Selectores
;######################################################################################

(define get-flow-id car)
(define get-flow-name-msg cadr)
(define get-flow-options caddr)


;######################################################################################
;        Pertenencias
;######################################################################################

;Descripcion de la funcion: Verifica si un flow existe
;Dominio: flows X flow
;Recorrido: boolean
;Tipo de recursion: Recursion natural
(define flow-exist? (lambda (flows flow)
    (cond ((null? flows) #f)
          ((= (get-flow-id (car flows)) (get-flow-id flow)) #t)
          (else (flow-exist? (cdr flows) flow)))))


;Descripcion de la funcion: Verifica si dos flows tienen el mismo ID
;Dominio: flow X new-flow(flow)
;Recorrido: boolean
;Tipo de recursion: N/A 
(define equal-flow-id? (lambda (flow new-flow)
    (cond ((null? flow) #f)
          ((= (get-flow-id flow) (get-flow-id new-flow)) #t)
          (else #f))))


;######################################################################################
;        Modificadores
;######################################################################################

;Descripcion de la funcion: Agrega una option a una lista de options
;Dominio: flow X option
;Recorrido: option-list
;Tipo de recursion: N/A
(define add-option-to-options (lambda (flow option)
    (cons option (get-flow-options flow))))


;Descripcion de la funcion: Obtiene uno o más option sin repetir un option ya agregado, compara el code, que puede verse como su id
;Dominio: options
;Recorrido: options
;Tipo de recursion: N/A
(define add-unique-options (lambda (options)
    (cond ((null? options) options)
          (else (cons (car options) 
                (add-unique-options (filter (lambda (option) (not (equal-option-code? (car options) option))) options)))))))


(provide (all-defined-out))
