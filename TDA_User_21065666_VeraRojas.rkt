#lang racket

;######################################################################################
;        TDA ChatBot
;######################################################################################
; user-name(string) X status(bool)

;######################################################################################
;        Constructor
;######################################################################################

;Descripcion de la funcion: Crea un usuario
;Dominio: user-name(string) X state(boolean)
;Recorrido: user
;Tipo de recursion: N/A 
(define new-user (lambda (user-name state)
    (list (string-downcase user-name) state)))


;######################################################################################
;        Pertenencias
;######################################################################################

;Descripcion de la funcion: Verifica si un usuario existe en una lista en base a su ID (su nombre)
;Dominio: users X user-name(string)
;Recorrido: boolean
;Tipo de recursion: Recursion natural
(define user-exist? (lambda (users user-name) 
    (cond ((null? users) #f)
          ((string-ci=? (get-user-name (car users)) user-name) #t)
          (else (user-exist? (cdr users) user-name)))))


;Descripcion de la funcion: Verifica si eexiste un usuario iniciado
;Dominio: users
;Recorrido: boolean
;Tipo de recursion: Recursion natural 
(define logged-user? (lambda (users)
    (cond ((null? users) #f)
          ((equal? (get-user-status (car users)) #t) #t)
          (else (logged-user? (cdr users))))))
          
;######################################################################################
;        Selectores
;######################################################################################
(define get-user-name car)
(define get-user-status cadr)


;Descripcion de la funcion: Obtiene al usuario iniciado
;Dominio: users
;Recorrido: user
;Tipo de recursion: Recursion natural 
(define get-logged-user (lambda (users)
    (cond ((null? users) users)
          ((equal? #t (get-user-status (car users))) (car users))
          (else (get-logged-user (cdr users))))))

;######################################################################################
;        Modificadores
;######################################################################################

;Descripcion de la funcion: Inicia la sesion de un usuario
;Dominio: user X user-name(string)
;Recorrido: user
;Tipo de recursion: N/A
(define log-in (lambda (user user-name)
    (cond ((string-ci=? (get-user-name user) user-name) (new-user (get-user-name user) #t))
          (else user))))


;Descripcion de la funcion: Inicia sesion de un usuario existente
;Dominio: users X user-name(string)
;Recorrido: users
;Tipo de recursion: Recursion de cola 
(define login-user (lambda (users user-name)
    (cond ((null? users) users)
          (else (cons (log-in (car users) user-name) (login-user (cdr users) user-name))))))


;Descripcion de la funcion: Cierra la sesion de un usuario 
;Dominio: user
;Recorrido: user
;Tipo de recursion: N/A
(define logout (lambda (user)
    (cond ((equal? (get-user-status user) #t) (new-user (get-user-name user) #f))
          (else user))))


(provide (all-defined-out))