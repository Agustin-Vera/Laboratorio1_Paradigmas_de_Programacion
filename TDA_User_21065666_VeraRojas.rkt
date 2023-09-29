#lang racket

;######################################################################################
;        TDA ChatBot
;######################################################################################
; user-name(string) X status(bool)

;######################################################################################
;        Constructor
;######################################################################################
(define new-user (lambda (user-name state)
    (list (string-downcase user-name) state)))


;######################################################################################
;        Pertenencias
;######################################################################################


(define user-exist? (lambda (users user-name) ;este map retorna '() la primera vez
    (cond ((null? users) #f)
          ((string-ci=? (get-user-name (car users)) user-name) #t)
          (else (user-exist? (cdr users) user-name)))))


;######################################################################################
;        Selectores
;######################################################################################
(define get-user-name car)
(define get-user-status cadr)


(provide (all-defined-out))