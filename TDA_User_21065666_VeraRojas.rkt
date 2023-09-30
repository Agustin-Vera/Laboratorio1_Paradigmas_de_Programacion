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

(define logged-user? (lambda (users)
    (cond ((null? users) #f)
          ((equal? (get-user-status (car users)) #t) #t)
          (else (logged-user? (cdr users))))))
          
;######################################################################################
;        Selectores
;######################################################################################
(define get-user-name car)
(define get-user-status cadr)

(define log-in (lambda (user user-name)
    (cond ((string-ci=? (get-user-name user) user-name) (new-user (get-user-name user) #t))
          (else user))))

(provide (all-defined-out))