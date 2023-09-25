#lang racket


;######################################################################################
;        TDA Option
;######################################################################################
; code(int) X message(str) X ChatbotCodeLink(int) X InitialFlowCodeLink (int) X Keyword(strs)



;######################################################################################
;        Pertenencias
;######################################################################################

;Compara el code de una opcion y una nueva opcion
(define equal-option-code? (lambda (option new-option)
    (cond ((null? option) #f)
          ((= (get-option-code option) (get-option-code new-option)) #t)
          (else #f))))

(define option-exist? (lambda (options new-option) ;este map retorna '() la primera vez
        (if (equal? (null? (filter (lambda (x) (equal? x #t)) 
                        (map (lambda (option) (equal-option-code? option new-option)) options))) #f)
            #t
            #f)))
;######################################################################################
;        Selectores
;######################################################################################

(define get-option-code car)
(define get-option-message cadr)


;######################################################################################
;        Otras funciones
;######################################################################################

(define transformar-a-minusculas (lambda (palabras)
    (map string-downcase palabras)))


(provide (all-defined-out))