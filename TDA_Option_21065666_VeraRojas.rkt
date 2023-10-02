#lang racket


;######################################################################################
;        TDA Option
;######################################################################################
; code(int) X message(str) X ChatbotCodeLink(int) X InitialFlowCodeLink (int) X Keyword(strs)

;El constructor de TDA Option queda definido en el Main de este programa

;######################################################################################
;        Pertenencias
;######################################################################################

;Descripcion de la funcion: Verifica si una option tiene el mismo code que otra option
;Dominio: option X new-option(option)
;Recorrido: boolean
;Tipo de recursion: N/A 
(define equal-option-code? (lambda (option new-option)
    (cond ((null? option) #f)
          ((= (get-option-code option) (get-option-code new-option)) #t)
          (else #f))))


;Descripcion de la funcion: Verifica si una option existe dentro de una lista de option
;Dominio: options X new-option(option)
;Recorrido: boolean
;Tipo de recursion: N/A 
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

;Descripcion de la funcion: Dada una lista de keywords las transforma a minusculas 
;Dominio: palabras(string-list)
;Recorrido: palabras(string-list)
;Tipo de recursion: N/A
(define transformar-a-minusculas (lambda (palabras)
    (map string-downcase palabras)))


(provide (all-defined-out))