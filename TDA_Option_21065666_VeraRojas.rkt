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


;Descripcion de la funcion: Verifica si una mensaje es valido, si es una keyword o si es un ID existente
;Dominio: options X message(string)
;Recorrido: boolean
;Tipo de recursion: Recursion de cola 
(define message-is-valid? (lambda (options message)
    (if (number? (string->number message))
        (option-exist-rec? options (string->number message))
        (keyword? options message))))


;Descripcion de la funcion: Verifica si existe una option con un code correspondiente
;Dominio: options X code(int)
;Recorrido: boolean
;Tipo de recursion: Recursion de cola 
(define option-exist-rec? (lambda (options code)
    (cond ((null? options) #f)
          ((= code (get-option-code (car options))) #t)
          (else (option-exist-rec? (cdr options) code)))))


;Descripcion de la funcion: Verifica si una palabra es una keyword dentro de una option
;Dominio: options X word(string)
;Recorrido: boolean
;Tipo de recursion: Recursion de cola 
(define keyword? (lambda (options word)
    (cond ((null? options) #f) 
          ((keyword-exist? (get-option-keywords (car options)) word) #t)
          (else (keyword? (cdr options) word)))))


;Descripcion de la funcion: Verifica si una palabra se encuentra dentro de una lista de keywords
;Dominio: keywords(string-list) x word(string)
;Recorrido: boolean
;Tipo de recursion: Recursion de cola
(define keyword-exist? (lambda (keywords word)
    (cond ((null? keywords) #f)
          ((string-ci=? word (car keywords)) #t)
          (else (keyword-exist? (cdr keywords) word)))))

;######################################################################################
;        Selectores
;######################################################################################

;Obtienen un elemento dada una option
;Poseen nombres representativos
;Dominio: option
;Recorrido: lo que su nombre indique
;Tipo de recursion: N/A
(define get-option-code car)
(define get-option-message cadr)
(define get-option-ChatbotCodeLink caddr)
(define get-option-InitialFlowCodeLink cadddr)
(define get-option-keywords (lambda (option) (cadddr (cdr option))))


;Descripcion de la funcion: Obtiene una option dentro de una lista de options dado un message
;Dominio: options X message(int or string)
;Recorrido: option
;Tipo de recursion: N/A
(define get-option-by-message (lambda (options message)
    (if (string? message)
        (if (keyword-exist? (get-option-keywords (car options)) message)
            (car options)
            (get-option-by-message (cdr options) message))
        (if (= message (get-option-code (car options)))
            (car options)
            (get-option-by-message (cdr options) message)))))


;Descripcion de la funcion: Obtiene una option dentro de una lista de options dado un message
;Dominio: options X message(int or string)
;Recorrido: option
;Tipo de recursion: N/A 
(define get-option-by-message-norec (lambda (options message)
    (if (number? (string->number message))
        (car (filter (lambda (option) (= (string->number message) (get-option-code option))) options))
        (car (filter (lambda (option) (keyword-exist? (get-option-keywords option) message)) options)))))


;######################################################################################
;        Otras funciones
;######################################################################################

;Descripcion de la funcion: Dada una lista de keywords las transforma a minusculas 
;Dominio: palabras(string-list)
;Recorrido: palabras(string-list)
;Tipo de recursion: N/A
(define transformar-a-minusculas (lambda (palabras)
    (map string-downcase palabras)))


;Descripcion de la funcion: Crea un string con todas las options que tenga
;Dominio: options
;Recorrido: string
;Tipo de recursion: Recursion de cola
(define options-to-string-rec (lambda (options)
    (cond ((null? options) "\n")
          (else (string-append (get-option-message (car options)) "\n" (options-to-string-rec (cdr options)))))))

(provide (all-defined-out))