#lang racket

(require "Main_21065666_VeraRojas.rkt")

;Este documento posee 2 scripts de pruebas y los ejemplos propios para las funciones implementadas
;Los script de pruebas 1 y 2 corresponden a los brindados por coordinacion
;Posterior a estos scripts se encuentran los 3 ejemplos propios para cada requerimiento implementado
;Los requerimientos NO implementados se dejaran comentados en los scripts

;Lo que se vera por consola corresponde a solo a los requerimientos que se ejecuten con la funcion display
;Por ende, solamente se vera por consola lo referente al RF14


;Este script se deja comentado debido a que se tienen los mismos simbolos asociados en el script 2
;######################################################################################
;        Script de Pruebas Numero 1
;######################################################################################
#|
(define op1 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8")) ;user8 no existe. No inicia sesión
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))  ;no permite iniciar sesión a user2, pues user1 ya inició sesión
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))
|#


;######################################################################################
;        Script de Pruebas Numero 2
;######################################################################################

;Ejemplo de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
(define op1 (option  1 "1) Viajar" 1 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2 y op1
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada            
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
;Chatbot1
(define op3 (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York"))
(define op4 (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel"))
(define op5 (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine"))
(define op6 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op7 (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park"))
(define op8 (option 2 "2) Museos" 1 2 "Museo"))
(define op9 (option 3 "3) Ningún otro atractivo" 1 3 "Museo"))
(define op10 (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir")) 
(define op11 (option 1 "1) Solo" 1 3 "Solo")) 
(define op12 (option 2 "2) En pareja" 1 3 "Pareja"))
(define op13 (option 3 "3) En familia" 1 3 "Familia"))
(define op14 (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos"))
(define op15 (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino"))
(define f20 (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f22 (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
(define cb1 (chatbot 1 "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f20 f21 f22))
;Chatbot2
(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))

(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define cb2 (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
;Sistema
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2"))
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8"))
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))
;las siguientes interacciones deben funcionar de igual manera con system-talk-rec  o system-talk-norec 
(define s11 (system-talk-rec s10 "hola"))
(define s12 (system-talk-rec s11 "1"))
(define s13 (system-talk-rec s12 "1"))
(define s14 (system-talk-rec s13 "Museo"))
(define s15 (system-talk-rec s14 "1"))
(define s16 (system-talk-rec s15 "3"))
(define s17 (system-talk-rec s16 "5"))
(display (system-synthesis s17 "user2"))
;(system-simulate s0 5 32131)


;######################################################################################
;        Ejemplos Propios
;######################################################################################
;Algunos requerimientos se les dejaran comentarios de "Auxiliares" debido a que
;si bien tambien pueden aportar como ejemplos de uso para su requerimiento
;ayudaran al funcionamiento o ejemplo de uso de requerimientos posteriores

;######################################################################################
;        RF2 - option
;######################################################################################

;Opcion de entretencion en chabot inicial
(define op1-f1-cb0 (option 3 "3) Entretenerme" 3 1 "diversion" "entretención"))

;Opciones de tipo de entretencion 
(define op1-f1-cb3 (option 1 "1) Películas" 3 2 "pelicula"))
(define op2-f1-cb3 (option 2 "2) Fútbol" 3 3 "Fútbol" "football"))

;Auxiliares: Usados en RF flow y RF flow-add-option
(define op1-f2-cb3 (option 1 "1) Avengers: Endgame" 3 2 "marvel"))
(define op2-f2-cb3 (option 2 "2) Meet Joe Black" 3 2 "Brad Pitt" "Claire Forlani"))
(define op1-f3-cb3 (option 1 "1) Manchester City vs Inter de Milán" 3 3 "Champions" "Europa"))
(define op2-f3-cb3 (option 2 "2) Universidad de Chile vs Liga de Quito" 3 3 "Sudamericana"))
(define op3-f3-cb3 (option 3 "3) Deseo hacer otra cosa" 3 1))


;######################################################################################
;        RF3 - flow
;######################################################################################

;Crea el flujo 1 y agrega las primeras opciones con code unico, por ende, op1 no es agregada
(define f1-cb3 (flow 1 "Flujo 1 Chatbot 3\n¿Qué te gustaría ver?" op1-f1-cb3 op2-f1-cb3 op1))

;Crea los flujos correspondientes a peliculas y futbol
(define f2-cb3 (flow 2 "Flujo 2 Chatbot 3\n¿Qué película te gustaría ver?" op1-f2-cb3 op2-f2-cb3))
(define f3-cb3 (flow 3 "Flujo 3 Chatbot 3\n¿Qué partido te gustaría ver?" op1-f3-cb3 op2-f3-cb3 op3-f3-cb3))

;Auxiliares: Usados en RF flow-add-option
(define f1-cb3-v2 (flow 1 "Flujo 1 Chatbot 3\n¿Qué te gustaría ver?"))
(define f0-cb3-v2 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op1-f1-cb0))


;######################################################################################
;        RF4 - flow-add-option
;######################################################################################

;Igual al flujo 1 del Chabot 3
(define f1-cb3-v3 (flow-add-option f1-cb3-v2 op1-f1-cb3))
(define f1-cb3-v4 (flow-add-option f1-cb3-v3 op2-f1-cb3))

;Se intenta agregar una option con un code existente
(define f1-cb3-v5 (flow-add-option f1-cb3-v4 op1-f1-cb3))


;######################################################################################
;        RF5 - chatbot
;######################################################################################

;Se crea un chatbot sin flujos
(define cb3-v0 (chatbot 3 "Agencia Entretención" "Bienvenido\n¿Qué te gustaría ver?" 1))

;Se quiere agregar un flow con ID ya existente
(define cb3-v1 (chatbot 3 "Agencia Entretención" "Bienvenido\n¿Qué te gustaría ver?" 1 f1-cb3-v3 f30))

;Chatbot 3: Agrencia de Entretencion
(define cb3 (chatbot 3 "Agencia Entretención" "Bienvenido\n¿Qué te gustaría ver?" 1 f1-cb3 f2-cb3 f3-cb3))

;Auxiliar: Se utiliza en RF 7
(define cb0-v2 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f0-cb3-v2))

;######################################################################################
;        RF6 - chatbot-add-flow
;######################################################################################

;Agrega el flujo 2 al chatbot 3
(define cb3-v2 (chatbot-add-flow cb3-v0 f2-cb3))

;Se intenta agregar un flujo con un ID existente
(define cb3-v3 (chatbot-add-flow cb3-v2 f2-cb3))

;El chatbot es casi igual al chatbot 3, pero le falta el flujo 3
(define cb3-v4 (chatbot-add-flow cb3-v3 f1-cb3))


;######################################################################################
;        RF7 - system
;######################################################################################

;Se crea un sistema sin chatbots
(define s0-v0 (system "Chatbots Pruebas" 0))

;Se crea un chatbot completo
(define s0-v1 (system "Chatbots Paradigmas V2" 0 cb0 cb1 cb2 cb3))

;Se intenta crear un system con distintos chatbots, solo agrega las primeras ocurrencias de cb0, cb1, cb2 y cb3
;system con 4 chatbots
(define s0-v2 (system "Chatbots Paradigmas V2" 0 cb0-v2 cb1 cb2 cb3 cb0-v2 cb1 cb2 cb3 cb0 cb1 cb2 cb3))


;######################################################################################
;        RF8 - system-add-chatbot
;######################################################################################

;Se agrega el chatbot inicial a un system vacio de chatbots
(define s0-v3 (system-add-chatbot s0-v0 cb0))

;Se agrega un chatbot no inicial a un system vacio de chatbots
(define s0-v4 (system-add-chatbot s0-v0 cb1))

;Se intenta agregar un chatbot con un mismo ID a un system
(define s0-v5 (system-add-chatbot s0-v4 cb1))


;######################################################################################
;        RF9 - system-add-user
;######################################################################################

;Se agregan usuarios al system
(define sys1 (system-add-user s0-v2 "Franck"))
(define sys2 (system-add-user sys1 "Gareth"))

;Se intenta agregar un usuario repetido
(define sys3 (system-add-user sys2 "Gareth"))

;Se agrega otro usuario
(define sys4 (system-add-user sys2 "Messi"))


;######################################################################################
;        RF10 - system-login
;######################################################################################

;Se intenta iniciar un usuario inexiistente
(define sys5 (system-login sys4 "user1"))

;Se inicia un usuario
(define sys6 (system-login sys5 "Gareth"))

;Se intenta iniciar un usuario pero ya hay un usuario iniciado
(define sys7 (system-login sys6 "Franck"))


;######################################################################################
;        RF11 - system-logout
;######################################################################################

;Se intenta cerrar sesion en un systema donde no hay usuarios iniciados
(define sys8 (system-logout sys5))

;Se cierra sesion de un usuario
(define sys9 (system-logout sys7))

;Se intenta cerrar sesion en un systema donde no hay usuarios iniciados
(define sys10 (system-logout sys9))

;Auxiliar: Se utiliza en RF12
(define sys11 (system-login sys10 "Messi"))


;######################################################################################
;        RF12 - system-talk-rec
;######################################################################################

;Se realizan interacciones entre el sistema de chatbots y un usuario
(define sys12 (system-talk-rec sys11 "Buenos días"))
(define sys13 (system-talk-rec sys12 "3"))
(define sys14 (system-talk-rec sys13 "2"))
(define sys15 (system-talk-rec sys14 "2"))


;######################################################################################
;        RF13 - system-talk-norec
;######################################################################################

;Se realizan las mismas interacciones que realizo el user2 en el scrpit de pruebas numero 2
;Sin embargo el usuario posee distinto nombre, la respuesta es la misma
(define sys12-norec (system-talk-norec sys11 "hola"))
(define sys13-norec (system-talk-norec sys12-norec "1"))
(define sys14-norec (system-talk-norec sys13-norec "1"))
(define sys15-norec (system-talk-norec sys14-norec "Museo"))
(define sys16-norec (system-talk-norec sys15-norec "1"))
(define sys17-norec (system-talk-norec sys16-norec "3"))
(define sys18-norec (system-talk-norec sys17-norec "5"))


;######################################################################################
;        RF14 - system-synthesis
;######################################################################################

;Misma synthesis del usuario Messi y el user2 del script de pruebas numero 2
(display (system-synthesis sys18-norec "Messi"))

;No se muestra nada debido a que el usuario no a interactuado con el sistema sys12
(display (system-synthesis sys12 "Gareth"))

;Se muestra la interaccion del usuario Messi con el sistema sys15, es un system distinto a sys18-norec
(display (system-synthesis sys15 "Messi"))