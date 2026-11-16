#lang eopl
;Especificación Léxica

(define lexica
  '(
    (white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?" "_"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    )
  )

;Especificación Sintáctica (gramática)

(define gramatica
  '(
    ;; =========================================================
    ;; PROGRAMA: Punto de entrada
    ;; =========================================================
    ;<programa> ::= GLOBALS { { <declaracion> }* } PROGRAM { proc main ( ) { return <expresion> } }
    ;               un-programa (decls exp pc)
    (programa
      ("GLOBALS" "{" (arbno declaracion) "}"
       "PROGRAM" "{" "proc" "main" "(" ")" "{" "return" expresion (arbno ";") "}" "}")
      un-programa)

    ;; =========================================================
    ;; DECLARACIÓN: Crear un nombre (variable, constante o proc)
    ;; =========================================================
    ;<declaracion> ::= <tipo> <identificador> = <expresion> ;
    ;                  var-decl (tipo id exp)
    (declaracion (tipo identifier "=" expresion ";") var-decl)

    ;<declaracion> ::= const <tipo> <identificador> = <expresion> ;
    ;                  const-decl (tipo id exp)
    (declaracion ("const" tipo identifier "=" expresion ";") const-decl)

    ;<declaracion> ::= proc <identificador> = <expresion>
    ;                  proc-decl (id exp)
    (declaracion ("proc" identifier "=" expresion) proc-decl)

    ;; =========================================================
    ;; TIPO: La etiqueta de la declaración
    ;; =========================================================
    ;<tipo> ::= int | float | string | bool | void
    ;         | list < <tipo> > | vector < <tipo> > | dict < <tipo> , <tipo> >
    (tipo ("int") tipo-int)
    (tipo ("float") tipo-float)
    (tipo ("string") tipo-string)
    (tipo ("bool") tipo-bool)
    (tipo ("void") tipo-void)
    (tipo ("list" "<" tipo ">") tipo-list)
    (tipo ("vector" "<" tipo ">") tipo-vector)
    (tipo ("dict" "<" tipo "," tipo ">") tipo-dict)

    ;; =========================================================
    ;; EXPRESIÓN: Cadena de operandos unidos por primitivas
    ;; =========================================================
    ;<expresion> ::= <operando> { <primitiva-binaria> <operando> }*
    ;                operacion-exp (primer-op prims otros-ops)
    (expresion (operando (arbno primitiva-binaria operando)) operacion-exp)

    ;; =========================================================
    ;; OPERANDO: Cada pieza atómica de una expresión
    ;; =========================================================
    ;<operando> ::= <numero>
    ;               numero-lit (num)
    (operando (number) numero-lit)

    ;<operando> ::= " <texto> "
    ;               texto-lit (txt)
    (operando (cadena) texto-lit)

    ;<operando> ::= True | False
    ;               true-lit () | false-lit ()
    (operando ("True") true-lit)
    (operando ("False") false-lit)

    ;<operando> ::= <identificador> <sufijo-id>
    ;               id-exp (id sufijo)
    (operando (identifier sufijo-id) id-exp)

    ;<sufijo-id> ::= vacio | ( { <expresion> }*(,) )
    ;                var-sufijo () | app-sufijo (args)
    (sufijo-id () var-sufijo)
    (sufijo-id ("(" (separated-list expresion ",") ")") app-sufijo)

    ;<operando> ::= ( <expresion> )
    ;               grupo-exp (exp)
    (operando ("(" expresion ")") grupo-exp)

    ;<operando> ::= <bloque>
    ;               bloque-exp (blk)
    (operando (bloque) bloque-exp)

    ;<operando> ::= <locales>
    ;               locales-exp (loc)
    (operando (locales) locales-exp)

    ;; =========================================================
    ;; PRIMITIVA BINARIA: Operadores
    ;; =========================================================
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("-") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("==") primitiva-comparador-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("->") primitiva-asignacion)

    ;; =========================================================
    ;; SENTENCIA: Paso dentro de un bloque
    ;; =========================================================
    ;<sentencia> ::= <expresion> ;
    ;                una-sentencia (exp)
    (sentencia (expresion ";") una-sentencia)

    ;; =========================================================
    ;; BLOCK: Ejecutar varias sentencias en orden
    ;; =========================================================
    ;<bloque> ::= BLOCK { <sentencia> { <sentencia> }* }
    ;             un-bloque (primera-sent resto-sents)
    (bloque ("BLOCK" "{" sentencia (arbno sentencia) "}") un-bloque)

    ;; =========================================================
    ;; LOCALS: Variables temporales y luego instrucciones
    ;; =========================================================
    ;<locales> ::= LOCALS { <declaracion> { <declaracion> }* } { <sentencia> { <sentencia> }* }
    ;              variableLocal-exp (primera-decl resto-decls primera-sent resto-sents)
    (locales
      ("LOCALS" "{" declaracion (arbno declaracion) "}"
                "{" sentencia (arbno sentencia) "}")
      variableLocal-exp)
  ))


;; ============================================================
;; Construcción de los datatypes y del parser a partir de la
;; especificación léxica y la gramática
;; ============================================================

;; Crea automáticamente los datatypes del árbol de sintaxis
;; (programa, expresion, primitiva-binaria, primitiva-unaria).
(sllgen:make-define-datatypes lexica gramatica)

;; Muestra los datatypes que sllgen generó a partir de la gramática.
;; Se usa para revisar cómo quedó el árbol de sintaxis.
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica))
)

;; Recibe un texto, lo escanea y lo parsea, y retorna su árbol de sintaxis.
;; Se usa para probar programas escritos como string.
(define scan&parse
  (sllgen:make-string-parser lexica gramatica)
)

;; Recibe un texto y retorna solo la lista de tokens (sin parsear).
;; Se usa para revisar la especificación léxica.
(define just-scan
  (sllgen:make-string-scanner lexica gramatica)
)



;; Recibe un booleano de Racket (#t o #f) y lo convierte en 1 o 0.
;; Se usa para que las comparaciones del lenguaje retornen 1 o 0.
(define boolean-out
  (lambda (boolean-exp)
    (if boolean-exp #t #f)))

(define aplicar-primitiva-binaria
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ (car args) (cadr args)))
      (primitiva-resta () (- (car args) (cadr args)))
      (primitiva-multi () (* (car args) (cadr args)))
      (primitiva-div () (if (= (cadr args) 0) (eopl:error 'aplicar-primitiva-binaria "division by zero") (/ (car args) (cadr args))))
      (primitiva-comparador-igual () (boolean-out (equal? (car args) (cadr args))))
      (primitiva-diferente () (boolean-out (not (equal? (car args) (cadr args)))))
      (primitiva-mayor () (boolean-out (> (car args) (cadr args))))
      (primitiva-menor () (boolean-out (< (car args) (cadr args))))
      (primitiva-mayor-igual () (boolean-out (>= (car args) (cadr args))))
      (primitiva-menor-igual () (boolean-out (<= (car args) (cadr args))))
      (primitiva-asignacion () (eopl:error 'aplicar-primitiva-binaria "El operador -> solo puede usarse justo despues de una variable"))
      )
    )
  )

 (define aplicar-tipo
  (lambda (t)
    (cases tipo t
      (tipo-int () "int")
      (tipo-float () "float")
      (tipo-string () "string")
      (tipo-bool () "bool")
      (tipo-void () "void")
      (tipo-list (tipo-elem) (string-append "list<" (aplicar-tipo tipo-elem) ">"))
      (tipo-vector (tipo-elem) (string-append "vector<" (aplicar-tipo tipo-elem) ">"))
      (tipo-dict (tipo-clave tipo-valor) (string-append "dict<" (aplicar-tipo tipo-clave) "," (aplicar-tipo tipo-valor) ">")))
    )
   )
      
 (define aplicar-operando
  (lambda (o env)
    (cases operando o
      (numero-lit (datum) datum)
      (texto-lit (cadena)(substring cadena 1 (- (string-length cadena) 1)))
      (true-lit () #t)
      (false-lit () #f)
      (id-exp (nombre sufijo)
              (cases sufijo-id sufijo
                (var-sufijo () (eopl:error 'aplicar-operando "ambiente aun no implementado: ~s" nombre))
                (app-sufijo (args) (eopl:error 'aplicar-operando "aun no implementado"))
               ))
      (grupo-exp (exp) (evaluar-expresion exp env) )
      (bloque-exp (exp) (evaluar-bloque exp env) )
      (locales-exp (exp) (evaluar-expresion exp env) )
    )
   )
  )

;; ============================================================
;; Evaluación
;; ============================================================

;; evaluar-expresion : <expresion> x ambiente -> valor

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls exp)
         (if (null? decls) (evaluar-expresion exp '()) (eopl:error 'evaluar-programa "aun no se ha implementado")))
        )))


(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
        (operacion-exp (primer prims resto)
                       (if (null? prims)
                           (aplicar-operando primer env)
                           (let ((res (aplicar-primitiva-binaria (car prims) 
                                      (list (aplicar-operando primer env) 
                                            (aplicar-operando (car resto) env)))))
  (evaluar-expresion 
    (operacion-exp (if (number? res) (numero-lit res) (if res (true-lit) (false-lit)))
                   (cdr prims) 
                   (cdr resto))
    env))
        )

  ))))

(define evaluar-sentencia
  (lambda (sent env)
    (cases sentencia sent
      (una-sentencia (exp)
            (evaluar-expresion exp env))
        )))

(define evaluar-bloque
  (lambda (block env)
    (cases bloque block
      (un-bloque (sentencia lista-sentencias)
                 (if (null? lista-sentencias)
                     (evaluar-sentencia sentencia env)
                     (begin
                       (evaluar-sentencia sentencia env)
                       (evaluar-bloque (un-bloque (car lista-sentencias)(cdr lista-sentencias)) env)
        ))))))

(define probar
  (lambda (texto)
    (evaluar-programa (scan&parse texto))))



