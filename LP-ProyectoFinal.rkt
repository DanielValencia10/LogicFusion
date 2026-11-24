#lang eopl

;Especificación Léxica

(define lexica
  '(
    (white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?" "_" "-"))) symbol)
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

    ;<operando> ::= empty
    (operando ("empty") empty-lit)

    ;<operando> ::= [ { <expresion> }*(,) ]
    (operando ("[" (separated-list expresion ",") "]") lista-lit)

    ;<operando> ::= #[ { <expresion> }*(,) ]
    (operando ("#[" (separated-list expresion ",") "]") vector-lit)

    ;<operando> ::= { { <expresion> : <expresion> }*(,) }
    (operando ("{" (separated-list par-dict ",") "}") dict-lit)

    ;; Regla auxiliar para cada pareja clave : valor del diccionario
    (par-dict (expresion ":" expresion) un-par)
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

    ;; =========================================================
    ;; CONSTRUCCIONES: procedimientos, print y control
    ;; (un solo operando agrupa todo lo que sigue)
    ;; =========================================================
    ;<operando> ::= <construccion>
    ;               construccion-exp (constr)
    (operando (construccion) construccion-exp)

    ;; =========================================================
    ;; PROCEDIMIENTOS: lambda (la invocación f(args) ya existe
    ;; en la gramática como app-sufijo)
    ;; =========================================================
    ;<construccion> ::= lambda ( { <tipo> <identificador> }*(,) ) { return <expresion> }
    ;               lambda-exp (tipos ids exp)
    (construccion
      ("lambda" "(" (separated-list tipo identifier ",") ")"
                "{" "return" expresion (arbno ";") "}")
      lambda-exp)

    ;; =========================================================
    ;; PRINT: Imprime varios argumentos separados por espacio
    ;; =========================================================
    ;<construccion> ::= print ( { <expresion> }*(,) )
    ;               print-exp (args)
    (construccion ("print" "(" (separated-list expresion ",") ")") print-exp)

    ;; =========================================================
    ;; CUERPO: Sentencias entre llaves (if, switch, while, for)
    ;; =========================================================
    ;<cuerpo> ::= { <sentencia> { <sentencia> }* }
    ;             un-cuerpo (primera-sent resto-sents)
    (cuerpo ("{" sentencia (arbno sentencia) "}") un-cuerpo)

    ;; =========================================================
    ;; ESTRUCTURAS DE CONTROL
    ;; =========================================================
    ;<construccion> ::= if ( <expresion> ) <cuerpo> else <cuerpo>
    ;               if-exp (condicion cuerpo-si cuerpo-no)
    (construccion ("if" "(" expresion ")" cuerpo "else" cuerpo) if-exp)

    ;<construccion> ::= switch <expresion> { { <caso> }* default : <cuerpo> }
    ;               switch-exp (exp casos cuerpo-default)
    (construccion
      ("switch" expresion "{" (arbno caso) "default" ":" cuerpo "}")
      switch-exp)

    ;<caso> ::= case <expresion> : <cuerpo>
    ;           un-caso (exp cuerpo)
    (caso ("case" expresion ":" cuerpo) un-caso)

    ;<construccion> ::= while ( <expresion> ) <cuerpo>
    ;               while-exp (condicion cuerpo)
    (construccion ("while" "(" expresion ")" cuerpo) while-exp)

    ;<construccion> ::= for ( <expresion> ; <expresion> ; <expresion> ) <cuerpo>
    ;               for-exp (inicio condicion paso cuerpo)
    (construccion
      ("for" "(" expresion ";" expresion ";" expresion ")" cuerpo)
      for-exp)
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
          (var-sufijo () (aplicar-ambiente env nombre))
          (app-sufijo (args)
            (let ((vals (map (lambda (a) (evaluar-expresion a env)) args)))
              (if (es-primitiva-coleccion? nombre)
                  (aplicar-primitiva-coleccion nombre vals)
                  ;; invocación de procedimiento de usuario: paso por valor
                  (aplicar-procedimiento (aplicar-ambiente env nombre) vals nombre)
                  )
              )
            )
          )
        )
      (grupo-exp (exp) (evaluar-expresion exp env) )
      (bloque-exp (exp) (evaluar-bloque exp env) )
      (locales-exp (exp) (evaluar-locales exp env))
      (construccion-exp (constr) (evaluar-construccion constr env))
      (empty-lit () '())
      (lista-lit (exp) (map (lambda (e) (evaluar-expresion e env)) exp))
      (vector-lit (exps) (vec-nuevo (list->vector (map (lambda (e) (evaluar-expresion e env)) exps))))
      (dict-lit (pares) (eopl:error 'aplicar-operando "Diccionarios pendientes Sara"))
      )
    )
   )

;; ============================================================
;; Primitivas de listas
;; ============================================================
(define primitivas-colecciones
  '(empty? head tail make-list append ref-list length list? vector? make-vector ref-vector set-vector append-vector delete-val-vector insert-val-vector))

;; es-primitiva-sistema? : symbol -> boolean
;; Decide por el nombre, no por el resultado: empty? y list? pueden dar #f.
(define es-primitiva-coleccion?
  (lambda (nombre)
    (if (memq nombre primitivas-colecciones) #t #f)))

;; exigir-argumentos : symbol x lista x entero -> 'ok
(define exigir-argumentos
  (lambda (nombre args n)
    (if (not (= (length args) n))
        (eopl:error nombre "Espera ~s argumento(s) y recibio ~s" n (length args))
        'ok)))


;; empty
(define lista-vacia '())

;; empty? : lista -> boolean
(define lista-empty?
  (lambda (lst) (null? lst)))

;; head : lista -> valor   (error si la lista está vacía)
(define lista-head
  (lambda (lst)
    (if (null? lst)
        (eopl:error 'head "La lista esta vacia")
        (car lst))))

;;lista-tail : lista -> el resto de la lista
(define lista-tail
  (lambda (lst)
    (if (null? lst)
        (eopl:error 'tail "La lista esta vacia")
        (cdr lst))))

;;lista-make : elem x lista -> lista.
(define lista-make
  (lambda (elem lst) (cons elem lst)))

;; lista-append : unión de dos listas
(define lista-append
  (lambda (lst1 lst2) (append lst1 lst2)))

;; lista-ref : devuelve el elemento en la posición pos

(define lista-ref
  (lambda (lst pos)
    (cond
      ((or (< pos 0) (null? lst))
       (eopl:error 'ref-list "Indice fuera de rango"))
      ((= pos 0) (car lst))
      (else (lista-ref (cdr lst) (- pos 1))))))

;; lista-length : la cantidad de elementos
(define lista-length
  (lambda (lst) (length lst)))

;; lista-es? : dice si un valor es una lista.
(define lista-es?
  (lambda (lst) (list? lst)))

;; ------------------------------------------------------------
;; Auxiliares de listas 
;; las usan vec-delete y vec-insert
;; ------------------------------------------------------------

;; lista-quitar : lista x entero -> lista
;; Retorna una lista nueva sin el elemento de la posición pos.
(define lista-quitar
  (lambda (lst pos)
    (cond
      ((= pos 0) (cdr lst))
      (else (cons (car lst) (lista-quitar (cdr lst) (- pos 1)))))))

;; lista-insertar : lista x entero x valor -> lista
;; Retorna una lista nueva con valor insertado en la posición pos.
(define lista-insertar
  (lambda (lst pos valor)
    (cond
      ((= pos 0) (cons valor lst))
      (else (cons (car lst) (lista-insertar (cdr lst) (- pos 1) valor))))))

;; ============================================================
;; Primitivas de vectores
;; ============================================================
;; Un vector de LogicFusion es un envoltorio: un vector de Racket de UNA
;; posición que guarda el vector real. Así append/insert/delete pueden
;; cambiar el tamaño: se arma un vector nuevo y se guarda en esa posición,
;; y todos los que tengan el envoltorio ven el cambio.

;; vec-nuevo : Racket-vector -> vector de LogicFusion (envoltorio)
(define vec-nuevo (lambda (datos) (vector datos)))

;; vec-datos : vector de LogicFusion -> Racket-vector con los elementos
(define vec-datos (lambda (v) (vector-ref v 0)))

;; vec-es? : valor -> boolean
;; Dice si un valor es un vector de LogicFusion.
(define vec-es?
  (lambda (v)
    (vector? v)))

;; vec-ref : vector x entero -> valor
;; Devuelve el elemento en la posición pos (error si está fuera de rango).
(define vec-ref
  (lambda (v pos)
    (let ((datos (vec-datos v)))
      (cond
        ((or (< pos 0) (>= pos (vector-length datos)))
         (eopl:error 'ref-vector "Indice fuera de rango"))
        (else (vector-ref datos pos))))))

;; vec-set! : vector x entero x valor -> vector
;; Cambia el elemento en pos por valor, modificando el vector original
;; (error si pos está fuera de rango). Retorna el mismo vector.
(define vec-set!
  (lambda (v pos valor)
    (let ((datos (vec-datos v)))
      (cond
        ((or (< pos 0) (>= pos (vector-length datos)))
             (eopl:error 'set-vector "Indice fuera de rango"))
         (else (vector-set! datos pos valor)
               v)))))

;; vec-make : entero -> vector
;; Crea un vector de tamaño n con todas las posiciones en 0.
;; TODO: llenar según el tipo declarado cuando esté el sistema de tipos.

(define vec-make
  (lambda (n)
    (if (< n 0)
        (eopl:error 'make-vector "El tamano no puede ser negativo")
        (vec-nuevo (make-vector n 0)))))

;; vec-append : vector x valor -> vector
;; Agrega valor al final del vector, modificando el vector original.
;; Retorna el mismo vector.
;; TODO: validar que valor coincida con el tipo del vector.
(define vec-append
  (lambda (v valor)
    (let ((nueva-lista (append (vector->list (vec-datos v)) (list valor))))
      (vector-set! v 0 (list->vector nueva-lista))
      v)))

;; vec-delete : vector x entero -> vector
;; Elimina el elemento de la posición pos, desplazando a la izquierda los
;; siguientes (error si pos está fuera de rango). Retorna el mismo vector.
(define vec-delete
  (lambda (v pos)
    (let ((datos (vec-datos v)))
      (if (or (< pos 0) (>= pos (vector-length datos)))
          (eopl:error 'delete-val-vector "Indice fuera de rango")
          (begin
            (vector-set! v 0 (list->vector (lista-quitar (vector->list datos) pos)))
            v)))))

;; vec-insert : vector x entero x valor -> vector
;; Inserta valor en la posición pos, desplazando a la derecha los siguientes
;; (error si pos está fuera de rango). Retorna el mismo vector.
;; TODO: validar que valor coincida con el tipo del vector.
(define vec-insert
  (lambda (v pos valor)
    (let ((datos (vec-datos v)))
      (if (or (< pos 0) (> pos (vector-length datos)))
          (eopl:error 'insert-val-vector "Indice fuera de rango")
          (begin
            (vector-set! v 0 (list->vector (lista-insertar (vector->list datos) pos valor)))
            v)))))

;; aplicar-primitiva-coleccion : symbol x (listof valor) -> valor
;; Solo se llama cuando es-primitiva-coleccion? es verdadero.
(define aplicar-primitiva-coleccion
  (lambda (nombre args)
    (cond
      ((eqv? nombre 'empty?)
       (exigir-argumentos nombre args 1) (lista-empty? (car args)))
      ((eqv? nombre 'head)
       (exigir-argumentos nombre args 1) (lista-head (car args)))
      ((eqv? nombre 'tail)
       (exigir-argumentos nombre args 1) (lista-tail (car args)))
      ((eqv? nombre 'make-list)
       (exigir-argumentos nombre args 2) (lista-make (car args) (cadr args)))
      ((eqv? nombre 'append)
       (exigir-argumentos nombre args 2) (lista-append (car args) (cadr args)))
      ((eqv? nombre 'ref-list)
       (exigir-argumentos nombre args 2) (lista-ref (car args) (cadr args)))
      ((eqv? nombre 'length)
       (exigir-argumentos nombre args 1) (lista-length (car args)))
      ((eqv? nombre 'list?)
       (exigir-argumentos nombre args 1) (lista-es? (car args)))
      ((eqv? nombre 'vector?)
       (exigir-argumentos nombre args 1) (vec-es? (car args)))
      ((eqv? nombre 'ref-vector)
       (exigir-argumentos nombre args 2) (vec-ref (car args) (cadr args)))
      ((eqv? nombre 'set-vector)
       (exigir-argumentos nombre args 3) (vec-set! (car args) (cadr args) (caddr args)))
      ((eqv? nombre 'make-vector)
       (exigir-argumentos nombre args 1) (vec-make (car args)))
      ((eqv? nombre 'append-vector)
       (exigir-argumentos nombre args 2) (vec-append (car args) (cadr args)))
      ((eqv? nombre 'delete-val-vector)
       (exigir-argumentos nombre args 2) (vec-delete (car args) (cadr args)))
      ((eqv? nombre 'insert-val-vector)
       (exigir-argumentos nombre args 3) (vec-insert (car args) (cadr args) (caddr args)))
      (else
       (eopl:error 'aplicar-primitiva-coleccion "~s no es una primitiva" nombre))
      )
    )
  )
  
;; ============================================================
;; Ambiente
;; ============================================================
;; Datatype para representar una referencia a una celda en memoria
(define-datatype referencia referencia?
  (una-ref (pos integer?)
           (vec vector?)))

;; Extrae el valor actual almacenado en la referencia
(define leer-ref
  (lambda (ref)
    (cases referencia ref
      (una-ref (pos vec) (car (vector-ref vec pos)))
      )
    )
  )

;; Modifica el valor de la referencia si no es constante
(define cambiar-ref!
  (lambda (ref nuevo-val)
    (cases referencia ref
      (una-ref (pos vec)
        (let ((celda (vector-ref vec pos)))
          (if (cdr celda)
              (eopl:error 'cambiar-ref! "No se puede modificar una constante")
              (begin (vector-set! vec pos (cons nuevo-val #f)) nuevo-val))
          )
        )
      )
    )
  )

;; Datatype del ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio-reg)
  (ambiente-extendido-reg
   (syms (list-of symbol?))
   (vec vector?)
   (env ambiente?)))

;; Crea un ambiente vacío
(define ambiente-vacio
  (lambda ()
    (ambiente-vacio-reg)
    )
  )

;; Extiende un ambiente con nuevos símbolos y sus celdas (valor . es-constante?)
(define extender-ambiente
  (lambda (syms celdas env)
    (ambiente-extendido-reg syms (list->vector celdas) env)
    )
  )

;; Busca la posición de un símbolo en una lista (retorna índice o #f)
(define buscar-posicion
  (lambda (sym syms)
    (let buscar ((ls syms) (pos 0))
      (cond
        ((null? ls) #f)
        ((eqv? sym (car ls)) pos)
        (else (buscar (cdr ls) (+ pos 1))))
      )
    )
  )

;; Busca un símbolo y retorna su referencia (para leer o asignar)
(define aplicar-ambiente-ref
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio-reg ()(eopl:error 'aplicar-ambiente-ref "Variable no definida: ~s" sym))
      (ambiente-extendido-reg (syms vec env-anterior)
        (let ((pos (buscar-posicion sym syms)))
          (if (number? pos)
              (una-ref pos vec)
              (aplicar-ambiente-ref env-anterior sym))
          )
        )
      )
    )
  )

;; Busca un símbolo y retorna directamente su valor
(define aplicar-ambiente
  (lambda (env sym)
    (leer-ref (aplicar-ambiente-ref env sym))
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
        (evaluar-expresion exp (evaluar-declaraciones decls (ambiente-vacio)))
         )
      )
    )
  )


(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (operacion-exp (primer prims resto)
        (cond
          ((null? prims)
           (aplicar-operando primer env))
          ((equal? (car prims) (primitiva-asignacion))
           (evaluar-asignacion primer prims resto env))
          (else
           (let ((res (aplicar-primitiva-binaria
                        (car prims)
                        (list (aplicar-operando primer env)
                              (aplicar-operando (car resto) env)))))
              (evaluar-expresion
                (operacion-exp
                  (if (number? res)
                      (numero-lit res)
                      (if res
                          (true-lit)
                          (false-lit)
                       )
                   )
                  (cdr prims)(cdr resto))
                env)
              )
            )
          )
        )
      )
    )
  )

(define evaluar-sentencia
  (lambda (sent env)
    (cases sentencia sent
      (una-sentencia (exp)(evaluar-expresion exp env))
      )
    )
  )

(define evaluar-bloque
  (lambda (block env)
    (cases bloque block
      (un-bloque (sentencia lista-sentencias)
        (if (null? lista-sentencias)
            (evaluar-sentencia sentencia env)
            (begin
              (evaluar-sentencia sentencia env)
              (evaluar-bloque (un-bloque (car lista-sentencias) (cdr lista-sentencias)) env)
             )
          )
        )
      )
    )
  )

(define evaluar-locales
  (lambda (local env)
    (cases locales local
      (variableLocal-exp (declaracion lista-declaraciones sentencia lista-sentencias)
        (let ((nuevo-env (evaluar-declaraciones (cons declaracion lista-declaraciones) env)))
          (evaluar-bloque (un-bloque sentencia lista-sentencias) nuevo-env)
          )
        )
      )
    )
  )

(define evaluar-declaraciones
  (lambda (decls env)
    (if (null? decls)
        env
        (cases declaracion (car decls)
          (var-decl (tipo id exp)
            (let ((val (evaluar-expresion exp env)))
              (evaluar-declaraciones (cdr decls)
                (extender-ambiente (list id) (list (cons val #f)) env)
                )
              )
            )
          (const-decl (tipo id exp)
             (let ((val (evaluar-expresion exp env)))
              (evaluar-declaraciones (cdr decls)
                (extender-ambiente (list id) (list (cons val #t)) env)
                )
              )
            )
          (proc-decl (id exp)
            ;; El nombre se agrega al ambiente ANTES de evaluar el lambda, así la
            ;; clausura captura un ambiente que ya contiene al procedimiento (recursión).
            (let* ((nuevo-env (extender-ambiente (list id)
                                                 (list (cons 'proc-pendiente #t))
                                                 env))
                   (val (evaluar-expresion exp nuevo-env)))
              (if (procVal? val)
                  (begin
                    (inicializar-ref! (aplicar-ambiente-ref nuevo-env id) val)
                    (evaluar-declaraciones (cdr decls) nuevo-env))
                  (eopl:error 'proc-decl "~s debe ser un procedimiento (lambda)" id)
                  )
              )
            )
          )
        )
    )
  )


(define evaluar-asignacion
  (lambda (primer prims resto env)
    (cases operando primer
      (id-exp (nombre sufijo)
        (cases sufijo-id sufijo
          (var-sufijo ()
            (let ((valor-nuevo (evaluar-expresion
                                 (operacion-exp (car resto) (cdr prims) (cdr resto))
                                 env)))
              (cambiar-ref! (aplicar-ambiente-ref env nombre) valor-nuevo)
              )
            )
          (else (eopl:error 'evaluar-asignacion "No se puede asignar a una llamada de funcion"))
          )
        )
      (else (eopl:error 'evaluar-asignacion "El lado izquierdo de -> debe ser una variable"))
      )
    )
  )

(define probar
  (lambda (texto)
    (evaluar-programa (scan&parse texto))
    )
  )



;; ============================================================
;; Procedimientos (clausuras)
;; ============================================================
;; Un procedimiento es una clausura: tipos y nombres de los parámetros,
;; la expresión del return y el ambiente donde se creó el lambda.
;; Los valores denotados siguen siendo referencias a celdas (valor . es-constante?).
(define-datatype procVal procVal?
  (cerradura (tipos (list-of tipo?))
             (params (list-of symbol?))
             (cuerpo-proc expresion?)
             (amb ambiente?)))

;; Asigna el valor a una referencia SIN revisar si es constante.
;; Solo se usa para inicializar un proc-decl, cuyo nombre se crea antes
;; (con valor pendiente) para que el procedimiento pueda llamarse a sí mismo.
(define inicializar-ref!
  (lambda (ref val)
    (cases referencia ref
      (una-ref (pos vec) (vector-set! vec pos (cons val #t)))
      )
    )
  )



;; ============================================================
;; Procedimientos y estructuras de control
;; ============================================================

;; aplicar-procedimiento : valor x (listof valor) x symbol -> valor
;; Paso de parámetros por valor: cada argumento ya llega evaluado y se copia
;; en una celda nueva (asignar al parámetro no cambia la variable del llamador).
;; El cuerpo se evalúa en la extensión del ambiente de la clausura.
;; TODO: validar aquí el tipo de cada parámetro (la clausura guarda los tipos).
(define aplicar-procedimiento
  (lambda (proc args nombre)
    (if (not (procVal? proc))
        (eopl:error 'aplicar-procedimiento "~s no es un procedimiento" nombre)
        (cases procVal proc
          (cerradura (tipos params cuerpo-proc env-clausura)
            (if (not (= (length params) (length args)))
                (eopl:error 'aplicar-procedimiento
                            "El procedimiento ~s espera ~s argumento(s) y recibio ~s"
                            nombre (length params) (length args))
                (evaluar-expresion
                  cuerpo-proc
                  (extender-ambiente params
                                     (map (lambda (arg) (cons arg #f)) args)
                                     env-clausura))
                )
            )
          )
        )
    )
  )

;; valor-verdad? : valor -> boolean
;; 0 es falso y cualquier otro número es verdadero (también acepta #t / #f)
(define valor-verdad?
  (lambda (v)
    (cond
      ((boolean? v) v)
      ((number? v) (not (= v 0)))
      (else (eopl:error 'valor-verdad? "La condicion debe ser numerica o booleana: ~s" v))
      )
    )
  )

;; El valor de un cuerpo es el de su última sentencia (igual que BLOCK)
(define evaluar-cuerpo
  (lambda (cuerpo-eval env)
    (cases cuerpo cuerpo-eval
      (un-cuerpo (primera restantes)
        (evaluar-bloque (un-bloque primera restantes) env))
      )
    )
  )

(define evaluar-if
  (lambda (condicion cuerpo-si cuerpo-no env)
    (if (valor-verdad? (evaluar-expresion condicion env))
        (evaluar-cuerpo cuerpo-si env)
        (evaluar-cuerpo cuerpo-no env))
    )
  )

;; Se compara con la misma primitiva que usa el operador ==
(define evaluar-switch
  (lambda (exp casos cuerpo-default env)
    (let ((val (evaluar-expresion exp env)))
      (let buscar ((cs casos))
        (if (null? cs)
            (evaluar-cuerpo cuerpo-default env)
            (cases caso (car cs)
              (un-caso (exp-caso cuerpo-caso)
                (if (aplicar-primitiva-binaria
                      (primitiva-comparador-igual)
                      (list val (evaluar-expresion exp-caso env)))
                    (evaluar-cuerpo cuerpo-caso env)
                    (buscar (cdr cs))))
              )
            )
        )
      )
    )
  )

;; while y for retornan 'void
(define evaluar-while
  (lambda (condicion cuerpo-while env)
    (let ciclo ()
      (if (valor-verdad? (evaluar-expresion condicion env))
          (begin
            (evaluar-cuerpo cuerpo-while env)
            (ciclo))
          'void)
      )
    )
  )

(define evaluar-for
  (lambda (inicio condicion paso cuerpo-for env)
    (evaluar-expresion inicio env)
    (let ciclo ()
      (if (valor-verdad? (evaluar-expresion condicion env))
          (begin
            (evaluar-cuerpo cuerpo-for env)
            (evaluar-expresion paso env)
            (ciclo))
          'void)
      )
    )
  )

;; print: muestra los argumentos separados por espacio y un salto de línea.
;; Los booleanos se muestran como True / False. Retorna 'void.
(define imprimir-valor
  (lambda (v)
    (cond
      ((boolean? v) (display (if v "True" "False")))
      ((procVal? v) (display "<procedimiento>"))
      (else (display v))
      )
    )
  )

(define evaluar-print
  (lambda (args env)
    (let imprimir ((vals (map (lambda (arg) (evaluar-expresion arg env)) args)))
      (cond
        ((null? vals) (newline))
        (else
         (imprimir-valor (car vals))
         (if (not (null? (cdr vals))) (display " ") 'ok)
         (imprimir (cdr vals)))
        )
      )
    'void
    )
  )


;; ============================================================
;; Extensión del evaluador
;; (la invocación f(args) y proc-decl quedaron integradas directamente en
;; aplicar-operando y evaluar-declaraciones)
;; ============================================================

;; evaluar-construccion : <construccion> x ambiente -> valor
;; (la llama aplicar-operando mediante construccion-exp)
(define evaluar-construccion
  (lambda (constr env)
    (cases construccion constr
      (lambda-exp (tipos ids exp) (cerradura tipos ids exp env))
      (print-exp (args) (evaluar-print args env))
      (if-exp (condicion cuerpo-si cuerpo-no)
              (evaluar-if condicion cuerpo-si cuerpo-no env))
      (switch-exp (exp casos cuerpo-default)
                  (evaluar-switch exp casos cuerpo-default env))
      (while-exp (condicion cuerpo-while)
                 (evaluar-while condicion cuerpo-while env))
      (for-exp (inicio condicion paso cuerpo-for)
               (evaluar-for inicio condicion paso cuerpo-for env))
      )
    )
  )


;; ============================================================
;; Ejemplos de procedimientos y estructuras de control
;; Cada ejemplo se parsea con scan&parse, se evalúa y se compara con lo esperado.
;; Uso:  (ejemplos-procedimientos-control)
;; ============================================================

;; Arma un programa: globals son las declaraciones y expr la expresión del return de main
(define armar-programa-ejemplo
  (lambda (globals expr)
    (string-append "GLOBALS { " globals " } PROGRAM { proc main ( ) { return "
                   expr " } }")
    )
  )

(define probar-ejemplo
  (lambda (nombre texto esperado)
    (let ((obtenido (evaluar-programa (scan&parse texto))))
      (if (equal? obtenido esperado)
          (begin (display "[OK]    ") (display nombre) (newline))
          (begin (display "[FALLO] ") (display nombre)
                 (display "   esperado: ") (display esperado)
                 (display "   obtenido: ") (display obtenido) (newline))
          )
      )
    )
  )

(define ejemplos-procedimientos-control
  (lambda ()
    (display "--- Ejemplos de procedimientos y estructuras de control ---") (newline)

    ;; proc-decl, lambda e invocación
    (probar-ejemplo "proc + lambda + invocacion: square(a)"
      (armar-programa-ejemplo "int a = 10; proc square = lambda ( int x ) { return x * x; }"
                         "square(a);")
      100)
    (probar-ejemplo "lambda con varios parametros"
      (armar-programa-ejemplo "proc suma = lambda ( int x , int y ) { return x + y; }"
                         "suma(2, 5);")
      7)
    (probar-ejemplo "lambda sin parametros"
      (armar-programa-ejemplo "proc cinco = lambda ( ) { return 5; }" "cinco();")
      5)
    (probar-ejemplo "paso por valor (el llamador no cambia)"
      (armar-programa-ejemplo "int a = 5; proc inc = lambda ( int x ) { return BLOCK { x -> x + 1; x; }; }"
                         "BLOCK { inc(a); a; }")
      5)
    (probar-ejemplo "clausura: captura el ambiente donde se creo"
      (armar-programa-ejemplo "proc make_adder = lambda ( int n ) { return lambda ( int x ) { return x + n; }; }"
                         "LOCALS { proc add5 = make_adder(5) } { add5(3); }")
      8)
    (probar-ejemplo "procedimiento que usa una variable global"
      (armar-programa-ejemplo "int base = 100; proc masbase = lambda ( int x ) { return x + base; }"
                         "masbase(1);")
      101)

    ;; print
    (probar-ejemplo "print con varios argumentos (imprime y retorna el ultimo valor)"
      (armar-programa-ejemplo "" "BLOCK { print(\"Hola\", 1, 2.5, True); 7; }")
      7)

    ;; if / else
    (probar-ejemplo "if / else anidado"
      (armar-programa-ejemplo "" "LOCALS { int edad = 15; } { if ( edad < 13 ) { \"Nino\"; } else { if ( edad < 18 ) { \"Adolescente\"; } else { \"Adulto\"; }; }; }")
      "Adolescente")
    (probar-ejemplo "if: 0 es falso"
      (armar-programa-ejemplo "" "if ( 0 ) { 1; } else { 2; }")
      2)
    (probar-ejemplo "if: cualquier otro numero es verdadero"
      (armar-programa-ejemplo "" "if ( 7 ) { 1; } else { 2; }")
      1)

    ;; switch
    (probar-ejemplo "switch: case"
      (armar-programa-ejemplo "string color = \"verde\";"
        "switch color { case \"rojo\" : { \"Detente\"; } case \"verde\" : { \"Sigue\"; } default : { \"Color desconocido\"; } }")
      "Sigue")
    (probar-ejemplo "switch: default"
      (armar-programa-ejemplo "string color = \"azul\";"
        "switch color { case \"rojo\" : { \"Detente\"; } case \"verde\" : { \"Sigue\"; } default : { \"Color desconocido\"; } }")
      "Color desconocido")

    ;; while y for
    (probar-ejemplo "while: suma de 1 a max_num"
      (armar-programa-ejemplo "int max_num = 5;"
        "LOCALS { int count = 1; int total = 0; } { while ( count <= max_num ) { total -> total + count; count -> count + 1; }; total; }")
      15)
    (probar-ejemplo "for: factorial iterativo"
      (armar-programa-ejemplo "proc factorial = lambda ( int num ) { return LOCALS { int result = 1; int i = 1; } { for ( i -> 1 ; i <= num ; i -> i + 1 ) { result -> result * i; }; result; }; }"
                         "factorial(5);")
      120)
    (probar-ejemplo "while: factorial iterativo"
      (armar-programa-ejemplo "proc factorial = lambda ( int num ) { return LOCALS { int result = 1; int i = 1; } { while ( i <= num ) { result -> result * i; i -> i + 1; }; result; }; }"
                         "factorial(5);")
      120)

    ;; recursion
    (probar-ejemplo "recursion: factorial"
      (armar-programa-ejemplo "proc factorial = lambda ( int n ) { return if ( n <= 1 ) { 1; } else { n * factorial(n - 1); }; }"
                         "factorial(5);")
      120)
    (probar-ejemplo "recursion: fibonacci"
      (armar-programa-ejemplo "proc fib = lambda ( int n ) { return if ( n < 2 ) { n; } else { fib(n - 1) + fib(n - 2); }; }"
                         "fib(10);")
      55)

    ;; LOCALS + proc: area de un triangulo
    (probar-ejemplo "LOCALS: area de un triangulo (triangle)"
      (armar-programa-ejemplo "float base = 10.5; proc triangle = lambda ( float base , float height ) { return LOCALS { float area = 0; float b = base; float h = height; } { print(\"Primera instruccion\"); area -> b * h / 2; area; }; }"
                         "triangle(base, 6.4);")
      33.6)

    (newline)
    (display "Casos que producen ERROR (escribalos dentro de un programa para probarlos):") (newline)
    (display "  square(1, 2)                  -> numero de argumentos incorrecto") (newline)
    (display "  a(1)   (con int a = 3)        -> a no es un procedimiento") (newline)
    (display "  if ( \"x\" ) { 1; } else { 2; }  -> la condicion debe ser numerica o booleana") (newline)
    (display "  proc p = 5                    -> p debe ser un procedimiento (lambda)") (newline)
    (display "  f(1)   (f no definida)        -> Variable no definida") (newline)
    )
  )

;; ============================================================
;; Ejemplos de listas y vectores
;; ============================================================
;; Cada ejemplo escribe un programa de LogicFusion como texto, lo pasa por
;; scan&parse, lo evalúa y compara con el resultado esperado.
;; Uso:  (ejemplos-colecciones)

(define ejemplos-colecciones
  (lambda ()
    (display "--- Ejemplos de listas y vectores ---") (newline)

    ;; listas
    (probar-ejemplo "empty: la lista vacia"
      (armar-programa-ejemplo "" "empty")
      '())
    (probar-ejemplo "literal de lista asignado a una variable"
      (armar-programa-ejemplo "list<int> nums = [1, 2, 3];" "nums")
      '(1 2 3))
    (probar-ejemplo "empty? sobre lista vacia"
      (armar-programa-ejemplo "" "empty?(empty)")
      #t)
    (probar-ejemplo "empty? sobre lista con elementos"
      (armar-programa-ejemplo "" "empty?([1])")
      #f)
    (probar-ejemplo "head"
      (armar-programa-ejemplo "" "head([10, 20, 30])")
      10)
    (probar-ejemplo "tail"
      (armar-programa-ejemplo "" "tail([10, 20, 30])")
      '(20 30))
    (probar-ejemplo "make-list"
      (armar-programa-ejemplo "" "make-list(1, [2, 3])")
      '(1 2 3))
    (probar-ejemplo "append"
      (armar-programa-ejemplo "" "append([1, 2], [3, 4])")
      '(1 2 3 4))
    (probar-ejemplo "ref-list"
      (armar-programa-ejemplo "" "ref-list([10, 20, 30], 1)")
      20)
    (probar-ejemplo "length"
      (armar-programa-ejemplo "" "length([5, 6, 7])")
      3)
    (probar-ejemplo "list? sobre una lista"
      (armar-programa-ejemplo "" "list?([1])")
      #t)
    (probar-ejemplo "list? sobre un numero"
      (armar-programa-ejemplo "" "list?(42)")
      #f)

    ;; vectores
    (probar-ejemplo "literal de vector (es un envoltorio)"
      (armar-programa-ejemplo "" "#[10, 20, 30]")
      (vec-nuevo (vector 10 20 30)))
    (probar-ejemplo "vector? sobre un vector"
      (armar-programa-ejemplo "" "vector?(#[1, 2])")
      #t)
    (probar-ejemplo "vector? sobre una lista"
      (armar-programa-ejemplo "" "vector?([1, 2])")
      #f)
    (probar-ejemplo "make-vector"
      (armar-programa-ejemplo "" "make-vector(3)")
      (vec-nuevo (vector 0 0 0)))
    (probar-ejemplo "ref-vector"
      (armar-programa-ejemplo "" "ref-vector(#[10, 20, 30], 1)")
      20)
    (probar-ejemplo "set-vector"
      (armar-programa-ejemplo "vector<int> v = #[1, 2, 3];"
        "BLOCK { set-vector(v, 1, 50); ref-vector(v, 1); }")
      50)
    (probar-ejemplo "append-vector"
      (armar-programa-ejemplo "vector<int> v = #[10, 20];"
        "BLOCK { append-vector(v, 30); ref-vector(v, 2); }")
      30)
    (probar-ejemplo "delete-val-vector (ejemplo del enunciado)"
      (armar-programa-ejemplo "vector<int> v = #[10, 20, 30, 40];"
        "BLOCK { delete-val-vector(v, 1); ref-vector(v, 1); }")
      30)
    (probar-ejemplo "insert-val-vector (ejemplo del enunciado)"
      (armar-programa-ejemplo "vector<int> v = #[10, 20, 30, 40];"
        "BLOCK { insert-val-vector(v, 1, -5); ref-vector(v, 1); }")
      -5)
    (probar-ejemplo "dos variables comparten el mismo vector"
      (armar-programa-ejemplo "vector<int> v = #[10, 20]; vector<int> w = v;"
        "BLOCK { set-vector(v, 0, 99); ref-vector(w, 0); }")
      99)

    (newline)
    (display "Casos que producen ERROR (escribalos dentro de un programa para probarlos):") (newline)
    (display "  head(empty)                   -> head: La lista esta vacia") (newline)
    (display "  tail(empty)                   -> tail: La lista esta vacia") (newline)
    (display "  ref-list([1, 2], 5)           -> ref-list: Indice fuera de rango") (newline)
    (display "  head([1], [2])                -> numero de argumentos incorrecto") (newline)
    (display "  ref-vector(#[1, 2], 2)        -> ref-vector: Indice fuera de rango") (newline)
    (display "  delete-val-vector(#[1], 9)    -> delete-val-vector: Indice fuera de rango") (newline)
    (display "  make-vector(-2)               -> make-vector: El tamano no puede ser negativo") (newline)
    )
  )