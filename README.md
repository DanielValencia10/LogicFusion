# LogicFusion

LogicFusion es un intérprete desarrollado en Racket para un lenguaje educativo con
sintaxis propia. El proyecto utiliza la especificación léxica y sintáctica de
EOPL/SLLGEN y contiene un evaluador con ambientes, procedimientos, estructuras de
control y colecciones.

## Integrantes

* **ANA SOFIA CANTILLO BOLAÑOS** — `202610112`
* **SARA JULISSA HERNANDEZ HERNANDEZ** — `202610102`
* **HECTOR DANIEL CARABALI VALENCIA** — `202410213`
* **SANTIAGO TORRES VANEGAS** — `202416930`

## Requisitos

- Racket y DrRacket instalados.
- La librería EOPL, incluida habitualmente en las distribuciones de Racket.

## Cómo correr el proyecto

### Desde DrRacket

1. Abrir `LP-ProyectoFinal.rkt`.
2. Presionar **Run** o usar `Ctrl + R`.
3. Ejecutar una prueba desde el área interactiva:

```racket
(ejemplos-procedimientos-control)
(ejemplos-colecciones)
```

### Desde PowerShell

Desde la carpeta del proyecto:

```powershell
raco make LP-ProyectoFinal.rkt
racket LP-ProyectoFinal.rkt
```

Para abrir el intérprete interactivo y cargar el proyecto:

```powershell
racket -i -f LP-ProyectoFinal.rkt
```

También se pueden evaluar textos de LogicFusion directamente:

```racket
(probar "GLOBALS { } PROGRAM { proc main ( ) { return 1 + 2; } }")
```

## Funcionalidades implementadas

- Declaración de variables y constantes.
- Tipos `int`, `float`, `string`, `bool` y `void`.
- Operadores aritméticos, comparadores y asignación con `->`.
- Bloques `BLOCK` y variables temporales con `LOCALS`.
- Funciones lambda, procedimientos nombrados, clausuras y recursión.
- Paso de parámetros por valor.
- Impresión con `print`.
- Control de flujo con `if`, `else`, `switch`, `while` y `for`.
- Listas inmutables y vectores mutables.
- Ambiente basado en referencias y celdas.

## Ajustes de sintaxis respecto al enunciado

El enunciado permite "leves ajustes" a la sintaxis y pide que cada grupo proponga la de los vectores. Estas son las decisiones que tomamos y por qué.

### Literales de colecciones

| Estructura | Cómo se escribe en LogicFusion | Ejemplo |
|---|---|---|
| Lista vacía | `empty` | `list<int> nums = empty;` |
| Lista | corchetes `[ ]` | `list<int> nums = [1, 2, 3];` |
| Vector | `#[ ]` | `vector<int> v = #[1, 2, 3];` |
| Diccionario | llaves `{ }` con `clave : valor` | `{"Juan": 25, "Ana": 30}`* |

En el enunciado las listas aparecen entre paréntesis, por ejemplo `(1, 2, 3)`. Los paréntesis ya se usan para agrupar expresiones, como en `(2 + 3) * 4`, y con ellos no se podría distinguir `(5)` de una lista de un solo elemento. Por eso las listas se escriben con corchetes, y los vectores llevan el prefijo `#` para no confundirse con ellas.

\* La sintaxis de los diccionarios está representada en la gramática, pero su
evaluación todavía no está implementada.

### Nombres de las primitivas

Los nombres de las primitivas llevan guion, como en el enunciado: `ref-list`, `make-list`, `make-vector`, `ref-vector`, `set-vector`, `append-vector`, `delete-val-vector` e `insert-val-vector`. Para que el analizador los lea como una sola palabra, el guion `-` se admite dentro de los identificadores. Consecuencia: la resta debe escribirse **con espacios** (`a - b`), porque `a-b` se lee como un único identificador.

### Primitivas de listas implementadas

`empty`, `empty?`, `head`, `tail`, `make-list`, `append`, `ref-list`, `length` y `list?`. Las listas son inmutables: `make-list`, `tail` y `append` devuelven una lista nueva y no modifican la original. `head` y `tail` sobre una lista vacía, y `ref-list` con un índice fuera de rango, producen un error.

### Primitivas de vectores implementadas

| Primitiva | Descripción |
|---|---|
| `vector?(valor)` | Indica si el valor es un vector |
| `make-vector(n)` | Crea un vector de tamaño `n` con ceros |
| `ref-vector(v, i)` | Obtiene el elemento en la posición `i` |
| `set-vector(v, i, valor)` | Cambia el elemento en la posición `i` |
| `append-vector(v, valor)` | Agrega un elemento al final |
| `delete-val-vector(v, i)` | Elimina el elemento en la posición `i` |
| `insert-val-vector(v, i, valor)` | Inserta un elemento en la posición `i` |

Los índices comienzan en `0`. Las operaciones sobre vectores modifican el vector
original y varias variables pueden compartir el mismo vector.

## Ejemplo básico

```racket
GLOBALS {
  int numero = 5;
  proc factorial = lambda ( int n ) {
    return if ( n <= 1 ) { 1; } else { n * factorial(n - 1); };
  }
}
PROGRAM {
  proc main ( ) {
    return factorial(numero);
  }
}
```

## Pruebas incorporadas

El archivo incluye dos suites de ejemplos que verifican el resultado esperado:

- `(ejemplos-procedimientos-control)`: procedimientos, lambda, clausuras,
  recursión, impresión y estructuras de control.
- `(ejemplos-colecciones)`: listas, vectores y casos de error.

Cada prueba exitosa imprime `[OK]` y una prueba fallida informa el resultado
esperado y el obtenido.

## Estado actual y trabajos pendientes

- Los diccionarios están declarados en la gramática, pero su evaluación todavía
  no está implementada.
- Los tipos se almacenan en las celdas del ambiente, pero falta aplicar validación
  completa de tipos en todas las operaciones.
- `make-vector`, `append-vector` e `insert-val-vector` requieren validación contra
  el tipo declarado del vector.

## Estructura del proyecto

```text
LogicFusion/

├──.gitignore
├── LP-ProyectoFinal.rkt
├── LP-ProyectoFinal.rkt
└── README.md

```

## Enlace al repositorio

[https://github.com/DanielValencia10/Proyecto-Final.git](https://github.com/DanielValencia10/Proyecto-Final.git)
