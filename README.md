## Integrantes

* **ANA SOFIA CANTILLO BOLAÑOS** — `202610112`
* **SARA JULISSA HERNANDEZ HERNANDEZ** — `202610102`
* **HECTOR DANIEL CARABALI VALENCIA** — `202410213`
* **SANTIAGO TORRES VANEGAS** — `202416930`

## Enlace al repositorio

[https://github.com/DanielValencia10/Proyecto-Final.git](https://github.com/DanielValencia10/Proyecto-Final.git)


## Ajustes de sintaxis respecto al enunciado

El enunciado permite "leves ajustes" a la sintaxis y pide que cada grupo proponga la de los vectores. Estas son las decisiones que tomamos y por qué.

### Literales de colecciones

| Estructura | Cómo se escribe en LogicFusion | Ejemplo |
|---|---|---|
| Lista vacía | `empty` | `list<int> nums = empty;` |
| Lista | corchetes `[ ]` | `list<int> nums = [1, 2, 3];` |
| Vector | `#[ ]` | `vector<int> v = #[1, 2, 3];` |
| Diccionario | llaves `{ }` con `clave : valor` | `{"Juan": 25, "Ana": 30}` |

En el enunciado las listas aparecen entre paréntesis, por ejemplo `(1, 2, 3)`. Los paréntesis ya se usan para agrupar expresiones, como en `(2 + 3) * 4`, y con ellos no se podría distinguir `(5)` de una lista de un solo elemento. Por eso las listas se escriben con corchetes, y los vectores llevan el prefijo `#` para no confundirse con ellas.

### Nombres de las primitivas

Los nombres de las primitivas llevan guion, como en el enunciado: `ref-list`, `make-list`, `make-vector`, `ref-vector`, `set-vector`, `append-vector`, `delete-val-vector` e `insert-val-vector`. Para que el analizador los lea como una sola palabra, el guion `-` se admite dentro de los identificadores. Consecuencia: la resta debe escribirse **con espacios** (`a - b`), porque `a-b` se lee como un único identificador.

### Primitivas de listas implementadas

`empty`, `empty?`, `head`, `tail`, `make-list`, `append`, `ref-list`, `length` y `list?`. Las listas son inmutables: `make-list`, `tail` y `append` devuelven una lista nueva y no modifican la original. `head` y `tail` sobre una lista vacía, y `ref-list` con un índice fuera de rango, producen un error.
