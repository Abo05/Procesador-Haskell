# Procesador de Lenguaje en Haskell

Este proyecto implementa un procesador de lenguaje para un lenguaje formal simplificado basado en JavaScript. 

## Estructura del Proyecto

El repositorio está organizado como un paquete estándar de Cabal:

* **`app/`**: Contiene `Procesador.hs`, el orquestador principal y punto de entrada de la mónada `IO`.
* **`src/`**: Contiene la lógica pura del procesador:
  * `Alex.hs`: Analizador Léxico.
  * `Asin.hs`: Analizador Sintáctico.
  * `TablaLL.hs` y `Reglas.hs`: Definición de la matriz y reglas gramaticales.
  * `Token.hs` y `Simbolos.hs`: Data Type's del modelo de datos.
  * `GErrores.hs`: Gestor de errores.
  * `TablaSimbolos.hs`: Estructura para el registro de variables y funciones.
* **`input/`**: Directorio donde hay unos pocos ficheros de prueba, se ha pensado para dejar ahí los ficheros a pasar para una mejor orgamización
* **`output/`**: Directorio donde el procesador generará los resultados del análisis (`Tokens.txt`, `TablaSimbolos.txt`, `Parse.txt`).
* **`memoria/`**: Documentación técnica detallada del proyecto (LaTeX y PDF).

## Requisitos y Compilación

Para compilar y ejecutar el proyecto necesitas tener instalado **GHC** (Glasgow Haskell Compiler) y la herramienta **Cabal**.

1. **Clonar el repositorio:**
   ```bash
   git clone <URL_DEL_REPOSITORIO>
   cd Procesador
   ```

2. **Compilar el proyecto**
   ```bash
   cabal build
   ```

3. **Uso del procesador**
    ```bash
    cabal run Procesador -- <fichero>
   ```

## Resultados y salida
Tras una ejecución exitosa (o con recuperación de errores), el procesador volcará los resultados estructurados en la carpeta output/:

1. Tokens.txt: El flujo secuencial de tokens detectados con sus respectivos atributos.

2. TablaSimbolos.txt: El registro de identificadores (variables y funciones) insertados durante el análisis léxico.

3. Parse.txt: La traza de derivación sintáctica (índices de las reglas aplicadas).

Además, si el código fuente contiene fallos, se imprimirán por la salida estándar de error indicando la línea exacta y el motivo del fallo.

## Autores
Álvaro Acedo Blanco
Daniel Czepiel Babiarz
