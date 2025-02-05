# 2048-PD-Project

## 2048 Haskell

## Descripcion

+ Este repositorio contiene una versión completamente implementada del popular juego 2048, desarrollado como proyecto final para un curso de programación declarativa. Construido usando Haskell, este proyecto muestra los principios de la programación funcional y aprovecha el potente sistema de tipos de Haskell.


## Estructura

Esta carpeta contiene el código fuente del juego. (en 2048Master tenemos otro juego 2048 pero sin una interfaz visual)


+ **Game2048/**: Contiene la lógica principal del juego
  + **Main.hs**: Controlador principal que gestiona el estado del juego y las interacciones entre los diferentes componentes.
  Implementa la lógica del juego, como mover y fusionar fichas, así como verificar si el jugador ha ganado o perdido.
  También representa el tablero del juego, incluyendo su inicialización y actualizaciones.
  Define la estructura de una ficha en el juego, incluyendo su valor y posición.
  Gestiona el sistema de puntuación del juego.
  También maneja la parte gráfica del juego.


### Ejecucion y desarrollo

Para desarrollar y ejecutar el juego usando Visual Studio Code (VSCode), necesitas algunas extensiones y herramientas específicas. A continuación se detallan los requisitos y los métodos de instalación desde la terminal.

#### Requisitos

1. Plataforma Haskell: Necesitas tener Haskell instalado en tu sistema. Esto incluye el compilador GHC y el gestor de paquetes Cabal.
Extensiones de VSCode:
2. • Haskell Language Server: Proporciona soporte para la edición de Haskell, incluyendo autocompletado, verificación de tipos y más.
• Haskell Syntax Highlighting: Mejora la legibilidad del código Haskell con resaltado de sintaxis.
• Code Runner (opcional): Permite ejecutar fragmentos de código directamente desde el editor.

#### Instalacion de requerimientos

Instalación de Haskell
Dependiendo de tu sistema operativo, puedes instalar Haskell usando GHCup, que es una herramienta recomendada para gestionar instalaciones de Haskell. Aquí te explicamos cómo hacerlo:

En macOS o Linux

Abre la terminal y ejecuta:

`curl --proto '=https' --tlsv1.2 -sSf <https://get.haskellstack.org/> | sh`

Esto instalará Stack, que es un gestor de proyectos para Haskell y también instalará GHC y Cabal si no están presentes.

En Windows

Puedes usar el instalador de GHCup disponible en su página oficial (https://www.haskell.org/downloads/). Descarga el instalador y sigue las instrucciones.

Instalación de Extensiones de VSCode
Una vez que tengas VSCode instalado, puedes instalar las extensiones necesarias desde la terminal o directamente desde la interfaz gráfica.

Desde la terminal

Si tienes la línea de comandos de VSCode (code) configurada, puedes usar los siguientes comandos:

`code --install-extension haskell.haskell`

`code --install-extension formulahendry.code-runner`

Desde la Interfaz Gráfica

Abre VSCode.
Ve a la sección de extensiones (puedes usar el atajo Ctrl + Shift + X).
Busca "Haskell" y selecciona "Haskell Language Server" para instalarlo.
Busca "Code Runner" e instálalo si lo deseas.
Configuración del Proyecto

Una vez que tengas todo instalado:

Abre el proyecto y navega a la carpeta del juego en VSCode:
code .
Instala las dependencias necesarias ejecutando:
`cabal build`
Ejecución del Juego

Para ejecutar el juego, puedes usar:

`cabal run`

Esto compilará y ejecutará la aplicación de Haskell.

## Como ejecutar el juego

Follow these steps:

Sigue estos pasos:

1. Clona este repositorio en tu máquina local.
2. Navega a la carpeta del proyecto.
3. Ejecuta cabal setup para configurar GHC y las dependencias.
4. Ejecuta cabal run para iniciar el juego.
5. ¡Disfruta jugando al 2048 en Haskell!
