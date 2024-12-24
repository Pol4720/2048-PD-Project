# 2048-PD-Project

## ▎2048 Game in Haskell

## Descripción

+ This repository contains a fully implemented version of the popular game 2048, developed as a final project for a declarative programming course. Built using Haskell, this project showcases functional programming principles and leverages Haskell's powerful type system.

## Estructura del Proyecto

### game/

+ **stack.yaml**: Archivo de configuración para Stack, que incluye las dependencias necesarias para compilar y ejecutar el juego.

#### app/

+ **Main.hs**: Este es el punto de entrada del programa. Aquí se inicializa el juego, se configuran los bucles de eventos y se inicia la ejecución.

#### src/

Esta carpeta contiene el código fuente del juego.

+ **Game/**: Contiene la lógica principal del juego.
  + **Game.hs**: Controlador principal que gestiona el estado del juego y las interacciones entre los diferentes componentes.
  + **GameLogic.hs**: Implementa la lógica del juego, como mover y fusionar los tiles, así como verificar si el jugador ha ganado o perdido.
  + **Grid.hs**: Representa el tablero del juego, incluyendo la inicialización y actualización de la misma.
  + **Tile.hs**: Define la estructura de un tile en el juego, incluyendo su valor y posición.
  + **Score.hs**: Maneja el sistema de puntuación del juego.

+ **Graphics/**: Maneja la parte gráfica del juego.
  + **Graphics.hs**: Configuraciones generales relacionadas con la biblioteca Gloss.
  + **Render.hs**: Funciones para renderizar el tablero, los tiles y otros elementos visuales en pantalla.
  + **InputHandler.hs**: Gestiona los eventos de entrada del usuario, como pulsaciones de teclas.

+ **Config/**: Contiene configuraciones generales del juego.
  + **Config.hs**: Define constantes y configuraciones como el tamaño de el tablero, colores, etc.

#### tests/

Esta carpeta contiene pruebas unitarias y pruebas de integración para asegurar la calidad del código.

+ **GameTests.hs**: Pruebas relacionadas con la lógica del juego.
+ **GraphicsTests.hs**: Pruebas relacionadas con la parte gráfica.

### assets/

Esta carpeta almacena todos los recursos utilizados en el juego.

+ **images/**: Imágenes que se utilizarán en el juego (por ejemplo, sprites de los tiles).
+ **sounds/**: Archivos de sonido para efectos o música de fondo.
+ **fonts/**: Fuentes utilizadas para mostrar texto en el juego.

#### Otros Archivos

+ **README.md**: Este archivo proporciona información sobre el proyecto, su estructura y cómo ejecutarlo.

### Desarrollo y Ejecución

Para desarrollar y ejecutar  usando Visual Studio Code (VSCode), se necesitan algunas extensiones y herramientas específicas. A continuación, se detallan los requisitos y las vías de instalación desde la terminal.

#### ▎Requisitos

1. Haskell Platform: Necesitas tener Haskell instalado en tu sistema. Esto incluye el compilador GHC y el gestor de paquetes Cabal.

2. Extensiones de VSCode:

   • Haskell Language Server: Proporciona soporte para la edición de Haskell, incluyendo autocompletado, verificación de tipos y más.

   • Haskell Syntax Highlighting: Mejora la legibilidad del código Haskell con resaltado de sintaxis.

   • Code Runner (opcional): Permite ejecutar fragmentos de código directamente desde el editor.

#### ▎Instalación de Requerimientos

▎1. Instalación de Haskell

Dependiendo de tu sistema operativo, puedes instalar Haskell utilizando GHCup, que es una herramienta recomendada para gestionar las instalaciones de Haskell. Aquí te muestro cómo hacerlo:

▎En macOS o Linux

Abre la terminal y ejecuta:

`curl --proto '=https' --tlsv1.2 -sSf <https://get.haskellstack.org/> | sh`

Esto instalará Stack, que es un gestor de proyectos para Haskell y también instalará GHC y Cabal si no están presentes.

▎En Windows

Puedes usar el instalador de GHCup disponible en su página oficial (<https://www.haskell.org/downloads/>). Descarga el instalador y sigue las instrucciones.

▎2. Instalación de Extensiones de VSCode

Una vez que tengas VSCode instalado, puedes instalar las extensiones necesarias desde la terminal o directamente desde la interfaz gráfica.

▎Desde la terminal

Si tienes la línea de comandos de VSCode (code) configurada, puedes usar los siguientes comandos:

`code --install-extension haskell.haskell`

`code --install-extension formulahendry.code-runner`

▎Desde la Interfaz Gráfica

1. Abre VSCode.

2. Ve a la sección de extensiones (puedes usar el atajo Ctrl + Shift + X).

3. Busca "Haskell" y selecciona "Haskell Language Server" para instalarlo.

4. Busca "Code Runner" e instálalo si lo deseas.

▎Configuración del Proyecto

Una vez que tengas todo instalado:

1. Abre el proyecto y navega hasta game en VSCode:

   `code .`

2. Instala las dependencias necesarias ejecutando:

   `stack setup`

   `stack build`

▎Ejecución del Juego

Para ejecutar el juego, puedes usar:

`stack run`

Esto compilará y ejecutará la aplicación Haskell.

## Cómo Ejecutar el Juego

Sigue estos pasos:

1. Clona este repositorio en tu máquina local.
2. Navega a la carpeta del proyecto.
3. Ejecuta stack setup para configurar GHC y las dependencias.
4. Ejecuta stack run para iniciar el juego.

¡Disfruta jugando 2048 en Haskell!
