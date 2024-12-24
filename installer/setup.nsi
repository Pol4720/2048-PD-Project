!define PRODUCT_NAME "2048 Game"
!define PRODUCT_VERSION "1.0"
!define PRODUCT_PUBLISHER "RM"
!define PRODUCT_URL "http://RichardYMauricio.com"

OutFile "2048-game-setup.exe"
InstallDir "$PROGRAMFILES${PRODUCT_NAME}"

Section "MainSection" SEC01
    SetOutPath "$INSTDIR"
    File "../dist/2048-game.exe"
    File "../dist/README.md"
    File "../assets/*.*" ; Copia todos los recursos necesarios
    CreateShortCut "$DESKTOP${PRODUCT_NAME}.lnk" "$INSTDIR2048-game.exe"
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR2048-game.exe"
    Delete "$INSTDIRREADME.md"
    Delete "$DESKTOP${PRODUCT_NAME}.lnk"
    RMDir "$INSTDIR"
SectionEnd
