
# Como usar la librería

La clase THaruPdf intenta proveer las mismas funciones de la clase TPrinter de
FW de modo que se pueda usar en su reemplazo con el mínimo de cambios.



## Declaración del objeto de impresión

Donde antes escribíamos

    DEFINE PRINTER oPrint .....

ahora escribimos

    oPrint:= THaruPdf():New()

opcionalmente podemos indicar desde el principio el nombre del pdf y los valores
de las contraseńas. Los parámetros completos son

 oPrint:= THaruPdf():New( cFileName, cPassword, cOwnerPassword, nPermission, lPreview )

cPassword es la clave para acceder al pdf con los limites indicados por
nPermission, cOwnerPassword es para poder modificarlo.
nPermission se establece por defecto como HPDF_ENABLE_READ + HPDF_ENABLE_PRINT + HPDF_ENABLE_COPY
Los parámetros de seguridad son totalmente opcionales.
lPreview indica si, al final de la generación, el programa debe lanzar la visualización del pdf generado. 
Para ello la librería lanzará el gestor de apertura que el usuario tenga definido en el sistema operativo de 
forma automática. Este comportamiento se puede cambiar definiendo el codeblock :bPreview de la clase.

Si estos valores se establecen al crear el objeto, cuando se invoque a End()
automáticamente grabará el pdf. Si no se indica nada al crear el objeto, se graba
manualmente con el método Save

    oPrint:Save( cNombreDelPdf ) //  --> devuelve un valor numérico

El método Save() devuelve un valor numérico. Si todo está bien, devuelve 0, un
valor diferente de 0 indica algún error de Haru. Véase la table de errores respectiva en la propia librería.

## Inicio y Fin de página

    oPrint:StartPage()
    oPrint:EndPage()

## Uso de comandos
Opcionalmente están disponibles algunos comandos equivalentes a los propios de la impresión con TPrinter, definidos
el el include 'HaruPrint.prg'

- Declaración del objeto THaruPdf
´´´
	PRINT <oPrint> TO HARU [ FILE <cFile> ] ;
          [ <lPreview: PREVIEW> ] ;
          [ USER PASS <cUserpass>  ] ;
          [ OWNER PASS <cOwnerpass>  ] ;
          [ PERMISION  <nPermision>  ] ;
´´´
- Inicio y fin de página
	PAGE
	ENDPAGE
- Fin de la impresión

	ENDPRINT


## Fuentes

La declaración es diferente. Con FW se escribía
    DEFINE FONT oFont1 NAME "COURIER NEW" SIZE 0, -8 Of oPrint

con THaruPdf :

    oFont1:= oPrint:DefineFont( 'Courier', 8 )

Un detalle importante es que tipo de fuente utilizamos, si es una predefinida, o
una fuente TrueType.

Las fuentes predefinidas (conocidas como Base14) son las más livianas en términos
del tamańo del pdf generado, porque no se insertan en el pdf al estar ya incluídas
en los lectores de pdf como el Acrobat Reader, Foxit, etc. Son las siguientes:

    Courier
    Courier-Bold
    Courier-Oblique
    Courier-BoldOblique
    Helvetica
    Helvetica-Bold
    Helvetica-Oblique
    Helvetica-BoldOblique
    Times-Roman
    Times-Bold
    Times-Italic
    Times-BoldItalic
    Symbol
    ZapfDingbats

Las fuentes TrueType deben ser incluídas en el pdf generado, por lo tanto el fi-
chero <fuente>.ttf debe estar disponible. El nombre de las fuentes no coincide,
en general, con el nombre del fichero, por lo que hay que incluir el nombre del
fichero en la declaración de las fuentes usando la función:

     HaruAddFont( cFontName, cTtfFile )

*cFontName* es el nombre de la fuente, y *cTtfFile* el fichero .ttf que lo
define.
Por comodidad y frecuencia de uso, ya están predefinidas las siguientes fuentes,
por lo que no es necesario declararlas.

    Arial
    Verdana
    Courier New
    Calibri
    Tahoma

Tambien, por comodidad, la función HaruAddFont busca el fichero en el directorio
actual, y si no lo encuentra, en el directorio de fuentes de Windows. Podemos
declarar un directorio alternativo para incluir nuestras propias fuentes con la
función:

    SetHaruFontDir(cDir)

donde cDir es el directorio donde queremos que busque las fuentes.

## imagenes
De momento la librería solo soporta imágenes en formato PNG y JPG.
Atención/Achtung/Warning: Las pruebas realizadas indican que usar pngs generados
con Paint producen pdfs enormes y son muy lentos.
La librería ha sido optimizada para cargar las imágenes de forma indexada, es
decir que cuando repetimos una imagen en varias páginas, la librería carga la imagen
dentro del pdf una sola vez, y usa la misma copia de imagen cada vez que se referencia,
por ejemplo como fondo de cada página.

## como guardar el fichero

No hace falta indicar ENDPRINT ni destruir las fuentes, es decir
    ENDPRINT no hace falta
    oFont:End() no hace falta

## poner contraseña.
La contraseńa del usuario y/o propietario se pueden indicar en el momento de
creación del objeto print, o bien asignar manualmente las DATAs de la clase
antes de llamar al método Save() o End()

    DATA nPermission
    DATA cPassword
    DATA cOwnerPassword

# Como enlazar
Además de incluir *PdfPrinter.lib*, se requiere *libhpdf.lib* y *png.lib*, que se
encuentran en el mismo directorio del proyecto de la librería, y las librerías
*hbhpdf.lib* y *hbzlib.lib* del propio Harbour.

# Límites
El límite más importante es el de la cantidad de páginas, que es bastante alto.
Generando nóminas con un bitmap de fondo se han generado pdfs de hasta 8000
páginas. Aparentemente superando esa cantidad de páginas se producen errores por
la falta de capacidad de las estructuras internas de los pdfs, que requiere una
reestructuración interna de la paginación que Haru no es capaz de hacer.

# Errores, cosas que faltan...
Como es una versión muy temprana, es probable que algo no funcione como se
espera o echemos en falta alguna funcionalidad. Para poder implementar nuevas
funciones o corregir/modificar las existentes, la clase está montada con el patrón
'variaciones protegidas', usando como fachada una clase intermedia vacía (THaruPDF),
que hereda toda la funcionalidad de la implementación real (THaruPDFBase).
Esto tiene como objeto permitirnos introducir ajustes y nuevas funciones sin tener
que modificar el código original. Si queremos modificar algo de la clase, EN
NUESTRO PROYECTO crearemos un nuevo prg y declararemos la clase THaruPDF que
herede de THaruPDFBase, podemos copiar el prg vacio que ya está en el proyecto
de la librería, y ańadiremos allí todo el nuevo código. Con esto lograremos que
la clase haga lo que queremos o ańadamos alguna prestación sin interferir con el
código público de la librería, usado en todos los proyectos.

    #include 'hbclass.ch'
    #include 'harupdf.ch'

    #define __NODEBUG__
    #include 'debug.ch'

    //------------------------------------------------------------------------------
    CLASS THaruPDF FROM THaruPDFBase
       // Clase intermedia para proveer de variaciones protegidas
    ENDCLASS


Cuando alguna nueva funcionalidad se incorpore a la librería, automáticamente se
incluirá en nuestro proyecto, sin afectar la parte local del código adicional que
se haya definido en nuestro proyecto.

