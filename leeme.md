
# Como usar la librer�a

La clase THaruPdf intenta proveer las mismas funciones de la clase TPrinter de
FW de modo que se pueda usar en su reemplazo con el m�nimo de cambios.



## Declaraci�n del objeto de impresi�n

Donde antes escrib�amos

    DEFINE PRINTER oPrint .....

ahora escribimos

    oPrint:= THaruPdf():New()

opcionalmente podemos indicar desde el principio el nombre del pdf y los valores
de las contrase�as. Los par�metros completos son

 oPrint:= THaruPdf():New( cFileName, cPassword, cOwnerPassword, nPermission )

cPassword es la clave para acceder al pdf con los limites indicados por
nPermission, cOwnerPassword es para poder modificarlo.
nPermission se establece por defecto como HPDF_ENABLE_READ + HPDF_ENABLE_PRINT + HPDF_ENABLE_COPY
Los par�metros de seguridad son totalmente opcionales.

Si estos valores se establecen al crear el objeto, cuando se invoque a End()
autom�ticamente grabar� el pdf. Si no se indica nada al crear el objeto, se graba
manualmente con el m�todo Save

    oPrint:Save( cNombreDelPdf ) //  --> devuelve un valor num�rico

El m�todo Save() devuelve un valor num�rico. Si todo est� bien, devuelve 0, un
valor diferente de 0 indica alg�n error de Haru.

## Inicio y Fin de p�gina

    oPrint:StartPage()
    oPrint:EndPage()

## Fuentes

La declaraci�n es diferente. Con FW se escrib�a
    DEFINE FONT oFont1 NAME "COURIER NEW" SIZE 0, -8 Of oPrint

con THaruPdf :

    oFont1:= oPrint:DefineFont( 'Courier', 8 )

Un detalle importante es que tipo de fuente utilizamos, si es una predefinida, o
una fuente TrueType.

Las fuentes predefinidas (conocidas como Base14) son las m�s livianas en t�rminos
del tama�o del pdf generado, porque no se insertan en el pdf al estar ya inclu�das
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


Las fuentes TrueType deben ser inclu�das en el pdf generado, por lo tanto el fi-
chero <fuente>.ttf debe estar disponible. El nombre de las fuentes no coincide,
en general, con el nombre del fichero, por lo que hay que incluir el nombre del
fichero en la declaraci�n de las fuentes usando la funci�n:

     HaruAddFont( cFontName, cTtfFile )

*cFontName* es el nombre de la fuente, y *cTtfFile* el fichero .ttf que lo
define.
Por comodidad y frecuencia de uso, ya est�n predefinidas las siguientes fuentes,
por lo que no es necesario declararlas.

    Arial
    Verdana
    Courier New
    Calibri
    Tahoma

Tambien, por comodidad, la funci�n HaruAddFont busca el fichero en el directorio
actual, y si no lo encuentra, en el directorio de fuentes de Windows. Podemos
declarar un directorio alternativo para incluir nuestras propias fuentes con la
funci�n:

    SetHaruFontDir(cDir)

donde cDir es el directorio donde queremos que busque las fuentes.

## imagenes
De momento solo soporta im�genes en formato PNG y JPG.
Atenci�n/Achtung/Warning: Las pruebas realizadas indican que usar pngs generados
con Paint producen pdfs enormes y son muy lentos.
La librer�a ha sido optimizada para cargar las im�genes de forma indexada, es
decir que cuando repetimos una imagen en varias p�ginas, la librer�a carga la imagen
dentro del pdf una sola vez, y usa la misma copia de imagen cada vez que se referencia,
por ejemplo como fondo de cada p�gina.

## como guardar el fichero

No hace falta indicar ENDPRINT ni destruir las fuentes, es decir
    ENDPRINT no hace falta
    oFont:End() no hace falta

## poner contrase�a.
La contrase�a del usuario y/o propietario se pueden indicar en el momento de
creaci�n del objeto print, o bien asignar manualmente las DATAs de la clase
antes de llamar al m�todo Save() o End()

   DATA nPermission
   DATA cPassword
   DATA cOwnerPassword

# Como enlazar
Adem�s de incluir *PdfPrinter.lib*, se requiere *libhpdf.lib* y *png.lib*, que se
encuentran en el mismo directorio del proyecto de la librer�a, y las librer�as
*hbhpdf.lib* y *hbzlib.lib* del propio Harbour.

# L�mites
El l�mite m�s importante es el de la cantidad de p�ginas, que es bastante alto.
Generando n�minas con un bitmap de fondo se han generado pdfs de hasta 8000
p�ginas. Aparentemente superando esa cantidad de p�ginas se producen errores por
la falta de capacidad de las estructuras internas de los pdfs, que requiere una
reestructuraci�n interna de la paginaci�n que Haru no es capaz de hacer.

# Errores, cosas que faltan...
Como es una versi�n muy temprana, es probable que algo no funcione como se
espera o echemos en falta alguna funcionalidad. Para poder implementar nuevas
funciones o corregir/modificar las existentes, la clase est� montada con el patr�n
'variaciones protegidas', usando como fachada una clase intermedia vac�a (THaruPDF),
que hereda toda la funcionalidad de la implementaci�n real (THaruPDFBase).
Esto tiene como objeto permitirnos introducir ajustes y nuevas funciones sin tener
que modificar el c�digo original. Si queremos modificar algo de la clase, EN
NUESTRO PROYECTO crearemos un nuevo prg y declararemos la clase THaruPDF que
herede de THaruPDFBase, podemos copiar el prg vacio que ya est� en el proyecto
de la librer�a, y a�adiremos all� todo el nuevo c�digo. Con esto lograremos que
la clase haga lo que queremos o a�adamos alguna prestaci�n sin interferir con el
c�digo p�blico de la librer�a, usado en todos los proyectos.

#include 'hbclass.ch'
#include 'harupdf.ch'

#define __NODEBUG__
#include 'debug.ch'

//------------------------------------------------------------------------------
CLASS THaruPDF FROM THaruPDFBase
   // Clase intermedia para proveer de variaciones protegidas
ENDCLASS


Cuando alguna nueva funcionalidad se incorpore a la librer�a, autom�ticamente se
incluir� en nuestro proyecto, sin afectar la parte local del c�digo adicional que
se haya definido en nuestro proyecto.

