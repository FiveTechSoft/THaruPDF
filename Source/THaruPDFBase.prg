/*
 * Proyecto: QuickSQL
 * Fichero: THaruPDFBase.prg
 * Descripción:
 * Autor: Carlos Mora
 * Fecha: 26/03/2013
 */

#include 'hbclass.ch'
#include 'harupdf.ch'

#define __NODEBUG__
#include 'debug.ch'

//------------------------------------------------------------------------------
CLASS THaruPDFBase
   // Implementación de la clase wrapper, que reemplaza al objeto Printer de FW
   // No se usa directamente sino a través de la
   DATA hPdf
   DATA hPage
   DATA LoadedFonts

   DATA nPageSize INIT HPDF_PAGE_SIZE_A4
   DATA nOrientation INIT HPDF_PAGE_PORTRAIT // HPDF_PAGE_LANDSCAPE
   DATA nHeight, nWidth

   DATA cFileName
   DATA nPermission
   DATA cPassword, cOwnerPassword

   DATA hImageList

   CONSTRUCTOR New()
   METHOD SetPage()
   METHOD SetLandscape()
   METHOD SetPortrait()
   METHOD SetCompression( cMode ) INLINE HPDF_SetCompressionMode( ::hPdf, cMode )
   METHOD StartPage()
   METHOD EndPage()
   METHOD Say()
   METHOD CmSay()
   METHOD DefineFont()
   METHOD Cmtr2Pix( nRow, nCol )
   METHOD Mmtr2Pix( nRow, nCol )
   METHOD CmRect2Pix()
   METHOD nVertRes() INLINE 72
   METHOD nHorzRes() INLINE 72
   METHOD nVertSize() INLINE HPDF_Page_GetHeight( ::hPage )
   METHOD nHorzSize() INLINE HPDF_Page_GetWidth( ::hPage )
   METHOD SizeInch2Pix()
   METHOD SayBitmap()
   METHOD GetImageFromFile()
   METHOD Line()
   METHOD Save()
   METHOD SyncPage()
   METHOD CheckPage()
   METHOD GetTextWidth()
   METHOD End()
ENDCLASS

//------------------------------------------------------------------------------
METHOD New( cFileName, cPassword, cOwnerPassword, nPermission )
//------------------------------------------------------------------------------
   ::hPdf := HPDF_New()
   ::LoadedFonts:= {}

   IF ::hPdf == NIL
      alert( " Pdf could not been created!" )
      RETURN NIL
   ENDIF

   HPDF_SetCompressionMode( ::hPdf, HPDF_COMP_ALL )

   ::cFileName := cFileName
   ::cPassword := cPassword
   ::cOwnerPassword := cOwnerPassword
   ::nPermission := nPermission

   ::hImageList:= { => }

RETURN Self

//------------------------------------------------------------------------------
METHOD SetPage( nPageSize )
//------------------------------------------------------------------------------
   ::nPageSize:= nPageSize
   ::SyncPage()
RETURN Self

//------------------------------------------------------------------------------
METHOD SyncPage()
//------------------------------------------------------------------------------
   IF ::hPage != NIL
      HPDF_Page_SetSize( ::hPage, ::nPageSize, ::nOrientation)
      ::nHeight := HPDF_Page_GetHeight( ::hPage )
      ::nWidth  := HPDF_Page_GetWidth( ::hPage )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------
METHOD CheckPage()
//------------------------------------------------------------------------------
   IF ::hPage == NIL
      ::StartPage()
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------
METHOD SetLandscape()
//------------------------------------------------------------------------------
   ::nOrientation:= HPDF_PAGE_LANDSCAPE
   ::SyncPage()
RETURN Self

//------------------------------------------------------------------------------
METHOD SetPortrait()
//------------------------------------------------------------------------------
   ::nOrientation:= HPDF_PAGE_PORTRAIT
   ::SyncPage()
RETURN Self

//------------------------------------------------------------------------------
METHOD StartPage()
//------------------------------------------------------------------------------
   ::hPage := HPDF_AddPage(::hPdf)
   ::SyncPage()
RETURN Self

//------------------------------------------------------------------------------
METHOD EndPage()
//------------------------------------------------------------------------------
   ::hPage:= NIL
RETURN Self

//------------------------------------------------------------------------------
METHOD Say( nRow, nCol, cText, oFont, nWidth, nClrText, nBkMode, nPad )
//------------------------------------------------------------------------------
   ::CheckPage()
   HPDF_Page_BeginText( ::hPage )
   HPDF_Page_SetFontAndSize( ::hPage, oFont[1], oFont[2] )
   /*
   IF Empty( nWidth )
      nWidth := HPDF_Page_TextWidth( ::hPage, cText )  +80
      DEBUG nWidth
   ENDIF

   IF nPad == NIL
      nPad:= HPDF_TALIGN_LEFT
   ENDIF
   */

   DO CASE
   CASE nPad == NIL .OR. nPad == HPDF_TALIGN_LEFT
      HPDF_Page_TextOut( ::hPage, nCol, ::nHeight - nRow - oFont[2], cText )
   CASE nPad == HPDF_TALIGN_RIGHT
      nWidth := HPDF_Page_TextWidth( ::hPage, cText )
      HPDF_Page_TextOut( ::hPage, nCol-nWidth, ::nHeight - nRow - oFont[2], cText )
   OTHER
      HPDF_Page_TextOut( ::hPage, nCol-nWidth/2, ::nHeight - nRow - oFont[2], cText )
   ENDCASE

   // HPDF_Page_TextRect( ::hPage, nCol, ::nHeight - nRow, nCol + nWidth, ::nHeight - nRow + oFont[2], cText, nPad, NIL)
   HPDF_Page_EndText( ::hPage )

   /* Recuadro del texto para probar
   HPDF_Page_SetLineWidth( ::hPage, 1 )
   HPDF_Page_Rectangle( ::hPage, nCol, ::nHeight - nRow - oFont[2], nWidth, oFont[2])
   HPDF_Page_Stroke( ::hPage )
   */

RETURN Self

//------------------------------------------------------------------------------
METHOD CmSay( nRow, nCol, cText, oFont, nWidth, nClrText, nBkMode, nPad, lO2A )
//------------------------------------------------------------------------------

       ::Cmtr2Pix( @nRow, @nCol )
       IF  nWidth != Nil
         ::Cmtr2Pix( 0, @nWidth )
      ENDIF
      ::Say( nRow, nCol, cText, oFont, nWidth, nClrText, nBkMode, nPad, lO2A )
RETURN Self


//------------------------------------------------------------------------------
METHOD DefineFont( cFontName, nSize )
//------------------------------------------------------------------------------
   LOCAL font_list  := { ;
                        "Courier",                  ;
                        "Courier-Bold",             ;
                        "Courier-Oblique",          ;
                        "Courier-BoldOblique",      ;
                        "Helvetica",                ;
                        "Helvetica-Bold",           ;
                        "Helvetica-Oblique",        ;
                        "Helvetica-BoldOblique",    ;
                        "Times-Roman",              ;
                        "Times-Bold",               ;
                        "Times-Italic",             ;
                        "Times-BoldItalic",         ;
                        "Symbol",                   ;
                        "ZapfDingbats"              ;
                      }

   LOCAL i, ttf_list

   i:= aScan( font_list, {|x| UPPER( x ) == Upper( cFontName ) } )
   IF i > 0 // Standard font
      cFontName:= font_list[i]
   ELSE
      i:= aScan( ::LoadedFonts, {|x| UPPER( x[1] ) == Upper( cFontName ) } )
      IF i > 0
         cFontName:= ::LoadedFonts[i][2]
         DEBUGMSG 'Activada fuente ' + cFontName
      ELSE
         ttf_list:= GetHaruFontList()
         i:= aScan( ttf_list, {|x| UPPER( x[1] ) == Upper( cFontName ) } )
         IF i > 0
            cFontName:= HPDF_LoadTTFontFromFile( ::hPdf, ttf_list[i,2], .f. )
            DEBUGMSG 'Cargada fuente ' + cFontName
            DEBUGMSG 'Fichero ' + ttf_list[i,2]
            aAdd( ::LoadedFonts, { ttf_list[i,1], cFontName } )
         ELSE
            Alert( 'Fuente desconocida '+cFontName )
            RETURN NIL
         ENDIF
      ENDIF
   ENDIF

RETURN { HPDF_GetFont( ::hPdf, cFontName, "WinAnsiEncoding" ), nSize }

METHOD Cmtr2Pix( nRow, nCol )
   nRow *= 72/ 2.54
   nCol *= 72/ 2.54
RETURN Self

METHOD Mmtr2Pix( nRow, nCol )
   nRow *= 72/ 25.4
   nCol *= 72/ 25.4
RETURN Self

METHOD CmRect2Pix(aRect)

   LOCAL aTmp[ 4 ]

   aTmp[ 1 ] = Max( 0, aRect[1] * 72/ 2.54 )
   aTmp[ 2 ] = Max( 0, aRect[2] * 72/ 2.54 )
   aTmp[ 3 ] = Max( 0, aRect[3] * 72/ 2.54 )
   aTmp[ 4 ] = Max( 0, aRect[4] * 72/ 2.54 )

RETURN aTmp

METHOD SizeInch2Pix( nHeight, nWidth )
   nHeight *= 72
   nWidth *= 72
RETURN { nHeight, nWidth }

METHOD GetImageFromFile( cImageFile )

   IF HB_HHasKey( ::hImageList, cImageFile )
      RETURN ::hImageList[ cImageFile ]
   ENDIF
   IF !File( cImageFile )
      IF( Lower(Right( cImageFile, 4 )) == '.bmp' ) // En el código esta como bmp, probar si ya fue transformado a png
         cImageFile:= Left( cImageFile, Len( cImageFile ) - 3 ) + 'png'
         RETURN ::GetImageFromFile( cImageFile )
      ELSE
         Alert( cImageFile+' no encontrado' )
         RETURN NIL
      ENDIF
   ENDIF
   IF( Lower(Right( cImageFile, 4 )) == '.png' )
      RETURN ( ::hImageList[ cImageFile ]:= HPDF_LoadPngImageFromFile(::hPdf, cImageFile ) )
   ENDIF

RETURN ( ::hImageList[ cImageFile ]:= HPDF_LoadJpegImageFromFile(::hPdf, cImageFile ) )

METHOD SayBitmap( nRow, nCol, xBitmap, nWidth, nHeight, nRaster )
   // LOCAL iw, ih, x, y
   LOCAL image

   IF !Empty( image := ::GetImageFromFile( xBitmap ) )

      /*
      iw := HPDF_Image_GetWidth(image)
      ih := HPDF_Image_GetHeight(image)
      // Alert( 'iw'+str(iw) )
      // Alert( 'ih'+str(ih) )

      x := nCol
      y := HPDF_Page_GetHeight(::hPage);
      */
      /* Draw image to the canvas.(normal-mode with actual size.)*/
      HPDF_Page_DrawImage(::hPage, image, nCol, ::nHeight - nRow - nWidth, nHeight, nWidth /* iw, ih*/)
   ENDIF

RETURN Self

METHOD Line()
RETURN Self

METHOD Save( cFilename )
   FErase( cFilename )

   IF ValType( ::nPermission ) != 'N'
      ::nPermission:= ( HPDF_ENABLE_READ + HPDF_ENABLE_PRINT + HPDF_ENABLE_COPY )
   ENDIF

   IF ValType( ::cPassword ) == 'C' .AND. !Empty( ::cPassword )
      IF Empty( ::cOwnerPassword )
         ::cOwnerPassword:= ::cPassword + '+1'
      ENDIF
      debug HPDF_SetPassword( ::hPdf, ::cOwnerPassword, ::cPassword )
      debug HPDF_SetPermission( ::hPdf, ::nPermission )
   ENDIF

RETURN HPDF_SaveToFile (::hPdf, cFilename)

METHOD GetTextWidth( cText, oFont )
   HPDF_Page_SetFontAndSize( ::hPage, oFont[1], oFont[2] )
RETURN HPDF_Page_TextWidth( ::hPage, cText )

METHOD End()
   LOCAL nResult
   DEBUGMSG ::cFileName
   IF ValType( ::cFileName ) == 'C'
      nResult:= ::Save( ::cFileName )
   ENDIF

   HPDF_Free( ::hPdf )

RETURN nResult
