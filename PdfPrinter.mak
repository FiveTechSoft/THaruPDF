
# Libray directories & filenames #############################################

LIB_NAME         = PdfPrinter.lib
LIB_PRG_DIR      = source
LIB_OBJ_DIR      = obj
LIB_INCLUDE_DIR  = include

# LIBRARY PRG files (your PRG files go here) #################################



LIB_PRG_LIST =  THaruPdf.prg \
                THaruPdfBase.prg \
                HaruFonts.prg 

LIB_C_LIST =   

LIB_CPP_LIST =  

# Make directives ############################################################

.autodepend
.swap
.suffixes: .prg .hrb .c .cpp

# Paths for dependent files ##################################################

.path.prg = $(LIB_PRG_DIR)
.path.c   = $(LIB_PRG_DIR)
.path.cpp = $(LIB_PRG_DIR)
.path.hrb = $(LIB_OBJ_DIR)
.path.obj = $(LIB_OBJ_DIR)


# Contruction of the rest dependency lists ###################################

LIB_PRGS  = $(LIB_PRG_LIST)
LIB_C     = $(LIB_C_LIST)
LIB_HRB   = $(LIB_PRG_LIST:.prg=.hrb)
LIB_OBJS  = $(LIB_PRG_LIST:.prg=.obj) $(LIB_C_LIST:.c=.obj) $(LIB_CPP_LIST:.cpp=.obj)


# Harbour directories & flags ################################################
HB_DIR              = D:\Harbour2016
HARBOUR_INCLUDE_DIR = $(HB_DIR)\include
HARBOUR_EXE_DIR     = $(HB_DIR)\bin
HARBOUR_LIB_DIR     = $(HB_DIR)\lib
HARBOUR_FLAGS       = -i$(LIB_INCLUDE_DIR);$(HARBOUR_INCLUDE_DIR) -n -m -w -es2 -gc0 -p
HARBOUR_EXE         = $(HARBOUR_EXE_DIR)\harbour.exe

# Borlanc directories & flags ################################################
BCC_DIR                  = D:\BCC7
BORLANDC_INCLUDE_DIR     = $(BCC_DIR)\include
BORLANDC_EXE_DIR         = $(BCC_DIR)\bin
BORLANDC_LIB_DIR         = $(BCC_DIR)\lib
BORLANDC_COMP_FLAGS      = -c -O2 -I$(HARBOUR_INCLUDE_DIR);$(BORLANDC_INCLUDE_DIR)
BORLANDC_COMP_CPP_FLAGS  = -c -P -O2 -I$(HARBOUR_INCLUDE_DIR);$(BORLANDC_INCLUDE_DIR)
BORLANDC_COMP_EXE        = $(BORLANDC_EXE_DIR)\bcc32.exe
BORLANDC_LIB_EXE         = $(BORLANDC_EXE_DIR)\tlib.exe

# Dependencies ###############################################################

all: $(LIB_OBJS) $(LIB_HRB) $(LIB_NAME)

# Implicit Rules #############################################################

.prg.hrb:
   $(HARBOUR_EXE) $(HARBOUR_FLAGS) $** -o$@

.cpp.obj:
   $(BORLANDC_COMP_EXE) $(BORLANDC_COMP_CPP_FLAGS) -D__HARBOUR__;HB_API_MACROS -o$@ $**
   $(BORLANDC_LIB_EXE) $(LIB_NAME) -+ $@,,

.c.obj:
   $(BORLANDC_COMP_EXE) $(BORLANDC_COMP_FLAGS) -D__HARBOUR__;HB_API_MACROS -o$@ $**
   $(BORLANDC_LIB_EXE) $(LIB_NAME) -+ $@,,

.hrb.obj:
   $(BORLANDC_COMP_EXE) $(BORLANDC_COMP_FLAGS) -o$@ $**
   $(BORLANDC_LIB_EXE) $(LIB_NAME) -+ $@,,




