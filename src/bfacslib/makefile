
# MAKEFILE for bfacslib
# Copyright (c) 2005 Markus Hahn

CC=bcc32
AR=tlib
LINK=ilink32
RC=brc32
LIB=implib
OPTFLAGS=-O2 -4
ROOT_PATH=..

all: bfacslib.dll bfacslib_dll.lib testbfacslib.exe

#################### libbzip2 ########################

LIBBZIP2_PATH=$(ROOT_PATH)\libbzip2

LIBBZIP2_FLAGS=$(OPTFLAGS) -w-8004 -w-8008 -w-8057 -w-8066

LIBBZIP2_SOURCES=					\
	$(LIBBZIP2_PATH)\blocksort.c	\
	$(LIBBZIP2_PATH)\bzlib.c		\
	$(LIBBZIP2_PATH)\compress.c		\
	$(LIBBZIP2_PATH)\crctable.c		\
	$(LIBBZIP2_PATH)\decompress.c	\
	$(LIBBZIP2_PATH)\huffman.c		\
	$(LIBBZIP2_PATH)\randtable.c

LIBBZIP2_HEADERS=					\
	$(LIBBZIP2_PATH)\bzlib.h		\
	$(LIBBZIP2_PATH)\bzlib_private.h	

LIBBZIP2_OBJS=							\
	+$(LIBBZIP2_PATH)\blocksort.obj		\
	+$(LIBBZIP2_PATH)\bzlib.obj			\
	+$(LIBBZIP2_PATH)\compress.obj		\
	+$(LIBBZIP2_PATH)\crctable.obj		\
	+$(LIBBZIP2_PATH)\decompress.obj	\
	+$(LIBBZIP2_PATH)\huffman.obj		\
	+$(LIBBZIP2_PATH)\randtable.obj

libbzip2.lib: $(LIBBZIP2_SOURCES) $(LIBBZIP2_HEADERS)
	${CC} -c $(LIBBZIP2_FLAGS) -n$(LIBBZIP2_PATH) $(LIBBZIP2_SOURCES)
	${AR} $* $(LIBBZIP2_OBJS)

#################### zlib ########################

ZLIB_PATH=$(ROOT_PATH)\zlib

ZLIB_FLAGS=$(OPTFLAGS) -w-8004 -w-8008 -w-8012 -w-8066

ZLIB_SOURCES=				\
	$(ZLIB_PATH)\adler32.c	\
	$(ZLIB_PATH)\compress.c	\
	$(ZLIB_PATH)\crc32.c	\
	$(ZLIB_PATH)\deflate.c	\
	$(ZLIB_PATH)\infback.c	\
	$(ZLIB_PATH)\inffast.c	\
	$(ZLIB_PATH)\inflate.c	\
	$(ZLIB_PATH)\inftrees.c	\
	$(ZLIB_PATH)\trees.c	\
	$(ZLIB_PATH)\uncompr.c	\
	$(ZLIB_PATH)\zutil.c

ZLIB_HEADERS=				\
	$(ZLIB_PATH)\crc32.h	\
	$(ZLIB_PATH)\deflate.h	\
	$(ZLIB_PATH)\inffast.h	\
	$(ZLIB_PATH)\inffixed.h	\
	$(ZLIB_PATH)\inflate.h	\
	$(ZLIB_PATH)\inftrees.h	\
	$(ZLIB_PATH)\trees.h	\
	$(ZLIB_PATH)\zconf.h	\
	$(ZLIB_PATH)\zconf.in.h	\
	$(ZLIB_PATH)\zlib.h		\
	$(ZLIB_PATH)\zutil.h

ZLIB_OBJS=						\
	+$(ZLIB_PATH)\adler32.obj	\
	+$(ZLIB_PATH)\compress.obj	\
	+$(ZLIB_PATH)\crc32.obj		\
	+$(ZLIB_PATH)\deflate.obj	\
	+$(ZLIB_PATH)\infback.obj	\
	+$(ZLIB_PATH)\inffast.obj	\
	+$(ZLIB_PATH)\inflate.obj	\
	+$(ZLIB_PATH)\inftrees.obj	\
	+$(ZLIB_PATH)\trees.obj		\
	+$(ZLIB_PATH)\uncompr.obj	\
	+$(ZLIB_PATH)\zutil.obj

zlib.lib: $(ZLIB_SOURCES) $(ZLIB_HEADERS)
	${CC} -c $(ZLIB_FLAGS) -n$(ZLIB_PATH) $(ZLIB_SOURCES)
	${AR} $* $(ZLIB_OBJS)

#################### cryptpak ########################

CRYPTPAK_PATH=$(ROOT_PATH)\cryptpak

CRYPTPAK_FLAGS=$(OPTFLAGS) -w-8065 -w-8057 -DWIN32 -DCRYPTPAK_DLL -DEXPORT_DLL

CRYPTPAK_SOURCES= 					\
	$(CRYPTPAK_PATH)\AES.c			\
	$(CRYPTPAK_PATH)\arcfour.c		\
	$(CRYPTPAK_PATH)\Blowfish.c		\
	$(CRYPTPAK_PATH)\cast.c			\
	$(CRYPTPAK_PATH)\CipherServer.c	\
	$(CRYPTPAK_PATH)\cpkernel.c		\
	$(CRYPTPAK_PATH)\CRC32.c		\
	$(CRYPTPAK_PATH)\LZSS.c			\
	$(CRYPTPAK_PATH)\MD5.c			\
	$(CRYPTPAK_PATH)\RandomPool.c	\
	$(CRYPTPAK_PATH)\Serpent.c		\
	$(CRYPTPAK_PATH)\SHA1.c			\
	$(CRYPTPAK_PATH)\SHA512.c		\
	$(CRYPTPAK_PATH)\Support.c		\
	$(CRYPTPAK_PATH)\TMS.c			\
	$(CRYPTPAK_PATH)\TripleDES.c	\
	$(CRYPTPAK_PATH)\Twofish.c		\
	$(CRYPTPAK_PATH)\Yarrow.c

CRYPTPAK_HEADERS= 						\
	$(CRYPTPAK_PATH)\AES.h				\
	$(CRYPTPAK_PATH)\AESBoxes.h			\
	$(CRYPTPAK_PATH)\arcfour.h			\
	$(CRYPTPAK_PATH)\BasicTypes.h		\
	$(CRYPTPAK_PATH)\Blowfish.h			\
	$(CRYPTPAK_PATH)\BlowfishBoxes.h	\
	$(CRYPTPAK_PATH)\cast.h				\
	$(CRYPTPAK_PATH)\cast_boxes.h		\
	$(CRYPTPAK_PATH)\CipherDef.h		\
	$(CRYPTPAK_PATH)\CipherServer.h		\
	$(CRYPTPAK_PATH)\cpconfig.h			\
	$(CRYPTPAK_PATH)\cpkernel.h			\
	$(CRYPTPAK_PATH)\CRC32.h			\
	$(CRYPTPAK_PATH)\CryptPak.h			\
	$(CRYPTPAK_PATH)\LZSS.h				\
	$(CRYPTPAK_PATH)\MD5.h				\
	$(CRYPTPAK_PATH)\RandomPool.h		\
	$(CRYPTPAK_PATH)\Serpent.h			\
	$(CRYPTPAK_PATH)\SHA1.h				\
	$(CRYPTPAK_PATH)\SHA512.h			\
	$(CRYPTPAK_PATH)\Support.h			\
	$(CRYPTPAK_PATH)\TMS.h				\
	$(CRYPTPAK_PATH)\TripleDES.h		\
	$(CRYPTPAK_PATH)\Twofish.h			\
	$(CRYPTPAK_PATH)\Yarrow.h

CRYPTPAK_OBJS= 							\
	+$(CRYPTPAK_PATH)\AES.obj			\
	+$(CRYPTPAK_PATH)\arcfour.obj		\
	+$(CRYPTPAK_PATH)\Blowfish.obj		\
	+$(CRYPTPAK_PATH)\cast.obj			\
	+$(CRYPTPAK_PATH)\CipherServer.obj	\
	+$(CRYPTPAK_PATH)\cpkernel.obj		\
	+$(CRYPTPAK_PATH)\CRC32.obj			\
	+$(CRYPTPAK_PATH)\LZSS.obj			\
	+$(CRYPTPAK_PATH)\MD5.obj			\
	+$(CRYPTPAK_PATH)\RandomPool.obj	\
	+$(CRYPTPAK_PATH)\Serpent.obj		\
	+$(CRYPTPAK_PATH)\SHA1.obj			\
	+$(CRYPTPAK_PATH)\SHA512.obj		\
	+$(CRYPTPAK_PATH)\Support.obj		\
	+$(CRYPTPAK_PATH)\TMS.obj			\
	+$(CRYPTPAK_PATH)\TripleDES.obj		\
	+$(CRYPTPAK_PATH)\Twofish.obj		\
	+$(CRYPTPAK_PATH)\Yarrow.obj

cryptpak.lib: $(CRYPTPAK_SOURCES) $(CRYPTPAK_HEADERS)
	${CC} -c $(CRYPTPAK_FLAGS) -n$(CRYPTPAK_PATH) $(CRYPTPAK_SOURCES)
	${AR} $* +$(CRYPTPAK_OBJS)

#################### bfacsext ########################

BFACSEXT_PATH=$(ROOT_PATH)\bfacsext

BFACSEXT_FLAGS=$(OPTFLAGS) -w-8057 -DWIN32

BFACSEXT_SOURCES= 					\
	$(BFACSEXT_PATH)\bfacsext.cpp	\
	$(BFACSEXT_PATH)\bfexinit.cpp	\
	$(BFACSEXT_PATH)\Ctxmenu.cpp

BFACSEXT_HEADERS= 					\
	$(BFACSEXT_PATH)\bfacsext.h		\
	$(BFACSEXT_PATH)\PRIV.H
	
BFACSEXT_OBJS= 						\
	+$(BFACSEXT_PATH)\bfacsext.obj	\
	+$(BFACSEXT_PATH)\bfexinit.obj	\
	+$(BFACSEXT_PATH)\Ctxmenu.obj

bfacsext.lib: $(BFACSEXT_SOURCES) $(BFACSEXT_HEADERS)
	${CC} -c $(BFACSEXT_FLAGS) -n$(BFACSEXT_PATH) $(BFACSEXT_SOURCES)
	${AR} $* $(BFACSEXT_OBJS)

#################### bfacslib ########################

BFACSLIB_FLAGS=$(OPTFLAGS) -w-8057 -DWIN32
BFACSLIB_LINKFLAGS=-Gz -Tpd
BFACSLIB_LINKSTART=c0d32x.obj
BFACSLIB_BINDIR=..\bfacs
BFACSLIB_INCLUDES= -I..\libbzip2 -I..\zlib -I..\cryptpak -I..\bfacsext

BFACSLIB_LIBS=libbzip2.lib zlib.lib cryptpak.lib bfacsext.lib
BFACSLIB_LINKLIBS=import32.lib cw32mt.lib ${BFACSLIB_LIBS}

BFACSLIB_SOURCES=bfacslib.cpp zlibex.c
BFACSLIB_HEADERS=zlibex.h
BFACSLIB_OBJS=bfacslib.obj zlibex.obj

bfacslib.dll: $(BFACSLIB_LIBS) bfacslib.res
	${CC} -c $(BFACSLIB_FLAGS) ${BFACSLIB_INCLUDES} $(BFACSLIB_SOURCES)
    $(LINK) $(BFACSLIB_LINKFLAGS) $(BFACSLIB_LINKSTART) $(BFACSLIB_OBJS),	\
    $(BFACSLIB_BINDIR)\$*,,$(BFACSLIB_LINKLIBS),bfacslib.def,bfacslib.res

bfacslib_dll.lib: $(BFACSLIB_BINDIR)\bfacslib.dll
    $(LIB) $* $?

bfacslib.res: bfacslib.rc
    $(RC) -r $?

#################### testbfacslib ########################

TESTBFACSLIB_FLAGS=$(OPTFLAGS) -WC -w-8057 -DWIN32 -DCRYPTPAK_DLL
TESTBFACSLIB_LINKSTART=c0x32.obj
TESTBFACSLIB_LINKFLAGS=-ap
TESTBFACSLIB_PATH=testbfacslib
TESTBFACSLIB_LIBS=bfacslib_dll.lib
TESTBFACSLIB_LINKLIBS=import32.lib cw32mt.lib bfacslib_dll.lib

TESTBFACSLIB_SOURCES=							\
	$(TESTBFACSLIB_PATH)\testbfacslib.cpp		\
	$(TESTBFACSLIB_PATH)\TestCipherServer.cpp	\
	$(TESTBFACSLIB_PATH)\TestCRC32.cpp			\
	$(TESTBFACSLIB_PATH)\TestLZSS.cpp			\
	$(TESTBFACSLIB_PATH)\TestMD5.cpp			\
	$(TESTBFACSLIB_PATH)\TestSHA1.cpp			\
	$(TESTBFACSLIB_PATH)\TestSHA512.cpp			\
	$(TESTBFACSLIB_PATH)\TestSupport.cpp		\
	$(TESTBFACSLIB_PATH)\TestUtils.cpp			\
	$(TESTBFACSLIB_PATH)\TestYarrow.cpp			\
	$(TESTBFACSLIB_PATH)\TestZLibEx.cpp

TESTBFACSLIB_OBJS=								\
	$(TESTBFACSLIB_PATH)\testbfacslib.obj		\
	$(TESTBFACSLIB_PATH)\TestCipherServer.obj	\
	$(TESTBFACSLIB_PATH)\TestCRC32.obj			\
	$(TESTBFACSLIB_PATH)\TestLZSS.obj			\
	$(TESTBFACSLIB_PATH)\TestMD5.obj			\
	$(TESTBFACSLIB_PATH)\TestSHA1.obj			\
	$(TESTBFACSLIB_PATH)\TestSHA512.obj			\
	$(TESTBFACSLIB_PATH)\TestSupport.obj		\
	$(TESTBFACSLIB_PATH)\TestUtils.obj			\
	$(TESTBFACSLIB_PATH)\TestYarrow.obj			\
	$(TESTBFACSLIB_PATH)\TestZLibEx.obj

TESTBFACSLIB_HEADERS=						\
	$(TESTBFACSLIB_PATH)\TestInterface.h	\
	$(TESTBFACSLIB_PATH)\TestUtils.h

testbfacslib.exe: $(TESTBFACSLIB_SOURCES) $(TESTBFACSLIB_HEADERS)	\
	bfacslib_dll.lib
	${CC} -c $(TESTBFACSLIB_FLAGS) ${BFACSLIB_INCLUDES} $(TESTBFACSLIB_SOURCES)
    $(LINK) $(TESTBFACSLIB_LINKFLAGS) $(TESTBFACSLIB_LINKSTART)	\
    $(TESTBFACSLIB_OBJS),$(BFACSLIB_BINDIR)\$*,,$(TESTBFACSLIB_LINKLIBS),,
