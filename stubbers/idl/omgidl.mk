CC=cc
CFLAGS = -DSTANDALONE
BISON = bison
FLEX = flex
PYTHON = python
SRC = idlparser.y idlscan.l omgidl.c omgidl.mk aprintf.c iluidl.h \
      bison.skel bisonparse.py README.omgidl

GENSRC = idlparser.tab.c idlparser.h idlparser-output.c idlscan.c

omgidl:	omgidl.o s-idlparser.o s-idlscan.o aprintf.o
	$(CC) -o omgidl omgidl.o s-idlparser.o s-idlscan.o aprintf.o

omgidl.o:	omgidl.c iluidl.h

s-idlparser.o:	idlparser.tab.c iluidl.h idlparser.h
	$(CC) $(CFLAGS) idlparser.tab.c -c -o s-idlparser.o

s-idlscan.o:	idlscan.c iluidl.h idlparser.h
	$(CC) $(CFLAGS) idlscan.c -c -o s-idlscan.o

idlparser.tab.c idlparser.h idlparser-output.c:	idlparser.y
	BISON_SIMPLE=./bison.skel ; export BISON_SIMPLE ; $(BISON) -v -p idl -t -d idlparser.y
	mv idlparser.tab.h idlparser.h
	$(PYTHON) bisonparse.py idlparser.output

idlscan.c:	idlscan.l
	$(FLEX) -Pidl -oidlscan.c -d idlscan.l

aprintf.o:	aprintf.c
	$(CC) -c aprintf.c -o aprintf.o

aprintf.c:
	cp ../parser/aprintf.c .

omgidl.tgz:	$(SRC) $(GENSRC)
	rm -rf omgidlsrc
	mkdir omgidlsrc
	cp $(SRC) $(GENSRC) omgidlsrc
	mv omgidlsrc/omgidl.mk omgidlsrc/Makefile
	tar cf - omgidlsrc | gzip > omgidl.tgz
	rm -rf omgidlsrc
