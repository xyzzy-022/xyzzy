.SUFFIXES: .cc

CC=cl -nologo
CFLAGS=-O2b1 -Gs -MD -W3 -DSTRICT

.cc.dll:
	$(CC) $(CFLAGS) -LD -Tp$< user32.lib kernel32.lib

.cc.exe:
	$(CC) $(CFLAGS) -Tp$< user32.lib

all: den8util.dll jdwrap.exe extract.exe tail-f.exe

extract.exe: unlha32.lib
	$(CC) $(CFLAGS) -Tpextract.cc kernel32.lib user32.lib unlha32.lib \
	  -link -machine:i386 -subsystem:windows -entry:WinMainCRTStartup \
	  -fixed -merge:.idata=.text

unlha32.lib: unlha32.def
	lib -machine:i386 -def:unlha32.def

tail-f.exe: tail-f.cc
	$(CC) $(CFLAGS) -Tptail-f.cc kernel32.lib user32.lib \
	  -link -machine:i386 -subsystem:console -entry:process_startup \
	  -fixed -merge:.idata=.text

clean: force
	-rm -f *.obj *.dll *.lib *.exp

realclean: clean
	-rm -f *~ #*

force:
