
.SUFFIXES: .cc

CC=cl -nologo
CFLAGS=-O2 -G5 -GF -Gs -Gy -Gz -MT $(DEFS)
DEFS=-DSTRICT -DWIN32 -D_WIN32 -D_MT
LD=link -nologo
LDFLAGS=-dll -machine:i386 -subsystem:windows,4.0
LIBS=kernel32.lib user32.lib gdi32.lib comctl32.lib

SRCS=privctrl.cc listviewex.cc url.cc
OBJS=$(SRCS:.cc=.obj)

BUILD=privctrl.dll

.cc.obj:
	$(CC) $(CFLAGS) -c -Tp$<

all: $(BUILD)

$(BUILD): $(OBJS)
	$(LD) $(LDFLAGS) -out:$@ $(OBJS) $(LIBS)

$(OBJS): privctlimpl.h privctrl.h

clean:
	rm -f $(BUILD) $(BUILD:.dll=.lib) $(BUILD:.dll=.exp) $(OBJS)

realclean: clean
	rm -f #* *~ XTAGS
