
# define VC++ version (4, 6, or 7)
VCVER=8

.SUFFIXES: .cc .rc .res .d

SRCS=\
Buffer.cc \
ChooseFont.cc \
ColorDialog.cc \
DnD.cc \
StrBuf.cc \
Window.cc \
alloc.cc \
arc-if.cc \
archiver.cc \
array.cc \
assert.cc \
backq.cc \
bignum.cc \
binfo.cc \
buffer-bar.cc \
byte-stream.cc \
bytecode.cc \
ces.cc \
char.cc \
chname.cc \
chtab.cc \
com.cc \
conf.cc \
colors.cc \
ctl3d.cc \
chunk.cc \
data.cc \
dde.cc \
dialogs.cc \
disp.cc \
dll.cc \
doc.cc \
dockbar.cc \
dumpver.cc \
edict.cc \
encdec.cc \
encoding.cc \
environ.cc \
eval.cc \
except.cc \
fileio.cc \
filer.cc \
fnkey.cc \
font.cc \
gime.cc \
glob.cc \
guid.cc \
gzip.cc \
hash.cc \
hashpjw.cc \
init.cc \
insdel.cc \
ipc.cc \
kanji.cc \
kbd.cc \
keymap.cc \
largeint.cc \
ldialog.cc \
lex.cc \
list.cc \
listen.cc \
lprint.cc \
lread.cc \
md5c.cc \
menu.cc \
minibuf.cc \
mman.cc \
mouse.cc \
move.cc \
msgbox.cc \
msgdef.cc \
number.cc \
oledata.cc \
package.cc \
pane.cc \
pathname.cc \
popup.cc \
popupl.cc \
pred.cc \
preview.cc \
print.cc \
printdlg.cc \
process.cc \
random.cc \
regex.cc \
resolver.cc \
search.cc \
sequence.cc \
sha1.cc \
signal.cc \
sock.cc \
sockinet.cc \
statarea.cc \
stdctl.cc \
stream.cc \
string.cc \
structure.cc \
symbol.cc \
symtable.cc \
syntax.cc \
sysdep.cc \
thread.cc \
toplev.cc \
ucs2.cc \
undo.cc \
usertab.cc \
usertool.cc \
utils.cc \
utimer.cc \
vector.cc \
version.cc \
vfs.cc \
wheel.cc \
winhelp.cc \
wstream.cc \
xdde.cc \
xlist.cc

PRIVCTRLDIR=../privctrl
PRIVCTRLOBJS=\
$(OUTDIR)/privctrl.obj \
$(OUTDIR)/listviewex.obj \
$(OUTDIR)/url.obj

BUILD=xyzzy.exe

!IF "$(VCVER)" == ""
!ERROR `VCVER' undeclared.
!ELSEIF "$(VCVER)" == "4"
CXXOPT=-Gf -GX
!ELSE
CXXOPT=-GF -EHa
!ENDIF

DEFS=-DSTRICT -DWIN32 -D_WIN32 -D_MT -D__XYZZY__ -DPRIVCTRL_NODLL -DHAVE_DIMM_H -DDEBUG_GC
!IF "$(VCVER)" == "8" || "$(VCVER)" == "9"
CXX=cl -nologo -Zc:wchar_t- -Zc:forScope- -D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_WARNINGS
!ELSE
CXX=cl -nologo
!ENDIF
CPP=cl -nologo -E
LD=link -nologo
LIBS=kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib \
comctl32.lib imm32.lib setargv.obj comdlg32.lib mpr.lib ole32.lib \
oleaut32.lib winspool.lib zlib/zlib.lib 
INC=-I$(PRIVCTRLDIR)

SPC=""
SPC=$(SPC:""= )

OBJS=$(SPC)$(SRCS:.cc=.obj)
OBJS=$(OBJS:  = )

!IFNDEF CFG
CFG=r
!ENDIF

!IF "$(CFG)" == "d"

OUTDIR=d
OBJS=$(OBJS: = d/)
CXXFLAGS=-Od -Gz -Gs -Gy -Gi -Gm $(CXXOPT) -W3 -MTd -Zi $(INC) $(DEFS) -DDEBUG -Fo$(OUTDIR)/
LDFLAGS=-debug -stack:0x2000000 -incremental:yes
FINAL_TARGET=$(OUTDIR)/$(BUILD)

!ELSE

OUTDIR=r
OBJS=$(OBJS: = r/)
CXXFLAGS=-Ob1 -Og -Oi -Ot -Oy -G5 -Gs -Gy -Gz $(CXXOPT) -W3 -MT -vmb $(INC) $(DEFS) -Fo$(OUTDIR)/
LDFLAGS=-stack:0x2000000 -map:$(OUTDIR)/xyzzy.map -incremental:no -fixed -opt:ref
FINAL_TARGET=../$(BUILD)

!ENDIF

.cc{$(OUTDIR)/}.obj:
	$(CXX) $(CXXFLAGS) -c -Tp$<

{$(PRIVCTRLDIR)/}.cc{$(OUTDIR)/}.obj:
	$(CXX) $(CXXFLAGS) -c -Tp$<

.rc.res:
	rc -r $<

.d.h:
	dpp $< $@

all: $(OUTDIR) dpp.exe includes ../xyzzyenv.exe \
     $(FINAL_TARGET) ../xyzzycli.exe libxpi.lib

includes: vars-decl.h fns-decl.h msgcode.h

$(OUTDIR)/$(BUILD): $(OBJS) $(PRIVCTRLOBJS) $(OUTDIR)/xyzzy.res
	$(LD) @<<
  $(LDFLAGS) -out:$@ $(OBJS) $(PRIVCTRLOBJS) -subsystem:windows $(LIBS) $(OUTDIR)/xyzzy.res
<<

$(OUTDIR): force
	-@if not exist $(OUTDIR)\nul mkdir $(OUTDIR)

force:

clean:
	rm -f $(OBJS:/=\) $(PRIVCTRLOBJS:/=\) $(OUTDIR)\$(BUILD)
	rm -f $(OUTDIR)\xyzzy.res $(OUTDIR)\xyzzy.rc $(OUTDIR)\$(BUILD:.exe=.map) \
	  $(OUTDIR)\$(BUILD:.exe=.ilk)
	rm -f gen-syms.obj gen-syms.exe gen-ctab.obj gen-ctab.exe
	rm -f gen-ktab.obj gen-ktab.exe gen-msg.obj gen-msg.exe
	rm -f gen-vinf.obj gen-vinf.exe verinfo.h
	rm -f gen-ucs2tab.obj gen-ucs2tab.exe
	rm -f gen-utf2sjis.obj gen-utf2sjis.exe
	rm -f gen-char-width.obj gen-char-width.exe
	rm -f gen-jisx0212-hash.exe gen-jisx0212-hash.obj
	rm -f gen-iso2022state.obj gen-iso2022state.exe
	rm -f gen-ibmext.obj gen-ibmext.exe
	rm -f dpp.exe dpp.obj num-arith.h

distclean: clean
	rm -f vars-decl.h fns-decl.h symtable.cc chtab.cc msgcode.h \
	  dumpver.cc msgdef.cc kanjitab.h char-width.h \
	  gen-jisx0212-width.obj gen-jisx0212-width.exe \
	  gen-lucida-width.obj gen-lucida-width.exe \
	  gen-fontrange.obj gen-fontrange.exe \
	  iso2022state.h ibmext.h utf2sjis.h jisx0212-hash.h \
	  xdde.obj xyzzycli.obj xyzzycli.exe xyzzyenv.obj xyzzyenv.exe \
	  xyzzycli.res xyzzyenv.res libxpi.lib xpi.obj \
	  vc40.idb vc40.pdb *.aps XTAGS #* *~ \
	  $(PRIVCTRLDIR)/#* $(PRIVCTRLDIR)/*~

realclean: distclean
	rm -f jisx0212-width.h lucida-width.h ucs2tab.h fontrange.h

../$(BUILD): $(OUTDIR)/$(BUILD)
	copy $(OUTDIR)\$(BUILD) .. > nul
	copy $(OUTDIR)\xyzzy.map .. > nul

../xyzzyenv.exe: xyzzyenv.exe
	copy xyzzyenv.exe .. > nul

xyzzyenv.exe: xyzzyenv.cc xyzzyenv.res
	$(CXX) -O2 -Tpxyzzyenv.cc xyzzyenv.res kernel32.lib user32.lib \
	   -link -fixed -subsystem:console -entry:process_startup

../xyzzycli.exe: xyzzycli.exe
	copy xyzzycli.exe .. > nul

CLIENT_OBJS=xyzzycli.obj
xyzzycli.exe: $(CLIENT_OBJS) xyzzycli.res
	$(CXX) $(CLIENT_OBJS) kernel32.lib user32.lib xyzzycli.res \
	  -link -fixed -subsystem:windows -entry:process_startup 
#	$(CXX) $(CLIENT_OBJS) setargv.obj kernel32.lib user32.lib xyzzycli.res

xyzzycli.obj: xyzzycli.cc xdde.h xyzzycli.h
	$(CXX) -O2 -W3 -c -DSTRICT -Tpxyzzycli.cc

xyzzycli.res: xyzzycli.h

libxpi.lib: xpi.obj
	lib -out:$@ xpi.obj

xpi.obj: xpi.cc
	$(CXX) -O2 -Zl -W3 -c -Tpxpi.cc

TMPFILE=tmpfile.$$$$

$(OUTDIR)/xyzzy.res: $(OUTDIR)/xyzzy.rc
	rc -r -fo$@ $(RCFLAGS) $(OUTDIR)/xyzzy.rc

$(OUTDIR)\xyzzy.rc: xyzzy.rc verinfo.h
	type xyzzy.rc > $@
	type verinfo.h >> $@

vars-decl.h: gen-syms.exe
	-@if exist $@ del $@ > nul
	.\gen-syms -vars > $(TMPFILE)
	move $(TMPFILE) $@ > nul

fns-decl.h: gen-syms.exe
	-@if exist $@ del $@ > nul
	.\gen-syms -proto > $(TMPFILE)
	move $(TMPFILE) $@ > nul

symtable.cc: gen-syms.exe
	-@if exist $@ del $@ > nul
	.\gen-syms -symtable > $(TMPFILE)
	move $(TMPFILE) $@ > nul

dumpver.cc: gen-syms.exe version.h
	-@if exist $@ del $@ > nul
	.\gen-syms -version > $(TMPFILE)
	move $(TMPFILE) $@ > nul

chtab.cc: gen-ctab.exe
	-@if exist $@ del $@ > nul
	.\gen-ctab > $(TMPFILE)
	move $(TMPFILE) $@ > nul

msgcode.h: gen-msg.exe
	-@if exist $@ del $@ > nul
	.\gen-msg -enum > $(TMPFILE)
	move $(TMPFILE) $@ > nul

msgdef.cc: gen-msg.exe
	-@if exist $@ del $@ > nul
	.\gen-msg -c > $(TMPFILE)
	move $(TMPFILE) $@ > nul

kanjitab.h: gen-ktab.exe
	-@if exist $@ del $@ > nul
	.\gen-ktab > $(TMPFILE)
	move $(TMPFILE) $@ > nul

verinfo.h: gen-vinf.exe
	-@if exist $@ del $@ > nul
	.\gen-vinf > $(TMPFILE)
	move $(TMPFILE) $@ > nul

ucs2tab.h: gen-ucs2tab.cc
	$(MAKE) -nologo gen-ucs2tab.exe
	-@if exist $@ del $@ > nul
	.\gen-ucs2tab > $(TMPFILE)
	move $(TMPFILE) $@ > nul

utf2sjis.h: gen-utf2sjis.exe
	-@if exist $@ del $@ > nul
	.\gen-utf2sjis > $(TMPFILE)
	move $(TMPFILE) $@ > nul

char-width.h: gen-char-width.exe
	-@if exist $@ del $@ > nul
	.\gen-char-width > $(TMPFILE)
	move $(TMPFILE) $@ > nul

jisx0212-width.h:
	$(MAKE) -nologo gen-jisx0212-width.exe
	.\gen-jisx0212-width > $(TMPFILE)
	-@if exist $@ del $@ > nul
	move $(TMPFILE) $@ > nul

jisx0212-hash.h: gen-jisx0212-hash.exe
	-@if exist $@ del $@ > nul
	.\gen-jisx0212-hash > $(TMPFILE)
	move $(TMPFILE) $@ > nul

lucida-width.h:
	$(MAKE) -nologo gen-lucida-width.exe
	.\gen-lucida-width > $(TMPFILE)
	-@if exist $@ del $@ > nul
	move $(TMPFILE) $@ > nul

iso2022state.h: gen-iso2022state.exe
	-@if exist $@ del $@ > nul
	.\gen-iso2022state > $(TMPFILE)
	move $(TMPFILE) $@ > nul

ibmext.h: gen-ibmext.exe
	-@if exist $@ del $@ > nul
	.\gen-ibmext > $(TMPFILE)
	move $(TMPFILE) $@ > nul

fontrange.h:
	$(MAKE) -nologo gen-fontrange.exe
	.\gen-fontrange > $(TMPFILE)
	-@if exist $@ del $@ > nul
	move $(TMPFILE) $@ > nul

gen-syms.exe: gen-syms.cc symbol.h function.h
	$(CXX) -Fe$@ -Tpgen-syms.cc

gen-ctab.exe: gen-ctab.cc chtype.h
	$(CXX) -Fe$@ -Tpgen-ctab.cc

gen-msg.exe: gen-msg.cc chtab.cc chtype.h msgdef.h
	$(CXX) -MT -Fe$@ -Tpgen-msg.cc

gen-ktab.exe: gen-ktab.cc
	$(CXX) -Fe$@ -Tpgen-ktab.cc

gen-vinf.exe: gen-vinf.cc version.h
	$(CXX) -Fe$@ -Tpgen-vinf.cc

gen-ucs2tab.exe: gen-ucs2tab.cc charset.h fontrange.h
	$(CXX) -Fe$@ -Tpgen-ucs2tab.cc

gen-utf2sjis.exe: gen-utf2sjis.cc
	$(CXX) -Fe$@ -Tpgen-utf2sjis.cc

gen-char-width.exe: gen-char-width.cc jisx0212-width.h
	$(CXX) -Fe$@ -Tpgen-char-width.cc

gen-jisx0212-hash.exe: gen-jisx0212-hash.cc charset.h jisx0212-width.h
	$(CXX) -Fe$@ -Tpgen-jisx0212-hash.cc

gen-jisx0212-width.exe: gen-jisx0212-width.cc ucs2tab.h charset.h
	$(CXX) -Fe$@ -Tpgen-jisx0212-width.cc user32.lib gdi32.lib

gen-lucida-width.exe: gen-lucida-width.cc charset.h
	$(CXX) -Fe$@ -Tpgen-lucida-width.cc user32.lib gdi32.lib

gen-fontrange.exe: gen-fontrange.cc charset.h
	$(CXX) -Fe$@ -Tpgen-fontrange.cc user32.lib gdi32.lib

gen-iso2022state.exe: gen-iso2022state.cc
	$(CXX) -Fe$@ -Tpgen-iso2022state.cc

gen-ibmext.exe: gen-ibmext.cc ucs2tab.h
	$(CXX) -Fe$@ -Tpgen-ibmext.cc

dpp.exe: dpp.cc
	$(CXX) -Fe$@ -Tpdpp.cc

ED_H=alloc.h array.h bignum.h Buffer.h cdecl.h char.h chtype.h chunk.h \
closure.h cons.h data.h dll.h ed.h error.h function.h hash.h \
kanji.h kbd.h largeint.h lisp.h list.h message.h mouse.h msgbox.h \
number.h oledata.h package.h process.h random.h stream.h string.h \
structure.h symbol.h syntax.h sysdep.h timer.h trace.h reconv.h \
vector.h Window.h utils.h xcolor.h readtab.h vfs.h waitobj.h charset.h \
font.h ces.h gime.h statarea.h utimer.h xlist.h mousemsg.h
MAINFRAME_H=mainframe.h dockbar.h pane.h

$(OUTDIR)/Buffer.obj: syntaxinfo.h filer.h glob.h dialogs.h binfo.h $(MAINFRAME_H) buffer-bar.h DnD.h
$(OUTDIR)/ChooseFont.obj: ChooseFont.h
$(OUTDIR)/ColorDialog.obj: ldialog.h ChooseFont.h ColorDialog.h colors.h DnD.h conf.h glob.h
$(OUTDIR)/DnD.obj: ../privctrl/privctrl.h com.h dialogs.h DnD.h filer.h glob.h buffer-bar.h dockbar.h
$(OUTDIR)/StrBuf.obj: StrBuf.h
$(OUTDIR)/Window.obj: conf.h ipc.h wheel.h
$(OUTDIR)/arc-if.obj: arc-if.h comm-arc.h
$(OUTDIR)/archiver.obj: arc-if.h archiver.h except.h comm-arc.h
$(OUTDIR)/buffer-bar.obj: buffer-bar.h DnD.h $(MAINFRAME_H) colors.h
$(OUTDIR)/bytecode.obj: lex.h
$(OUTDIR)/byte-stream.obj: byte-stream.h StrBuf.h encoding.h
$(OUTDIR)/colors.obj: conf.h colors.h filer.h $(MAINFRAME_H)
$(OUTDIR)/com.obj: com.h
$(OUTDIR)/conf.obj: conf.h print.h wheel.h
$(OUTDIR)/ctl3d.obj: ctl3d.h
$(OUTDIR)/data.obj: dataP.h lex.h symtable.h $(MAINFRAME_H)
$(OUTDIR)/dde.obj: safe_ptr.h xdde.h
$(OUTDIR)/dialogs.obj: ../privctrl/privctrl.h dialogs.h conf.h thread.h xstrlist.h
$(OUTDIR)/disp.obj: syntaxinfo.h lucida-width.h jisx0212-hash.h binfo.h $(MAINFRAME_H) regex.h
$(OUTDIR)/doc.obj: except.h mman.h
$(OUTDIR)/dockbar.obj: dockbar.h
$(OUTDIR)/edict.obj: except.h mman.h
$(OUTDIR)/encdec.obj: byte-stream.h StrBuf.h encoding.h
$(OUTDIR)/encoding.obj: encoding.h ibmext.h utf2sjis.h
$(OUTDIR)/environ.obj: environ.h fnkey.h conf.h
$(OUTDIR)/eval.obj: lex.h
$(OUTDIR)/except.obj: except.h
$(OUTDIR)/fileio.obj: except.h mman.h encoding.h
$(OUTDIR)/filer.obj: ../privctrl/privctrl.h dialogs.h DnD.h environ.h filer.h glob.h conf.h colors.h
$(OUTDIR)/font.obj: font.h conf.h
$(OUTDIR)/fnkey.obj: fnkey.h
$(OUTDIR)/glob.obj: glob.h
$(OUTDIR)/init.obj: ../privctrl/privctrl.h ctl3d.h environ.h except.h fnkey.h xdde.h syntaxinfo.h ipc.h sock.h $(MAINFRAME_H)
$(OUTDIR)/insdel.obj: syntaxinfo.h sequence.h byte-stream.h StrBuf.h encoding.h
$(OUTDIR)/ipc.obj: ipc.h
$(OUTDIR)/kanji.obj: kanjitab.h safe_ptr.h byte-stream.h StrBuf.h encoding.h iso2022state.h
$(OUTDIR)/kbd.obj: filer.h dialogs.h encoding.h
$(OUTDIR)/ldialog.obj: ldialog.h ColorDialog.h colors.h DnD.h glob.h ../privctrl/privctrl.h
$(OUTDIR)/lex.obj: lex.h
$(OUTDIR)/list.obj: sequence.h
$(OUTDIR)/listen.obj: listen.h
$(OUTDIR)/lprint.obj: StrBuf.h wstream.h sock.h
$(OUTDIR)/lread.obj: StrBuf.h
$(OUTDIR)/mman.obj: mman.h
$(OUTDIR)/number.obj: num-arith.h
$(OUTDIR)/pathname.obj: dyn-handle.h environ.h pathname.h comm-arc.h except.h mman.h thread.h xstrlist.h
$(OUTDIR)/pane.obj: $(MAINFRAME_H)
$(OUTDIR)/pred.obj: lex.h
$(OUTDIR)/preview.obj: conf.h print.h preview.h wheel.h
$(OUTDIR)/print.obj: print.h lucida-width.h preview.h wheel.h
$(OUTDIR)/printdlg.obj: print.h printdlg.h
$(OUTDIR)/process.obj: dyn-handle.h byte-stream.h StrBuf.h sock.h sockinet.h encoding.h $(MAINFRAME_H)
$(OUTDIR)/regex.obj: regex.h
$(OUTDIR)/search.obj: regex.h
$(OUTDIR)/sequence.obj: safe_ptr.h sequence.h
$(OUTDIR)/stream.obj: sequence.h StrBuf.h wstream.h sock.h sockinet.h
$(OUTDIR)/string.obj: sequence.h StrBuf.h
$(OUTDIR)/symtable.obj: symtable.h
$(OUTDIR)/syntax.obj: syntaxinfo.h
$(OUTDIR)/thread.obj: thread.h
$(OUTDIR)/toplev.obj: ctl3d.h DnD.h environ.h fnkey.h $(MAINFRAME_H) wheel.h
$(OUTDIR)/usertab.obj: $(MAINFRAME_H) colors.h
$(OUTDIR)/usertool.obj: $(MAINFRAME_H)
$(OUTDIR)/ucs2.obj: ucs2tab.h char-width.h
$(OUTDIR)/vector.obj: sequence.h
$(OUTDIR)/version.obj: version.h
$(OUTDIR)/wheel.obj: wheel.h
$(OUTDIR)/wstream.obj: StrBuf.h wstream.h
$(OUTDIR)/xdde.obj: xdde.h
$(OUTDIR)/resolver.obj: resolver.h sockimpl.h
$(OUTDIR)/sock.obj: sock.h resolver.h sockimpl.h
$(OUTDIR)/sockinet.obj: sockinet.h sock.h sockimpl.h

$(OBJS): $(ED_H)

depend:
	set CC=$(CXX)
	set CFLAGS=$(CXXFLAGS)
	mkdep $(SRCS:.cc=)

.cc.cod:
	$(CXX) $(CXXFLAGS) -FAcs -c -Tp$<
.cc.asm:
	$(CXX) $(CXXFLAGS) -Fa -c -Tp$<


PROFDIR=$(OUTDIR)\prof

prof-dir: force
	-@if not exist $(PROFDIR)\nul mkdir $(PROFDIR)

prof-target: $(PROFDIR)\$(BUILD)

$(PROFDIR)/$(BUILD): $(OBJS) $(PRIVCTRLOBJS) $(OUTDIR)/xyzzy.res
	$(LD) @<<
  -out:$@ $(OBJS) $(PRIVCTRLOBJS) -profile -stack:0x2000000
	-map:$(PROFDIR)/xyzzy.map -subsystem:windows $(LIBS) $(OUTDIR)/xyzzy.res
<<

profile: prof-dir prof-target
	@cd ..
	prep -nologo -OM -FT src/$(PROFDIR)/xyzzy
	profile -nologo src/$(PROFDIR)/xyzzy
	prep -nologo -M src/$(PROFDIR)/xyzzy
	plist -nologo -sc src/$(PROFDIR)/xyzzy

