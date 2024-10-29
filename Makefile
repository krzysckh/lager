LDFLAGS:=-L/usr/local/lib -lraylib -lm
CFLAGS=-O2 -Wall -g -I/usr/local/include
OLFLAGS=-O3

OS!=uname -s

.if $(OS) == "OpenBSD"
LDFLAGS:=$(LDFLAGS) -lpthread
.endif

ol-rl.exe:
	wget -O $@ "https://pub.krzysckh.org/$@"
libraylib5-winlegacy.a:
	wget -O $@ "https://pub.krzysckh.org/$@"
lager-win.c: ol-rl.exe lager.scm
	wine ol-rl.exe -o lager-win.c lager.scm
lager.exe: lager-win.c libraylib5-winlegacy.a
	i686-w64-mingw32-gcc -static -o lager.exe -I/usr/local/include lager-win.c -L. -l:libraylib5-winlegacy.a -lm -lopengl32 -lwinmm -lgdi32 -lws2_32
lager.c: lager.scm
	ol-rl $(OLFLAGS) -o lager.c lager.scm
lager-bin: lager.c
	$(CC) -o lager-bin lager.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -f lager-bin lager.exe *.c
packup: lager.exe
	mkdir -p build
	cp -v lager.exe build
pubcpy: lager.exe
	yes | pubcpy tmp lager.exe
test:
	ol-rl -r lager-server.scm & ( sleep 1 ; ol-rl -r lager.scm )
