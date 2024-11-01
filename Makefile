LDFLAGS:=-L/usr/local/lib -L. -l:$(COMP_RAYLIB) -lm
CFLAGS=-O2 -Wall -g -I/usr/local/include
OLFLAGS=-O3

COMP_OLRL=./ol-rl-x86_64-linux-gnu
COMP_RAYLIB=libraylib5-linux64-opengl33.a

OS!=uname -s

.if $(OS) == "OpenBSD"
LDFLAGS:=$(LDFLAGS) -lpthread
.endif

$(COMP_RAYLIB):
	wget -O $@ "https://pub.krzysckh.org/raylib-bin/$@"
$(COMP_OLRL):
	wget -O $@ "https://pub.krzysckh.org/$@"
	chmod +x $@
ol-rl.exe:
	wget -O $@ "https://pub.krzysckh.org/$@"
libraylib5-winlegacy.a:
	wget -O $@ "https://pub.krzysckh.org/$@"
lager-win.c: ol-rl.exe lager.scm
	wine ol-rl.exe -o lager-win.c lager.scm
lager.exe: lager-win.c libraylib5-winlegacy.a
	i686-w64-mingw32-gcc -static -o lager.exe -I/usr/local/include lager-win.c -L. -l:libraylib5-winlegacy.a -lm -lopengl32 -lwinmm -lgdi32 -lws2_32
lager-server-win.c: ol-rl.exe lager-server.scm
	wine ol-rl.exe -o lager-server-win.c lager-server.scm
lager-server.exe: lager-server-win.c libraylib5-winlegacy.a
	i686-w64-mingw32-gcc -static -o lager-server.exe -I/usr/local/include lager-server-win.c -L. -l:libraylib5-winlegacy.a -lm -lopengl32 -lwinmm -lgdi32 -lws2_32
lager.c: $(COMP_OLRL) lager.scm
	$(COMP_OLRL) $(OLFLAGS) -o lager.c lager.scm
lager-server.c: $(COMP_OLRL) lager-server.scm
	$(COMP_OLRL) $(OLFLAGS) -o lager-server.c lager-server.scm
lager-bin: lager.c $(COMP_RAYLIB)
	$(CC) -o lager-bin lager.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -f lager-bin lager.exe *.c
packup: lager.exe lager-server.exe lager-bin lager.c
	mkdir -p build
	cp -v lager.exe build
	cp -v lager-server.exe build
	cp -v lager-bin build/lager-`$(CC) -dumpmachine`
	cp -v lager.c build
	cp -v lager-server.c build
	cp -v _quickbuild.sh build/quickbuild.sh
pubcpy: lager.exe lager-server.exe
	yes | pubcpy tmp lager.exe
	yes | pubcpy tmp lager-server.exe
test: $(COMP_OLRL)
	$(COMP_OLRL) -r lager-server.scm & ( sleep 1 ; $(COMP_OLRL) -r lager.scm "local player" localhost )
test-local:
	ol-rl -r lager-server.scm & ( sleep 1 ; ol-rl -r lager.scm "local player" localhost )
win-test: lager.exe lager-server.exe
	wine lager-server.exe & ( sleep 2 ; wine lager.exe "local player" localhost )
