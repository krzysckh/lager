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
libraylib5-web.a:
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
weblager.c: $(COMP_OLRL) lager.scm
	$(COMP_OLRL) $(OLFLAGS) -o weblager.c lager.scm -i WEBLAGER
lager-server.c: $(COMP_OLRL) lager-server.scm
	$(COMP_OLRL) $(OLFLAGS) -o lager-server.c lager-server.scm
lager-bin: lager.c $(COMP_RAYLIB)
	$(CC) -o lager-bin lager.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -fr build lager-bin lager.exe *.c
build/lager.html: weblager.c libraylib5-web.a
	mkdir -p build
	emcc -O2 -DPLATFORM_WEB -I/usr/local/include weblager.c \
		libraylib5-web.a --shell-file emshell.html \
		-o build/lager.html \
		-s USE_GLFW=3 -s ERROR_ON_UNDEFINED_SYMBOLS=0 \
		-lwebsocket.js \
		-s WEBSOCKET_SUBPROTOCOL=binary \
		-s ALLOW_MEMORY_GROWTH=1 -s ASYNCIFY -s ASSERTIONS=0 || true
build/weblager.zip: build/lager.html
	rm -f build/weblager.zip
	mv build/lager.html build/index.html
	cd build ; zip weblager.zip index.html lager.js lager.wasm
	rm build/index.html build/lager.wasm build/lager.js
packup: lager.exe lager-server.exe lager-bin lager.c lager-server.c build/weblager.zip
	mkdir -p build
	cp -v lager.exe build
	cp -v lager-server.exe build
	cp -v lager-bin build/lager-`$(CC) -dumpmachine`
	cp -v lager.c build
	cp -v lager-server.c build
	cp -v _quickbuild.sh build/quickbuild.sh
	cp -v README.md build
pubcpy: lager.exe lager-server.exe
	yes | pubcpy tmp lager.exe
	yes | pubcpy tmp lager-server.exe
test: $(COMP_OLRL)
	$(COMP_OLRL) -r lager-server.scm & ( sleep 1 ; $(COMP_OLRL) -r lager.scm ) & ( read _ )
test2: $(COMP_OLRL)
	$(COMP_OLRL) -r lager-server.scm & ( sleep 1 ; $(COMP_OLRL) -r lager.scm ) & ( sleep 1 ; $(COMP_OLRL) -r lager.scm ) & ( read _ )
win-test: lager.exe lager-server.exe
	wine lager-server.exe & ( sleep 2 ; wine lager.exe )
