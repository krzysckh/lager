LDFLAGS:=-L/usr/local/lib -lraylib -lm
CFLAGS=-O2 -Wall -g -I/usr/local/include
OLFLAGS=-O3

OS!=uname -s

.if $(OS) == "OpenBSD"
LDFLAGS:=$(LDFLAGS) -lpthread
.endif

lager.c: lager.scm
	ol-rl $(OLFLAGS) -o lager.c lager.scm
lager: lager.c
	$(CC) -o lager lager.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -f lager
