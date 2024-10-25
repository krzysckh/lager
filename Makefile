LDFLAGS=-lraylib -lm
CFLAGS=-Wall -g -fsanitize=address

lager.c: lager.scm
	ol-rl -o lager.c lager.scm
lager: lager.c
	$(CC) -o lager lager.c $(CFLAGS) $(LDFLAGS)
clean:
	rm -f lager
