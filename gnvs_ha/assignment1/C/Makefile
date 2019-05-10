CC=cc
AR=ar
RANLIB=ranlib

CFLAGS=-Wall -Wextra -std=c89 -pedantic -O2
LDFLAGS=-s

BIN=moep
OBJ=$(BIN:=.o)

all: $(BIN)

$(BIN): $(OBJ)

.o:
	$(CC) $(LDFLAGS) -o $@ $(OBJ)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(BIN) $(OBJ)

.PHONY: all clean install uninstall
