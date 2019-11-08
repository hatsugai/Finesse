CFLAGS=-std=c11 -O3
CDEFS=
LDFLAGS=-lm
GOSH=gosh

TARGET=finesse
INITS=base.init boot.init
VM_FILES=vminst-defs.scm vminst-defs.h vminst-defs.c
BUILTIN_FILES=builtin.init builtin.h builtin-table.c
OBJECTS=main.o vm.o object.o io.o builtin.o vminst-defs.o builtin-table.o
SOURCES=builtin-table.c builtin.c io.c main.c object.c vm.c vminst-defs.c

%.o:	%.c
	$(CC) -c $(CFLAGS) $(CDEFS) $<

%.init:	%.scm
	$(GOSH) xc.scm $< > $@

all:	$(TARGET) $(INITS) $(BUILTIN_FILES)

$(TARGET):	$(OBJECTS)
	$(CC) -o $@  $(CFLAGS) $^ $(LDFLAGS)

$(VM_FILES): vminst.scm
	$(GOSH) $<

$(BUILTIN_FILES): builtin.scm builtin-def-base.scm
	$(GOSH) builtin.scm "builtin" builtin-def-base.scm

dep:	$(VM_FILES) $(BUILTIN_FILES)
	gcc $(CFLAGS) $(CDEFS) -MM $(SOURCES) > .depend

TAGS:	*.scm *.c *.h
	etags $^

clean:
	-rm $(TARGET) *.o *.init $(TARGETS) $(VM_FILES) $(BUILTIN_FILES)


-include .depend
