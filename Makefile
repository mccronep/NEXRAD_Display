SHELL=/bin/sh

include MakeInclude

CFLAGS=-I$(RADAR_LDM)/include -I/usr/local/include -g
CMD = 		nexradII.exe

all: nexradII

nexradII:	nexradII.o getbuf.o
	$(CC) -o $(CMD) $(NEXRAD_LDFLAGS) nexradII.o getbuf.o -L$(RADAR_LDM)/lib -lldm -lbz2

clean:
	rm -f *.o

install : all
	@if [ -d $(EXE_DIR) ]; then set +x; \
	else (set -x; mkdir $(EXE_DIR)); fi
	mv $(CMD) $(EXE_DIR)
