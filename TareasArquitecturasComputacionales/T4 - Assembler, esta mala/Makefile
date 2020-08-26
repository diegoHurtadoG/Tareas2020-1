CC=gcc
CFLAGS= -g -m32 -std=c99 -Wall -pedantic
LDFLAGS= -g

%.s: %.c
	$(CC) -O -m32 -std=c99 -Wall -pedantic -S $<

test-sort-x86: test-sort-x86.o sort-x86.o

test-sort-x86-apnom: test-sort-x86.o sort-x86-apnom.o
	$(CC) $(CFLAGS) test-sort-x86.o sort-x86-apnom.o -o test-sort-x86-apnom

test-sort-c: test-sort-x86.o sort-c.o
	$(CC) $(CFLAGS) test-sort-x86.o sort-c.o -o test-sort-c

clean:
	rm -f *.o test-sort-x86 test-sort-x86-apnom test-sort-c
