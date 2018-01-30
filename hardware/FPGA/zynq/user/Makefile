ARCH=arm
CROSS_COMPILE=arm-linux-gnueabihf-

CC=$(CROSS_COMPILE)gcc

tart: main.c
	$(CC) -o $@ $^

clean:
	rm -rf tart main.o

