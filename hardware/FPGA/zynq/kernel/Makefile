ARCH=arm
CROSS_COMPILE=arm-linux-gnueabihf-
KERNEL_DIR=~/src/linux-xlnx/

tart-objs := main.o
obj-m += tart.o

all:
	make ARCH=$(ARCH) CROSS_COMPILE=$(CROSS_COMPILE) M=$(PWD) -C $(KERNEL_DIR) modules

clean:
	make ARCH=$(ARCH) CROSS_COMPILE=$(CROSS_COMPILE) M=$(PWD) -C $(KERNEL_DIR) clean

