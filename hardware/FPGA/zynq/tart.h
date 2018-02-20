#define TART_MAJOR_NUM          144

#define TART_IOCTL_DMA \
  _IOW(TART_MAJOR_NUM, 0, bool)

#define TART_IOCTL_DMA_ENABLE   0
#define TART_IOCTL_DMA_DISABLE  1

#define TART_IOCTL_READ_REG \
  _IOWR(TART_MAJOR_NUM, 1, struct tart_reg)

#define TART_IOCTL_WRITE_REG \
  _IOWR(TART_MAJOR_NUM, 2, struct tart_reg)

struct tart_reg {
	uint8_t reg;
	uint8_t val;
};

