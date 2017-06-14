#ifndef __SPI_XFER_H__
#define __SPI_XFER_H__

extern int spi_cs0_fd;				//file descriptor for the SPI device
extern int spi_cs1_fd;				//file descriptor for the SPI device
extern unsigned char spi_mode;
extern unsigned char spi_bitsPerWord;
extern unsigned int spi_speed;

int SpiOpenPort (int spi_device);
int SpiClosePort (int spi_device);
int SpiWriteAndRead (int spi_device, unsigned char *data, int length);

#endif // __SPI_XFER_H__
