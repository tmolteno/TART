#include <fcntl.h>				//Needed for SPI port
#include <sys/ioctl.h>			//Needed for SPI port
#include <linux/spi/spidev.h>	//Needed for SPI port
#include <unistd.h>			//Needed for SPI port
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <unistd.h>
#include <cstring>

#include "spi-xfer.h"


#define SET_SPIDEV(dev) ((dev) ? &spi_cs1_fd : &spi_cs0_fd)


int TartSpiReset (int spi_device)
{
  struct spi_ioc_transfer spi;
  const struct spi_ioc_transfer *cmd = &spi;
  int ret = -1;
  int *spi_cs_fd;
  unsigned char rst_cmd[4] = {0x8f, 0x01};

  if (spi_device)
    spi_cs_fd = &spi_cs1_fd;
  else
    spi_cs_fd = &spi_cs0_fd;

  memset(cmd, 0, sizeof (spi));

  spi.tx_buf        = (unsigned long)(rst_cmd+0);
  spi.rx_buf        = (unsigned long)(rst_cmd+2); // receive into "data"
  spi.len           = 2;
  spi.speed_hz      = spi_speed;
  spi.bits_per_word = spi_bitsPerWord;

	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(1), &cmd) ;

	if(ret < 0) {
    perror("Error - Problem transmitting spi data..ioctl");
		exit(1);
	} else {
    printf("Read back: {%02x, %02x}\n", (unsigned)rst_cmd[2], (unsigned)rst_cmd[3]);
  }

  return ret;
}

int TartSpiRead (int spi_device, unsigned char *data, int length)
{
  struct spi_ioc_transfer spi[2];
  int ret = -1;
  int *spi_cs_fd;
  const unsigned char read_cmd[8] = {0x02, 0xe5, 0, 0, 0, 0, 0, 0};

  if (spi_device)
    spi_cs_fd = &spi_cs1_fd;
  else
    spi_cs_fd = &spi_cs0_fd;

  memset(spi, 0, 2*sizeof (spi[0]));

	// Build SPI transactions for a read register command, and then to receive
  // data:
  spi[0].tx_buf        = (unsigned long)(&read_cmd);
  spi[0].len           = 2;
  spi[0].speed_hz      = spi_speed;
  spi[0].bits_per_word = spi_bitsPerWord;

  spi[1].rx_buf        = (unsigned long) data; // receive into "data"
  spi[1].len           = (unsigned int) length;
  spi[1].speed_hz      = spi_speed;
  spi[1].bits_per_word = spi_bitsPerWord;

	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(2), &spi) ;

	if(ret < 0) {
    perror("Error - Problem transmitting spi data..ioctl");
		exit(1);
	}
  
  return ret;
}


int main(int argc, char* argv[])
{
  int res;
  res = SpiOpenPort(0);
  printf("SPI port-open result = %d\n", res);

  res = SpiClosePort(0);
  printf("SPI port-close result = %d\n", res);

  return 0;
}
