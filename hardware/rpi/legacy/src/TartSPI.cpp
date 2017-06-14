/*
 * TODO:
 *  + parse command-line arguments to determine action to take;
 *
 */

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

#define REG_STREAM 0x00
#define REG_DATA0  0x01
#define REG_DATA1  0x02
#define REG_DATA2  0x03
#define REG_DELAY  0x05
#define REG_DEBUG  0x06
#define REG_AQUIRE 0x07
#define REG_STATUS 0x0c
#define REG_EXTRA  0x0d
#define REG_MISC   0x0e
#define REG_RESET  0x0f

#define REG_WRITE  0x80


int TartSpiReset (int spi_device)
{
  struct spi_ioc_transfer spi[1];
  //   struct spi_ioc_transfer spi;
  // const struct spi_ioc_transfer *cmd = &spi;
  int ret = -1;
  int *spi_cs_fd;
  unsigned char rst_cmd[4] = {REG_WRITE | REG_RESET, 0x01, 0, 0};

  spi_cs_fd = SET_SPIDEV(spi_device);

  //   memset(&spi, 0, sizeof (spi));
  memset(spi, 0, sizeof (spi[0]));
  spi[0].tx_buf        = (unsigned long)(rst_cmd+0);
  spi[0].rx_buf        = (unsigned long)(rst_cmd+2);
  spi[0].len           = 2;
  spi[0].delay_usecs   = 1000;
  spi[0].speed_hz      = spi_speed;
  spi[0].bits_per_word = spi_bitsPerWord;

	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(1), &spi);
  // 	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(1), &cmd);

	if(ret < 0) {
    perror("Error - Problem transmitting spi data..ioctl");
		exit(1);
	} else {
    printf("Read back: {%02x, %02x}\n", (unsigned)rst_cmd[2], (unsigned)rst_cmd[3]);
  }

  return ret;
}

int TartSpiAquire (int spi_device)
{
  struct spi_ioc_transfer spi[1];
  int ret = -1;
  int *spi_cs_fd;
  unsigned char daq_cmd[8] = {REG_WRITE | REG_AQUIRE, 0x01, 0x01, 0x01, 0, 0, 0, 0};

  spi_cs_fd = SET_SPIDEV(spi_device);
  memset(spi, 0, sizeof (spi[0]));

  spi[0].tx_buf        = (unsigned long)(daq_cmd+0);
  spi[0].rx_buf        = (unsigned long)(daq_cmd+4);
  spi[0].len           = 4;
  spi[0].speed_hz      = spi_speed;
  spi[0].bits_per_word = spi_bitsPerWord;

	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(1), &spi);

	if(ret < 0) {
    perror("Error - Problem transmitting spi data..ioctl");
		exit(1);
	} else {
    printf("Read back: {%02x, %02x, %02x, %02x}\n", (unsigned)daq_cmd[4]
           , (unsigned)daq_cmd[5], (unsigned)daq_cmd[6], (unsigned)daq_cmd[7]);
  }

  return ret;
}

int TartSpiStatus (int spi_device)
{
  struct spi_ioc_transfer spi[1];
  int ret = -1;
  int *spi_cs_fd;
  //   unsigned char daq_cmd[8] = {REG_STATUS, 0, 0, 0, 0, 0, 0, 0};
  unsigned char daq_cmd[8] = {REG_EXTRA, 0, 0, 0, 0, 0, 0, 0};

  spi_cs_fd = SET_SPIDEV(spi_device);
  memset(spi, 0, sizeof (spi[0]));

  spi[0].tx_buf        = (unsigned long)(daq_cmd+0);
  spi[0].rx_buf        = (unsigned long)(daq_cmd+4);
  spi[0].len           = 4;
  spi[0].speed_hz      = spi_speed;
  spi[0].bits_per_word = spi_bitsPerWord;

	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(1), &spi);

	if(ret < 0) {
    perror("Error - Problem transmitting spi data..ioctl");
		exit(1);
	} else {
    printf("Read back: {%02x, %02x, %02x, %02x}\n", (unsigned)daq_cmd[4]
           , (unsigned)daq_cmd[5], (unsigned)daq_cmd[6], (unsigned)daq_cmd[7]);
  }

  return ret;
}

int TartSpiReadPackets (const int spi_device, unsigned char *data, int packets, const int length)
{
  struct spi_ioc_transfer spi[++packets];
  int ret = -1;
  int *spi_cs_fd;
  const unsigned char read_cmd[8] = {REG_STREAM, 0xe5, 0, 0, 0, 0, 0, 0};

  spi_cs_fd = SET_SPIDEV(spi_device);
  memset(spi, 0, packets*sizeof (spi[0]));

	// Build SPI transactions for a read register command, and then to receive
  // data:
  spi[0].tx_buf        = (unsigned long)(&read_cmd);
  spi[0].len           = 2;
  spi[0].speed_hz      = spi_speed;
  spi[0].bits_per_word = spi_bitsPerWord;

  for (int i = 1; i < packets; i++) {
    spi[i].rx_buf        = (unsigned long)(data+(i-1)*length);
    spi[i].len           = (unsigned int) length;
    spi[i].speed_hz      = spi_speed;
    spi[i].bits_per_word = spi_bitsPerWord;
  }
	ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(packets), &spi) ;

	if(ret < 0) {
    perror("Error - Problem transmitting spi data..ioctl");
		exit(1);
	}
  
  return ret;
}

int TartSpiRead (const int spi_device, unsigned char *data, const int packets, const int length)
{
  struct spi_ioc_transfer spi[2];
  int ret = -1, tot = 0;
  int *spi_cs_fd;
  const unsigned char read_cmd[8] = {REG_STREAM, 0xe5, 0, 0, 0, 0, 0, 0};

  spi_cs_fd = SET_SPIDEV(spi_device);
  memset(spi, 0, 2*sizeof (spi[0]));

	// Build SPI transactions for a read register command, and then to receive
  // data:
  spi[0].tx_buf        = (unsigned long)(&read_cmd);
  spi[0].len           = 2;
  spi[0].speed_hz      = spi_speed;
  spi[0].bits_per_word = spi_bitsPerWord;

  for (int i = 0; i < packets; i++) {
    spi[1].rx_buf        = (unsigned long)(data+i*length);
    spi[1].len           = (unsigned int) length;
    spi[1].speed_hz      = spi_speed;
    spi[1].bits_per_word = spi_bitsPerWord;
    ret = ioctl(*spi_cs_fd, SPI_IOC_MESSAGE(2), &spi) ;

    if(ret < 0) {
      perror("Error - Problem transmitting spi data..ioctl");
      exit(1);
    }
    tot += ret;
  }
  
  return tot;
}

#define READ_LENGTH 32768
#define NUM_PACKETS 512
#define NUM_TRANSFERS 3


int main(int argc, char* argv[])
{
  int res;
  res = SpiOpenPort(0);
  printf("SPI port-open result = %d\n", res);

  if (argc > 1) {
    if (argv[1][0] == 'r') {
      res = TartSpiReset(0);
      printf("SPI reset result = %d\n", res);
    } else if (argv[1][0] == 'a') {
      res = TartSpiAquire(0);
      printf("SPI aquisition ON result = %d\n", res);
    } else if (argv[1][0] == 's') {
      res = TartSpiStatus(0);
      printf("SPI status result = %d\n", res);
    }
  }

  /*
  printf("SPI beginning transfer...\n");
  unsigned char* data = (unsigned char*) malloc(READ_LENGTH*NUM_PACKETS);
  for (int i=0; i<NUM_TRANSFERS; i++) {
    res = TartSpiRead(0, data, NUM_PACKETS, READ_LENGTH);
    printf("SPI read-back result = %d\n", res);
  }
  free((void*) data);
  */

  res = SpiClosePort(0);
  printf("SPI port-close result = %d\n", res);

  return 0;
}
