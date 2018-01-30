/*
 * Userside of tart ip driver.
 *
 * Copyright 2017 - 2018 Mytchel Hammond
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2
 * as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdint.h>

#include "../tart.h"

int main(void)
{
  struct tart_reg tr;
  size_t len, l;
  int fd, r;
  void *m;

  fd = open("/dev/tart", O_RDWR);
  if (fd < 0) {
    printf("failed to open /dev/tart %i\n", fd);
    return fd;
  }

  r = ioctl(fd, TART_IOCTL_DMA, TART_IOCTL_DMA_ENABLE);
  printf("dma got %d\n", r);

  tr.reg = 3 << 2 | 1;
  r = ioctl(fd, TART_IOCTL_READ_REG, &tr);
  printf("read got %d, val = %d\n", r, tr.val);

  tr.reg = 5 << 2 | 1;
  tr.val = 10;
  r = ioctl(fd, TART_IOCTL_WRITE_REG, &tr);
  printf("write got %d\n", r);

  len = 4096 * 4;
  m = mmap(NULL, len, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
  if (m == MAP_FAILED) {
    perror("mmap failed\n");
    return 1;
  }

  printf("mapped 0x%x bytes at %p\n", len, m);

  for (l = 0; l < len; l += sizeof(uint32_t) * 0x100) {
	printf("0x%04x = 0x%08x\n", l, 
	    *((uint32_t *) ((size_t) m + l)));
  }

  munmap(m, len);

  close(fd);

  return 0;
}

