/*
 * Driver for tart ip module.
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

#include <linux/bitops.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/module.h>
#include <linux/of.h>
#include <linux/of_device.h>
#include <linux/of_irq.h>
#include <linux/of_platform.h>
#include <linux/of_address.h>
#include <linux/interrupt.h>
#include <linux/io.h>
#include <linux/irq.h>
#include <linux/irqchip/chained_irq.h>
#include <linux/irqdomain.h>
#include <linux/slab.h>
#include <linux/pm_runtime.h>
#include <linux/dmaengine.h>
#include <linux/of_dma.h>
#include <linux/dma/xilinx_dma.h>
#include <asm/uaccess.h>
#include <linux/mutex.h>
#include <linux/list.h>

#include "../tart.h"

#define DEVICE_NAME "tart"
#define CLASS_NAME "tart"

struct dma_mem {
	struct dma_mem *next;
	size_t offset;
};

static DEFINE_MUTEX(tart_mutex);

static struct class *class;
static struct device *dev;

static struct dma_chan *chan;
static u32 *regs;

static struct resource reserved;

static struct dma_mem *holders = NULL;
static struct dma_mem *free    = NULL;
static struct dma_mem *full    = NULL;

static void tart_mmap_open(struct vm_area_struct *vma)
{
  printk(KERN_INFO "tart mmap open\n");
}

static void tart_mmap_close(struct vm_area_struct *vma)
{
  printk(KERN_INFO "tart mmap close\n");
}

static int tart_mmap_fault(struct vm_area_struct *vma, struct vm_fault *vmf)
{
  unsigned long offset, addr;
  struct dma_mem *m;

  printk(KERN_INFO "tart mmap fault at %p\n", vmf->virtual_address);

  if ((unsigned long) vmf->virtual_address > vma->vm_end) {
    return VM_FAULT_SIGBUS;
  }

  offset = ((unsigned long) vmf->virtual_address - vma->vm_start)
    >> PAGE_SHIFT;

  m = (struct dma_mem *) vma->vm_private_data;
  while (offset > 0 && m != NULL) {
    offset--;
    m = m->next;
  }

  if (m == NULL) {
    return VM_FAULT_SIGBUS;
  }

  addr = reserved.start + m->offset;
  printk(KERN_INFO "tart giving page at 0x%lx\n", addr);

  vmf->page = pfn_to_page(addr >> PAGE_SHIFT);

  get_page(vmf->page);

  /* I am not sure if get_page should be called. If it should I
     do not know if we have to release it on mmap_close. Seeing
     as we are using reserved memory we can manage it ourselves
     so it shouldn't matter. */

  return 0;
}

struct vm_operations_struct tart_mmap_ops = {
  .open   = tart_mmap_open,
  .close  = tart_mmap_close,
  .fault  = tart_mmap_fault,
};

static int tart_mmap(struct file *f, struct vm_area_struct *vma)
{
  struct dma_mem *head, **h, *m;
  size_t len;

  printk(KERN_INFO "tart mmap\n");

  h = &head;
  for (len = 0; len < vma->vm_end - vma->vm_start; len += PAGE_SIZE) {
    if (full == NULL) {
      printk(KERN_ERR "TART has no fulled pages!\n");

      *h = full;
      full = head;

      return -1;
    }

    m = full;
    full = m->next;

    *h = m;
    h = &m->next;
  }

  *h = NULL;

  printk("mmap using pages:\n");
  for (m = head; m != NULL; m = m->next) {
    printk("0x%u\n", m->offset);
  }

  vma->vm_private_data = head;
  vma->vm_ops = &tart_mmap_ops;
  vma->vm_flags |= VM_DONTEXPAND | VM_DONTDUMP | VM_PFNMAP;

  tart_mmap_open(vma);

  return 0;
}

static int tart_open(struct inode *in, struct file *f)
{
  printk(KERN_INFO "tart open\n");

  if (!mutex_trylock(&tart_mutex)) {
    printk(KERN_ALERT "tart busy\n");
    return -EBUSY;
  }

  return 0;
}

static int tart_release(struct inode *in, struct file *f)
{
  printk(KERN_INFO "tart release\n");
  mutex_unlock(&tart_mutex);

  return 0;
}

static u32 tart_reg(u8 i)
{
  u32 dev, adr, a;

  dev = i >> 2;
  adr = i & 3;

  a = ((dev << 5) | (adr)) << 2;

  return a;
}

int tart_dma(void);

static long tart_unlocked_ioctl(struct file *f, unsigned int cmd, unsigned long arg)
{
  struct tart_reg tr;
  u32 *r;

  printk(KERN_INFO "tart unlocked ioctl %u, %lu\n", cmd, arg);

  switch (cmd) {
    case TART_IOCTL_DMA:
      if (arg == TART_IOCTL_DMA_ENABLE) {
	printk("should enable dma\n");
      } else {
	printk("should disable dma\n");
      }

      tart_dma();

      break;

    case TART_IOCTL_READ_REG:
      if (copy_from_user(&tr, (void *) arg, sizeof(tr))) {
	printk(KERN_ERR "copy_from_user failed for 0x%p\n", (void *) arg);
	return -EFAULT;
      }

      printk(KERN_INFO "tart read reg 0x%x\n", tr.reg);

      r = regs + tart_reg(tr.reg);
      tr.val = 5;/**r;*/

      printk(KERN_INFO "tart read reg 0x%x = 0x%x\n", tr.reg, tr.val);

      if (copy_to_user((void *) arg, &tr, sizeof(tr))) {
	printk(KERN_ERR "copy_to_user failed for 0x%p\n", (void *) arg);
	return -EFAULT;
      }

      break;

    case TART_IOCTL_WRITE_REG:
      if (copy_from_user(&tr, (void *) arg, sizeof(tr))) {
	printk(KERN_ERR "copy_from_user failed for 0x%p\n", (void *) arg);
	return -EFAULT;
      }

      printk(KERN_INFO "tart write reg 0x%x = 0x%x\n", tr.reg, tr.val);

      r = regs + tart_reg(tr.reg);
      /**r = tr.val;*/

      printk(KERN_INFO "tart wrote reg\n");
      break;

    default:
      return -EINVAL;
  }

  return 0;
}

static struct file_operations tart_fops = {
  .owner           = THIS_MODULE,
  .open            = tart_open,
  .unlocked_ioctl  = tart_unlocked_ioctl,
  .mmap            = tart_mmap,
  .release         = tart_release,
};

static void axidma_sync_callback(void *cmp)
{
  printk(KERN_INFO "sync_callback\n");
  complete(cmp);
}

static dma_cookie_t axidma_prep_buffer(struct dma_chan *chan, dma_addr_t buf, size_t len,
    enum dma_transfer_direction dir, struct completion *cmp)
{
  enum dma_ctrl_flags flags = DMA_CTRL_ACK | DMA_PREP_INTERRUPT;
  struct dma_async_tx_descriptor *chan_desc;

  chan_desc = dmaengine_prep_slave_single(chan, buf, len, dir, flags);

  if (chan_desc) {
    chan_desc->callback = axidma_sync_callback;
    chan_desc->callback_param = cmp;
    return dmaengine_submit(chan_desc);
  } else {
    printk(KERN_ERR "dmaengine_prep_slave_single error\n");
    return -EBUSY;
  }
}

static void axidma_start_transfer(struct dma_chan *chan, struct completion *cmp,
    dma_cookie_t cookie)
{
  unsigned long timeout = msecs_to_jiffies(5000);
  enum dma_status status;

  init_completion(cmp);
  dma_async_issue_pending(chan);

  printk("Waiting for dma\n");

  timeout = wait_for_completion_timeout(cmp, timeout);
  status = dma_async_is_tx_complete(chan, cookie, NULL, NULL);

  if (timeout == 0) {
    printk(KERN_ERR "DMA timed out\n");
  } else if (status != DMA_COMPLETE) {
    printk(KERN_ERR "DMA returned completion status of: %s\n",
	status == DMA_ERROR ? "error" : "in progress");
  }
}

int tart_dma(void)
{
  struct dma_mem *m, **n;
  struct completion cmp;
  dma_cookie_t cookie;
  dma_addr_t handle;
  size_t len;
  void *buf;

  if (free == NULL) {
    printk(KERN_ERR "TART out of free pages!\n");
    return -1;
  }

  m = free;
  free = m->next;

  buf = (void *) (reserved.start + m->offset);
  len = PAGE_SIZE;

  printk(KERN_INFO "TART dma into %p\n", buf);
  /*
     handle = dma_map_single(chan->device->dev, buf, len, DMA_FROM_DEVICE);

     cookie = axidma_prep_buffer(chan, handle, len, DMA_FROM_DEVICE, &cmp);

     if (dma_submit_error(cookie)) {
     printk(KERN_ERR "xdma_prep_buffer error\n");
     return -1;
     }

     printk(KERN_INFO "Starting DMA transfers\n");

     axidma_start_transfer(chan, &cmp, cookie);

     dma_unmap_single(chan->device->dev, handle, len, DMA_FROM_DEVICE);
   */

  printk(KERN_INFO "full was %p\n", full);
  for (n = &full; *n != NULL; n = &(*n)->next)
    ;

  *n = m;
  m->next = NULL;

  printk(KERN_INFO "full now %p\n", full);

  return 0;
}

int test_regs(void)
{
  u32 dev, adr, a, v;

  printk(KERN_INFO "reset\n");
  dev = 3, adr = 3;	
  a = ((dev << 5) | (adr)) << 2;
  v = 1;
  printk(KERN_INFO "write %i 0x%x -> dev %i, adr %i / 0x%x\n", v, v, dev, adr, a);
  *((u32 *) ((size_t) regs + a)) = v;

  printk(KERN_INFO "enable capture\n");
  dev = 0, adr = 3;	
  a = ((dev << 5) | (adr)) << 2;
  v = (1<<7);
  printk(KERN_INFO "write %i 0x%x -> dev %i, adr %i / 0x%x\n", v, v, dev, adr, a);
  *((u32 *) ((size_t) regs + a)) = v;

  printk(KERN_INFO "enable acquire\n");
  dev = 1, adr = 3;	
  a = ((dev << 5) | (adr)) << 2;
  v = (1<<7);
  printk(KERN_INFO "write %i 0x%x -> dev %i, adr %i / 0x%x\n", v, v, dev, adr, a);
  *((u32 *) ((size_t) regs + a)) = v;

  printk(KERN_INFO "enable correlator\n");
  dev = 2, adr = 3;
  a = ((dev << 5) | (adr)) << 2;
  v = (1<<7);
  printk(KERN_INFO "write %i 0x%x -> dev %i, adr %i / 0x%x\n", v, v, dev, adr, a);
  *((u32 *) ((size_t) regs + a)) = v;

  return 0;
}

static int tart_of_probe(struct platform_device *pdev)
{
  size_t offset, count, i;
  struct device_node *np;
  struct resource *res;
  struct dma_mem *m;
  int rc;

  /* TODO: free on error. */

  printk(KERN_INFO "probe tart dev\n");

  res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
  regs = devm_ioremap_resource(&pdev->dev, res);

  if (IS_ERR(regs)) {
    printk(KERN_ERR "Failed to remap resource!\n");
    return -1;
  }

  printk(KERN_INFO "got regs at 0x%p\n", regs);

  chan = dma_request_slave_channel(&pdev->dev, "rawdma");

  if (chan == NULL || IS_ERR(chan)) {
    printk(KERN_ERR "failed to get channel\n");
    return PTR_ERR(chan);
  }

  printk(KERN_INFO "got dma channel 0x%p %s\n", chan, dma_chan_name(chan));

  np = of_parse_phandle(pdev->dev.of_node, "memory-region", 0);
  if (!np) {
    printk(KERN_ERR "Failed to get memory-region!\n");
    return -1;
  }

  rc = of_address_to_resource(np, 0, &reserved);
  if (rc) {
    printk(KERN_ERR "No memory address assigned to the region\n");
    return -1;
  }

  printk(KERN_INFO "Tart got reserved memory 0x%0x bytes at paddr: 0x%0lx\n",
      resource_size(&reserved), (long unsigned int) reserved.start);

  count = resource_size(&reserved) / PAGE_SIZE;
  holders = vmalloc(sizeof(struct dma_mem) * count);
  if (holders == NULL) {
    printk(KERN_ERR "tart failed to allocate dma_mem structures!\n");
    return -1;
  }

  printk(KERN_INFO "Tart created %u pages for managing reserved memory.\n", count);

  offset = 0;
  for (i = 0; i < count; i++) {
    m = &holders[i];

    m->offset = offset;

    m->next = free;
    free = m;

    offset += PAGE_SIZE;
  }

  return 0;
}

static int tart_remove(struct platform_device *pdev)
{
  printk(KERN_INFO "tart remove\n");

  vfree(holders);

  dma_release_channel(chan);
  memunmap(regs);

  return 0;
}


static const struct of_device_id tart_of_match[] = {
  { .compatible = "xlnx,tart-1.0", },
  { /* end of list */ },
};

MODULE_DEVICE_TABLE(of, tart_of_match);

static struct platform_driver tart_driver = {
  .probe = tart_of_probe,
  .remove = tart_remove,
  .driver = {
    .name = "tart",
    .of_match_table = tart_of_match,
  },
};

static int __init tart_init(void)
{
  int ret;

  printk(KERN_INFO "tart init\n");

  mutex_init(&tart_mutex);

  ret = register_chrdev(TART_MAJOR_NUM, DEVICE_NAME, &tart_fops);
  if (ret < 0) {
    printk(KERN_ERR "tart failed to register a major number\n");
    return ret;
  }

  class = class_create(THIS_MODULE, CLASS_NAME);
  if (IS_ERR(class)) {
    unregister_chrdev(TART_MAJOR_NUM, DEVICE_NAME);
    printk(KERN_ERR "tart failed to register device class\n");
    return PTR_ERR(class);
  }

  dev = device_create(class, NULL, MKDEV(TART_MAJOR_NUM, 0), NULL, DEVICE_NAME);
  if (IS_ERR(dev)) {
    class_destroy(class);
    unregister_chrdev(TART_MAJOR_NUM, DEVICE_NAME);
    printk(KERN_ERR "tart failed to create device\n");
    return PTR_ERR(dev);
  }

  printk(KERN_INFO "tart device created\n");

  return platform_driver_register(&tart_driver);
}

static void __exit tart_exit(void)
{
  printk(KERN_INFO "tart exit\n");

  platform_driver_unregister(&tart_driver);

  device_destroy(class, MKDEV(TART_MAJOR_NUM, 0));
  class_unregister(class);
  class_destroy(class);
  unregister_chrdev(TART_MAJOR_NUM, DEVICE_NAME);

  mutex_destroy(&tart_mutex);
}

module_init(tart_init);
module_exit(tart_exit);

MODULE_AUTHOR("Mytchel Hammond");
MODULE_DESCRIPTION("Test driver for custom zynq IP.");
MODULE_LICENSE("GPL");

