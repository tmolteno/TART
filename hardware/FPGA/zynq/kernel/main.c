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
#include <linux/kthread.h>
#include <linux/delay.h>

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
/* TODO: full will need a lock of some kind. */
static struct dma_mem *full    = NULL;

/* TODO: clean up task when the module is removed. 
   Also need some way to notify the task that it should
   stop rather than killing it. */

static struct task_struct *dma_task = NULL;

static void tart_mmap_open(struct vm_area_struct *vma)
{
  printk(KERN_INFO "tart mmap open\n");

  /* I am not sure what the purpose of this is. Or when it
     gets called other than by tart_mmap. */
}

static void tart_mmap_close(struct vm_area_struct *vma)
{
  struct dma_mem *h, *t;

  printk(KERN_INFO "tart mmap close\n");

  h = (struct dma_mem *) vma->vm_private_data;
  if (h == NULL) {
    printk(KERN_ERR "Tart mmap private data not valid!\n");
    return;
  }

  for (t = h; t->next != NULL; t = t->next)
    ;

  t->next = free;
  h = free;
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
    while (full == NULL) {
      if (dma_task != NULL) {
	schedule();
      } else {
	printk(KERN_ERR "TART has no full pages and dma is not running!\n");

	*h = full;
	full = head;

	return -1;
      }
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
  return ((uint32_t) i) << 2;
}

static void axidma_sync_callback(void *cmp)
{
  complete(cmp);
}

static int dma_thread(void *data)
{
  size_t len = 32*sizeof(u32);
  size_t count = PAGE_SIZE / len;

  unsigned long timeout = msecs_to_jiffies(2000);
  size_t sleep_time = 10;
  
  struct dma_async_tx_descriptor *rxd;
  struct dma_mem *m, **n;
  enum dma_status status;
  struct completion cmp;
  dma_cookie_t cookie;
  size_t buf, i;

  /* TODO: locks for getting and adding pages. */

  printk(KERN_INFO "TART starting DMA thread\n");

  while (!kthread_should_stop()) {
    sleep_time = 10;
    while (!kthread_should_stop() && free == NULL) {
      printk(KERN_ERR "TART out of free pages!\n");
      msleep_interruptible(sleep_time);
      if (sleep_time < 5000)
	sleep_time *= 2;
    }

    m = free;
    free = m->next;

    i = 0;
    while (i < count && !kthread_should_stop()) {
      buf = reserved.start + m->offset + i * len;

      rxd = dmaengine_prep_slave_single(chan, 
	  (dma_addr_t) buf, 
	  len, 
	  DMA_DEV_TO_MEM, 
	  DMA_CTRL_ACK | DMA_PREP_INTERRUPT);

      init_completion(&cmp);
      rxd->callback = axidma_sync_callback;
      rxd->callback_param = &cmp;

      cookie = rxd->tx_submit(rxd);

      if (dma_submit_error(cookie)) {
	printk(KERN_ERR "dma submit error error\n");
	return -1;
      }

      dma_async_issue_pending(chan);

      do {
	timeout = wait_for_completion_timeout(&cmp, timeout);
	status = dma_async_is_tx_complete(chan, cookie, NULL, NULL);
      } while (!kthread_should_stop() && status == DMA_IN_PROGRESS);

      if (status != DMA_COMPLETE) {
	printk(KERN_ERR "DMA returned completion status of:");
	switch (status) {
	  case DMA_IN_PROGRESS:
	    printk("in progress\n");
	    break;
	  case DMA_PAUSED:
	    printk("paused\n");
	    break;
	  case DMA_ERROR:
	    printk("error\n");
	    break;
	  default:
	    printk("unknown\n");
	    break;
	}

	break;
      
      } else {
	i++;
      }
    }

    for (n = &full; *n != NULL; n = &(*n)->next)
      ;

    *n = m;
    m->next = NULL;
  }

  printk(KERN_INFO "TART dma stopped\n");

  return 0;
}

static long tart_unlocked_ioctl(struct file *f, unsigned int cmd, unsigned long arg)
{
  struct tart_reg tr;
  u32 *r;

  printk(KERN_INFO "tart unlocked ioctl %u, %lu\n", cmd, arg);

  switch (cmd) {
    case TART_IOCTL_READ_REG:
      if (copy_from_user(&tr, (void *) arg, sizeof(tr))) {
	printk(KERN_ERR "copy_from_user failed for 0x%p\n", (void *) arg);
	return -EFAULT;
      }

      printk(KERN_INFO "tart read reg 0x%x\n", tr.reg);

      r = (uint32_t *) ((size_t) regs + tart_reg(tr.reg));
      tr.val = *r;

      printk(KERN_INFO "tart read reg 0x%x / %p = 0x%x\n", tr.reg, r, tr.val);

      if (copy_to_user((void *) arg, &tr, sizeof(tr))) {
	printk(KERN_ERR "copy_to_user failed for 0x%p\n", (void *) arg);
	return -EFAULT;
      }

      return 0;

    case TART_IOCTL_WRITE_REG:
      if (copy_from_user(&tr, (void *) arg, sizeof(tr))) {
	printk(KERN_ERR "copy_from_user failed for 0x%p\n", (void *) arg);
	return -EFAULT;
      }

      r = (uint32_t *) ((size_t) regs + tart_reg(tr.reg));
      printk(KERN_INFO "tart write reg 0x%x / %p = 0x%x\n", tr.reg, r, tr.val);
      *r = tr.val;

      printk(KERN_INFO "tart wrote reg 0x%x now = 0x%x\n", tr.reg, *r);

      return 0;

    default:
      printk(KERN_ERR "tart got invalid ioctl %i\n", cmd);
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

static int tart_of_probe(struct platform_device *pdev)
{
  size_t offset, count, i;
  struct device_node *np;
  struct resource *res;
  struct dma_mem *m;
  int rc;

  /* TODO: free things on error. */

  printk(KERN_INFO "probe tart dev\n");

  res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
  /* res should probably be checked somehow. */

  regs = devm_ioremap_resource(&pdev->dev, res);

  if (IS_ERR(regs)) {
    printk(KERN_ERR "Failed to remap resource!\n");
    return -1;
  }

  printk(KERN_INFO "got regs at 0x%p\n", regs);

  chan = dma_request_slave_channel(&pdev->dev, "rawdma");

  if (chan == NULL || IS_ERR(chan)) {
    printk(KERN_ERR "failed to get dma channel\n");
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

  mutex_init(&tart_mutex);

  dma_task = kthread_run(&dma_thread,
      NULL, "tart-dma");

  if (IS_ERR(dma_task)) {
    return PTR_ERR(dma_task);
  }

  return 0;
}

static int tart_remove(struct platform_device *pdev)
{
  printk(KERN_INFO "tart remove\n");

  kthread_stop(dma_task);

  dma_release_channel(chan);

  memunmap(regs);
  
  vfree(holders);

  mutex_destroy(&tart_mutex);

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
}

module_init(tart_init);
module_exit(tart_exit);

MODULE_AUTHOR("Mytchel Hammond");
MODULE_DESCRIPTION("Driver for TART zynq IP.");
MODULE_LICENSE("GPL");

