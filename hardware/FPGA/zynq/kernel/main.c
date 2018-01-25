/*
 * Driver for test ip module. Based of:
 *
 * Xilinx gpio driver for xps/axi_gpio IP.
 *
 * Copyright 2008 - 2013 Xilinx, Inc.
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
#include <linux/of_device.h>
#include <linux/of_irq.h>
#include <linux/of_platform.h>
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

static void test_dma(struct dma_chan *chan)
{
	const int len = 32 * sizeof(u32);
	dma_addr_t handle;
	dma_cookie_t cookie;
	struct completion cmp;
	u32 i;

	char *buf = kzalloc(len, GFP_KERNEL);
	if (!buf) {
		printk(KERN_ERR "allocating DMA memory failed!\n");
		return;
	}

	handle = dma_map_single(chan->device->dev, buf, len, DMA_FROM_DEVICE);

	cookie = axidma_prep_buffer(chan, handle, len, DMA_FROM_DEVICE, &cmp);

	if (dma_submit_error(cookie)) {
		printk(KERN_ERR "xdma_prep_buffer error\n");
		return;
	}

	printk(KERN_INFO "Starting DMA transfers\n");

	axidma_start_transfer(chan, &cmp, cookie);

	dma_unmap_single(chan->device->dev, handle, len, DMA_FROM_DEVICE);

	for (i = 0; i < len; i += sizeof(u32)) {
		printk(KERN_INFO "%4i = %08x\n", i, *((u32 *) &buf[i]));
	}

	kfree(buf);
}

static int tart_remove(struct platform_device *pdev)
{
	return 0;
}

	static void
dump_regs(u32 * regs)
{
	u32 dev, adr, v;

	printk(KERN_INFO "regs currently:\n");
	for (dev = 0; dev < 4; dev++) {
		for (adr = 0; adr < 4; adr++) {
			u32 a = ((dev << 5) | (adr)) << 2;

			printk(KERN_INFO "dev = %i, adr = %i, a = 0x%x\n",
					dev, adr, a);

			v = *((u32 *) ((size_t) regs + a));
			printk(KERN_INFO " = 0x%x / %i\n", v, v);
		}
	}
}

static int tart_of_probe(struct platform_device *pdev)
{
	u32 *regs = (u32 *) pdev->resource->start;
	struct resource *res;
	u32 dev, adr, a, v;
	struct dma_chan *chan;
	dma_cap_mask_t mask;

	res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
	regs = devm_ioremap_resource(&pdev->dev, res);

	if (IS_ERR(regs)) {
		printk(KERN_INFO "Failed to remap resource!\n");
		return -1;
	}

	printk("tart regs mapped at %p\n", regs);

	dma_cap_zero(mask);
	dma_cap_set(DMA_SLAVE, mask);
	dma_cap_set(DMA_PRIVATE, mask);

	/*chan = dma_request_channel(mask, NULL, NULL);*/
	chan = dma_request_slave_channel(&pdev->dev, "rawdma");

	if (chan == NULL || IS_ERR(chan)) {
		printk(KERN_ERR "failed to get channel\n");
		return -1;
	}

	printk(KERN_INFO "got dma channel %p %s\n", chan, dma_chan_name(chan));

	/*
	printk(KERN_INFO "reset\n");
	dev = 3, adr = 3;	
	a = ((dev << 5) | (adr)) << 2;
	v = 1;
	printk(KERN_INFO "write %i 0x%x -> dev %i, adr %i / 0x%x\n", v, v, dev, adr, a);
	*((u32 *) ((size_t) regs + a)) = v;
	*/
	
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

	dump_regs(regs);

	printk(KERN_INFO "test dma\n");

	test_dma(chan);

	printk(KERN_INFO "release dma chan\n");

	dma_release_channel(chan);

	printk(KERN_INFO "done\n");

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
	return platform_driver_register(&tart_driver);
}

/* Make sure we get initialized before anyone else tries to use us */
subsys_initcall(tart_init);

static void __exit tart_exit(void)
{
	platform_driver_unregister(&tart_driver);
}

module_exit(tart_exit);

MODULE_AUTHOR("Mytchel Hammond");
MODULE_DESCRIPTION("Test driver for custom zynq IP.");
MODULE_LICENSE("GPL");

