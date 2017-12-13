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
#include <linux/of_gpio.h>
#include <linux/interrupt.h>
#include <linux/io.h>
#include <linux/irq.h>
#include <linux/irqchip/chained_irq.h>
#include <linux/irqdomain.h>
#include <linux/gpio.h>
#include <linux/slab.h>
#include <linux/pm_runtime.h>

static int test_remove(struct platform_device *pdev)
{
	return 0;
}

	static void
dump_regs(uint32_t * regs)
{
	uint32_t v, dat, done, fail, busy;
	uint32_t dev, adr;

	printk("regs currently:\n");
	for (dev = 0; dev < 4; dev++) {
		if (dev == 2) continue;
		for (adr = 0; adr < 4; adr++) {
			uint32_t a = ((dev << 5) | (adr)) << 2;

			printk("dev = %i, adr = %i, a = 0x%x\n",
					dev, adr, a);

			v = *((uint32_t *) ((size_t) regs + a));
			dat = ((v&~(1<<31)) >> 2);
			done = (v >> 1) & 1;
			fail = v & 1;
			busy = v >> 31;

			printk("    raw = 0x%x,  data = 0x%x,  busy = %i, done = %i, fail = %i\n",
					v, dat, busy, done, fail);
		}
	}


}

static int test_of_probe(struct platform_device *pdev)
{
	uint32_t *regs = (uint32_t *) pdev->resource->start;
	struct resource *res;

	res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
	regs = devm_ioremap_resource(&pdev->dev, res);

	printk("regs at %p\n", regs);	
	if (IS_ERR(regs)) {
		printk("Failed to remap resource!\n");
		return -1;
	}

	dump_regs(regs);
		
	uint32_t dev = 0, adr = 3;	
	uint32_t a = ((dev << 5) | (adr)) << 2;
	*((uint32_t *) ((size_t) regs + a)) = 3;
	
	dump_regs(regs);

	return 0;
}

static const struct of_device_id test_of_match[] = {
	{ .compatible = "xlnx,tart-1.0", },
	{ /* end of list */ },
};
MODULE_DEVICE_TABLE(of, test_of_match);

static struct platform_driver test_driver = {
	.probe = test_of_probe,
	.remove = test_remove,
	.driver = {
		.name = "tart",
		.of_match_table = test_of_match,
	},
};

static int __init test_init(void)
{
	return platform_driver_register(&test_driver);
}

/* Make sure we get initialized before anyone else tries to use us */
subsys_initcall(test_init);

static void __exit test_exit(void)
{
	platform_driver_unregister(&test_driver);
}
module_exit(test_exit);

MODULE_AUTHOR("Mytchel Hammond");
MODULE_DESCRIPTION("Test driver for custom zynq IP.");
MODULE_LICENSE("GPL");
