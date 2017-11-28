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

static int remove(struct platform_device *pdev)
{
	printk(KERN_INFO "removing tart.\n");
	return 0;
}

static int of_probe(struct platform_device *pdev)
{
	uint32_t *regs = (uint32_t *) pdev->resource->start;
	struct resource *res;
	int i, j;

	printk(KERN_INFO "tart of_probe.\n");

	res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
	regs = devm_ioremap_resource(&pdev->dev, res);

	printk(KERN_INFO "regs mapped to %p\n", regs);	

	if (IS_ERR(regs)) {
		printk(KERN_ERR "Failed to remap resource!\n");
		return -1;
	}

	/* Set led high. */
	*(regs + 0) = 0xffffffff;

	for (j = 0; j < 10; j++) {
		printk(KERN_INFO " ------ run %i -----  \n", j);
		for (i = 0; i < 6; i++) {
			uint32_t *a = regs + 1 + i;
			printk(KERN_INFO "antenna %d = %u\n", i, *a);
		}
	}

	/* Set led low. */	
	*(regs + 0) = 0;

	return 0;
}

static const struct of_device_id test_of_match[] = {
	{ .compatible = "xlnx,tart-1.0", },
	{ /* end of list */ },
};
MODULE_DEVICE_TABLE(of, test_of_match);

static struct platform_driver test_driver = {
	.probe = of_probe,
	.remove = remove,
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
MODULE_DESCRIPTION("Driver for the TART IP.");
MODULE_LICENSE("GPL");
