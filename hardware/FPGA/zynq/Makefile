all: boot/devicetree.dtb boot/zynq.bit

boot/devicetree.dtb: project/project.sdk/device_tree_bsp_0/*.dts
	dtc -I dts -O dtb -o $@ project/project.sdk/device_tree_bsp_0/system-top.dts

boot/zynq.bit: project/project.sdk/design_1_wrapper_hw_platform_0/design_1_wrapper.bit
	cp $< $@

