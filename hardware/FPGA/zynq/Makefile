all: boot/devicetree.dtb boot/devicetree.dts boot/zynq.bit

boot/devicetree.dtb: project/project.sdk/device_tree_bsp_0/*.dts
	dtc -I dts -O dtb -o $@ project/project.sdk/device_tree_bsp_0/system-top.dts

boot/devicetree.dts: boot/devicetree.dtb
	dtc -I dtb -O dts -o $@ $<


boot/zynq.bit: project/project.runs/impl_1/design_1_wrapper.bit
	cp $< $@

