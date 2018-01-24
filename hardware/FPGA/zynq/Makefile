all: boot/devicetree.dtb boot/devicetree.dts boot/zynq.bit

DTBROOT= project/project.sdk/device_tree_bsp_0/
boot/devicetree.dtb: $(DTBROOT)/*.dts $(DTBROOT)/*.dtsi
	dtc -I dts -O dtb -o $@ $(DTBROOT)/system-top.dts

boot/devicetree.dts: boot/devicetree.dtb
	dtc -I dtb -O dts -o $@ $<


boot/zynq.bit: project/project.runs/impl_1/design_1_wrapper.bit
	cp $< $@

clean:
	rm -f boot/zynq.bit boot/devicetree.*

