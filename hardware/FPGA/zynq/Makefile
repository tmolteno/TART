all: boot/tart.dtb boot/tart.dts boot/tart.bit

DTBROOT= project/project.sdk/device_tree_bsp_0/
boot/tart.dtb: $(DTBROOT)/*.dts $(DTBROOT)/*.dtsi
	dtc -I dts -O dtb -o $@ $(DTBROOT)/system-top.dts

boot/tart.dts: boot/tart.dtb
	dtc -I dtb -O dts -o $@ $<


boot/tart.bit: project/project.runs/impl_1/design_1_wrapper.bit
	cp $< $@

clean:
	rm -f boot/tart.*

