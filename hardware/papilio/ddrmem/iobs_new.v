module iobs_new (
	clock_i,
	reset_ni,
	
	// Control signals from the boss
	ctl_ce_i,	// Turns on/off CK/CK# pins
	ctl_cke_i,	// Physical enable to the IC
	
	ctl_cs_ni,
	ctl_ras_ni,
	ctl_cas_ni,
	ctl_we_ni,
	
	ctl_ba_i,
	ctl_a_i,
	
	ctl_dqs_i,
	ctl_dqs_o,
	ctl_dm_i,
	
	ctl_dq_i,
	ctl_dq_o,
	
	// Physical DDR pins/IOBs
	ddr_cke_o,
	ddr_ck_o,
	ddr_ck_no,
	
	ddr_cs_no,
	ddr_ras_no,
	ddr_cas_no,
	ddr_we_no,
	
	ddr_dqs_io,
	ddr_dm_o,
	
	ddr_dq_io
);

input	clock_i;
input	reset_ni;


input	ctl_ce_i;
input	ctl_cke_i;

input	ctl_cs_ni;
input	ctl_ras_ni;
input	ctl_cas_ni;
input	ctl_we_ni;

input	ctl_ba_i;
input	ctl_a_i;

input	ctl_dqs_i;
output	ctl_dqs_o;
input	ctl_dm_i;

input	ctl_dq_i;
output	ctl_dq_o;


output	ddr_cke_o;
output	ddr_ck_o;
output	ddr_ck_no;

output	ddr_cs_no;
output	ddr_ras_no;
output	ddr_cas_no;
output	ddr_we_no;

inout	ddr_dqs_io;
output	ddr_dm_o;

inout	ddr_dq_io;


// Generate the data masks (masks when high).
FDDRRSE dm_fddr0 (
	.C0	(clock_270i),
	.C1	(~clock_270i),
	.CE	(1'b1),
	.D0	(cntrl_dm_i [2]),
	.D1	(cntrl_dm_i [0]),
	.R	(1'b0),
	.S	(1'b0),
	.Q	(ddr_dm_w [0])
);

FDDRRSE dm_fddr1 (
	.C0	(clock_270i),
	.C1	(~clock_270i),
	.CE	(1'b1),
	.D0	(ctl_dm_i [3]),
	.D1	(ctl_dm_i [1]),
	.R	(1'b0),
	.S	(1'b0),
	.Q	(ddr_dm_w [1])
);



endmodule	// iobs_new
