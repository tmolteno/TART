//
// Copyright (c) 1994 - 2003 Synplicity Inc
// $Header: /cvsroot/physics/physics/people/patrick/ddr_controller/spartan3/rtl/spartan3.v,v 1.2 2007-06-29 15:26:38 tim Exp $
//

module BSCAN_SPARTAN3(CAPTURE, DRCK1, DRCK2, RESET, SEL1, SEL2, SHIFT, TDI, UPDATE, TDO1, TDO2); // synthesis syn_black_box
input TDO1;
input TDO2;
output CAPTURE;
output DRCK1;
output DRCK2;
output RESET;
output SEL1;
output SEL2;
output SHIFT;
output TDI;
output UPDATE;
endmodule
module BUF(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module BUFCF(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module BUFG(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module BUFGCE(O, CE, I); // synthesis syn_black_box
output O;
input  CE;
input I;
endmodule
module BUFGCE_1(O, CE, I); // synthesis syn_black_box
output O;
input  CE;
input I;
endmodule
module BUFGDLL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module BUFGMUX(O, I0, I1, S); // synthesis syn_black_box
input  I0;
input  I1;
input  S;
output O;
endmodule
module BUFGMUX_1(O, I0, I1, S); // synthesis syn_black_box
input  I0;
input  I1;
input  S;
output O;
endmodule
module BUFGP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module CAPTURE_SPARTAN3(CAP, CLK); // synthesis syn_black_box .noprune = 1
input CAP;
input CLK;
endmodule
module CLKDLL (CLK0, CLK180, CLK270, CLK2X, CLK90, CLKDV, LOCKED, CLKFB, CLKIN, RST); // synthesis syn_black_box
parameter CLKDV_DIVIDE = 2.0;
parameter DUTY_CYCLE_CORRECTION = "TRUE";
parameter FACTORY_JF = 16'hC080;		// non-simulatable
parameter MAXPERCLKIN = 100000;
parameter STARTUP_WAIT = "FALSE";		// non-simulatable
input  CLKFB, CLKIN, RST;
output CLK0, CLK180, CLK270, CLK2X, CLK90, CLKDV, LOCKED;
endmodule
module CLKDLLE (CLK0, CLK180, CLK270, CLK2X, CLK2X180, CLK90, CLKDV, LOCKED, CLKFB, CLKIN, RST); // synthesis syn_black_box
parameter CLKDV_DIVIDE = 2.0;
parameter DUTY_CYCLE_CORRECTION = "TRUE";
parameter FACTORY_JF = 16'hC080;		// non-simulatable
parameter MAXPERCLKIN = 100000;
parameter STARTUP_WAIT = "FALSE";		// non-simulatable
input  CLKFB, CLKIN, RST;
output CLK0, CLK180, CLK270, CLK2X, CLK2X180, CLK90, CLKDV;
output LOCKED;
endmodule
module CLKDLLHF (CLK0, CLK180, CLKDV, LOCKED, CLKFB, CLKIN, RST); // synthesis syn_black_box
parameter CLKDV_DIVIDE = 2.0;
parameter DUTY_CYCLE_CORRECTION = "TRUE";
parameter FACTORY_JF = 16'hFFF0;		// non-simulatable
parameter MAXPERCLKIN = 100000;
parameter STARTUP_WAIT = "FALSE";		// non-simulatable
input  CLKFB, CLKIN, RST;
output CLK0, CLK180, CLKDV, LOCKED;
endmodule
module DCM (
	CLK0, CLK180, CLK270, CLK2X, CLK2X180, CLK90,
	CLKDV, CLKFX, CLKFX180, LOCKED, PSDONE, STATUS,
	CLKFB, CLKIN, DSSEN, PSCLK, PSEN, PSINCDEC, RST); // synthesis syn_black_box
parameter CLK_FEEDBACK = "1X";
parameter CLKDV_DIVIDE = 2.0;
parameter CLKFX_DIVIDE = 1;
parameter CLKFX_MULTIPLY = 4;
parameter CLKIN_DIVIDE_BY_2 = "FALSE";
parameter CLKIN_PERIOD = 0.0;			// non-simulatable
parameter CLKOUT_PHASE_SHIFT = "NONE";
parameter DESKEW_ADJUST = "SYSTEM_SYNCHRONOUS";	// non-simulatable
parameter DFS_FREQUENCY_MODE = "LOW";
parameter DLL_FREQUENCY_MODE = "LOW";
parameter DSS_MODE = "NONE";			// non-simulatable
parameter DUTY_CYCLE_CORRECTION = "TRUE";
parameter FACTORY_JF = 16'hC080;		// non-simulatable
parameter MAXPERCLKIN = 1000000;
parameter MAXPERPSCLK = 100000000;
parameter PHASE_SHIFT = 0;
parameter STARTUP_WAIT = "FALSE";		// non-simulatable
input CLKFB, CLKIN, DSSEN;
input PSCLK, PSEN, PSINCDEC, RST;
output CLK0, CLK180, CLK270, CLK2X, CLK2X180, CLK90;
output CLKDV, CLKFX, CLKFX180, LOCKED, PSDONE;
output [7:0] STATUS;
endmodule
module FD(Q, C, D); // synthesis syn_black_box
output Q;
input C;
input D;
endmodule
module FDC(Q, C, CLR, D); // synthesis syn_black_box
output Q;
input C;
input CLR;
input D;
endmodule
module FDCE(Q, C, CE, CLR, D); // synthesis syn_black_box
output Q;
input C;
input CE;
input CLR;
input D;
endmodule
module FDCE_1(Q, C, CE, CLR, D); // synthesis syn_black_box
output Q;
input C;
input CE;
input CLR;
input D;
endmodule
module FDCP(Q, C, CLR, D, PRE); // synthesis syn_black_box
output Q;
input C;
input CLR;
input D;
input PRE;
endmodule
module FDCPE(Q, C, CE, CLR, D, PRE); // synthesis syn_black_box
output Q;
input C;
input CE;
input CLR;
input D;
input PRE;
endmodule
module FDCPE_1(Q, C, CE, CLR, D, PRE); // synthesis syn_black_box
output Q;
input C;
input CE;
input CLR;
input D;
input PRE;
endmodule
module FDCP_1(Q, C, CLR, D, PRE); // synthesis syn_black_box
output Q;
input C;
input CLR;
input D;
input PRE;
endmodule
module FDC_1(Q, C, CLR, D); // synthesis syn_black_box
output Q;
input C;
input CLR;
input D;
endmodule
module FDDRCPE(Q, C0, C1, CE, CLR, D0, D1, PRE); // synthesis syn_black_box
input  C0;
input C1;
input CE;
input CLR;
input D0;
input D1;
input PRE;
output Q;
endmodule
module FDDRRSE(Q, C0, C1, CE, D0, D1, R, S); // synthesis syn_black_box
input  C0;
input  C1;
input  CE;
input  D0;
input  D1;
input  R;
input  S;
output Q;
endmodule
module FDE(Q, C, CE, D); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
endmodule
module FDE_1(Q, C, CE, D); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
endmodule
module FDP(Q, C, D, PRE); // synthesis syn_black_box
output Q;
input C;
input D;
input PRE;
endmodule
module FDPE(Q, C, CE, D, PRE); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input PRE;
endmodule
module FDPE_1(Q, C, CE, D, PRE); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input PRE;
endmodule
module FDP_1(Q, C, D, PRE); // synthesis syn_black_box
output Q;
input C;
input D;
input PRE;
endmodule
module FDR(Q, C, D, R); // synthesis syn_black_box
output Q;
input C;
input D;
input R;
endmodule
module FDRE(Q, C, CE, D, R); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input R;
endmodule
module FDRE_1(Q, C, CE, D, R); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input R;
endmodule
module FDRS(Q, C, D, R, S); // synthesis syn_black_box
output Q;
input C;
input D;
input R;
input S;
endmodule
module FDRSE(Q, C, CE, D, R, S); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input R;
input S;
endmodule
module FDRSE_1(Q, C, CE, D, R, S); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input R;
input S;
endmodule
module FDRS_1(Q, C, D, R, S); // synthesis syn_black_box
output Q;
input C;
input D;
input R;
input S;
endmodule
module FDR_1(Q, C, D, R); // synthesis syn_black_box
output Q;
input C;
input D;
input R;
endmodule
module FDS(Q, C, D, S); // synthesis syn_black_box
output Q;
input C;
input D;
input S;
endmodule
module FDSE(Q, C, CE, D, S); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input S;
endmodule
module FDSE_1(Q, C, CE, D, S); // synthesis syn_black_box
output Q;
input C;
input CE;
input D;
input S;
endmodule
module FDS_1(Q, C, D, S); // synthesis syn_black_box
output Q;
input C;
input D;
input S;
endmodule
module FD_1(Q, C, D); // synthesis syn_black_box
output Q;
input C;
input D;
endmodule
module GND(G); // synthesis syn_black_box .noprune = 1
output G;
endmodule
module IBUF(O, I); // synthesis syn_black_box
parameter IOSTANDARD="default";
output O;
input I;
endmodule
module IBUFDS(O, I, IB); // synthesis syn_black_box
parameter IOSTANDARD="default";
output O;
input I;
input IB;
endmodule
module IBUFDS_BLVDS_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_LDT_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_LVDSEXT_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_LVDSEXT_33(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_LVDS_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_LVDS_33(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_LVPECL_33(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFDS_ULVDS_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFG(O, I); // synthesis syn_black_box
parameter IOSTANDARD="default";
output O;
input I;
endmodule
module IBUFDS_DIFF_OUT (O, OB, I, IB); // synthesis syn_black_box
parameter IOSTANDARD = "LVDS_25";
output O, OB;
input  I, IB;
endmodule
module IBUFGDS_DIFF_OUT (O, OB, I, IB); // synthesis syn_black_box
parameter IOSTANDARD = "LVDS_25";
output O, OB;
input  I, IB;
endmodule
module IBUFGDS(O, I, IB); // synthesis syn_black_box
parameter IOSTANDARD="default";
output O;
input I;
input IB;
endmodule
module IBUFGDS_BLVDS_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_LDT_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_LVDSEXT_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_LVDSEXT_33(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_LVDS_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_LVDS_33(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_LVPECL_33(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFGDS_ULVDS_25(O, I, IB); // synthesis syn_black_box
output O;
input I;
input IB;
endmodule
module IBUFG_AGP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_GTL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_GTL_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_GTLP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_GTLP_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_HSTL_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_HSTL_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_HSTL_III(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_HSTL_III_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_HSTL_IV(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_HSTL_IV_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVCMOS15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVCMOS18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVCMOS2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVCMOS25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVCMOS33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_DV2_15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_DV2_18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_DV2_25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVDCI_DV2_33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_LVTTL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_PCI33_3(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_PCI66_3(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_PCIX(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL2_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL2_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL2_II(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL2_II_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL3_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL3_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL3_II(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUFG_SSTL3_II_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_AGP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_GTL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_GTL_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_GTLP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_GTLP_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_HSTL_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_HSTL_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_HSTL_III(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_HSTL_III_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_HSTL_IV(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_HSTL_IV_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVCMOS15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVCMOS18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVCMOS2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVCMOS25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVCMOS33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_DV2_15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_DV2_18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_DV2_25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVDCI_DV2_33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_LVTTL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_PCI33_3(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_PCI66_3(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_PCIX(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL2_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL2_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL2_II(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL2_II_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL3_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL3_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL3_II(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IBUF_SSTL3_II_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IFDDRCPE(Q0, Q1, C0, C1, CE, CLR, D, PRE); // synthesis syn_black_box
output Q0;
output Q1;
input C0;
input C1;
input CE;
input CLR;
input D;
input PRE;
endmodule
module IFDDRRSE(Q0, Q1, C0, C1, CE, D, R, S); // synthesis syn_black_box
output Q0;
output Q1;
input C0;
input C1;
input CE;
input D;
input R;
input S;
endmodule
module INV(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module IOBUF(O, IO, I, T); // synthesis syn_black_box
parameter IOSTANDARD="default";
parameter SLEW = "SLOW";
parameter DRIVE = 12;
output O;
inout IO;
input I;
input T;
endmodule
module IOBUFDS(O, IO, IOB, I, T); // synthesis syn_black_box
parameter IOSTANDARD="default";
parameter SLEW = "SLOW";
parameter DRIVE = 12;
output O;
inout IO;
inout IOB;
input I;
input T;
endmodule
module IOBUFDS_BLVDS_25(O, IO, IOB, I, T); // synthesis syn_black_box
output O;
inout IO;
inout IOB;
input I;
input T;
endmodule
module IOBUF_AGP(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_F_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_GTL(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_GTL_DCI(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_GTLP(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_GTLP_DCI(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_HSTL_I(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_HSTL_III(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_HSTL_IV(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_HSTL_IV_DCI(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_F_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_F_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_F_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_F_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_F_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_S_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_S_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_S_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_S_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS15_S_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_F_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_F_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_F_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_F_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_F_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_F_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_S_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_S_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_S_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_S_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_S_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS18_S_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_F_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS25_S_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_F_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVCMOS33_S_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_15(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_18(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_25(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_33(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_DV2_15(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_DV2_18(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_DV2_25(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVDCI_DV2_33(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_F_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_LVTTL_S_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_PCI33_3(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_PCI66_3(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_PCIX(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_SSTL2_I(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_SSTL2_II(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_SSTL2_II_DCI(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_SSTL3_I(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_SSTL3_II(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_SSTL3_II_DCI(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_12(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_16(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_2(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_24(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_4(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_6(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module IOBUF_S_8(O, IO, I, T); // synthesis syn_black_box
output O;
inout IO;
input I;
input T;
endmodule
module KEEPER(O); // synthesis syn_black_box .noprune = 1
inout O;
endmodule
module LD(Q, D, G); // synthesis syn_black_box
output Q;
input D;
input G;
endmodule
module LDC(Q, CLR, D, G); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
endmodule
module LDCE(Q, CLR, D, G, GE); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
input GE;
endmodule
module LDCE_1(Q, CLR, D, G, GE); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
input GE;
endmodule
module LDCP(Q, CLR, D, G, PRE); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
input PRE;
endmodule
module LDCPE(Q, CLR, D, G, GE, PRE); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
input GE;
input PRE;
endmodule
module LDCPE_1(Q, CLR, D, G, GE, PRE); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
input GE;
input PRE;
endmodule
module LDCP_1(Q, CLR, D, G, PRE); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
input PRE;
endmodule
module LDC_1(Q, CLR, D, G); // synthesis syn_black_box
output Q;
input CLR;
input D;
input G;
endmodule
module LDE(Q, D, G, GE); // synthesis syn_black_box
output Q;
input D;
input G;
input GE;
endmodule
module LDE_1(Q, D, G, GE); // synthesis syn_black_box
output Q;
input D;
input G;
input GE;
endmodule
module LDP(Q, D, G, PRE); // synthesis syn_black_box
output Q;
input D;
input G;
input PRE;
endmodule
module LDPE(Q, D, G, GE, PRE); // synthesis syn_black_box
output Q;
input D;
input G;
input GE;
input PRE;
endmodule
module LDPE_1(Q, D, G, GE, PRE); // synthesis syn_black_box
output Q;
input D;
input G;
input GE;
input PRE;
endmodule
module LDP_1(Q, D, G, PRE); // synthesis syn_black_box
output Q;
input D;
input G;
input PRE;
endmodule
module LD_1(Q, D, G); // synthesis syn_black_box
output Q;
input D;
input G;
endmodule
module LUT1(O, I0); // synthesis xc_map=lut syn_black_box
output O;
input I0;
parameter INIT = 2'b0;
assign O = I0 ? INIT[1] : INIT[0];
endmodule
module LUT1_D(LO, O, I0); // synthesis xc_map=lut syn_black_box
output O;
output LO;
input I0;
parameter INIT = 2'b0;
LUT1 d(O, I0);
defparam d.INIT = INIT;
assign LO=O;
endmodule
module LUT1_L(LO, I0); // synthesis xc_map=lut syn_black_box
output LO;
input I0;
parameter INIT = 2'b0;
assign LO = I0 ? INIT[1] : INIT[0];
endmodule
module LUT2(O, I0, I1); // synthesis xc_map=lut syn_black_box
output O;
input I0;
input I1;
parameter INIT = 4'b0;
assign O = I1 ? (I0 ? INIT[3] : INIT[2]) : (I0 ? INIT[1] : INIT[0]);
endmodule
module LUT2_D(LO, O, I0, I1); // synthesis xc_map=lut syn_black_box
output O;
output LO;
input I0;
input I1;
parameter INIT = 4'b0;
LUT2 d(O, I0, I1);
defparam d.INIT = INIT;
assign LO=O;
endmodule
module LUT2_L(LO, I0, I1); // synthesis xc_map=lut syn_black_box
output LO;
input I0;
input I1;
parameter INIT = 4'b0;
assign LO = I1 ? (I0 ? INIT[3] : INIT[2]) : (I0 ? INIT[1] : INIT[0]);
endmodule
module LUT3(O, I0, I1, I2); // synthesis xc_map=lut syn_black_box
output O;
input I0;
input I1;
input I2;
parameter INIT = 8'b0;
assign O = I2 ?
     (I1 ? (I0 ? INIT[7] : INIT[6]) : (I0 ? INIT[5] : INIT[4])) : 
     (I1 ? (I0 ? INIT[3] : INIT[2]) : (I0 ? INIT[1] : INIT[0]));
endmodule
module LUT3_D(LO, O, I0, I1, I2); // synthesis xc_map=lut syn_black_box
output O;
output LO;
input I0;
input I1;
input I2;
parameter INIT = 8'b0;
LUT3 d(O, I0, I1, I2);
defparam d.INIT = INIT;
assign LO=O;
endmodule
module LUT3_L(LO, I0, I1, I2); // synthesis xc_map=lut syn_black_box
output LO;
input I0;
input I1;
input I2;
parameter INIT = 8'b0;
assign LO = I2 ?
     (I1 ? (I0 ? INIT[7] : INIT[6]) : (I0 ? INIT[5] : INIT[4])) : 
     (I1 ? (I0 ? INIT[3] : INIT[2]) : (I0 ? INIT[1] : INIT[0]));
endmodule
module LUT4(O, I0, I1, I2, I3); // synthesis xc_map=lut syn_black_box
output O;
input I0;
input I1;
input I2;
input I3;
parameter INIT = 16'b0;
assign O = I3 ?
    (I2 ?
     (I1 ? (I0 ? INIT[15] : INIT[14]) : (I0 ? INIT[13] : INIT[12])) : 
     (I1 ? (I0 ? INIT[11] : INIT[10]) : (I0 ? INIT[9] : INIT[8]))) :
    (I2 ?
     (I1 ? (I0 ? INIT[7] : INIT[6]) : (I0 ? INIT[5] : INIT[4])) : 
     (I1 ? (I0 ? INIT[3] : INIT[2]) : (I0 ? INIT[1] : INIT[0])));
endmodule
module LUT4_D(LO, O, I0, I1, I2, I3); // synthesis xc_map=lut syn_black_box
output O;
output LO;
input I0;
input I1;
input I2;
input I3;
parameter INIT = 16'b0;
LUT4 d(O, I0, I1, I2, I3);
defparam d.INIT = INIT;
assign LO=O;
endmodule
module LUT4_L(LO, I0, I1, I2, I3); // synthesis xc_map=lut syn_black_box
output LO;
input I0;
input I1;
input I2;
input I3;
parameter INIT = 16'b0;
assign LO = I3 ?
    (I2 ?
     (I1 ? (I0 ? INIT[15] : INIT[14]) : (I0 ? INIT[13] : INIT[12])) : 
     (I1 ? (I0 ? INIT[11] : INIT[10]) : (I0 ? INIT[9] : INIT[8]))) :
    (I2 ?
     (I1 ? (I0 ? INIT[7] : INIT[6]) : (I0 ? INIT[5] : INIT[4])) : 
     (I1 ? (I0 ? INIT[3] : INIT[2]) : (I0 ? INIT[1] : INIT[0])));
endmodule
module MULT18X18(P, A, B); // synthesis syn_black_box
input  [17:0] A;
input  [17:0] B;
output [35:0] P;
endmodule
module MULT18X18S(P, A, B, C, CE, R);
output [35:0] P;
input  [17:0] A;
input  [17:0] B;
input  C;
input  CE;
input  R;
endmodule
module MULT_AND(LO, I0, I1); // synthesis syn_black_box
output LO;
input I0;
input I1;
endmodule
module MUXCY(O, CI, DI, S); // synthesis syn_black_box
output O;
input CI;
input DI;
input S;
endmodule
module MUXCY_D(LO, O, CI, DI, S); // synthesis syn_black_box
output O;
output LO;
input CI;
input DI;
input S;
endmodule
module MUXCY_L(LO, CI, DI, S); // synthesis syn_black_box
output LO;
input CI;
input DI;
input S;
endmodule
module MUXF5(O, I0, I1, S); // synthesis syn_black_box
output O;
input I0;
input I1;
input S;
endmodule
module MUXF5_D(LO, O, I0, I1, S); // synthesis syn_black_box
output O;
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF5_L(LO, I0, I1, S); // synthesis syn_black_box
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF6(O, I0, I1, S); // synthesis syn_black_box
output O;
input I0;
input I1;
input S;
endmodule
module MUXF6_D(LO, O, I0, I1, S); // synthesis syn_black_box
output O;
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF6_L(LO, I0, I1, S); // synthesis syn_black_box
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF7(O, I0, I1, S); // synthesis syn_black_box
output O;
input I0;
input I1;
input S;
endmodule
module MUXF7_D(LO, O, I0, I1, S); // synthesis syn_black_box
output O;
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF7_L(LO, I0, I1, S); // synthesis syn_black_box
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF8(O, I0, I1, S); // synthesis syn_black_box
output O;
input I0;
input I1;
input S;
endmodule
module MUXF8_D(LO, O, I0, I1, S); // synthesis syn_black_box
output O;
output LO;
input I0;
input I1;
input S;
endmodule
module MUXF8_L(LO, I0, I1, S); // synthesis syn_black_box
output LO;
input I0;
input I1;
input S;
endmodule
module OBUF(O, I); // synthesis syn_black_box
parameter IOSTANDARD="default";
parameter SLEW = "SLOW";
parameter DRIVE = 12;
output O;
input I;
endmodule
module OBUFDS(O, OB, I); // synthesis syn_black_box
parameter IOSTANDARD="default";
parameter SLEW = "SLOW";
parameter DRIVE = 12;
output O;
output OB;
input I;
endmodule
module OBUFDS_BLVDS_25(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_LDT_25(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_LVDSEXT_25(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_LVDSEXT_33(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_LVDS_25(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_LVDS_33(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_ULVDS_25(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFDS_LVPECL_33(O, OB, I); // synthesis syn_black_box
output O;
output OB;
input I;
endmodule
module OBUFT(O, I, T); // synthesis syn_black_box
parameter IOSTANDARD="default";
parameter SLEW = "SLOW";
parameter DRIVE = 12;
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS(O, OB, I, T); // synthesis syn_black_box
parameter IOSTANDARD="default";
parameter SLEW = "SLOW";
parameter DRIVE = 12;
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_BLVDS_25(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_LDT_25(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_LVDSEXT_25(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_LVDSEXT_33(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_LVDS_25(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_LVDS_33(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_ULVDS_25(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFTDS_LVPECL_33(O, OB, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
output OB /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_AGP(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_F_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_GTL(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_GTL_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_GTLP(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_GTLP_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_HSTL_I(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_HSTL_I_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_HSTL_III(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_HSTL_III_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_HSTL_IV(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_HSTL_IV_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_F_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_F_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_F_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_F_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_F_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_S_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_S_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_S_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_S_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS15_S_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_F_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_F_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_F_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_F_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_F_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_F_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_S_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_S_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_S_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_S_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_S_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS18_S_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_F_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS25_S_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_F_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVCMOS33_S_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_15(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_18(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_25(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_33(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_DV2_15(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_DV2_18(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_DV2_25(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVDCI_DV2_33(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_F_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_LVTTL_S_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_PCI33_3(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_PCI66_3(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_PCIX(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL2_I(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL2_I_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL2_II(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL2_II_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL3_I(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL3_I_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL3_II(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_SSTL3_II_DCI(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_12(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_16(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_2(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_24(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_4(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_6(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUFT_S_8(O, I, T); // synthesis syn_black_box
output O /* synthesis syn_tristate = 1 */;
input I;
input T;
endmodule
module OBUF_AGP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_F_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_GTL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_GTL_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_GTLP(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_GTLP_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_HSTL_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_HSTL_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_HSTL_III(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_HSTL_III_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_HSTL_IV(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_HSTL_IV_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_F_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_F_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_F_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_F_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_F_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_S_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_S_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_S_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_S_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS15_S_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_F_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_F_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_F_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_F_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_F_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_F_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_S_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_S_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_S_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_S_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_S_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS18_S_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_F_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS25_S_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_F_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVCMOS33_S_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_DV2_15(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_DV2_18(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_DV2_25(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVDCI_DV2_33(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_F_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_LVTTL_S_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_PCI33_3(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_PCI66_3(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_PCIX(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL2_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL2_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL2_II(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL2_II_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL3_I(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL3_I_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL3_II(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_SSTL3_II_DCI(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_12(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_16(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_2(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_24(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_4(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_6(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OBUF_S_8(O, I); // synthesis syn_black_box
output O;
input I;
endmodule
module OFDDRCPE(Q, C0, C1, CE, CLR, D0, D1, PRE); // synthesis syn_black_box
output Q;
input C0;
input C1;
input CE;
input CLR;
input D0;
input D1;
input PRE;
endmodule
module OFDDRRSE(Q, C0, C1, CE, D0, D1, R, S); // synthesis syn_black_box
output Q;
input C0;
input C1;
input CE;
input D0;
input D1;
input R;
input S;
endmodule
module OFDDRTCPE(O, C0, C1, CE, CLR, D0, D1, PRE, T); // synthesis syn_black_box
output O;
input C0;
input C1;
input CE;
input CLR;
input D0;
input D1;
input PRE;
input T;
endmodule
module OFDDRTRSE(O, C0, C1, CE, D0, D1, R, S, T); // synthesis syn_black_box
output O;
input C0;
input C1;
input CE;
input D0;
input D1;
input R;
input S;
input T;
endmodule
module PULLDOWN(O); // synthesis syn_black_box .noprune = 1
output O /* synthesis syn_not_a_driver = 1 */;
endmodule
module PULLUP(O); // synthesis syn_black_box .noprune = 1
output O /* synthesis syn_not_a_driver = 1 */;
endmodule
module RAM16X1D(DPO, SPO, A0, A1, A2, A3, D, DPRA0, DPRA1, DPRA2, DPRA3, WCLK, WE); // synthesis syn_black_box
parameter INIT = 16'h0000;
output DPO;
output SPO;
input A0;
input A1;
input A2;
input A3;
input D;
input DPRA0;
input DPRA1;
input DPRA2;
input DPRA3;
input WCLK;
input WE;
endmodule
module RAM16X1D_1(DPO, SPO, A0, A1, A2, A3, D, DPRA0, DPRA1, DPRA2, DPRA3, WCLK, WE); // synthesis syn_black_box
parameter INIT = 16'h0000;
output DPO;
output SPO;
input A0;
input A1;
input A2;
input A3;
input D;
input DPRA0;
input DPRA1;
input DPRA2;
input DPRA3;
input WCLK;
input WE;
endmodule
module RAM16X1S(O, A0, A1, A2, A3, D, WCLK, WE); // synthesis syn_black_box
parameter INIT = 16'h0000;
output O;
input A0;
input A1;
input A2;
input A3;
input D;
input WCLK;
input WE;
endmodule
module RAM16X1S_1(O, A0, A1, A2, A3, D, WCLK, WE); // synthesis syn_black_box
parameter INIT = 16'h0000;
output O;
input A0;
input A1;
input A2;
input A3;
input D;
input WCLK;
input WE;
endmodule
module RAM16X2S(O0, O1, A0, A1, A2, A3, D0, D1, WCLK, WE); // synthesis syn_black_box
parameter INIT_00 = 16'h0000;
parameter INIT_01 = 16'h0000;
output O0;
output O1;
input  A0;
input  A1;
input  A2;
input  A3;
input  D0;
input  D1;
input  WCLK;
input  WE;
endmodule
module RAM16X4S(O0, O1, O2, O3, A0, A1, A2, A3, D0, D1, D2, D3, WCLK, WE); //synthesis syn_black_box
parameter INIT_00 = 16'h0000;
parameter INIT_01 = 16'h0000;
parameter INIT_02 = 16'h0000;
parameter INIT_03 = 16'h0000;
output O0;
output O1;
output O2;
output O3;
input  A0;
input  A1;
input  A2;
input  A3;
input  D0;
input  D1;
input  D2;
input  D3;
input  WCLK;
input  WE;
endmodule
module RAM32X1S(O, A0, A1, A2, A3, A4, D, WCLK, WE); // synthesis syn_black_box
parameter INIT = 32'h00000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input D;
input WCLK;
input WE;
endmodule
module RAM32X1S_1(O, A0, A1, A2, A3, A4, D, WCLK, WE); // synthesis syn_black_box
parameter INIT = 32'h00000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input D;
input WCLK;
input WE;
endmodule
module RAM32X2S(O0, O1, A0, A1, A2, A3, A4, D0, D1, WCLK, WE); // synthesis syn_black_box
parameter INIT_00 = 32'h00000000;
parameter INIT_01 = 32'h00000000;
output O0;
output O1;
input  A0;
input  A1;
input  A2;
input  A3;
input  A4;
input  D0;
input  D1;
input  WCLK;
input  WE;
endmodule
module RAM64X1S(O, A0, A1, A2, A3, A4, A5, D, WCLK, WE); // synthesis syn_black_box
parameter INIT = 64'h0000000000000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input A5;
input D;
input WCLK;
input WE;
endmodule
module RAM64X1S_1(O, A0, A1, A2, A3, A4, A5, D, WCLK, WE); // synthesis syn_black_box
parameter INIT = 64'h0000000000000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input A5;
input D;
input WCLK;
input WE;
endmodule
module RAMB16_S1 (DO, ADDR, CLK, DI, EN, SSR, WE) /* synthesis syn_black_box */ ;
    parameter INIT = 1'h0;
    parameter SRVAL = 1'h0;
    parameter WRITE_MODE = "WRITE_FIRST";

    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DO;
    input [13:0] ADDR;
    input [0:0] DI;
    input EN, CLK, WE, SSR;
endmodule
module RAMB16_S1_S1 (DOA, DOB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 1'h0;
    parameter INIT_B = 1'h0;
    parameter SRVAL_A = 1'h0;
    parameter SRVAL_B = 1'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DOA;
    input [13:0] ADDRA;
    input [0:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [0:0] DOB;
    input [13:0] ADDRB;
    input [0:0] DIB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S1_S18 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 1'h0;
    parameter INIT_B = 18'h0;
    parameter SRVAL_A = 1'h0;
    parameter SRVAL_B = 18'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DOA;
    input [13:0] ADDRA;
    input [0:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [15:0] DOB;
    output [1:0] DOPB;
    input [9:0] ADDRB;
    input [15:0] DIB;
    input [1:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S1_S2 (DOA, DOB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 1'h0;
    parameter INIT_B = 2'h0;
    parameter SRVAL_A = 1'h0;
    parameter SRVAL_B = 2'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DOA;
    input [13:0] ADDRA;
    input [0:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [1:0] DOB;
    input [12:0] ADDRB;
    input [1:0] DIB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S1_S36 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 1'h0;
    parameter INIT_B = 36'h0;
    parameter SRVAL_A = 1'h0;
    parameter SRVAL_B = 36'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DOA;
    input [13:0] ADDRA;
    input [0:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [31:0] DOB;
    output [3:0] DOPB;
    input [8:0] ADDRB;
    input [31:0] DIB;
    input [3:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S1_S4 (DOA, DOB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 1'h0;
    parameter INIT_B = 4'h0;
    parameter SRVAL_A = 1'h0;
    parameter SRVAL_B = 4'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DOA;
    input [13:0] ADDRA;
    input [0:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [3:0] DOB;
    input [11:0] ADDRB;
    input [3:0] DIB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S1_S9 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 1'h0;
    parameter INIT_B = 9'h0;
    parameter SRVAL_A = 1'h0;
    parameter SRVAL_B = 9'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [0:0] DOA;
    input [13:0] ADDRA;
    input [0:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [7:0] DOB;
    output [0:0] DOPB;
    input [10:0] ADDRB;
    input [7:0] DIB;
    input [0:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule
module RAMB16_S18 (DO, DOP, ADDR, CLK, DI, DIP, EN, SSR, WE) /* synthesis syn_black_box */ ;
    parameter INIT = 18'h0;
    parameter SRVAL = 18'h0;
    parameter WRITE_MODE = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [15:0] DO;
    output [1:0] DOP;
    input [9:0] ADDR;
    input [15:0] DI;
    input [1:0] DIP;
    input EN, CLK, WE, SSR;
endmodule

module RAMB16_S18_S18 (DOA, DOB, DOPA, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPA, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 18'h0;
    parameter INIT_B = 18'h0;
    parameter SRVAL_A = 18'h0;
    parameter SRVAL_B = 18'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [15:0] DOA;
    output [1:0] DOPA;
    input [9:0] ADDRA;
    input [15:0] DIA;
    input [1:0] DIPA;
    input ENA, CLKA, WEA, SSRA;
    output [15:0] DOB;
    output [1:0] DOPB;
    input [9:0] ADDRB;
    input [15:0] DIB;
    input [1:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S18_S36 (DOA, DOB, DOPA, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPA, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;
    parameter INIT_A = 18'h0;
    parameter INIT_B = 36'h0;
    parameter SRVAL_A = 18'h0;
    parameter SRVAL_B = 36'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    output [15:0] DOA;
    output [1:0] DOPA;
    input [9:0] ADDRA;
    input [15:0] DIA;
    input [1:0] DIPA;
    input ENA, CLKA, WEA, SSRA;
    output [31:0] DOB;
    output [3:0] DOPB;
    input [8:0] ADDRB;
    input [31:0] DIB;
    input [3:0] DIPB;
    input ENB, CLKB, WEB, SSRB;

endmodule
module RAMB16_S2 (DO, ADDR, CLK, DI, EN, SSR, WE) /* synthesis syn_black_box */ ;
    parameter INIT = 2'h0;
    parameter SRVAL = 2'h0;
    parameter WRITE_MODE = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    output [1:0] DO;
    input [12:0] ADDR;
    input [1:0] DI;
    input EN, CLK, WE, SSR;
endmodule
module RAMB16_S2_S18 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;
    parameter INIT_A = 2'h0;
    parameter INIT_B = 18'h0;
    parameter SRVAL_A = 2'h0;
    parameter SRVAL_B = 18'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    output [1:0] DOA;
    input [12:0] ADDRA;
    input [1:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [15:0] DOB;
    output [1:0] DOPB;
    input [9:0] ADDRB;
    input [15:0] DIB;
    input [1:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S2_S2 (DOA, DOB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 2'h0;
    parameter INIT_B = 2'h0;
    parameter SRVAL_A = 2'h0;
    parameter SRVAL_B = 2'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [1:0] DOA;
    input [12:0] ADDRA;
    input [1:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [1:0] DOB;
    input [12:0] ADDRB;
    input [1:0] DIB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S2_S36 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 2'h0;
    parameter INIT_B = 36'h0;
    parameter SRVAL_A = 2'h0;
    parameter SRVAL_B = 36'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [1:0] DOA;
    input [12:0] ADDRA;
    input [1:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [31:0] DOB;
    output [3:0] DOPB;
    input [8:0] ADDRB;
    input [31:0] DIB;
    input [3:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S2_S4 (DOA, DOB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 2'h0;
    parameter INIT_B = 4'h0;
    parameter SRVAL_A = 2'h0;
    parameter SRVAL_B = 4'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [1:0] DOA;
    input [12:0] ADDRA;
    input [1:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [3:0] DOB;
    input [11:0] ADDRB;
    input [3:0] DIB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S2_S9 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 2'h0;
    parameter INIT_B = 9'h0;
    parameter SRVAL_A = 2'h0;
    parameter SRVAL_B = 9'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [1:0] DOA;
    input [12:0] ADDRA;
    input [1:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [7:0] DOB;
    output [0:0] DOPB;
    input [10:0] ADDRB;
    input [7:0] DIB;
    input [0:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule
module RAMB16_S36 (DO, DOP, ADDR, CLK, DI, DIP, EN, SSR, WE) /* synthesis syn_black_box */ ;

    parameter INIT = 36'h0;
    parameter SRVAL = 36'h0;
    parameter WRITE_MODE = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [31:0] DO;
    output [3:0] DOP;
    input [8:0] ADDR;
    input [31:0] DI;
    input [3:0] DIP;
    input EN, CLK, WE, SSR;
endmodule
module RAMB16_S36_S36 (DOA, DOB, DOPA, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPA, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 36'h0;
    parameter INIT_B = 36'h0;
    parameter SRVAL_A = 36'h0;
    parameter SRVAL_B = 36'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [31:0] DOA;
    output [3:0] DOPA;
    input [8:0] ADDRA;
    input [31:0] DIA;
    input [3:0] DIPA;
    input ENA, CLKA, WEA, SSRA;
    output [31:0] DOB;
    output [3:0] DOPB;
    input [8:0] ADDRB;
    input [31:0] DIB;
    input [3:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule
module RAMB16_S4 (DO, ADDR, CLK, DI, EN, SSR, WE) /* synthesis syn_black_box */ ;

    parameter INIT = 4'h0;
    parameter SRVAL = 4'h0;
    parameter WRITE_MODE = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [3:0] DO;
    input [11:0] ADDR;
    input [3:0] DI;
    input EN, CLK, WE, SSR;
endmodule
module RAMB16_S4_S18 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 4'h0;
    parameter INIT_B = 18'h0;
    parameter SRVAL_A = 4'h0;
    parameter SRVAL_B = 18'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [3:0] DOA;
    input [11:0] ADDRA;
    input [3:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [15:0] DOB;
    output [1:0] DOPB;
    input [9:0] ADDRB;
    input [15:0] DIB;
    input [1:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S4_S36 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 4'h0;
    parameter INIT_B = 36'h0;
    parameter SRVAL_A = 4'h0;
    parameter SRVAL_B = 36'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [3:0] DOA;
    input [11:0] ADDRA;
    input [3:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [31:0] DOB;
    output [3:0] DOPB;
    input [8:0] ADDRB;
    input [31:0] DIB;
    input [3:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S4_S4 (DOA, DOB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 4'h0;
    parameter INIT_B = 4'h0;
    parameter SRVAL_A = 4'h0;
    parameter SRVAL_B = 4'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [3:0] DOA;
    input [11:0] ADDRA;
    input [3:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [3:0] DOB;
    input [11:0] ADDRB;
    input [3:0] DIB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S4_S9 (DOA, DOB, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 4'h0;
    parameter INIT_B = 9'h0;
    parameter SRVAL_A = 4'h0;
    parameter SRVAL_B = 9'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [3:0] DOA;
    input [11:0] ADDRA;
    input [3:0] DIA;
    input ENA, CLKA, WEA, SSRA;
    output [7:0] DOB;
    output [0:0] DOPB;
    input [10:0] ADDRB;
    input [7:0] DIB;
    input [0:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule
module RAMB16_S9 (DO, DOP, ADDR, CLK, DI, DIP, EN, SSR, WE) /* synthesis syn_black_box */ ;

    parameter INIT = 9'h0;
    parameter SRVAL = 9'h0;
    parameter WRITE_MODE = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [7:0] DO;
    output [0:0] DOP;
    input [10:0] ADDR;
    input [7:0] DI;
    input [0:0] DIP;
    input EN, CLK, WE, SSR;
endmodule
module RAMB16_S9_S18 (DOA, DOB, DOPA, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPA, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 9'h0;
    parameter INIT_B = 18'h0;
    parameter SRVAL_A = 9'h0;
    parameter SRVAL_B = 18'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [7:0] DOA;
    output [0:0] DOPA;
    input [10:0] ADDRA;
    input [7:0] DIA;
    input [0:0] DIPA;
    input ENA, CLKA, WEA, SSRA;
    output [15:0] DOB;
    output [1:0] DOPB;
    input [9:0] ADDRB;
    input [15:0] DIB;
    input [1:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S9_S36 (DOA, DOB, DOPA, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPA, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 9'h0;
    parameter INIT_B = 36'h0;
    parameter SRVAL_A = 9'h0;
    parameter SRVAL_B = 36'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [7:0] DOA;
    output [0:0] DOPA;
    input [10:0] ADDRA;
    input [7:0] DIA;
    input [0:0] DIPA;
    input ENA, CLKA, WEA, SSRA;
    output [31:0] DOB;
    output [3:0] DOPB;
    input [8:0] ADDRB;
    input [31:0] DIB;
    input [3:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module RAMB16_S9_S9 (DOA, DOB, DOPA, DOPB, ADDRA, ADDRB, CLKA, CLKB, DIA, DIB, DIPA, DIPB, ENA, ENB, SSRA, SSRB, WEA, WEB) /* synthesis syn_black_box */ ;

    parameter INIT_A = 9'h0;
    parameter INIT_B = 9'h0;
    parameter SRVAL_A = 9'h0;
    parameter SRVAL_B = 9'h0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter INIT_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INIT_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
    parameter INITP_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;

    output [7:0] DOA;
    output [0:0] DOPA;
    input [10:0] ADDRA;
    input [7:0] DIA;
    input [0:0] DIPA;
    input ENA, CLKA, WEA, SSRA;
    output [7:0] DOB;
    output [0:0] DOPB;
    input [10:0] ADDRB;
    input [7:0] DIB;
    input [0:0] DIPB;
    input ENB, CLKB, WEB, SSRB;
endmodule

module ROM16X1(O, A0, A1, A2, A3); // synthesis syn_black_box
parameter INIT = 16'h0000;
output O;
input A0;
input A1;
input A2;
input A3;
endmodule
module ROM32X1(O, A0, A1, A2, A3, A4); // synthesis syn_black_box
parameter INIT = 32'h00000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
endmodule
module ROM64X1(O, A0, A1, A2, A3, A4, A5); // synthesis syn_black_box
parameter INIT = 64'h0000000000000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input A5;
endmodule
module ROM128X1(O, A0, A1, A2, A3, A4, A5, A6); // synthesis syn_black_box
parameter INIT = 128'h00000000000000000000000000000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input A5;
input A6;
endmodule
module ROM256X1(O, A0, A1, A2, A3, A4, A5, A6, A7); // synthesis syn_black_box
parameter INIT = 256'h0000000000000000000000000000000000000000000000000000000000000000;
output O;
input A0;
input A1;
input A2;
input A3;
input A4;
input A5;
input A6;
input A7;
endmodule
module SRL16(Q, A0, A1, A2, A3, CLK, D); // synthesis syn_black_box
output Q;
input A0;
input A1;
input A2;
input A3;
input CLK;
input D;
endmodule
module SRL16E(Q, A0, A1, A2, A3, CE, CLK, D); // synthesis syn_black_box
output Q;
input A0;
input A1;
input A2;
input A3;
input CE;
input CLK;
input D;
endmodule
module SRL16E_1(Q, A0, A1, A2, A3, CE, CLK, D); // synthesis syn_black_box
output Q;
input A0;
input A1;
input A2;
input A3;
input CE;
input CLK;
input D;
endmodule
module SRL16_1(Q, A0, A1, A2, A3, CLK, D); // synthesis syn_black_box
output Q;
input A0;
input A1;
input A2;
input A3;
input CLK;
input D;
endmodule
module SRLC16(Q, Q15, A0, A1, A2, A3, CLK, D); // synthesis syn_black_box
output Q;
output Q15;
input A0;
input A1;
input A2;
input A3;
input CLK;
input D;
endmodule
module SRLC16E(Q, Q15, A0, A1, A2, A3, CE, CLK, D); // synthesis syn_black_box
output Q;
output Q15;
input A0;
input A1;
input A2;
input A3;
input CE;
input CLK;
input D;
endmodule
module SRLC16E_1(Q, Q15, A0, A1, A2, A3, CE, CLK, D); // synthesis syn_black_box
output Q;
output Q15;
input A0;
input A1;
input A2;
input A3;
input CE;
input CLK;
input D;
endmodule
module SRLC16_1(Q, Q15, A0, A1, A2, A3, CLK, D); // synthesis syn_black_box
output Q;
output Q15;
input A0;
input A1;
input A2;
input A3;
input CLK;
input D;
endmodule

module STARTUP_SPARTAN3_CLK(CLK) /* synthesis syn_black_box .noprune=1 xc_alias="STARTUP_SPARTAN3"*/;
    input  CLK;
endmodule
module STARTUP_SPARTAN3_GSR(GSR) /* synthesis syn_black_box .noprune=1 xc_alias="STARTUP_SPARTAN3"*/;
    input  GSR;
endmodule
module STARTUP_SPARTAN3_GTS(GTS) /* synthesis syn_black_box .noprune=1 xc_alias="STARTUP_SPARTAN3"*/;
    input  GTS;
endmodule
module STARTUP_SPARTAN3_ALL(GSR, GTS, CLK)
/* synthesis syn_black_box .noprune=1 xc_alias="STARTUP_SPARTAN3" */ ;
input GSR /* synthesis syn_defaultvalue=0 */,
      GTS /* synthesis syn_defaultvalue=0 */,
      CLK /* synthesis syn_defaultvalue=0 */;
endmodule
module STARTUP_SPARTAN3(CLK, GSR, GTS)
/* synthesis .noprune=1 */;
input GSR /* synthesis syn_defaultvalue=0 */,
      GTS /* synthesis syn_defaultvalue=0 */,
      CLK /* synthesis syn_defaultvalue=0 */;
STARTUP_SPARTAN3_GSR gsr( GSR );
STARTUP_SPARTAN3_GTS gts( GTS );
STARTUP_SPARTAN3_CLK clk(.CLK(CLK));
endmodule

module VCC(P); // synthesis syn_black_box .noprune = 1
output P;
endmodule
module XORCY(O, CI, LI); // synthesis syn_black_box
output O;
input CI;
input LI;
endmodule
module XORCY_D(LO, O, CI, LI); // synthesis syn_black_box
output O;
output LO;
input CI;
input LI;
endmodule
module XORCY_L(LO, CI, LI); // synthesis syn_black_box
output LO;
input CI;
input LI;
endmodule


module IBUFDS_LVPECL_25 (O, I, IB);// synthesis syn_black_box
output O;
input  I, IB;
endmodule


module IBUFGDS_LVPECL_25 (O, I, IB); // synthesis syn_black_box
output O;
input  I, IB;
endmodule


module OBUFDS_LVPECL_25 (O, OB, I); // synthesis syn_black_box
output O, OB;
input  I;
endmodule

module OBUFTDS_LVPECL_25 (O, OB, I, T); // synthesis syn_black_box
output O, OB;
input  I, T;
endmodule

