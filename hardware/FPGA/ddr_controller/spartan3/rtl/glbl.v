

// $Header: /cvsroot/physics/physics/people/patrick/ddr_controller/spartan3/rtl/glbl.v,v 1.2 2007-06-29 15:26:38 tim Exp $

`timescale  1 ps / 1 ps

module glbl (GSR,
             GTS,
             PRLD);
             
    output GSR, GTS, PRLD; 

    parameter ROC_WIDTH = 100000;
    parameter TOC_WIDTH = 0;

    wire GSR;
    wire GTS;
    wire PRLD;

    reg GSR_int;
    reg GTS_int;
    reg PRLD_int;

    assign (weak1, weak0) GSR = GSR_int;
    assign (weak1, weak0) GTS = GTS_int;
    assign (weak1, weak0) PRLD = PRLD_int;

    initial begin
	GSR_int = 1'b1;
	PRLD_int = 1'b1;
	#(ROC_WIDTH)
	GSR_int = 1'b0;
	PRLD_int = 1'b0;
    end

    initial begin
	GTS_int = 1'b1;
	#(TOC_WIDTH)
	GTS_int = 1'b0;
    end

endmodule
