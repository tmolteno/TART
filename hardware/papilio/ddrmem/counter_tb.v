`timescale 1ns/100ps
module counter_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	
	reg	reset	= 0;
	
	reg	start	= 0;
	wire	done;
	
	initial begin : Sim
		$display ("Time CLK RST Start Done");
		$monitor ("%5t  %b  %b  %b    %b  ", $time, clock, reset, start, done);
		
		#5
		reset	<= 1;
		
		#10
		reset	<= 0;
		
		#10
		start	<= 1;
		
		#10
		start	<= 0;
		
		#150
		$finish;
	end	// Sim
	
	defparam	CNT10.COUNT	= 10;
	defparam	CNT10.BITS	= 4;
	counter CNT10 (
		.clock_i(clock),
		.reset_i(reset),
		.start_i(start),
		.done_o	(done)
	);
	
endmodule	// counter_tb
