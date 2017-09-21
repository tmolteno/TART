/*
 * Module      : afifo16
 * Copyright   : (C) Patrick Suggate 2007
 * License     : see below
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : non-portable
 * 
 * This source file may be used and distributed without          *
 * restriction provided that this copyright statement is not     *
 * removed from the file and that any derivative work contains   *
 * the original copyright notice and the associated disclaimer.  *
 *                                                               *
 *     THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY       *
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED     *
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *
 * FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL THE AUTHOR        *
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,           *
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES      *
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE     *
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR          *
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF    *
 * LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY, OR TORT    *
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT    *
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE           *
 * POSSIBILITY OF SUCH DAMAGE.                                   *
 *
 * Description:
 * Asynchronous FIFO with a buffer-size of 16 entries.
 * 
 * Changelog:
 *  + ??/??/2007  --  initial file;
 * 
 * TODO:
 * 
 */

`timescale 1ns/1ps
module afifo16 (
		reset_ni,
		
		rd_clk_i,
		rd_en_i,
		rd_data_o,
		
		wr_clk_i,
		wr_en_i,
		wr_data_i,
		
		rempty_o,
		wfull_o
	);
	
	parameter	WIDTH	= 16;
	
	input	reset_ni;
	
	input	rd_clk_i;
	input	rd_en_i;
	output	[WIDTH-1:0]	rd_data_o;
	
	input	wr_clk_i;
	input	wr_en_i;
	input	[WIDTH-1:0]	wr_data_i;
	
	output	rempty_o;
	output	wfull_o;
	
	fifo_dc_gray #(WIDTH,4,16) FIFO0 (
		.rd_clk		(rd_clk_i),
		.wr_clk		(wr_clk_i),
		.rst		(reset_ni),
		.clr		(1'b0),
		.din		(wr_data_i),
		.we		(wr_en_i),
		
		.dout		(rd_data_o),
		.re		(rd_en_i),
		.full		(wfull_o),
		.empty		(rempty_o),
		.wr_level	(),
		.rd_level	()
	);
	
endmodule	// afifo16
