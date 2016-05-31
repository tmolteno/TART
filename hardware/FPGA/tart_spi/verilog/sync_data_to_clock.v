`timescale 1ns/1ns
module sync_antennas_to_clock(
      input fast_clk,
      input [23:0] data_in,
      output wire slow_clk,
      output wire [23:0] data_out
      );
      sync_antennas_fsm safsm_a00(.fast_clk(fast_clk), .input_signal(data_in[0]), .sampled_bit(data_out[0]),.slow_clk(slow_clk));
      sync_antennas_fsm safsm_a01(.fast_clk(fast_clk), .input_signal(data_in[1]), .sampled_bit(data_out[1]));
      sync_antennas_fsm safsm_a02(.fast_clk(fast_clk), .input_signal(data_in[2]), .sampled_bit(data_out[2]));
      sync_antennas_fsm safsm_a03(.fast_clk(fast_clk), .input_signal(data_in[3]), .sampled_bit(data_out[3]));
      sync_antennas_fsm safsm_a04(.fast_clk(fast_clk), .input_signal(data_in[4]), .sampled_bit(data_out[4]));
      sync_antennas_fsm safsm_a05(.fast_clk(fast_clk), .input_signal(data_in[5]), .sampled_bit(data_out[5]));
      sync_antennas_fsm safsm_a06(.fast_clk(fast_clk), .input_signal(data_in[6]), .sampled_bit(data_out[6]));
      sync_antennas_fsm safsm_a07(.fast_clk(fast_clk), .input_signal(data_in[7]), .sampled_bit(data_out[7]));
      sync_antennas_fsm safsm_a08(.fast_clk(fast_clk), .input_signal(data_in[8]), .sampled_bit(data_out[8]));
      sync_antennas_fsm safsm_a09(.fast_clk(fast_clk), .input_signal(data_in[9]), .sampled_bit(data_out[9]));
      sync_antennas_fsm safsm_a10(.fast_clk(fast_clk),.input_signal(data_in[10]),.sampled_bit(data_out[10]));
      sync_antennas_fsm safsm_a11(.fast_clk(fast_clk),.input_signal(data_in[11]),.sampled_bit(data_out[11]));
      sync_antennas_fsm safsm_a12(.fast_clk(fast_clk),.input_signal(data_in[12]),.sampled_bit(data_out[12]));
      sync_antennas_fsm safsm_a13(.fast_clk(fast_clk),.input_signal(data_in[13]),.sampled_bit(data_out[13]));
      sync_antennas_fsm safsm_a14(.fast_clk(fast_clk),.input_signal(data_in[14]),.sampled_bit(data_out[14]));
      sync_antennas_fsm safsm_a15(.fast_clk(fast_clk),.input_signal(data_in[15]),.sampled_bit(data_out[15]));
      sync_antennas_fsm safsm_a16(.fast_clk(fast_clk),.input_signal(data_in[16]),.sampled_bit(data_out[16]));
      sync_antennas_fsm safsm_a17(.fast_clk(fast_clk),.input_signal(data_in[17]),.sampled_bit(data_out[17]));
      sync_antennas_fsm safsm_a18(.fast_clk(fast_clk),.input_signal(data_in[18]),.sampled_bit(data_out[18]));
      sync_antennas_fsm safsm_a19(.fast_clk(fast_clk),.input_signal(data_in[19]),.sampled_bit(data_out[19]));
      sync_antennas_fsm safsm_a20(.fast_clk(fast_clk),.input_signal(data_in[20]),.sampled_bit(data_out[20]));
      sync_antennas_fsm safsm_a21(.fast_clk(fast_clk),.input_signal(data_in[21]),.sampled_bit(data_out[21]));
      sync_antennas_fsm safsm_a22(.fast_clk(fast_clk),.input_signal(data_in[22]),.sampled_bit(data_out[22]));
      sync_antennas_fsm safsm_a23(.fast_clk(fast_clk),.input_signal(data_in[23]),.sampled_bit(data_out[23]));

endmodule
