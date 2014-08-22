

 
 
 




window new WaveWindow  -name  "Waves for BMG Example Design"
waveform  using  "Waves for BMG Example Design"

      waveform add -signals /tart_block_buffer_ipcore_tb/status
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/CLKA
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/ADDRA
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/DINA
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/WEA
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/CLKB
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/ADDRB
      waveform add -signals /tart_block_buffer_ipcore_tb/tart_block_buffer_ipcore_synth_inst/bmg_port/DOUTB

console submit -using simulator -wait no "run"
