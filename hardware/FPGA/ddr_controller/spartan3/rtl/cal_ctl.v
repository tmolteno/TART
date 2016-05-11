`resetall
`timescale 1ns/10ps
  
`define tap1 5'b01111
`define tap2 5'b10111
`define tap3 5'b11011
`define tap4 5'b11101
`define tap5 5'b11110
`define tap6 5'b11111

`define defaultTap `tap4
  
  module cal_ctl(flop2,clk,reset,tapForDqs);
   
   input   	     clk;
   input 	     reset;
   input [31:0]      flop2;
 
   
   output [4:0]      tapForDqs/* synthesis syn_keep=1 */;
   
   reg [5:0] 	     cnt/* synthesis syn_keep=1 */; /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [5:0] 	     cnt1/* synthesis syn_keep=1 */; /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */

   reg 		     trans_oneDtct/* synthesis syn_keep=1 */; 
   reg 		     trans_twoDtct/* synthesis syn_keep=1 */; 
   
   reg  [4:0]	     phase_cnt/* synthesis syn_keep=1 */;		

   reg [4:0] 	     tapForDqs/* synthesis syn_keep=1 */;
   
 

   reg 	[31:0]	tap_dly_reg/* synthesis syn_keep=1 */;	

   reg          enb_trans_two_dtct/* synthesis syn_keep=1 */;
   
   
   /***************************** Changed on April 9 For Successive Transition********************/
	always @(posedge clk)	
	begin
	   if(reset)
	      enb_trans_two_dtct <= 1'b0;
 	   else if(phase_cnt >= 5'd3)	
	      enb_trans_two_dtct <= 1'b1;
	   else
	      enb_trans_two_dtct <= 1'b0;		
	end

   /************************************ Changed on April 6 ************************************/

   always @(posedge clk)
   begin
      if(reset)
	     tap_dly_reg <= 32'd0;
      else if(cnt[5] == 1'b1)
	     tap_dly_reg <= flop2;
      else
  	     tap_dly_reg <= tap_dly_reg;
   end		
   
   /******************** Free Running Counter For Counting 32 States ****************************/
   
   always @(posedge clk)
     begin
	if(reset || (cnt[5] == 1'b1)) 
	  cnt[5:0] <= 6'b0;
	else
	  cnt[5:0] <= cnt[5:0] + 1'b1;
     end

   always @(posedge clk)
     begin
	if(reset || (cnt1[5] == 1'b1)) 
	  cnt1[5:0] <= 6'b0;
	else
	  cnt1[5:0] <= cnt1[5:0] + 1'b1;
     end
   

  always @(posedge clk)
  begin
     if(reset || (cnt[5] == 1'b1))
     begin
        phase_cnt <= 5'd0;			
     end	
     else if (trans_oneDtct & (~trans_twoDtct))
        phase_cnt <= phase_cnt + 1;
     else
        phase_cnt <= phase_cnt;
  end				

   /******************** Checking For The First Transition *************************************/
   always @(posedge clk)
   begin
      if(reset)
	begin
	   trans_oneDtct <= 1'b0;
	   trans_twoDtct <= 1'b0;
	  end
	else if(cnt[5] == 1'b1) // cnt is 32
	begin
	   trans_oneDtct <= 1'b0;
	   trans_twoDtct <= 1'b0;
      end
	else if (cnt[4:0] == 5'd0) 
      begin
         if ((tap_dly_reg[0] ~^ tap_dly_reg[1]))
         begin
            trans_oneDtct <= 1'b1;
	      trans_twoDtct <= 1'b0;
         end 
      end 
	else if ((cnt[4:0] == 5'd1) & (trans_twoDtct == 1'b0)) 
      begin
         if (tap_dly_reg[1] ~^ tap_dly_reg[2]) 
         begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) ) 
            begin	
		   trans_twoDtct <= 1'b1;
	      end 
            else 
            begin
		   trans_oneDtct <= 1'b1;
	      end
         end
	   else 
         begin
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
      else if ((cnt[4:0] == 5'd2) & (trans_twoDtct == 1'b0) ) 
      begin
         if (tap_dly_reg[2] ~^ tap_dly_reg[3]) 
         begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) ) 
            begin	
		   trans_twoDtct <= 1'b1;
	      end 
            else 
            begin
		   trans_oneDtct <= 1'b1;
	      end
         end 
         else 
         begin                 
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd3) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[3] ~^ tap_dly_reg[4]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin	
		   trans_twoDtct <= 1'b1;
		end
		else
		begin
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd4) & (trans_twoDtct == 1'b0))
	begin
         if (tap_dly_reg[4] ~^ tap_dly_reg[5]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin	
		   trans_twoDtct <= 1'b1;
	      end
	      else
		begin
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 	
	else if ((cnt[4:0] == 5'd5) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[5] ~^ tap_dly_reg[6]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin	
               trans_twoDtct <= 1'b1;
		end
		else
		begin		     
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 	
	else if ((cnt[4:0] == 5'd6) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[6] ~^ tap_dly_reg[7]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin
               trans_twoDtct <= 1'b1;
  	      end
            else
		begin
	         trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin               
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd7) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[7] ~^ tap_dly_reg[8]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		    
               trans_twoDtct <= 1'b1;
		end
		else
		begin		      
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin                 
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd8) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[8] ~^ tap_dly_reg[9]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		     
               trans_twoDtct <= 1'b1;
		end
		else
		begin		     
		   trans_oneDtct <= 1'b1;
	      end
         end 
         else 
	   begin              
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 	
	else if ((cnt[4:0] == 5'd9) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[9] ~^ tap_dly_reg[10]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		     
               trans_twoDtct <= 1'b1;
		end
		else
		begin		     
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin                 
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd10) & (trans_twoDtct == 1'b0)) 
      begin
         if (tap_dly_reg[10] ~^ tap_dly_reg[11]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		      
               trans_twoDtct <= 1'b1;
	      end
		else
		begin		      
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin                 
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd11) & (trans_twoDtct == 1'b0))
	begin
         if (tap_dly_reg[11] ~^ tap_dly_reg[12]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			      
               trans_twoDtct <= 1'b1;
		end
		else
		begin		      
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin                  
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd12) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[12] ~^ tap_dly_reg[13]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			      
               trans_twoDtct <= 1'b1;
		end
		else
		begin		     
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin                  
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt[4:0] == 5'd13) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[13] ~^ tap_dly_reg[14]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		      
               trans_twoDtct <= 1'b1;
		end
		else
		begin		      
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin                  
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd14) & (trans_twoDtct == 1'b0))
	begin
         if (tap_dly_reg[14] ~^ tap_dly_reg[15]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			      
               trans_twoDtct <= 1'b1;
	      end
		else
		begin		      
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin                
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd15) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[15] ~^ tap_dly_reg[16]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			    
               trans_twoDtct <= 1'b1;
		end
		else
		begin	    
		   trans_oneDtct <= 1'b1;
		end
         end 
         else 
	   begin                
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd16) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[16] ~^ tap_dly_reg[17]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			    
               trans_twoDtct <= 1'b1;
	      end
	      else
		begin		  
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin                
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd17) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[17] ~^ tap_dly_reg[18]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			    
               trans_twoDtct <= 1'b1;
		end
		else
		begin		    
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin                
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd18) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[18] ~^ tap_dly_reg[19]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		    
               trans_twoDtct <= 1'b1;
	      end
		else
		begin
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin                
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd19) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[19] ~^ tap_dly_reg[20]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		  
               trans_twoDtct <= 1'b1;
	      end
	      else
		begin		  
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin               
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd20) & (trans_twoDtct == 1'b0)) 
      begin
         if (tap_dly_reg[20] ~^ tap_dly_reg[21]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
	      begin		 
               trans_twoDtct <= 1'b1;
		end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin             
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd21) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[21] ~^ tap_dly_reg[22]) 
	   begin
            if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		 
               trans_twoDtct <= 1'b1;
		end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin             
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd22) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[22] ~^ tap_dly_reg[23]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			 
               trans_twoDtct <= 1'b1;
		end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin             
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd23) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[23] ~^ tap_dly_reg[24]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			
               trans_twoDtct <= 1'b1;
		end
	      else
		begin
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
      else if ((cnt1[4:0] == 5'd24) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[24] ~^ tap_dly_reg[25]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			
               trans_twoDtct <= 1'b1;
	      end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin             
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd25) & (trans_twoDtct == 1'b0))
	begin
         if (tap_dly_reg[25] ~^ tap_dly_reg[26]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin			 
               trans_twoDtct <= 1'b1;
	      end
	      else
	      begin		 
               trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin            
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
      else if ((cnt1[4:0] == 5'd26) & (trans_twoDtct == 1'b0)) 
      begin
         if (tap_dly_reg[26] ~^ tap_dly_reg[27]) 
         begin
            if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
	      begin	 
               trans_twoDtct <= 1'b1;
	      end
	      else
	      begin		 
	         trans_oneDtct <= 1'b1;
	      end
         end 
         else 
         begin             
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct; 
         end
      end 
	else if ((cnt1[4:0] == 5'd27) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[27] ~^ tap_dly_reg[28]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		 
               trans_twoDtct <= 1'b1;
		end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin            
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 	
	else if ((cnt1[4:0] == 5'd28) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[28] ~^ tap_dly_reg[29]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		 
               trans_twoDtct <= 1'b1;
		end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
		end
         end 
	   else 
	   begin             
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd29) & (trans_twoDtct == 1'b0))
	begin
	   if (tap_dly_reg[29] ~^ tap_dly_reg[30]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct) )
		begin		
               trans_twoDtct <= 1'b1;
		end
		else
		begin		 
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin          
            trans_oneDtct <= trans_oneDtct;    
		trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else if ((cnt1[4:0] == 5'd30) & (trans_twoDtct == 1'b0)) 
	begin
         if (tap_dly_reg[30] ~^ tap_dly_reg[31]) 
	   begin
	      if((trans_oneDtct == 1'b1) && (enb_trans_two_dtct))
		begin		
               trans_twoDtct <= 1'b1;
	      end
	      else
		begin		
		   trans_oneDtct <= 1'b1;
	      end
         end 
	   else 
	   begin
            trans_oneDtct <= trans_oneDtct;    
	      trans_twoDtct <= trans_twoDtct;    
         end
      end 
	else
	begin
         trans_oneDtct <= trans_oneDtct;    				
	  trans_twoDtct <= trans_twoDtct;    				
	end
   end
   
always @(posedge clk)
begin
   if(reset)
      tapForDqs <= `defaultTap;
   else if(cnt1[4] && cnt1[3] && cnt1[2] && cnt1[1] && cnt1[0])	
   begin
      if((trans_oneDtct == 1'b0) || (trans_twoDtct == 1'b0) || (phase_cnt > 5'b01011))
         tapForDqs <= `tap6;
	  else if((phase_cnt < 5'b01000)) 
		 tapForDqs <= `tap3;
	  else if((phase_cnt < 5'b01010)) 
		 tapForDqs <= `tap4;
	  else
		 tapForDqs <= `tap4;
   end
   else
      tapForDqs <= tapForDqs;
end		


endmodule
