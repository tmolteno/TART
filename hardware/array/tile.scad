
module tower(height=300)
{
	capheight = 20; 
	module pluming_fitting(height=50, r1=50, r2=80){
      r_hole = (r2+r1)/2;
	   translate([0,0, 5]) cylinder(h=5,r=r2);
		translate([r_hole,0,3])  cylinder(h=10,r=3);
		translate([-r_hole,0,3]) cylinder(h=10,r=3);
		translate([0,r_hole,3])  cylinder(h=10,r=3);
		translate([0,-r_hole,3]) cylinder(h=10,r=3);
	}
	module gps_antenna(){
		cylinder(h=15,r=25); // antenna lower
		translate([0,0,15]) cylinder(h=33,r=25, r2=16); // antenna upper
	}

	color("lightblue") cylinder(h=height,r=45); 				// pluming pipe
   color("LightSkyBlue") pluming_fitting();						// pluming fitting
	color("LightSkyBlue") translate([0,0, height])cylinder(h=capheight,r=46); // cap
	color("lightgrey") translate([0,0, height+capheight])cylinder(h=1,r=75); // reflector
	color("Gray") translate([0,0, height+capheight]) gps_antenna();
}



module steel_frame(
		framewidth = 25,
		squaresize = 1000,
		leglength  = 400)
{
	module frameside(squaresize,framewidth)
	{	
			screws(squaresize = squaresize, framewidth=framewidth);
			translate([0,0,-framewidth])          cube([framewidth,squaresize,framewidth]);
	      translate([0,0,-framewidth-leglength])cube([framewidth,squaresize,framewidth]);
	}
	rotate([0,0,0])   translate([0,0,0]) 										frameside(squaresize,framewidth);
	rotate([0,0,0])   translate([squaresize-framewidth,0])				frameside(squaresize,framewidth);
	rotate([0,0,90])  translate([0,-squaresize])								frameside(squaresize,framewidth);
	rotate([0,0,90])  translate([squaresize-framewidth,-squaresize]) 	frameside(squaresize,framewidth);

	//legs
	
	translate([0,0,-framewidth-leglength])      	cube([framewidth,framewidth,leglength]);
	translate([squaresize-framewidth,0,-framewidth-leglength])      	cube([framewidth,framewidth,leglength]);
	translate([squaresize-framewidth,squaresize-framewidth,-framewidth-leglength])      	cube([framewidth,framewidth,leglength]);
	translate([0,squaresize-framewidth,-framewidth-leglength])      	cube([framewidth,framewidth,leglength]);


}

module screws(squaresize = 1000, framewidth=25)
{
	for ( i = [50 : 100 : squaresize] ) {translate([framewidth/2,i,0])cylinder(h=10,r=3);}
}


module tile(squaresize = 1000, framewidth = 25, transport=false){
	color("brown") cube([squaresize,squaresize,5]);
	if (transport){
			  translate([0,squaresize,0]) rotate([180,0,0]) color("SteelBlue") steel_frame(framewidth = framewidth, squaresize = squaresize);
	}else{
	  color("SteelBlue") steel_frame(framewidth = framewidth, squaresize = squaresize);
	}
	translate([279,790]) tower(); //0
	translate([300,470]) tower(); //1
	translate([130,900]) tower(); //2
	translate([799,110]) tower(); //3
	translate([860,840]) tower(); //4
	translate([890,380]) tower(); //5
}

module tile_c(){
	translate([-500,-500]) tile();
}
translate([1000,0,0])  rotate([0,0,0]) tile_c();
translate([-1000,0,0]) rotate([0,0,330]) tile_c();
translate([0,1000,0])  rotate([0,0,210]) tile_c();
translate([0,-1000,0]) rotate([0,0,60]) tile_c();

//translate ([1200,0,-425]) tile(transport=true);

//tower();
