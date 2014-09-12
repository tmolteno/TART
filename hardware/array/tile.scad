
module tower(height=300)
{
	capheight = 20; 
	cylinder(h=height,r=45); // pluming pipe
	translate([0,0, height])cylinder(h=capheight,r=46); // cap
	translate([0,0, height+capheight])cylinder(h=1,r=75); // relector
	module gps_antenna(){
		cylinder(h=15,r=25); // antenna lower
		translate([0,0,15]) cylinder(h=33,r=25, r2=16); // antenna upper
	}
	translate([0,0, height+capheight]) gps_antenna();
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
	for ( i = [50 : 100 : squaresize] )
	{
		translate([framewidth/2,i,0])cylinder(h=10,r=3);
	}

}

squaresize = 1000;
framewidth = 25;

cube([squaresize,squaresize,5]);
steel_frame(framewidth = framewidth, squaresize = squaresize);
translate([150,150]) tower(); //0
translate([700,150]) tower(); //1
translate([400,400]) tower(); //2
translate([700,350]) tower(); //3
translate([300,850]) tower(); //4
translate([850,850]) tower(); //5
