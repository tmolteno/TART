
dimx = 200;
dimy = 200;
dimz = 40;
flange = 25;
thickness = 2;

module smaholes(r_sma=5){
	translate([-75,0,0])rotate([90,0,0]) cylinder(r=r_sma,h=300, center=true);
	translate([-50,0,0])rotate([90,0,0]) cylinder(r=r_sma,h=300, center=true);
	translate([-25,0,0])rotate([90,0,0]) cylinder(r=r_sma,h=300, center=true);
}

module cat6_hole(r_cat=10){
	translate([80,0,0])rotate([0,90,0]) cylinder(r=r_cat,h=100, center=true);

}
//cat6_hole();
//smaholes();

module mountholes(){
	translate([(dimx+flange)/2,-(dimy+flange)/2,0]) cylinder(r=5,h=800, center=true);
	translate([(dimx+flange)/2, (dimy+flange)/2,0]) cylinder(r=5,h=800, center=true);
	translate([-(dimx+flange)/2,-(dimy+flange)/2,0]) cylinder(r=5,h=800, center=true);
	translate([-(dimx+flange)/2, (dimy+flange)/2,0]) cylinder(r=5,h=800, center=true);
}


module flange(){
   translate([dimx/2+2,0,dimz/2+1])    cube([2,dimy+6,2], center=true);
   translate([-(dimx/2+2),0,dimz/2+1]) cube([2,dimy+6,2], center=true);
   translate([0,-(dimx/2+2),dimz/2+1]) cube([dimy+6,2,2], center=true);
   translate([0,(dimx/2+2),dimz/2+1])  cube([dimy+6,2,2], center=true);
}



difference(){
	color("LightSteelBlue") union(){
		translate([0,0,150]) cube([dimx+2+2*flange,dimy+thickness+2*flange,thickness], center=true);

      translate([0,0,dimz/2]) cube([dimx+2+2*flange,dimy+thickness+2*flange,thickness], center=true);
		cube([dimx+thickness,dimy+thickness,dimz+thickness], center=true);
      
	};
	color("White") translate([0,0,thickness]) cube([dimx,dimy,dimz], center=true);
   color("White") mountholes();
	flange();
	color("White") smaholes();
	color("White") cat6_hole();

};

color("DimGray", a=0.3) flange();


