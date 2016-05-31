module sma() {
    cylinder(d=8, h=10, $fn=61);
}

antenna_len = 47.0;

module lna() {
    rotate([90,0,0]) union() {
        color("blue") cube([antenna_len+18,4,1.6]);
        color("blue") translate([0,-12.5,0]) cube([18,25,1.6]);
        color("gold") translate([7.5,7.5,1]) sma();
    }
}

module cable_space() {
    rotate([90,0,0]) union() {
        cube([antenna_len+18,4,1.6]);
        translate([0,-12.5,0]) cube([18,25,1.6]);
        translate([7.5,7.5,1]) cylinder(d=12,h=30, $fn=63);
    }
}


diameter=123.0;
height = 112.5;
frame_depth = 3;
frame_thickness = 15;

module frame() {
    //translate([-diameter/2, height/2+1.6,-2]) lna();
    difference() {
        cube([diameter, height, frame_thickness], center=true);
        cube([diameter-frame_depth*2, height-frame_depth*2, 20], center=true);
        translate([-diameter/2-0.1, height/2+1.0,-2]) cable_space();
        translate([-diameter/2 + 20, -height/2+1.6,0]) rotate([90,0,0]) cylinder(d=10, h=20, center=true);
    }
}

frame();

