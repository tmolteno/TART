/*   Endcap for Circular Wageguide Antenna

      License. GPL v3.
      
      Author. Tim Molteno tim@elec.ac.nz
 */
 
 $fn = 151;
waveguide_diameter = 125.0;
wall_thickness = 1.6;
outer_diameter = waveguide_diameter+2*wall_thickness;

plug_depth = 10;
taper = 1.0;
c_size = waveguide_diameter*2;

module waveguide() {
  translate([0,0,-0.5]) difference() {
    cylinder(h = plug_depth, d=outer_diameter);
//    translate([0,0,-0.5]) cylinder(h = plug_depth+1, d2=(waveguide_diameter + taper/2), d1=waveguide_diameter-taper/2);
  }
}

module endcap() {
  difference() {
    sphere(d=outer_diameter+10.0);
    translate([-c_size/2, -c_size/2, -c_size])cube([c_size,c_size,c_size]);
    waveguide();
  }
}

difference() {
    translate([0,0,25]) rotate([0,170,0]) endcap();
    translate([-c_size/2, -c_size/2, -c_size])cube([c_size,c_size,c_size]);
}    

