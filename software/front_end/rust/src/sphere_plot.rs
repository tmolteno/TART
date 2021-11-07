
use svg::SVG;

use std::num;

use cdshealpix::ring::{vertices, hash};
use utils::{PI_OVER_2, VectorReal, median, mean};

use sphere::{Hemisphere, LonLat, HpAngle, ElAz};

use tart_api::{Source};

struct PlotCoords {
    w: i32,
    center: i32,
    scale: f64,
    line_size: u32
}

impl PlotCoords {

    pub fn new(w: i32) -> PlotCoords {
        let scale = (w as f64)/2.1;
        let center = ((w as f64)/2.0).round() as i32;

        let line_size = (w / 400) as u32;

        PlotCoords {
            w: w,
            center: center,
            scale: scale,
            line_size: line_size
        }
    }
    fn from_d(&self, d: f64) -> u32 {
        (d*self.scale) as u32
    }
    
    fn from_x(&self, x: f64) -> i32 {
        (x*self.scale) as i32 + self.center
    }
    fn from_y(&self, y: f64) -> i32 {
        (y*self.scale) as i32 + self.center
    }
    
    fn from_elaz(&self, elaz: &ElAz) -> (i32, i32) {
        let hp = elaz.to_hp();
        let (x,y) = hp.proj();
        (self.from_x(x), self.from_y(y))
    }
}

impl Hemisphere {

    fn corners(&self, pixel: u64) -> [(f64, f64); 4] {
        vertices(self.nside, pixel)
    }
    
    fn get_pix(&self, hp: &HpAngle) -> u64 {
        let lonlat = LonLat::from_hp(&hp);
        hash(self.nside, lonlat.lon, lonlat.lat)
    }

    pub fn to_svg(&self,
            pixels_only: bool,
            show_grid: bool,
            sources: Option<&Vec<Source>>) ->SVG {
    
        
        let mut image = SVG::new(12, 12);

        let w = 4000;
        image.view_box(0, 0, w, w);
        
        let pc = PlotCoords::new(w);
        let line_size = pc.line_size;
        
        image.desc("Gridless imaging from visibilities.");
        
        let mut max_p: f64 = -1e99;
        let mut min_p: f64 = 1e99;
        let mut mean_p: f64 = 0.0;
        
        for p in self.visible_pix.iter() {
            max_p = p.max(max_p);
            min_p = p.min(min_p);
            mean_p += p;
        }
        mean_p = mean_p / (self.npix as f64);
        
        let mut sdev_p: f64 = 0.0;
        for p in self.visible_pix.iter() {
            sdev_p += (p - mean_p) * (p - mean_p);
        }
        sdev_p = (sdev_p / (self.npix as f64)).sqrt();

        print!("'N_s':{}, 'S/N': {}, 'min': {}, 'max': {}, 'mean': {}, 'sdev': {}", self.npix, (max_p/sdev_p), min_p, max_p, mean_p, sdev_p);
        
//         let mean2 = mean(&self.visible_pix.as_slice().expect("")).expect("Fail");
        let med = median(&self.visible_pix.as_slice().expect("")).expect("Fail");
        
        
        
        let mut deviation : VectorReal = VectorReal::zeros(self.visible_pix.raw_dim());
        deviation = deviation + &self.visible_pix - med;
        deviation.mapv_inplace(f64::abs);
        let mad_p: f64 = median(&deviation.as_slice().expect("")).expect("Fail");
        

        println!(", 'R_mad': {}, 'MAD': {}, 'median': {}", (max_p/mad_p), mad_p, med);

        if !pixels_only {
            let base_poly_attrib = "stroke-width=2 stroke-linejoin=round stroke-opacity=1.0";
            image.g_attribs(&base_poly_attrib);
        }
        for i in 0..self.npix {
            let pixel = self.visible_indices[i];
            let corners = self.corners(pixel); // lon lat
            let value = self.visible_pix[i];
            
            let mut poly = Vec::new();
            
            let mut x_mean = 0.0;
            let mut y_mean = 0.0;
            
            let mut max_lat = 0.0;
            let mut min_lat = PI_OVER_2;

            for p in &corners {
                max_lat = f64::max(max_lat, p.1);
                min_lat = f64::min(min_lat, p.1);
                
                let ll = LonLat::new(p.0, p.1);
                let hp = HpAngle::from_lonlat(&ll);
                
                if hp.theta > PI_OVER_2 {
                    panic!("colatitude {} < PI_OVER_2", hp.theta);
                }
                let (x,y) = hp.proj();
            
                poly.push((pc.from_x(x), pc.from_y(y)));
                
                x_mean = x_mean + x;
                y_mean = y_mean + y;
            }
            x_mean = x_mean/4.0;
            y_mean = y_mean/4.0;

            if pixels_only {
                let dlat = max_lat - min_lat;
                
                let font_size = pc.from_d(dlat*max_lat.sin())/5;

                
                let attrib_txt = format!("font-size={}px text-anchor=middle", font_size);
                let attrib_txt_poly = format!("fill=none stroke=black stroke-width={} stroke-linejoin=round", font_size/10);

                image.polygon(&poly, &attrib_txt_poly);
                let label = format!("{}", i);
                
                image.text(pc.from_x(x_mean), pc.from_y(y_mean) + (font_size/2) as i32, &label, &attrib_txt);
                
            } else {
                let (r, g, b) = cmap((value - min_p) / ( max_p - min_p));
                let color = format!("rgb({:.1},{:.1},{:.1})", r, g, b);

                let attrib = format!("fill={} stroke={}",color, color);
                if max_lat > 0.07 {
                    image.polygon(&poly, &attrib);
                }
            }
        }
        if !pixels_only {
            image.g_end(); // end the attribute group for polygons
        }
        if show_grid {
            let attrib_grid = format!("fill=none stroke=white stroke-width={} stroke-linejoin=round stroke-dasharray={},{}", line_size, 5*line_size, 10*line_size);
            for angle in &[30, 60, 90] {
                let rad = (*angle as f64).to_radians();
                let radius = pc.from_d(rad.sin());
                image.circle(pc.from_x(0.0), pc.from_y(0.0), radius, &attrib_grid );
            }
            for angle in (0..360).step_by(30) {
                let rad = (angle as f64).to_radians();
                let x = rad.sin();
                let y = rad.cos();
                image.line(pc.from_x(0.0), pc.from_y(0.0), pc.from_x(x), pc.from_y(y), &attrib_grid );
            }
        }

        match sources {
            Some(src) => {
                let angular_size = (2.0 as f64).to_radians();
                let attrib_source = format!("fill=none stroke=red stroke-width={}", line_size);
                for s in src {
                    if s.el > 20.0 {
                        let el = s.el.to_radians();
                        let az = s.az.to_radians();
                        let elaz = ElAz::new(el, az);
                        
                        let (x,y) = pc.from_elaz(&elaz);
                        
                        let radius = pc.from_d(angular_size);
                        //    let minor_axis = major_axis*el.sin();
                        //    let transform = image.transform();
                        image.circle(x,y, radius, &attrib_source );
                    }
                }
            },
            None    => {/* nothing to do */}
        }
        
        //let mut output = BufWriter::new(File::create(fname).unwrap());
       // image.finalize(&mut output).expect("Writing SVG image failed");
        return image
    }
    
}

use num::clamp;

/* Generate a colour Map
// http://inversed.ru/Blog_2.htm
// THese are CUbeHelix values
*/
fn cmap(fract: f64) -> (f64, f64, f64) {
    let start = 1.0;
    let rot = -1.5;
    let sat = 1.5; 
    let _gamma = 1.0;
    
    let pi = 3.14159265;
    
    let angle = 2.0 * pi * (start / 3.0 + rot * fract + 1.);

    let amp = sat * fract * (1. - fract) / 2.;

    // compute the RGB vectors according to main equations
    let mut red = fract + amp * (-0.14861 * angle.cos() + 1.78277 * angle.sin());
    let mut grn = fract + amp * (-0.29227 * angle.cos() - 0.90649 * angle.sin());
    let mut blu = fract + amp * (1.97294 * angle.cos());

    // find where RBB are outside the range [0,1], clip
    red = clamp(red, 0.0, 1.0);
    grn = clamp(grn, 0.0, 1.0);
    blu = clamp(blu, 0.0, 1.0);

    (red*255.0, grn*255.0, blu*255.0)
}
