extern crate structopt;
extern crate ndarray;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate chrono;

extern crate cdshealpix;
extern crate num;

#[cfg(test)]
extern crate rand;

mod sphere;
mod gridless;
mod tart_api;
mod tart_obs;
mod utils;
mod img;
mod svg;
mod sphere_plot;

use svg::SVG;

use sphere::{Hemisphere};
use std::time::{Instant};


fn make_svg(fname: &str,  nside: u32, show_pixels: bool, show_sources: bool) -> SVG{

    let mut sky = Hemisphere::new(nside);

    let data = tart_api::full_calibration_data(&fname);
    let obs = tart_obs::get_full(&data);                        
    let (u,v,w) = img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);

    let start = Instant::now();
    gridless::image_visibilities(
                            &obs.vis_arr, 
                            &u, &v, &w, &mut sky,
                            false);
    println!("Gridless took {} ms", start.elapsed().as_millis()); 
    
    let dstring = obs.timestamp.format("%Y_%m_%d_%H_%M_%S_%Z");

    if show_sources {
        let sources = tart_obs::get_sources(&data);
        return sky.to_svg( show_pixels, true, Some(&sources));
    } else {
        return sky.to_svg( show_pixels, true, None);
    }

}
