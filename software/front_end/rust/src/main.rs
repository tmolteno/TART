extern crate structopt;
extern crate ndarray;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate chrono;
// #[macro_use] extern crate reqwest;

// use ndarray::prelude::*;
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

use structopt::StructOpt;
use std::io::BufWriter;
use std::fs::File;

use sphere::{Hemisphere};
use std::time::{Instant};


/// Gridless deconvolution
/// 
/// Example: ./gridless nside 8

#[derive(StructOpt, Debug)]
#[structopt(name = "gridless")]
// #[structopt(raw(setting = "clap::AppSettings::ColoredHelp"))]
// #[structopt(raw(setting = "clap::AppSettings::AllowLeadingHyphen"))]
struct Opt {
    #[structopt(long="nside")]
    nside: u32,
    #[structopt(long="pixels")]
    pixels: bool,
    #[structopt(long="harmonics")]
    harmonics: bool,
    #[structopt(long="sources")]
    show_sources: bool,
    #[structopt(long="file", default_value="data.json")]
    file: String,
}

fn main() {
    let opt = Opt::from_args();
    let nside = opt.nside;
    let show_pixels = opt.pixels;
        
    // let mut sky = Hemisphere::new(2u32.pow(nside));
    let mut sky = Hemisphere::new(nside);

    if show_pixels {
        let fname = format!("hemisphere_pixels_{}.svg", nside);
        let mut svg_data = sky.to_svg( show_pixels, false, None);
        let mut output = BufWriter::new(File::create(fname).unwrap());
        svg_data.finalize(&mut output).expect("Writing SVG image failed");
        return
    }
    
    let data = tart_api::full_calibration_data(&opt.file);
    let obs = tart_obs::get_full(&data);                        
    let (u,v,w) = img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);

    if opt.harmonics {
        // Go through the uvw list and create an image for each one 
        // assuming the corresponding visibility is 1.0
        for i in 0..obs.vis_arr.len() {
            let mut new_vis = &obs.vis_arr * 0.0;
            new_vis[i] = utils::C64::new(1.0, 0.0);
            gridless::image_visibilities(
                                    &new_vis, 
                                    &u, &v, &w, &mut sky,
                                    true);
                                    
            let fname = format!("harmonics/harmonic_{}.svg", i);
            let mut svg_data =  sky.to_svg( false, false, None);
            let mut output = BufWriter::new(File::create(fname).unwrap());
            svg_data.finalize(&mut output).expect("Writing SVG image failed");
        }
        return
    }
    let start = Instant::now();
    gridless::image_visibilities(
                            &obs.vis_arr, 
                            &u, &v, &w, &mut sky,
                            false);
    println!("Gridless took {} ms", start.elapsed().as_millis()); 
    
    let dstring = obs.timestamp.format("%Y_%m_%d_%H_%M_%S_%Z");

  
    
    let fname = format!("gridless_{}.svg", dstring);

    let mut output = BufWriter::new(File::create(fname).unwrap());
    if opt.show_sources {
        let sources = tart_obs::get_sources(&data);
        let mut svg_data = sky.to_svg( show_pixels, true, Some(&sources));
        svg_data.finalize(&mut output).expect("Writing SVG image failed");
   } else {
        let mut svg_data = sky.to_svg( show_pixels, true, None);
        svg_data.finalize(&mut output).expect("Writing SVG image failed");
  }


}
