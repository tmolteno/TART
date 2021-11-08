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
extern crate gridlesslib;

#[cfg(test)]
extern crate rand;

// mod sphere;
// mod gridless;
// mod tart_obs;
// mod utils;
// mod svg;
// mod sphere_plot;
use structopt::StructOpt;
use std::io::BufWriter;
use std::fs::File;

use gridlesslib::sphere::{Hemisphere};
use gridlesslib::tart_api::FullDataset;
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
    #[structopt(long="sources")]
    show_sources: bool,
    #[structopt(long="file", default_value="data.json")]
    file: String,
}

fn main() {
    let opt = Opt::from_args();
    let nside = opt.nside;
        
    let start = Instant::now();
    let data = gridlesslib::parse_file(&opt.file);
    let obs = gridlesslib::get_obs_from_data(&data);                        
    let (u,v,w) = gridlesslib::get_uvw_from_obs(&obs);

    let mut svg_data = gridlesslib::make_svg(&data,  nside, opt.show_sources);
    let dstring = obs.timestamp.format("%Y_%m_%d_%H_%M_%S_%Z");
    let fname = format!("gridless_{}.svg", dstring);

    let mut output = BufWriter::new(File::create(fname).unwrap());
    svg_data.finalize(&mut output).expect("Writing SVG image failed");

//     gridless::image_visibilities(
//                             &obs.vis_arr, 
//                             &u, &v, &w, &mut sky,
//                             false);
    println!("Gridless took {} ms", start.elapsed().as_millis()); 
//     
// 
//   
//     
// 
//     if opt.show_sources {
//         let sources = tart_obs::get_sources(&data);
//         let mut svg_data = sky.to_svg( show_pixels, true, Some(&sources));
//         svg_data.finalize(&mut output).expect("Writing SVG image failed");
//    } else {
//         let mut svg_data = sky.to_svg( show_pixels, true, None);
//         svg_data.finalize(&mut output).expect("Writing SVG image failed");
//   }


}
