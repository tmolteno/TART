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

use structopt::StructOpt;
use std::io::BufWriter;
use std::fs::File;

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
    
    let sources = if opt.show_sources {
        Some(gridlesslib::get_sources_from_data(&data))
    } else {
        None
    };
        
    let mut svg_data = gridlesslib::make_svg(&obs,  nside, sources);
    
    let dstring = obs.timestamp.format("%Y_%m_%d_%H_%M_%S_%Z");
    let fname = format!("gridless_{}.svg", dstring);

    let mut output = BufWriter::new(File::create(fname).unwrap());
    svg_data.finalize(&mut output).expect("Writing SVG image failed");

    println!("Gridless took {} ms", start.elapsed().as_millis()); 


}
