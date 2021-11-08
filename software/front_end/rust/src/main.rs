//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//
extern crate structopt;
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
    let data = gridlesslib::json_to_dataset(&opt.file);
    let obs = gridlesslib::get_obs_from_dataset(&data);
    
    let (u,v,w) = gridlesslib::img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);

    let sources = if opt.show_sources {
        Some(gridlesslib::get_sources_from_dataset(&data))
    } else {
        None
    };

    // Main library call. Returns some SVG data
    let mut svg_data = gridlesslib::make_svg(&obs.vis_arr, &u, &v, &w,  nside, sources);
    
    let dstring = obs.timestamp.format("%Y_%m_%d_%H_%M_%S_%Z");
    let fname = format!("gridless_{}.svg", dstring);

    let mut output = BufWriter::new(File::create(fname).unwrap());
    svg_data.finalize(&mut output).expect("Writing SVG image failed");

    println!("Gridless took {} ms", start.elapsed().as_millis()); 
}
