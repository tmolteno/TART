//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//
extern crate structopt;
extern crate gridlesslib;

#[cfg(test)]
extern crate rand;

use structopt::StructOpt;
use std::io::BufWriter;
use std::io::Write;
use std::io::Read;
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
    
    let mut file = File::open(&opt.file).unwrap();
    let mut json = String::new();
    file.read_to_string(&mut json).unwrap();

   // Main library call. Returns some SVG data
    let (svg_data, timestamp) = gridlesslib::json_to_svg(&json,  nside, opt.show_sources);
    
    let dstring = timestamp.format("%Y_%m_%d_%H_%M_%S_%Z");
    let fname = format!("gridless_{}.svg", dstring);

    let mut output = BufWriter::new(File::create(fname).unwrap());
    //svg_data.finalize(&mut output).expect("Writing SVG image failed");
    (&mut output).write_all(svg_data.as_bytes()).expect("Writing SVG image failed");

    println!("Gridless took {} ms", start.elapsed().as_millis()); 
}
