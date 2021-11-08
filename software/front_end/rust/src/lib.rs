extern crate ndarray;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;
extern crate chrono;
extern crate cdshealpix;
extern crate num;
// 
// #[cfg(test)]
// extern crate rand;

pub mod sphere;
pub mod gridless;
pub mod tart_api;
mod tart_obs;
mod utils;
mod img;
mod svg;
mod sphere_plot;

use svg::SVG;
use tart_api::FullDataset;
use sphere::{Hemisphere};

use tart_obs::get_full;
use tart_obs::get_sources;
use tart_api::Source;

use img::get_uvw;
pub use gridless::image_visibilities;
use utils::{VectorReal, VectorComplex, C64};
use tart_obs::Observation;

pub fn make_svg(obs: &Observation,  nside: u32, sources: Option<&Vec<Source>>) -> SVG {


    // let data = tart_api::full_calibration_data(&fname);
    let (u,v,w) = img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);

    let mut sky = Hemisphere::new(nside);

    gridless::image_visibilities(
                            &obs.vis_arr, 
                            &u, &v, &w, &mut sky,
                            false);
    return sky.to_svg(true, sources);
//     if show_sources {
//         let sources = tart_obs::get_sources(&data);
//         return sky.to_svg(true, Some(&sources));
//     } else {
//         return sky.to_svg(true, None);
//     }
}

pub fn parse_file(fname: &str) -> FullDataset {
    let data = tart_api::full_calibration_data(&fname);
    return data;
}

pub fn get_obs_from_data(data: &FullDataset) -> Observation {
    let obs = tart_obs::get_full(&data);
    return obs;
}

pub fn get_sources_from_data(data: &FullDataset) -> &Vec<Source> {
    let sources = tart_obs::get_sources(&data);
    return sources;
}


pub fn get_uvw_from_obs(obs: &Observation) -> (VectorReal, VectorReal, VectorReal) {
    let (u,v,w) = img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);
                            
    return (u, v, w);
}

