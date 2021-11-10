//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//

extern crate cfg_if;
extern crate wasm_bindgen;
extern crate web_sys;



extern crate ndarray;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;
extern crate chrono;
extern crate cdshealpix;
extern crate num;


pub mod gridless;
pub mod img;

mod sphere;

mod tart_api;
mod tart_obs;
mod utils;
mod svg;
mod sphere_plot;

use chrono::{DateTime, Utc};

use tart_api::FullDataset;
use sphere::{Hemisphere};

use tart_api::Source;
use utils::{VectorReal, VectorComplex};
use tart_obs::Observation;



use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
    // allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

pub fn make_svg(vis: &VectorComplex, 
            u: &VectorReal, v: &VectorReal, w: &VectorReal,  
            nside: u32, sources: Option<&Vec<Source>>) -> String {

    let mut sky = Hemisphere::new(nside);

    gridless::image_visibilities(
                            &vis, 
                            &u, &v, &w, &mut sky,
                            false);
    return sky.to_svg(true, sources).to_string();
}

#[wasm_bindgen]
pub fn json_to_svg_ext(json: String, nside: u32, show_sources: bool) -> JsValue {

    // https://rustwasm.github.io/wasm-bindgen/reference/arbitrary-data-with-serde.html
    let data = tart_api::json_to_dataset(&json);
    let obs = get_obs_from_dataset(&data);
    
    let (u,v,w) = img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);

    let sources = if show_sources {
        Some(get_sources_from_dataset(&data))
    } else {
        None
    };
    let svg = make_svg(&obs.vis_arr, &u, &v, &w,  nside, sources);
    JsValue::from_serde(&svg).unwrap()

}

pub fn json_to_svg(json: &String, nside: u32, show_sources: bool) -> (String, DateTime<Utc>) {
    let data = tart_api::json_to_dataset(&json);
    let obs = get_obs_from_dataset(&data);
    
    let (u,v,w) = img::get_uvw(
                            &obs.baselines,
                            &obs.ant_x,
                            &obs.ant_y,
                            &obs.ant_z);

    let sources = if show_sources {
        Some(get_sources_from_dataset(&data))
    } else {
        None
    };

    return (make_svg(&obs.vis_arr, &u, &v, &w,  nside, sources), obs.timestamp);

}

pub fn file_to_dataset(fname: &str) -> FullDataset {
    let data = tart_api::file_to_dataset(&fname);
    return data;
}

pub fn get_obs_from_dataset(data: &FullDataset) -> Observation {
    let obs = tart_obs::get_full(&data);
    return obs;
}

pub fn get_sources_from_dataset(data: &FullDataset) -> &Vec<Source> {
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

