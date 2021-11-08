//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//

use serde::de::DeserializeOwned;

// use reqwest::Error;
// fn api_get<T: DeserializeOwned>(route: &str) -> T {
//     let request_url = format!("https://tart.elec.ac.nz/signal/api/v1/{route}",
//                                 route = route);
//     println!("URL: {}", request_url);
//     let mut response = reqwest::get(&request_url).unwrap();
//     response.json().expect(&format!("Failed to get data from route {}", route))
// }


use std::fs::File;
use std::io::prelude::*;

fn api_parse_json<T: DeserializeOwned>(contents: &String) -> T {
    serde_json::from_str(&contents).expect(&format!("Failed to get data from string {}", contents))
}

fn api_parse<T: DeserializeOwned>(fname: &str) -> T {

    let mut file = File::open(fname).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    api_parse_json(&contents)
}


/************************************* Gains and Phases ***************************************/

#[derive(Deserialize, Debug)]
pub struct Gains {
    pub gain: Vec<f64>,
    pub phase_offset: Vec<f64>,
}

#[allow(dead_code)]
pub fn gains() -> Gains  {
//     api_get::<Gains>("calibration/gain")
    api_parse::<Gains>("gains.json")
}


/************************************* Visibilities ***************************************/
#[derive(Deserialize, Debug)]
pub struct VisData {
    pub data: Vec<VisEntry>,
    pub timestamp: String,
}

#[derive(Deserialize, Debug)]
pub struct VisEntry {
    pub i: u32,
    pub j: u32,
    pub im: f64,
    pub re: f64
}

#[allow(dead_code)]
pub fn visibilities() -> VisData  {
//     api_get::<VisData>("imaging/vis")
    api_parse::<VisData>("vis.json")
}


/************************************* Info ***************************************/
#[derive(Deserialize, Debug)]
pub struct TARTinfo {
    #[allow(dead_code)]
    info: TARTdetail
}

#[allow(non_snake_case)]
#[derive(Deserialize, Debug)]
pub struct TARTdetail {
    pub L0_frequency: f64,
    pub baseband_frequency: f64,
    pub location: Location,
    pub name: String,
    pub num_antenna: u32,
    pub operating_frequency: f64,
    pub sampling_frequency: f64
}

#[derive(Deserialize, Debug)]
pub struct Location {
    pub alt: f64,
    pub lat: f64,
    pub lon: f64,
}

#[allow(dead_code)]
pub fn info() -> TARTinfo  {
//     api_get::<TARTinfo>("info")
    api_parse::<TARTinfo>("info.json")
}


/************************************* Antenna Positions ***************************************/
#[derive(Deserialize, Debug)]
pub struct AntPosition {
    pub x: f64,
    pub y: f64,
    pub z: f64
}

#[allow(dead_code)]
pub fn ant_positions() -> Vec<AntPosition>  {
    api_parse::<Vec<AntPosition>>("antenna_positions.json")
//     api_get::<Vec<AntPosition>>("imaging/antenna_positions")
}


/************************************* FullDataset ***************************************
* 
* Downloaded using the tart_calibration_data command line utility in tart_tools
*
*/

#[derive(Deserialize, Debug)]
pub struct Source {
    pub az: f64, //"az": 326.26603,
    pub el: f64, //"el": 31.51368,
    pub jy: f64, //"jy": 1500000.0,
    pub name: String, //"name": "MTSAT-2 (MSAS/PRN 137)",
    pub r: f64, //"r": 38458365.1
}

#[derive(Deserialize, Debug)]
pub struct VisSource {
    pub data: VisData, 
    pub sources: Vec<Source>
}

#[derive(Deserialize, Debug)]
pub struct FullDataset {
    pub info: TARTinfo,
    pub ant_pos: Vec<AntPosition>,
    pub gains: Gains,
    pub data: Vec<VisSource>
}


pub fn file_to_dataset(fname: &str) -> FullDataset  {
    api_parse::<FullDataset>(&fname)
}

pub fn json_to_dataset(json: &String) -> FullDataset  {
    api_parse_json::<FullDataset>(&json)
}
