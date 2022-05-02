//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//

use tart_api;
use tart_api::{Gains, VisData, Source, TARTinfo, AntPosition, FullDataset};
use utils::{VectorReal, VectorComplex, C64};
use chrono::{DateTime, Utc};

pub struct Observation {
    pub timestamp: DateTime<Utc>,
    pub vis_arr: VectorComplex,
    pub ant_x: VectorReal,
    pub ant_y: VectorReal,
    pub ant_z: VectorReal,
    pub baselines: Vec<(u32, u32)>
}



impl Observation {
    pub fn new(cal_data: &Gains, 
                vis: &VisData,
                _info: &TARTinfo, 
                ant_positions: &Vec<AntPosition>) -> Observation {
        
        let rfc3339 = DateTime::parse_from_rfc3339(&vis.timestamp).expect("Couldn't parse timestamp");
        println!("{}", rfc3339);

        let num_antenna = ant_positions.len();
        
        let mut vis_vec = Vec::<C64>::new();
        let mut baselines = Vec::new();
        let mut ant_x = Vec::new();
        let mut ant_y = Vec::new();
        let mut ant_z = Vec::new();
        
        for i in 0..num_antenna {
            ant_x.push(ant_positions[i].x);
            ant_y.push(ant_positions[i].y);
            ant_z.push(ant_positions[i].z);
        }

        for v in &vis.data {
            vis_vec.push(C64::new(v.re, v.im));
            baselines.push((v.i as u32,v.j as u32));
        }

        let cal_vis = apply_gains(&baselines, &vis_vec, &cal_data);
        
        Observation {
            timestamp: rfc3339.with_timezone(&Utc),
            ant_x: VectorReal::from_vec(ant_x),
            ant_y: VectorReal::from_vec(ant_y),
            ant_z: VectorReal::from_vec(ant_z),
            vis_arr: VectorComplex::from_vec(cal_vis),
            baselines: baselines
        }
    }

}


pub fn apply_gains(
                baselines: &Vec::<(u32, u32)>,
                vis_arr: &Vec::<C64>,
                cal: &tart_api::Gains) -> Vec<C64> 
{
    let mut cal_vis = Vec::<C64>::new();
    
    for k in 0..baselines.len() {
        let i = baselines[k].0 as usize;
        let j = baselines[k].1 as usize;

        let theta = -C64::new(0.0, cal.phase_offset[i] - cal.phase_offset[j]);
        let v = vis_arr[k] * cal.gain[i]*cal.gain[j] * theta.exp() ;
        
        cal_vis.push(v);
    }
    
    cal_vis
}


pub fn get_sources(data: &FullDataset) -> &Vec<Source> {
    &data.data[0].sources
}

    
pub fn get_full(data: &FullDataset) -> Observation {
    let cal_data = &data.gains;
    let vis = &data.data[0].data;
    let info = &data.info;
    let ant_positions = &data.ant_pos;

    Observation::new(&cal_data, &vis, &info, &ant_positions)
}


#[allow(dead_code)]
pub fn get() -> Observation {
    let cal_data = tart_api::gains();
    let vis = tart_api::visibilities();
    let info = tart_api::info();
    let ant_positions = tart_api::ant_positions();
    
    Observation::new(&cal_data, &vis, &info, &ant_positions)
}
