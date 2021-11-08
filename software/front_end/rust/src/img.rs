//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//
use utils::{VectorReal, L1_WAVELENGTH};

fn spatial_frequency(a: f64, b: f64) -> f64 {
    (a - b) / L1_WAVELENGTH
}

pub fn get_uvw( baselines: &Vec<(u32, u32)>, 
                x: &VectorReal, 
                y: &VectorReal,
                z: &VectorReal) -> (VectorReal, VectorReal, VectorReal)
{
    let mut uu_a = Vec::new();
    let mut vv_a = Vec::new();
    let mut ww_a = Vec::new();
    
    for bl in baselines {
        let i = bl.0 as usize;
        let j = bl.1 as usize;
        uu_a.push(spatial_frequency(x[i], x[j]));
        vv_a.push(spatial_frequency(y[i], y[j]));
        ww_a.push(spatial_frequency(z[i], z[j]));
    }
    (   VectorReal::from_vec(uu_a),
        VectorReal::from_vec(vv_a),
        VectorReal::from_vec(ww_a)
    )
}
