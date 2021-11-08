//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//
use utils::{VectorReal, VectorComplex, C64, PI};
use sphere::Hemisphere;

use ndarray::{Ix1};

pub fn get_harmonics(
            sky: &Hemisphere,
            u_arr: &VectorReal, 
            v_arr: &VectorReal, 
            w_arr: &VectorReal) -> Vec::<VectorComplex> {
                        
    let mut harmonics = Vec::new();
    
    let n_arr_minus_1 = &sky.n - 1.0;

    let p2j = C64::new(0.0, 2.0*PI);
    
    for i in 0..u_arr.len() {
        let u = u_arr[i];
        let v = v_arr[i];
        let w = w_arr[i];
        
        let theta = u*&sky.l + v*&sky.m + w*&n_arr_minus_1;
        let harmonic = theta.mapv(|x| (-p2j*x).exp() / (sky.npix as f64).sqrt());
        harmonics.push(harmonic);
    }

    return harmonics
}


pub fn image_visibilities( 
            vis: &VectorComplex,
            u: &VectorReal,
            v: &VectorReal,
            w: &VectorReal,
            sky: &mut Hemisphere,
            real_only: bool)
{
    let n_s = &sky.visible_pix.len();

    let mut pixels = VectorComplex::zeros(Ix1(*n_s));

    let harmonics = get_harmonics(sky, u, v, w);
    
    for i in 0..vis.len() {
        pixels = pixels + vis[i]*&harmonics[i];
    }
    
    if real_only {
        sky.visible_pix = pixels.mapv(|p| p.re);
    } else { 
        sky.visible_pix = pixels.mapv(|p| p.norm());
    }
}
