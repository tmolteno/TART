//
// Copyright (c) 2019-2021 Tim Molteno tim@elec.ac.nz
//



use ndarray::{Array1};

pub type C64 = num::complex::Complex<f64>;

pub type VectorReal = Array1::<f64>;

pub type VectorComplex = Array1::<C64>;

pub const C: f64  = 2.99793e8; 
pub const L1_FREQUENCY: f64 = 1.57542e9; 
pub const L1_WAVELENGTH: f64 = C/L1_FREQUENCY;

pub const PI: f64 = std::f64::consts::PI;
pub const PI_OVER_2: f64 = PI/2.0;



#[allow(dead_code)]
pub fn mean(data: &[f64]) -> Option<f64> {

    let sum = data.iter().sum::<f64>() as f64;
    let count = data.len();

    let mean = match count {
       positive if positive > 0 => Some(sum  / count as f64),
       _ => None
    };
    mean
}

use std::cmp::Ordering;

fn partition(data: &[f64]) -> Option<(Vec<f64>, f64, Vec<f64>)> {
    match data.len() {
        0 => None,
        _ => {
            let (pivot_slice, tail) = data.split_at(1);
            let pivot = pivot_slice[0];
            let (left, right) = tail.iter()
                .fold((vec![], vec![]), |mut splits, next| {
                    {
                        let (ref mut left, ref mut right) = &mut splits;
                        if next < &pivot {
                            left.push(*next);
                        } else {
                            right.push(*next);
                        }
                    }
                    splits
                });

            Some((left, pivot, right))
        }
    }
}

fn select(data: &[f64], k: usize) -> Option<f64> {
    let part = partition(data);

    match part {
        None => None,
        Some((left, pivot, right)) => {
            let pivot_idx = left.len();

            match pivot_idx.cmp(&k) {
                Ordering::Equal => Some(pivot),
                Ordering::Greater => select(&left, k),
                Ordering::Less => select(&right, k - (pivot_idx + 1)),
            }
        },
    }
}

pub fn median(data: &[f64]) -> Option<f64> {
    let size = data.len();

    match size {
        even if even % 2 == 0 => {
            let fst_med = select(data, (even / 2) - 1);
            let snd_med = select(data, even / 2);

            match (fst_med, snd_med) {
                (Some(fst), Some(snd)) => Some((fst + snd) as f64 / 2.0),
                _ => None
            }
        },
        odd => select(data, odd / 2).map(|x| x as f64)
    }
}
