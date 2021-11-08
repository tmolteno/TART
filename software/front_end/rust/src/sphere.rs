/*
    Classes to hold pixelated spheres

    Tim Molteno tim@elec.ac.nz 2019-2021
*/



use cdshealpix::ring::{n_hash, center};
use utils::{VectorReal, PI_OVER_2};



/*
*   Elevation and Azimuth (in radians)
*/
#[derive(Debug)]
pub struct ElAz {
    pub el: f64,
    pub az: f64
}


/*
*   A LonLat is the internal structure used by CDS healpix.
*/
#[derive(Debug)]
pub struct LonLat {
    pub lon: f64,
    pub lat: f64
}


/*
*   A HpAngle is the internal structure used by HEALPY
*   phi is a longitude, and 
*   theta is a colatitude (zero at the zenith, pi/2 at the horizon and -pi at the south pole.
*/
#[derive(Debug)]
pub struct HpAngle {
    pub theta: f64, // colatitude
    pub phi: f64
}



impl LonLat {
    pub fn new(lon: f64,  lat: f64) -> LonLat {
        LonLat {
            lon: lon,
            lat: lat
        }
    }
    #[allow(dead_code)]
    pub fn from_hp(hp: &HpAngle) -> LonLat {
        LonLat::new(hp.phi, PI_OVER_2 - hp.theta)
    }
    
    pub fn from_pix(nside: u32, pix: u64) -> LonLat {
        let (lon, lat) = center(nside, pix); 
        LonLat::new(lon, lat)
    }
}


impl HpAngle {
    pub fn new(theta: f64,  phi: f64) -> HpAngle {
        HpAngle {
            theta: theta,
            phi: phi
        }
    }
    
    pub fn from_lonlat(lonlat: &LonLat) -> HpAngle {
        HpAngle::new(PI_OVER_2 - lonlat.lat, lonlat.lon)
    }
    
    pub fn from_elaz(el: f64,  az: f64) -> HpAngle {
        let theta = PI_OVER_2 - el;
        let phi = -az;
        HpAngle::new(theta, phi)
    }
    
    pub fn proj(&self) -> (f64, f64) {
        // viewpoint is from straight up, projected down.
        let r = f64::sin(self.theta);
        
        let x = r*f64::sin(self.phi);
        let y = -r*f64::cos(self.phi);
        
        (x,y)
    }
}

impl ElAz {
    pub fn new(el: f64, az: f64) -> ElAz {
        ElAz {
            el: el,
            az: az
        }
    }
    
    pub fn from_hp(hp: &HpAngle) -> ElAz {
        let el = PI_OVER_2 - hp.theta;
        let az = -hp.phi;
        ElAz::new(el, az)
    }
    
    pub fn to_hp(&self) -> HpAngle {
        HpAngle::from_elaz(self.el, self.az)
    }
    
    pub fn to_lmn(&self) -> (f64, f64, f64) {
        let l = self.az.sin()*self.el.cos();
        let m = self.az.cos()*self.el.cos();
        let n = self.el.sin(); // Often written in this weird way... np.sqrt(1.0 - l**2 - m**2)
        return (l, m, n)
    }
}


pub struct Hemisphere {
    pub nside: u32,
    pub npix: usize,
    pub visible_pix: VectorReal,
    pub visible_indices: Vec::<u64>,
    #[allow(dead_code)]
    elaz: Vec::<ElAz>,
    pub l: VectorReal,
    pub m: VectorReal,
    pub n: VectorReal
}



impl Hemisphere {

    pub fn new(nside: u32) -> Hemisphere {

        let npix = n_hash(nside);
        
        let mut elaz_arr = Vec::new();
        let mut l_arr = Vec::new();
        let mut m_arr = Vec::new();
        let mut n_arr = Vec::new();

        let mut visible_pixels = Vec::new();
        let mut visible_indices = Vec::new();

        for pix in 0..npix {
            let ll = LonLat::from_pix(nside, pix);
            let hp = HpAngle::from_lonlat(&ll);
            
            // Find only the visible pixels (above the horizon)
            if hp.theta < PI_OVER_2 {
                visible_pixels.push(0.0 as f64);
                visible_indices.push(pix);
                
                let elaz = ElAz::from_hp(&hp);
                let (l, m, n) = elaz.to_lmn();
                
                elaz_arr.push(elaz);
                
                l_arr.push(l);
                m_arr.push(m);
                n_arr.push(n);
            }
        }

        Hemisphere {
            nside: nside,
            npix: visible_pixels.len(),
            visible_pix: VectorReal::from_vec(visible_pixels),
            visible_indices: visible_indices,
            elaz: elaz_arr,
            l: VectorReal::from_vec(l_arr),
            m: VectorReal::from_vec(m_arr),
            n: VectorReal::from_vec(n_arr)
        }
    }

}



#[cfg(test)]
mod tests {
    
    use rand::Rng;
    use super::*;

    #[test]
    fn test_nside() {
        let sph = Hemisphere::new(8);
        assert_eq!(sph.nside, 8);
    }

    
    #[test]
    fn test_angles() {
        let mut rng = rand::thread_rng();
        for i in 0..1000 {
            let lon : f64 = rng.gen_range(0.0, PI*2.0);
            let lat : f64 = rng.gen_range(-PI_OVER_2, PI_OVER_2);
            
            let hp = HpAngle::from_lonlat(&LonLat::new(lon, lat));
            
            let ll = LonLat::from_hp(&hp);
            assert!((ll.lon - lon).abs() < 1.0e-12);
            assert!((ll.lat - lat).abs() < 1.0e-12);
        }
    }
    
    #[test]
    fn test_zenith() {
        
        let hp = HpAngle::from_elaz(PI_OVER_2, 0.0);
        assert_eq!(hp.theta, 0.0);

        let (x,y) = hp.proj();
        assert_eq!(x, 0.0);
        assert_eq!(y, 0.0);

        let ll = LonLat::from_hp(&hp);
        assert_eq!(ll.lat, PI_OVER_2);
        
        for log in 2..8 {
            let nside = 2u32.pow(log);            
            let sph = Hemisphere::new(nside);
            // find the zenith pixel
            
            let pix = &sph.get_pix(&hp);
            assert_eq!(pix, &3);
            
            let ll2 = LonLat::from_pix(nside, *pix);
            assert_eq!(ll.lat, ll2.lat);
        }
    }

    #[test]
    fn test_horizon() {
        let sph = Hemisphere::new(4);
        
        let hp = HpAngle::from_elaz(0.0, 0.0);
        assert_eq!(hp.theta, PI_OVER_2);

        let (x,y) = hp.proj();
        assert_eq!(x, 0.0);
        assert_eq!(y, 1.0);

        let ll = LonLat::from_hp(&hp);
        assert_eq!(ll.lat, 0.0);
    }
}
