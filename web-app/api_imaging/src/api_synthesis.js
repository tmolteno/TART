
var zeros = require("zeros")
var ndarray = require("ndarray")
var fft = require("ndarray-fft")
var savePixels = require("save-pixels")
var linspace = require('linspace');

function number_of_bins_lt_x(array, x){
  var ret = 0;
  var arr_len = array.length;
  for(var i=0;i<arr_len;i++){
    if (array[i]<x){
      ret++;
    }
    else{
      break
    }
  }
  return ret
}

function ifftshift(array){
  arr_len = array.shape[0];
  var ret = ndarray(new Float64Array(Math.pow(arr_len,2)), array.shape);
  arr_len2 = arr_len/2;
  for(var i=0; i<arr_len; i++){
    for(var j=0; j<arr_len; j++){
      idx = (i+arr_len2)%arr_len;
      idy = (j+arr_len2)%arr_len;
      ret.set(idx,idy,array.get(i,j));
    };
  };
  return ret;
}

function scale(array){
  var min = 1000;
  var max = -1000;
  for(var i=0; i<array.shape[0]; i++) {
    for(var j=0; j<array.shape[1]; j++) {
      val = array.get(i,j);
      if (min>val){min = val;}
      if (max<val){max = val;}
    }
  }
  for(var i=0; i<array.shape[0]; i++) {
    for(var j=0; j<array.shape[1]; j++) {
      array.set(i,j,((255./max) * (array.get(i,j)-min)));
    }
  }
  return array;
}

function abs(real, imag){
  real_len = real.shape[0];
  var ret = ndarray(new Float64Array(Math.pow(real_len,2)), real.shape);
  for(var i=0; i<real.shape[0]; i++) {
    for(var j=0; j<real.shape[1]; j++) {
      ret.set(i,j, Math.sqrt(Math.pow(real.get(i,j),2) + Math.pow(imag.get(i,j),2))    );
    }
  }
  return ret;
}


var gen_image = function(vis, ant_pos, calib, nw, num_bin){

    var L1_WAVELENGTH = 0.1905; // meter
    var uu_a = [];
    var vv_a = [];
    var ww_a = [];
    var vis_l = [];
    for(key in vis){
      var a = eval(key)
      uu_a.push((ant_pos[a[0]][0] - ant_pos[a[1]][0])/L1_WAVELENGTH);
      vv_a.push((ant_pos[a[0]][1] - ant_pos[a[1]][1])/L1_WAVELENGTH);
      ww_a.push((ant_pos[a[0]][2] - ant_pos[a[1]][2])/L1_WAVELENGTH);
      var gain_corr_real_part =   calib.gain[a[0]] * calib.gain[a[1]] * Math.cos(calib.phase_offset[a[0]] - calib.phase_offset[a[1]]);
      var gain_corr_imag_part = - calib.gain[a[0]] * calib.gain[a[1]] * Math.sin(calib.phase_offset[a[0]] - calib.phase_offset[a[1]]);
      var vis_cal_real = vis[key][0] * gain_corr_real_part;
      var vis_cal_imag = vis[key][1] * gain_corr_imag_part;
      vis_l.push([vis_cal_real, vis_cal_imag]);
    }

    var uu_edges = linspace(-nw, nw, num_bin+1);
    var vv_edges = linspace(-nw, nw, num_bin+1);
    var grid_idxs = [];
    for(var k=0; k<uu_a.length;k++){
      var uu = uu_a[k];
      var vv = vv_a[k];
      i = number_of_bins_lt_x(uu_edges,uu)-1;
      j = number_of_bins_lt_x(vv_edges,vv)-1;
      i2 = number_of_bins_lt_x(uu_edges,-uu)-1;
      j2 = number_of_bins_lt_x(vv_edges,-vv)-1;
      grid_idxs.push([i,j,i2,j2])
    }

    var real_part = zeros([num_bin, num_bin]);
    var imag_part = zeros([num_bin, num_bin]);

    for(var i=0; i<grid_idxs.length;i++){
      var idx =grid_idxs[i];
      real_part.set(idx[0],idx[1],vis_l[i][0]);
      imag_part.set(idx[0],idx[1],vis_l[i][1]);
      real_part.set(idx[2],idx[3],vis_l[i][0]);
      imag_part.set(idx[2],idx[3],-vis_l[i][1]);
    }

    var shifted_real_part = ifftshift(real_part);
    var shifted_imag_part = ifftshift(imag_part);

    //Invert transform
    fft(1, shifted_real_part, shifted_imag_part)

    real_part = ifftshift(shifted_real_part);
    imag_part = ifftshift(shifted_imag_part);

    // Absolute value from real and imag part
    SAbs = abs(real_part,imag_part);

    SAbs_scaled = scale(SAbs);

//     console.log(SAbs_scaled);
    return savePixels(SAbs_scaled, 'CANVAS');
};


module.exports = {
  gen_image: gen_image
}
