
// npm init
// npm install zeros ndarray-fft ndarray-ops save-pixels fs linspace --save

var axios = require('axios');
var zeros = require("zeros")
var ndarray = require("ndarray")
var ops = require("ndarray-ops")
var fft = require("ndarray-fft")
var savePixels = require("save-pixels")
var linspace = require('linspace');


var api_endpoint = "http://tart2-raspberry/api/v1"

function api_get_vis(){
  return axios.get(api_endpoint+'/imaging/vis');
}

function api_get_antenna_positions(){
  return axios.get(api_endpoint+'/imaging/antenna_positions');
}


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

function ifftshift(array,num_bin){
  var ret = ndarray(new Float64Array(num_bin**2), array.shape);
  arr_len = array.shape[0];
//   console.log('arrlen', arr_len);
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
  return array
}

function abs(real, imag,num_bin){
  var ret = ndarray(new Float64Array(num_bin**2), real.shape);
  for(var i=0; i<real.shape[0]; i++) {
    for(var j=0; j<real.shape[1]; j++) {
      ret.set(i,j, (real.get(i,j)**2 + imag.get(i,j)**2)**(0.5)    );
    }
  }
  return ret
}


// var vis = null;
// var ant_pos = null
// var L1_WAVELENGTH = 0.1905; // meter
//
// var nw = 30; // Number of wavelengths
// var num_bin = 2**9; // bins in the fft

// api_get_vis()
// .then(function(data) {
//   // do something with data
//   console.log(Object.keys(data));
//   vis = data.data;
//   return api_get_antenna_positions();
// })
// .then(function(more_data) {
//   ant_pos = more_data.data;
//   console.log(Object.keys(more_data));
//   gen_image_zenith(vis, ant_pos, nw, num_bin, null,'CANVAS')
// })
// .catch(function(error){
//   console.log(error.stack);
// })


function gen_image_zenith(vis,ant_pos,nw, num_bin, out,out_format){
          var uu_a = [];
          var vv_a = [];
          var ww_a = [];
          var vis_l = [];
          for(key in vis){
            var a = eval(key)
            uu_a.push((ant_pos[a[0]][0] - ant_pos[a[1]][0])/L1_WAVELENGTH);
            vv_a.push((ant_pos[a[0]][1] - ant_pos[a[1]][1])/L1_WAVELENGTH);
            ww_a.push((ant_pos[a[0]][2] - ant_pos[a[1]][2])/L1_WAVELENGTH);
            vis_l.push(vis[key]);
          }

          var uu_edges = linspace(-nw, nw, num_bin+1);
          console.log(uu_edges);
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

          var shifted_real_part = ifftshift(real_part,num_bin);
          var shifted_imag_part = ifftshift(imag_part,num_bin);

          //Invert transform
          fft(1, shifted_real_part, shifted_imag_part)

          real_part = ifftshift(shifted_real_part,num_bin);
          imag_part = ifftshift(shifted_imag_part,num_bin);

          // Absolute value from real and imag part
          SAbs = abs(real_part,imag_part, num_bin);

          SAbs_scaled = scale(SAbs);

          console.log(SAbs_scaled);
          return savePixels(SAbs_scaled, out_format);


        };
//         )
//       );
// };


function gen_image(vis,ant_pos,nw, num_bin){
  return gen_image_zenith(vis,ant_pos,nw, num_bin,null, 'CANVAS')
}


module.exports = {
  gen_image: gen_image
}


