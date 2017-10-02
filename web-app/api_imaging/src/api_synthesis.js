
var zeros = require("zeros")
var ndarray = require("ndarray")
var ops = require('ndarray-ops')
var fft = require("ndarray-fft")
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

function flip_ud(arr) {
  var flipped_arr = arr.step(-1,1);
  return flipped_arr;
}


var gen_image = function(vis, ant_pos, calib, nw, num_bin, colourmap){

    var L1_WAVELENGTH = 0.1905; // meter
    var uu_a = [];
    var vv_a = [];
    var ww_a = [];
    var vis_l = [];
    for(var i=0; i<vis.data.length; i++){
      var v = vis.data[i];
      var a = [v.i,v.j];
      uu_a.push((ant_pos[a[0]][0] - ant_pos[a[1]][0])/L1_WAVELENGTH);
      vv_a.push((ant_pos[a[0]][1] - ant_pos[a[1]][1])/L1_WAVELENGTH);
      ww_a.push((ant_pos[a[0]][2] - ant_pos[a[1]][2])/L1_WAVELENGTH);
      var gain_corr_real_part =   calib.gain[a[0]] * calib.gain[a[1]] * Math.cos(calib.phase_offset[a[0]] - calib.phase_offset[a[1]]);
      var gain_corr_imag_part = - calib.gain[a[0]] * calib.gain[a[1]] * Math.sin(calib.phase_offset[a[0]] - calib.phase_offset[a[1]]);
      var vis_cal_real = v.re * gain_corr_real_part - v.im*gain_corr_imag_part;
      var vis_cal_imag = v.re * gain_corr_imag_part + v.im*gain_corr_real_part;
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

    var SAbs_scaled = scale(SAbs);

    var SAbs_scaled_flipped = flip_ud(SAbs_scaled);
    var colourmap = colourmap || 'viridis';
    canvas = apply_colormap(SAbs_scaled_flipped, colourmap);
    return canvas;
};


function handleData(array, data, frame) {
  if (array.shape.length === 2) {
    ops.assign(
      ndarray(data,
              [array.shape[0], array.shape[1], 3],
              [4, 4 * array.shape[0], 1]),
               ndarray(array.data,
                       [array.shape[0], array.shape[1], 3],
                       [array.stride[0], array.stride[1], 0],
                       array.offset))
    ops.assigns(
      ndarray(data,
              [array.shape[0] * array.shape[1]],
              [4],
              3),
              255)
  } else {
    return new Error('Incompatible array shape')
  }
  return data
}

function gen_canvas(array){
  var canvas = document.createElement('canvas')
  var context = canvas.getContext('2d')
  canvas.width = array.shape[0]
  canvas.height = array.shape[1]
  var imageData = context.getImageData(0, 0, canvas.width, canvas.height)
  var data = imageData.data
  data = handleData(array, data)
  if (typeof data === 'Error') return haderror(data)
    context.putImageData(imageData, 0, 0)
    return canvas
}

function apply_colormap(ndarray, colourmap){
  var cm = require("colormap")({
    colormap: colourmap,   // pick a builtin colormap or add your own
    nshades: 256,       // how many divisions
    format: 'rgb',     // "hex" or "rgb" or "rgbaString"
      alpha: 0           // set an alpha value or a linear alpha mapping [start, end]
  })
  var canvas = gen_canvas(ndarray);
  var ImageData = canvas.getContext('2d').getImageData(0,0,canvas.width,canvas.height)
  for(var i=0; i<ImageData.data.length; i=i+4) {
    var gray_c = ImageData.data[i]
    ImageData.data[i+0] = cm[gray_c][0];
    ImageData.data[i+1] = cm[gray_c][1];
    ImageData.data[i+2] = cm[gray_c][2];
  }
  canvas.getContext('2d').putImageData(ImageData, 0, 0);
  return canvas
}

function get_max_ang(nw, num_bin){
  var max_ang = (num_bin/(4*nw) * 180/Math.PI);
  return max_ang;
}


function get_bin_width(nw, num_bin) {
  var max_ang = get_max_ang(nw, num_bin);
  return num_bin/(2.*max_ang)
}


/** Convert angle to pixels **/
function ang_2_px(ang_deg, nw, num_bin){
  var dpx = ang_deg * get_bin_width(nw, num_bin);
  return Math.floor(dpx);
}

/** Convert degrees to radians **/
function deg2rad(deg) {
    return deg*Math.PI/180;
}

/** Get the x-component in the image coordinates **/
function get_l(el_rad, az_rad) {
    return -Math.sin(az_rad)*Math.cos(el_rad);
}

/** Get the y-component in the image coordinates **/
function get_m(el_rad, az_rad) {
    return  Math.cos(az_rad)*Math.cos(el_rad);
}

function horizontal_2_px(el, az, nw, num_bin){
    var az_rad = deg2rad(az);
    var el_rad = deg2rad(el);

    var l = get_l(el_rad, az_rad);
    var m = get_m(el_rad, az_rad);

    var n2 = num_bin/2;
    var x_px = Math.round(n2 - l * n2);
    var y_px = Math.round(n2 - m * n2);

    return {x:x_px, y:y_px};
}



function proj_ang_2_px(ang_deg, nw, num_bin){
    var az_rad = deg2rad(0.0);
    var el_rad = deg2rad(90.0 - ang_deg);

    var l = get_l(el_rad, az_rad);
    var m = get_m(el_rad, az_rad);

    var n2 = num_bin/2;
    var dx = (l * n2);
    var dy = (m * n2);

    var r_px = Math.sqrt(dx*dx + dy*dy);

    return r_px
}

function setFont(ctx, num_bin) {
    var pix = Math.round(num_bin / 44);
    ctx.font = '' + pix + 'px sans';
}

function draw_src(ctx, el, az, label, nw, num_bin, show_name) {
    var pos = horizontal_2_px(el, az, nw, num_bin)
    var r_dot = ang_2_px(2.0, nw, num_bin);
    ctx.beginPath();
    ctx.strokeStyle = 'rgba(255, 0, 0, 0.5)';
    ctx.fillStyle = 'rgba(255, 255, 0, 0.125)';
    ctx.arc(pos.x, pos.y, r_dot, 0, 2 * Math.PI);
    //ctx.fill()
    ctx.stroke();
    var show_name = show_name !== undefined ? show_name : 1;
    if (show_name){
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.strokeStyle = 'rgba(255, 255, 255, 0.5)';
        ctx.fillStyle = 'rgba(255, 255, 255, 0.5)';
        var shortLabel = (label.split('(')[0]).trim();
        ctx.fillText(shortLabel, pos.x, pos.y+2*r_dot);
    }
}



function draw_circ(ctx, ang, nw, num_bin) {
    var r = proj_ang_2_px(ang, nw, num_bin);
    var x_0 = num_bin/2. //+ proj_ang_2_px(0, nw, num_bin);
    var y_0 = num_bin/2. //- proj_ang_2_px(0, nw, num_bin);
    ctx.beginPath();
    ctx.arc(x_0, y_0, r, 0, 2 * Math.PI);
    ctx.stroke();
}

function overlay_grid(ctx, nw, num_bin){
    setFont(ctx, num_bin);
    ctx.beginPath();
    ctx.lineWidth = Math.ceil(num_bin/500);
    ctx.imageSmoothingEnabled = true;
    ctx.moveTo(num_bin/2, 0);
    ctx.lineTo(num_bin/2, num_bin);
    ctx.moveTo(0,num_bin/2);
    ctx.lineTo(num_bin, num_bin/2);
    ctx.stroke();
    var const_el = [80,70,60,50,40,30,20,10,0];
    for (i=0; i< const_el.length;i++){
        draw_circ(ctx, 90-const_el[i],nw,num_bin)
    }
    var pos_n = horizontal_2_px(80  , 0, nw, num_bin)
    var pos_e = horizontal_2_px(80 , 90, nw, num_bin)
    var pos_s = horizontal_2_px(80, 180, nw, num_bin)
    var pos_w = horizontal_2_px(80, 270, nw, num_bin)
    ctx.fillStyle = 'white';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText('N', pos_n.x, pos_n.y);
    ctx.fillText('E', pos_e.x, pos_e.y);
    ctx.fillText('S', pos_s.x, pos_s.y);
    ctx.fillText('W', pos_w.x, pos_w.y);
}


function overlay_satellites(ctx, sat_list, nw, num_bin, show_name){
    setFont(ctx, num_bin);
    ctx.imageSmoothingEnabled = true;
    for (i in sat_list){
        var s = sat_list[i];
        draw_src(ctx, s.el, s.az, s.name, nw, num_bin, show_name);
    }
}



module.exports = {
  gen_image: gen_image,
  overlay_satellites: overlay_satellites,
  overlay_grid: overlay_grid
}
