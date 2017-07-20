
var api_synthesis = require('../src/api_synthesis.js');
var fake_data = require('./fake_data')
var vis = fake_data.vis;
var ant_pos = fake_data.ant_pos;
var calib = fake_data.calib;
var sat = fake_data.sat;

var nw = 60; // Number of wavelengths
var num_bin = Math.pow(2,9); // bins in the fft


function get_max_ang(nw, num_bin){
  var max_ang = (num_bin/(4*nw) * 180/Math.PI);
//   console.log(max_ang)
  return max_ang;
}

function ang_2_pos(ang, nw, num_bin, ){
  var max_ang = get_max_ang(nw, num_bin);
  //   -max_ang  0
  //   0         num_bin/2
  //   max_ang   num_bin
  return Math.floor((ang + max_ang)/(2*max_ang) * num_bin)
}

function ang_2_px(ang, nw, num_bin, ){
  var max_ang = get_max_ang(nw, num_bin);
  //   -max_ang  0
  //   0         num_bin/2
  //   max_ang   num_bin
  return Math.floor(ang * (num_bin)/(2*max_ang))
}

function draw_circ(ctx, ang, nw, num_bin) {
  var r = ang_2_px(ang, nw, num_bin);
  var x_0 = ang_2_pos(0, nw, num_bin);
  var y_0 = ang_2_pos(0, nw, num_bin);
  ctx.beginPath();
  ctx.arc(x_0, y_0, r, 0, 2 * Math.PI);
  ctx.stroke();
}

function draw_src(ctx, el, az, label, nw, num_bin) {
  var max_ang = get_max_ang(nw, num_bin);
  var r = ang_2_px(2, nw, num_bin);
  var x_s = ang_2_pos((90-el)*Math.sin(az*Math.PI/180), nw, num_bin);
  var y_s = num_bin-ang_2_pos((90-el)*Math.cos(az*Math.PI/180), nw, num_bin);
  ctx.beginPath();
  ctx.fillStyle = 'white';
  ctx.arc(x_s, y_s, r, 0, 2 * Math.PI);
  ctx.fill()
  ctx.stroke();
  ctx.font = 'Bold 14px sans';
  ctx.fillText(label, x_s+r+1, y_s+r+1);
}

function overlay_grid(ctx,nw, num_bin){
  ctx.beginPath();
  ctx.lineWidth = 2;
  ctx.moveTo(num_bin/2, 0);
  ctx.lineTo(num_bin/2, num_bin);
  ctx.moveTo(0,num_bin/2);
  ctx.lineTo(num_bin, num_bin/2);
  ctx.stroke();
  var const_el = [80,70,60,50,40,30,0];
  for (i=0; i< const_el.length;i++){
    draw_circ(ctx, 90-const_el[i],nw,num_bin)
  }
}

function overlay_satellites(ctx, sat_list, nw, num_bin, ){
  for (i in sat_list){
    var s = sat_list[i];
    draw_src(ctx, s.el, s.az, s.PRN, nw, num_bin);
  }
}

window.onload = function() {
  var dummyCanvas = api_synthesis.gen_image(vis, ant_pos, calib, nw, num_bin,'electric');
  var dummyImageData = dummyCanvas.getContext('2d').getImageData(0,0,dummyCanvas.width,dummyCanvas.height);
  var myCanvas = document.getElementById('my-canvas');
  var ctx = myCanvas.getContext('2d');
  ctx.putImageData(dummyImageData, 0, 0);
  overlay_grid(ctx,nw, num_bin);
  overlay_satellites(ctx,sat, nw, num_bin);

}

