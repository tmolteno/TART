
var api_synthesis = require('../src/api_synthesis.js');
var fake_data = require('./fake_data')
var vis = fake_data.vis;
var ant_pos = fake_data.ant_pos;
var calib = fake_data.calib;
var sat = fake_data.sat;

var nw = 60; // Number of wavelengths
var num_bin = Math.pow(2,9); // bins in the fft


window.onload = function() {
  var dummyCanvas = api_synthesis.gen_image(vis, ant_pos, calib, nw, num_bin,'magma');
  var dummyImageData = dummyCanvas.getContext('2d').getImageData(0,0,dummyCanvas.width,dummyCanvas.height);
  var myCanvas = document.getElementById('my-canvas');
  var ctx = myCanvas.getContext('2d');
  ctx.putImageData(dummyImageData, 0, 0);
  api_synthesis.overlay_grid(ctx, nw, num_bin);
  api_synthesis.overlay_satellites(ctx, sat, nw, num_bin, 1);

}

