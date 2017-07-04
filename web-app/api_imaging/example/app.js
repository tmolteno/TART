

var api_synthesis = require('../src/api_synthesis.js');
// TODO: generage image from synthesis, which should return a
// TODO: canvas element, then get image data from that and inject in my-canvas (if possible)



var vis = require('./fake_vis');
var ant_pos = require('./fake_ant_pos');
var L1_WAVELENGTH = 0.1905; // meter

var nw = 30; // Number of wavelengths
var num_bin = 2**9; // bins in the fft

window.onload = function() {
	var myCanvas = document.getElementById('my-canvas')
	var dummyCanvas = api_synthesis.gen_image(vis, ant_pos, nw, num_bin)
	var dummyImageData = dummyCanvas.getContext('2d').getImageData(0,0,dummyCanvas.width,dummyCanvas.height)
	myCanvas.getContext('2d').putImageData(dummyImageData, 0, 0);
}

