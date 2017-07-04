

var api_synthesis = require('../src/api_synthesis.js');
// TODO: generage image from synthesis, which should return a
// TODO: canvas element, then get image data from that and inject in my-canvas (if possible)



var vis = require('./fake_vis');
var ant_pos = require('./fake_ant_pos');
var L1_WAVELENGTH = 0.1905; // meter

var nw = 30; // Number of wavelengths
var num_bin = 2**9; // bins in the fft

window.onload = function() {
	var dummyCanvas = api_synthesis.gen_image(vis, ant_pos, nw, num_bin);
        console.log(dummyCanvas);
	var dummyImageData = dummyCanvas.getContext('2d').getImageData(0,0,dummyCanvas.width,dummyCanvas.height);
        //console.log(dummyImageData.data);	
        var myCanvas = document.getElementById('my-canvas');
        var ctx = myCanvas.getContext('2d');
	var imgData=ctx.createImageData(100,100);
	for (var i=0;i<imgData.data.length;i+=4)
	  {
		  imgData.data[i+0]=255;
		  imgData.data[i+1]=0;
		  imgData.data[i+2]=0;
		  imgData.data[i+3]=255;
	  }
	ctx.putImageData(dummyImageData, 0, 0);
	ctx.putImageData(imgData,10,10);
        var canvasTarget = document.getElementById('append-target');
        canvasTarget.appendChild(dummyCanvas);

}

