import { Component, ElementRef, ViewChild } from '@angular/core';
import { ImagingService } from '../../services/imaging.service';
const thing = require('vis_imaging/src/api_synthesis');
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/forkJoin';

@Component({
    selector: 'app-imaging',
    templateUrl: './imaging.component.html',
    styleUrls: ['./imaging.component.css']
})
export class ImagingComponent {
    @ViewChild('my-canvas') imagingCanvas: ElementRef;

    canvasSizeModifier = 0.8;
    // number of bins picker settings
    numBins: number = 7;
    minBins: number = 5;
    maxBins: number = 12;
    // number of waves settings
    defaultWaves: number = 36;
    //refresh time settings
    defaultRefreshTime: number = 5;
    canvasSize: number = 0;
    isHeightBigger: boolean = false;

    constructor(private imagingService: ImagingService) { }

    ngOnInit() {
        this.updateImageSize();
    }

    updateImageSize() {
        this.isHeightBigger = window.innerHeight > window.innerWidth;
        let baseSize = Math.min(window.innerHeight, window.innerWidth);
        this.canvasSize = Math.floor(baseSize * this.canvasSizeModifier);
    }

    ngAfterViewInit() {
        
        console.log("afterViewInit called");
        console.log(thing.hi);
        this.drawImage();
    }

    drawImage() {
        //let visData = ;
        //let antennaPos = ;
        Observable.forkJoin([
                this.imagingService.getVis(),
                this.imagingService.getAntennaPositions()
            ]).subscribe(result => {
                let visData = result[0];
                let antennaPos = result[1];
                console.log("result: " + JSON.stringify(result));
                let genImg = thing.gen_image(visData, antennaPos, 36, 2**7);
                let genImgData = genImg.getContext('2d').getImageData(0, 0, genImg.width, genImg.height);
                var ctx = this.imagingCanvas.nativeElement.getContext('2d');
                ctx.putImageData(genImgData, 0, 0);
            });
        // call draw jpg code, then display jpg in window
        // TODO: more important to redraw current data than get new data
        console.log("drawing visibles");
    }

    onNumBinsChanged(value) {
        console.log("updated num bins");
    }

    onNumWavesChanged(value) {
        console.log("updated num waves");
    }
}
