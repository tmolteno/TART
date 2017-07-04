const visImaging = require('vis_imaging/src/api_synthesis');
import { Component, ElementRef, ViewChild } from '@angular/core';
import { ImagingService } from '../../services/imaging.service';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/forkJoin';

@Component({
    selector: 'app-imaging',
    templateUrl: './imaging.component.html',
    styleUrls: ['./imaging.component.css']
})
export class ImagingComponent {
    @ViewChild('imagingCanvas') imagingCanvas: ElementRef;

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
        //this.imagingCanvas.nativeElement.width = 50; // TODO: calcualate size of canvas
        //this.imagingCanvas.nativeElement.height = 50;
        this.drawImage();
    }

    drawImage() {
        Observable.forkJoin([
            this.imagingService.getVis(),
            this.imagingService.getAntennaPositions()
        ]).subscribe(result => {

            let visData = result[0];
            let antennaPos = result[1];
            let genImg = visImaging.gen_image(visData, antennaPos, 36, 2**7);

            let img = new Image();
            img.onload = () => {
                let ctx = this.imagingCanvas.nativeElement.getContext('2d');
                let widthScale = this.imagingCanvas.nativeElement.width / genImg.width;
                let heightScale = this.imagingCanvas.nativeElement.height / genImg.height;
                ctx.scale(widthScale, heightScale);
                ctx.drawImage(img, 0, 0);
            };
            img.src = genImg.toDataURL();
        });
        // call draw jpg code, then display jpg in window
        // TODO: more important to redraw current data than get new data
    }

    onNumBinsChanged(value) {
        console.log("updated num bins");
    }

    onNumWavesChanged(value) {
        console.log("updated num waves");
    }
}
