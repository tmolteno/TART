import { Component, ElementRef, ViewChild } from '@angular/core';

@Component({
    selector: 'app-imaging',
    templateUrl: './imaging.component.html',
    styleUrls: ['./imaging.component.css']
})
export class ImagingComponent {
    //@ViewChild('imagingCanvas') imagingCanvas: ElementRef;

    canvasSizeModifier = 0.8;
    defaultBins: number = 7;
    defaultWaves: number = 36;
    defaultRefreshTime: number = 5;
    canvasSize: number = 0;
    isHeightBigger: boolean = false;

    constructor() { }

    ngOnInit() {
        this.updateImageSize();
    }

    updateImageSize() {
        this.isHeightBigger = window.innerHeight > window.innerWidth;
        let baseSize = Math.min(window.innerHeight, window.innerWidth);
        this.canvasSize = Math.floor(baseSize * this.canvasSizeModifier);
    }

    ngAfterViewInit() {
        console.log("afterViewInit called")
        //this.ctx = this.imagingCanvas.nativeElement.getContext('2d');
        //this.drawImage();
    }

    drawImage() {
        // call draw jpg code, then display jpg in window
        console.log("drawing visibles");
    }
}
