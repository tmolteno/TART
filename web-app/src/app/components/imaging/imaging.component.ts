const visImaging = require('vis_imaging/src/api_synthesis'); // I would use import, but it cannot find this.
import { Component, ElementRef, ViewChild, HostListener } from '@angular/core';
import { ImagingService } from '../../services/imaging.service';
import { Observable } from 'rxjs/Observable';
import { Subject } from 'rxjs/Subject';
import 'rxjs/add/observable/forkJoin';

@Component({
    selector: 'app-imaging',
    templateUrl: './imaging.component.html',
    styleUrls: ['./imaging.component.css'],
    host: {
        '(window:resize)': 'onResize($event)'
    }
})
export class ImagingComponent {
    @ViewChild('imagingCanvas') imagingCanvas: ElementRef;

    canvasSizeModifierLandscape: number = 0.6;
    canvasSizeModifierPortrait: number = 0.8;
    // number of bins picker settings
    numBins: number = 9;
    minNumBins: number = 5;
    maxNumBins: number = 10;
    numBinsStep: number = 1;
    numBinsLabel: string = "Number of bins (2**n)";
    // number of waves settings
    waves: number = 36;
    minWaves: number = 10;
    maxWaves: number = 100;
    wavesStep: number = 1;
    wavesLabel: string = "UV-Plane Extend [# wavelengths]"
    // refresh time settings
    refreshTime: number = 5;
    minRefreshTime: number = 5;
    maxRefreshTime: number = 60;
    refreshTimeStep: number = 5;
    refreshTimeLabel: string = "Refresh timer (seconds)";

    // flag to block refresh image update
    blockRefresh: boolean = false;
    // timer
    updateImageTimer: any;
    timerSubscription: any;

    constructor(private imagingService: ImagingService) { }

    ngOnInit() {
        this.setCanvasSize();
    }

    setCanvasSize() {
        let baseSize = 0;
        let viewWidth = window.innerWidth;
        let viewHeight = window.innerHeight;
        if (viewWidth >= viewHeight) {
            baseSize = viewHeight * this.canvasSizeModifierLandscape;
        } else {
            baseSize = viewWidth * this.canvasSizeModifierPortrait;
        }
        this.imagingCanvas.nativeElement.width = baseSize;
        this.imagingCanvas.nativeElement.height = baseSize;
    }

    ngAfterViewInit() {
        this.startUpdateImageTimer();
    }

    startUpdateImageTimer() {
        this.updateImageTimer = Observable.timer(0, this.refreshTime * 1000);
        this.timerSubscription = this.updateImageTimer
            .subscribe(tick => this.onRefreshTimerTick(tick));
    }

    ngOnDestroy() {
        this.timerSubscription.unsubscribe();
    }

    onRefreshTimerTick(tick) {
        if (!this.blockRefresh) {
            this.drawImage();
        }
    }

    drawImage() {
        this.blockRefresh = true;
        Observable.forkJoin([
            this.imagingService.getVis(),
            this.imagingService.getAntennaPositions()
        ]).subscribe(result => {

            let visData = result[0];
            let antennaPos = result[1];
            let genImg = visImaging.gen_image(visData, antennaPos, this.waves,
                Math.pow(2, this.numBins));

            let img = new Image();
            img.onload = () => {
                let ctx = this.imagingCanvas.nativeElement.getContext('2d');
                let widthScale = this.imagingCanvas.nativeElement.width / genImg.width;
                let heightScale = this.imagingCanvas.nativeElement.height / genImg.height;
                ctx.save();
                ctx.scale(widthScale, heightScale);
                ctx.drawImage(img, 0, 0);
                ctx.restore();
                this.blockRefresh = false;
            };
            img.src = genImg.toDataURL();
        }, err => {
            this.blockRefresh = false;
        });
        // call draw jpg code, then display jpg in window
        // TODO: more important to redraw current data than get new data
    }

    onNumBinsChanged(value) {
        if (value !== this.numBins) {
            this.numBins = value;
            this.drawImage();
        }
    }

    onNumWavesChanged(value) {
        if (value !== this.waves) {
            this.waves = value;
            this.drawImage();
        }
    }

    onRefreshTimerChanged(value) {
        if (value !== this.refreshTime) {
            this.updateImageTimer.unsubscribe();
            this.refreshTime = value;
            this.startUpdateImageTimer();
        }
    }

    onResize(event) {
        this.setCanvasSize();
        this.drawImage();
    }
}
