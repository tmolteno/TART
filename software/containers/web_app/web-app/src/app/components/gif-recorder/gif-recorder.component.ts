import {
    Component,
    Input,
    Output,
    ViewChild,
    ElementRef,
    EventEmitter,
    HostListener
} from '@angular/core';
const gifshot = require('gifshot');
import * as moment from 'moment/moment';

@Component({
    selector: 'app-gif-recorder',
    templateUrl: './gif-recorder.component.html',
    styleUrls: ['./gif-recorder.component.css'],
    host: {
        '(window:resize)': 'onResize($event)'
    }
})
export class GifRecorderComponent {
    @ViewChild('gifDisplay') gifDisplay: ElementRef;
    @ViewChild('hiddenGifDownloader') hiddenGifDownloader: ElementRef;

    @Input()
    dataSource: any[] = [];

    @Input()
    numFrames: number = 0;

    @Output()
    clickedStart = new EventEmitter();

    @Output()
    clickedStop = new EventEmitter();

    @Output()
    clickedReset = new EventEmitter();

    isRecording: boolean = false;
    hasGif: boolean = false;

    constructor() { }

    ngAfterViewInit() {
        this.setGifOutputSize();
    }

    setGifOutputSize() {
        this.gifDisplay.nativeElement.width = this.gifDisplay.nativeElement.height;
    }

    onStartClick(event) {
        this.isRecording = true;
        this.clickedStart.emit(event);
    }

    onStopClick(event) {
        this.isRecording = false;
        this.clickedStop.emit(event);
    }

    onClickedReset(event) {
        this.clickedReset.emit(event);
        this.gifDisplay.nativeElement.src = "//:0";
        this.hasGif = false;
    }

    onSaveGifClick(event) {
        gifshot.createGIF({ images: this.dataSource }, (obj) => {
            if (!obj.error) {
                let image = obj.image;
                this.gifDisplay.nativeElement.src = image;
            }
        });
        this.hasGif = true;
    }

    onClickedDownloadBtn(event) {
        let image = this.gifDisplay.nativeElement.src;
        this.hiddenGifDownloader.nativeElement.download = this.generateGifFilename();
        this.hiddenGifDownloader.nativeElement.href = image;
        this.hiddenGifDownloader.nativeElement.click();
    }

    generateGifFilename() {
        let timestamp = moment().format();
        let gmtDateTime = moment.utc(timestamp);
        let localDateTime = gmtDateTime.local().format("YYYY_MM_DD_HH_mm_ss");
        return `radio_gif_${localDateTime}`;
    }

    onResize(event) {
        this.setGifOutputSize();
    }
}
