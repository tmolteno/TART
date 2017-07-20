import {
    Component,
    Input,
    Output,
    ViewChild,
    ElementRef,
    EventEmitter
} from '@angular/core';
const gifshot = require('gifshot/build/gifshot');

@Component({
    selector: 'app-gif-recorder',
    templateUrl: './gif-recorder.component.html',
    styleUrls: ['./gif-recorder.component.css']
})
export class GifRecorderComponent {
    @ViewChild('gifDisplay') gifDisplay: ElementRef;

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

    constructor() { }

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
    }

    onSaveGifClick(event) {
        gifshot.createGIF({ images: this.dataSource }, (obj) => {
            if (!obj.error) {
                let image = obj.image;
                this.gifDisplay.nativeElement.src = image;
          } else {
                console.log("Failed with: " + obj.error);
          }
      });
  }
}
