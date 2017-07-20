import { Component, Input, ViewChild, ElementRef } from '@angular/core';
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

    constructor() { }

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
