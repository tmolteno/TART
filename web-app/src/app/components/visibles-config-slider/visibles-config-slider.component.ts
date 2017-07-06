import { Component, Input, Output, EventEmitter } from '@angular/core';

@Component({
    selector: 'app-visibles-config-slider',
    templateUrl: './visibles-config-slider.component.html',
    styleUrls: ['./visibles-config-slider.component.css']
})
export class VisiblesConfigSliderComponent {

    @Input()
    startValue: number;

    @Input()
    minValue: number;

    @Input()
    maxValue: number;

    @Input()
    stepSize: number;

    @Input()
    labelText: any;

    @Output()
    valueChanged = new EventEmitter();
    sliderValue: number;
    labelValue: number;

    constructor() {
    }

    ngOnInit() {
        console.log("OnInit");
        console.log(this.startValue);
        this.sliderValue = this.startValue;
        this.labelValue = this.startValue;
    }

    onSliderSlide(value) {
        this.labelValue = value;
    }

    onSliderEnd(value) {
        this.valueChanged.emit(value);
    }
}
