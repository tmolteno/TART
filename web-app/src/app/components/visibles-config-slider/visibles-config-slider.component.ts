import {
    Component,
    Input,
    Output,
    EventEmitter,
} from '@angular/core';

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
    slideEnd = new EventEmitter();

    @Output()
    valueChanged = new EventEmitter();

    sliderValue: number;
    labelValue: number;

    constructor() {
    }

    ngOnInit() {
        this.sliderValue = this.startValue;
        this.labelValue = this.startValue;
    }

    onSliderSlide(value) {
        this.labelValue = value;
        this.valueChanged.emit(value);
    }

    onSliderEnd(value) {
        this.slideEnd.emit(value);
    }
}
