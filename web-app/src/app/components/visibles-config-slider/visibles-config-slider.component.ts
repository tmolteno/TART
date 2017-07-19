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
    startValue: number = 2;

    @Input()
    minValue: number = 1;

    @Input()
    maxValue: number = 3;

    @Input()
    stepSize: number = 1;

    @Input()
    labelText: any;

    @Output()
    sliderStart = new EventEmitter();

    @Output()
    slideEnd = new EventEmitter();

    @Output()
    valueChanged = new EventEmitter();

    sliderValue: number = 2;
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

    onSliderStart(value) {
        this.sliderStart.emit(value);
    }

    onSliderEnd(value) {
        this.slideEnd.emit(value);
    }
}
