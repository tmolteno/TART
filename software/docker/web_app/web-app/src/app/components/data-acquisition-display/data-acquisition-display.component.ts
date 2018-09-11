import {
    Component,
    Input,
    Output,
    EventEmitter,
    ViewChild,
    ElementRef,
    SimpleChanges
} from '@angular/core';

@Component({
    selector: 'app-data-acquisition-display',
    templateUrl: './data-acquisition-display.component.html',
    styleUrls: ['./data-acquisition-display.component.css']
})
export class DataAcquisitionDisplayComponent {
    @ViewChild('numSamplesExpInput') numSamplesExpInput: ElementRef;
    @ViewChild('saveDataToggle') saveDataToggle: ElementRef;

    @Input()
    minNumSamplesExp: number;
    @Input()
    maxNumSamplesExp: number;
    @Input()
    numSamplesExp: number = null;
    @Input()
    samplingFreq: number  = null;
    @Output()
    numSamplesExpChanged = new EventEmitter();

    @Input()
    doSaveData: boolean;
    @Output()
    doSaveDataChanged = new EventEmitter();

    integrationTime: number;

    constructor() { }

    ngOnInit() {
        this.saveDataToggle.nativeElement.checked = this.doSaveData;
    }

    ngOnChanges(changes: SimpleChanges) {
        this.saveDataToggle.nativeElement.checked = this.doSaveData;
        this.calculateIntegrationTime();
    }

    calculateIntegrationTime()  {
        this.integrationTime = Math.pow(2, this.numSamplesExp) / this.samplingFreq;
    }

    onNumSamplesExpChanged(event) {
        let value = this.numSamplesExpInput.nativeElement.value;
        this.numSamplesExpChanged.emit(value);
    }

    onSaveDataChanged(event) {
        let value = this.saveDataToggle.nativeElement.checked;
        this.doSaveDataChanged.emit(value);
    }
}
