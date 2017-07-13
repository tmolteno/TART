import { Component, Input, Output, EventEmitter, ViewChild, ElementRef } from '@angular/core';

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
    numSamplesExp: number;
    @Input()
    samplingFreq: number;
    @Output()
    numSamplesExpChanged = new EventEmitter();

    @Input()
    doSaveData: boolean;
    @Output()
    doSaveDataChanged = new EventEmitter();

    integrationTime: number;

    constructor() { }

    ngOnInit() {
        this.integrationTime = this.calculateIntegrationTime(this.numSamplesExp,
            this.samplingFreq);
        this.saveDataToggle.nativeElement.checked = this.doSaveData;
    }

    calculateIntegrationTime(numSamplesExp: number, samplingFreq: number)  {
        return Math.pow(2, numSamplesExp) / samplingFreq;
    }

    onNumSamplesExpChanged(event) {
        let value = this.numSamplesExpInput.nativeElement.value;
        this.integrationTime = this.calculateIntegrationTime(value,
            this.samplingFreq);
        this.numSamplesExpChanged.emit(value);
    }

    onSaveDataChanged(event) {
        let value = this.saveDataToggle.nativeElement.checked;
        this.doSaveDataChanged.emit(value);
    }
}
