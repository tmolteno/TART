import { Component } from '@angular/core';
import { InfoService } from '../../services/info.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-info',
    templateUrl: './info.component.html',
    styleUrls: ['./info.component.css']
})
export class InfoComponent {

    ghzMod: number  = 1000000000;
    mhzMod: number = 1000000;

    mode: string;
    l0Freq: number;
    operationFreq: number;
    bandwidth: number;
    basebandFreq: number;
    samplingFreq: number;
    numAntenna: number;

    constructor(
        private infoService: InfoService,
        private modeService: ModeService
    ) { }

    ngOnInit() {
        this.infoService.getInfo().subscribe(info => {
            this.l0Freq = info.L0_frequency / this.ghzMod;
            this.operationFreq = info.operating_frequency / this.ghzMod;
            this.bandwidth =  info.bandwidth / this.mhzMod;
            this.basebandFreq = info.baseband_frequency / this.mhzMod;
            this.samplingFreq = info.sampling_frequency / this.mhzMod;
            this.numAntenna = info.num_antenna;
        });
        this.modeService.getOperatingMode().subscribe(currentMode => {
            this.mode = currentMode['mode'];
        })
    }
}
