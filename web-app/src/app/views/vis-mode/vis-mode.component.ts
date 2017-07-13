import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { InfoService } from '../../services/info.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-vis-mode',
    templateUrl: './vis-mode.component.html',
    styleUrls: ['./vis-mode.component.css']
})
export class VisModeComponent implements OnInit {

    minNumSamples: number = 16;
    maxNumSamples: number = 24;
    numSamples: number;
    samplingFreq: number;
    saveData: boolean;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private infoService: InfoService,
        private dataAcquisitionService: DataAcquisitionService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/vis-data-mode') {
            this.router.navigateByUrl('/');
        } else {
            this.setVisDataMode();
            this.getNumSamples();
            this.getSamplingFreq();
            this.getSaveDataFlag();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/vis-data-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }

    setVisDataMode() {
        this.modeService.setOperatingMode('vis')
            .subscribe(res => {
                this.checkCorrectMode();
            });
    }

    checkCorrectMode() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                if (mode !== 'vis') {
                    this.setVisDataMode();
                }
            })
    }

    getNumSamples() {
        this.dataAcquisitionService.getVisNumSamplesExp()
            .subscribe(numSamplesExp => {
                this.numSamples = numSamplesExp;
            }, err => {
                console.log(err);
            });
    }

    setNumSamples(value: number) {
        this.dataAcquisitionService.setVisNumSamplesExp(value)
            .subscribe(res => {
                this.numSamples = value;
            }, err => {
                console.log(err);
            });
    }

    getSamplingFreq() {
        this.infoService.getInfo()
            .subscribe(info => {
                this.samplingFreq = info["sampling_frequency"];
            })
    }

    getSaveDataFlag() {
        this.dataAcquisitionService.getVisSaveFlag()
            .subscribe(saveDataFlag => {
                this.saveData = saveDataFlag;
            }, err => {
                console.log(err);
            });
    }

    setSaveDataFlag(value: boolean) {
        this.dataAcquisitionService.setVisSaveFlag(value)
            .subscribe(res => {
                this.saveData = value;
            }, err => {
                console.log(err);
            });
    }

    onNumSamplesExpChanged(value: number) {
        this.setNumSamples(value);
    }

    onSaveDataChanged(value: boolean) {
        this.setSaveDataFlag(value);
    }
}
