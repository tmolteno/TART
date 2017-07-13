import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { InfoService } from '../../services/info.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-raw-mode',
    templateUrl: './raw-mode.component.html',
    styleUrls: ['./raw-mode.component.css']
})
export class RawModeComponent implements OnInit {


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
            && this.router.url === '/raw-data-mode') {
            this.router.navigateByUrl('/');
        }  else {
            this.setRawDataMode();
            this.getNumSamples();
            this.getSamplingFreq();
            this.getSaveDataFlag();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/raw-data-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }

    setRawDataMode() {
        this.modeService.setOperatingMode('raw')
            .subscribe(res => {
                this.checkCorrectMode();
            });
    }

    checkCorrectMode() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                if (mode !== 'raw') {
                    this.setRawDataMode();
                }
            })
    }

    getNumSamples() {
        this.dataAcquisitionService.getRawNumSamplesExp()
            .subscribe(numSamplesExp => {
                this.numSamples = numSamplesExp;
            }, err => {
                console.log(err);
            });
    }

    setNumSamples(value: number) {
        this.dataAcquisitionService.setRawNumSamplesExp(value)
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
        this.dataAcquisitionService.getRawSaveFlag()
            .subscribe(saveDataFlag => {
                this.saveData = saveDataFlag;
            }, err => {
                console.log(err);
            });
    }

    setSaveDataFlag(value: boolean) {
        this.dataAcquisitionService.setRawSaveFlag(value)
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
