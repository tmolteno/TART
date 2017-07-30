import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { InfoService } from '../../services/info.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/forkJoin';

@Component({
    selector: 'app-vis-mode',
    templateUrl: './vis-mode.component.html',
    styleUrls: ['./vis-mode.component.css']
})
export class VisModeComponent {

    modeSubscription: any;

    minNumSamples: number = 16;
    maxNumSamples: number = 24;
    numSamples: any;
    samplingFreq: any;
    saveData: any;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private infoService: InfoService,
        private dataAcquisitionService: DataAcquisitionService,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/vis-data-mode') {
            this.router.navigateByUrl('/');
        } else {
            this.setVisDataMode();
            this.getInitData();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/vis-data-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }

    ngOnDestroy() {
        if (this.modeSubscription) {
            this.modeSubscription.unsubscribe();
        }
    }

    setVisDataMode() {
        this.modeSubscription = this.modeService.setOperatingMode('vis')
            .subscribe();
    }

    getInitData() {
        Observable.forkJoin([
            this.dataAcquisitionService.getVisNumSamplesExp(),
            this.infoService.getInfo(),
            this.dataAcquisitionService.getVisSaveFlag()
        ]).subscribe(result => {
            this.numSamples = result[0];
            this.samplingFreq =  result[1]["sampling_frequency"];
            this.saveData = result[2];
        }, err => {
            console.log("VisModeComponent.getInitData() failed");
            console.log(err.message);
        });
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
