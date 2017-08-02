import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { InfoService } from '../../services/info.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

import { Observable } from 'rxjs/Rx';

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
            .subscribe(result => {},
            err => {
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
            });
    }

    getInitData() {
        this.dataAcquisitionService.getVisNumSamplesExp()
        .catch(() => { return Observable.of(0); })
        .flatMap(result => {
            this.numSamples = result;
            return this.infoService.getInfo()
        })
        .catch(() => { return Observable.of({ sampling_frequency: 0 }); })
        .flatMap(result => {
            this.samplingFreq = result["sampling_frequency"];
            return this.dataAcquisitionService.getVisSaveFlag()
        })
        .catch(() => { return Observable.of(false); })
        .subscribe(result => { this.saveData = result; });
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
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
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
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
            });
    }

    onNumSamplesExpChanged(value: number) {
        this.setNumSamples(value);
    }

    onSaveDataChanged(value: boolean) {
        this.setSaveDataFlag(value);
    }
}
