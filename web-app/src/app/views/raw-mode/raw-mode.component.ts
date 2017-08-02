import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { InfoService } from '../../services/info.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/forkJoin';

@Component({
    selector: 'app-raw-mode',
    templateUrl: './raw-mode.component.html',
    styleUrls: ['./raw-mode.component.css']
})
export class RawModeComponent implements OnInit {

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
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/raw-data-mode') {
            this.router.navigateByUrl('/');
        }  else {
            this.setRawDataMode();
            this.getInitData();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/raw-data-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }

    ngOnDestroy() {
        if (this.modeSubscription) {
            this.modeSubscription.unsubscribe();
        }
    }

    setRawDataMode() {
        this.modeSubscription = this.modeService.setOperatingMode('raw')
            .subscribe(result => {},
            err => {
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
            });
    }

    getInitData() {
        Observable.forkJoin([
            this.dataAcquisitionService.getRawNumSamplesExp(),
            this.infoService.getInfo(),
            this.dataAcquisitionService.getRawSaveFlag()
        ]).subscribe(result => {
            this.numSamples = result[0];
            this.samplingFreq =  result[1]["sampling_frequency"];
            this.saveData = result[2];
        }, err => {
            console.log("RawModeComponent.getInitData() failed");
            console.log(err.message);
        });
        // TODO: replace forkJoin with flatMap
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
