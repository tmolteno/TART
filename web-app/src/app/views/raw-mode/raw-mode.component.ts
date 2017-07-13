import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-raw-mode',
    templateUrl: './raw-mode.component.html',
    styleUrls: ['./raw-mode.component.css']
})
export class RawModeComponent implements OnInit {


    minNumSamples: number;// = 16;
    maxNumSamples: number;// = 24;
    numSamples: number;// = 20;
    samplingFreq: number;// = 1636800;
    saveData: boolean;// = false;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        /*if (!this.authService.isTokenValid()
            && this.router.url === '/raw-data-mode') {
            this.router.navigateByUrl('/');
        }  else {
            this.modeService.setOperatingMode('raw')
                .subscribe(res => {
                    console.log('set raw data mode!');
                    // TODO: get and display data for this mode
                });
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/raw-data-mode') {
                this.router.navigateByUrl('/');
            }
        });*/
    }

    onNumSamplesExpChanged(value) {
        this.numSamples = value;
    }

    onSaveDataChanged(value) {
        console.log("save data value changed: " + value);
    }
}
