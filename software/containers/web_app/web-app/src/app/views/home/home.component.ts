import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { FpgaStatusComponent } from
'../../components/fpga-status/fpga-status.component';
import { ModeService } from '../../services/mode.service';
import { AuthService } from '../../services/auth.service';
import { TartService } from '../../services/tart.service';
import { ImagingService } from '../../services/imaging.service';
import { DataAcquisitionService } from '../../services/data-acquisition.service';

//import { Observable } from 'rxjs/Observable';
import { timer } from 'rxjs';
import * as moment from 'moment/moment';

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent {
    @ViewChild('fpgaStatus') fpgaStatus: FpgaStatusComponent;

    channelsStatus: any[] = [];
    antennaPositions: any[] = [];

    private currentMode: string = '';

    refreshFpgaTime: number = 60; //seconds
    updateFpgaTimer: any;
    fpgaTimerSubscription: any;

    private showFpga: boolean = true;

    rawFilePaths: any[] = [];
    visFilePaths: any[] = [];

    timeNow: string = moment().format();

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private tartService: TartService,
        private dataAcquisitionService: DataAcquisitionService,
        private router: Router
    ) { }

    ngOnInit() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                this.currentMode = mode['mode'];
                if (this.authService.isTokenValid()) {
                    this.redirectToModePage();
                }
            });
        this.dataAcquisitionService.getRawFilePaths()
            .subscribe(result => {
                this.rawFilePaths = result;
            });

        this.dataAcquisitionService.getVisFilePaths()
            .subscribe(result => {
                this.visFilePaths = result;
            })
    }

    ngAfterViewInit() {
        this.startUpdateFpgaTimer();
    }

    ngOnDestroy() {
        if (this.fpgaTimerSubscription) {
            this.fpgaTimerSubscription.unsubscribe();
        }
    }

    startUpdateFpgaTimer() {
        let updateTime = this.refreshFpgaTime * 1000;
        this.updateFpgaTimer = timer(updateTime, updateTime);

        this.fpgaTimerSubscription = this.updateFpgaTimer
            .subscribe(tick => {

                this.timeNow = moment().format();
                this.fpgaStatus.updateFpgaStatus()
            });
    }

    redirectToModePage() {
        switch(this.currentMode) {
            case 'off':
                this.router.navigateByUrl('/off-mode');
                break;
            case 'diag':
                this.router.navigateByUrl('/diag-mode');
                break;
            case 'raw':
                this.router.navigateByUrl('/raw-data-mode');
                break;
            case 'vis':
                this.router.navigateByUrl('/vis-data-mode');
                break;
            case 'cal':
                this.router.navigateByUrl('/calibrate-mode');
                break;
            case 'hd_img':
                this.router.navigateByUrl('/hd-img-mode');
                break;
            default:
                break;
        }
    }
}
