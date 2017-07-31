import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { FpgaStatusComponent } from
'../../components/fpga-status/fpga-status.component';
import { ModeService } from '../../services/mode.service';
import { AuthService } from '../../services/auth.service';
import { TartService } from '../../services/tart.service';
import { ImagingService } from '../../services/imaging.service';
import { DataAcquisitionService } from '../../services/data-acquisition.service';

import { Observable } from 'rxjs/Observable';

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

    filePaths: any[] = [
      {
        "Id": 1170,
        "checksum": "9587a59b6a46faf6570a7b0df8d39265326062c3d93844a6feb78e49265b7151",
        "filename": "raw/2017/7/31/02_04_36.890779_data.pkl",
        "timestamp": "2017-07-31 02:04:44.391875"
      },
      {
        "Id": 1169,
        "checksum": "bff7d16e0507dcf17096408abc23a971a97b72f4b1c35b7158247f58fade8039",
        "filename": "raw/2017/7/31/02_04_29.310015_data.pkl",
        "timestamp": "2017-07-31 02:04:36.855546"
      }
  ];

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
                this.currentMode = mode;
                if (this.authService.isTokenValid()) {
                    this.redirectToModePage();
                }
            });
        this.dataAcquisitionService.getRawFilePaths()
            .subscribe(result => {
                console.log("got raw data paths");
                this.filePaths = result;
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
        this.updateFpgaTimer = Observable.timer(updateTime, updateTime);
        this.fpgaTimerSubscription = this.updateFpgaTimer
            .subscribe(tick => this.fpgaStatus.updateFpgaStatus());
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
