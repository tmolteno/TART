import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { FpgaStatusComponent } from
'../../components/fpga-status/fpga-status.component';
import { ChannelsStatusComponent } from
'../../components/channels-status/channels-status.component';
import { TartService } from '../../services/tart.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';
import { ImagingService } from '../../services/imaging.service';

//import { Observable } from 'rxjs/Observable';
import { timer } from 'rxjs';

@Component({
    selector: 'app-diagnose-mode',
    templateUrl: './diagnose-mode.component.html',
    styleUrls: ['./diagnose-mode.component.css']
})
export class DiagnoseModeComponent {
    @ViewChild('fpgaStatus') fpgaStatus: FpgaStatusComponent;
    @ViewChild('channelsStatus') channelsStatus: ChannelsStatusComponent;

    modeSubscription: any;

    refreshTime: number = 60; //seconds
    updateStatusTimer: any;
    timerSubscription: any;

    statusMapChannels: any[] = [];
    antennaPositions: any[] = [];

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private tartService: TartService,
        private imagingService: ImagingService,
        private router: Router
    ) { }

    ngOnInit() {
        let isTokenValid = this.authService.isTokenValid();
        if (!isTokenValid && this.router.url === '/diag-mode') {
            this.router.navigateByUrl('/');
        } else if (isTokenValid) {
            this.setDiagnoseMode();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/diag-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }

    ngAfterViewInit() {
        this.startUpdateTimer();
    }

    ngOnDestroy() {
        if (this.modeSubscription) {
            this.modeSubscription.unsubscribe();
        }

        if (this.timerSubscription) {
            this.timerSubscription.unsubscribe();
        }
    }

    startUpdateTimer() {
        let updateTime = this.refreshTime * 1000;
        this.updateStatusTimer = timer(0, updateTime);
        this.timerSubscription = this.updateStatusTimer
            .subscribe(tick => this.updateStatus());
    }

    updateStatus() {
        this.fpgaStatus.updateFpgaStatus();
        this.updateChannelsStatus();
        this.updateAntennaPositions();
    }

    updateChannelsStatus() {
        this.tartService.getChannelsStatus()
            .subscribe(channelsStatus => {
                this.statusMapChannels = channelsStatus;
            });
    }

    updateAntennaPositions() {
        this.imagingService.getAntennaPositions()
            .subscribe(antennaPositions => {
                this.antennaPositions = antennaPositions;
            });
    }

    setDiagnoseMode() {
        this.modeSubscription = this.modeService.setOperatingMode('diag')
            .subscribe(result => {},
            err => {
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
            });
    }

    onAntennaEnabledChanged(event) {
        this.tartService.setChannelEnabled(event.id, event.enabled)
            .subscribe(response => {
                this.updateChannelsStatus();
            },
            err => {
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
            });
    }

    getType(item) {
        return typeof item;
    }
}
