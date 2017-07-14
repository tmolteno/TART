import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { FpgaStatusComponent } from
'../../components/fpga-status/fpga-status.component';
import { ChannelsStatusComponent } from
'../../components/channels-status/channels-status.component';
import { TartService } from '../../services/tart.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

import { Observable } from 'rxjs/Observable';

@Component({
    selector: 'app-diagnose-mode',
    templateUrl: './diagnose-mode.component.html',
    styleUrls: ['./diagnose-mode.component.css']
})
export class DiagnoseModeComponent {
    @ViewChild('fpgaStatus') fpgaStatus: FpgaStatusComponent;
    @ViewChild('channelsStatus') channelsStatus: ChannelsStatusComponent;

    refreshTime: number = 60; //seconds
    updateStatusTimer: any;
    timerSubscription: any;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
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
        if (this.timerSubscription) {
            this.timerSubscription.unsubscribe();
        }
    }

    startUpdateTimer() {
        let updateTime = this.refreshTime * 1000;
        this.updateStatusTimer = Observable.timer(updateTime, updateTime);
        this.timerSubscription = this.updateStatusTimer
            .subscribe(tick => this.updateStatus());
    }

    updateStatus() {
        this.fpgaStatus.updateFpgaStatus();
        this.channelsStatus.updateChannelsStatus();
    }

    setDiagnoseMode() {
        this.modeService.setOperatingMode('diag')
            .subscribe(() => {
                this.checkCorrectMode();
            });
    }

    checkCorrectMode() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                if (mode !== 'diag') {
                    this.setDiagnoseMode();
                }
            })
    }

    getType(item) {
        return typeof item;
    }
}
