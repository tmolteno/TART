import { Component, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { FpgaStatusComponent } from
'../../components/fpga-status/fpga-status.component';
import { ModeService } from '../../services/mode.service';
import { AuthService } from '../../services/auth.service';

import { Observable } from 'rxjs/Observable';

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent {
    @ViewChild('fpgaStatus') fpgaStatus: FpgaStatusComponent;

    private currentMode: string = '';

    refreshFpgaTime: number = 60; //seconds
    updateFpgaTimer: any;
    fpgaTimerSubscription: any;

    private showFpga: boolean = true;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
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
