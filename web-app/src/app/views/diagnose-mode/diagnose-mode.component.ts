import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { TartService } from '../../services/tart.service';
import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-diagnose-mode',
    templateUrl: './diagnose-mode.component.html',
    styleUrls: ['./diagnose-mode.component.css']
})
export class DiagnoseModeComponent implements OnInit {

    fpgaStatus: Object = {};
    channelsStatus: Object[] = [];

    fpgaStatusVisible: boolean = true;
    channelsVisible: boolean = true;

    constructor(
        private tartService: TartService,
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        let isTokenValid = this.authService.isTokenValid();
        if (!isTokenValid && this.router.url === '/diag-mode') {
            this.router.navigateByUrl('/');
        } else if (isTokenValid) {
            this.setDiagnoseMode();
        } else {
            this.displayMode();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/diag-mode') {
                this.router.navigateByUrl('/');
            }
        });
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
                } else {
                    this.displayMode();
                }
            })
    }

    displayMode() {
        this.getFpgaStatus();
        this.getChannelStatus();
    }

    toggleFpgaStatus(event) {
        this.fpgaStatusVisible = !this.fpgaStatusVisible;
    }

    toggleChannelStatus(event) {
        this.channelsVisible = !this.channelsVisible;
    }

    getFpgaStatus() {
        this.tartService.getFpgaStatus()
            .subscribe(result => {
                this.fpgaStatus = result;
                this.ref.detectChanges();
            });
    }

    getChannelStatus() {
        this.tartService.getChannelStatus()
            .subscribe(result => {
                this.channelsStatus = result;
                this.ref.detectChanges();
            });
    }

    getType(item) {
        return typeof item;
    }
}
