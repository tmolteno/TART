import { Component, OnInit } from '@angular/core';
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
