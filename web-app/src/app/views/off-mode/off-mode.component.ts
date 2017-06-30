import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-off-mode',
    templateUrl: './off-mode.component.html',
    styleUrls: ['./off-mode.component.css']
})
export class OffModeComponent implements OnInit {

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        let isTokenValid = this.authService.isTokenValid();
        if (!this.authService.isTokenValid()
            && this.router.url === '/off-mode') {
            this.router.navigateByUrl('/');
        }  else if (isTokenValid) {
            this.setOffMode();
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/off-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }

    setOffMode() {
        this.modeService.setOperatingMode('off')
            .subscribe(res => {
                console.log('set off mode!');
                if (res.mode !== 'off') {
                    this.setOffMode();
                }
            });
    }

    checkCorrectMode() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                if (mode !== 'off') {
                    this.setOffMode();
                }
            })
    }
}
