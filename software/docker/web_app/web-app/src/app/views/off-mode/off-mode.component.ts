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

    modeSubscription: any;

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

    ngOnDestroy() {
        if (this.modeSubscription) {
            this.modeSubscription.unsubscribe();
        }
    }

    setOffMode() {
        this.modeSubscription = this.modeService.setOperatingMode('off')
            .subscribe(result => {},
            err => {
                if (err.message === AuthService.TOKEN_EXPIRED_ERR_MSG) {
                    this.router.navigateByUrl('/');
                }
            });
    }
}
