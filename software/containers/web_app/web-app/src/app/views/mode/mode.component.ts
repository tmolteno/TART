import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
      selector: 'app-mode',
      templateUrl: './mode.component.html',
      styleUrls: ['./mode.component.css']
})
export class ModeComponent implements OnInit {

    currentMode: string = '';
    //private currentMode: string = '';

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()) {
            this.router.navigateByUrl('/');
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus) {
                this.router.navigateByUrl('/');
            }
        });
        this.getActiveMode();
    }

    getActiveMode() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                this.currentMode = mode['mode'];
                this.ref.detectChanges();
            });
    }
}
