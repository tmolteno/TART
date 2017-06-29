import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
      selector: 'app-cal-mode',
      templateUrl: './cal-mode.component.html',
      styleUrls: ['./cal-mode.component.css']
})
export class CalModeComponent implements OnInit {

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/calibrate-mode') {
            this.router.navigateByUrl('/');
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/calibrate-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }
}
