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
        if (!this.authService.isTokenValid()
            && this.router.url === '/off-mode') {
            this.router.navigateByUrl('/');
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/off-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }
}
