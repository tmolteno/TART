import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

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
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/diag-mode') {
            this.router.navigateByUrl('/');
        } else {
            this.modeService.setOperatingMode('diag')
                .subscribe(res => {
                    console.log('set diag mode!');
                    // TODO: get and display data for this mode (fgpa and status)
                });
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/diag-mode') {
                this.router.navigateByUrl('/');
            }
        });
        // TODO: switch mode
    }
}
