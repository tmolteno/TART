import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-vis-mode',
    templateUrl: './vis-mode.component.html',
    styleUrls: ['./vis-mode.component.css']
})
export class VisModeComponent implements OnInit {

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/vis-data-mode') {
            this.router.navigateByUrl('/');
        } else {
            this.modeService.setOperatingMode('vis')
                .subscribe(res => {
                    console.log('set vis data mode!');
                    // TODO: get and display data for this mode
                });
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/vis-data-mode') {
                this.router.navigateByUrl('/');
            }
        });
    }
}
