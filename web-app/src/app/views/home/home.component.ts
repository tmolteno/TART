import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { ModeService } from '../../services/mode.service';
import { AuthService } from '../../services/auth.service';

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

    private currentMode: string = '';

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        this.modeService.getOperatingMode()
            .subscribe(mode => {
                this.currentMode = mode;
                this.ref.detectChanges();
                if (this.authService.isTokenValid()) {
                    this.redirectToModePage();
                }
            });
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
