import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { ModeService } from '../../services/mode.service';

@Component({
    selector: 'app-hd-img-mode',
    templateUrl: './hd-img-mode.component.html',
    styleUrls: ['./hd-img-mode.component.css']
})
export class HdImgModeComponent implements OnInit {

    modeSubscription: any;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private ref: ChangeDetectorRef,
        private router: Router
    ) { }

    ngOnInit() {
        if (!this.authService.isTokenValid()
            && this.router.url === '/hd-img-mode') {
            this.router.navigateByUrl('/');
        } else {
            /*this.modeSubscription = this.modeService.setOperatingMode('hd_img')
                .subscribe();*/
        }
        this.authService.login$.subscribe(loginStatus => {
            if (!loginStatus && this.router.url === '/hd-img-mode') {
                this.router.navigateByUrl('/');
            }
        });
        // TODO: swtch mode
    }

    ngOnDestroy() {
        if (this.modeSubscription) {
            this.modeSubscription.unsubscribe();
        }
    }
}
