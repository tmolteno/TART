import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { ModeService } from '../../services/mode.service';
import { AuthService } from '../../services/auth.service';

@Component({
    selector: 'app-login',
    templateUrl: './login.component.html',
    styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {

    currentMode: string = '';
    password: string = '';
    displayLoginFailMsg: boolean = false;

    constructor(
        private authService: AuthService,
        private modeService: ModeService,
        private router: Router
    ) { }

    ngOnInit() { }

    onPasswordKeyPress() {
        this.displayLoginFailMsg = false;
    }

    onLoginClick() {
        this.authService.login(this.password)
            .subscribe(res => {
                if (res) {
                    this.redirectToModePage();
                } else {
                    this.displayLoginFailMsg = true;
                }
            },
            err => {
                this.displayLoginFailMsg = true;
            });
    }

    redirectToModePage() {

        this.modeService.getOperatingMode()
            .subscribe(mode => {
                switch(mode) {
                    case 'off':
                        this.router.navigateByUrl('off-mode');
                        break;
                    case 'diag':
                        this.router.navigateByUrl('diag-mode');
                        break;
                    case 'raw':
                        this.router.navigateByUrl('raw-data-mode');
                        break;
                    case 'vis':
                        this.router.navigateByUrl('vis-data-mode');
                        break;
                    case 'cal':
                        this.router.navigateByUrl('calibrate-mode');
                        break;
                    case 'hd_img':
                        this.router.navigateByUrl('hd-img-mode');
                        break;
                    default:
                        break;
                }
            },
            err => {
                console.log(`something broke: ${err.message}`);
            });
    }
}
