import { Component } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';

@Component({
    selector: 'app-login',
    templateUrl: './login.component.html',
    styleUrls: ['./login.component.css']
})
export class LoginComponent {

    password: string = '';
    displayLoginFailMsg: boolean = false;

    constructor(
        private authService: AuthService,
        private router: Router
    ) {}

    onPasswordKeyPress() {
        this.displayLoginFailMsg = false;
    }

    onLoginClick() {
        this.authService.login(this.password)
            .subscribe(res => {
                if (res) {
                    this.router.navigateByUrl('/mode');
                } else {
                    this.displayLoginFailMsg = true;
                }
            },
            err => {
                this.displayLoginFailMsg = true;
            });
    }
}
