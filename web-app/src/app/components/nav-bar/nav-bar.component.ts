import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';

@Component({
  selector: 'app-nav-bar',
  templateUrl: './nav-bar.component.html',
  styleUrls: [
      './nav-bar.component.css',
      '../../../../node_modules/ng-loading-bar/loading-bar.css'
  ]
})
export class NavBarComponent implements OnInit {

  isLoggedIn: boolean = false;

    constructor(
        private authService: AuthService,
        private router: Router
    ) { }

    ngOnInit() {
        this.authService.login$.subscribe(loginStatus => {
            this.isLoggedIn = loginStatus;
        });
        this.authService.load();
    }

    onLogoutClick() {
        this.authService.logout();
    }
}
