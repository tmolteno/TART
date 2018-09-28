import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { AuthService } from '../../services/auth.service';
import { InfoService } from '../../services/info.service';

@Component({
  selector: 'app-nav-bar',
  templateUrl: './nav-bar.component.html',
  styleUrls: [
      './nav-bar.component.css'
  ]
})
export class NavBarComponent implements OnInit {

    isLoggedIn: boolean = false;

    name: string;
    lat: number;
    lng: number;
    alt: number;

    constructor(
        private authService: AuthService,
        private infoService: InfoService,
        private router: Router
    ) { }

    ngOnInit() {
        this.authService.login$.subscribe(loginStatus => {
            this.isLoggedIn = loginStatus;
        });
        this.infoService.getInfo().subscribe(info => {
            this.name = info.name;
            this.lat = info.location.lat;
            this.lng = info.location.lon;
            this.alt = info.location.alt;
        })
        this.authService.load();
    }

    onLogoutClick() {
        this.authService.logout();
    }
}
