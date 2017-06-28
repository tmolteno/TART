import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { environment } from '../../environments/environment';

import 'rxjs/add/operator/map';

@Injectable()
export class AuthService {

    tokenStorageKey: string = 'token';
    tokenGetTimeStorageKey: string = 'tokenGetTime';
    tokenMaxAge: number = 270000; // 4 minutes 30 seconds in milleseconds

    apiUrl: string = '';

    public login$: Observable<boolean>;
    private loginObserver: any;
    private loginStatus: boolean;

    constructor(private http: Http) {
        this.apiUrl = environment.apiUrl;
        this.loginStatus = this.isTokenValid();
        this.login$ = new Observable(observer => {
            this.loginObserver = observer;
        }).share();
    }

    load() {
        this.loginObserver.next(this.loginStatus);
    }

    login(password: string) {
        return this.http.post(`${this.apiUrl}/auth`, {
            'username': 'admin',
            'password': password
        }).map((res: Response) => {
            let body  = res.json();
            if (body.access_token) {
                this.setAuthToken(body.access_token);
                return true;
            } else {
                return false;
            };
        })
        .catch((err: any) => {
            return Observable.throw(new Error(err.status));
        });
    }

    setAuthToken(token: string) {
        let getTime = new Date().getTime().toString();
        localStorage.setItem(this.tokenStorageKey, token);
        localStorage.setItem(this.tokenGetTimeStorageKey, getTime);
        // update observers with login status
        this.loginStatus = true;
        this.loginObserver.next(this.loginStatus);
    }

    logout() {
        localStorage.removeItem(this.tokenStorageKey);
        localStorage.removeItem(this.tokenGetTimeStorageKey);
        // update observers with login status
        this.loginStatus = false;
        this.loginObserver.next(this.loginStatus);
    }

    isTokenValid() {
        let tokenGetTime = parseInt(
            localStorage.getItem(this.tokenGetTimeStorageKey));
        if (!Number.isInteger(tokenGetTime)) {
            return false;
        }
        let timeNow = new Date().getTime();
        let timeDelta = timeNow - tokenGetTime;
        return timeDelta < this.tokenMaxAge;
    }

    getAuthToken() {
        // check that token is still valid
        if (!this.isTokenValid()) {
            this.logout();
            return null;
        }
        return localStorage.getItem(this.tokenStorageKey);
        // TODO: if there is a way to get a new token from an old token,
        // TODO: and the old token is near expiry, get a new token.
    }
}
