import { Injectable } from '@angular/core';
import { Http, Response, RequestOptions, Headers } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { PlatformLocation } from '@angular/common';

import 'rxjs/add/operator/map';

@Injectable()
export class AuthService {

    public static readonly TOKEN_EXPIRED_ERR_MSG = 'Token expired';

    private tokenStorageKey: string = 'token';
    private tokenGetTimeStorageKey: string = 'tokenGetTime';
    public tokenMaxAge: number = 3600000; // 1 hour in milliseconds

    private apiUrl: string = '';

    public login$: Observable<boolean>;
    private loginObserver: any;
    private loginStatus: boolean;

    constructor(
        private http: Http,
        private platformLocation: PlatformLocation
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
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

    getAuthRequestOptions() {
        let token = this.getAuthToken();
        if (token === null) {
            throw new Error(AuthService.TOKEN_EXPIRED_ERR_MSG);
        }
        return new RequestOptions({
            headers: new Headers({
                'Authorization': `JWT ${token}`
            })
        });
    }
}
