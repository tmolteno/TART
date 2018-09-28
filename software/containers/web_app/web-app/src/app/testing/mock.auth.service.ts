import { Injectable } from '@angular/core';
import { Http, Response, RequestOptions, Headers } from '@angular/http';
import { Observable } from 'rxjs/Rx';


/**
 * Mock auth service.
 * This should be provided when testing components that rely on auth service.
 */
@Injectable()
export class MockAuthService {

    private authToken: string;
    private loginObserver: any;
    private loginStatus: boolean;
    public login$: Observable<boolean>;

    constructor() {
        this.authToken = 'fake_token';
        this.loginStatus = true;
        this.login$ = new Observable<boolean>(observer => {
            this.loginObserver = observer;
        }).share();
    }

    login(password: string) {
        return Observable.create(observer => {
            console.log("in observable function...");
            let result = false;

            if (password === 'password') {
                this.setAuthToken('fake_token');
                result = true;
            }
            observer.next(result);
            observer.complete();
        });
    }

    load() {
        this.loginObserver.next();
    }

    logout() {
        this.authToken = null;
    }

    setAuthToken(token: string) {
        this.authToken = token;
    }

    isTokenValid() {
        return this.authToken !== null;
    }

    getAuthToken() {
        if (!this.isTokenValid()) {
            return null;
        }
        return this.authToken;
    }

    getAuthRequestOptions() {
        if (!this.isTokenValid()) {
            throw new Error('token not found or expired');
        }
        return new RequestOptions({
            headers: new Headers({
                'Authorization': `JWT ${this.authToken}`
            })
        });
    }
}
