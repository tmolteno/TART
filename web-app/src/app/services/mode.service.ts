import { Injectable } from '@angular/core';
import { Http, Response, RequestOptions, Headers } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { environment } from '../../environments/environment';

import { AuthService } from './auth.service';

@Injectable()
export class ModeService {

    public operatingModes: string[] = ['off', 'diag', 'raw', 'vis', 'cal'];
    private apiUrl: string = '';

    private modes$: Observable<string[]>;
    private modeObserver: any;

    constructor(
        private http: Http,
        private authService: AuthService
    ) {
        this.apiUrl = environment.apiUrl;
        this.modes$ = new Observable(observer => {
            this.modeObserver = observer;
        }).share();
    }

    getModes() {
        return this.http.get(`${this.apiUrl}/mode`)
            .map((res: Response) => {
                return res.json().modes;
            });
    }

    getOperatingMode() {
        return this.http.get(`${this.apiUrl}/mode/current`)
            .map((res: Response) => {
                return res.json().mode;
            })
    }

    setOperatingMode(mode: string) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(new Error('token expired'));
        }
        mode = mode.toLowerCase();
        if (!this.operatingModes.includes(mode)) {
            return Observable.throw(new TypeError('mode parameter is not an operating mode.'));
        }
        let options = this.authService.getAuthRequestOptions();
        return this.http.post(`${this.apiUrl}/mode/${mode}`, {}, options)
            .map((res: Response) => {
                return res.json();
            });
    }
}
