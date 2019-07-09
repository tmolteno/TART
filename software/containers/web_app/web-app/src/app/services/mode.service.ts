import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs/Rx';
import { PlatformLocation } from '@angular/common';

import { Utils } from '../utils';
import { AuthService } from './auth.service';

@Injectable()
export class ModeService {

    public operatingModes: string[] = ['off', 'diag', 'raw', 'vis', 'cal'];
    private apiUrl: string = '';

    private modes$: Observable<string[]>;
    private modeObserver: any;

    constructor(
        private httpClient: HttpClient,
        private old_http: Http,
        private authService: AuthService,
        private platformLocation: PlatformLocation
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
        this.modes$ = new Observable<string[]>(observer => {
            this.modeObserver = observer;
        }).share();
    }

    getModes() {
        this.httpClient.get(`${this.apiUrl}/mode`);
    }

    getOperatingMode() {
        return this.httpClient.get(`${this.apiUrl}/mode/current`);
    }

    setOperatingMode(mode: string) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(Utils.createUnauthorizedError());
        }
        mode = mode.toLowerCase();
        if (!this.operatingModes.includes(mode)) {
            return Observable.throw(new TypeError('mode parameter is not an operating mode.'));
        }
        let options = this.authService.getAuthRequestOptions();
        return this.old_http.post(`${this.apiUrl}/mode/${mode}`, {}, options)
            .map((res: Response) => {
                return res.json();
            })
            .catch(e => {
                if (e.status === 401) {
                    return Observable.throw(Utils.createUnauthorizedError());
                } else {
                    return Observable.throw(e);
                }
            });
    }
}
