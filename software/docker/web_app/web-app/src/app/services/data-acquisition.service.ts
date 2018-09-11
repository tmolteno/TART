import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { PlatformLocation } from '@angular/common';

import { Utils } from '../utils';
import { AuthService } from './auth.service';

@Injectable()
export class DataAcquisitionService {

    apiUrl: string = '';

    minNumSamplesExp: number = 16;
    maxNumSamplesExp: number = 24;

    constructor(
        private http: Http,
        private authService: AuthService,
        private platformLocation: PlatformLocation
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
    }

    getRawNumSamplesExp() {
        return this.http.get(`${this.apiUrl}/acquire/raw/num_samples_exp`)
            .map((res: Response) => {
                return res.json()['N_samples_exp'];
            });
    }

    setRawNumSamplesExp(value: number) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(Utils.createUnauthorizedError());
        }
        if (value < this.minNumSamplesExp || value > this.maxNumSamplesExp) {
            return Observable.throw(new RangeError('Number of samples exp is out of range'));
        }
        let options = this.authService.getAuthRequestOptions();

        return this.http.put(`${this.apiUrl}/acquire/raw/num_samples_exp/${value}`, {},
            options).map((res: Response) => {
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

    getVisNumSamplesExp() {
        return this.http.get(`${this.apiUrl}/acquire/vis/num_samples_exp`)
            .map((res: Response) => {
                return res.json()['N_samples_exp'];
            });
    }

    setVisNumSamplesExp(value: number) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(Utils.createUnauthorizedError());
        }
        if (value < this.minNumSamplesExp || value > this.maxNumSamplesExp) {
            return Observable.throw(new RangeError('Number of samples exp is out of range'));
        }
        let options = this.authService.getAuthRequestOptions();

        return this.http.put(`${this.apiUrl}/acquire/vis/num_samples_exp/${value}`, {},
            options).map((res: Response) => {
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

    getRawFilePaths() {
        return this.http.get(`${this.apiUrl}/raw/data`)
            .map((res: Response) => {
                return res.json();
            });
    }

    getVisFilePaths() {
        return this.http.get(`${this.apiUrl}/vis/data`)
            .map((res: Response) => {
                return res.json();
            });
    }

    getRawSaveFlag() {
        return this.http.get(`${this.apiUrl}/acquire/raw/save`)
            .map((res: Response) => {
                return !!res.json().save;
            });
    }

    setRawSaveFlag(doSave: boolean) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(Utils.createUnauthorizedError());
        }
        let options = this.authService.getAuthRequestOptions();
        return this.http.put(`${this.apiUrl}/acquire/raw/save/${doSave ? 1 : 0}`, {},
            options).map((res: Response) => {
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

    getVisSaveFlag() {
        return this.http.get(`${this.apiUrl}/acquire/vis/save`)
            .map((res: Response) => {
                return !!res.json().save;
            });
    }

    setVisSaveFlag(doSave: boolean) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(Utils.createUnauthorizedError());
        }
        let options = this.authService.getAuthRequestOptions();
        return this.http.put(`${this.apiUrl}/acquire/vis/save/${doSave ? 1 : 0}`, {},
            options).map((res: Response) => {
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
