import { Injectable } from '@angular/core';
import { Http, Response, RequestOptions, Headers } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { PlatformLocation } from '@angular/common';
import 'rxjs/add/operator/map';

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
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1/acquire';
    }

    getRawNumSamplesExp() {
        return this.http.get(`${this.apiUrl}/raw/num_samples_exp`)
            .map((res: Response) => {
                return res.json()['N_samples_exp'];
            });
    }

    setRawNumSamplesExp(value: number) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(new Error('token expired'));
        }
        if (value < this.minNumSamplesExp || value > this.maxNumSamplesExp) {
            return Observable.throw(new RangeError('Number of samples exp is out of range'));
        }
        let options = this.authService.getAuthRequestOptions();

        return this.http.put(`${this.apiUrl}/raw/num_samples_exp/${value}`, {},
            options).map((res: Response) => {
                return res.json();
            });
    }

    getVisNumSamplesExp() {
        return this.http.get(`${this.apiUrl}/vis/num_samples_exp`)
            .map((res: Response) => {
                return res.json()['N_samples_exp'];
            });
    }

    setVisNumSamplesExp(value: number) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(new Error('token expired'));
        }
        if (value < this.minNumSamplesExp || value > this.maxNumSamplesExp) {
            return Observable.throw(new RangeError('Number of samples exp is out of range'));
        }
        let options = this.authService.getAuthRequestOptions();

        return this.http.put(`${this.apiUrl}/vis/num_samples_exp/${value}`, {},
            options).map((res: Response) => {
                return res.json();
            });
    }

    getRawSaveFlag() {
        return this.http.get(`${this.apiUrl}/raw/save`)
            .map((res: Response) => {
                return !!res.json().save;
            });
    }

    setRawSaveFlag(doSave: boolean) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(new Error('token expired'));
        }
        let options = this.authService.getAuthRequestOptions();
        return this.http.put(`${this.apiUrl}/raw/save/${doSave ? 1 : 0}`, {},
            options).map((res: Response) => {
                return res.json();
            });
    }

    getVisSaveFlag() {
        return this.http.get(`${this.apiUrl}/vis/save`)
            .map((res: Response) => {
                return !!res.json().save;
            });
    }

    setVisSaveFlag(doSave: boolean) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(new Error('token expired'));
        }
        let options = this.authService.getAuthRequestOptions();
        return this.http.put(`${this.apiUrl}/vis/save/${doSave ? 1 : 0}`, {},
            options).map((res: Response) => {
                return res.json();
            });
    }
}
