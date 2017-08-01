import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { PlatformLocation } from '@angular/common';
import { Observable } from 'rxjs/Rx';

import { Utils } from '../utils';
import { AuthService } from './auth.service';

@Injectable()
export class CalibrationService {

    apiUrl: string = '';

    constructor(
        private http: Http,
        private platformLocation: PlatformLocation,
        private authService: AuthService
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
    }

    getGain() {
        return this.http.get(`${this.apiUrl}/calibration/gain`)
            .map((res: Response) => {
                return res.json();
            });
    }

    startCalibration(calibrationConfig: any) {
        let options = this.authService.getAuthRequestOptions();

        return this.http.post(`${this.apiUrl}/calibrate`, calibrationConfig,
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
