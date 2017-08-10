import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { PlatformLocation } from '@angular/common';
import 'rxjs/add/operator/map';

@Injectable()
export class ImagingService {

    apiUrl: string = '';

    constructor(
        private http: Http,
        private platformLocation: PlatformLocation
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
    }

    getVis() {
        return this.http.get(`${this.apiUrl}/imaging/vis`)
            .map((res: Response) => {
                return res.json();
            });
    }

    getAntennaPositions() {
        return this.http.get(`${this.apiUrl}/imaging/antenna_positions`)
            .map((res: Response) => {
                return res.json();
            });
    }
}
