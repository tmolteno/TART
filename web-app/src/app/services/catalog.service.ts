import { Injectable } from '@angular/core';
import { Http, Response, URLSearchParams } from '@angular/http';
import { PlatformLocation } from '@angular/common';
import 'rxjs/add/operator/map';

@Injectable()
export class CatalogService {

    private apiUrl: string = 'https://tart.elec.ac.nz/catalog';

    constructor(private http: Http) { }

    getSatellites() {//lat: number, lon: number, date = new Date()) {
        // TODO: get search params
        return this.http.get(`${this.apiUrl}/catalog?lat=-45.85&lon=170.5`)
            .map((res: Response) => {
                return res.json();
            });
    }
}
