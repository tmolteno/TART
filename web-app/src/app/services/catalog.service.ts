import { Injectable } from '@angular/core';
import { Http, Response, URLSearchParams } from '@angular/http';
import { PlatformLocation } from '@angular/common';
import 'rxjs/add/operator/map';
import * as moment from 'moment/moment';

@Injectable()
export class CatalogService {

    private apiUrl: string = 'https://tart.elec.ac.nz/catalog';

    constructor(private http: Http) { }

    getSatellites(lat: number, lon: number, date = new Date()) {
        // TODO: get search params
        let getQuery = new URLSearchParams();
        getQuery.set('date', moment(date).format());
        getQuery.set('lat', String(lat));
        getQuery.set('lon', String(lon));

        return this.http.get(`${this.apiUrl}/catalog`, {
            search: getQuery
        }).map((res: Response) => {
                return res.json();
            });
    }
}
