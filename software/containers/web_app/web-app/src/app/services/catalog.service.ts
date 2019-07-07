import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { PlatformLocation } from '@angular/common';
import 'rxjs/add/operator/map';
import * as moment from 'moment/moment';

@Injectable()
export class CatalogService {

    private apiUrl: string = 'https://tart.elec.ac.nz/catalog';

    constructor(private http: HttpClient) { }

    getSatellites(lat: number, lon: number, date = new Date()) {
        let headers = new HttpHeaders();
        
        let params = new HttpParams();
        params = params.append('date', moment(date).format());
        params = params.append('lat', String(lat));
        params = params.append('lon', String(lon));

        return this.http.get(`${this.apiUrl}/catalog`, { headers, params });
    }
}
