import { Injectable } from '@angular/core';
import { Http, Response, RequestOptions, Headers } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { PlatformLocation } from '@angular/common';

@Injectable()
export class InfoService {

    private apiUrl: string = '';

    constructor(
        private http: Http,
        private platformLocation: PlatformLocation
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
    }

    getInfo() {
        return this.http.get(`${this.apiUrl}/info`)
            .map((res: Response) => {
                return res.json().info;
            });
    }
}
