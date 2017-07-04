import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { environment } from '../../environments/environment';
import 'rxjs/add/operator/map';

@Injectable()
export class ImagingService {

    apiUrl: string = '';

    constructor(private http: Http) {
        this.apiUrl = environment.apiUrl;
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
