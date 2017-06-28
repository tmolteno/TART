import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import 'rxjs/add/operator/map';

@Injectable()
export class TartService {

    apiUrl: string = 'http://tart2-raspberry:5000';

    constructor(private http: Http) { }

    getStatus() {
        return this.http.get(`${this.apiUrl}/status`)
            .map((res:Response) => {
                return res.json();
            });
    }

    getChannelStatus() {
        return this.http.get(`${this.apiUrl}/status/channel`)
            .map((res:Response) => {
                return res.json();
            });
    }

    getFpgaStatus() {
        return this.http.get(`${this.apiUrl}/status/fpga`)
            .map((res:Response) => {
                return res.json();
            });
    }
}
