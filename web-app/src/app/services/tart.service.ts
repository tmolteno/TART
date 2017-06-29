import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { environment } from '../../environments/environment';
import 'rxjs/add/operator/map';

@Injectable()
export class TartService {

    apiUrl: string = '';

    constructor(private http: Http) {
        this.apiUrl = environment.apiUrl;
    }

    getStatus() {
        return this.http.get(`${this.apiUrl}/status`)
            .map((res: Response) => {
                return res.json();
            });
    }

    getChannelStatus() {
        return this.http.get(`${this.apiUrl}/status/channel`)
            .map((res: Response) => {
                let channels = res.json();
                if (!Array.isArray(channels)) {
                    channels = [];
                }
                return channels;
            });
    }

    getFpgaStatus() {
        return this.http.get(`${this.apiUrl}/status/fpga`)
            .map((res: Response) => {
                return res.json();
            });
    }
}
