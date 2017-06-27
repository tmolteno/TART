import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import 'rxjs/add/operator/map';

@Injectable()
export class TartService {

  constructor(private http: Http) { }

  getStatus() {
      return this.http.get(`http://tart2-raspberry:5000/status`)
        .map((res:Response) => {
            return res.json();
        });
  }

  getChannelStatus() {
      return this.http.get(`http://tart2-raspberry:5000/status/channel`)
        .map((res:Response) => {
            return res.json();
        });
  }

  getFpgaStatus() {
      return this.http.get(`http://tart2-raspberry:5000/status/fpga`)
        .map((res:Response) => {
            return res.json();
        });
  }
}
