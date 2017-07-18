/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { TartService } from './tart.service';

describe('TartServiceService', () => {
  beforeEach(() => {
      TestBed.configureTestingModule({
      imports: [HttpModule],
          providers: [
              {provide: XHRBackend, useClass: MockBackend},
              TartService
          ]
      });
  });

  it('should test that get channels status works as intended',
      inject([TartService, XHRBackend],
      (tartService: TartService, mockBackend: MockBackend) => {
      // setup mock backend
      mockBackend.connections.subscribe(connection => {
          connection.mockRespond(new Response(new ResponseOptions({
              body: _mockChannelStatusResponse()
          })));
      })

      tartService.getChannelStatus().subscribe(result => {
         expect(result.length).toEqual(2);
     });
 }));

const _mockChannelStatusResponse = () => {
    return [
        {
            "id": 0,
            "phase": {
                "N_samples": 150,
                "measured": 4,
                "ok": 1,
                "stability": 0.98,
                "threshold": 0.95
            },
            "radio_mean": {
                "mean": 0.5646,
                "ok": 1,
                "threshold": 0.2
            }
        }, {
            "id": 1,
            "phase": {
                "N_samples": 150,
                "measured": 5,
                "ok": 1,
                "stability": 0.999,
                "threshold": 0.95
            },
            "radio_mean": {
                "mean": 0.5043,
                "ok": 1,
                "threshold": 0.2
            }
        }
    ];
};

});
