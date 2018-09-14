/* tslint:disable:no-unused-variable */

import { TestBed, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { TartService } from './tart.service';
import { AuthService } from './auth.service';

describe('TartServiceService', () => {
  beforeEach(() => {
      TestBed.configureTestingModule({
          imports: [HttpModule],
          providers: [
              {provide: XHRBackend, useClass: MockBackend},
              TartService,
              AuthService
          ]
      });
  });

  it('should be created', inject([TartService], (service: TartService) => {
    expect(service).toBeTruthy();
  }));

  it('should test that get channels status works as intended',
      inject([TartService, XHRBackend],
      (tartService: TartService, mockBackend: MockBackend) => {
      // setup mock backend
      mockBackend.connections.subscribe(connection => {
          connection.mockRespond(new Response(new ResponseOptions({
              body: _mockChannelStatusResponse()
          })));
      })

      tartService.getChannelsStatus().subscribe(result => {
         let jsonResponse = _mockChannelStatusResponse();
         expect(result.length).toEqual(2);

         for(let i = 0; i < result.length; i++) {
             let responseChannel = result[i];
             let jsonChannel = jsonResponse[i];

             expect(responseChannel.id).toEqual(jsonChannel.id);
             expect(responseChannel.phase).toBeDefined();
             expect(responseChannel.phase.nSamples)
                .toEqual(jsonChannel.phase.N_samples);
             expect(responseChannel.phase.measured)
                .toEqual(jsonChannel.phase.measured);
             expect(responseChannel.phase.ok).toEqual(jsonChannel.phase.ok);
             expect(responseChannel.phase.stability)
                .toEqual(jsonChannel.phase.stability);
             expect(responseChannel.phase.threshold)
                .toEqual(jsonChannel.phase.threshold);
             expect(responseChannel.radioMean).toBeDefined();
             expect(responseChannel.radioMean.mean)
                .toEqual(jsonChannel.radio_mean.mean);
             expect(responseChannel.radioMean.ok)
                .toEqual(jsonChannel.radio_mean.ok);
             expect(responseChannel.radioMean.threshold)
                .toEqual(jsonChannel.radio_mean.threshold);
         }
     });
 }));

const _mockChannelStatusResponse = () => {
    return [
        {
            "id": 1,
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
            "id": 2,
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
