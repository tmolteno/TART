import { TestBed, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { CalibrationService } from './calibration.service';

describe('CalibrationService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
        {provide: XHRBackend, useClass: MockBackend},
        CalibrationService
      ]
    });
  });

  it('should be created', inject([CalibrationService], (service: CalibrationService) => {
    expect(service).toBeTruthy();
  }));
});
