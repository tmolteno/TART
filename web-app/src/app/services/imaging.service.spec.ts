import { TestBed, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { ImagingService } from './imaging.service';

describe('ImagingService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
        {provide: XHRBackend, useClass: MockBackend},
        ImagingService
      ]
    });
  });

  it('should be created', inject([ImagingService], (service: ImagingService) => {
    expect(service).toBeTruthy();
  }));
});
