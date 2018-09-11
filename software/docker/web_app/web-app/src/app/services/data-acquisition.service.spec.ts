import { TestBed, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { DataAcquisitionService } from './data-acquisition.service';
import { AuthService } from './auth.service';

describe('DataAcquisitionService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
          {provide: XHRBackend, useClass: MockBackend},
          DataAcquisitionService,
          AuthService
      ]
    });
  });

  it('should be created', inject([DataAcquisitionService, AuthService], (service: DataAcquisitionService) => {
    expect(service).toBeTruthy();
  }));
});
