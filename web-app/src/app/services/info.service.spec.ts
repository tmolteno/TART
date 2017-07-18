import { TestBed, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { InfoService } from './info.service';

describe('InfoService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
        {provide: XHRBackend, useClass: MockBackend},
        InfoService
      ]
    });
  });

  it('should be created', inject([InfoService], (service: InfoService) => {
    expect(service).toBeTruthy();
  }));
});
