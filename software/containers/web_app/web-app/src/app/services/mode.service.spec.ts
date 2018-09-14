import { TestBed, inject } from '@angular/core/testing';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { ModeService } from './mode.service';
import { AuthService } from './auth.service';

describe('ModeService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
        {provide: XHRBackend, useClass: MockBackend},
        ModeService,
        AuthService
      ]
    });
  });

  it('should be created', inject([ModeService], (service: ModeService) => {
    expect(service).toBeTruthy();
  }));
});
