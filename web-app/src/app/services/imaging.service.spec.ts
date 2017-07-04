import { TestBed, inject } from '@angular/core/testing';

import { ImagingService } from './imaging.service';

describe('ImagingService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ImagingService]
    });
  });

  it('should be created', inject([ImagingService], (service: ImagingService) => {
    expect(service).toBeTruthy();
  }));
});
