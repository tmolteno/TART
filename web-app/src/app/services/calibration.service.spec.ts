import { TestBed, inject } from '@angular/core/testing';

import { CalibrationService } from './calibration.service';

describe('CalibrationService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CalibrationService]
    });
  });

  it('should be created', inject([CalibrationService], (service: CalibrationService) => {
    expect(service).toBeTruthy();
  }));
});
