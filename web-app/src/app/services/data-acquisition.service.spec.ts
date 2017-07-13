import { TestBed, inject } from '@angular/core/testing';

import { DataAcquisitionService } from './data-acquisition.service';

describe('DataAcquisitionService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [DataAcquisitionService]
    });
  });

  it('should be created', inject([DataAcquisitionService], (service: DataAcquisitionService) => {
    expect(service).toBeTruthy();
  }));
});
