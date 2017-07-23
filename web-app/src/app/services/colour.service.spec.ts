import { TestBed, inject } from '@angular/core/testing';

import { ColourServiceService } from './colour-service.service';

describe('ColourServiceService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ColourServiceService]
    });
  });

  it('should be created', inject([ColourServiceService], (service: ColourServiceService) => {
    expect(service).toBeTruthy();
  }));
});
