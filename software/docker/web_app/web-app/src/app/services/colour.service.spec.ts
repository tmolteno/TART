import { TestBed, inject } from '@angular/core/testing';

import { ColourService } from './colour.service';

describe('ColourService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ColourService]
    });
  });

  it('should be created', inject([ColourService], (service: ColourService) => {
    expect(service).toBeTruthy();
  }));
});
