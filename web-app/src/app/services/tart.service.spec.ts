/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { TartServiceService } from './tart-service.service';

describe('TartServiceService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [TartServiceService]
    });
  });

  it('should ...', inject([TartServiceService], (service: TartServiceService) => {
    expect(service).toBeTruthy();
  }));
});
