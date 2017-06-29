/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { ModeService } from './mode.service';

describe('ModeService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ModeService]
    });
  });

  it('should ...', inject([ModeService], (service: ModeService) => {
    expect(service).toBeTruthy();
  }));
});
