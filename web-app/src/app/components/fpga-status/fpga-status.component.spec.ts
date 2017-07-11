import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { FpgaStatusComponent } from './fpga-status.component';

describe('FpgaStatusComponent', () => {
  let component: FpgaStatusComponent;
  let fixture: ComponentFixture<FpgaStatusComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ FpgaStatusComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FpgaStatusComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
