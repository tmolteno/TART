import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { VisCalibrationComponent } from './vis-calibration.component';

describe('VisCalibrationComponent', () => {
  let component: VisCalibrationComponent;
  let fixture: ComponentFixture<VisCalibrationComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ VisCalibrationComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VisCalibrationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
