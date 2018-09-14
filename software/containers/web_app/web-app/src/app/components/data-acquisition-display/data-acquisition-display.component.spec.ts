import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { DataAcquisitionDisplayComponent } from './data-acquisition-display.component';

describe('DataAcquisitionDisplayComponent', () => {
  let component: DataAcquisitionDisplayComponent;
  let fixture: ComponentFixture<DataAcquisitionDisplayComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ DataAcquisitionDisplayComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DataAcquisitionDisplayComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
