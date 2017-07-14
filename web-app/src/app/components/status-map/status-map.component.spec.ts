import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { StatusMapComponent } from './status-map.component';

describe('StatusMapComponent', () => {
  let component: StatusMapComponent;
  let fixture: ComponentFixture<StatusMapComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ StatusMapComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(StatusMapComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
