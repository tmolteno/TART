/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { NouisliderModule } from 'ng2-nouislider';
import { VisiblesConfigSliderComponent } from './visibles-config-slider.component';

describe('VisiblesConfigSliderComponent', () => {
  let component: VisiblesConfigSliderComponent;
  let fixture: ComponentFixture<VisiblesConfigSliderComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [NouisliderModule],
        declarations: [VisiblesConfigSliderComponent]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VisiblesConfigSliderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
