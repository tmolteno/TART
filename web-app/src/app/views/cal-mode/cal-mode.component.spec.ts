/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { CalModeComponent } from './cal-mode.component';

describe('CalModeComponent', () => {
  let component: CalModeComponent;
  let fixture: ComponentFixture<CalModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CalModeComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CalModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
