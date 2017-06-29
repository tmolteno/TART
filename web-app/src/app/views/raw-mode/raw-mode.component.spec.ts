/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { RawModeComponent } from './raw-mode.component';

describe('RawModeComponent', () => {
  let component: RawModeComponent;
  let fixture: ComponentFixture<RawModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ RawModeComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RawModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
