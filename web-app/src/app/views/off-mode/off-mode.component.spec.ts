/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { OffModeComponent } from './off-mode.component';

describe('OffModeComponent', () => {
  let component: OffModeComponent;
  let fixture: ComponentFixture<OffModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ OffModeComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OffModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
