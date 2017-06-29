/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { VisModeComponent } from './vis-mode.component';

describe('VisModeComponent', () => {
  let component: VisModeComponent;
  let fixture: ComponentFixture<VisModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ VisModeComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VisModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
