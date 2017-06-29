/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { HdImgModeComponent } from './hd-img-mode.component';

describe('HdImgModeComponent', () => {
  let component: HdImgModeComponent;
  let fixture: ComponentFixture<HdImgModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ HdImgModeComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(HdImgModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
