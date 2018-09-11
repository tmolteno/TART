import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { GifRecorderComponent } from './gif-recorder.component';

describe('GifRecorderComponent', () => {
  let component: GifRecorderComponent;
  let fixture: ComponentFixture<GifRecorderComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ GifRecorderComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(GifRecorderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
