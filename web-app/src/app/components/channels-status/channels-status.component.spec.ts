import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChannelsStatusComponent } from './channels-status.component';

describe('ChannelsStatusComponent', () => {
  let component: ChannelsStatusComponent;
  let fixture: ComponentFixture<ChannelsStatusComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChannelsStatusComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChannelsStatusComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
