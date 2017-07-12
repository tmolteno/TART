import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChannelCardComponent } from './channel-card.component';

describe('ChannelCardComponent', () => {
  let component: ChannelCardComponent;
  let fixture: ComponentFixture<ChannelCardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChannelCardComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChannelCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
