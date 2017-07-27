import {
    Component,
    Input,
    Output,
    ViewChild,
    ElementRef,
    EventEmitter
} from '@angular/core';

import { ChannelStatus } from '../../models/ChannelStatus';

@Component({
    selector: 'app-channel-card',
    templateUrl: './channel-card.component.html',
    styleUrls: ['./channel-card.component.css']
})
export class ChannelCardComponent {
    @ViewChild('channelCardCheckbox') channelCardCheckbox : ElementRef;

    @Input()
    channel : ChannelStatus;

    @Output()
    antennaEnabledChange = new EventEmitter();

    constructor() { }

    ngAfterViewInit() {
        this.channelCardCheckbox.nativeElement.checked = this.channel.enabled;
    }

    onChannelEnabledChanged(event) {
        this.channel.enabled = this.channelCardCheckbox.nativeElement.checked;
        this.antennaEnabledChange.emit({id: this.channel.id,
            enabled: this.channel.enabled});
    }
}
