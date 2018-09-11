import {
    Component,
    Input,
    Output,
    EventEmitter
} from '@angular/core';

import { ChannelStatus } from '../../models/ChannelStatus';

@Component({
    selector: 'app-channels-status',
    templateUrl: './channels-status.component.html',
    styleUrls: ['./channels-status.component.css']
})
export class ChannelsStatusComponent {

    @Input()
    showChannelsStatus: boolean;

    @Input()
    channelsStatus: ChannelStatus[] = [];

    @Output()
    antennaEnabledChange = new EventEmitter();

    toggleChannelStatus(event) {
        this.showChannelsStatus = !this.showChannelsStatus;
    }

    onChannelEnabledChange(event) {
        this.antennaEnabledChange.emit({id: event.id,
            enabled: event.enabled});
    }
}
