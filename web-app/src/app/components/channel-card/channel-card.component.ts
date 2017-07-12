import { Component, Input } from '@angular/core';

import { ChannelStatus } from '../../models/ChannelStatus';

@Component({
    selector: 'app-channel-card',
    templateUrl: './channel-card.component.html',
    styleUrls: ['./channel-card.component.css']
})
export class ChannelCardComponent {

    @Input()
    channel : ChannelStatus;

    constructor() { }

}
