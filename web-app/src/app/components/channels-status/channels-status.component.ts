import { Component, Input } from '@angular/core';

import { ChannelStatus } from '../../models/ChannelStatus';
import { TartService } from '../../services/tart.service';

@Component({
    selector: 'app-channels-status',
    templateUrl: './channels-status.component.html',
    styleUrls: ['./channels-status.component.css']
})
export class ChannelsStatusComponent {

    @Input()
    showChannelsStatus: boolean;

    channelsStatus: ChannelStatus[] = [];

    constructor(private tartService: TartService) { }

    ngOnInit() {
        this.getChannelStatus();
    }

    getChannelStatus() {
        this.tartService.getChannelStatus()
            .subscribe(result => {
                this.channelsStatus = result;
            });
    }

    toggleChannelStatus(event) {
        this.showChannelsStatus = !this.showChannelsStatus;
    }
}
