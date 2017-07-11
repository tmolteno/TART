import { Component, Input } from '@angular/core';

import { TartService } from '../../services/tart.service';

@Component({
    selector: 'app-fpga-status',
    templateUrl: './fpga-status.component.html',
    styleUrls: ['./fpga-status.component.css']
})

export class FpgaStatusComponent {

    fpgaStatus: Object = {};

    @Input()
    showFpgaStatus: boolean;

    constructor(private tartService: TartService) { }

    ngOnInit() {
        this.tartService.getFpgaStatus()
            .subscribe(result => {
                this.fpgaStatus = result;
            });
    }

    toggleFpgaStatus(event) {
        this.showFpgaStatus = !this.showFpgaStatus;
    }

    getType(item) {
        return typeof item;
    }
}
