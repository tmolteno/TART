import { Component, Input } from '@angular/core';

import { FpgaStatus } from '../../models/FpgaStatus';
import { TartService } from '../../services/tart.service';

@Component({
    selector: 'app-fpga-status',
    templateUrl: './fpga-status.component.html',
    styleUrls: ['./fpga-status.component.css']
})

export class FpgaStatusComponent {

    fpgaStatus: FpgaStatus;

    @Input()
    showFpgaStatus: boolean;

    constructor(private tartService: TartService) { }

    ngOnInit() {
        this.updateFpgaStatus();
    }

    updateFpgaStatus() {
        this.tartService.getFpgaStatus()
            .subscribe(result => {
                this.fpgaStatus = result;
            });
    }

    toggleFpgaStatus(event) {
        this.showFpgaStatus = !this.showFpgaStatus;
    }
}
