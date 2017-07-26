import { Component } from '@angular/core';

import { ImagingService } from '../../services/imaging.service';
import { CalibrationService } from '../../services/calibration.service';

import { Observable } from 'rxjs/Observable';
import * as moment from 'moment/moment';
import 'rxjs/add/observable/forkJoin';

@Component({
    selector: 'app-vis-calibration',
    templateUrl: './vis-calibration.component.html',
    styleUrls: ['./vis-calibration.component.css']
})
export class VisCalibrationComponent {

    elevationSliderLabel: string = 'Elevation';
    elevationSliderMin: number = 30;
    elevationSliderMax: number = 90;
    elevationSliderStepSize: number = 10;
    elevationSliderValue: number = 90;

    azimuthSliderLabel: string = 'Azimuth Degrees';
    azimuthSliderMin: number = 0;
    azimuthSliderMax: number = 360;
    azimuthSliderStepSize: number = 45;
    azimuthSliderValue: number = 0;


    calMeasurementCounter: number = 1;
    calMeasurements: any[] = [];

    constructor(
        private imagingService: ImagingService,
        private calibrationService: CalibrationService) {
    }

    onElevationChanged(value) {
        this.elevationSliderValue = value;
    }

    onAzimuthChanged(value) {
        this.azimuthSliderValue = value;
    }

    onAddCalClicked(event) {
        Observable.forkJoin([
            this.imagingService.getVis(),
            this.imagingService.getTimestamp()
        ]).subscribe(result => {
            let vis = result[0];
            let timestamp = result[1];
            let calMeasurement = this.generateCalMeasurement(vis, timestamp);
            this.calMeasurements.push(calMeasurement);
        })
    }

    generateCalMeasurement(vis: any, timestamp: string) {
        return {
            no: this.calMeasurementCounter++,
            el: this.elevationSliderValue,
            az: this.azimuthSliderValue,
            data: {
                vis: vis,
                timestamp: timestamp
            }
        };
    }

    onExportCalClicked(event) {
        this.calibrationService.startCalibration(this.calMeasurements);
    }

    onRemoveCalClicked(calId) {
        this.calMeasurements = this.calMeasurements
            .filter(cal => cal.no != calId);
    }
}
