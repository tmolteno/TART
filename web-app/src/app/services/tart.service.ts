import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { PlatformLocation } from '@angular/common';

import { FpgaStatus } from  '../models/FpgaStatus';

import 'rxjs/add/operator/map';

@Injectable()
export class TartService {

    apiUrl: string = '';

    constructor(
        private http: Http,
        private platformLocation: PlatformLocation

    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
    }

    getStatus() {
        return this.http.get(`${this.apiUrl}/status`)
            .map((res: Response) => {
                return res.json();
            });
    }

    getChannelStatus() {
        return this.http.get(`${this.apiUrl}/status/channel`)
            .map((res: Response) => {
                let channels = res.json();
                if (!Array.isArray(channels)) {
                    channels = [];
                }
                return channels;
            });
    }

    getFpgaStatus() {
        return this.http.get(`${this.apiUrl}/status/fpga`)
            .map((res: Response) => {
                return this.createFpgaStatus(res.json());
            });
    }

    /**
     * Creates and returns an instance of FpgaStatus from the
     * data returned by a request to the /status/fpga endpoint.
     */
    createFpgaStatus(apiResponse: any) {
        let aqStream = { data: apiResponse.AQ_STREAM.data };
        let aqSystem = {
            '512Mb': apiResponse.AQ_SYSTEM['512Mb'],
            sdram: apiResponse.AQ_SYSTEM['SDRAM ready'],
            enabled: apiResponse.AQ_SYSTEM.enabled,
            error: apiResponse.AQ_SYSTEM.error,
            overflow: apiResponse.AQ_SYSTEM.overflow,
            state: apiResponse.AQ_SYSTEM.state
        };
        let spiStats = {
            fifoOverflow: apiResponse.SPI_STATS['FIFO overflow'],
            fifoUnderrun: apiResponse.SPI_STATS['FIFO underrun'],
            spiBusy: apiResponse.SPI_STATS.spi_busy
        };
        let sysStats = {
            acqEn: apiResponse.SYS_STATS.acq_en,
            capDebug: apiResponse.SYS_STATS.cap_debug,
            capEn: apiResponse.SYS_STATS.cap_en,
            state: apiResponse.SYS_STATS.state,
            vizEn: apiResponse.SYS_STATS.viz_en,
            vizPend: apiResponse.SYS_STATS.viz_pend
        };
        let tcCentre = {
            centre: apiResponse.TC_CENTRE.centre,
            delay: apiResponse.TC_CENTRE.delay,
            drift: apiResponse.TC_CENTRE.drift,
            invert: apiResponse.TC_CENTRE.invert
        };
        let tcDebug = {
            count: apiResponse.TC_DEBUG.count,
            debug: apiResponse.TC_DEBUG.debug,
            numAntenna: apiResponse.TC_DEBUG.numantenna,
            shift: apiResponse.TC_DEBUG.shift
        };
        let tcStatus = {
            delta: apiResponse.TC_STATUS.delta,
            phase: apiResponse.TC_STATUS.phase
        };
        let tcSystem = {
            enabled: apiResponse.TC_SYSTEM.enabled,
            error: apiResponse.TC_SYSTEM.error,
            locked: apiResponse.TC_SYSTEM.locked,
            source: apiResponse.TC_SYSTEM.source
        };
        let vxDebug = {
            limp: apiResponse.VX_DEBUG.limp,
            stuck: apiResponse.VX_DEBUG.stuck
        };
        let vxStatus = {
            accessed: apiResponse.VX_STATUS.accessed,
            available: apiResponse.VX_STATUS.available,
            bank: apiResponse.VX_STATUS.bank,
            overflow: apiResponse.VX_STATUS.overflow
        };
        let vxStream = { data: apiResponse.VX_STREAM.data };
        let vxSystem = {
            blockSize: apiResponse.VX_SYSTEM.blocksize,
            enabled: apiResponse.VX_SYSTEM.enabled,
            overwrite: apiResponse.VX_SYSTEM.overwrite
        };
        return new FpgaStatus(apiResponse.hostname, apiResponse['timestamp (UTC)'],
            aqStream, aqSystem, spiStats, sysStats, tcCentre, tcDebug, tcStatus,
            tcSystem, vxDebug, vxStatus, vxStream, vxSystem);
    }
}
