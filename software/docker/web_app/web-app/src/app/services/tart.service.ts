import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { PlatformLocation } from '@angular/common';
import { Observable } from 'rxjs/Rx';

import { FpgaStatus } from  '../models/FpgaStatus';
import { ChannelStatus } from '../models/ChannelStatus';

import { Utils } from '../utils';
import { AuthService } from './auth.service';
// TODO: this service should be split into channel service and fpga service (confirm this is OK)
@Injectable()
export class TartService {

    apiUrl: string = '';

    constructor(
        private http: Http,
        private authService: AuthService,
        private platformLocation: PlatformLocation
    ) {
        this.apiUrl = platformLocation.getBaseHrefFromDOM() + 'api/v1';
    }

    getChannelsStatus() {
        return this.http.get(`${this.apiUrl}/status/channel`)
            .map((res: Response) => {
                let channelsJSON = res.json();
                if (!Array.isArray(channelsJSON)) {
                    channelsJSON = [];
                }
                let channels = [];
                channelsJSON.forEach(channelJSON => {
                    channels.push(this.createChannelStatus(channelJSON));
                });
                return channels;
            });
    }

    getChannelStatus(id: number) {
        return this.http.get(`${this.apiUrl}/status/channel/${id}`)
            .map((res: Response) => {
                return this.createChannelStatus(res.json());
            });
    }

    setChannelEnabled(id: number, doEnable: boolean) {
        if (!this.authService.isTokenValid()) {
            return Observable.throw(Utils.createUnauthorizedError());
        }
        let options = this.authService.getAuthRequestOptions();
        return this.http.put(`${this.apiUrl}/channel/${id}/${doEnable? 1 : 0}`,
            {}, options).map((res: Response) => {
                return res.json();
            })
            .catch(e => {
                if (e.status === 401) {
                    return Observable.throw(Utils.createUnauthorizedError());
                } else {
                    return Observable.throw(e);
                }
            });
    }

    getFpgaStatus() {
        return this.http.get(`${this.apiUrl}/status/fpga`)
            .map((res: Response) => {
                return this.createFpgaStatus(res.json());
            });
    }

    /**
     * Creates and returns a ChannelStatus instance from data returned
     * in an array from the /status/channel endpoint.
     */
    createChannelStatus(channelJSON: any) {
        let phase = {
            nSamples: channelJSON.phase.N_samples,
            measured: channelJSON.phase.measured,
            ok: channelJSON.phase.ok,
            stability: channelJSON.phase.stability,
            threshold: channelJSON.phase.threshold
        };
        let radioMean = {
            mean: channelJSON.radio_mean.mean,
            ok: channelJSON.radio_mean.ok,
            threshold: channelJSON.radio_mean.threshold
        };
        return new ChannelStatus(channelJSON.id, !!channelJSON.enabled, phase,
            radioMean, channelJSON.freq, channelJSON.power);
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
