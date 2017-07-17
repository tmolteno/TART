import { Component, Input, ElementRef, ViewChild, Renderer, Directive } from '@angular/core';

import { Antenna } from '../../models/Antenna';
import { ChannelStatus } from '../../models/ChannelStatus';

@Component({
    selector: 'app-status-map',
    templateUrl: './status-map.component.html',
    styleUrls: ['./status-map.component.css']
})

@Directive({
    selector: '[canvasRenderer]'
})
export class StatusMapComponent {
    @ViewChild('statusMapCanvas') statusMapCanvas: ElementRef;
    private canvasElement: Node;

    antennaPopupId: string = 'antenna-popup';
    canvasFontStyle: string = '0.8em arial';
    // Antenna colours
    antennaOkColour: string = "rgba(92, 184, 92, 0.8)";
    antennaErrorColor: string = "rgba(217, 83, 79, 0.8)";

    // position on canvas that translates to the 0 position in antennaPositions
    mapZeroXCanvasPosition: number = 250;
    mapZeroYCanvasPosition: number = 250;
    // radius of antenna marker
    antennaRadius: number = 10;
    // canvas padding
    canvasPaddingX: number = 14;
    canvasPaddingY: number = 14;

    @Input()
    channelsStatus: ChannelStatus[] = [];

    @Input()
    antennaPositions: any[] = [];

    canvasAntennas: Antenna[] = [];

    constructor(
        private renderer: Renderer,
        private element: ElementRef) {

        this.canvasElement = element.nativeElement;
    }

    ngOnInit() {
        this.initMapCanvasClickListener();
    }

    initMapCanvasClickListener() {
        this.statusMapCanvas.nativeElement.addEventListener('click', event => {
            let x = event.pageX - this.statusMapCanvas.nativeElement.offsetLeft;
            let y = event.pageY - this.statusMapCanvas.nativeElement.offsetTop;

            let oldPopup = document.getElementById(this.antennaPopupId);
            if (oldPopup) {
                oldPopup.remove();
            }

            this.canvasAntennas.forEach(antenna => {
                if (antenna.isClicked(x, y)) {
                    if (antenna.antennaStatus.radioMean.ok === 0 ||
                        antenna.antennaStatus.phase.ok === 0) {
                        this.displayAntennaPopup(event.pageX, event.pageY, antenna);
                    }
                }
            });
        })
    }

    displayAntennaPopup(absolutePositionX: number, absolutePositionY: number,
        antenna: Antenna) {
        let antennaStatus = antenna.antennaStatus;
        let popupText = '';

        if (antennaStatus.radioMean.ok === 0) {
            popupText = popupText + 'Radio Mean: ' + antennaStatus.radioMean.mean;
        }

        if (antennaStatus.phase.ok === 0) {
            if (popupText.length > 0) {
                popupText = popupText + '\n';
            }
            popupText = popupText + 'Phase Stability: ' + antennaStatus.phase.stability;
        }

        let antennaPopup = this.renderer.createElement(this.canvasElement, 'div');
        this.renderer.createText(antennaPopup, popupText);
        this.renderer.setElementAttribute(antennaPopup, 'id', this.antennaPopupId);
        this.renderer.setElementStyle(antennaPopup, 'position', 'absolute');
        this.renderer.setElementStyle(antennaPopup, 'left', `${absolutePositionX}px`);
        this.renderer.setElementStyle(antennaPopup, 'top', `${absolutePositionY}px`);
        this.renderer.setElementStyle(antennaPopup, 'background-color', 'white');
        this.renderer.setElementStyle(antennaPopup, 'cursor', 'default');
        this.renderer.setElementStyle(antennaPopup, 'padding', '4px');
        this.renderer.setElementStyle(antennaPopup, 'border', '1px solid black');
        this.renderer.setElementStyle(antennaPopup, 'font-size', '0.8em');
    }


    ngOnChanges() {
        if (!this.antennaPositions || !this.channelsStatus) {
            return;
        }
        this.canvasAntennas = this.createAntennas(this.antennaPositions,
            this.channelsStatus);
        this.drawMeterLength(this.antennaPositions);
        //this.drawCompass();
        this.drawAntennaPositions(this.canvasAntennas);
    }

    createAntennas(antennaPositions, channelStatuses:  ChannelStatus[]) {
        let antennas = [];
        if (antennaPositions.length < 1 || channelStatuses.length < 1) {
            return antennas;
        }

        let drawPositionMod = this.getAntennaDrawPositionModifier(antennaPositions);
        // TODO:  antenna xyz coordinate indexes should be defined by the service that supplies the antenna positions.
        channelStatuses.forEach(channelStatus => {
            let antennaPos = antennaPositions[channelStatus.id];
            // calculate draw position of antenna
            let drawX = (drawPositionMod * antennaPos[0])
                + this.mapZeroXCanvasPosition;
            let drawY = (drawPositionMod * antennaPos[1] * -1)
                + this.mapZeroYCanvasPosition;
            // create new antenna and add to antennas
            let antenna = new Antenna(antennaPos[0], antennaPos[1], antennaPos[2],
                drawX, drawY, this.antennaRadius, channelStatus);
            antennas.push(antenna);
        })
        return antennas;
    }

    getAntennaDrawPositionModifier(antennaPositions) {
        let drawCanvas = this.statusMapCanvas.nativeElement;
        let xCoordIndex = 0;
        let yCoordIndex = 1;
        let numAntennaPos = antennaPositions.length;
        // calculate maximum usable area in map (should be (size / 2) - some offset  )
        let xBounds = (drawCanvas.width / 2) - this.canvasPaddingX;
        let yBounds = (drawCanvas.height / 2) - this.canvasPaddingY;
        // calculate max and min x and y position
        let minX = antennaPositions[0][xCoordIndex];
        let maxX = antennaPositions[0][xCoordIndex];

        let minY = antennaPositions[0][yCoordIndex];
        let maxY = antennaPositions[0][yCoordIndex];

        for (let i = 1; i < numAntennaPos; i++) {
            if (antennaPositions[i][xCoordIndex] < minX) {
                minX = antennaPositions[i][xCoordIndex];
            } else if (antennaPositions[i][xCoordIndex] > maxX) {
                maxX = antennaPositions[i][xCoordIndex];
            }

            if (antennaPositions[i][yCoordIndex] < minY) {
                minY = antennaPositions[i][yCoordIndex];
            } else if (antennaPositions[i][yCoordIndex] > maxY) {
                maxY = antennaPositions[i][yCoordIndex];
            }
        }
        // calculate max in x and y position
        if (minX < 0) {
            minX *= -1;
            maxX = Math.max(minX, maxX);
        }
        if (minY < 0) {
            minY *= -1;
            maxY = Math.max(minY, maxY);
        }

        let xMod = xBounds / (maxX);
        let yMod = yBounds / (maxY);
        let smallestMod = xMod > yMod? yMod : xMod;
        return smallestMod;
    }

    drawAntennaPositions(antennas: Antenna[]) {
        let drawCanvas = this.statusMapCanvas.nativeElement;
        let ctx = drawCanvas.getContext('2d');
        // draw each antenna
        antennas.forEach(antenna => {
            ctx.beginPath();
            // fill antenna
            ctx.fillStyle = this.getAntennaDrawColour(antenna);
            ctx.ellipse(antenna.drawX , antenna.drawY, antenna.drawRadius,
                antenna.drawRadius, Math.PI / 180, 0, 2 * Math.PI);
            ctx.fill();
            // write antenna id
            ctx.font = this.canvasFontStyle;
            ctx.fillStyle= 'black';
            let textDrawX = antenna.antennaStatus.id < 10 ?
                antenna.drawX - (antenna.drawRadius / 3) :
                antenna.drawX - (antenna.drawRadius / 1.5);
            let textDrawY = antenna.drawY + (antenna.drawRadius / 2);
            ctx.fillText(antenna.antennaStatus.id, textDrawX, textDrawY);
            // draw antenna border
            ctx.beginPath();
            ctx.ellipse(antenna.drawX , antenna.drawY, antenna.drawRadius,
                antenna.drawRadius, Math.PI / 180, 0, 2 * Math.PI);
            ctx.stroke();
        });
    }

    drawMeterLength(antennaPositions) {
        if(antennaPositions.length < 1) {
            return;
        }
        let drawCanvas = this.statusMapCanvas.nativeElement;
        let ctx = drawCanvas.getContext('2d');
        ctx.save();
        ctx.strokeStyle = 'black';
        ctx.translate(0.5, 0.5);    // otherwise lines appear blurry

        let drawMod = this.getAntennaDrawPositionModifier(antennaPositions);
        let meterStartX =  this.canvasPaddingX;
        let meterEndX = Math.floor(meterStartX + drawMod);
        // draw meter line
        ctx.beginPath();
        ctx.moveTo(meterStartX, 480)
        ctx.lineTo(meterEndX, 480);
        ctx.stroke();
        //draw end lines
        ctx.beginPath();
        ctx.moveTo(meterStartX, 470);
        ctx.lineTo(meterStartX, 490);
        ctx.stroke();
        ctx.beginPath();
        ctx.moveTo(meterEndX, 470);
        ctx.lineTo(meterEndX, 490);
        ctx.stroke();
        // draw trianges
        ctx.fillStyle = 'black';
        ctx.beginPath();
        ctx.moveTo(meterStartX, 480);
        ctx.lineTo(meterStartX + 8, 470);
        ctx.lineTo(meterStartX + 8, 490);
        ctx.fill();
        ctx.beginPath();
        ctx.moveTo(meterEndX, 480);
        ctx.lineTo(meterEndX - 8, 470);
        ctx.lineTo(meterEndX - 8, 490);
        ctx.fill();
        // write 1m
        ctx.font = "1em arial";
        let textDrawX = Math.floor(meterEndX / 2);
        let textDrawY = 475;
        ctx.fillText("1m", textDrawX, textDrawY);
        ctx.restore();
    }

    drawCompass() {
        // TODO: this isn't ready yet...
        let drawCanvas = this.statusMapCanvas.nativeElement;
        let ctx = drawCanvas.getContext('2d');
        ctx.save();
        ctx.strokeStyle = 'black';
        ctx.translate(0.5, 0.5);    // otherwise lines appear blurry

        ctx.beginPath();
        ctx.moveTo(450, 20);
        ctx.lineTo(450, 100);
        ctx.stroke();
        ctx.restore();
    }

    getAntennaDrawColour(antenna: Antenna) {
        let status = antenna.antennaStatus;
        if (status.phase.ok === 0 || status.radioMean.ok === 0) {
            return this.antennaErrorColor;
        }
        return this.antennaOkColour;
    }
}
