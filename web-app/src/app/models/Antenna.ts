import { ChannelStatus } from './ChannelStatus';
/**
 * Status Map canvas Antenna entity.
 * @author Tim Miller
 */
 export class Antenna {
    private _xPos: number;
    private _yPos: number;
    private _zPos: number;
    private _drawX: number;
    private _drawY: number;
    private _drawRadius: number;

    _antennaStatus: ChannelStatus;

    constructor(xPos: number,yPos: number, zPos: number, drawX: number,
                 drawY: number, drawRadius: number, antennaStatus: ChannelStatus) {
        this._xPos = xPos;
        this._yPos = yPos;
        this._zPos = zPos;
        this._drawX = drawX;
        this._drawY = drawY;
        this._drawRadius = drawRadius;
        this._antennaStatus = antennaStatus;
    }

    get xPos(): number {
        return this._xPos;
    }

    get yPos(): number {
        return this._yPos;
    }

    get zPos(): number {
        return this._zPos;
    }

    get drawX(): number {
        return this._drawX;
    }

    set drawX(xPos: number) {
        this._drawX = xPos;
    }

    get drawY(): number {
        return this._drawY;
    }

    set drawY(yPos: number) {
        this._drawY = yPos;
    }

    get drawRadius(): number {
        return this._drawRadius;
    }

    set drawRadius(drawRadius: number) {
        this._drawRadius = drawRadius;
    }

    get antennaStatus(): ChannelStatus {
        return this._antennaStatus;
    }

    set antennaStatus(antennaStatus: ChannelStatus) {
        this._antennaStatus = antennaStatus;
    }

    isClicked(clickX: number, clickY: number): boolean {
        let squareDist = Math.pow(clickX - this._drawX, 2) + Math.pow(clickY - this._drawY, 2);
        return squareDist < Math.pow(this._drawRadius, 2);
    }
}
