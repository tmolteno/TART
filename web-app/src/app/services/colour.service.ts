import { Injectable } from '@angular/core';
import { ImageColour } from '../models/ImageColour';

@Injectable()
export class ColourService {

    private _defaultColours: any = {};

    constructor() {
        this._defaultColours  = {
            default: new ImageColour('Default', null),
            jet: new ImageColour('Jet', 'jet'),
            hsv: new ImageColour('HSV', 'hsv'),
            hot: new ImageColour('Hot', 'hot'),
            cool: new ImageColour('Cool', 'cool'),
            spring: new ImageColour('Spring', 'spring'),
            autumn: new ImageColour('Autumn', 'autumn')
        };
    }

    getColoursMap() {
        return this._defaultColours;
    }

    getColoursArray() {
        return [
            this._defaultColours.default,
            this._defaultColours.jet,
            this._defaultColours.hsv,
            this._defaultColours.hot,
            this._defaultColours.cool,
            this._defaultColours.spring,
            this._defaultColours.autumn
        ];
    }
}
