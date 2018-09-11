import { Injectable } from '@angular/core';
import { ImageColour } from '../models/ImageColour';

@Injectable()
export class ColourService {

    private selectedColourKey: string = 'visImagingColourScheme';
    private defaultSelectedColourValue: string = 'default';

    private _defaultColours: any = {};

    constructor() {
        this._defaultColours  = {
            default: new ImageColour('Default', null),
            jet: new ImageColour('Jet', 'jet'),
            hsv: new ImageColour('HSV', 'hsv'),
            hot: new ImageColour('Hot', 'hot'),
            cool: new ImageColour('Cool', 'cool'),
            spring: new ImageColour('Spring', 'spring'),
            autumn: new ImageColour('Autumn', 'autumn'),
            winter: new ImageColour('Winter', 'winter'),
            bone: new ImageColour('Bone', 'bone'),
            copper: new ImageColour('Copper', 'copper'),
            greys: new ImageColour('Greys', 'greys'),
            yiGnBl: new ImageColour('Yellow Green Blue', 'yignbu'),
            greens: new ImageColour('Greens', 'greens'),
            yiOrRd: new ImageColour('Yellow Orange Red', 'yiorrd'),
            blueRed: new ImageColour('Blue Red', 'bluered'),
            redBlue: new ImageColour('Red Blue', 'rdbu'),
            picnic: new ImageColour('Picnic', 'picnic'),
            rainbow: new ImageColour('Rainbow', 'rainbow'),
            portland: new ImageColour('Portland', 'portland'),
            blackbody: new ImageColour('Blackbody', 'blackbody'),
            earth: new ImageColour('Earth', 'earth'),
            electric: new ImageColour('Electric', 'electric'),
            viridis: new ImageColour('Viridis', 'viridis'),
            inferno: new ImageColour('Inferno', 'inferno'),
            magma: new ImageColour('Magma', 'magma'),
            plasma: new ImageColour('Plasma', 'plasma'),
            warm: new ImageColour('Warm', 'warm'),
            rainbowSoft: new ImageColour('Rainbow Soft', 'rainbow-soft'),
            bathymetry: new ImageColour('Bathymetry', 'bathymetry'),
            cdom: new ImageColour('Cdom', 'cdom'),
            chlorophyll: new ImageColour('Chlorophyll', 'chlorophyll'),
            density: new ImageColour('Density', 'density'),
            freesurfaceBlue: new ImageColour('Freesurface Blue', 'freesurface-blue'),
            freesurfaceRed: new ImageColour('Freesurface Red', 'freesurface-red'),
            oxygen: new ImageColour('Oxygen', 'oxygen'),
            par: new ImageColour('Par', 'par'),
            phase: new ImageColour('Phase', 'phase'),
            salinity: new ImageColour('Salinity', 'salinity'),
            temperature: new ImageColour('Temperature', 'temperature'),
            turbidity: new ImageColour('Turbidity', 'turbidity'),
            velocityBlue: new ImageColour('Velocity Blue', 'velocity-blue'),
            velocityGreen: new ImageColour('Velocity Green', 'velocity-green'),
            cubehelix: new ImageColour('Cubehelix', 'cubehelix')
        };
    }

    getColoursArray() {
        return [
            this._defaultColours.default,
            this._defaultColours.jet,
            this._defaultColours.hsv,
            this._defaultColours.hot,
            this._defaultColours.cool,
            this._defaultColours.spring,
            this._defaultColours.autumn,
            this._defaultColours.winter,
            this._defaultColours.bone,
            this._defaultColours.copper,
            this._defaultColours.greys,
            this._defaultColours.yiGnBl,
            this._defaultColours.greens,
            this._defaultColours.yiOrRd,
            this._defaultColours.blueRed,
            this._defaultColours.redBlue,
            this._defaultColours.picnic,
            this._defaultColours.rainbow,
            this._defaultColours.portland,
            this._defaultColours.blackbody,
            this._defaultColours.earth,
            this._defaultColours.electric,
            this._defaultColours.viridis,
            this._defaultColours.inferno,
            this._defaultColours.magma,
            this._defaultColours.plasma,
            this._defaultColours.warm,
            this._defaultColours.rainbowSoft,
            this._defaultColours.bathymetry,
            this._defaultColours.cdom,
            this._defaultColours.chlorophyll,
            this._defaultColours.density,
            this._defaultColours.freesurfaceBlue,
            this._defaultColours.freesurfaceRed,
            this._defaultColours.oxygen,
            this._defaultColours.par,
            this._defaultColours.phase,
            this._defaultColours.salinity,
            this._defaultColours.temperature,
            this._defaultColours.turbidity,
            this._defaultColours.velocityBlue,
            this._defaultColours.velocityGreen,
            this._defaultColours.cubehelix
        ];
    }

    getColoursMap() {
        return this._defaultColours;
    }

    /**
     * Returns the value of the selected colour in local storage.  If a
     * selected colour is not found or doesn't map to a key in
     * this._defaultColours, the default colour value is set and returned.
     */
    getSelectedColour() {
        let colour = localStorage.getItem(this.selectedColourKey);

        if (!!this._defaultColours[colour]) {
            return this._defaultColours[colour].value;
        } else {
            this.setSelectedColour(this.defaultSelectedColourValue);
            return null;
        }
    }

    /**
     * Sets a selected colour in local storage.
     * @param {string} value - The value of the color to set.  This must match a
     *                         key value in this._defaultColours.
     */
    setSelectedColour(value: string) {
        localStorage.setItem(this.selectedColourKey, value);
    }
}
