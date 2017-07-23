/**
 * ColorMap model.
 * @author Tim Miller
 */
export class ImageColour {
    /** Value used by colormap. */
    private _value: string;
    /** Display name used by TART web interface. */
    private _name: string;

    constructor(name: string, value: string) {
        this._name = name;
        this._value = value;
    }

    get value(): string {
        return this._value;
    }

    get name(): string {
        return this._name;
    }
}
