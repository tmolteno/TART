// import * as wasm from "wasm-tart-imaging";
import { json_to_svg_ext } from "wasm-tart-imaging";
let data = require("../data.json")
let ret = json_to_svg_ext(JSON.stringify(data), 46, false);

var container = document.getElementById("container");
container.innerHTML = ret;
