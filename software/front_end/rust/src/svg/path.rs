// The MIT License (MIT)
//
// Copyright (c) 2014 Jeremy Letang (letang.jeremy@gmail.com)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use std::fmt::Debug;
use std::collections::HashMap;

use super::common::{insert_attribs, insert_transform, finalize};
use super::transform::Transform;
use super::SVGEntity;

#[derive(Debug, Eq, Clone)]
pub struct EllipticalArc {
    pub x: i32,
    pub y: i32,
    pub x_radius: u32,
    pub y_radius: u32,
    pub x_axis_rotation: i32,
    pub large_arc: bool,
    pub sweep: bool,
    pub close: bool,
    pub attribs: HashMap<~str, ~str>,
    pub transform: Option<Transform>
}

pub struct CubicBezier {

}

fn to_int(b: bool) -> int { if b == true { 1 } else { 0 } }

