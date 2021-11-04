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

use std::collections::HashMap;
use num::Integer;

use super::transform::Transform;

pub fn insert_attribs(mut o: String, attribs: &HashMap<String, String>) -> String {
    for (at, value) in attribs.iter() {
        o.push_str(format!(" {}=\"{}\"", *at, *value).as_str())
    }
    o
}

pub fn insert_transform(mut o: String, transform: &Option<Transform>) -> String {
    match *transform {
        Some(ref t) => o.push_str(format!(" {}", t.get()).as_str()),
        None    => {/* nothing to do */}
    }
    o
}

pub fn finalize(mut o: String) -> String{ o.push_str(" />\n"); o }

fn or_max<T: Integer + Ord>(int: T, max: T) -> T {
    if int < max { int } else { max }
}

pub fn rgb(red: u8,
           green: u8,
           blue: u8) -> String {
    format!("rgb({:?}, {:?}, {:?})", or_max(red, 255),
                               or_max(green, 255),
                               or_max(blue, 255))
}

pub fn rgba(red: u8,
            green: u8,
            blue: u8,
            alpha: f32) -> String {
    format!("rgba({:?}, {:?}, {:?}, {:?})",
            or_max(red, 255),
            or_max(green, 255),
            or_max(blue, 255),
            if alpha < 1.0 { alpha } else { 1.0 })
}
