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

use super::common::{insert_attribs, insert_transform};
use super::transform::Transform;
use super::SVGEntity;

#[derive(Debug, PartialEq, Clone)]
pub struct Text {
    pub x: i32,
    pub y: i32,
    pub text: String,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

impl SVGEntity for Text {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<text x=\"{:?}\" y=\"{:?}\"",
                           self.x, self.y).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        o.push_str(format!(" >{}</text>", self.text).as_str());
        o
    }
}
