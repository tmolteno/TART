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
use std::vec::Vec;
use std::collections::HashMap;

use num::Integer;

use super::common::{insert_attribs, insert_transform, finalize};
use super::transform::Transform;
use super::SVGEntity;

#[derive(Debug, PartialEq, Clone)]
pub struct Circle {
    pub x: i32,
    pub y: i32,
    pub radius: u32,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ellipse {
    pub x: i32,
    pub y: i32,
    pub x_radius: u32,
    pub y_radius: u32,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Line {
    pub x1: i32,
    pub y1: i32,
    pub x2: i32,
    pub y2: i32,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

#[derive(Debug, PartialEq, Clone)]
pub struct RoundedRect {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    pub x_round: u32,
    pub y_round: u32,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

#[derive(Debug, PartialEq, Clone)]
pub struct PolyLine<T> {
    pub points: Vec<(T, T)>,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Polygon<T> {
    pub points: Vec<(T, T)>,
    pub attribs: HashMap<String, String>,
    pub transform: Option<Transform>
}

impl<T: Integer + Debug> PolyLine<T> {
    pub fn add_point(&mut self, x: T, y: T) {
        self.points.push((x, y))
    }
}

impl<T: Integer + Debug> Polygon<T> {
    pub fn add_point(&mut self, x: T, y: T) {
        self.points.push((x, y))
    }
}

fn get_points<T: Integer + Debug>(points: &Vec<(T, T)>) -> String {
    let mut p: String = String::from("points=\"");
    for &(ref x, ref y) in points.iter() {
        p.push_str(format!("{:?},{:?} ", x, y).as_str())
    }
    p.push_str("\"");
    p
}

impl SVGEntity for Circle {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<circle cx=\"{:?}\" cy=\"{:?}\" r=\"{:?}\"",
                           self.x, self.y, self.radius).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}

impl<T: Integer + Debug> SVGEntity for PolyLine<T> {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<polyline {:?}", get_points(&self.points)).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}

impl<T: Integer + Debug> SVGEntity for Polygon<T> {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<polygon {}", get_points(&self.points)).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}

impl SVGEntity for Line {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<line x1=\"{:?}\" y1=\"{:?}\" x2=\"{:?}\" y2=\"{:?}\"",
                           self.x1, self.y1, self.x2, self.y2).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}

impl SVGEntity for Ellipse {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<ellipse cx=\"{:?}\" cy=\"{:?}\" rx=\"{:?}\" ry=\"{:?}\"",
                           self.x, self.y, self.x_radius, self.y_radius).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}

impl SVGEntity for Rect {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<rect x=\"{:?}\" y=\"{:?}\" width=\"{:?}\" height=\"{:?}\"",
                           self.x, self.y, self.width, self.height).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}

impl SVGEntity for RoundedRect {
    fn gen_output(&self) -> String {
        let mut o = String::new();
        o.push_str(format!("<rect x=\"{:?}\" y=\"{:?}\" width=\"{:?}\" height=\"{:?}\" \
                           rx=\"{:?}\" ry=\"{:?}\"",
                           self.x, self.y, self.width, self.height,
                           self.x_round, self.y_round).as_str());
        o = insert_attribs(insert_transform(o, &self.transform), &self.attribs);
        finalize(o)
    }
}
