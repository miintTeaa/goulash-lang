use std::{
    fmt::{Debug, Display},
    ops::Range,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub length: u16,
}

impl Span {
    pub fn end(&self) -> usize {
        (self.start + self.length as u32) as usize
    }

    pub fn range(&self) -> Range<usize> {
        (self.start as usize)..(self.start + self.length as u32) as usize
    }

    pub fn set_end(&mut self, at: usize) {
        self.length = at.checked_sub(self.start as usize).unwrap() as u16;
    }

    pub fn to(mut self, end: usize) -> Span {
        self.length = end.checked_sub(self.start as usize).unwrap() as u16;
        self
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start.clamp(0, u32::MAX as usize) as u32,
            length: (value.end - value.start).clamp(0, u16::MAX as usize) as u16,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}..{}", self.start, self.end()))
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl PartialEq<Range<usize>> for Span {
    fn eq(&self, other: &Range<usize>) -> bool {
        other.start == self.start as usize && other.end == self.end() as usize
    }
}
