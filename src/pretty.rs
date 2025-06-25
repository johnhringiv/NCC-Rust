pub trait ItfDisplay {
    fn itf_fmt(&self, f: &mut String, indent: usize);
    fn itf_string(&self) -> String {
        let mut s = String::new();
        self.itf_fmt(&mut s, 0);
        s
    }
}

pub(crate) fn indent_line(f: &mut String, indent: usize, text: &str) {
    for _ in 0..indent {
        f.push(' ');
    }
    f.push_str(text);
    f.push('\n');
}

macro_rules! simple_display {
    ($ty:ty) => {
        impl ItfDisplay for $ty {
            fn itf_fmt(&self, f: &mut String, indent: usize) {
                indent_line(f, indent, &format!("{:?}", self));
            }
        }
    };
}

pub(crate) use simple_display;

impl ItfDisplay for String {
    fn itf_fmt(&self, f: &mut String, indent: usize) {
        indent_line(f, indent, self);
    }
}

impl ItfDisplay for &str {
    fn itf_fmt(&self, f: &mut String, indent: usize) {
        indent_line(f, indent, self);
    }
}

impl ItfDisplay for i64 {
    fn itf_fmt(&self, f: &mut String, indent: usize) {
        indent_line(f, indent, &self.to_string());
    }
}
