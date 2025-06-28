use std::fmt;

pub fn cyan<T: std::fmt::Display>(text: T) -> String {
    format!("\x1b[36m{}\x1b[0m", text)
}

pub fn green<T: fmt::Display>(text: T) -> String {
    format!("\x1b[32m{}\x1b[0m", text)
}

pub fn magenta<T: fmt::Display>(text: T) -> String {
    format!("\x1b[35m{}\x1b[0m", text)
}

pub fn yellow<T: fmt::Display>(text: T) -> String {
    format!("\x1b[33m{}\x1b[0m", text)
}

#[derive(Clone)]
pub struct Node {
    pub text: String,
    pub children: Vec<Node>,
}

impl Node {
    pub fn leaf<T: Into<String>>(text: T) -> Self {
        Node {
            text: text.into(),
            children: Vec::new(),
        }
    }

    pub fn branch<T: Into<String>>(text: T, children: Vec<Node>) -> Self {
        Node {
            text: text.into(),
            children,
        }
    }

    fn fmt_impl(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool, root: bool) -> fmt::Result {
        if root {
            writeln!(f, "{}", self.text)?;
        } else {
            write!(f, "{}{} ", prefix, if last { "└──" } else { "├──" })?;
            writeln!(f, "{}", self.text)?;
        }
        let new_prefix = if root {
            String::new()
        } else if last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        for (i, child) in self.children.iter().enumerate() {
            child.fmt_impl(f, &new_prefix, i + 1 == self.children.len(), false)?;
        }
        Ok(())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_impl(f, "", true, true)
    }
}

pub trait ItfDisplay {
    fn itf_node(&self) -> Node;
    fn itf_string(&self) -> String {
        self.itf_node().to_string()
    }
}

macro_rules! simple_node {
    ($ty:ty) => {
        impl ItfDisplay for $ty {
            fn itf_node(&self) -> Node {
                Node::leaf(cyan(format!("{:?}", self)))
            }
        }
    };
}

pub(crate) use simple_node;

impl ItfDisplay for String {
    fn itf_node(&self) -> Node {
        Node::leaf(green(self))
    }
}

impl ItfDisplay for &str {
    fn itf_node(&self) -> Node {
        Node::leaf(green(self))
    }
}

impl ItfDisplay for i64 {
    fn itf_node(&self) -> Node {
        Node::leaf(magenta(self))
    }
}
