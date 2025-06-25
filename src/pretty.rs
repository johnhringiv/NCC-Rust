use colored::Colorize;
use std::fmt;

#[derive(Clone)]
pub struct Node {
    pub text: String,
    pub children: Vec<Node>,
}

impl Node {
    pub fn leaf<T: Into<String>>(text: T) -> Self {
        Node { text: text.into(), children: Vec::new() }
    }

    pub fn branch<T: Into<String>>(text: T, children: Vec<Node>) -> Self {
        Node { text: text.into(), children }
    }

    fn fmt_impl(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
        if !prefix.is_empty() {
            write!(f, "{}{} ", prefix, if last { "└──" } else { "├──" })?;
        }
        writeln!(f, "{}", self.text)?;
        let new_prefix = if prefix.is_empty() {
            String::new()
        } else if last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        for (i, child) in self.children.iter().enumerate() {
            child.fmt_impl(f, &new_prefix, i + 1 == self.children.len())?;
        }
        Ok(())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_impl(f, "", true)
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
                Node::leaf(format!("{:?}", self).cyan().to_string())
            }
        }
    };
}

pub(crate) use simple_node;

impl ItfDisplay for String {
    fn itf_node(&self) -> Node {
        Node::leaf(self.green().to_string())
    }
}

impl ItfDisplay for &str {
    fn itf_node(&self) -> Node {
        Node::leaf(self.green().to_string())
    }
}

impl ItfDisplay for i64 {
    fn itf_node(&self) -> Node {
        Node::leaf(self.to_string().magenta().to_string())
    }
}
