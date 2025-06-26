pub fn cyan<T: std::fmt::Display>(text: T) -> String {
    format!("\x1b[36m{}\x1b[0m", text)
}

pub fn green<T: std::fmt::Display>(text: T) -> String {
    format!("\x1b[32m{}\x1b[0m", text)
}

pub fn magenta<T: std::fmt::Display>(text: T) -> String {
    format!("\x1b[35m{}\x1b[0m", text)
}

pub fn yellow<T: std::fmt::Display>(text: T) -> String {
    format!("\x1b[33m{}\x1b[0m", text)
}
