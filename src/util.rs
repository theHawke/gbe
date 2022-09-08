pub mod disas;

use std::error;
use std::fmt;
use std::io;

pub type GBEResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    BootromSizeError(usize),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Error::IOError(ref err) => write!(f, "IO error: {}", err),
            Error::BootromSizeError(s) => write!(
                f,
                "Specified bootrom has wrong size (is {} bytes, should be 256 bytes).",
                s
            ),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match *self {
            Error::IOError(ref err) => Some(err),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IOError(err)
    }
}
