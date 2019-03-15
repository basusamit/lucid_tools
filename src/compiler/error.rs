use std::fmt;
use std::error::Error;

#[derive(Debug, Clone)]
pub struct ProgramError {
    name: String,
    desc: String,
}

impl fmt::Display for ProgramError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{err}: {reason}", err = self.name, reason = self.desc)
    }
}

impl Error for ProgramError {
    fn description(&self) -> &str {
            &self.desc
        }
}

impl ProgramError {
    pub fn of(name: &str, desc: &str) -> ProgramError {
        ProgramError {
            name: String::from(name),
            desc: String::from(desc),
        }
    }
}
