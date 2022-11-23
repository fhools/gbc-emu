use std::path::{Path, PathBuf};
use std::fs::{self,File};
use std::io::Read;
use thiserror::Error;
use anyhow::{Result, Context};

#[derive(Error, Debug)]
pub enum ROMError {
    #[error("Could not read filepath: {filepath:?}")]
    BadFilePath {
        filepath: PathBuf 
    }
}

pub struct GBCRom(Vec<u8>);

impl GBCRom {
    fn entry_point(&self) -> &[u8] {
        &self.0[0x100..=0x103]
    }

    fn title(&self) -> String {
        String::from_utf8_lossy(&self.0[0x134..=0x143]).into_owned()
    }
}


pub fn read_rom<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    let mut f = File::open(path.as_ref()).with_context(|| format!("Failed to open file: {}", path.as_ref().display()))?;
    let fmeta = fs::metadata(path.as_ref())?;
    let fsize = fmeta.len();
    let mut buf : Vec<u8> = vec![0u8; fsize as usize];
    let len = f.read(&mut buf[..]);
    if let Err(_) = len {
        Err(ROMError::BadFilePath { filepath: path.as_ref().into() }.into())
    } else {
        Ok(buf)
    }
}

#[ignore]
#[test]
fn test_read_rom() {
    let a = read_rom("test.fs");

    if let Err(myerr) = a {
        println!("myerr: {}", myerr);
    }
}
