use std::ops::{Index, IndexMut};
use thiserror::Error;

#[derive(Debug, PartialEq, Error)]
pub enum MemError {
    #[error("Memory access out of bounds, with index {index}, limit {limit:?}")]
    OutOfBounds { index: usize, limit: Option<usize> },
}

pub type Error = MemError;
pub type Result<T> = std::result::Result<T, MemError>;

pub struct ZMem {
    inner: Vec<u8>,
    limit: Option<usize>,
}

impl AsRef<Vec<u8>> for ZMem {
    fn as_ref(&self) -> &Vec<u8> {
        &self.inner
    }
}

impl Default for ZMem {
    fn default() -> Self {
        Self::new()
    }
}

impl ZMem {
    pub fn new() -> Self {
        ZMem {
            inner: Vec::new(),
            limit: None,
        }
    }

    pub fn allocate(&mut self, size: usize) {
        if size > self.inner.capacity() {
            self.inner.reserve(size - self.inner.capacity());
        }
        if size > self.inner.len() {
            self.inner.resize(size, 0);
        }
    }

    pub fn with_limit(self, limit: usize) -> Self {
        ZMem {
            inner: self.inner,
            limit: Some(limit),
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.inner
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.inner
    }

    pub fn set(&mut self, index: usize, value: u8) -> Result<()> {
        self.check_index(index)?;
        self.allocate(index + 1);
        self.inner[index] = value;
        Ok(())
    }

    pub fn check_index(&self, index: usize) -> Result<()> {
        if let Some(limit) = self.limit
            && index >= limit
        {
            return Err(MemError::OutOfBounds {
                index,
                limit: Some(limit),
            });
        }
        Ok(())
    }

    pub fn get(&self, index: usize) -> Result<u8> {
        self.check_index(index)?;
        Ok(self.inner.get(index).copied().unwrap_or(0))
    }
}

impl Index<usize> for ZMem {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        self.check_index(index).unwrap();
        if index >= self.inner.len() {
            &0
        } else {
            &self.inner[index]
        }
    }
}

impl IndexMut<usize> for ZMem {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.check_index(index).unwrap();
        self.allocate(index + 1);
        &mut self.inner[index]
    }
}

#[cfg(test)]
mod tests {
    use crate::mem::{MemError, ZMem};

    #[test]
    fn mem() {
        let mut mem = ZMem::new();
        mem.set(3, 5).unwrap();
        mem.set(6, 9).unwrap();
        mem.set(1, 9).unwrap();
        assert_eq!(mem.as_slice(), &[0, 9, 0, 5, 0, 0, 9]);
    }

    #[test]
    fn limit() {
        let mut mem = ZMem::new().with_limit(8);
        assert!(matches!(mem.set(3, 5), Ok(())));
        assert!(matches!(mem.set(8, 5), Err(MemError::OutOfBounds { .. })));
    }

    #[test]
    fn index() {
        let mut mem = ZMem::new();
        mem[3] = 5;
        mem[6] = 9;
        mem[1] = 9;
        assert_eq!(mem[0], 0);
        assert_eq!(mem[1], 9);
        assert_eq!(mem[3], 5);
        assert_eq!(mem[6], 9);
        assert_eq!(mem.as_slice(), &[0, 9, 0, 5, 0, 0, 9]);
    }

    #[test]
    #[should_panic]
    fn index_limit_read() {
        let mem = ZMem::new().with_limit(8);
        // This should panic due to the limit check in `index()`.
        let _ = mem[8];
    }

    #[test]
    #[should_panic]
    fn index_limit_write() {
        let mut mem = ZMem::new().with_limit(8);
        // This should panic due to the limit check in `index_mut()`.
        mem[8] = 5;
    }
}
