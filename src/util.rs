use std::ops::Deref;
use std::ops::DerefMut;
use std::cell::{UnsafeCell};
use std::rc::Rc;


/* This is taken from michelhe/rustboyadvance-ng
 */
#[repr(transparent)]
#[derive(Debug)]
pub struct Shared<T>(Rc<UnsafeCell<T>>); 

impl<T> Deref for Shared<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // Rc implements Deref for it's T. so the get() is UnsafeCell's get()
        unsafe { &*self.0.get() }
    }
}

impl<T> DerefMut for Shared<T> {

    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
       unsafe { &mut *self.0.get() }
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(self.0.clone())
    }
}

impl<T> Shared<T> {
    pub fn new(t: T) -> Self {
        Shared(Rc::new(UnsafeCell::new(t)))
    }
}
