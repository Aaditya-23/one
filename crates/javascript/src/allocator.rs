use std::ops::Range;

use bumpalo::{boxed::Box, collections::Vec, Bump};

pub trait CloneIn<'a> {
    fn clone_in(&self, arena: &'a Bump) -> Self;
}

impl<'a, T> CloneIn<'a> for Vec<'a, T>
where
    T: CloneIn<'a>,
{
    fn clone_in(&self, arena: &'a Bump) -> Self {
        let mut new_vec = Vec::new_in(arena);
        for item in self.iter() {
            new_vec.push(item.clone_in(arena));
        }

        new_vec
    }
}

impl<'a, T> CloneIn<'a> for Box<'a, T>
where
    T: CloneIn<'a>,
{
    fn clone_in(&self, arena: &'a Bump) -> Self {
        vec![3, 4, 3];
        let inner_clone = self.as_ref().clone_in(arena);
        Box::new_in(inner_clone, arena)
    }
}

impl<'a, T> CloneIn<'a> for Option<T>
where
    T: CloneIn<'a>,
{
    fn clone_in(&self, arena: &'a Bump) -> Self {
        if let Some(inner) = self {
            Some(inner.clone_in(arena))
        } else {
            None
        }
    }
}

impl<'a, T: CloneIn<'a>> CloneIn<'a> for Range<T> {
    fn clone_in(&self, arena: &'a Bump) -> Self {
        self.start.clone_in(arena)..self.end.clone_in(arena)
    }
}

macro_rules! impl_clone_in {
    ($($x:ty),*) => {
        $(
            impl <'a> CloneIn<'a> for $x {
                fn clone_in(&self, _: &'a Bump) -> Self {
                    *self
                }
            }
        )*
    }
}

impl_clone_in! {u32, f64, bool, &str}
