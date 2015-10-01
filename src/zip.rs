use ::Zdd ;

/// ZDD are hashed to `u64`.
pub type HKey = u64 ;

/// Cache key for `offset`, `onset` and `change` caches with `Info` the type of
/// the labels of the zdd. Also used for `count` with `Info = ()`.
pub type UnaryKey<Info> = (HKey, Info) ;

/// Cache key for `union`, `inter` and `minus` caches.
pub type BinaryKey = (HKey, HKey) ;


pub enum Res<D, NY> {
  Done(D),
  NYet(NY)
}

trait Zipper<Data> {
  #[inline(always)]
  fn push(& mut self, Data) ;
  #[inline(always)]
  fn pop(& mut self) -> Option<Data> ;
}

macro_rules! mk_zip {
  ($arity:ident
    $id:ident< $($t_param:ident),+ > of (
      $key:ty, $lbl:ty, $info:ty, $data:ty
    ) by $fun:ident
  ) => (
    pub struct $id<$($t_param),+> {
      zip: Vec<$arity::Step<$key, $lbl, $info, $data>>,
    }
    pub fn $fun<$($t_param),+>() -> $id<$($t_param),+> { $id { zip: vec![] } }
    impl<$($t_param),+> $id<$($t_param),+> {
      #[inline(always)]
      pub fn push(& mut self, step: $arity::Step<$key, $lbl, $info, $data>) {
        self.zip.push(step)
      }
    }
    impl<$($t_param),+> Zipper<
      $arity::Step<$key, $lbl, $info, $data>
    > for $id<$($t_param),+> {
      #[inline(always)]
      fn push(& mut self, step: $arity::Step<$key, $lbl, $info, $data>) {
        self.zip.push(step)
      }
      #[inline(always)]
      fn pop(& mut self) -> Option<$arity::Step<$key, $lbl, $info, $data>> {
        self.zip.pop()
      }
    }
  ) ;
}

mk_zip!{
  unary Count<Label> of (HKey, Label, (), usize) by count
}

mk_zip!{
  unary Offset<Label> of (
    (HKey,Label), Label, Label, Zdd<Label>
  ) by offset
}
mk_zip!{
  unary Onset<Label> of (
    (HKey,Label), Label, Label, Zdd<Label>
  ) by onset
}
mk_zip!{
  unary Change<Label> of (
    (HKey,Label), Label, Label, Zdd<Label>
  ) by change
}

mk_zip!{
  binary Union<Label> of ((HKey, HKey), Label, Label, Zdd<Label>) by union
}
mk_zip!{
  binary Inter<Label> of ((HKey, HKey), Label, Label, Zdd<Label>) by inter
}
mk_zip!{
  binary Minus<Label> of ((HKey, HKey), Label, Label, Zdd<Label>) by minus
}

mk_zip!{
  binary Subset<Label> of ((HKey, HKey), Label, (), bool) by subset
}

/**
Zips up a `Zipper` through a factory implementing `unary::Zip` or
`binary::Zip` from some data.

Zipping up a unary zipper:

```ignore
zip_up!(factory > zip > data)
```

Zipping up a binary zipper:

```ignore
zip_up!(factory >> zip > data)
```
*/
#[macro_export]
macro_rules! zip_up {
  ($has_zip:ident > $zip:ident > $data:expr) => (
    {
      use $crate::zip::unary::Zip ;
      let data = $data ;
      match $has_zip.zip(data, & mut $zip) {
        $crate::zip::Res::NYet(rgt) => rgt,
        $crate::zip::Res::Done(data) => return data,
      }
    }
  ) ;
  ($has_zip:ident >> $zip:ident > $data:expr) => (
    {
      use $crate::zip::binary::Zip ;
      let data = $data ;
      match $has_zip.zip(data, & mut $zip) {
        $crate::zip::Res::NYet(rgt) => rgt,
        $crate::zip::Res::Done(data) => return data,
      }
    }
  ) ;
}

pub mod unary {
  use ::Zdd ;
  pub use self::Step::* ;
  pub use super::Res::* ;
  pub use super::HKey ;

  pub enum Step<Key, Label, Info, Data> {
    Lft(Key, Info, Zdd<Label>),
    Rgt(Key, Info, Data),
  }

  pub trait Zip<
    Key, Label, Info, Data,
    Zip: super::Zipper<Step<Key, Label, Info, Data>>
  > {
    /// Insert into the cache corresponding to `Zip`.
    #[inline(always)]
    fn cache_insert(& mut self, Key, & Data) ;

    /// Combines data, used in terminal steps.
    #[inline(always)]
    fn combine(& mut self, Info, Data, Data) -> Data ;

    fn zip(
      & mut self, mut data: Data, zip: & mut Zip
    ) -> super::Res<Data,Zdd<Label>> {
      loop {
        data = match zip.pop() {
          // Can't zip up, done.
          None => return Done(data),

          // A right branch hasn't been explored yet.
          Some( Lft(key, info, rgt) ) => {
            zip.push( Rgt(key, info, data) ) ;
            return NYet(rgt)
          },

          // We were in a right branch, going up.
          Some( Rgt(key, info, l_data) ) => {
            // Combine data.
            let data = self.combine(info, l_data, data) ;
            self.cache_insert(key, & data) ;
            data
          },
        }
      }
    }
  }
}

pub mod binary {
  use ::Zdd ;
  pub use self::Step::* ;
  pub use super::Res::* ;

  pub enum Step<Key, Label, Info, Data> {
    Lft(Key, Info, Zdd<Label>, Zdd<Label>),
    TLft(Key, Info, Data),
    Rgt(Key, Info, Data),
  }

  pub trait Zip<
    Key, Label, Info, Data,
    Zip: super::Zipper<Step<Key, Label, Info, Data>>
  > {
    /// Insert into the cache corresponding to `Zip`.
    #[inline(always)]
    fn cache_insert(& mut self, Key, & Data) ;

    /// Combines data, used in terminal steps.
    #[inline(always)]
    fn combine(& mut self, Info, Data, Data) -> Data ;

    fn zip(
      & mut self, mut data: Data, zip: & mut Zip
    ) -> super::Res<Data,(Zdd<Label>, Zdd<Label>)> {
      loop {
        data = match zip.pop() {
          // Can't zip up, done.
          None => return Done(data),

          // Some right branches haven't been explored yet.
          Some( Lft(key, info, l_rgt, r_rgt) ) => {
            zip.push( Rgt(key, info, data) ) ;
            return NYet((l_rgt, r_rgt))
          },

          // We were in a terminal left branch, going up.
          Some( TLft(key, info, r_data) ) => {
            // Combine data.
            let data = self.combine(info, data, r_data) ;
            self.cache_insert(key, & data) ;
            data
          },

          // We were in a right branch, going up.
          Some( Rgt(key, info, l_data) ) => {
            // Combine data.
            let data = self.combine(info, l_data, data) ;
            self.cache_insert(key, & data) ;
            data
          },
        }
      }
    }
  }
}



