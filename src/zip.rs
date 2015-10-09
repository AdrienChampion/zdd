// Copyright 2015 Adrien Champion. See the COPYRIGHT file at the top-level
// directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*! Zipper things for ZDDs.

  A zipper is parameterized by four types:

  * `Key` is for hash key. `HKey` if cached, `()` if not;
  * `Label` is for the elements stored in the ZDD;
  * `Info` is for the information we want available to construct the result;
  * `Data` is for what will be returned eventually.

  A zipper can be unary or binary, see `unary` and `binary` modules. Each
  Each provide a `Zip` trait allowing to go up a zipper. Eached *cached* operation should define its own zipper type, and the ZDD factory should
  implement it. This is the case of
  * `count`, `offset`, `onset` and `change` (unary);
  * `union`, `inter`, `minus` and `subset` (binary).

  A generic zipper for non-cached operations is fine, although `Zdd::iter`
  should do just fine. */

use ::Zdd ;

/** ZDD are hashed to `u64`. */
pub type HKey = u64 ;

/** Cache key for `offset`, `onset` and `change` caches with `Info` the type of
    the labels of the zdd. Also used for `count` with `Info = ()`. */
pub type UnaryKey<Info> = (HKey, Info) ;

/** Cache key for `union`, `inter`, `minus` and `subset` caches. */
pub type BinaryKey = (HKey, HKey) ;

/** Result returned by the zip function from `unary::Zip` and `binary::Zip`. */
pub enum Res<D, NY> {
  /** Could not zip upward, we are done. */
  Done(D),
  /** We're **N**ot done **Yet**, contains one ZDD or two depending if the
  zipper is unary or binary. */
  NYet(NY)
}

/** A zipper, a sequence of steps from `unary::Step` or `binary::Step`. */
trait Zipper<Data> {
  #[inline(always)]
  fn push(& mut self, Data) ;
  #[inline(always)]
  fn pop(& mut self) -> Option<Data> ;
}

/** Creates a dedicated zipper, used for the basic (cached) operations. */
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

/** Zipper for `count`. */
mk_zip!{
  unary Count<Label> of (HKey, Label, (), usize) by count
}

/** Zipper for `offset`. */
mk_zip!{
  unary Offset<Label> of (
    (HKey,Label), Label, Label, Zdd<Label>
  ) by offset
}
/** Zipper for `onset`. */
mk_zip!{
  unary Onset<Label> of (
    (HKey,Label), Label, Label, Zdd<Label>
  ) by onset
}
/** Zipper for `change`. */
mk_zip!{
  unary Change<Label> of (
    (HKey,Label), Label, Label, Zdd<Label>
  ) by change
}

/** Zipper for `union`. */
mk_zip!{
  binary Union<Label> of ((HKey, HKey), Label, Label, Zdd<Label>) by union
}
/** Zipper for `inter`. */
mk_zip!{
  binary Inter<Label> of ((HKey, HKey), Label, Label, Zdd<Label>) by inter
}
/** Zipper for `minus`. */
mk_zip!{
  binary Minus<Label> of ((HKey, HKey), Label, Label, Zdd<Label>) by minus
}

/** Zipper for `subset`. */
mk_zip!{
  binary Subset<Label> of ((HKey, HKey), Label, (), bool) by subset
}

/** Zips up a `Zipper` through a factory implementing `unary::Zip` or
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

/** Unary zipper step enum type and trait.

  Used by `count`, `offset`, `onset` and `change`.

  The idea is that when performing some operation *op* on a zdd, reaching a
  node `zdd = Node(lbl,lft,rgt)` and wanting to go in both branches, then we
  create a `Step::Lft(key.hkey(), info, rgt)`. `info` is whatever we need to
  remember so that we can go up afterwards. `lbl` for instance, if we want to
  construct a new zdd somehow.

  So then we go down `lft` to generate some data. Eventually we call
  `self.zip(data, zip)` from trait `Zip` which will pop the `Step::Lft(key,
  info, rgt)` from the zipper, and

  * push a `Step::Rgt(key, info, data)` before
  * returning `Res::NYet(rgt)` so that we can go down the right kid.

  When `self.zip(othr_data, zip)` pops the `Rgt(key, info, data)`, the two
  pieces of data are `combine`d to produce data that's inserted in the cache
  using `cache_insert`. The zipper then proceeds upwards.*/
pub mod unary {
  use ::Zdd ;
  pub use self::Step::* ;
  pub use super::Res::* ;
  pub use super::HKey ;

  /** A unary zipper step. Type

    * `Key` is for hash key. `HKey` if cached, `()` if not;
    * `Label` is for the elements stored in the ZDD;
    * `Info` is for the information we want available to construct the result;
    * `Data` is for what will be returned eventually. */
  pub enum Step<Key, Label, Info, Data> {
    Lft(Key, Info, Zdd<Label>),
    Rgt(Key, Info, Data),
  }

  /** Can zip up a unary zipper. */
  pub trait Zip<
    Key, Label, Info, Data,
    Zip: super::Zipper<Step<Key, Label, Info, Data>>
  > {
    /** Insert into the cache corresponding to `Zip`. */
    #[inline(always)]
    fn cache_insert(& self, Key, & Data) ;

    /** Combines data, used in terminal steps. */
    #[inline(always)]
    fn combine(& self, Info, Data, Data) -> Data ;

    /** Zips up a unary zip. */
    fn zip(
      & self, mut data: Data, zip: & mut Zip
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

/** Unary zipper step enum type and trait.

  Pretty much the same as `super::unary` but for going down two ZDDs at the
  same time. The biggest difference is that there is `Step::TLft` zipper step
  now that means we're going down the left branch and ignore the right one.*/
pub mod binary {
  use ::Zdd ;
  pub use self::Step::* ;
  pub use super::Res::* ;

  /** A unary zipper step. Type

    * `Key` is for hash key. `HKey` if cached, `()` if not ;
    * `Label` is for the elements stored in the ZDD ;
    * `Info` is for the information we want available to construct the result;
    * `Data` is for what will be returned eventually. */
  pub enum Step<Key, Label, Info, Data> {
    Lft(Key, Info, Zdd<Label>, Zdd<Label>),
    TLft(Key, Info, Data),
    Rgt(Key, Info, Data),
  }

  /** Can zip up a binary zipper. */
  pub trait Zip<
    Key, Label, Info, Data,
    Zip: super::Zipper<Step<Key, Label, Info, Data>>
  > {
    /** Insert into the cache corresponding to `Zip`. */
    #[inline(always)]
    fn cache_insert(& self, Key, & Data) ;

    /** Combines data, used in terminal steps. */
    #[inline(always)]
    fn combine(& self, Info, Data, Data) -> Data ;

    /** Zips up a binary zip. */
    fn zip(
      & self, mut data: Data, zip: & mut Zip
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



