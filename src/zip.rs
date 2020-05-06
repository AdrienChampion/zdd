// See the LICENSE files at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Zipper things for ZDDs.
//!
//! A zipper is parameterized by four types:
//!
//! - `Key` is for hash key. `HKey` if cached, `()` if not;
//! - `Label` is for the elements stored in the ZDD;
//! - `Info` is for the information we want available to construct the result;
//! - `Data` is for what will be returned eventually.
//!
//! A zipper can be unary or binary, see `unary` and `binary` modules. Each Each provide a `Zip`
//! trait allowing to go up a zipper. Eached *cached* operation should define its own zipper type,
//! and the ZDD factory should implement it. This is the case of
//!
//! - `count`, `offset`, `onset` and `change` (unary);
//! - `union`, `inter`, `minus` and `subset` (binary).
//!
//! A generic zipper for non-cached operations is fine, although `Zdd::iter` should do just fine.

use crate::Zdd;

/// ZDD are hashed to `u64`.
pub type HKey = u64;

/// Cache key for `offset`, `onset` and `change` caches with `Info` the type of the labels of the
/// zdd.
///
/// Also used for `count` with `Info = ()`. */
pub type UnaryKey<Info> = (HKey, Info);

/// Cache key for `union`, `inter`, `minus` and `subset` caches.
pub type BinaryKey = (HKey, HKey);

/// Result returned by the zip function from `unary::Zip` and `binary::Zip`.
pub enum Res<D, NY> {
    /// Could not zip upward, we are done.
    Done(D),
    /// We're **N**ot done **Yet**, contains one ZDD or two depending if the zipper is unary or
    /// binary.
    NYet(NY),
}

/// A zipper, a sequence of steps from `unary::Step` or `binary::Step`.
pub trait Zipper<Data> {
    fn push(&mut self, data: Data);
    fn pop(&mut self) -> Option<Data>;
}

/// Creates a dedicated zipper, used for the basic (cached) operations.
macro_rules! mk_zip {
    (
        $(#[$meta:meta])*
        $arity:ident $id:ident of (
            $key:ty, $lbl:ident, $info:ty, $data:ty
        ) by $fun:ident
    ) => (
        $(#[$meta])*
        pub struct $id<$lbl: Clone> {
            zip: Vec<$arity::Step<$key, $lbl, $info, $data>>,
        }
        pub fn $fun<$lbl: Clone>() -> $id<$lbl> { $id { zip: vec![] } }
        impl<$lbl: Clone> $id<$lbl> {
            #[inline(always)]
            pub fn push(&mut self, step: $arity::Step<$key, $lbl, $info, $data>) {
                self.zip.push(step)
            }
        }
        impl<$lbl: Clone> Zipper<
            $arity::Step<$key, $lbl, $info, $data>
        > for $id<$lbl> {
            #[inline(always)]
            fn push(&mut self, step: $arity::Step<$key, $lbl, $info, $data>) {
                self.zip.push(step)
            }
            #[inline(always)]
            fn pop(&mut self) -> Option<$arity::Step<$key, $lbl, $info, $data>> {
                self.zip.pop()
            }
        }
    ) ;
}

mk_zip! {
    /// Zipper for `count`.
    unary Count of (HKey, Label, (), usize) by count
}

mk_zip! {
    /// Zipper for `offset`.
    unary Offset of (
        (HKey,Label), Label, Label, Zdd<Label>
    ) by offset
}
mk_zip! {
    /// Zipper for `onset`.
    unary Onset of (
        (HKey,Label), Label, Label, Zdd<Label>
    ) by onset
}
mk_zip! {
    /// Zipper for `change`.
    unary Change of (
        (HKey,Label), Label, Label, Zdd<Label>
    ) by change
}

mk_zip! {
    /// Zipper for `union`.
    binary Union of (
        (HKey, HKey), Label, Label, Zdd<Label>
    ) by union
}

mk_zip! {
    /// Zipper for `inter`.
    binary Inter of (
        (HKey, HKey), Label, Label, Zdd<Label>
    ) by inter
}

mk_zip! {
    /// Zipper for `minus`.
    binary Minus of (
        (HKey, HKey), Label, Label, Zdd<Label>
    ) by minus
}

mk_zip! {
    /// Zipper for `subset`.
    binary Subset of (
        (HKey, HKey), Label, (), bool
    ) by subset
}

/// Zips up a `Zipper` through a factory implementing `unary::Zip` or `binary::Zip` from some data.
///
/// Zipping up a unary zipper:
///
/// ```ignore
/// zip_up!(factory > zip > data)
/// ```
///
/// Zipping up a binary zipper:
///
/// ```ignore
/// zip_up!(factory >> zip > data)
/// ```
macro_rules! zip_up {
    ($has_zip:ident > $zip:ident > $data:expr) => {{
        use $crate::zip::unary::Zip;
        let data = $data;
        match $has_zip.zip(data, &mut $zip) {
            $crate::zip::Res::NYet(rgt) => rgt,
            $crate::zip::Res::Done(data) => return data,
        }
    }};
    ($has_zip:ident >> $zip:ident > $data:expr) => {{
        use $crate::zip::binary::Zip;
        let data = $data;
        match $has_zip.zip(data, &mut $zip) {
            $crate::zip::Res::NYet(rgt) => rgt,
            $crate::zip::Res::Done(data) => return data,
        }
    }};
}

/// Unary zipper step enum type and trait.
///
/// Used by `count`, `offset`, `onset` and `change`.
///
/// The idea is that when performing some operation *op* on a zdd, reaching a node `zdd =
/// Node(lbl,lft,rgt)` and wanting to go in both branches, then we create a `Step::Lft(key.hkey(),
/// info, rgt)`. `info` is whatever we need to remember so that we can go up afterwards. `lbl` for
/// instance, if we want to construct a new zdd somehow.
///
/// So then we go down `lft` to generate some data. Eventually we call `self.zip(data, zip)` from
/// trait `Zip` which will pop the `Step::Lft(key, info, rgt)` from the zipper, and
///
/// - push a `Step::Rgt(key, info, data)` before
/// - returning `Res::NYet(rgt)` so that we can go down the right kid.
///
/// When `self.zip(othr_data, zip)` pops the `Rgt(key, info, data)`, the two pieces of data are
/// `combine`d to produce data that's inserted in the cache using `cache_insert`. The zipper then
/// proceeds upwards.
pub mod unary {
    pub use super::*;

    /// A unary zipper step. Type
    ///
    /// - `Key` is for hash key. `HKey` if cached, `()` if not;
    /// - `Label` is for the elements stored in the ZDD;
    /// - `Info` is for the information we want available to construct the result;
    /// - `Data` is for what will be returned eventually.
    pub enum Step<Key, Label, Info, Data> {
        Lft(Key, Info, Zdd<Label>),
        Rgt(Key, Info, Data),
    }

    /// Can zip up a unary zipper.
    pub trait Zip<Key, Label: Clone, Info, Data, Zip: super::Zipper<Step<Key, Label, Info, Data>>> {
        /// Insert into the cache corresponding to `Zip`.
        fn cache_insert(&self, key: Key, data: &Data);

        /// Combines data, used in terminal steps.
        fn combine(&self, info: Info, lhs: Data, rhs: Data) -> Data;

        /// Zips up a unary zip.
        fn zip(&self, mut data: Data, zip: &mut Zip) -> Res<Data, Zdd<Label>> {
            loop {
                data = match zip.pop() {
                    // Can't zip up, done.
                    None => return Res::Done(data),

                    // A right branch hasn't been explored yet.
                    Some(Step::Lft(key, info, rgt)) => {
                        zip.push(Step::Rgt(key, info, data));
                        return Res::NYet(rgt);
                    }

                    // We were in a right branch, going up.
                    Some(Step::Rgt(key, info, l_data)) => {
                        // Combine data.
                        let data = self.combine(info, l_data, data);
                        self.cache_insert(key, &data);
                        data
                    }
                }
            }
        }
    }
}

/// Unary zipper step enum type and trait.
///
/// Pretty much the same as `super::unary` but for going down two ZDDs at the same time. The biggest
/// difference is that there is `Step::TLft` zipper step now that means we're going down the left
/// branch and ignore the right one.
pub mod binary {
    use super::*;

    /// A unary zipper step. Type
    ///
    /// - `Key` is for hash key. `HKey` if cached, `()` if not;
    /// - `Label` is for the elements stored in the ZDD;
    /// - `Info` is for the information we want available to construct the result;
    /// - `Data` is for what will be returned eventually.
    pub enum Step<Key, Label, Info, Data> {
        Lft(Key, Info, Zdd<Label>, Zdd<Label>),
        TLft(Key, Info, Data),
        Rgt(Key, Info, Data),
    }

    /// Can zip up a binary zipper.
    pub trait Zip<Key, Label, Info, Data, Zip: super::Zipper<Step<Key, Label, Info, Data>>> {
        /// Inserts into the cache corresponding to `Zip`.
        fn cache_insert(&self, key: Key, data: &Data);

        /// Combines data, used in terminal steps.
        fn combine(&self, info: Info, lhs: Data, rhs: Data) -> Data;

        /// Zips up a binary zip.
        fn zip(&self, mut data: Data, zip: &mut Zip) -> Res<Data, (Zdd<Label>, Zdd<Label>)> {
            loop {
                data = match zip.pop() {
                    // Can't zip up, done.
                    None => return Res::Done(data),

                    // Some right branches haven't been explored yet.
                    Some(Step::Lft(key, info, l_rgt, r_rgt)) => {
                        zip.push(Step::Rgt(key, info, data));
                        return Res::NYet((l_rgt, r_rgt));
                    }

                    // We were in a terminal left branch, going up.
                    Some(Step::TLft(key, info, r_data)) => {
                        // Combine data.
                        let data = self.combine(info, data, r_data);
                        self.cache_insert(key, &data);
                        data
                    }

                    // We were in a right branch, going up.
                    Some(Step::Rgt(key, info, l_data)) => {
                        // Combine data.
                        let data = self.combine(info, l_data, data);
                        self.cache_insert(key, &data);
                        data
                    }
                }
            }
        }
    }
}
