// See the LICENSE files at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! ZDDs containing their factory.
//!
//! They are equipped with the following operators:
//!
//! | Unary operators |                      |    Binary operators  |      |
//! |:-----------:|:------------------------:|:------------:|:------------:|
//! | <code>zdd &#124; elem</code> | offset of `elm` in `zdd` | `lhs + rhs`  | union        |
//! | `zdd % elm` | onset of `elm` in `zdd`  | `lhs - rhs`  | difference   |
//! | `zdd ^ elm` | change of `elm` in `zdd` | `lhs & rhs`  | intersection |
//! |             |                          | `lhs << rhs` | subset       |

use std::borrow::Borrow;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::BTreeSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;
use std::marker::{Send, Sync};
use std::ops::{Add, BitAnd, BitOr, BitXor, Deref, Rem, Shl, Sub};
use std::sync::Arc;

use hashconsing::HConsed;

use crate::factory::{FactoryBinOps, FactoryUnLblOps, FactoryUnOps};

pub use crate::{factory, print::ZddPrint, Iterator, ZddTree, ZddTreeOps};

/// An `Arc` of a factory.
pub type Factory<Label> = Arc<factory::Factory<Label>>;

/// Creates a thread-safe factory.
pub fn mk_factory<Label>(capacity: usize) -> Factory<Label>
where
    Label: Eq + Hash + Ord + Clone,
{
    Arc::new(factory::Factory::mk(capacity))
}

/// A wrapper around a ZDD and its factory.
#[derive(Clone)]
pub struct Zdd<Label: Eq + Hash + Clone> {
    zdd: HConsed<ZddTree<Label>>,
    factory: Factory<Label>,
}

impl<Label: Eq + Hash + Clone + Ord> Zdd<Label> {
    /// Creates an empty wrapped ZDD.
    pub fn zero(factory: &Factory<Label>) -> Self {
        Zdd {
            zdd: factory.zero(),
            factory: factory.clone(),
        }
    }

    /// Creates a wrapped ZDD containing the empty combination only.
    pub fn one(factory: &Factory<Label>) -> Self {
        Zdd {
            zdd: factory.one(),
            factory: factory.clone(),
        }
    }

    /// Creates a wrapped ZDD from a regular ZDD.
    pub fn of(zdd: &HConsed<ZddTree<Label>>, factory: &Factory<Label>) -> Self {
        Zdd {
            zdd: zdd.clone(),
            factory: factory.clone(),
        }
    }

    /// The underlying Zdd.
    pub fn get(&self) -> &ZddTree<Label> {
        &self.zdd
    }
    /// The underlying factory.
    pub fn get_factory(&self) -> &Factory<Label> {
        &self.factory
    }

    /// Adds the empty combination to a ZDD if it's not already there.
    #[inline(always)]
    pub fn add_one(&self) -> Self {
        let zdd = self.factory.add_one(&self.zdd);
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }

    /// Removes the empty combination from a ZDD if it's there.
    #[inline(always)]
    pub fn rm_one(&self) -> Self {
        let zdd = self.factory.rm_one(&self.zdd);
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }

    /// Returns the left subtree if the ZDD is a node, an error
    ///
    /// - of `true` if the ZDD is `One` (more precisely `HasOne(Zero)`) and
    /// - of `false` if it is `Zero`.
    #[inline(always)]
    pub fn lft(&self) -> Result<Zdd<Label>, bool> {
        match self.factory.lft(&self.zdd) {
            Ok(zdd) => Ok(Zdd {
                zdd: zdd,
                factory: self.factory.clone(),
            }),
            Err(b) => Err(b),
        }
    }

    /// Returns the right subtree if the ZDD is a node, an error of `true` if the ZDD is `One` and
    /// `false` if it is `Zero`.
    #[inline(always)]
    pub fn rgt(&self) -> Result<Zdd<Label>, bool> {
        match self.factory.lft(&self.zdd) {
            Ok(zdd) => Ok(Zdd {
                zdd: zdd,
                factory: self.factory.clone(),
            }),
            Err(b) => Err(b),
        }
    }

    /// Returns the subtrees if the ZDD is a node, an error of `true` if the ZDD is `One` and
    /// `false` if it is `Zero`.
    #[inline(always)]
    pub fn kids(&self) -> Result<(Zdd<Label>, Zdd<Label>), bool> {
        match self.factory.kids(&self.zdd) {
            Ok((lft, rgt)) => Ok((
                Zdd {
                    zdd: lft,
                    factory: self.factory.clone(),
                },
                Zdd {
                    zdd: rgt,
                    factory: self.factory.clone(),
                },
            )),
            Err(b) => Err(b),
        }
    }

    /// The number of combinations in a ZDD. Cached.
    pub fn count(&self) -> usize {
        self.factory.count(&self.zdd)
    }

    /// The set of combinations of `zdd` in which `lbl` does not appear. Cached.
    pub fn offset(&self, lbl: Label) -> Self {
        let zdd = self.factory.offset(&self.zdd, lbl.borrow());
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }
    /// The set of combinations of `zdd` in which `lbl` appears, without `lbl` in them. Cached.
    pub fn onset(&self, lbl: Label) -> Self {
        let zdd = self.factory.onset(&self.zdd, lbl.borrow());
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }
    /// Switches `lbl` in each combination of `zdd`. Inverts `offset` and `onset`. Cached.
    pub fn change(&self, lbl: Label) -> Self {
        let zdd = self.factory.change(&self.zdd, lbl.borrow());
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }

    /// The union of two ZDDs. Cached.
    pub fn union(&self, rhs: Self) -> Self {
        let zdd = self.factory.union(&self.zdd, &rhs.borrow().zdd);
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }
    /// The intersection of two ZDDs. Cached.
    pub fn inter(&self, rhs: Self) -> Self {
        let zdd = self.factory.inter(&self.zdd, &rhs.borrow().zdd);
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }
    /// The difference of two ZDDs. Cached.
    pub fn minus(&self, rhs: Self) -> Self {
        let zdd = self.factory.minus(&self.zdd, &rhs.borrow().zdd);
        Zdd {
            zdd: zdd,
            factory: self.factory.clone(),
        }
    }

    /// Returns true iff `lhs` is a subset of `rhs`. Cached.
    pub fn subset(&self, rhs: Self) -> bool {
        self.factory.subset(&self.zdd, &rhs.borrow().zdd)
    }
}

impl<Label: Eq + Hash + Ord + Clone> ZddTreeOps<Label> for Zdd<Label> {
    fn is_zero(&self) -> bool {
        self.zdd.is_zero()
    }
    fn is_one(&self) -> bool {
        self.zdd.is_one()
    }
    fn has_one(&self) -> bool {
        self.zdd.has_one()
    }
    fn top(&self) -> Result<Label, bool> {
        self.zdd.top()
    }
    fn to_set(&self) -> BTreeSet<BTreeSet<Label>> {
        self.zdd.to_set()
    }
    fn iter(&self) -> Iterator<Label> {
        self.zdd.iter()
    }
}

unsafe impl<Label: Eq + Hash + Clone> Sync for Zdd<Label> {}
unsafe impl<Label: Eq + Hash + Clone> Send for Zdd<Label> {}

impl<Label: Eq + Hash + Clone> PartialEq for Zdd<Label> {
    fn eq(&self, rhs: &Self) -> bool {
        self.zdd.eq(&rhs.zdd)
    }
}

impl<Label: Eq + Hash + Clone> Eq for Zdd<Label> {}

impl<Label: Eq + Hash + Clone> Hash for Zdd<Label> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.zdd.hash(state)
    }
}

impl<Label: Eq + Hash + Clone> PartialOrd for Zdd<Label> {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        self.zdd.partial_cmp(&rhs.zdd)
    }
}
impl<Label: Eq + Hash + Clone> Ord for Zdd<Label> {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.zdd.cmp(&rhs.zdd)
    }
}

impl<Label: Eq + Hash + Clone> Deref for Zdd<Label> {
    type Target = ZddTree<Label>;
    fn deref(&self) -> &ZddTree<Label> {
        &self.zdd
    }
}

impl<Label: Eq + Hash + Clone + Ord + fmt::Display> fmt::Display for Zdd<Label> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.zdd.fmt(fmt)
    }
}

macro_rules! mk_op {
    ($id:ident<$lbl:ident>, $fun:ident(
        $lhs:ident, $rhs:ident: $t:ty
    ) $out:ty = $b:block) => (
        impl<$lbl: Eq + Hash + Clone + Ord> $id<$t> for Zdd<$lbl> {
            type Output = $out ;
            fn $fun($lhs, $rhs: $t) -> $out $b
        }
        impl<
            'a, $lbl: Eq + Hash + Clone + Ord
        > $id<$t> for &'a Zdd<$lbl> {
            type Output = $out ;
            fn $fun(self, rhs: $t) -> $out { self.clone().$fun(rhs) }
        }
        impl<
            'a, $lbl: Eq + Hash + Clone + Ord
        > $id<&'a $t> for Zdd<$lbl> {
            type Output = $out ;
            fn $fun(self, rhs: &'a $t) -> $out {
                self.$fun(rhs.clone())
            }
        }
        impl<
            'a, 'b, $lbl: Eq + Hash + Clone + Ord
        > $id<&'a $t> for &'b Zdd<$lbl> {
            type Output = $out ;
            fn $fun(self, rhs: &'a $t) -> $out {
                self.clone().$fun(rhs.clone())
            }
        }
    );
}

mk_op! {
    BitOr<Label>, bitor(self, lbl: Label) Zdd<Label> = { self.offset(lbl) }
}
mk_op! {
    Rem<Label>, rem(self, lbl: Label) Zdd<Label> = { self.onset(lbl) }
}
mk_op! {
    BitXor<Label>, bitxor(self, lbl: Label) Zdd<Label> = { self.change(lbl) }
}
mk_op! {
    Add<Label>, add(self, rhs: Zdd<Label>) Zdd<Label> = { self.union(rhs) }
}
mk_op! {
    Sub<Label>, sub(self, rhs: Zdd<Label>) Zdd<Label> = { self.minus(rhs) }
}
mk_op! {
    BitAnd<Label>, bitand(self, rhs: Zdd<Label>) Zdd<Label> = { self.inter(rhs) }
}
mk_op! {
    Shl<Label>, shl(self, rhs: Zdd<Label>) bool = { self.subset(rhs) }
}

impl<Label: fmt::Display + Ord + Hash + Clone> ZddPrint<Label> for Zdd<Label> {
    fn print(&self, pref: String) {
        self.zdd.print(pref)
    }

    fn write_as_gv(&self, wrt: &mut impl io::Write) -> io::Result<()> {
        self.zdd.write_as_gv(wrt)
    }
}
