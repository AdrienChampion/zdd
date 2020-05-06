// See the LICENSE files at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A ZDD library, based on [this paper by Shin-Ichi Minato][zdd paper].
//!
//! ZDDs are hash consed so equality is constant time. All operations on ZDDs provided by `Factory`
//! are cached: `count`, `offset`, `onset`, `change`, `union`, `inter`, `minus` and `subset`.
//!
//! # Warning
//!
//! I wrote this crate a very long time ago, when I was a Rust newbie. There seems to be little
//! interest in this library, so I only barely maintain it. If you are serious about using it,
//! consider letting me know to see if I or someone else can improve or rewrite it.
//!
//! # Factory
//!
//! A `Factory` maintains the hash cons table for the ZDDs, as well as the caches for basic
//! operations. A factory is thread-safe. Usage is *Caml*-module-like
//!
//! ```
//! use zdd::* ;
//! let f = Factory::<&'static str>::mk(7) ;
//!
//! let ref zero = f.zero() ;
//! let ref one = f.one() ;
//!
//! let ref x = f.mk_node("x", zero, one) ;
//! let ref _x = f.change(one, "x") ;
//! println!(" x: {}",  x) ;
//! println!("_x: {}", _x) ;
//! assert!(x == _x) ;
//!
//! let ref y = f.change(one, "y") ;
//! let ref x_u_y = f.union(x, y) ;
//! let ref z = f.change(one, "z") ;
//! let ref x_u_y_m_z = f.minus(x_u_y, z) ;
//! assert!(x_u_y_m_z == x_u_y) ;
//! ```
//!
//! # Wrapped
//!
//! The easiest way to manipulate ZDDs is to use `wrapped::Zdd` which is wrapper around a ZDD and
//! its associated factory.
//!
//! ```
//! use zdd::wrapped::* ;
//! let f = mk_factory::<&'static str>(7) ;
//!
//! let ref one = Zdd::one(&f) ;
//!
//! let ref x = one ^ "x"         ; // Change "x" in one.
//! let ref y = one ^ "y"         ; // change "y" in one.
//! let ref z = one ^ "z"         ; // Change "z" in one.
//! let ref x_u_y = x + y         ; // Union.
//! let ref x_u_y_m_z = x_u_y - z ; // Difference.
//! assert!(x_u_y_m_z == x_u_y) ;
//! ```
//!
//! [zdd paper]: http://link.springer.com/article/10.1007%2Fs100090100038
//! (Zero-suppressed BDDs and their applications)

#[macro_use]
extern crate hashconsing;

use std::cmp::Eq;
use std::collections::BTreeSet;
use std::fmt;

use hashconsing::HConsed;

mod print;
pub use print::ZddPrint;

#[macro_use]
mod zip;

pub mod factory;
pub use factory::{
    Factory, FactoryBinOps, FactoryBuilder, FactoryUnLblOps, FactoryUnOps, ZddMaker,
};

pub mod wrapped;

/// A hash consed ZDD.
pub type Zdd<Label> = HConsed<ZddTree<Label>>;

/// Actual ZDD enum type.
///
/// Usually a ZDD is either:
///
/// - a **node** with a label and a left and a right hash consed subtree,
/// - the **one** terminal, the set containing only the null combination,
/// - the **zero** terminal, the empty set.
///
/// However we use *0-element edges* that indicate a path contains the null combination. So there's
/// no **one** terminal.
#[derive(Clone, PartialEq, Hash)]
pub enum ZddTree<Label> {
    /// A node with a label and two kids.
    Node(Label, Zdd<Label>, Zdd<Label>),
    /// Indicates the underlying contains the empty combination.
    HasOne(Zdd<Label>),
    /// The empty set.
    Zero,
}

impl<Label: Ord + Clone> ZddTree<Label> {
    /// Returns an iterator over a ZddTree. There's no `into_iter` since a ZDD is immutable anyway.
    pub fn iter(&self) -> Iterator<Label> {
        let stack = match *self {
            ZddTree::Node(ref lbl, ref lft, ref rgt) => {
                vec![(vec![], lft.clone()), (vec![lbl.clone()], rgt.clone())]
            }
            ZddTree::HasOne(ref kid) => vec![(vec![], kid.clone())],
            ZddTree::Zero => vec![],
        };
        Iterator { stack: stack }
    }
}

impl<Label: Ord + Clone + fmt::Display> fmt::Display for ZddTree<Label> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{{ ")?;
        let mut is_fst = true;
        let stack = match *self {
            ZddTree::HasOne(ref kid) => {
                write!(fmt, "{{}}")?;
                if kid.is_zero() {
                    return write!(fmt, " }}");
                } else {
                    write!(fmt, ", ")?;
                    vec![(vec![], kid.clone())]
                }
            }
            ZddTree::Node(ref lbl, ref lft, ref rgt) => {
                vec![(vec![], lft.clone()), (vec![lbl.clone()], rgt.clone())]
            }
            ZddTree::Zero => return write!(fmt, " }}"),
        };
        let iter = Iterator { stack: stack };
        for vec in iter {
            if is_fst {
                is_fst = false
            } else {
                write!(fmt, ", ")?
            };
            write!(fmt, "{{")?;
            let mut is_fst = true;
            for e in vec.into_iter() {
                if is_fst {
                    is_fst = false
                } else {
                    write!(fmt, ", ")?
                };
                write!(fmt, "{}", e)?;
            }
            write!(fmt, "}}")?;
        }
        write!(fmt, " }}")
    }
}

// |===| Implementations necessary for hash consing.

impl<Label: Clone + Eq> Eq for ZddTree<Label> {}

/// Basic operations on ZDD.
pub trait ZddTreeOps<Label: Ord + Clone> {
    /// Returns true iff the ZDD is *zero*.
    fn is_zero(&self) -> bool;
    /// Returns true iff the ZDD is *one*.
    fn is_one(&self) -> bool;
    /// Returns true for all ZDDs containing the empty combination.
    fn has_one(&self) -> bool;

    /// Returns the top label if the ZDD is a node, an error of `true` if the ZDD is *one* and
    /// `false` if it is *zero*.
    fn top(&self) -> Result<Label, bool>;

    /// Turns a ZDD in the corresponding set of sets of labels.
    fn to_set(&self) -> BTreeSet<BTreeSet<Label>>;

    /// Returns an iterator over a ZDD.
    fn iter(&self) -> Iterator<Label>;
}

impl<Label: Ord + Clone> ZddTreeOps<Label> for Zdd<Label> {
    fn is_zero(&self) -> bool {
        self.top() == Err(false)
    }
    fn is_one(&self) -> bool {
        self.top() == Err(true)
    }
    fn has_one(&self) -> bool {
        match **self {
            ZddTree::HasOne(_) => true,
            _ => false,
        }
    }
    fn top(&self) -> Result<Label, bool> {
        match **self {
            ZddTree::Zero => Err(false),
            // Only one recursive call if ZDD is well-formed.
            ZddTree::HasOne(ref kid) => match **kid {
                ZddTree::Zero => Err(true),
                ZddTree::Node(ref lbl, _, _) => Ok(lbl.clone()),
                _ => panic!("[top] ZDD is ill-formed"),
            },
            ZddTree::Node(ref lbl, _, _) => Ok(lbl.clone()),
        }
    }

    fn to_set(&self) -> BTreeSet<BTreeSet<Label>> {
        match self.top() {
            Err(false) => BTreeSet::new(),
            Err(true) => {
                let mut sset = BTreeSet::new();
                sset.insert(BTreeSet::new());
                sset
            }
            _ => {
                let mut set = BTreeSet::new();
                let mut path = vec![];
                let mut res = BTreeSet::new();
                let mut zdd = self.clone();
                loop {
                    zdd = match *zdd {
                        ZddTree::Node(ref top, ref lft, ref rgt) => {
                            let mut rgt_set = set.clone();
                            rgt_set.insert(top.clone());
                            path.push((rgt.clone(), rgt_set));
                            lft.clone()
                        }
                        ZddTree::HasOne(ref kid) => {
                            res.insert(set.clone());
                            kid.clone()
                        }
                        ZddTree::Zero => {
                            if let Some((nu_zdd, nu_set)) = path.pop() {
                                set = nu_set;
                                nu_zdd
                            } else {
                                return res;
                            }
                        }
                    }
                }
            }
        }
    }

    fn iter(&self) -> Iterator<Label> {
        Iterator {
            stack: vec![(vec![], self.clone())],
        }
    }
}

/// An iterator over the combinations of a ZDD.
pub struct Iterator<Label: Clone> {
    /// A stack of `(prefix, zdd)` where `prefix` are the elements in the combination `zdd` is the
    /// suffix of.
    stack: Vec<(Vec<Label>, Zdd<Label>)>,
}

impl<Label: Ord + Clone> std::iter::Iterator for Iterator<Label> {
    type Item = Vec<Label>;
    fn next(&mut self) -> Option<Vec<Label>> {
        if let Some((prefix, zdd)) = self.stack.pop() {
            let mut pair = (prefix, zdd);
            loop {
                let (mut prefix, zdd) = pair;
                pair = if zdd.is_one() {
                    return Some(prefix);
                } else {
                    if zdd.is_zero() {
                        if let Some((prefix, zdd)) = self.stack.pop() {
                            (prefix, zdd)
                        } else {
                            return None;
                        }
                    } else {
                        match *zdd {
                            ZddTree::HasOne(ref zdd) => {
                                self.stack.push((prefix.clone(), zdd.clone()));
                                return Some(prefix);
                            }
                            ZddTree::Node(ref lbl, ref lft, ref rgt) => {
                                let lft_prefix = prefix.clone();
                                prefix.push(lbl.clone());
                                self.stack.push((lft_prefix, lft.clone()));
                                (prefix, rgt.clone())
                            }
                            _ => unreachable!(),
                        }
                    }
                };
            }
        } else {
            None
        }
    }
}
