// Copyright 2015 Adrien Champion. See the COPYRIGHT file at the top-level
// directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*! ZDD factory.

Everything is hashconsed, so in the end all operations are performed through a
factory. It contains the hash cons table and the cash for the most common
operations.

One should **never** create more than one factory for a given element type.
*/

use std::collections::{ HashMap, BTreeSet, HashSet } ;
use std::sync::{ Arc, Mutex, MutexGuard } ;
use std::marker::Sync ;
use std::cmp::Eq ;
use std::hash::Hash ;

use hashconsing::* ;

use ::{ ZddTree, Zdd, ZddTreeOps } ;
use ZddTree::* ;

use zip ;
use zip::{ HKey, UnaryKey, BinaryKey } ;


/** Unary operations on ZDDs. */
pub trait FactoryUnOps<Label: Ord + Eq + Hash + Clone, ZDD> {
  /** The number of combinations in a ZDD. Cached. */
  fn count (& self, ZDD) -> usize ;
}

/** Unary operations on ZDDs taking a label as parameter. */
pub trait FactoryUnLblOps<Label: Ord + Eq + Hash + Clone, ZDD, LabelParam> {
  /** The set of combinations of `zdd` in which `lbl` does not appear.
    Cached. */
  fn offset(& self, ZDD, LabelParam) -> Zdd<Label> ;
  /** The set of combinations of `zdd` in which `lbl` appears, without `lbl`
    in them. Cached. */
  fn onset (& self, ZDD, LabelParam) -> Zdd<Label> ;
  /** Switches `lbl` in each combination of `zdd`. Inverts `offset` and
    `onset`. Cached. */
  fn change(& self, ZDD, LabelParam) -> Zdd<Label> ;
}

/** Binary operations on ZDDs. */
pub trait FactoryBinOps<Label: Ord + Eq + Hash + Clone, LHS, RHS> {
  /** The union of two ZDDs. Cached. */
  fn union (& self, LHS, RHS) -> Zdd<Label> ;
  /** The intersection of two ZDDs. Cached. */
  fn inter (& self, LHS, RHS) -> Zdd<Label> ;
  /** The difference of two ZDDs. Cached. */
  fn minus (& self, LHS, RHS) -> Zdd<Label> ;

  /** Returns true iff `lhs` is a subset of `rhs`. Cached. */
  fn subset(& self, LHS, RHS) -> bool ;
}

/** Internal ZDD factory trait, creates a node without checking the consistency
  of the label. */
trait ZddFactory<Label: Eq + Hash> {
  /** Creates a node following the ZDD rules, but without checking that the
    label is actually consistent with the kids. */
  fn node(& self, Label, Zdd<Label>, Zdd<Label>) -> Zdd<Label> ;
}

/** Provides safe node creation. */
pub trait ZddMaker<Label: Eq + Hash, Lft, Rgt> {
  /** The precise semantics of this function is as follows:

    ```
    use zdd::* ;

    let consign = Factory::mk() ;
    let lbl = 0 ;
    let zero = consign.zero() ;
    let one = consign.one() ;
    let lft = zero.clone() ;
    let rgt = one.clone() ;

    let zdd1 = {
      let lft = consign.offset(& lft, & lbl) ;
      let rgt = consign.change(& rgt, & lbl) ;
      consign.union(lft, rgt)
    } ;
    let zdd2 = consign.mk_node(lbl, & lft, rgt) ;

    println!("zdd1 = {}", zdd1) ;
    println!("zdd2 = {}", zdd2) ;

    assert!(zdd1 == zdd2) ;

    let lbl = 1 ;

    let zdd1 = {
      let lft = consign.offset(& lft,  & lbl) ;
      let rgt = consign.change(& zdd1, & lbl) ;
      consign.union(lft, rgt)
    } ;
    let zdd2 = consign.mk_node(lbl, lft, zdd2) ;

    println!("zdd1 = {}", zdd1) ;
    println!("zdd2 = {}", zdd2) ;

    assert!(zdd1 == zdd2) ;
    assert_eq!(zdd2.top().unwrap(), 0) ;
    ```

    So, if `lbl` is above all the labels in `lft` and `rgt` it corresponds to
    creating the node `Node(lbl, lft, rgt)` (modulo zdd creation rules
    regarding `HasOne` and `Zero`) with a slight overhead.

    If `lbl` is not above all the labels in `lft` and `rgt` however (as is the
    case in the second call of the example) the construction is safe: the
    resulting ZDD is well-formed. This is not the intented usage though, which
    is why it has slightly weird semantics.

    Providing the actual node creation function is way too dangerous as there's
    a lot of room for screwing up the ZDD well-formed-ness. */
  fn mk_node(& self, Label, Lft, Rgt) -> Zdd<Label> ;
}

/** A ZDD factory.

  Wraps a hash consing table. Functions `count`, `offset`, `onset`, `change`,
  `union`, `inter`, `minus` and `subset` are cached. */
pub struct Factory<Label: Eq + Hash> {
  /** The hash table for ZDDs. */
  consign: Mutex< HashConsign<ZddTree<Label>> >,

  /** The ZDD containing only the empty combination. */
  one: Zdd<Label>,
  /** The empty ZDD. */
  zero: Zdd<Label>,

  /** Cache for `count`. */
  count_cache: Mutex< HashMap<UnaryKey<()>, usize> >,

  /** Cache for `offset`. */
  offset_cache: Mutex< HashMap<UnaryKey<Label>, Zdd<Label>> >,
  /** Cache for `onset`. */
  onset_cache: Mutex< HashMap<UnaryKey<Label>, Zdd<Label>> >,
  /** Cache for `change`. */
  change_cache: Mutex< HashMap<UnaryKey<Label>, Zdd<Label>> >,

  /** Cache for `union`. */
  union_cache: Mutex< HashMap<BinaryKey, Zdd<Label>> >,
  /** Cache for `inter`. */
  inter_cache: Mutex< HashMap<BinaryKey, Zdd<Label>> >,
  /** Cache for `minus`. */
  minus_cache: Mutex< HashMap<BinaryKey, Zdd<Label>> >,

  /** Cache for `subset`. */
  subset_cache: Mutex< HashMap<BinaryKey, bool> >,
}

unsafe impl<Label: Eq + Hash> Sync for Factory<Label> {}


impl<Label: Ord + Eq + Hash + Clone> ZddFactory<Label> for Factory<Label> {
  fn node(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    if rgt == self.zero {
      // Right is zero, no need for `lbl`.
      lft
    } else {
      if let HasOne(ref lft) = * lft {
        // Left is a `HasOne`, pushing upward.
        let node = self.consign.lock().unwrap().mk(
          Node(lbl, lft.clone(), rgt)
        ) ;
        return self.consign.lock().unwrap().mk( HasOne(node) )
      } ;
      self.consign.lock().unwrap().mk( Node(lbl, lft, rgt) )
    }
  }
}

impl<Label: Ord + Eq + Hash + Clone> ZddMaker<
  Label, Zdd<Label>, Zdd<Label>
> for Factory<Label> {
  fn mk_node(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    let lft = self.offset(lft, & lbl) ;
    let rgt = self.change(rgt, & lbl) ;
    self.union(lft, rgt)
  }
}

impl<'a, 'b, Label: Ord + Eq + Hash + Clone> ZddMaker<
  Label, & 'a Zdd<Label>, & 'b Zdd<Label>
> for Factory<Label> {
  fn mk_node(
    & self, lbl: Label, lft: & 'a Zdd<Label>, rgt: & 'b Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft.clone(), rgt.clone())
  }
}

impl<'a, Label: Ord + Eq + Hash + Clone> ZddMaker<
  Label, & 'a Zdd<Label>, Zdd<Label>
> for Factory<Label> {
  fn mk_node(
    & self, lbl: Label, lft: & 'a Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl.clone(), lft.clone(), rgt)
  }
}

impl<'a, Label: Ord + Eq + Hash + Clone> ZddMaker<
  Label, Zdd<Label>, & 'a Zdd<Label>
> for Factory<Label> {
  fn mk_node(
    & self, lbl: Label, lft: Zdd<Label>, rgt: & 'a Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl.clone(), lft, rgt.clone())
  }
}


impl<'a, 'b, Label: Ord + Eq + Hash + Clone + 'a + 'b > Factory<Label>
where Factory<Label> :
FactoryUnOps<Label, Zdd<Label>> +
FactoryUnOps<Label, & 'a Zdd<Label>> +
FactoryUnLblOps<Label, Zdd<Label>, Label> +
FactoryUnLblOps<Label, & 'a Zdd<Label>, Label> +
FactoryUnLblOps<Label, Zdd<Label>, & 'b Label> +
FactoryUnLblOps<Label, & 'a Zdd<Label>, & 'b Label> +
FactoryBinOps<Label, Zdd<Label>, Zdd<Label>> +
FactoryBinOps<Label, & 'a Zdd<Label>, Zdd<Label>> +
FactoryBinOps<Label, Zdd<Label>, & 'b Zdd<Label>> +
FactoryBinOps<Label, & 'a Zdd<Label>, & 'b Zdd<Label>> {

  /** Creates a new factory.

    One should **never** create more than one factory for a given element type.*/
  pub fn mk() -> Self {
    let mut consign = HashConsign::empty() ;
    let zero = consign.mk(Zero) ;
    let one = consign.mk( HasOne(zero.clone()) ) ;
    Factory {
      consign: Mutex::new(consign),

      one: one,
      zero: zero,

      count_cache: Mutex::new( HashMap::new() ),

      offset_cache: Mutex::new( HashMap::new() ),
      onset_cache: Mutex::new( HashMap::new() ),
      change_cache: Mutex::new( HashMap::new() ),

      union_cache: Mutex::new( HashMap::new() ),
      inter_cache: Mutex::new( HashMap::new() ),
      minus_cache: Mutex::new( HashMap::new() ),

      subset_cache: Mutex::new( HashMap::new() ),
    }
  }

  /** The *zero* element, *i.e.* the empty set. */
  #[inline(always)]
  pub fn zero(& self) -> Zdd<Label> { self.zero.clone() }

  /** The *one* element, *i.e.* the set containing only the empty combination.
    */
  #[inline(always)]
  pub fn one(& self) -> Zdd<Label> { self.one.clone() }

  /** Adds the empty combination to a ZDD if it's not already there. */
  #[inline(always)]
  pub fn add_one(& self, kid: & Zdd<Label>) -> Zdd<Label> {
    match * * kid {
      HasOne(_) => kid.clone(),
      _ => self.consign.lock().unwrap().mk(HasOne(kid.clone())),
    }
  }

  /** Removes the empty combination from a ZDD if it's there. */
  #[inline(always)]
  pub fn rm_one(& self, zdd: & Zdd<Label>) -> Zdd<Label> {
    match * * zdd {
      HasOne(ref kid) => kid.clone(),
      _ => zdd.clone(),
    }
  }

  /** Returns the left subtree if the ZDD is a node, an error

    * of `true` if the ZDD is `One` (more precisely `HasOne(Zero)`) and
    * of `false` if it is `Zero`. */
  #[inline(always)]
  pub fn lft(& self, zdd: & Zdd<Label>) -> Result<Zdd<Label>,bool> {
    match * * zdd {
      Node(_, ref lft, _) => Ok(lft.clone()),
      HasOne(ref kid) => match * * kid {
        Node(_, ref lft, _) => Ok(self.add_one(lft)),
        Zero => Err(true),
        _ => panic!("[lft] ZDD is ill-formed"),
      },
      Zero => Err(false),
    }
  }

  /** Returns the right subtree if the ZDD is a node, an error of `true` if the
    ZDD is `One` and `false` if it is `Zero`. */
  #[inline(always)]
  pub fn rgt(& self, zdd: & Zdd<Label>) -> Result<Zdd<Label>,bool> {
    match * * zdd {
      Node(_, _, ref rgt) => Ok(rgt.clone()),
      HasOne(ref kid) => match * * kid {
        Node(_, _, ref rgt) => Ok(rgt.clone()),
        Zero => Err(true),
        _ => panic!("[rgt] ZDD is ill-formed"),
      },
      Zero => Err(false),
    }
  }

  /** Returns the subtrees if the ZDD is a node, an error of `true` if the
    ZDD is `One` and `false` if it is `Zero`. */
  #[inline(always)]
  pub fn kids(
    & self, zdd: & Zdd<Label>
  ) -> Result<(Zdd<Label>, Zdd<Label>),bool> {
    match * * zdd {
      Node(_, ref lft, ref rgt) => Ok((lft.clone(), rgt.clone())),
      HasOne(ref kid) => match * * kid {
        Node(_, ref lft, ref rgt) => Ok(
          (self.add_one(lft), rgt.clone())
        ),
        Zero => Err(true),
        _ => panic!("[rgt] ZDD is ill-formed"),
      },
      Zero => Err(false),
    }
  }

  /** Creates a ZDD containing the combination corresponding to an iterator.
    **Assumes each elements appears only once.** */
  fn of_iterator<T: ::std::iter::Iterator<Item = & 'b Label>>(
    & self, iter: T
  ) -> Zdd<Label> {
    let mut zdd = self.one() ;
    for e in iter {
      zdd = self.change(zdd, e) ;
    } ;
    zdd
  }

  /** Creates a ZDD containing the combination corresponding to a `BTreeSet`.
    */
  pub fn of_btree_set(& self, set: & 'b BTreeSet<Label>) -> Zdd<Label> {
    self.of_iterator(set.iter())
  }

  /** Creates a ZDD containing the combination corresponding to a `HashSet`. */
  pub fn of_hashset(& self, set: & 'b HashSet<Label>) -> Zdd<Label> {
    self.of_iterator(set.iter())
  }

  /** Queries a unary cache. */
  #[inline(always)]
  fn unary_cache_get<Info: Eq + Hash + Clone, Out: Clone>(
    cache: & MutexGuard<HashMap<UnaryKey<Info>, Out>>,
    zdd: & Zdd<Label>,
    info: & Info
  ) -> Option<Out> {
    match (* cache).get( & (zdd.hkey(), info.clone()) ) {
      None => None, Some(out) => Some(out.clone()),
    }
  }

  /** Queries the count cache. */
  #[inline(always)]
  fn count_cache_get(
    & self, zdd: & Zdd<Label>
  ) -> Option<usize> {
    Factory::unary_cache_get(
      & self.count_cache.lock().unwrap(), zdd, & ()
    )
  }

  /** Queries the offset cache. */
  #[inline(always)]
  fn offset_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(
      & self.offset_cache.lock().unwrap(), zdd, lbl
    )
  }

  /** Queries the onset cache. */
  #[inline(always)]
  fn onset_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(
      & self.onset_cache.lock().unwrap(), zdd, lbl
    )
  }

  /** Queries the change cache. */
  #[inline(always)]
  fn change_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(
      & self.change_cache.lock().unwrap(), zdd, lbl
    )
  }

  /** Queries a binary cache. */
  #[inline(always)]
  fn binary_cache_get<T: Clone>(
    cache: & MutexGuard<HashMap<BinaryKey, T>>,
    lhs: & Zdd<Label>,
    rhs: & Zdd<Label>
  ) -> Option<T> {
    match (* cache).get( & (lhs.hkey(), rhs.hkey()) ) {
      None => None, Some(zdd) => Some(zdd.clone()),
    }
  }

  /** Queries the union cache. */
  #[inline(always)]
  fn union_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(
      & self.union_cache.lock().unwrap(), lhs, rhs
    )
  }

  /** Queries the inter cache. */
  #[inline(always)]
  fn inter_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(
      & self.inter_cache.lock().unwrap(), lhs, rhs
    )
  }

  /** Queries the minus cache. */
  #[inline(always)]
  fn minus_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(
      & self.minus_cache.lock().unwrap(), lhs, rhs
    )
  }

  /** Queries the subset cache. */
  #[inline(always)]
  fn subset_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<bool> {
    Factory::binary_cache_get(
      & self.subset_cache.lock().unwrap(), lhs, rhs
    )
  }

  /** The size of the consign. */
  #[inline(always)]
  pub fn consign_len(& self) -> usize { self.consign.lock().unwrap().len() }

  /** The size of the `count` cache. */
  #[inline(always)]
  pub fn count_cache_len(& self) -> usize {
    self.count_cache.lock().unwrap().len()
  }
  /** The size of the `offset` cache. */
  #[inline(always)]
  pub fn offset_cache_len(& self) -> usize {
    self.offset_cache.lock().unwrap().len()
  }
  /** The size of the `onset` cache. */
  #[inline(always)]
  pub fn onset_cache_len(& self) -> usize {
    self.onset_cache.lock().unwrap().len()
  }
  /** The size of the `change` cache. */
  #[inline(always)]
  pub fn change_cache_len(& self) -> usize {
    self.change_cache.lock().unwrap().len()
  }

  /** The size of the `union` cache. */
  #[inline(always)]
  pub fn union_cache_len(& self) -> usize {
    self.union_cache.lock().unwrap().len()
  }
  /** The size of the `inter` cache. */
  #[inline(always)]
  pub fn inter_cache_len(& self) -> usize {
    self.inter_cache.lock().unwrap().len()
  }
  /** The size of the `minus` cache. */
  #[inline(always)]
  pub fn minus_cache_len(& self) -> usize {
    self.minus_cache.lock().unwrap().len()
  }

  /** The size of the `subset` cache. */
  #[inline(always)]
  pub fn subset_cache_len(& self) -> usize {
    self.subset_cache.lock().unwrap().len()
  }
}



impl<Label: Ord + Eq + Hash + Clone> FactoryUnOps<Label, Zdd<Label>>
for Factory<Label> {
  fn count(& self, mut zdd: Zdd<Label>) -> usize {
    use zip::unary::Step::Lft ;
    let mut zip = zip::count() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => zip_up!(self > zip > 0),

        Err(true) => zip_up!(self > zip > 1),

        _ => match self.count_cache_get(& zdd) {
          Some(count) => zip_up!(self > zip > count),
          None => {
            let key = zdd.hkey() ;
            let (lft, rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, (), rgt) ) ;
            lft
          },
        },
      }
    }
  }
}

impl<'a, Label: Ord + Eq + Hash + Clone> FactoryUnOps<
  Label, & 'a Zdd<Label>
> for Factory<Label> {
  fn count(& self, zdd: & 'a Zdd<Label>) -> usize {
    self.count(zdd.clone())
  }
}


impl<Label: Ord + Eq + Hash + Clone> FactoryUnLblOps<Label, Zdd<Label>, Label>
for Factory<Label> {

  fn offset(
    & self, mut zdd: Zdd<Label>, lbl: Label
  ) -> Zdd<Label> {
    use zip::unary::Step::Lft ;
    let mut zip = zip::offset() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => zip_up!(self > zip > self.zero()),

        Err(true) => zip_up!(self > zip > self.one()),

        // Below the label, going up.
        Ok(ref top) if top.gt(& lbl) => zip_up!(self > zip > zdd),

        Ok(ref top) if top.eq(& lbl) => {
          // Only keep left part and go up.
          zip_up!(self > zip > self.lft(& zdd).unwrap())
        },

        // Above the label, querying cache.
        Ok(ref top) => match self.offset_cache_get(& zdd, & lbl) {
          // Cache hit.
          Some(res) => zip_up!(self > zip > res),
          // Not found.
          None => {
            let key = (zdd.hkey(), lbl.clone()) ;
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, top.clone(), rgt) ) ;
            lft
          },
        },
      }
    }
  }

  fn onset(
    & self, mut zdd: Zdd<Label>, lbl: Label
  ) -> Zdd<Label> {
    use zip::unary::Step::Lft ;
    let mut zip = zip::onset() ;
    loop {
      zdd = match zdd.top() {

        Err(_) => zip_up!(self > zip > self.zero()),

        // Below the label, it's not there.
        Ok(ref top) if top.gt(& lbl) => zip_up!(self > zip > self.zero()),

        Ok(ref top) if top.eq(& lbl) => {
          // Only keep right part and go up.
          let rgt = self.rgt(& zdd).unwrap() ;
          zip_up!(self > zip > rgt)
        }

        // Above the label, querying cache.
        Ok(ref top) => match self.onset_cache_get(& zdd, & lbl) {
          // Cache hit.
          Some(res) => zip_up!(self > zip > res),
          // Not found.
          None => {
            let key = (zdd.hkey(), lbl.clone()) ;
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, top.clone(), rgt) ) ;
            lft
          },
        },
      }
    }
  }

  fn change(
    & self, mut zdd: Zdd<Label>, lbl: Label
  ) -> Zdd<Label> {
    use zip::unary::Step::Lft ;
    let mut zip = zip::change() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => zip_up!(self > zip > self.zero()),

        Err(true) => {
          let zero = self.zero() ;
          let one = self.one() ;
          let zdd = self.node(lbl.clone(), zero, one) ;
          zip_up!(self > zip > zdd)
        },

        Ok(ref top) if top.gt(& lbl) => {
          // Below the label, not there so adding it.
          let zero = self.zero() ;
          let zdd = self.node(lbl.clone(), zero, zdd.clone()) ;
          zip_up!(self > zip > zdd)
        },

        Ok(ref top) if top.eq(& lbl) => {
          // Swap left and right.
          let (lft,rgt) = self.kids(& zdd).unwrap() ;
          let zdd = self.node(top.clone(), rgt, lft) ;
          zip_up!(self > zip > zdd)
        },

        // Above the label, querying cache.
        Ok(ref top) => match self.change_cache_get(& zdd, & lbl) {
          // Cache hit.
          Some(res) => zip_up!(self > zip > res),
          // Not found.
          None => {
            let key = (zdd.hkey(), lbl.clone()) ;
            let (lft, rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, top.clone(), rgt) ) ;
            lft
          },
        },
      }
    }
  }
}

impl<'a, Label: Ord + Eq + Hash + Clone> FactoryUnLblOps<
  Label, Zdd<Label>, & 'a Label
> for Factory<Label> {
  fn offset(& self, zdd: Zdd<Label>, lbl: & 'a Label) -> Zdd<Label> {
    self.offset(zdd, lbl.clone())
  }
  fn onset (& self, zdd: Zdd<Label>, lbl: & 'a Label) -> Zdd<Label> {
    self.onset(zdd, lbl.clone())
  }
  fn change(& self, zdd: Zdd<Label>, lbl: & 'a Label) -> Zdd<Label> {
    self.change(zdd, lbl.clone())
  }
}

impl<'a, Label: Ord + Eq + Hash + Clone> FactoryUnLblOps<
  Label, & 'a Zdd<Label>, Label
> for Factory<Label> {
  fn offset(& self, zdd: & 'a Zdd<Label>, lbl: Label) -> Zdd<Label> {
    self.offset(zdd.clone(), lbl)
  }
  fn onset (& self, zdd: & 'a Zdd<Label>, lbl: Label) -> Zdd<Label> {
    self.onset(zdd.clone(), lbl)
  }
  fn change(& self, zdd: & 'a Zdd<Label>, lbl: Label) -> Zdd<Label> {
    self.change(zdd.clone(), lbl)
  }
}

impl<'a, 'b, Label: Ord + Eq + Hash + Clone> FactoryUnLblOps<
  Label, & 'a Zdd<Label>, & 'b Label
> for Factory<Label> {
  fn offset(& self, zdd: & 'a Zdd<Label>, lbl: & 'b Label) -> Zdd<Label> {
    self.offset(zdd.clone(), lbl.clone())
  }
  fn onset (& self, zdd: & 'a Zdd<Label>, lbl: & 'b Label) -> Zdd<Label> {
    self.onset(zdd.clone(), lbl.clone())
  }
  fn change(& self, zdd: & 'a Zdd<Label>, lbl: & 'b Label) -> Zdd<Label> {
    self.change(zdd.clone(), lbl.clone())
  }
}




impl<Label: Ord + Eq + Hash + Clone> FactoryBinOps<
  Label, Zdd<Label>, Zdd<Label>
> for Factory<Label> {

  fn union(
    & self, lhs: Zdd<Label>, rhs: Zdd<Label>
  ) -> Zdd<Label> {
    use zip::binary::Step::{ Lft, TLft } ;
    let mut zip = zip::union() ;
    let mut pair = (lhs, rhs) ;
    loop {

      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > lhs)
      } else {
        match (lhs.top(), rhs.top()) {

          // One of them is the empty set.
          (Err(false), _) => zip_up!(self >> zip > rhs),
          (_, Err(false)) => zip_up!(self >> zip > lhs),

          // One of them contains only the empty combination.
          (Err(true), _) => {
            let zdd = self.add_one(& rhs) ;
            zip_up!(self >> zip > zdd)
          },
          (_, Err(true)) => {
            let zdd = self.add_one(& lhs) ;
            zip_up!(self >> zip > zdd)
          },

          // Both are nodes.
          (Ok(l_top), Ok(r_top)) => {
            // Reordering to have rhs below lhs.
            let (lhs, rhs, l_top, r_top) = if l_top > r_top {
              (rhs, lhs, r_top, l_top)
            } else {
              (lhs, rhs, l_top, r_top)
            } ;

            // Querying cache.
            match self.union_cache_get(& lhs, & rhs) {
              // Cache hit.
              Some(res) => zip_up!(self >> zip > res),
              // Not found.
              None => {
                let key = (lhs.hkey(), rhs.hkey()) ;

                if l_top == r_top {
                  // Extracting kids.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                  // Recursing.
                  zip.push(Lft(key, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                } else {
                  // Making sure lhs is above.
                  assert!(l_top < r_top) ;
                  // Extracting kids from lhs.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  // Recursing on left kid.
                  zip.push(TLft(key, l_top, l_rgt)) ;
                  (l_lft, rhs)
                }
              },
            }
          },
        }
      } ;
    }
  }

  fn inter(
    & self, lhs: Zdd<Label>, rhs: Zdd<Label>
  ) -> Zdd<Label> {
    use zip::binary::Step::Lft ;
    let mut zip = zip::inter() ;
    let mut pair = (lhs, rhs) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > lhs)
      } else {
        match (lhs.top(), rhs.top()) {

          // Trivial cases.
          (Err(false), _) | (_, Err(false)) =>
            zip_up!(self >> zip > self.zero()),

          (Err(true), _) => {
            let zdd = if rhs.has_one() { lhs } else { self.zero() } ;
            zip_up!(self >> zip > zdd)
          },
          (_, Err(true)) => {
            let zdd = if lhs.has_one() { rhs } else { self.zero() } ;
            zip_up!(self >> zip > zdd)
          },

          // Both are nodes.
          (Ok(l_top), Ok(r_top)) => if l_top == r_top {
            // Querying cache.
            match self.inter_cache_get(& lhs, & rhs) {
              // Cache hit.
              Some(res) => zip_up!(self >> zip > res),
              // Not found.
              None => {
                let key = (lhs.hkey(), rhs.hkey()) ;
                // Extracting kids.
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                // Recursing.
                zip.push(Lft(key, l_top, l_rgt, r_rgt)) ;
                (l_lft, r_lft)
              },
            }
          } else {
            if l_top < r_top {
              (self.lft(& lhs).unwrap(), rhs)
            } else {
              (self.lft(& rhs).unwrap(), lhs)
            }
          },
        }
      } ;
    }
  }

  fn minus(
    & self, lhs: Zdd<Label>, rhs: Zdd<Label>
  ) -> Zdd<Label> {
    use zip::binary::Step::{ Lft, TLft } ;
    let mut zip = zip::minus() ;
    let mut pair = (lhs, rhs) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > self.zero())
      } else {
        match (lhs.top(), rhs.top()) {

          // One of them is the empty set.
          (Err(false), _) | (_, Err(false)) =>
            zip_up!(self >> zip > lhs),

          // One of them contains only the empty combination.
          (Err(true), _) => {
            let zdd = if rhs.has_one() { self.zero() } else { lhs } ;
            zip_up!(self >> zip > zdd)
          },
          (_, Err(true)) => {
            let zdd = self.rm_one(& lhs) ;
            zip_up!(self >> zip > zdd)
          },

          // Both are nodes, querying cache.
          (Ok(l_top), Ok(r_top)) => match self.minus_cache_get(& lhs, & rhs) {
            // Cache hit.
            Some(res) => zip_up!(self >> zip > res),
            // Not found.
            None => {
              let key = (lhs.hkey(), rhs.hkey()) ;
              if l_top < r_top {
                // lhs is above.
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                // Recursing on left kid.
                zip.push(TLft(key, l_top, l_rgt)) ;
                (l_lft, rhs)
              } else {
                if r_top < l_top {
                  // rhs is above, discarding its right kid.
                  let r_lft = self.lft(& rhs).unwrap() ;
                  // Recursing.
                  (lhs, r_lft)
                } else {
                  // Extracting kids.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                  // Recursing.
                  zip.push(Lft(key, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                }
              }
            },
          },
        }
      } ;
    }
  }

  fn subset(
    & self, lhs: Zdd<Label>, rhs: Zdd<Label>
  ) -> bool {
    use zip::binary::Step::{ Lft, TLft } ;
    let mut zip = zip::subset() ;
    // More natural for me to reverse them.
    let mut pair = (rhs, lhs) ;
    loop {
      // Remember we're answering "is `lhs` a subset of `rhs`" as we reversed
      // `lhs` and `rhs`.
      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > true)
      } else {
        match (lhs.top(), rhs.top()) {
          (_, Err(false)) => zip_up!(self >> zip > true),
          (Err(true), _) |
          (Err(false), _) => zip_up!(self >> zip > false),
          (_, Err(true)) => zip_up!(self >> zip > lhs.has_one()),
          (Ok(l_top), Ok(r_top)) => match self.subset_cache_get(& lhs, & rhs) {
            // Cache hit.
            Some(res) => zip_up!(self >> zip > res),
            // Not found.
            None => {
              let key = (lhs.hkey(), rhs.hkey()) ;
              if l_top == r_top {
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                zip.push(Lft(key, (), l_rgt, r_rgt)) ;
                (l_lft, r_lft)
              } else {
                if l_top < r_top {
                  // lhs is above, going left.
                  let l_lft = self.lft(& lhs).unwrap() ;
                  zip.push(TLft(key, (), true)) ;
                  (l_lft, rhs)
                } else {
                  // rhs is above, lhs cannot be a subset.
                  zip_up!(self >> zip > false)
                }
              }
            },
          }
        }
      } ;
    }
  }
}


impl<'a, Label: Ord + Eq + Hash + Clone> FactoryBinOps<
  Label, & 'a Zdd<Label>, Zdd<Label>
> for Factory<Label> {
  fn union(
    & self, lhs: & 'a Zdd<Label>, rhs: Zdd<Label>
  ) -> Zdd<Label> { self.union(lhs.clone(), rhs) }
  fn inter(
    & self, lhs: & 'a Zdd<Label>, rhs: Zdd<Label>
  ) -> Zdd<Label> { self.inter(lhs.clone(), rhs) }
  fn minus(
    & self, lhs: & 'a Zdd<Label>, rhs: Zdd<Label>
  ) -> Zdd<Label> { self.minus(lhs.clone(), rhs) }
  fn subset(
    & self, lhs: & 'a Zdd<Label>, rhs: Zdd<Label>
  ) -> bool { self.subset(lhs.clone(), rhs) }
}


impl<'a, Label: Ord + Eq + Hash + Clone> FactoryBinOps<
  Label, Zdd<Label>, & 'a Zdd<Label>
> for Factory<Label> {
  fn union(
    & self, lhs: Zdd<Label>, rhs: & 'a Zdd<Label>
  ) -> Zdd<Label> { self.union(lhs, rhs.clone()) }
  fn inter(
    & self, lhs: Zdd<Label>, rhs: & 'a Zdd<Label>
  ) -> Zdd<Label> { self.inter(lhs, rhs.clone()) }
  fn minus(
    & self, lhs: Zdd<Label>, rhs: & 'a Zdd<Label>
  ) -> Zdd<Label> { self.minus(lhs, rhs.clone()) }
  fn subset(
    & self, lhs: Zdd<Label>, rhs: & 'a Zdd<Label>
  ) -> bool { self.subset(lhs, rhs.clone()) }
}


impl<'a, 'b, Label: Ord + Eq + Hash + Clone> FactoryBinOps<
  Label, & 'a Zdd<Label>, & 'b Zdd<Label>
> for Factory<Label> {
  fn union(
    & self, lhs: & 'a Zdd<Label>, rhs: & 'b Zdd<Label>
  ) -> Zdd<Label> { self.union(lhs.clone(), rhs.clone()) }
  fn inter(
    & self, lhs: & 'a Zdd<Label>, rhs: & 'b Zdd<Label>
  ) -> Zdd<Label> { self.inter(lhs.clone(), rhs.clone()) }
  fn minus(
    & self, lhs: & 'a Zdd<Label>, rhs: & 'b Zdd<Label>
  ) -> Zdd<Label> { self.minus(lhs.clone(), rhs.clone()) }
  fn subset(
    & self, lhs: & 'a Zdd<Label>, rhs: & 'b Zdd<Label>
  ) -> bool { self.subset(lhs.clone(), rhs.clone()) }
}



#[cfg(test)]
#[inline(always)]
fn cache_overwrite<T>(insert_result: Option<T>, name: & str) {
  match insert_result {
    None => (),
    Some(_) => panic!("cache overwrite in {} cache", name),
  }
}

#[cfg(not(test))]
#[inline(always)]
fn cache_overwrite<T>(_: Option<T>, _: & str) { () }

impl<Label: Ord + Eq + Hash + Clone> zip::unary::Zip<
  HKey, Label, (), usize, zip::Count<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: HKey, count: & usize) {
    let _res = self.count_cache.lock().unwrap().insert((key, ()), * count) ;
    cache_overwrite(_res, "count")
  }
  fn combine(& self, _: (), l_count: usize, r_count: usize) -> usize {
    l_count + r_count
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::unary::Zip<
  (HKey, Label), Label, Label, Zdd<Label>, zip::Offset<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, Label), zdd: & Zdd<Label>) {
    let _res = self.offset_cache.lock().unwrap().insert(key, zdd.clone()) ;
    cache_overwrite(_res, "offset")
  }
  fn combine(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.node(lbl, lft, rgt)
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::unary::Zip<
  (HKey, Label), Label, Label, Zdd<Label>, zip::Onset<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, Label), zdd: & Zdd<Label>) {
    let _res = self.onset_cache.lock().unwrap().insert(key, zdd.clone()) ;
    cache_overwrite(_res, "onset")
  }
  fn combine(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.node(lbl, lft, rgt)
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::unary::Zip<
  (HKey, Label), Label, Label, Zdd<Label>, zip::Change<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, Label), zdd: & Zdd<Label>) {
    let _res = self.change_cache.lock().unwrap().insert(key, zdd.clone()) ;
    cache_overwrite(_res, "change")
  }
  fn combine(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.node(lbl, lft, rgt)
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, Label, Zdd<Label>, zip::Union<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, HKey), zdd: & Zdd<Label>) {
    let _res = self.union_cache.lock().unwrap().insert(key, zdd.clone()) ;
    cache_overwrite(_res, "union")
  }
  fn combine(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.node(lbl, lft, rgt)
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, Label, Zdd<Label>, zip::Inter<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, HKey), zdd: & Zdd<Label>) {
    let _res = self.inter_cache.lock().unwrap().insert(key, zdd.clone()) ;
    cache_overwrite(_res, "inter")
  }
  fn combine(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.node(lbl, lft, rgt)
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, Label, Zdd<Label>, zip::Minus<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, HKey), zdd: & Zdd<Label>) {
    let _res = self.minus_cache.lock().unwrap().insert(key, zdd.clone()) ;
    cache_overwrite(_res, "minus")
  }
  fn combine(
    & self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.node(lbl, lft, rgt)
  }
}


impl<Label: Ord + Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, (), bool, zip::Subset<Label>
> for Factory<Label> {
  fn cache_insert(& self, key: (HKey, HKey), val: & bool) {
    let _res = self.subset_cache.lock().unwrap().insert(key, * val) ;
    cache_overwrite(_res, "subset")
  }
  fn combine(& self, _: (), lft: bool, rgt: bool) -> bool {
    lft && rgt
  }
}



/** A factory for `Factory` to set the capacity of the consign and the caches.

  ## Example

  Each initial capacity can be set separately.

  ```
  use zdd::FactoryBuilder ;

  let factory = FactoryBuilder::mk()
    .consign_len(1)
    .count_cache_len(2)
    .offset_cache_len(3).onset_cache_len(4).change_cache_len(5)
    .union_cache_len(6).inter_cache_len(7).minus_cache_len(8)
    .build::<usize>() ;
  ```

  Or at the same time.

  ```
  use zdd::FactoryBuilder ;
  let factory = FactoryBuilder::mk().len(7).build::<usize>() ;
  ```

  It is also possible to set all the caches capacities at once.

  ```
  use zdd::FactoryBuilder ;
  let factory = FactoryBuilder::mk().caches_len(7).build::<usize>() ;
  ```

  */
pub struct FactoryBuilder {
  consign: usize,

  count: usize,

  offset: usize, onset: usize, change: usize,

  union: usize, inter: usize, minus: usize,

  subset: usize,
}

impl FactoryBuilder {
  /** Builds a factory with the corresponding capacities. */
  pub fn build<Label: Eq + Hash>(self) -> Factory<Label> {
    let mut consign = HashConsign::empty_with_capacity(self.consign) ;
    let zero = consign.mk(Zero) ;
    let one = consign.mk( HasOne(zero.clone()) ) ;
    Factory {
      consign: Mutex::new(consign),

      one: one,
      zero: zero,

      count_cache: Mutex::new( HashMap::with_capacity(self.count) ),

      offset_cache: Mutex::new( HashMap::with_capacity(self.offset) ),
      onset_cache: Mutex::new( HashMap::with_capacity(self.onset) ),
      change_cache: Mutex::new( HashMap::with_capacity(self.change) ),

      union_cache: Mutex::new( HashMap::with_capacity(self.union) ),
      inter_cache: Mutex::new( HashMap::with_capacity(self.inter) ),
      minus_cache: Mutex::new( HashMap::with_capacity(self.minus) ),

      subset_cache: Mutex::new( HashMap::with_capacity(self.subset) ),
    }
  }

  /** Builds an Arc of a factory with the corresponding capacities. */
  pub fn build_arc<Label: Eq + Hash>(self) -> Arc<Factory<Label>> {
    Arc::new( self.build() )
  }

  /** Creates a new factory builder with all capacities equal to zero. */
  pub fn mk() -> Self {
    FactoryBuilder {
      consign: 0,

      count: 0,

      offset: 0, onset: 0, change: 0,

      union: 0, inter: 0, minus: 0,

      subset: 0,
    }
  }

  /** Sets the capacities of the consign and all the caches at once. */
  pub fn len(mut self, l: usize) -> Self {
    self.consign = l ;
    self.caches_len(l)
  }

  /** Sets the capacity of the consign. */
  pub fn consign_len(mut self, l: usize) -> Self {
    self.consign = l ; self
  }

  /** Sets the capacities of all the caches at once. */
  pub fn caches_len(mut self, l: usize) -> Self {
    self.count = l ;
    self.offset = l ;
    self.onset = l ;
    self.change = l ;
    self.union = l ;
    self.inter = l ;
    self.minus = l ;
    self.subset = l ;
    self
  }

  /** Sets the capacity of the count cache. */
  pub fn count_cache_len(mut self, l: usize) -> Self {
    self.count = l ; self
  }

  /** Sets the capacity of the offset cache. */
  pub fn offset_cache_len(mut self, l: usize) -> Self {
    self.offset = l ; self
  }
  /** Sets the capacity of the onset cache. */
  pub fn onset_cache_len(mut self, l: usize) -> Self {
    self.onset = l ; self
  }
  /** Sets the capacity of the change cache. */
  pub fn change_cache_len(mut self, l: usize) -> Self {
    self.change = l ; self
  }

  /** Sets the capacity of the union cache. */
  pub fn union_cache_len(mut self, l: usize) -> Self {
    self.union = l ; self
  }
  /** Sets the capacity of the inter cache. */
  pub fn inter_cache_len(mut self, l: usize) -> Self {
    self.inter = l ; self
  }
  /** Sets the capacity of the minus cache. */
  pub fn minus_cache_len(mut self, l: usize) -> Self {
    self.minus = l ; self
  }

  /** Sets the capacity of the minus cache. */
  pub fn subset_cache_len(mut self, l: usize) -> Self {
    self.subset = l ; self
  }
}