/*! ZDD containing their factory. */

use std::fmt ;
use std::cmp::{ PartialEq, Eq, PartialOrd, Ord, Ordering } ;
use std::ops::{ Add, Sub, BitAnd, Deref, DerefMut } ;
use std::hash::{ Hash, Hasher } ;
use std::borrow::Borrow ;
use std::collections::BTreeSet ;
use std::sync::{ Arc, Mutex,  } ;
use std::marker::{ Send, Sync } ;

use hashconsing::HConsed ;

pub use ZddTree ;
pub use ZddTreeOps ;
pub use Iterator ;
use factory::{ FactoryUnOps, FactoryUnLblOps, FactoryBinOps } ;

// trait FactoryOps<Label> {
//   fn mk() -> Self ;
//   fn zero(& self) -> Zdd<Label> ;
//   fn one(& self) -> Zdd<Label> ;
//   fn add_one(& self, kid: Zdd<Label>) -> Zdd<Label> ;
//   fn rm_one(& self, zdd: Zdd<Label>) -> Zdd<Label> ;
//   fn lft(& self, zdd: Zdd<Label>
// }

pub type Factory<Label> = Arc<Mutex<::factory::Factory<Label>>> ;

#[derive(Clone)]
pub struct Zdd<Label: Eq + Hash + Clone> {
  zdd: HConsed<ZddTree<Label>>,
  factory: Factory<Label>,
}

impl<Label: Eq + Hash + Clone> Zdd<Label> {
  pub fn of(
    zdd: & HConsed<ZddTree<Label>>, factory: & Factory<Label>
  ) -> Self {
    Zdd { zdd: zdd.clone(), factory: factory.clone() }
  }
  pub fn get(& self) -> & ZddTree<Label> { & self.zdd }
}

impl<Label: Eq + Hash + Clone + Ord> Zdd<Label> {
  pub fn count(& self) -> usize {
    self.factory.lock().unwrap().deref_mut().count(& self.zdd)
  }

  pub fn offset(& self, lbl: Label) -> Self {
    let zdd = self.factory.lock().unwrap().deref_mut().offset(
      & self.zdd, lbl.borrow()
    ) ;
    Zdd { zdd: zdd, factory: self.factory.clone() }
  }
  pub fn onset(& self, lbl: Label) -> Self {
    let zdd = self.factory.lock().unwrap().deref_mut().onset(
      & self.zdd, lbl.borrow()
    ) ;
    Zdd { zdd: zdd, factory: self.factory.clone() }
  }
  pub fn change(& self, lbl: Label) -> Self {
    let zdd = self.factory.lock().unwrap().deref_mut().change(
      & self.zdd, lbl.borrow()
    ) ;
    Zdd { zdd: zdd, factory: self.factory.clone() }
  }

  pub fn union(& self, rhs: Self) -> Self {
    let zdd = self.factory.lock().unwrap().deref_mut().union(
      & self.zdd, & rhs.borrow().zdd
    ) ;
    Zdd { zdd: zdd, factory: self.factory.clone() }
  }
  pub fn inter(& self, rhs: Self) -> Self {
    let zdd = self.factory.lock().unwrap().deref_mut().inter(
      & self.zdd, & rhs.borrow().zdd
    ) ;
    Zdd { zdd: zdd, factory: self.factory.clone() }
  }
  pub fn minus(& self, rhs: Self) -> Self {
    let zdd = self.factory.lock().unwrap().deref_mut().minus(
      & self.zdd, & rhs.borrow().zdd
    ) ;
    Zdd { zdd: zdd, factory: self.factory.clone() }
  }

  pub fn subset(& self, rhs: Self) -> bool {
    self.factory.lock().unwrap().deref_mut().subset(
      & self.zdd, & rhs.borrow().zdd
    )
  }
}

impl<Label: Eq + Hash + Ord + Clone> ZddTreeOps<Label> for Zdd<Label> {
  fn is_zero(& self) -> bool { self.zdd.is_zero() }
  fn is_one(& self) -> bool { self.zdd.is_one() }
  fn has_one(& self) -> bool { self.zdd.has_one() }
  fn top(& self) -> Result<Label,bool> { self.zdd.top() }
  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> { self.zdd.to_set() }
  fn iter(& self) -> Iterator<Label> { self.zdd.iter() }
}

unsafe impl<Label: Eq + Hash + Clone> Sync for Zdd<Label> {}
unsafe impl<Label: Eq + Hash + Clone> Send for Zdd<Label> {}

impl<Label: Eq + Hash + Clone> PartialEq for Zdd<Label> {
  fn eq(& self, rhs: & Self) -> bool { self.zdd.eq(& rhs.zdd) }
}

impl<Label: Eq + Hash + Clone> Eq for Zdd<Label> { }

impl<Label: Eq + Hash + Clone> Hash for Zdd<Label> {
  fn hash<H: Hasher>(& self, state: & mut H) {
    self.zdd.hash(state)
  }
}

impl<Label: Eq + Hash + Clone> PartialOrd for Zdd<Label> {
  fn partial_cmp(& self, rhs: & Self) -> Option<Ordering> {
    self.zdd.partial_cmp(& rhs.zdd)
  }
}
impl<Label: Eq + Hash + Clone> Ord for Zdd<Label> {
  fn cmp(& self, rhs: & Self) -> Ordering {
    self.zdd.cmp(& rhs.zdd)
  }
}

impl<Label: Eq + Hash + Clone> Deref for Zdd<Label> {
  type Target = ZddTree<Label> ;
  fn deref(& self) -> & ZddTree<Label> { & self.zdd }
}

impl<
  Label: Eq + Hash + Clone + Ord + fmt::Display
> fmt::Display for Zdd<Label> {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    self.zdd.fmt(fmt)
  }
}

macro_rules! mk_op {
  ($id:ident, $fun:ident($lhs:ident, $rhs:ident) $b:block) => (
    impl<Label: Eq + Hash + Clone + Ord> $id<Zdd<Label>> for Zdd<Label> {
      type Output = Zdd<Label> ;
      fn $fun($lhs, $rhs: Zdd<Label>) -> Zdd<Label> $b
    }
    impl<
      'a, Label: Eq + Hash + Clone + Ord
    > $id<Zdd<Label>> for & 'a Zdd<Label> {
      type Output = Zdd<Label> ;
      fn $fun(self, rhs: Zdd<Label>) -> Zdd<Label> { self.clone().$fun(rhs) }
    }
    impl<
      'a, Label: Eq + Hash + Clone + Ord
    > $id<& 'a Zdd<Label>> for Zdd<Label> {
      type Output = Zdd<Label> ;
      fn $fun(self, rhs: & 'a Zdd<Label>) -> Zdd<Label> {
        self.$fun(rhs.clone())
      }
    }
    impl<
      'a, 'b, Label: Eq + Hash + Clone + Ord
    > $id<& 'a Zdd<Label>> for & 'b Zdd<Label> {
      type Output = Zdd<Label> ;
      fn $fun(self, rhs: & 'a Zdd<Label>) -> Zdd<Label> {
        self.clone().$fun(rhs.clone())
      }
    }
  ) ;
}

mk_op!{ Add, add(self, rhs) { self.union(rhs) } }
mk_op!{ Sub, sub(self, rhs) { self.minus(rhs) } }
mk_op!{ BitAnd, bitand(self, rhs) { self.inter(rhs) } }



