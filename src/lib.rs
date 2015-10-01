#![doc = "
A ZDD library, based on the paper by [Shin-Ichi Minato][zdd paper].

# Todo

* constructor for factory to set the size of the caches and the consign
* subset (Cached? Probably)

[zdd paper]: http://link.springer.com/article/10.1007%2Fs100090100038 (Zero-suppressed BDDs and their applications)
"]

#[macro_use]
extern crate hashconsing ;

use std::cmp::Eq ;
use std::collections::BTreeSet ;

use self::ZddTree::* ;

mod print ;
pub use print::ZddPrint ;

#[macro_use]
mod zip ;

mod factory ;
pub use factory::FactoryBuilder ;
pub use factory::Factory ;

/// A hash consed ZDD.
pub type Zdd<Label> = ::std::rc::Rc<
  hashconsing::HashConsed<ZddTree<Label>>
> ;

/// A ZDD factory can create nodes.
trait FactoryTrait<Label> {
  /// Creates a hash consed ZDD from a label and two kids.
  fn mk_node(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> ;
}

/** Actual ZDD enum type.

Usually a ZDD is either:

* a **node** with a label and a left and a right hash consed subtree,
* the **one** terminal, the set containing only the null combination,
* the **zero** terminal, the empty set.

However we use *0-element edges* that indicate a path contains the null
combination. So there's no **one** terminal.
*/
#[derive(PartialEq, Hash)]
pub enum ZddTree<Label> {
  /// A node with a label and two kids.
  Node(Label, Zdd<Label>, Zdd<Label>),
  /// Indicates the underlying contains the empty combination.
  HasOne(Zdd<Label>),
  /// The empty set.
  Zero,
}


// |===| Implementations necessary for hash consing.

impl<Label: Eq> Eq for ZddTree<Label> {}



/// Basic operations on ZDD.
pub trait ZddTreeOps<Label> {

  /// Returns true iff the ZDD is *zero*.
  #[inline(always)]
  fn is_zero(& self) -> bool ;
  /// Returns true iff the ZDD is *one*.
  #[inline(always)]
  fn is_one(& self) -> bool ;
  /// Returns true for all ZDDs containing the empty combination.
  #[inline(always)]
  fn has_one(& self) -> bool ;

  /// Returns the top label if the ZDD is a node, an error of `true` if the
  /// ZDD is *one* and `false` if it is *zero*.
  #[inline(always)]
  fn top(& self) -> Result<Label,bool> ;

  /// Turns a ZDD in the corresponding set of sets of labels. Non-destructive.
  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> ;

  /// Turns a ZDD in the corresponding set of sets of labels. Destructive.
  fn into_set(self) -> BTreeSet<BTreeSet<Label>> ;
}

impl<Label: Ord + Clone> ZddTreeOps<Label> for Zdd<Label> {
  fn is_zero(& self) -> bool { self.top() == Err(false) }
  fn is_one(& self) -> bool { self.top() == Err(true) }
  fn has_one(& self) -> bool {
    match self.get() { & HasOne(_) => true, _ => false }
  }
  fn top(& self) -> Result<Label,bool> {
    match self.get() {
      & Zero => Err(false),
      // Only one recursive call if ZDD is well-formed.
      & HasOne(ref kid) => match kid.get() {
        & Zero => Err(true),
        & Node(ref lbl, _, _) => Ok(lbl.clone()),
        _ => panic!("[top] ZDD is ill-formed"),
      },
      & Node(ref lbl, _, _) => Ok(lbl.clone()),
    }
  }

  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> {
    match self.top() {
      Err(false) => BTreeSet::new(),
      Err(true) => {
        let mut sset = BTreeSet::new() ;
        sset.insert(BTreeSet::new()) ;
        sset
      },
      _ => self.clone().into_set()
    }
  }

  fn into_set(self) -> BTreeSet<BTreeSet<Label>> {
    let mut set = BTreeSet::new() ;
    let mut path = vec![] ;
    let mut res = BTreeSet::new() ;
    let mut zdd = self ;
    loop {
      zdd = match zdd.get() {
        & Node(ref top, ref lft, ref rgt) => {
          let mut rgt_set = set.clone() ;
          rgt_set.insert(top.clone()) ;
          path.push((rgt.clone(), rgt_set)) ;
          lft.clone()
        },
        & HasOne(ref kid) => {
          res.insert(set.clone()) ;
          kid.clone()
        },
        & Zero => {
          if let Some((nu_zdd, nu_set)) = path.pop() {
            set = nu_set ;
            nu_zdd
          } else {
            return res
          }
        },
      }
    }
  }
}


