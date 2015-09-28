#[macro_use]
extern crate hashconsing ;

use std::cmp::Eq ;
use std::collections::BTreeSet ;

use self::ZddTree::* ;

mod print ;
pub use print::ZddPrint ;

mod cache ;

mod zip ;

mod factory ;
pub use factory::Factory ;

/// A hash consed ZDD.
hash_cons!{ pub Zdd<Label> for ZddTree<Label> }

/**
A ZDD is either
* a node with a label and a left and a right hash consed subtree,
* the one terminal, the set containing only the null combination,
* the zero terminal, the empty set.

Now, we actually implement the version with the *0-element edges* that indicate
a path contains the null combination. So we don't have the one terminal.
*/
#[derive(PartialEq, Hash)]
pub enum ZddTree<Label> {
  Node(Label, Zdd<Label>, Zdd<Label>),
  HasOne(Zdd<Label>),
  Zero,
}


// |===| Implementations necessary for hash consing.

impl<Label: Eq> Eq for ZddTree<Label> {}



/// Basic operations on ZDD.
pub trait ZddTreeOps<Label> {

  /// Returns true for all ZDDs containing the empty combination.
  fn has_one(& self) -> bool ;

  /// Returns the top label if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  fn top(& self) -> Result<Label,bool> ;

  /// Turns a ZDD in the corresponding set of sets of labels. Non-destructive.
  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> ;

  /// Turns a ZDD in the corresponding set of sets of labels. Destructive.
  fn into_set(self) -> BTreeSet<BTreeSet<Label>> ;
}

impl<Label: Ord + Copy> ZddTreeOps<Label> for Zdd<Label> {
  fn has_one(& self) -> bool {
    match self.get() { & HasOne(_) => true, _ => false }
  }
  fn top(& self) -> Result<Label,bool> {
    match self.get() {
      & Zero => Err(false),
      // Only one recursive call if ZDD is well-formed.
      & HasOne(ref kid) => match kid.get() {
        & Zero => Err(true),
        & Node(ref lbl, _, _) => Ok(* lbl),
        _ => panic!("[top] ZDD is ill-formed"),
      },
      & Node(ref lbl, _, _) => Ok(* lbl),
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
          rgt_set.insert(* top) ;
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

