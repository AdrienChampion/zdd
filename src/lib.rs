#[macro_use]
extern crate hashconsing ;

use std::cmp::Eq ;
use std::collections::BTreeSet ;

use self::ZddTree::* ;

mod print ;
pub use print::ZddTreePrint ;

mod cache ;

mod zip ;

mod factory ;
pub use factory::Factory ;


/// A hash consed ZDD.
hash_cons!{ pub Zdd<Label> for ZddTree<Label> }


/// A ZDD is either
/// * a node with a label and a left and a right hash consed subtree,
/// * the one terminal, the set containing only the null combination,
/// * the zero terminal, the empty set.
#[derive(Hash)]
pub enum ZddTree<Label> {
  Node(Label, Zdd<Label>, Zdd<Label>),
  One,
  Zero,
}


// |===| Implementations necessary for hash consing.

impl<Label: Eq> PartialEq for ZddTree<Label> {
  fn eq(& self, rhs: & Self) -> bool { self == rhs }
}

impl<Label: Eq> Eq for ZddTree<Label> {}



/// Basic operations on ZDD.
pub trait ZddTreeOps<Label> {
  /// Returns the top label if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  fn top(& self) -> Result<Label,bool> ;

  /// Returns the left subtree if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  fn lft(& self) -> Result<Zdd<Label>,bool> ;

  /// Returns the right subtree if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  fn rgt(& self) -> Result<Zdd<Label>,bool> ;

  /// Turns a ZDD in the corresponding set of sets of labels.
  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> ;
}

impl<Label: Ord + Copy> ZddTreeOps<Label> for Zdd<Label> {
  fn top(& self) -> Result<Label,bool> { self.get().top() }
  fn lft(& self) -> Result<Zdd<Label>,bool> { self.get().lft() }
  fn rgt(& self) -> Result<Zdd<Label>,bool> { self.get().rgt() }
  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> { self.get().to_set() }
}

impl<Label: Ord + Copy> ZddTreeOps<Label> for ZddTree<Label> {
  fn top(& self) -> Result<Label,bool> {
    match self {
      & Zero => Err(false),
      & One => Err(true),
      & Node(ref lbl, _, _) => Ok(* lbl),
    }
  }
  fn lft(& self) -> Result<Zdd<Label>,bool> {
    match self {
      & Zero => Err(false),
      & One => Err(true),
      & Node(_, ref lft, _) => Ok(lft.clone()),
    }
  }
  fn rgt(& self) -> Result<Zdd<Label>,bool> {
    match self {
      & Zero => Err(false),
      & One => Err(true),
      & Node(_, _, ref rgt) => Ok(rgt.clone()),
    }
  }

  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> {
    let mut set = BTreeSet::new() ;
    let mut path = vec![] ;
    let mut res = BTreeSet::new() ;
    let mut zdd = match self {
      & Node(ref top, ref lft, ref rgt) => {
        let mut rgt_set = set.clone() ;
        rgt_set.insert(* top) ;
        path.push((rgt.clone(), rgt_set)) ;
        lft.clone()
      },
      & Zero => {
        return res
      },
      & One => {
        res.insert(set) ;
        return res
      },
    } ;
    loop {
      zdd = match zdd.get() {
        & Node(ref top, ref lft, ref rgt) => {
          let mut rgt_set = set.clone() ;
          rgt_set.insert(* top) ;
          path.push((rgt.clone(), rgt_set)) ;
          lft.clone()
        },
        & One => {
          res.insert(set) ;
          if let Some((nu_zdd, nu_set)) = path.pop() {
            set = nu_set ;
            nu_zdd
          } else {
            return res
          }
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

