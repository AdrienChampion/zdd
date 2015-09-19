#[macro_use]
extern crate hashconsing ;

use std::fmt ;
use std::cmp::{ PartialEq, Eq } ;
use std::io ;

use hashconsing::* ;

use self::ZddTree::* ;

hash_cons!{ pub Zdd<Label> for ZddTree<Label> }

#[derive(Hash)]
pub enum ZddTree<Label> {
  Node(Label, Zdd<Label>, Zdd<Label>),
  One,
  Zero,
}

impl<Label: fmt::Display> ZddTree<Label> {
  pub fn top(& self) -> Result<& Label,bool> {
    match * self {
      Zero => Err(false),
      One => Err(true),
      Node(ref lbl, _, _) => Ok(lbl),
    }
  }
  pub fn print(& self, pref: String) {
    match * self {
      Node(ref lbl, ref left, ref right) => {
        println!("{}{}", pref, lbl) ;
        left.get().print(format!("{}  ", pref)) ;
        right.get().print(format!("{}  ", pref))
      },
      One => println!("{}1", pref),
      Zero => println!("{}0", pref),
    }
  }
  pub fn graph_print<>(& self, fmt: & mut io::Write) -> io::Result<()> {
    match * self {
      Zero => write!(fmt, "  0;\n"),
      One => write!(fmt, "  1;\n"),
      Node(ref lbl, ref left, ref right) => {
        try!( write!(fmt, "  {} -> ", lbl, ) ) ;
        match left.get().top() {
          Err(false) => try!( write!(fmt, "0") ),
          Err(true) => try!( write!(fmt, "1") ),
          Ok(lbl) => try!( write!(fmt, "{}", lbl) ),
        } ;
        try!( write!(fmt, " [label=\"0\"] ;\n") ) ;
        try!( write!(fmt, "  {} -> ", lbl, ) ) ;
        match right.get().top() {
          Err(false) => try!( write!(fmt, "0") ),
          Err(true) => try!( write!(fmt, "1") ),
          Ok(lbl) => try!( write!(fmt, "{}", lbl) ),
        } ;
        try!( write!(fmt, " [label=\"1\"] ;\n") ) ;
        try!( left.get().graph_print(fmt) ) ;
        right.get().graph_print(fmt)
      },
    }
  }
  pub fn to_file(& self, file: & str) {
    use std::fs::{ OpenOptions } ;
    use std::io::Write ;
    use std::process::Command ;
    match OpenOptions::new().write(true).create(true).open(
      format!("{}.gv", file)
    ) {
      Ok(mut f) => {
        let fmt = & mut f ;
        write!(fmt, "digraph {{\n").unwrap() ;
        self.graph_print(fmt).unwrap() ;
        write!(fmt, "}}\n").unwrap() ;
        fmt.flush().unwrap() ;
        ()
      },
      Err(e) => panic!("{}", e),
    }
    let _ = Command::new("dot").arg("-Tpdf").arg("-o").arg(
      format!("{}.pdf", file)
    ).arg(
      format!("{}.gv", file)
    ).output().unwrap() ;
    ()
  }
}

impl<Label: Eq> PartialEq for ZddTree<Label> {
  fn eq(& self, rhs: & Self) -> bool { self == rhs }
}
impl<Label: Eq> Eq for ZddTree<Label> {}


// // |===| Zipper stuff.

use ::ZddZipperStep::* ;
use ::ZddZipRes::* ;

enum ZddZipperStep<Label> {
  Lft(Label, Zdd<Label>),
  Rgt(Label, Zdd<Label>),
}

type ZddPath<Label> = Vec<ZddZipperStep<Label>> ;

type ZddZipper<Label> = (Zdd<Label>, ZddPath<Label>) ;
fn new_zipper<Label>(zdd: & Zdd<Label>) -> ZddZipper<Label> {
  (zdd.clone(), Vec::new())
}

enum ZddZipRes<Label> {
  Done(Zdd<Label>), Mada(ZddZipper<Label>)
}



pub struct ZddFactory<Label: Hash> {
  consign: HashConsign<ZddTree<Label>>,
  one: Zdd<Label>,
  zero: Zdd<Label>,
}

impl<Label: Hash> ZddFactory<Label> {
  /// Creates an empty ZDD factory.
  pub fn mk() -> Self {
    let mut consign = HashConsign::empty() ;
    let one = consign.mk(One) ;
    let zero = consign.mk(Zero) ;
    ZddFactory {
      consign: consign, one: one, zero: zero
    }
  }

  /// Creates a new node.
  pub fn node(
    & mut self, lbl: Label, left: Zdd<Label>, right: Zdd<Label>
  ) -> Zdd<Label> {
    if right == self.zero() { left } else {
      self.consign.mk(Node(lbl, left, right))
    }
  }

  /// The `1` leaf.
  pub fn one(& self) -> Zdd<Label> { self.one.clone() }

  /// The `0` node.
  pub fn zero(& self) -> Zdd<Label> { self.zero.clone() }

  /// *Zipper function.* Goes up as long as it's in the right branch.
  fn up_of_path(
    & mut self, zip: ZddZipper<Label>
  ) -> ZddZipRes<Label> {
    let (mut zdd, mut path) = zip ;
    loop {
      match path.pop() {
        None => return Done(zdd),
        Some(Lft(lbl, rgt)) => {
          path.push( Rgt(lbl,zdd) ) ;
          return Mada((rgt, path))
        },
        Some(Rgt(lbl, lft)) => {
          zdd = self.node(lbl, lft, zdd) ;
        }
      }
    }
  }
}

pub trait ZddOps<Label> {
  // |===| Operations on ZDDs.

  /// The label at the root of the ZDD.
  fn top<'a>(& self, & 'a Zdd<Label>) -> Option<& 'a Label> ;
  /// Selects the subset of combinations not containing a label.
  fn offset(& mut self, & Zdd<Label>, & Label) -> Zdd<Label> ;
  /// Selects the subset of combinations containing a label, removing it from
  /// them.
  fn onset(& mut self, & Zdd<Label>, & Label) -> Zdd<Label> ;
  /// Inverts a label on each combination.
  fn change(& mut self, & Zdd<Label>, & Label) -> Zdd<Label> ;
  /// The union of two ZDDs.
  fn union(& mut self, & Zdd<Label>, & Zdd<Label>) -> Zdd<Label> ;
  /// The intersection of two ZDDs.
  fn inter(& mut self, & Zdd<Label>, & Zdd<Label>) -> Zdd<Label> ;
  /// The difference of two ZDDs.
  fn minus(& mut self, & Zdd<Label>, & Zdd<Label>) -> Zdd<Label> ;
  /// The number of combinations in a ZDD.
  fn count(& self, & Zdd<Label>) -> usize ;
}


// |===| Zipper macros.

macro_rules! return_if_done {
  ( $slf:ident ^ $zdd:expr, $path:expr ) => (
    match $slf.up_of_path(($zdd, $path)) {
      Done(zdd) => return zdd,
      Mada(zip) => zip,
    }
  ) ;
}

macro_rules! zdd_fold_pattern {
  (
    Node($top:ident, $left:ident, $right:ident) => {
      if equal $equal_block:block,
      if below $below_block:block,
      if above $above_block:block,
    }
  ) => (
    & Node(ref $top, ref $left, ref $right) =>
      if $lbl == $top $equal_block else {
        if $lbl < $top $below_block else $above_block
      }
  ) ;
  (One => $b:block) => (
    & One => $b
  ) ;
  (Zero => $b:block) => (
    & Zero => $b
  ) ;
  (_ => $b:block) => (
    _ => $b
  ) ;
}

macro_rules! zdd_fold {
  // (zipper $zip:ident, label $lbl:ident {
  //   $( $pat:pat => $b:block ),+
  // }) => (
  //   loop {
  //     $zip = match $zip.0.get() {
  //       $( zdd_fold_pattern!( $pat => $b:block ) ),+
  //     }
  //   }
  // ) ;
  (zipper $zip:ident, label $lbl:ident {
    Node($top:ident, $left:ident, $right:ident) => {
      if equal $equal_block:block,
      if below $below_block:block,
      if above $above_block:block,
    },
    One => $one_block:block,
    Zero => $zero_block:block,
  }) => (
    loop {
      $zip = match $zip.0.get() {
        & Node(ref $top, ref $left, ref $right) =>
          if $lbl == $top $equal_block else {
            if $lbl < $top $below_block else $above_block
          },
        & One => $one_block,
        & Zero => $zero_block,
      }
    }
  ) ;
  (zipper $zip:ident, label $lbl:ident {
    Node($top:ident, $left:ident, $right:ident) => {
      if equal $equal_block:block,
      if below $below_block:block,
      if above $above_block:block,
    },
    _ => $leaf_block:block,
  }) => (
    loop {
      $zip = match $zip.0.get() {
        & Node(ref $top, ref $left, ref $right) =>
          if $lbl == $top $equal_block else {
            if $lbl < $top $below_block else $above_block
          },
        _ => $leaf_block,
      }
    }
  ) ;
}

impl<Label: Hash + Ord + Eq + Copy + fmt::Display> ZddOps<Label> for ZddFactory<Label> {
  fn top<'a>(& self, zdd: & 'a Zdd<Label>) -> Option<& 'a Label> {
    match zdd.get() {
      & Node(ref lbl, _, _) => Some(& lbl), _ => None,
    }
  }

  // TODO: make this cached.
  fn offset(& mut self, zdd: & Zdd<Label>, lbl: & Label) -> Zdd<Label> {
    // Preparing zipper.
    let mut zip = new_zipper(zdd) ;
    zdd_fold! {
      zipper zip, label lbl {
        Node(top, left, right) => {
          if equal {
            // Only keep left part and go up.
            return_if_done!(self ^ left.clone(), zip.1)
          }, if below {
            // We're below the label, going up.
            return_if_done!(self ^ zip.0.clone(), zip.1)
          }, if above {
            // We're above the label, going left first and updating path.
            zip.1.push( Lft(* top, right.clone()) ) ;
            (left.clone(), zip.1)
          },
        },
        _ => {
          let zero = self.zero() ;
          return_if_done!(self ^ zero, zip.1)
        },
      }
    }
  }

  // TODO: make this cached.
  fn onset(& mut self, zdd: & Zdd<Label>, lbl: & Label) -> Zdd<Label> {
    // Preparing zipper.
    let mut zip = new_zipper(zdd) ;
    zdd_fold! {
      zipper zip, label lbl {
        Node(top, left, right) => {
          if equal {
            // Only keep right part and go up.
            return_if_done!(self ^ right.clone(), zip.1)
          }, if below {
            // We're below the label, it's not there.
            let zero = self.zero() ;
            return_if_done!(self ^ zero, zip.1)
          }, if above {
            // We're above the label, going left first and updating path.
            zip.1.push( Lft(* top, right.clone()) ) ;
            (left.clone(), zip.1)
          },
        },
        _ => {
          let zero = self.zero() ;
          return_if_done!(self ^ zero, zip.1)
        },
      }
    }
  }

  // TODO: make this cached.
  fn change(& mut self, zdd: & Zdd<Label>, lbl: & Label) -> Zdd<Label> {
    // Preparing zipper.
    let mut zip = new_zipper(zdd) ;
    zdd_fold! {
      zipper zip, label lbl {
        Node(top, left, right) => {
          if equal {
            // Change and go up.
            let zdd = self.node(* top, right.clone(), left.clone()) ;
            return_if_done!(self ^ zdd, zip.1)
          }, if below {
            // We're below the label, it's not there.
            return_if_done!(self ^ zip.0.clone(), zip.1)
          }, if above {
            // We're above the label, going left first and updating path.
            zip.1.push( Lft(* top, right.clone()) ) ;
            (left.clone(), zip.1)
          },
        },
        One => {
          let zero = self.zero() ;
          let zdd = self.node(* lbl, zero, zip.0.clone()) ;
          return_if_done!(self ^ zdd, zip.1)
        },
        Zero => {
          return_if_done!(self ^ zip.0.clone(), zip.1)
        },
      }
    }
  }

  // TODO: make this non recursive and cached.
  fn union(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> Zdd<Label> {
    match lhs.get() {
      & Zero => rhs.clone(),
      & One => match rhs.get() {
        & Node(ref r_top, ref r_left, ref r_right) => {
          let left = self.union(r_left, lhs) ;
          self.node(* r_top, left, r_right.clone())
        },
        _ => lhs.clone(),
      },
      & Node(ref l_top, ref l_left, ref l_right) => match rhs.get() {
        & Zero => lhs.clone(),
        & One => {
          let left = self.union(l_left, rhs) ;
          self.node(* l_top, left, l_right.clone())
        }
        & Node(ref r_top, ref r_left, ref r_right) => if l_top < r_top {
          let left = self.union(l_left, rhs) ;
          self.node(* l_top, left, l_right.clone())
        } else {
          if l_top > r_top {
            let left = self.union(lhs, r_left) ;
            self.node(* r_top, left, r_right.clone())
          } else {
            let left = self.union(l_left, r_left) ;
            let right = self.union(l_right, r_right) ;
            self.node(* l_top, left, right)
          }
        },
      }
    }
  }

  // TODO: make this non recursive and cached.
  fn inter(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> Zdd<Label> {
    match lhs.get() {
      & Zero => self.zero(),
      & One => match rhs.get() {
        & Node(_, ref r_left, _) => self.inter(lhs, r_left),
        _ => rhs.clone(),
      },
      & Node(ref l_top, ref l_left, ref l_right) => match rhs.get() {
        & Zero => self.zero(),
        & One => self.inter(l_left, rhs),
        & Node(ref r_top, ref r_left, ref r_right) => if l_top < r_top {
          self.inter(l_left, rhs)
        } else {
          if l_top > r_top {
            self.inter(lhs, r_left)
          } else {
            let left = self.inter(l_left, r_left) ;
            let right = self.inter(l_right, r_right) ;
            self.node(* l_top, left, right)
          }
        },
      }
    }
  }

  // TODO: make this non recursive and cached.
  fn minus(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> Zdd<Label> {
    if lhs == rhs { self.zero() } else { match lhs.get() {
      & Zero => self.zero(),
      & One => match rhs.get() {
        & Zero => self.one(),
        & Node(_, ref r_left, _) => self.minus(lhs, r_left),
        _ => unreachable!(),
      },
      & Node(ref l_top, ref l_left, ref l_right) => match rhs.get() {
        & Zero => lhs.clone(),
        & One => self.minus(l_left, rhs),
        & Node(ref r_top, ref r_left, ref r_right) => if l_top < r_top {
          let left = self.minus(l_left, rhs) ;
          self.node(* l_top, left, l_right.clone())
        } else {
          if l_top > r_top { self.minus(lhs, r_left) } else {
            let left = self.minus(l_left, r_left) ;
            let right = self.minus(l_right, r_right) ;
            self.node(* l_top, left, right)
          }
        },
      }
    }}
  }

  fn count(& self, zdd: & Zdd<Label>) -> usize {
    match zdd.get() {
      & Zero => 0,
      & One => 1,
      & Node(_, ref left, ref right) => {
        let left = self.count(left) ;
        let right = self.count(right) ;
        left + right
      }
    }
  }
}

fn print_and_wtf(
  zdd: & Zdd<& 'static str>, id: usize, name: & 'static str, dir: & 'static str
) {
  println!("----------------------------") ;
  println!("\n> [{:>3}] {}", id, name) ;
  zdd.get().print("".to_string()) ;
  println!("----------------------------") ;
  zdd.get().to_file(& format!("{}/g_{:0>3}_{}", dir, id, name)) ;
}

pub fn run() {
  use std::process::Command ;
  let dir = "./graphs" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\nCreating factory.") ;
  let mut factory = ZddFactory::<& 'static str>::mk() ;
  let one = factory.one() ;
  println!("----------------------------") ;
  one.get().print("".to_string()) ;
  println!("----------------------------") ;

  let zdd1 = factory.change(& one, & "a") ;
  print_and_wtf(& zdd1, 1, "a", dir) ;

  let zdd2 = factory.change(& one, & "b") ;
  print_and_wtf(& zdd2, 2, "b", dir) ;

  let zdd3 = factory.union(& zdd1, & zdd2) ;
  print_and_wtf(& zdd3, 3, "union_1_2", dir) ;

  let zdd4 = factory.union(& one, & zdd3) ;
  print_and_wtf(& zdd4, 4, "union_one_3", dir) ;

  let zdd5 = factory.onset(& zdd4, & "b") ;
  print_and_wtf(& zdd5, 5, "onset_4_b", dir) ;

  let zdd6 = factory.inter(& zdd2, & zdd4) ;
  print_and_wtf(& zdd6, 6, "inter_2_4", dir) ;

  let zdd7 = factory.minus(& zdd4, & zdd2) ;
  print_and_wtf(& zdd7, 7, "minus_2_4", dir) ;

  let zdd8 = factory.inter(& zdd3, & zdd4) ;
  print_and_wtf(& zdd8, 8, "inter_3_4", dir) ;

  let zdd9 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "offset_4_a", dir) ;

  let zdd10 = factory.change(& zdd4, & "c") ;
  print_and_wtf(& zdd10, 10, "change_4_c", dir) ;

  let zdd11 = factory.offset(& zdd10, & "b") ;
  print_and_wtf(& zdd11, 11, "offset_10_b", dir) ;

  println!("Done.\n") ;

  ()
}