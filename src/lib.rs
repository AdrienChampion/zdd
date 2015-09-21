#[macro_use]
extern crate hashconsing ;

use std::fmt ;
use std::cmp::Eq ;
use std::io ;
use std::collections::BTreeSet ;

use hashconsing::* ;

use self::ZddTree::* ;

mod cache ;
mod zip ;
mod factory ;

hash_cons!{ pub Zdd<Label> for ZddTree<Label> }

#[derive(Hash)]
pub enum ZddTree<Label> {
  Node(Label, Zdd<Label>, Zdd<Label>),
  One,
  Zero,
}

trait ZddTreeOps<Label: Ord> {
  #[inline(always)]
  fn top(& self) -> Result<Label,bool> ;
  #[inline(always)]
  fn lft(& self) -> Result<Zdd<Label>,bool> ;
  #[inline(always)]
  fn rgt(& self) -> Result<Zdd<Label>,bool> ;
  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> ;
}

impl<Label: Ord + Copy> ZddTreeOps<Label> for Zdd<Label> {
  fn top(& self) -> Result<Label,bool> {
    match self.get() {
      & Zero => Err(false),
      & One => Err(true),
      & Node(ref lbl, _, _) => Ok(* lbl),
    }
  }
  fn lft(& self) -> Result<Zdd<Label>,bool> {
    match self.get() {
      & Zero => Err(false),
      & One => Err(true),
      & Node(_, ref lft, _) => Ok(lft.clone()),
    }
  }
  fn rgt(& self) -> Result<Zdd<Label>,bool> {
    match self.get() {
      & Zero => Err(false),
      & One => Err(true),
      & Node(_, _, ref rgt) => Ok(rgt.clone()),
    }
  }

  fn to_set(& self) -> BTreeSet<BTreeSet<Label>> {
    let mut zdd = self.clone() ;
    let mut set = BTreeSet::new() ;
    let mut path = vec![] ;
    let mut res = BTreeSet::new() ;
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

impl<Label: fmt::Display + Ord + Copy> ZddTree<Label> {

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
        match left.top() {
          Err(false) => try!( write!(fmt, "0") ),
          Err(true) => try!( write!(fmt, "1") ),
          Ok(lbl) => try!( write!(fmt, "{}", lbl) ),
        } ;
        try!( write!(fmt, " [label=\"0\"] ;\n") ) ;
        try!( write!(fmt, "  {} -> ", lbl, ) ) ;
        match right.top() {
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






fn print_and_wtf(
  zdd: & Zdd<& 'static str>, id: usize, name: & 'static str, dir: & 'static str
) {
  println!("----------------------------") ;
  println!("\n> [{:>3}] {}", id, name) ;
  zdd.get().print("".to_string()) ;
  println!("to_set = {{") ;
  for set in zdd.to_set().into_iter() {
    print!("  {{ ") ;
    let mut first = true ;
    for e in set.into_iter() {
      print!("{}{}",
        if first { first = false ; "" } else { ", " },
        e
      )
    } ;
    println!(" }}")
  } ;
  println!("}}") ;
  println!("----------------------------") ;
  zdd.get().to_file(& format!("{}/g_{:0>3}_{}", dir, id, name)) ;
}

pub fn run() {
  use std::process::Command ;
  use factory::* ;
  let dir = "./graphs" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\nCreating factory.") ;
  let mut factory = Factory::<& 'static str>::mk() ;
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
  print_and_wtf(& zdd7, 7, "minus_4_2", dir) ;

  let zdd8 = factory.inter(& zdd3, & zdd4) ;
  print_and_wtf(& zdd8, 8, "inter_3_4", dir) ;

  let zdd9 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "offset_4_a", dir) ;

  let zdd9 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "offset_4_a", dir) ;

  let zdd10 = factory.change(& zdd4, & "c") ;
  print_and_wtf(& zdd10, 10, "change_4_c", dir) ;

  let zdd11 = factory.offset(& zdd10, & "b") ;
  print_and_wtf(& zdd11, 11, "offset_10_b", dir) ;

  let zdd11 = factory.offset(& zdd10, & "b") ;
  print_and_wtf(& zdd11, 11, "offset_10_b", dir) ;
  println!("Done.\n") ;

  ()
}


pub fn run2() {
  use std::process::Command ;
  use factory::* ;
  let dir = "./graphs" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\nCreating factory.") ;
  let mut factory = Factory::<& 'static str>::mk() ;
  let one = factory.one() ;
  let zero = factory.zero() ;
  println!("----------------------------") ;
  one.get().print("".to_string()) ;
  println!("----------------------------") ;

  let zdd1 = factory.node("a", zero.clone(), one.clone()) ;
  print_and_wtf(& zdd1, 1, "a", dir) ;

  let zdd2 = factory.node("b", zero.clone(), one.clone()) ;
  print_and_wtf(& zdd2, 2, "b", dir) ;

  let zdd3 = factory.union(& zdd1, & zdd2) ;
  print_and_wtf(& zdd3, 3, "1_union_2", dir) ;

  let zdd4 = factory.union(& zdd2, & zdd1) ;
  print_and_wtf(& zdd4, 4, "2_union_1", dir) ;

  let zdd5 = factory.offset(& zdd4, & "b") ;
  print_and_wtf(& zdd5, 5, "4_offset_b", dir) ;

  let zdd6 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd6, 6, "4_offset_a", dir) ;

  let zdd7 = factory.onset(& zdd4, & "b") ;
  print_and_wtf(& zdd7, 7, "4_onset_b", dir) ;

  let zdd8 = factory.onset(& zdd4, & "a") ;
  print_and_wtf(& zdd8, 8, "4_onset_a", dir) ;

  let zdd9 = factory.change(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "4_change_a", dir) ;

  let zdd10 = factory.change(& zdd4, & "b") ;
  print_and_wtf(& zdd10, 10, "4_change_b", dir) ;

  let zdd11 = factory.union(& zdd4, & zdd10) ;
  print_and_wtf(& zdd11, 11, "4_union_10", dir) ;

  let zdd12 = factory.inter(& zdd4, & zdd11) ;
  print_and_wtf(& zdd12, 12, "4_inter_11", dir) ;

  let zdd13 = factory.minus(& zdd11, & zdd1) ;
  print_and_wtf(& zdd13, 13, "11_minus_1", dir) ;

  let zdd14 = factory.minus(& zdd11, & zdd2) ;
  print_and_wtf(& zdd14, 14, "11_minus_2", dir) ;

  let zdd15 = factory.minus(& zdd1, & zdd4) ;
  print_and_wtf(& zdd15, 15, "1_minus_4", dir) ;

  println!("count 1: {}", factory.count(& zdd1)) ;
  println!("count 4: {}", factory.count(& zdd4)) ;
  println!("count 11: {}", factory.count(& zdd11)) ;

  ()

}