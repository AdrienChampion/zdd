//! Printing-related traits and implementations.

use std::fmt ;
use std::io ;

use ::{ Zdd, ZddTree, ZddTreeOps } ;
use ::ZddTree::* ;

/// Printing-related stuff for ZDDs.
pub trait ZddTreePrint<Label> {
  /// Pretty prints a ZDD with a prefix.
  fn print(& self, String) ;

  /// Prints a ZDD as a graphviz graph to a `Write`.
  fn graph_print(& self, & mut io::Write) -> io::Result<()> ;

  /// Print a ZDD as a graphviz graph to a file.
  fn graph_to_file(& self, & str) ;
}

impl<Label: fmt::Display + Ord + Copy> ZddTreePrint<Label> for Zdd<Label> {
  fn print(& self, pref: String) { self.get().print(pref) }
  fn graph_print(& self, fmt: & mut io::Write) -> io::Result<()> {
    self.get().graph_print(fmt)
  }
  fn graph_to_file(& self, file: & str) { self.get().graph_to_file(file) }
}

impl<Label: fmt::Display + Ord + Copy> ZddTreePrint<Label> for ZddTree<Label> {

  fn print(& self, pref: String) {
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

  fn graph_print<>(& self, fmt: & mut io::Write) -> io::Result<()> {
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

  fn graph_to_file(& self, file: & str) {
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