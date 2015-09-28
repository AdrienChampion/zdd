//! Printing-related traits and implementations.

use std::fmt ;
use std::io ;

use ::{ Zdd, ZddTreeOps } ;
use ::ZddTree::* ;

/// Printing-related stuff for ZDDs.
pub trait ZddPrint<Label> {
  /// Pretty prints a ZDD with a prefix.
  fn print(& self, String) ;
  /// Print a ZDD as a graphviz graph to a file.
  fn graph_to_file(& self, & str) -> io::Result<()> ;
}

/// Prints a ZDD as a graphviz graph to a `Write`.
fn graph_print<Label: fmt::Display + Ord + Copy>(
  fmt: & mut io::Write,
  zdd: & Zdd<Label>,
  root: & 'static str,
  zero: & 'static str
) -> io::Result<()> {
  use std::collections::HashSet ;
  let mut mem = HashSet::new() ;

  let mut to_do = vec![ (root.to_string(), "", false, zdd.clone()) ] ;
  let has_one_style= " arrowtail=odot  dir=both" ;

  loop {
    if let Some((parent, edge_lbl, has_one, zdd)) = to_do.pop() {
      let style = if has_one { has_one_style } else { "" } ;
      match zdd.get() {
        & Zero => try!(
          write!(
            fmt, "  {} -> {} [{}{}] ;\n", parent, zero, edge_lbl, style
          )
        ),
        & HasOne(ref kid) => to_do.push(
          (parent, edge_lbl, true, kid.clone())
        ),
        & Node(ref lbl, ref left, ref right) => {
          let name = zdd.hkey().to_string() ;
          try!(write!(fmt, "  {} [label=\"{}\"] ; \n", name, lbl)) ;
          try!(
            write!(
              fmt, "  {} -> {} [{}{}] ;\n",
              parent, name, edge_lbl, style
            )
          ) ;
          if ! mem.contains(& zdd) {
            mem.insert(zdd.clone()) ;
            to_do.push(
              (name.clone(), "arrowhead=empty", false, right.clone())
            ) ;
            to_do.push(
              (name, "", false, left.clone())
            )
          }
        },
      }
    } else {
      return Ok(())
    }
  }
}

impl<Label: fmt::Display + Ord + Copy> ZddPrint<Label> for Zdd<Label> {

  fn print(& self, pref: String) {
    println!("{}{{", pref) ;
    for set in self.to_set().into_iter() {
      print!("{}  {{ ", pref) ;
      let mut first = true ;
      for e in set.into_iter() {
        print!(
          "{}{}", if first { first = false ; "" } else { ", " }, e
        )
      } ;
      println!(" }}") ;
    } ;
    println!("{}}}", pref)
  }

  fn graph_to_file(& self, file: & str) -> io::Result<()> {
    use std::fs::{ OpenOptions } ;
    use std::io::Write ;
    use std::process::Command ;
    let root = "root_of_the_zdd" ;
    let zero = "zero_of_the_zdd" ;
    match OpenOptions::new().write(true).create(true).truncate(true).open(
      format!("{}.gv", file)
    ) {
      Ok(mut f) => {
        let fmt = & mut f ;
        try!( write!(fmt, "digraph {{\n\n") ) ;
        try!( write!(fmt, "  graph [bgcolor=black margin=0.0] ;\n") ) ;
        try!( write!(fmt, "  node [style=invisible] ; {} ;\n\n", root) ) ;
        try!( write!(fmt,
          "  node [\
              style=filled \
              fillcolor=black \
              fontcolor=\"#1e90ff\" \
              color=\"#666666\"\
          ] ;\n"
        ) ) ;
        try!( write!(fmt,
          "  edge [color=\"#1e90ff\" fontcolor=\"#222222\"] ;\n\n"
        ) ) ;
        try!( write!(fmt,
          "  node [shape=doublecircle] ; {} [label=\"{{}}\"] ;\n", zero
        ) ) ;
        try!( write!(fmt, "  node [shape=circle] ;\n") ) ;
        try!( graph_print(fmt, self, root, zero) ) ;
        try!( write!(fmt, "}}\n") ) ;
        try!( fmt.flush() ) ;
      },
      Err(e) => panic!("{}", e),
    }
    let _ = Command::new("dot").arg("-Tpdf").arg("-o").arg(
      format!("{}.pdf", file)
    ).arg(
      format!("{}.gv", file)
    ).output().unwrap() ;
    Ok(())
  }
}