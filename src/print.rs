// See the LICENSE files at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Printing-related traits and implementations.

use std::fmt;
use std::hash::Hash;
use std::io::{Result as IoRes, Write};

use crate::{Zdd, ZddTree, ZddTreeOps};

/// Printing and logging to graphviz.
pub trait ZddPrint<Label> {
    /// Pretty prints a ZDD with a prefix.
    fn print(&self, pref: String);
    /// Logs a ZDD as graphviz to a `Write`.
    fn write_as_gv(&self, w: &mut impl Write) -> IoRes<()>;
}

/// Prints a ZDD as a graphviz graph to a `Write`.
fn graph_print<Label: Hash + Clone + fmt::Display + Ord>(
    wrt: &mut impl Write,
    zdd: &Zdd<Label>,
    root: &'static str,
    zero: &'static str,
) -> IoRes<()> {
    use std::collections::HashSet;
    let mut mem = HashSet::new();

    let mut to_do = vec![(root.to_string(), "", false, zdd.clone())];
    let has_one_style = " arrowtail=odot  dir=both";

    loop {
        if let Some((parent, edge_lbl, has_one, zdd)) = to_do.pop() {
            let style = if has_one { has_one_style } else { "" };
            match zdd.get() {
                &ZddTree::Zero => {
                    write!(wrt, "  {} -> {} [{}{}] ;\n", parent, zero, edge_lbl, style)?
                }
                &ZddTree::HasOne(ref kid) => to_do.push((parent, edge_lbl, true, kid.clone())),
                &ZddTree::Node(ref lbl, ref left, ref right) => {
                    let name = zdd.uid().to_string();
                    write!(wrt, "  {} [label=\"{}\"] ; \n", name, lbl)?;
                    write!(wrt, "  {} -> {} [{}{}] ;\n", parent, name, edge_lbl, style)?;
                    if !mem.contains(&zdd) {
                        mem.insert(zdd.clone());
                        to_do.push((name.clone(), "arrowhead=empty", false, right.clone()));
                        to_do.push((name, "", false, left.clone()))
                    }
                }
            }
        } else {
            return Ok(());
        }
    }
}

impl<Label: fmt::Display + Ord + Clone + Hash> ZddPrint<Label> for Zdd<Label> {
    fn print(&self, pref: String) {
        println!("{}{{", pref);
        for vec in self.iter() {
            print!("{}  {{ ", pref);
            let mut first = true;
            for e in vec.into_iter() {
                print!(
                    "{}{}",
                    if first {
                        first = false;
                        ""
                    } else {
                        ", "
                    },
                    e
                )
            }
            println!(" }}");
        }
        println!("{}}}", pref)
    }

    fn write_as_gv(&self, wrt: &mut impl Write) -> IoRes<()> {
        let root = "root_of_the_zdd";
        let zero = "zero_of_the_zdd";
        write!(wrt, "digraph {{\n\n")?;
        write!(wrt, "    graph [bgcolor=black margin=0.0];\n")?;
        write!(wrt, "    node [style=invisible]; {};\n\n", root)?;
        write!(
            wrt,
            "    \
            node [ style=filled  fillcolor=black  fontcolor=\"#1e90ff\"  color=\"#666666\" ] ;\n"
        )?;
        write!(
            wrt,
            "  edge [color=\"#1e90ff\" fontcolor=\"#222222\"] ;\n\n"
        )?;
        write!(
            wrt,
            "  node [shape=doublecircle] ; {} [label=\"{{}}\"] ;\n",
            zero
        )?;
        write!(wrt, "  node [shape=circle] ;\n")?;
        graph_print(wrt, self, root, zero)?;
        write!(wrt, "}}\n")
    }
}
