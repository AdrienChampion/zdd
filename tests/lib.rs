extern crate zdd ;
extern crate rand ;

use std::collections::BTreeSet ;

use zdd::ZddTreeOps ;
use zdd::ZddPrint ;

use rand::{ StdRng, Rand, random, Closed01 } ;

type Lbl = u8 ;
type Set = BTreeSet<Lbl> ;
type SSet = BTreeSet<Set> ;
type Zdd = zdd::Zdd<Lbl> ;
type Factory = zdd::Factory<Lbl> ;

fn sset_print(sset: & SSet, pref: String) {
  println!("{}{{", pref) ;
  for set in sset.iter() {
    print!("{}  {{ ", pref) ;
    let mut first = true ;
    for e in set.iter() {
      print!(
        "{}{}", if first { first = false ; "" } else { ", " }, e
      )
    } ;
    println!(" }}") ;
  }
  println!("{}}}", pref)
}

fn set_zero() -> SSet {
  SSet::new()
}

fn set_one() -> SSet {
  let mut sset = SSet::new() ;
  sset.insert(Set::new()) ;
  sset
}

fn set_offset(sset: & SSet, lbl: & Lbl) -> SSet {
  let mut nu_sset = SSet::new() ;
  for set in sset.iter() {
    let mut nu_set = Set::new() ;
    let mut saw_lbl = false ;
    for e in set.iter() {
      if lbl == e { saw_lbl = true } else {
        nu_set.insert(* e) ; ()
      }
    } ;
    if !saw_lbl {
      nu_sset.insert(nu_set) ; ()
    }
  } ;
  nu_sset
}

fn set_onset(sset: & SSet, lbl: & Lbl) -> SSet {
  let mut nu_sset = SSet::new() ;
  for set in sset.iter() {
    let mut nu_set = Set::new() ;
    let mut saw_lbl = false ;
    for e in set.iter() {
      if lbl == e { saw_lbl = true } else {
        nu_set.insert(* e) ; ()
      }
    } ;
    if saw_lbl {
      nu_sset.insert(nu_set) ; ()
    }
  } ;
  nu_sset
}

fn set_change(sset: & SSet, lbl: & Lbl) -> SSet {
  let mut nu_sset = SSet::new() ;
  for set in sset.iter() {
    let mut nu_set = Set::new() ;
    let mut saw_lbl = false ;
    for e in set.iter() {
      if lbl == e { saw_lbl = true } else {
        nu_set.insert(* e) ; ()
      }
    } ;
    if ! saw_lbl { nu_set.insert(* lbl) ; () } ;
    nu_sset.insert(nu_set) ; ()
  } ;
  nu_sset
}

fn set_union(lhs: & SSet, rhs: & SSet) -> SSet {
  let mut nu_sset = lhs.clone() ;
  for set in rhs.iter() {
    nu_sset.insert(set.clone()) ; ()
  } ;
  nu_sset
}


fn set_inter(lhs: & SSet, rhs: & SSet) -> SSet {
  let mut nu_sset = SSet::new() ;
  for set in lhs.iter() {
    if rhs.contains(set) { nu_sset.insert(set.clone()) ; () }
  } ;
  nu_sset
}

fn set_minus(lhs: & SSet, rhs: & SSet) -> SSet {
  let mut nu_sset = lhs.clone() ;
  for set in rhs.iter() {
    nu_sset.remove(set) ; ()
  } ;
  nu_sset
}

fn offset(
  f: & mut Factory, zdd: & Zdd, sset: & SSet, lbl: & Lbl
) -> (Zdd, SSet) {
  let name = "offset" ;
  // if * sset != zdd.to_set() {
  //   panic!("{} | inconsistent inputs", name)
  // }
  let res_zdd = f.offset(zdd, lbl) ;
  let res_sset = set_offset(sset, lbl) ;
  if res_zdd.to_set() != res_sset {
    println!("{} {}", name, lbl) ;
    println!("zdd:") ;
    zdd.print("| ".to_string()) ;
    println!("set:") ;
    sset_print(& sset, "| ".to_string()) ;
    println!("res_zdd:") ;
    res_zdd.print("| ".to_string()) ;
    println!("res_set:") ;
    sset_print(& res_sset, "| ".to_string()) ;
    panic!("{} | result mismatch", name)
  } ;
  (res_zdd, res_sset)
}

fn onset(
  f: & mut Factory, zdd: & Zdd, sset: & SSet, lbl: & Lbl
) -> (Zdd, SSet) {
  let name = "onset" ;
  // if * sset != zdd.to_set() {
  //   panic!("{} | inconsistent inputs", name)
  // }
  let res_zdd = f.onset(zdd, lbl) ;
  let res_sset = set_onset(sset, lbl) ;
  if res_sset != res_zdd.to_set() {
    println!("{} {}", name, lbl) ;
    println!("zdd:") ;
    zdd.print("| ".to_string()) ;
    println!("set:") ;
    sset_print(& sset, "| ".to_string()) ;
    println!("res_zdd:") ;
    res_zdd.print("| ".to_string()) ;
    println!("res_set:") ;
    sset_print(& res_sset, "| ".to_string()) ;
    panic!("{} | result mismatch", name)
  } ;
  (res_zdd, res_sset)
}

fn change(
  f: & mut Factory, zdd: & Zdd, sset: & SSet, lbl: & Lbl
) -> (Zdd, SSet) {
  let name = "change" ;
  // if * sset != zdd.to_set() {
  //   panic!("{} | inconsistent inputs", name)
  // }
  let res_zdd = f.change(zdd, lbl) ;
  let res_sset = set_change(sset, lbl) ;
  if res_sset != res_zdd.to_set() {
    println!("{} {}", name, lbl) ;
    println!("zdd:") ;
    zdd.print("| ".to_string()) ;
    println!("set:") ;
    sset_print(& sset, "| ".to_string()) ;
    println!("res_zdd:") ;
    res_zdd.print("| ".to_string()) ;
    println!("res_set:") ;
    sset_print(& res_sset, "| ".to_string()) ;
    panic!("{} | result mismatch", name)
  } ;
  (res_zdd, res_sset)
}

fn union(
  f: & mut Factory,
  lhs_zdd: & Zdd, rhs_zdd: & Zdd,
  lhs_sset: & SSet, rhs_sset: & SSet,
) -> (Zdd, SSet) {
  let name = "union" ;
  // if * lhs_sset != lhs_zdd.to_set() || * rhs_sset != rhs_zdd.to_set() {
  //   panic!("{} | inconsistent inputs", name)
  // }
  let res_zdd = f.union(lhs_zdd, rhs_zdd) ;
  let res_sset = set_union(lhs_sset, rhs_sset) ;
  if res_sset != res_zdd.to_set() {
    println!("{}", name) ;
    println!("lhs_zdd:") ;
    lhs_zdd.print("| ".to_string()) ;
    println!("rhs_zdd:") ;
    rhs_zdd.print("| ".to_string()) ;
    println!("lhs_set:") ;
    sset_print(& lhs_sset, "| ".to_string()) ;
    println!("rhs_set:") ;
    sset_print(& rhs_sset, "| ".to_string()) ;
    println!("res_zdd:") ;
    res_zdd.print("| ".to_string()) ;
    println!("res_set:") ;
    sset_print(& res_sset, "| ".to_string()) ;
    panic!("{} | result mismatch", name)
  } ;
  (res_zdd, res_sset)
}

fn inter(
  f: & mut Factory,
  lhs_zdd: & Zdd, rhs_zdd: & Zdd,
  lhs_sset: & SSet, rhs_sset: & SSet,
) -> (Zdd, SSet) {
  let name = "inter" ;
  // if * lhs_sset != lhs_zdd.to_set() || * rhs_sset != rhs_zdd.to_set() {
  //   panic!("{} | inconsistent inputs", name)
  // }
  let res_zdd = f.inter(lhs_zdd, rhs_zdd) ;
  let res_sset = set_inter(lhs_sset, rhs_sset) ;
  if res_sset != res_zdd.to_set() {
    println!("{}", name) ;
    println!("lhs_zdd:") ;
    lhs_zdd.print("| ".to_string()) ;
    println!("rhs_zdd:") ;
    rhs_zdd.print("| ".to_string()) ;
    println!("lhs_set:") ;
    sset_print(& lhs_sset, "| ".to_string()) ;
    println!("rhs_set:") ;
    sset_print(& rhs_sset, "| ".to_string()) ;
    println!("res_zdd:") ;
    res_zdd.print("| ".to_string()) ;
    println!("res_set:") ;
    sset_print(& res_sset, "| ".to_string()) ;
    panic!("{} | result mismatch", name)
  } ;
  (res_zdd, res_sset)
}

fn minus(
  f: & mut Factory,
  lhs_zdd: & Zdd, rhs_zdd: & Zdd,
  lhs_sset: & SSet, rhs_sset: & SSet,
) -> (Zdd, SSet) {
  let name = "minus" ;
  // if * lhs_sset != lhs_zdd.to_set() || * rhs_sset != rhs_zdd.to_set() {
  //   panic!("{} | inconsistent inputs", name)
  // }
  let res_zdd = f.minus(lhs_zdd, rhs_zdd) ;
  let res_sset = set_minus(lhs_sset, rhs_sset) ;
  if res_sset != res_zdd.to_set() {
    println!("{}", name) ;
    println!("lhs_zdd:") ;
    lhs_zdd.print("| ".to_string()) ;
    println!("rhs_zdd:") ;
    rhs_zdd.print("| ".to_string()) ;
    println!("lhs_set:") ;
    sset_print(& lhs_sset, "| ".to_string()) ;
    println!("rhs_set:") ;
    sset_print(& rhs_sset, "| ".to_string()) ;
    println!("res_zdd:") ;
    res_zdd.print("| ".to_string()) ;
    println!("res_set:") ;
    sset_print(& res_sset, "| ".to_string()) ;
    panic!("{} | result mismatch", name)
  } ;
  (res_zdd, res_sset)
}

fn run(factory: & mut Factory, u_bound: usize) {
  let mut rng = StdRng::new().unwrap() ;
  let mut vec = vec![
    (factory.zero(), set_zero()),
    (factory.one(), set_one()),
  ] ;
  for i in 0..u_bound {
    // println!("at {}", i) ;
    let res = match random::<Closed01<f64>>().0 {
      f if f < 0.50f64 || i < 10 => {
        let index = usize::rand(& mut rng) % (vec.len()) ;
        let (ref zdd, ref sset) = vec[index] ;
        let c = Lbl::rand(& mut rng) % 100u8 ;
        match f {
          f if f < 0.16f64 || i < 10 => offset(
            factory, zdd, sset, & c
          ),
          f if f < 0.32f64 => onset(
            factory, zdd, sset, & c
          ),
          _ => change(
            factory, zdd, sset, & c
          ),
        }
      },
      f => {
        let index = usize::rand(& mut rng) % (vec.len()) ;
        let (ref lhs_zdd, ref lhs_sset) = vec[index] ;
        let index = usize::rand(& mut rng) % (vec.len()) ;
        let (ref rhs_zdd, ref rhs_sset) = vec[index] ;
        match f {
          f if f < 0.66f64 => union(
            factory, lhs_zdd, rhs_zdd, lhs_sset, rhs_sset
          ),
          f if f < 0.83f64 => inter(
            factory, lhs_zdd, rhs_zdd, lhs_sset, rhs_sset
          ),
          _ => minus(
            factory, lhs_zdd, rhs_zdd, lhs_sset, rhs_sset
          ),
        }
      }
    } ;
    vec.push(res)
  }
}

#[test]
fn thirteen_times_100() {
  let mut factory = Factory::mk() ;
  for _ in 0..13 {
    run(& mut factory, 100)
  }
}

#[test]
fn seven_times_10000() {
  let mut factory = Factory::mk() ;
  for _ in 0..7 {
    run(& mut factory, 10000)
  }
}

#[test]
fn five_times_1000000() {
  let mut factory = Factory::mk() ;
  for _ in 0..5 {
    run(& mut factory, 1000000)
  }
}