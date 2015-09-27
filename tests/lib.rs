extern crate zdd ;

use std::collections::BTreeSet ;

use zdd::ZddTreeOps ;

type Lbl = & 'static str ;
type Set = BTreeSet<Lbl> ;
type SSet = BTreeSet<Set> ;
type Factory = zdd::Factory<Lbl> ;

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
        nu_set.insert(e) ; ()
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
        nu_set.insert(e) ; ()
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
        nu_set.insert(e) ; ()
      }
    } ;
    if ! saw_lbl { nu_set.insert(lbl) ; () } ;
    nu_sset.insert(nu_set) ; ()
  } ;
  nu_sset
}

#[test]
fn change_basic() {
  let mut factory = Factory::mk() ;

  let zdd = factory.zero() ;
  let set = set_zero() ;
  assert!(zdd.to_set() == set) ;

  let zdd = factory.one() ;
  let set = set_one() ;
  assert!(zdd.to_set() == set) ;

  let zdd = factory.change(& zdd, & "a") ;
  let set = set_change(& set, & "a") ;
  assert!(zdd.to_set() == set) ;

  let zdd = factory.change(& zdd, & "b") ;
  let set = set_change(& set, & "b") ;
  assert!(zdd.to_set() == set) ;

  let zdd = factory.change(& zdd, & "a") ;
  let set = set_change(& set, & "a") ;
  assert!(zdd.to_set() == set) ;
}

#[test]
fn offset_basic() {
  let mut factory = Factory::mk() ;

  let zdd = factory.zero() ;
  let set = set_zero() ;
  let zdd = factory.offset(& zdd, & "a") ;
  let set = set_offset(& set, & "a") ;
  assert!(zdd.to_set() == set) ;
}

#[test]
fn onset_basic() {
  let mut factory = Factory::mk() ;

  let zdd = factory.zero() ;
  let set = set_zero() ;
  let zdd = factory.onset(& zdd, & "a") ;
  let set = set_onset(& set, & "a") ;
  assert!(zdd.to_set() == set) ;
}
