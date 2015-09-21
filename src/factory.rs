use std::collections::HashMap ;
use std::cmp::Eq ;

use hashconsing::* ;

use ::{ ZddTree, Zdd, ZddTreeOps } ;
use ZddTree::* ;

use cache ;
use cache::{ UnaryKey, BinaryKey, UnaryCache, BinaryCache } ;
use cache::UnaryCache::* ;
use cache::BinaryCache::* ;

use zip::ZipResult ;
use zip::ZipResult::* ;
use zip::UnaryZip ;
use zip::BinaryZip ;



pub struct Factory<Label: Eq + Hash> {
  consign: HashConsign<ZddTree<Label>>,

  one: Zdd<Label>,
  zero: Zdd<Label>,

  offset_cache: HashMap<UnaryKey<Label>, Zdd<Label>>,
  onnset_cache: HashMap<UnaryKey<Label>, Zdd<Label>>,
  change_cache: HashMap<UnaryKey<Label>, Zdd<Label>>,

  union_cache: HashMap<BinaryKey, Zdd<Label>>,
  inter_cache: HashMap<BinaryKey, Zdd<Label>>,
  minus_cache: HashMap<BinaryKey, Zdd<Label>>,

  count_cache: HashMap<UnaryKey<usize>, Zdd<Label>>,
}

impl<Label: Eq + Hash + Copy> Factory<Label> {
  pub fn mk() -> Self {
    let mut consign = HashConsign::empty() ;
    let one = consign.mk(One) ;
    let zero = consign.mk(Zero) ;
    Factory {
      consign: consign,

      one: one,
      zero: zero,

      offset_cache: HashMap::new(),
      onnset_cache: HashMap::new(),
      change_cache: HashMap::new(),

      union_cache: HashMap::new(),
      inter_cache: HashMap::new(),
      minus_cache: HashMap::new(),

      count_cache: HashMap::new(),
    }
  }

  #[inline(always)]
  pub fn one(& self) -> Zdd<Label> { self.one.clone() }
  #[inline(always)]
  pub fn zero(& self) -> Zdd<Label> { self.zero.clone() }

  #[inline(always)]
  pub fn is_one(& self, zdd: & Zdd<Label>) -> bool {
    self.one.hkey() == zdd.hkey()
  }
  #[inline(always)]
  pub fn is_zero(& self, zdd: & Zdd<Label>) -> bool {
    self.zero.hkey() == zdd.hkey()
  }

  #[inline(always)]
  pub fn node(
    & mut self, lbl: Label, left: Zdd<Label>, right: Zdd<Label>
  ) -> Zdd<Label> {
    if right == self.zero { left } else {
      self.consign.mk( Node(lbl, left, right) )
    }
  }

  #[inline(always)]
  fn unary_cache_get<Info: Eq + Hash + Copy>(
    cache: & HashMap<UnaryKey<Info>, Zdd<Label>>,
    zdd: & Zdd<Label>,
    info: & Info
  ) -> Option<Zdd<Label>> {
    match cache.get( & (zdd.hkey(), * info) ) {
      None => None, Some(zdd) => Some(zdd.clone()),
    }
  }

  #[inline(always)]
  fn binary_cache_get(
    cache: & HashMap<BinaryKey, Zdd<Label>>,
    lhs: & Zdd<Label>,
    rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    match cache.get( & (lhs.hkey(), rhs.hkey()) ) {
      None => None, Some(zdd) => Some(zdd.clone()),
    }
  }

  fn unary_cache_insert(
    & mut self,
    cache: UnaryCache,
    lbl: & Label,
    zdd: & Zdd<Label>
  ) {
    match match cache {
      Offset(key) => self.offset_cache.insert( (key, * lbl), zdd.clone() ),
      Onnset(key) => self.onnset_cache.insert( (key, * lbl), zdd.clone() ),
      Change(key) => self.change_cache.insert( (key, * lbl), zdd.clone() ),
    } {
      None => (), Some(_) => panic!("cache overwrite"),
    }
  }

  fn binary_cache_insert(
    & mut self,
    cache: BinaryCache,
    zdd: & Zdd<Label>
  ) {
    match match cache {
      Union(key1, key2) => {
        // Symmetric.
        self.union_cache.insert( (key1, key2), zdd.clone() ) ;
        self.union_cache.insert( (key2, key1), zdd.clone() )
      },
      Inter(key1, key2) => {
        // Symmetric.
        self.inter_cache.insert( (key1, key2), zdd.clone() ) ;
        self.inter_cache.insert( (key2, key1), zdd.clone() )
      },
      Minus(key1, key2) =>
        self.minus_cache.insert( (key1, key2), zdd.clone() ),
    } {
      None => (), Some(_) => panic!("cache overwrite"),
    }
  }

  fn unary_zip(
    & mut self, zdd: Zdd<Label>, zip: & mut UnaryZip<Label>, 
  ) -> ZipResult<Label, Zdd<Label>> {
    use zip::UnaryStep::* ;
    let mut zip = zip ;
    let mut zdd = zdd ;
    loop {
      zdd = match zip.pop() {
        None => return Done(zdd),
        Some( Lft(cache, top, rgt) ) => {
          zip.push(
            Rgt(cache, top, zdd.clone())
          ) ;
          return NYet(rgt)
        },
        Some( Rgt(cache, top, lft) ) => {
          let zdd = self.node(top, lft, zdd) ;
          self.unary_cache_insert(cache, zip.lbl(), & zdd) ;
          zdd
        },
      }
    }
  }

  fn binary_zip(
    & mut self, zdd: Zdd<Label>, zip: & mut BinaryZip<Label>, 
  ) -> ZipResult<Label, (Zdd<Label>, Zdd<Label>)> {
    use zip::BinaryStep::* ;
    let mut zip = zip ;
    let mut zdd = zdd ;
    loop {
      zdd = match zip.pop() {
        None => return Done(zdd),
        Some( Lft(cache, top, l_rgt, r_rgt) ) => {
          zip.push(
            Rgt(cache, top, zdd.clone())
          ) ;
          return NYet((l_rgt, r_rgt))
        },
        Some( TLft(cache, top, rgt) ) => {
          let zdd = self.node(top, zdd, rgt) ;
          self.binary_cache_insert(cache, & zdd) ;
          zdd
        },
        Some( Rgt(cache, top, lft) ) => {
          let zdd = self.node(top, lft, zdd) ;
          self.binary_cache_insert(cache, & zdd) ;
          zdd
        },
      }
    }
  }
}

macro_rules! return_if_done {
  ($slf:ident ^ bin $zdd:expr, $zip:expr) => (
    return_if_done!($slf ^ binary_zip $zdd, $zip)
  ) ;
  ($slf:ident ^ un $zdd:expr, $zip:expr) => (
    return_if_done!($slf ^ unary_zip $zdd, $zip)
  ) ;
  ($slf:ident ^ $fun:ident $zdd:expr, $zip:expr) => (
    match $slf.$fun($zdd, & mut $zip) {
      Done(zdd) => return zdd,
      NYet(data) => data,
    }
  ) ;
}

macro_rules! zdd_match_height {
  ($top:expr,
    above $lbl:expr => $abv_b:block,
    equal => $eql_b:block,
    below => $blw_b:block,
  ) => (
    if $top < $lbl $abv_b else { if $top == $lbl $eql_b else $blw_b }
  ) ;
  ($top:expr,
    above $lbl:expr => $abv_b:block,
    equal => $eql_b:block,
    below => $blw_b:block
  ) => (
    if $top < $lbl $abv_b else { if $top == $lbl $eql_b else $blw_b }
  ) ;
}

pub trait ZddOps<Label> {
  // |===| Operations on ZDDs.

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
  fn count(& mut self, & Zdd<Label>) -> usize ;
}


impl<Label: Ord + Eq + Hash + Copy> ZddOps<Label> for Factory<Label> {

  fn offset(& mut self, zdd: & Zdd<Label>, lbl: & Label) -> Zdd<Label> {
    use zip::UnaryStep::* ;
    let mut zip = UnaryZip::mk(lbl) ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.get() {

        & Node(ref top, ref lft, ref rgt) => {
          match Factory::unary_cache_get(& self.offset_cache, & zdd, lbl) {
            Some(res) => {
              println!("cache hit, offset") ;
              return_if_done!(self ^ un res, zip)
            },
            None => zdd_match_height!(top,
              above lbl => {
                // We're above the label, going left first and updating path.
                let cache = cache::offset(& zdd) ;
                zip.push(Lft(cache, * top, rgt.clone())) ;
                lft.clone()
              },
              equal => {
                // Only keep left part and go up.
                return_if_done!(self ^ un lft.clone(), zip)
              },
              below => {
                // We're below the label, going up.
                return_if_done!(self ^ un zdd.clone(), zip)
              },
            ),
          }
        },

        _ => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        }
      } ;
    }
  }

  fn onset(& mut self, zdd: & Zdd<Label>, lbl: & Label) -> Zdd<Label> {
    use zip::UnaryStep::* ;
    let mut zip = UnaryZip::mk(lbl) ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.get() {

        & Node(ref top, ref lft, ref rgt) => {
          match Factory::unary_cache_get(& self.onnset_cache, & zdd, lbl) {
            Some(res) => {
              println!("cache hit, onset") ;
              return_if_done!(self ^ un res, zip)
            },
            None => zdd_match_height!(top,
              above lbl => {
                // We're above the label, going left first and updating path.
                let cache = cache::onnset(& zdd) ;
                zip.push(Lft(cache, * top, rgt.clone())) ;
                lft.clone()
              },
              equal => {
                // Only keep left part and go up.
                return_if_done!(self ^ un rgt.clone(), zip)
              },
              below => {
                // We're below the label, it's not there.
                let zero = self.zero() ;
                return_if_done!(self ^ un zero, zip)
              },
            ),
          }
        },

        _ => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },
      } ;
    }
  }

  fn change(& mut self, zdd: & Zdd<Label>, lbl: & Label) -> Zdd<Label> {
    use zip::UnaryStep::* ;
    let mut zip = UnaryZip::mk(lbl) ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.get() {

        & Node(ref top, ref lft, ref rgt) => {
          match Factory::unary_cache_get(& self.change_cache, & zdd, lbl) {
            Some(res) => {
              println!("cache hit, onset") ;
              return_if_done!(self ^ un res, zip)
            },
            None => zdd_match_height!(top,
              above lbl => {
                // We're above the label, going left first and updating path.
                let cache = cache::change(& zdd) ;
                zip.push(Lft(cache, * top, rgt.clone())) ;
                lft.clone()
              },
              equal => {
                // Change and go up.
                let zdd = self.node(* top, rgt.clone(), lft.clone()) ;
                return_if_done!(self ^ un zdd, zip)
              },
              below => {
                // We're below the label, it's not there.
                return_if_done!(self ^ un zdd.clone(), zip)
              },
            ),
          }
        },

        & One => {
          let zero = self.zero() ;
          let zdd = self.node(* lbl, zero, zdd.clone()) ;
          return_if_done!(self ^ un zdd, zip)
        },

        & Zero => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },
      } ;
    }
  }


  fn union(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> Zdd<Label> {
    use zip::BinaryStep::* ;
    if lhs == rhs { return lhs.clone() } ;
    let mut zip = BinaryZip::mk() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;
      pair = if lhs == rhs {
        return_if_done!(self ^ bin lhs, zip)
      } else { match (lhs.top(), rhs.top()) {

        (Err(false), _) => {
          return_if_done!(self ^ bin rhs, zip)
        },

        (_, Err(false)) => {
          return_if_done!(self ^ bin lhs, zip)
        },

        (t_lhs, t_rhs) => match Factory::binary_cache_get(
          & self.union_cache, & lhs, & rhs
        ) {
          Some(res) => {
            println!("cache hit, union") ;
            return_if_done!(self ^ bin res, zip)
          },
          None => {
            let cache = cache::union(& lhs, & rhs) ;
            let (lhs, rhs, t_lhs, t_rhs) = if t_rhs < t_lhs {
              (rhs, lhs, t_rhs, t_lhs)
            } else {
              (lhs, rhs, t_lhs, t_rhs)
            } ;
            match (lhs.get(), rhs.get()) {

              (& Node(ref top, ref lft, ref rgt), _)
              if t_lhs < t_rhs => {
                // `lhs` is above `rhs`.
                zip.push(TLft(cache, * top, rgt.clone())) ;
                (lft.clone(), rhs.clone())
              },

              (
                & Node(ref l_top, ref l_lft, ref l_rgt),
                & Node(ref r_top, ref r_lft, ref r_rgt)
              ) => {
                assert!( l_top == r_top ) ;
                zip.push(Lft(cache, * l_top, l_rgt.clone(), r_rgt.clone())) ;
                (l_lft.clone(), r_lft.clone())
              },

              _ => unreachable!(),
            }
          },
        },
      } }
    }
  }


  fn inter(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> Zdd<Label> {
    use zip::BinaryStep::* ;
    if lhs == rhs { return lhs.clone() } ;
    let mut zip = BinaryZip::mk() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;
      pair = if lhs == rhs {
        return_if_done!(self ^ bin lhs, zip)
      } else { match (lhs.top(), rhs.top()) {

        (Err(false), _) |
        (_, Err(false)) => {
          let zero = self.zero() ;
          return_if_done!(self ^ bin zero, zip)
        },

        (t_lhs, t_rhs) => match Factory::binary_cache_get(
          & self.inter_cache, & lhs, & rhs
        ) {
          Some(res) => {
            println!("cache hit, inter") ;
            return_if_done!(self ^ bin res, zip)
          },
          None => {
            let cache = cache::inter(& lhs, & rhs) ;
            let (lhs, rhs, t_lhs, t_rhs) = if t_rhs < t_lhs {
              (rhs, lhs, t_rhs, t_lhs)
            } else {
              (lhs, rhs, t_lhs, t_rhs)
            } ;
            match (lhs.get(), rhs.get()) {

              (& Node(ref top, ref lft, _), _)
              if t_lhs < t_rhs => {
                // `lhs` is above `rhs`.
                zip.push(TLft(cache, * top, self.zero())) ;
                (lft.clone(), rhs.clone())
              },

              (
                & Node(ref l_top, ref l_lft, ref l_rgt),
                & Node(ref r_top, ref r_lft, ref r_rgt)
              ) => {
                assert!( l_top == r_top ) ;
                zip.push(Lft(cache, * l_top, l_rgt.clone(), r_rgt.clone())) ;
                (l_lft.clone(), r_lft.clone())
              },

              _ => unreachable!(),
            }
          },
        },
      } }
    }
  }


  fn minus(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> Zdd<Label> {
    use zip::BinaryStep::* ;
    if lhs == rhs { return lhs.clone() } ;
    let mut zip = BinaryZip::mk() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;
      pair = if lhs == rhs {
        let zero = self.zero() ;
        return_if_done!(self ^ bin zero, zip)
      } else { match (lhs.top(), rhs.top()) {

        (Err(false), _) => {
          let zero = self.zero() ;
          return_if_done!(self ^ bin zero, zip)
        },

        (_, Err(false)) => {
          return_if_done!(self ^ bin lhs.clone(), zip)
        },

        (t_lhs, t_rhs) => match Factory::binary_cache_get(
          & self.minus_cache, & lhs, & rhs
        ) {
          Some(res) => {
            println!("cache hit, minus") ;
            return_if_done!(self ^ bin res, zip)
          },
          None => {
            let cache = cache::minus(& lhs, & rhs) ;
            match (lhs.get(), rhs.get()) {

              (& Node(ref top, ref lft, ref rgt), _)
              if t_lhs < t_rhs => {
                // `lhs` is above `rhs`.
                zip.push(TLft(cache, * top, rgt.clone())) ;
                (lft.clone(), rhs.clone())
              },

              (_, & Node(ref top, ref r_lft, _))
              if t_lhs > t_rhs => {
                zip.push(TLft(cache, * top, self.zero())) ;
                (lhs.clone(), r_lft.clone())
              },

              (
                & Node(ref l_top, ref l_lft, ref l_rgt),
                & Node(ref r_top, ref r_lft, ref r_rgt)
              ) => {
                assert!( l_top == r_top ) ;
                zip.push(Lft(cache, * l_top, l_rgt.clone(), r_rgt.clone())) ;
                (l_lft.clone(), r_lft.clone())
              },

              _ => unreachable!(),
            }
          },
        },
      } }
    }
  }

  // TODO: Make this cached.
  fn count(& mut self, zdd: & Zdd<Label>) -> usize {
    if self.is_zero(zdd) { return 0 } ;
    if self.is_one(zdd) { return 1 } ;
    let mut to_visit = vec![ zdd.clone() ] ;
    let mut count = 0 ;
    loop {
      if let Some(zdd) = to_visit.pop() {
        match zdd.get() {

          & Node(_, ref lft, ref rgt) => {
            to_visit.push(lft.clone()) ;
            to_visit.push(rgt.clone())
          },

          & One => count = count + 1,

          & Zero => (),
        }

      } else { return count }
    }
  }



}