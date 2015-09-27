//! TODO: optimize basic functions, there's too much cloning going on.

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

  count_cache: HashMap<UnaryKey<()>, usize>,
}

impl<Label: Ord + Eq + Hash + Copy + ::std::fmt::Display> Factory<Label> {
  pub fn mk() -> Self {
    let mut consign = HashConsign::empty() ;
    let zero = consign.mk(Zero) ;
    let one = consign.mk( HasOne(zero.clone()) ) ;
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
  fn node(
    & mut self, lbl: Label, left: Zdd<Label>, right: Zdd<Label>
  ) -> Zdd<Label> {
    if right == self.zero {
      // Right is zero, no need for `lbl`.
      left
    } else {
      if let & HasOne(ref left) = left.get() {
        // Left is a `HasOne`, pushing upward.
        let node = self.consign.mk( Node(lbl, left.clone(), right) ) ;
        return self.consign.mk( HasOne(node) )
      } ;
      self.consign.mk( Node(lbl, left, right) )
    }
  }

  #[inline(always)]
  fn has_one(& mut self, kid: Zdd<Label>) -> Zdd<Label> {
    if match kid.get() { & HasOne(_) => true, _ => false, } {
      kid
    } else {
      self.consign.mk(HasOne(kid))
    }
  }

  #[inline(always)]
  fn unary_cache_get<Info: Eq + Hash + Copy, Out: Clone>(
    cache: & HashMap<UnaryKey<Info>, Out>,
    zdd: & Zdd<Label>,
    info: & Info
  ) -> Option<Out> {
    match cache.get( & (zdd.hkey(), * info) ) {
      None => None, Some(out) => Some(out.clone()),
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
    let mut zdd = if zip.has_one() {
      zip.reset_has_one() ;
      self.has_one(zdd)
    } else { zdd } ;
    loop {
      zdd = match zip.pop() {
        None => return Done(zdd),
        Some( Lft(cache, top, rgt) ) => {
          zip.push( Rgt(cache, top, zdd.clone()) ) ;
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
    let mut zdd = if zip.has_one() {
      zip.reset_has_one() ;
      self.has_one(zdd)
    } else { zdd } ;
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

  pub fn offset(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
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

        & HasOne(ref kid) => {
          zip.set_has_one() ;
          kid.clone()
        },

        & Zero => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        }
      } ;
    }
  }

  pub fn onset(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
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
                // Only keep right part and go up.
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

        & HasOne(ref kid) => {
          kid.clone()
        },

        & Zero => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },
      } ;
    }
  }

  pub fn change(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
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

        & HasOne(ref kid) => {
          zip.set_has_one() ;
          kid.clone()
        },

        & Zero => {
          if zip.has_one() {
            let zero = self.zero() ;
            let one = self.one() ;
            let zdd = self.node(* lbl, zero, one) ;
            zip.reset_has_one() ;
            return_if_done!(self ^ un zdd, zip)
          } else {
            let zero = self.zero() ;
            return_if_done!(self ^ un zero, zip)
          }
        },
      } ;
    }
  }


  pub fn union(
    & mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Zdd<Label> {
    use zip::BinaryStep::* ;
    let mut zip = BinaryZip::mk() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = match (
        lhs == rhs,
        self.is_zero(& lhs), self.is_zero(& rhs),
        self.is_one(& lhs), self.is_one(& rhs)
      ) {

        // Catching simple cases first.
        (true,_,_,_,_) => return_if_done!(self ^ bin lhs, zip),
        (_,true,_,_,_) => return_if_done!(self ^ bin rhs, zip),
        (_,_,true,_,_) => return_if_done!(self ^ bin lhs, zip),
        (_,_,_,true,_) => {
          let zdd = self.has_one(rhs) ;
          return_if_done!(self ^ bin zdd, zip)
        },
        (_,_,_,_,true) => {
          let zdd = self.has_one(lhs) ;
          return_if_done!(self ^ bin zdd, zip)
        },

        // Querying cache.
        _ => match Factory::binary_cache_get(
          & self.union_cache, & lhs, & rhs
        ) {
          Some(res) => {
            println!("cache hit, union") ;
            return_if_done!(self ^ bin res, zip)
          },
          None => match (lhs.get(), rhs.get()) {

            // One of them has one.
            (& HasOne(ref l_kid), & HasOne(ref r_kid)) => {
              zip.set_has_one() ;
              (l_kid.clone(), r_kid.clone())
            },
            (& HasOne(ref l_kid), _) => {
              zip.set_has_one() ;
              (l_kid.clone(), rhs.clone())
            },
            (_, & HasOne(ref r_kid)) => {
              zip.set_has_one() ;
              (lhs.clone(), r_kid.clone())
            },

            // Both are nodes.
            _ => {
              let cache = cache::union(& lhs, & rhs) ;
              // Reordering so that highest one is `lhs`.
              let tops = (lhs.top(), rhs.top()) ;
              let (lhs, rhs, l_lbl, r_lbl) = match tops {
                (Some(l), Some(r)) =>
                  if l > r { (& rhs, & lhs, r, l) }
                  else { (& lhs, & rhs, l, r) },
                _ => unreachable!(),
              } ;
              match (lhs.get(), rhs.get()) {

                (& Node(ref top, ref lft, ref rgt), _) if l_lbl < r_lbl => {
                  // `lhs` is above `rhs`.
                  zip.push(TLft(cache, * top, rgt.clone())) ;
                  (lft.clone(), rhs.clone())
                },

                (
                  & Node(ref l_lbl, ref l_lft, ref l_rgt),
                  & Node(ref r_lbl, ref r_lft, ref r_rgt)
                ) => {
                  // `lhs` and `rhs` are at the same level.
                  assert!( l_lbl == r_lbl ) ;
                  zip.push(Lft(cache, * l_lbl, l_rgt.clone(), r_rgt.clone())) ;
                  (l_lft.clone(), r_lft.clone())
                },

                _ => unreachable!(),

              }
            }
          }
        }
      }
    }
  }


  pub fn inter(
    & mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Zdd<Label> {
    use zip::BinaryStep::* ;
    let mut zip = BinaryZip::mk() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = match (
        lhs == rhs,
        self.is_zero(& lhs), self.is_zero(& rhs),
        self.is_one(& lhs), self.is_one(& rhs)
      ) {

        // Catching simple cases first.
        (true,_,_,_,_) => return_if_done!(self ^ bin lhs, zip),
        (_,true,_,_,_) | (_,_,true,_,_) => {
          let zdd = self.zero() ;
          return_if_done!(self ^ bin zdd, zip)
        },
        (_,_,_,true,_) => {
          let zdd = if rhs.has_one() { self.one() } else { self.zero() } ;
          return_if_done!(self ^ bin zdd, zip)
        },
        (_,_,_,_,true) => {
          let zdd = if lhs.has_one() { self.one() } else { self.zero() } ;
          return_if_done!(self ^ bin zdd, zip)
        },

        // Querying cache.
        _ => match Factory::binary_cache_get(
          & self.inter_cache, & lhs, & rhs
        ) {
          Some(res) => {
            println!("cache hit, union") ;
            return_if_done!(self ^ bin res, zip)
          },
          None => match (lhs.get(), rhs.get()) {

            // One of them has one.
            (& HasOne(ref l_kid), & HasOne(ref r_kid)) => {
              zip.set_has_one() ;
              (l_kid.clone(), r_kid.clone())
            },
            (& HasOne(ref l_kid), _) => (l_kid.clone(), rhs.clone()),
            (_, & HasOne(ref r_kid)) => (lhs.clone(), r_kid.clone()),

            // Both are nodes.
            _ => {
              let cache = cache::inter(& lhs, & rhs) ;
              // Reordering so that highest one is `lhs`.
              let tops = (lhs.top(), rhs.top()) ;
              let (lhs, rhs, l_lbl, r_lbl) = match tops {
                (Some(l), Some(r)) =>
                  if l > r { (& rhs, & lhs, r, l) }
                  else { (& lhs, & rhs, l, r) },
                _ => unreachable!(),
              } ;
              match (lhs.get(), rhs.get()) {

                (& Node(_, ref lft, _), _)
                if l_lbl < r_lbl => {
                  // `lhs` is above `rhs`.
                  (lft.clone(), rhs.clone())
                },

                (
                  & Node(ref l_lbl, ref l_lft, ref l_rgt),
                  & Node(ref r_lbl, ref r_lft, ref r_rgt)
                ) => {
                  // `lhs` and `rhs` are at the same level.
                  assert!( l_lbl == r_lbl ) ;
                  zip.push(Lft(cache, * l_lbl, l_rgt.clone(), r_rgt.clone())) ;
                  (l_lft.clone(), r_lft.clone())
                },

                _ => unreachable!(),

              }
            }
          }
        }
      }
    }
  }


  pub fn minus(
    & mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Zdd<Label> {
    use zip::BinaryStep::* ;
    let mut zip = BinaryZip::mk() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = match (
        lhs == rhs,
        self.is_zero(& lhs), self.is_zero(& rhs),
        self.is_one(& lhs), self.is_one(& rhs)
      ) {

        // Catching simple cases first.
        (true,_,_,_,_) | (_,true,_,_,_) => {
          let zdd = self.zero() ;
          return_if_done!(self ^ bin zdd, zip)
        },
        (_,_,true,_,_) => {
          return_if_done!(self ^ bin lhs, zip)
        },
        (_,_,_,true,_) => {
          let zdd = if rhs.has_one() { self.zero() } else { self.one() } ;
          return_if_done!(self ^ bin zdd, zip)
        },
        (_,_,_,_,true) => {
          let zdd = match lhs.get() {
            & HasOne(ref kid) => kid.clone(),
            _ => lhs.clone(),
          } ;
          return_if_done!(self ^ bin zdd, zip)
        },

        // Querying cache.
        _ => match Factory::binary_cache_get(
          & self.minus_cache, & lhs, & rhs
        ) {
          Some(res) => {
            println!("cache hit, union") ;
            return_if_done!(self ^ bin res, zip)
          },
          None => match (lhs.get(), rhs.get()) {

            // One of them has one.
            (& HasOne(ref l_kid), & HasOne(ref r_kid)) =>
              (l_kid.clone(), r_kid.clone()),
            (_, & HasOne(ref r_kid)) =>
              (lhs.clone(), r_kid.clone()),
            (& HasOne(ref l_kid), _) => {
              zip.set_has_one() ;
              (l_kid.clone(), rhs.clone())
            },

            // Both are nodes.
            _ => {
              let cache = cache::minus(& lhs, & rhs) ;
              let (l_lbl, r_lbl) = match (lhs.top(), rhs.top()) {
                (Some(l), Some(r)) => (l,r),
                _ => unreachable!(),
              } ;
              match (lhs.get(), rhs.get()) {

                (& Node(ref lbl, ref lft, ref rgt), _) if l_lbl < r_lbl => {
                  // `lhs` is above `rhs`.
                  zip.push(TLft(cache, *lbl, rgt.clone())) ;
                  (lft.clone(), rhs.clone())
                },

                (_, & Node(_, ref lft, _)) if l_lbl > r_lbl => {
                  // `lhs` is below `rhs`.
                  (lhs.clone(), lft.clone())
                },

                (
                  & Node(ref l_lbl, ref l_lft, ref l_rgt),
                  & Node(ref r_lbl, ref r_lft, ref r_rgt)
                ) => {
                  // `lhs` and `rhs` are at the same level.
                  assert!( l_lbl == r_lbl ) ;
                  zip.push(Lft(cache, * l_lbl, l_rgt.clone(), r_rgt.clone())) ;
                  (l_lft.clone(), r_lft.clone())
                },

                _ => unreachable!(),

              }
            }
          }
        }
      }
    }
  }

  pub fn count(& mut self, zdd: & Zdd<Label>) -> usize {
    use zip::CountZip::* ;
    if self.is_zero(zdd) { return 0 } ;
    if self.is_one(zdd) { return 1 } ;
    let mut zip = vec![ ] ;
    let mut zdd = zdd.clone() ;
    let mut count = 0 ;
    loop {
      if ! self.is_zero(& zdd) {
        match Factory::unary_cache_get(
          & self.count_cache, & zdd, & ()
        ) {
          Some(cnt) => {
            count = count + cnt ;
            zdd = self.zero()
          },
          None => zdd = match zdd.get() {
            & Node(_, ref lft, ref rgt) => {
              zip.push( Lft(zdd.clone(), rgt.clone()) ) ;
              lft.clone()
            },
            & HasOne(ref kid) => {
              zip.push( One ) ;
              kid.clone()
            },
            _ => unreachable!(),
          }
        }
      } else {

        loop {
          match zip.pop() {
            None => return count,
            Some( Lft(key, rgt) ) => {
              zip.push(Rgt(key, count)) ;
              count = 0 ;
              zdd = rgt ;
              break
            },
            Some( One ) => {
              count = count + 1
            },
            Some( Rgt(key, lft_cnt) ) => {
              count = count + lft_cnt ;
              self.count_cache.insert( (key.hkey(), ()), count ) ;
              ()
            },
          }
        }
      }
    }
  }



}