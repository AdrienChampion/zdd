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

impl<
  Label: Ord + Eq + Hash + Copy + ::std::fmt::Display
> Factory<Label> {
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
  fn rm_one(& mut self, zdd: & Zdd<Label>) -> Zdd<Label> {
    match zdd.get() {
      & HasOne(ref kid) => kid.clone(),
      _ => zdd.clone(),
    }
  }

  /// Returns the left subtree if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  pub fn lft(& mut self, zdd: & Zdd<Label>) -> Result<Zdd<Label>,bool> {
    match zdd.get() {
      & Node(_, ref lft, _) => Ok(lft.clone()),
      & HasOne(ref kid) => match kid.get() {
        & Node(_, ref lft, _) => Ok(self.has_one(lft.clone())),
        & Zero => Err(true),
        _ => panic!("[lft] ZDD is ill-formed"),
      },
      & Zero => Err(false),
    }
  }

  /// Returns the right subtree if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  fn rgt(& mut self, zdd: & Zdd<Label>) -> Result<Zdd<Label>,bool> {
    match zdd.get() {
      & Node(_, _, ref rgt) => Ok(rgt.clone()),
      & HasOne(ref kid) => match kid.get() {
        & Node(_, _, ref rgt) => Ok(rgt.clone()),
        & Zero => Err(true),
        _ => panic!("[rgt] ZDD is ill-formed"),
      },
      & Zero => Err(false),
    }
  }

  /// Returns the subtrees if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  #[inline(always)]
  fn kids(
    & mut self, zdd: & Zdd<Label>
  ) -> Result<(Zdd<Label>, Zdd<Label>),bool> {
    match zdd.get() {
      & Node(_, ref lft, ref rgt) => Ok((lft.clone(), rgt.clone())),
      & HasOne(ref kid) => match kid.get() {
        & Node(_, ref lft, ref rgt) => Ok(
          (self.has_one(lft.clone()), rgt.clone())
        ),
        & Zero => Err(true),
        _ => panic!("[rgt] ZDD is ill-formed"),
      },
      & Zero => Err(false),
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
  fn offset_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(& self.offset_cache, zdd, lbl)
  }

  #[inline(always)]
  fn onnset_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(& self.onnset_cache, zdd, lbl)
  }

  #[inline(always)]
  fn change_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(& self.change_cache, zdd, lbl)
  }

  #[inline(always)]
  fn count_cache_get(
    & self, zdd: & Zdd<Label>
  ) -> Option<usize> {
    Factory::unary_cache_get(& self.count_cache, zdd, & ())
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

  #[inline(always)]
  fn union_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(& self.union_cache, lhs, rhs)
  }

  #[inline(always)]
  fn inter_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(& self.inter_cache, lhs, rhs)
  }

  #[inline(always)]
  fn minus_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(& self.minus_cache, lhs, rhs)
  }

  fn unary_cache_insert(
    & mut self,
    cache: UnaryCache,
    lbl: & Label,
    zdd: & Zdd<Label>
  ) {
    use ::ZddPrint ;
    // println!("inserting cache ({}, {})", cache, lbl) ;
    // zdd.print("| ".to_string()) ;
    match match cache {
      Offset(key) => self.offset_cache.insert( (key, * lbl), zdd.clone() ),
      Onnset(key) => self.onnset_cache.insert( (key, * lbl), zdd.clone() ),
      Change(key) => self.change_cache.insert( (key, * lbl), zdd.clone() ),
    } {
      None => (), Some(zdd) => {
        println!("overwrite:") ;
        zdd.print("| ".to_string()) ;
        panic!("cache overwrite for {} ({})", cache, lbl)
      },
    }
  }

  fn binary_cache_insert(
    & mut self,
    cache: BinaryCache,
    zdd: & Zdd<Label>
  ) {
    match match cache {
      Union(key1, key2) => {
        self.union_cache.insert( (key1, key2), zdd.clone() ).is_some()
      },
      Inter(key1, key2) => {
        self.inter_cache.insert( (key1, key2), zdd.clone() ).is_some()
      },
      Minus(key1, key2) =>
        self.minus_cache.insert( (key1, key2), zdd.clone() ).is_some(),
    } {
      false => (), true => panic!("cache overwrite ({})", cache),
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

  pub fn offset(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
    use zip::UnaryStep::* ;
    let mut zip = UnaryZip::mk(lbl) ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },

        Err(true) => {
          let one = self.one() ;
          return_if_done!(self ^ un one, zip)
        },

        Ok(top) if top.gt(lbl) => {
          // Below the label, going up.
          return_if_done!(self ^ un zdd.clone(), zip)
        },

        Ok(top) if top.eq(lbl) => {
          // Only keep left part and go up.
          let lft = self.lft(& zdd).unwrap() ;
          return_if_done!(self ^ un lft, zip)
        },

        // Above the label, querying cache.
        Ok(top) => match self.offset_cache_get(& zdd, lbl) {
          // Cache hit.
          Some(res) => return_if_done!(self ^ un res, zip),
          // Not found.
          None => {
            let cache = cache::offset(& zdd) ;
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(cache, top, rgt) ) ;
            lft
          },
        },
      }
    }
  }

  pub fn onset(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
    use zip::UnaryStep::* ;
    let mut zip = UnaryZip::mk(lbl) ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(_) => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },

        Ok(top) if top.gt(lbl) => {
          // Below the label, it's not there.
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },

        Ok(top) if top.eq(lbl) => {
          // Only keep right part and go up.
          let rgt = self.rgt(& zdd).unwrap() ;
          return_if_done!(self ^ un rgt, zip)
        }

        // Above the label, querying cache.
        Ok(top) => match self.onnset_cache_get(& zdd, lbl) {
          // Cache hit.
          Some(res) => return_if_done!(self ^ un res, zip),
          // Not found.
          None => {
            let cache = cache::onnset(& zdd) ;
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(cache, top, rgt) ) ;
            lft
          },
        },
      }
    }
  }

  pub fn change(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
    use zip::UnaryStep::* ;
    let mut zip = UnaryZip::mk(lbl) ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => {
          let zero = self.zero() ;
          return_if_done!(self ^ un zero, zip)
        },

        Err(true) => {
          let zero = self.zero() ;
          let one = self.one() ;
          let zdd = self.node(* lbl, zero, one) ;
          return_if_done!(self ^ un zdd, zip)
        },

        Ok(top) if top.gt(lbl) => {
          // Below the label, not there so adding it.
          let zero = self.zero() ;
          let zdd = self.node(* lbl, zero, zdd.clone()) ;
          return_if_done!(self ^ un zdd, zip)
        },

        Ok(top) if top.eq(lbl) => {
          // Swap left and right.
          let (lft,rgt) = self.kids(& zdd).unwrap() ;
          let zdd = self.node(top, rgt, lft) ;
          return_if_done!(self ^ un zdd, zip)
        },

        // Above the label, querying cache.
        Ok(top) => match self.change_cache_get(& zdd, lbl) {
          // Cache hit.
          Some(res) => return_if_done!(self ^ un res, zip),
          // Not found.
          None => {
            let cache = cache::change(& zdd) ;
            let (lft, rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(cache, top, rgt) ) ;
            lft
          },
        },
      }
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

      pair = if lhs == rhs {
        return_if_done!(self ^ bin lhs, zip)
      } else {
        match (lhs.top(), rhs.top()) {

          // One of them is the empty set.
          (Err(false), _) => return_if_done!(self ^ bin rhs, zip),
          (_, Err(false)) => return_if_done!(self ^ bin lhs, zip),

          // One of them contains only the empty combination.
          (Err(true), _) => {
            let zdd = self.has_one(rhs) ;
            return_if_done!(self ^ bin zdd, zip)
          },
          (_, Err(true)) => {
            let zdd = self.has_one(lhs) ;
            return_if_done!(self ^ bin zdd, zip)
          },

          // Both are nodes.
          (Ok(l_top), Ok(r_top)) => {
            // Reordering to have rhs below lhs.
            let (lhs, rhs, l_top, r_top) = if l_top > r_top {
              (rhs, lhs, r_top, l_top)
            } else {
              (lhs, rhs, l_top, r_top)
            } ;

            // Querying cache.
            match self.union_cache_get(& lhs, & rhs) {
              // Cache hit.
              Some(res) => return_if_done!(self ^ bin res, zip),
              // Not found.
              None => {
                let cache = cache::union(& lhs, & rhs) ;

                if l_top == r_top {
                  // Extracting kids.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                  // Recursing.
                  zip.push(Lft(cache, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                } else {
                  // Making sure lhs is above.
                  assert!(l_top < r_top) ;
                  // Extracting kids from lhs.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  // Recursing on left kid.
                  zip.push(TLft(cache, l_top, l_rgt)) ;
                  (l_lft, rhs)
                }
              },
            }
          },
        }
      } ;
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

      pair = if lhs == rhs {
        return_if_done!(self ^ bin lhs, zip)
      } else {
        match (lhs.top(), rhs.top()) {

          // Trivial cases.
          (Err(false), _) | (_, Err(false)) => {
            let zero = self.zero() ;
            return_if_done!(self ^ bin zero, zip)
          },

          (Err(true), _) => {
            let zdd = if rhs.has_one() { lhs } else { self.zero() } ;
            return_if_done!(self ^ bin zdd, zip)
          },
          (_, Err(true)) => {
            let zdd = if lhs.has_one() { rhs } else { self.zero() } ;
            return_if_done!(self ^ bin zdd, zip)
          },

          // Both are nodes.
          (Ok(l_top), Ok(r_top)) => {
            // Reordering to have rhs below lhs.
            let (lhs, rhs, l_top, r_top) = if l_top > r_top {
              (rhs, lhs, r_top, l_top)
            } else {
              (lhs, rhs, l_top, r_top)
            } ;

            // Querying cache.
            match self.inter_cache_get(& lhs, & rhs) {
              // Cache hit.
              Some(res) => return_if_done!(self ^ bin res, zip),
              // Not found.
              None => {
                let cache = cache::inter(& lhs, & rhs) ;
                if l_top == r_top {
                  // Extracting kids.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                  // Recursing.
                  zip.push(Lft(cache, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                } else {
                  // Making sure lhs is above.
                  assert!(l_top < r_top) ;
                  // Discarding right kid, it's not in the intersection.
                  let l_lft = self.lft(& lhs).unwrap() ;
                  (l_lft, rhs)
                }
              },
            }
          },
        }
      } ;
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

      pair = if lhs == rhs {
        let zero = self.zero() ;
        return_if_done!(self ^ bin zero, zip)
      } else {
        match (lhs.top(), rhs.top()) {

          // One of them is the empty set.
          (Err(false), _) | (_, Err(false)) =>
            return_if_done!(self ^ bin lhs, zip),

          // One of them contains only the empty combination.
          (Err(true), _) => {
            let zdd = if rhs.has_one() { self.zero() } else { lhs } ;
            return_if_done!(self ^ bin zdd, zip)
          },
          (_, Err(true)) => {
            let zdd = self.rm_one(& lhs) ;
            return_if_done!(self ^ bin zdd, zip)
          },

          // Both are nodes, querying cache.
          (Ok(l_top), Ok(r_top)) => match self.minus_cache_get(& lhs, & rhs) {
            // Cache hit.
            Some(res) => return_if_done!(self ^ bin res, zip),
            // Not found.
            None => {
              let cache = cache::minus(& lhs, & rhs) ;

              if l_top < r_top {
                // lhs is above.
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                // Recursing on left kid.
                zip.push(TLft(cache, l_top, l_rgt)) ;
                (l_lft, rhs)
              } else {
                if r_top < l_top {
                  // rhs is above, discarding its right kid.
                  let r_lft = self.lft(& rhs).unwrap() ;
                  // Recursing.
                  (lhs, r_lft)
                } else {
                  // Extracting kids.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                  // Recursing.
                  zip.push(Lft(cache, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                }
              }
            },
          },
        }
      } ;
    }
  }

  pub fn count(& mut self, zdd: & Zdd<Label>) -> usize {
    use zip::CountZip::* ;
    let mut zip = vec![ ] ;
    let mut zdd = zdd.clone() ;
    let mut count = 0 ;
    loop {
      match zdd.top() {
        // ZDD is a leaf, looping.
        Err(has_one) => {
          if has_one { count = count + 1 } ;
          loop {
            match zip.pop() {
              // We done.
              None => return count,

              // Recursing, memorizing count.
              Some( Lft(key, rgt) ) => {
                zip.push(Rgt(key, count)) ;
                count = 0 ;
                zdd = rgt ;
                break
              },

              // Going up.
              Some( Rgt(key, lft_cnt) ) => {
                count = count + lft_cnt ;
                self.count_cache.insert( (key.hkey(), ()), count ) ;
                ()
              },
            }
          }
        },

        // It's a node, querying cache.
        _ => match self.count_cache_get(& zdd) {
          // Cache hit.
          Some(cnt) => {
            count = count + cnt ;
            zdd = self.zero()
          },

          // Not found.
          None => {
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push(Lft(zdd, rgt)) ;
            zdd = lft
          },
        },
      }
    }
  }


}