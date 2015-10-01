use std::collections::{ HashMap, BTreeSet } ;
use std::cmp::Eq ;

use hashconsing::* ;

use ::{ FactoryTrait, ZddTree, Zdd, ZddTreeOps } ;
use ZddTree::* ;

use zip ;
use zip::{ HKey, UnaryKey, BinaryKey } ;

/** A ZDD factory.

Wraps a hash consing table. Functions `count`, `offset`, `onset`, `change`,
`union`, `inter`, `minus` and `subset` are cached.
*/
pub struct Factory<Label: Eq + Hash> {
  consign: HashConsign<ZddTree<Label>>,

  one: Zdd<Label>,
  zero: Zdd<Label>,

  count_cache: HashMap<HKey, usize>,

  offset_cache: HashMap<UnaryKey<Label>, Zdd<Label>>,
  onset_cache: HashMap<UnaryKey<Label>, Zdd<Label>>,
  change_cache: HashMap<UnaryKey<Label>, Zdd<Label>>,

  union_cache: HashMap<BinaryKey, Zdd<Label>>,
  inter_cache: HashMap<BinaryKey, Zdd<Label>>,
  minus_cache: HashMap<BinaryKey, Zdd<Label>>,

  subset_cache: HashMap<BinaryKey, bool>,
}


impl<Label: Eq + Hash> FactoryTrait<Label> for Factory<Label> {
  fn mk_node(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    if rgt == self.zero {
      // Right is zero, no need for `lbl`.
      lft
    } else {
      if let & HasOne(ref lft) = lft.get() {
        // Left is a `HasOne`, pushing upward.
        let node = self.consign.mk( Node(lbl, lft.clone(), rgt) ) ;
        return self.consign.mk( HasOne(node) )
      } ;
      self.consign.mk( Node(lbl, lft, rgt) )
    }
  }
}

#[cfg(test)]
#[inline(always)]
fn cache_overwrite<T>(insert_result: Option<T>, name: & str) {
  match insert_result {
    None => (),
    Some(_) => panic!("cache overwrite in {} cache", name),
  }
}

#[cfg(not(test))]
#[inline(always)]
fn cache_overwrite<T>(_: Option<T>, _: & str) { () }

impl<Label: Eq + Hash + Clone> zip::unary::Zip<
  HKey, Label, (), usize, zip::Count<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: HKey, count: & usize) {
    let _res = self.count_cache.insert(key, * count) ;
    cache_overwrite(_res, "count")
  }
  fn combine(& mut self, _: (), l_count: usize, r_count: usize) -> usize {
    l_count + r_count
  }
}


impl<Label: Eq + Hash + Clone> zip::unary::Zip<
  (HKey, Label), Label, Label, Zdd<Label>, zip::Offset<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, Label), zdd: & Zdd<Label>) {
    let _res = self.offset_cache.insert(key, zdd.clone()) ;
    cache_overwrite(_res, "offset")
  }
  fn combine(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft, rgt)
  }
}


impl<Label: Eq + Hash + Clone> zip::unary::Zip<
  (HKey, Label), Label, Label, Zdd<Label>, zip::Onset<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, Label), zdd: & Zdd<Label>) {
    let _res = self.onset_cache.insert(key, zdd.clone()) ;
    cache_overwrite(_res, "onset")
  }
  fn combine(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft, rgt)
  }
}


impl<Label: Eq + Hash + Clone> zip::unary::Zip<
  (HKey, Label), Label, Label, Zdd<Label>, zip::Change<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, Label), zdd: & Zdd<Label>) {
    let _res = self.change_cache.insert(key, zdd.clone()) ;
    cache_overwrite(_res, "change")
  }
  fn combine(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft, rgt)
  }
}


impl<Label: Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, Label, Zdd<Label>, zip::Union<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, HKey), zdd: & Zdd<Label>) {
    let _res = self.union_cache.insert(key, zdd.clone()) ;
    cache_overwrite(_res, "union")
  }
  fn combine(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft, rgt)
  }
}


impl<Label: Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, Label, Zdd<Label>, zip::Inter<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, HKey), zdd: & Zdd<Label>) {
    let _res = self.inter_cache.insert(key, zdd.clone()) ;
    cache_overwrite(_res, "inter")
  }
  fn combine(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft, rgt)
  }
}


impl<Label: Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, Label, Zdd<Label>, zip::Minus<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, HKey), zdd: & Zdd<Label>) {
    let _res = self.minus_cache.insert(key, zdd.clone()) ;
    cache_overwrite(_res, "minus")
  }
  fn combine(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    self.mk_node(lbl, lft, rgt)
  }
}


impl<Label: Eq + Hash + Clone> zip::binary::Zip<
  (HKey, HKey), Label, (), bool, zip::Subset<Label>
> for Factory<Label> {
  fn cache_insert(& mut self, key: (HKey, HKey), val: & bool) {
    let _res = self.subset_cache.insert(key, * val) ;
    cache_overwrite(_res, "subset")
  }
  fn combine(& mut self, _: (), lft: bool, rgt: bool) -> bool {
    lft && rgt
  }
}


impl<Label: Ord + Eq + Hash + Clone + ::std::fmt::Display> Factory<Label> {

  /// Creates a new factory.
  pub fn mk() -> Self {
    let mut consign = HashConsign::empty() ;
    let zero = consign.mk(Zero) ;
    let one = consign.mk( HasOne(zero.clone()) ) ;
    Factory {
      consign: consign,

      one: one,
      zero: zero,

      count_cache: HashMap::new(),

      offset_cache: HashMap::new(),
      onset_cache: HashMap::new(),
      change_cache: HashMap::new(),

      union_cache: HashMap::new(),
      inter_cache: HashMap::new(),
      minus_cache: HashMap::new(),

      subset_cache: HashMap::new(),
    }
  }

  /// The *one* element.
  #[inline(always)]
  pub fn one(& self) -> Zdd<Label> { self.one.clone() }

  /// The *zero* element.
  #[inline(always)]
  pub fn zero(& self) -> Zdd<Label> { self.zero.clone() }

  /// Adds the empty combination to a ZDD if it was not already there.
  #[inline(always)]
  pub fn add_one(& mut self, kid: Zdd<Label>) -> Zdd<Label> {
    if match kid.get() { & HasOne(_) => true, _ => false, } {
      kid
    } else {
      self.consign.mk(HasOne(kid))
    }
  }

  /// Creates a new node.
  #[inline(always)]
  pub fn node(
    & mut self, lbl: Label, lft: Zdd<Label>, rgt: Zdd<Label>
  ) -> Zdd<Label> {
    (self as & mut FactoryTrait<Label>).mk_node(lbl, lft, rgt)
  }

  /// Removes the empty combination from a ZDD if it's there.
  #[inline(always)]
  pub fn rm_one(& mut self, zdd: & Zdd<Label>) -> Zdd<Label> {
    match zdd.get() {
      & HasOne(ref kid) => kid.clone(),
      _ => zdd.clone(),
    }
  }

  /// Returns the left subtree if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  ///
  /// Mutability on the factory is necessary because
  /// `lft(HasOne(Node(_,lft,_)))` is `HasOne(lft)`, thus triggering node
  /// creation and hash consing table lookup.
  #[inline(always)]
  pub fn lft(& mut self, zdd: & Zdd<Label>) -> Result<Zdd<Label>,bool> {
    match zdd.get() {
      & Node(_, ref lft, _) => Ok(lft.clone()),
      & HasOne(ref kid) => match kid.get() {
        & Node(_, ref lft, _) => Ok(self.add_one(lft.clone())),
        & Zero => Err(true),
        _ => panic!("[lft] ZDD is ill-formed"),
      },
      & Zero => Err(false),
    }
  }

  /// Returns the right subtree if the ZDD is a node, an error of `true` if the
  /// ZDD is `One` and `false` if it is `Zero`.
  ///
  /// Unlike `lft`, mutability on the factory is not necessary.
  #[inline(always)]
  pub fn rgt(& self, zdd: & Zdd<Label>) -> Result<Zdd<Label>,bool> {
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
  ///
  /// Mutability on the factory is mandatory for the same reason as `lft`.
  #[inline(always)]
  pub fn kids(
    & mut self, zdd: & Zdd<Label>
  ) -> Result<(Zdd<Label>, Zdd<Label>),bool> {
    match zdd.get() {
      & Node(_, ref lft, ref rgt) => Ok((lft.clone(), rgt.clone())),
      & HasOne(ref kid) => match kid.get() {
        & Node(_, ref lft, ref rgt) => Ok(
          (self.add_one(lft.clone()), rgt.clone())
        ),
        & Zero => Err(true),
        _ => panic!("[rgt] ZDD is ill-formed"),
      },
      & Zero => Err(false),
    }
  }

  /// Queries a unary cache.
  #[inline(always)]
  fn unary_cache_get<Info: Eq + Hash + Clone, Out: Clone>(
    cache: & HashMap<UnaryKey<Info>, Out>,
    zdd: & Zdd<Label>,
    info: & Info
  ) -> Option<Out> {
    match cache.get( & (zdd.hkey(), info.clone()) ) {
      None => None, Some(out) => Some(out.clone()),
    }
  }

  /// Queries the count cache.
  #[inline(always)]
  fn count_cache_get(
    & self, zdd: & Zdd<Label>
  ) -> Option<usize> {
    match self.count_cache.get( & zdd.hkey() ) {
      None => None, Some(out) => Some(out.clone()),
    }
  }

  /// Queries the offset cache.
  #[inline(always)]
  fn offset_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(& self.offset_cache, zdd, lbl)
  }

  /// Queries the onset cache.
  #[inline(always)]
  fn onset_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(& self.onset_cache, zdd, lbl)
  }

  /// Queries the change cache.
  #[inline(always)]
  fn change_cache_get(
    & self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Option<Zdd<Label>> {
    Factory::unary_cache_get(& self.change_cache, zdd, lbl)
  }

  /// Queries a binary cache.
  #[inline(always)]
  fn binary_cache_get<T: Clone>(
    cache: & HashMap<BinaryKey, T>,
    lhs: & Zdd<Label>,
    rhs: & Zdd<Label>
  ) -> Option<T> {
    match cache.get( & (lhs.hkey(), rhs.hkey()) ) {
      None => None, Some(zdd) => Some(zdd.clone()),
    }
  }

  /// Queries the union cache.
  #[inline(always)]
  fn union_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(& self.union_cache, lhs, rhs)
  }

  /// Queries the inter cache.
  #[inline(always)]
  fn inter_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(& self.inter_cache, lhs, rhs)
  }

  /// Queries the minus cache.
  #[inline(always)]
  fn minus_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<Zdd<Label>> {
    Factory::binary_cache_get(& self.minus_cache, lhs, rhs)
  }

  /// Queries the subset cache.
  #[inline(always)]
  fn subset_cache_get(
    & self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Option<bool> {
    Factory::binary_cache_get(& self.subset_cache, lhs, rhs)
  }

  /// The number of combinations in a ZDD. Cached.
  pub fn count(& mut self, zdd: & Zdd<Label>) -> usize {
    use zip::unary::Step::Lft ;
    let mut zip = zip::count() ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => zip_up!(self > zip > 0),

        Err(true) => zip_up!(self > zip > 1),

        _ => match self.count_cache_get(& zdd) {
          Some(count) => zip_up!(self > zip > count),
          None => {
            let key = zdd.hkey() ;
            let (lft, rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, (), rgt) ) ;
            lft
          },
        },
      }
    }
  }

  /// The set of combinations of `zdd` in which `lbl` does not appear.
  /// Cached.
  pub fn offset(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
    use zip::unary::Step::Lft ;
    let mut zip = zip::offset() ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => zip_up!(self > zip > self.zero()),

        Err(true) => zip_up!(self > zip > self.one()),

        // Below the label, going up.
        Ok(ref top) if top.gt(lbl) => zip_up!(self > zip > zdd),

        Ok(ref top) if top.eq(lbl) => {
          // Only keep left part and go up.
          zip_up!(self > zip > self.lft(& zdd).unwrap())
        },

        // Above the label, querying cache.
        Ok(ref top) => match self.offset_cache_get(& zdd, lbl) {
          // Cache hit.
          Some(res) => zip_up!(self > zip > res),
          // Not found.
          None => {
            let key = (zdd.hkey(), lbl.clone()) ;
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, top.clone(), rgt) ) ;
            lft
          },
        },
      }
    }
  }

  /// The set of combinations of `zdd` in which `lbl` appears, without `lbl`
  /// in them. Cached.
  pub fn onset(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
    use zip::unary::Step::Lft ;
    let mut zip = zip::onset() ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(_) => zip_up!(self > zip > self.zero()),

        // Below the label, it's not there.
        Ok(ref top) if top.gt(lbl) => zip_up!(self > zip > self.zero()),

        Ok(ref top) if top.eq(lbl) => {
          // Only keep right part and go up.
          let rgt = self.rgt(& zdd).unwrap() ;
          zip_up!(self > zip > rgt)
        }

        // Above the label, querying cache.
        Ok(ref top) => match self.onset_cache_get(& zdd, lbl) {
          // Cache hit.
          Some(res) => zip_up!(self > zip > res),
          // Not found.
          None => {
            let key = (zdd.hkey(), lbl.clone()) ;
            let (lft,rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, top.clone(), rgt) ) ;
            lft
          },
        },
      }
    }
  }

  /// Switches `lbl` in each combination of `zdd`. Inverts `offset` and
  /// `onset`. Cached.
  pub fn change(
    & mut self, zdd: & Zdd<Label>, lbl: & Label
  ) -> Zdd<Label> {
    use zip::unary::Step::Lft ;
    let mut zip = zip::change() ;
    let mut zdd = zdd.clone() ;
    loop {
      zdd = match zdd.top() {

        Err(false) => zip_up!(self > zip > self.zero()),

        Err(true) => {
          let zero = self.zero() ;
          let one = self.one() ;
          let zdd = self.node(lbl.clone(), zero, one) ;
          zip_up!(self > zip > zdd)
        },

        Ok(ref top) if top.gt(lbl) => {
          // Below the label, not there so adding it.
          let zero = self.zero() ;
          let zdd = self.node(lbl.clone(), zero, zdd.clone()) ;
          zip_up!(self > zip > zdd)
        },

        Ok(ref top) if top.eq(lbl) => {
          // Swap left and right.
          let (lft,rgt) = self.kids(& zdd).unwrap() ;
          let zdd = self.node(top.clone(), rgt, lft) ;
          zip_up!(self > zip > zdd)
        },

        // Above the label, querying cache.
        Ok(ref top) => match self.change_cache_get(& zdd, lbl) {
          // Cache hit.
          Some(res) => zip_up!(self > zip > res),
          // Not found.
          None => {
            let key = (zdd.hkey(), lbl.clone()) ;
            let (lft, rgt) = self.kids(& zdd).unwrap() ;
            zip.push( Lft(key, top.clone(), rgt) ) ;
            lft
          },
        },
      }
    }
  }

  /// The union of two ZDDs. Cached.
  pub fn union(
    & mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Zdd<Label> {
    use zip::binary::Step::{ Lft, TLft } ;
    let mut zip = zip::union() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {

      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > lhs)
      } else {
        match (lhs.top(), rhs.top()) {

          // One of them is the empty set.
          (Err(false), _) => zip_up!(self >> zip > rhs),
          (_, Err(false)) => zip_up!(self >> zip > lhs),

          // One of them contains only the empty combination.
          (Err(true), _) => {
            let zdd = self.add_one(rhs) ;
            zip_up!(self >> zip > zdd)
          },
          (_, Err(true)) => {
            let zdd = self.add_one(lhs) ;
            zip_up!(self >> zip > zdd)
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
              Some(res) => zip_up!(self >> zip > res),
              // Not found.
              None => {
                let key = (lhs.hkey(), rhs.hkey()) ;

                if l_top == r_top {
                  // Extracting kids.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                  // Recursing.
                  zip.push(Lft(key, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                } else {
                  // Making sure lhs is above.
                  assert!(l_top < r_top) ;
                  // Extracting kids from lhs.
                  let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                  // Recursing on left kid.
                  zip.push(TLft(key, l_top, l_rgt)) ;
                  (l_lft, rhs)
                }
              },
            }
          },
        }
      } ;
    }
  }

  /// The intersection of two ZDDs. Cached.
  pub fn inter(
    & mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Zdd<Label> {
    use zip::binary::Step::Lft ;
    let mut zip = zip::inter() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > lhs)
      } else {
        match (lhs.top(), rhs.top()) {

          // Trivial cases.
          (Err(false), _) | (_, Err(false)) =>
            zip_up!(self >> zip > self.zero()),

          (Err(true), _) => {
            let zdd = if rhs.has_one() { lhs } else { self.zero() } ;
            zip_up!(self >> zip > zdd)
          },
          (_, Err(true)) => {
            let zdd = if lhs.has_one() { rhs } else { self.zero() } ;
            zip_up!(self >> zip > zdd)
          },

          // Both are nodes.
          (Ok(l_top), Ok(r_top)) => if l_top == r_top {
            // Querying cache.
            match self.inter_cache_get(& lhs, & rhs) {
              // Cache hit.
              Some(res) => zip_up!(self >> zip > res),
              // Not found.
              None => {
                let key = (lhs.hkey(), rhs.hkey()) ;
                // Extracting kids.
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                // Recursing.
                zip.push(Lft(key, l_top, l_rgt, r_rgt)) ;
                (l_lft, r_lft)
              },
            }
          } else {
            if l_top < r_top {
              (self.lft(& lhs).unwrap(), rhs)
            } else {
              (self.lft(& rhs).unwrap(), lhs)
            }
          },
        }
      } ;
    }
  }

  /// The difference of two ZDDs. Cached.
  pub fn minus(
    & mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>
  ) -> Zdd<Label> {
    use zip::binary::Step::{ Lft, TLft } ;
    let mut zip = zip::minus() ;
    let mut pair = (lhs.clone(), rhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;

      pair = if lhs == rhs {
        zip_up!(self >> zip > self.zero())
      } else {
        match (lhs.top(), rhs.top()) {

          // One of them is the empty set.
          (Err(false), _) | (_, Err(false)) =>
            zip_up!(self >> zip > lhs),

          // One of them contains only the empty combination.
          (Err(true), _) => {
            let zdd = if rhs.has_one() { self.zero() } else { lhs } ;
            zip_up!(self >> zip > zdd)
          },
          (_, Err(true)) => {
            let zdd = self.rm_one(& lhs) ;
            zip_up!(self >> zip > zdd)
          },

          // Both are nodes, querying cache.
          (Ok(l_top), Ok(r_top)) => match self.minus_cache_get(& lhs, & rhs) {
            // Cache hit.
            Some(res) => zip_up!(self >> zip > res),
            // Not found.
            None => {
              let key = (lhs.hkey(), rhs.hkey()) ;
              if l_top < r_top {
                // lhs is above.
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                // Recursing on left kid.
                zip.push(TLft(key, l_top, l_rgt)) ;
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
                  zip.push(Lft(key, l_top, l_rgt, r_rgt)) ;
                  (l_lft, r_lft)
                }
              }
            },
          },
        }
      } ;
    }
  }

  /// Returns true iff `lhs` is a subset of `rhs`. Cached.
  pub fn subset(& mut self, lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> bool {
    use zip::binary::Step::{ Lft, TLft } ;
    let mut zip = zip::subset() ;
    // More natural for me to reverse them.
    let mut pair = (rhs.clone(), lhs.clone()) ;
    loop {
      let (lhs, rhs) = pair ;


      pair = if lhs == rhs {
        zip_up!(self >> zip > true)
      } else {
        match (lhs.top(), rhs.top()) {
          (_, Err(false)) => zip_up!(self >> zip > true),
          (Err(true), _) |
          (Err(false), _) => zip_up!(self >> zip > false),
          (_, Err(true)) => zip_up!(self >> zip > lhs.has_one()),
          (Ok(l_top), Ok(r_top)) => match self.subset_cache_get(& lhs, & rhs) {
            // Cache hit.
            Some(res) => zip_up!(self >> zip > res),
            // Not found.
            None => {
              let key = (lhs.hkey(), rhs.hkey()) ;
              if l_top == r_top {
                let (l_lft,l_rgt) = self.kids(& lhs).unwrap() ;
                let (r_lft,r_rgt) = self.kids(& rhs).unwrap() ;
                zip.push(Lft(key, (), l_rgt, r_rgt)) ;
                (l_lft, r_lft)
              } else {
                if l_top < r_top {
                  // lhs is above, going left.
                  let l_lft = self.lft(& lhs).unwrap() ;
                  zip.push(TLft(key, (), true)) ;
                  (l_lft, rhs)
                } else {
                  // rhs is above, lhs cannot be a subset.
                  zip_up!(self >> zip > false)
                }
              }
            },
          }
        }
      } ;
    }
  }

  /// The size of the consign.
  #[inline(always)]
  pub fn consign_len(& self) -> usize { self.consign.len() }

  /// The size of the `count` cache.
  #[inline(always)]
  pub fn count_cache_len(& self) -> usize { self.count_cache.len() }
  /// The size of the `offset` cache.
  #[inline(always)]
  pub fn offset_cache_len(& self) -> usize { self.offset_cache.len() }
  /// The size of the `onset` cache.
  #[inline(always)]
  pub fn onset_cache_len(& self) -> usize { self.onset_cache.len() }
  /// The size of the `change` cache.
  #[inline(always)]
  pub fn change_cache_len(& self) -> usize { self.change_cache.len() }

  /// The size of the `union` cache.
  #[inline(always)]
  pub fn union_cache_len(& self) -> usize { self.union_cache.len() }
  /// The size of the `inter` cache.
  #[inline(always)]
  pub fn inter_cache_len(& self) -> usize { self.inter_cache.len() }
  /// The size of the `minus` cache.
  #[inline(always)]
  pub fn minus_cache_len(& self) -> usize { self.minus_cache.len() }

  /// The size of the `subset` cache.
  #[inline(always)]
  pub fn subset_cache_len(& self) -> usize { self.subset_cache.len() }
}

/** A factory for `Factory` to set the capacity of the consign and the caches.

## Example

Each initial capacity can be set separately.

```
use zdd::FactoryBuilder ;

let factory = FactoryBuilder::mk()
  .consign_len(1)
  .count_cache_len(2)
  .offset_cache_len(3).onset_cache_len(4).change_cache_len(5)
  .union_cache_len(6).inter_cache_len(7).minus_cache_len(8)
  .build::<usize>() ;
```

Or at the same time.

```
use zdd::FactoryBuilder ;
let factory = FactoryBuilder::mk().len(7).build::<usize>() ;
```

It is also possible to set all the caches capacities at once.

```
use zdd::FactoryBuilder ;
let factory = FactoryBuilder::mk().caches_len(7).build::<usize>() ;
```

*/
pub struct FactoryBuilder {
  consign: usize,

  count: usize,

  offset: usize, onset: usize, change: usize,

  union: usize, inter: usize, minus: usize,

  subset: usize,
}

impl FactoryBuilder {
  /// Builds the factory with the capacities.
  pub fn build<Label: Eq + Hash>(self) -> Factory<Label> {
    let mut consign = HashConsign::empty_with_capacity(self.consign) ;
    let zero = consign.mk(Zero) ;
    let one = consign.mk( HasOne(zero.clone()) ) ;
    Factory {
      consign: consign,

      one: one,
      zero: zero,

      count_cache: HashMap::with_capacity(self.count),

      offset_cache: HashMap::with_capacity(self.offset),
      onset_cache: HashMap::with_capacity(self.onset),
      change_cache: HashMap::with_capacity(self.change),

      union_cache: HashMap::with_capacity(self.union),
      inter_cache: HashMap::with_capacity(self.inter),
      minus_cache: HashMap::with_capacity(self.minus),

      subset_cache: HashMap::with_capacity(self.subset),
    }
  }

  /// Creates a new factory builder with all capacities equal to zero.
  pub fn mk() -> Self {
    FactoryBuilder {
      consign: 0,

      count: 0,

      offset: 0, onset: 0, change: 0,

      union: 0, inter: 0, minus: 0,

      subset: 0,
    }
  }

  /// Sets the capacities of the consign and all the caches at once.
  pub fn len(mut self, l: usize) -> Self {
    self.consign = l ;
    self.caches_len(l)
  }

  /// Sets the capacity of the consign.
  pub fn consign_len(mut self, l: usize) -> Self {
    self.consign = l ; self
  }

  /// Sets the capacities of all the caches at once.
  pub fn caches_len(mut self, l: usize) -> Self {
    self.count = l ;
    self.offset = l ;
    self.onset = l ;
    self.change = l ;
    self.union = l ;
    self.inter = l ;
    self.minus = l ;
    self.subset = l ;
    self
  }

  /// Sets the capacity of the count cache.
  pub fn count_cache_len(mut self, l: usize) -> Self {
    self.count = l ; self
  }

  /// Sets the capacity of the offset cache.
  pub fn offset_cache_len(mut self, l: usize) -> Self {
    self.offset = l ; self
  }
  /// Sets the capacity of the onset cache.
  pub fn onset_cache_len(mut self, l: usize) -> Self {
    self.onset = l ; self
  }
  /// Sets the capacity of the change cache.
  pub fn change_cache_len(mut self, l: usize) -> Self {
    self.change = l ; self
  }

  /// Sets the capacity of the union cache.
  pub fn union_cache_len(mut self, l: usize) -> Self {
    self.union = l ; self
  }
  /// Sets the capacity of the inter cache.
  pub fn inter_cache_len(mut self, l: usize) -> Self {
    self.inter = l ; self
  }
  /// Sets the capacity of the minus cache.
  pub fn minus_cache_len(mut self, l: usize) -> Self {
    self.minus = l ; self
  }

  /// Sets the capacity of the minus cache.
  pub fn subset_cache_len(mut self, l: usize) -> Self {
    self.subset = l ; self
  }
}