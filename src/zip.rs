use ::{ Zdd, FactoryTrait } ;

/// ZDD are hashed to `u64`.
pub type HKey = u64 ;

/// Cache key for `offset`, `onset` and `change` caches with `Info` the type of
/// the labels of the zdd. Also used for `count` with `Info = ()`.
pub type UnaryKey<Info> = (HKey, Info) ;

/// Cache key for `union`, `inter` and `minus` caches.
pub type BinaryKey = (HKey, HKey) ;


pub enum Res<D, NY> {
  Done(D),
  NYet(NY)
}


pub enum UnZipStep<Info, Label, Terminal> {
  Lft(Info, Zdd<Label>),
  Rgt(Info, Terminal),
}

pub type LabelUnZipStep<Label> = UnZipStep< Label, Label, Zdd<Label> > ;
pub type CountUnZipStep<Label> = UnZipStep< (), Label, usize > ;

pub enum BinZipStep<Info, Label> {
  Lft(Info, Zdd<Label>, Zdd<Label>),
  TLft(Info, Zdd<Label>),
  Rgt(Info, Zdd<Label>),
}

pub type LabelBinZipStep<Label> = BinZipStep<Label, Label> ;



trait ZipHasLabel<Label> {
  #[inline(always)]
  fn label(& self) -> & Label ;
}

trait ZipHasVec<Data> {
  #[inline(always)]
  fn push(& mut self, Data) ;
  #[inline(always)]
  fn pop(& mut self) -> Option<Data> ;
}


macro_rules! mk_zip {
  ($id:ident< $($ty:ident),+ > {
    lbl : $i_ty:ty , Vec<$($v_id:ident : $v_ty:ty),+>
  } fn $fun:ident) => (
    pub struct $id<$($ty),+> {
      lbl: $i_ty,
      vec: Vec<( $($v_ty),+ )>,
    }
    impl<$($ty),+> $id<$($ty),+> {
      fn mk(lbl: $i_ty) -> Self {
        $id { lbl: lbl, vec: vec![] }
      }
      mk_zip!{ rec fn vec<$($v_id: $v_ty),+> }
    }
    impl<$($ty),+> ZipHasLabel<$i_ty> for $id<$($ty),+> {
      mk_zip!{ rec fn label $i_ty }
    }
    impl<$($ty),+> ZipHasVec<($($v_ty),+)> for $id<$($ty),+> {
      mk_zip!{ rec impl fn vec<$($v_ty),+> }
    }
    pub fn $fun<$($ty),+>(lbl: $i_ty) -> $id<$($ty),+> { $id::mk(lbl) }
  ) ;
  ($id:ident< $($ty:ident),+ > {
    Vec<$($v_id:ident : $v_ty:ty),+>
  } fn $fun:ident) => (
    pub struct $id<$($ty),+> {
      vec: Vec<( $($v_ty),+ )>,
    }
    impl<$($ty),+> $id<$($ty),+> {
      fn mk() -> Self {
        $id { vec: vec![] }
      }
      mk_zip!{ rec fn vec<$($v_id: $v_ty),+> }
    }
    impl<$($ty),+> ZipHasVec<($($v_ty),+)> for $id<$($ty),+> {
      mk_zip!{ rec impl fn vec<$($v_ty),+> }
    }
    pub fn $fun<$($ty),+>() -> $id<$($ty),+> { $id::mk() }
  ) ;
  (rec fn label $i_ty:ty) => (
    fn label(& self) -> & $i_ty { & self.lbl }
  ) ;
  (rec fn vec<$($id:ident: $ty:ty),+>) => (
    #[inline(always)]
    pub fn push(& mut self, $($id: $ty),+) {
      self.vec.push(($($id),+))
    }
  ) ;
  (rec impl fn vec<$($ty:ty),+>) => (
    fn push(& mut self, e: ($($ty),+)) {
      self.vec.push(e)
    }
    fn pop(& mut self) -> Option<($($ty),+)> {
      self.vec.pop()
    }
  ) ;
}

mk_zip!{
  OffsetZip<Label> {
    lbl: Label,
    Vec< key: HKey, step: LabelUnZipStep<Label> >
  } fn offset
}

mk_zip!{
  OnsetZip<Label> {
    lbl: Label,
    Vec< key: HKey, step: LabelUnZipStep<Label> >
  } fn onset
}

mk_zip!{
  ChangeZip<Label> {
    lbl: Label,
    Vec< key: HKey, step: LabelUnZipStep<Label> >
  } fn change
}

mk_zip!{
  CountZip2<Label> {
    Vec< key: HKey, step: CountUnZipStep<Label> >
  } fn count
}

mk_zip!{
  UnionZip<Label> {
    Vec< l_key: HKey, r_key: HKey, step: LabelBinZipStep<Label> >
  } fn union
}

mk_zip!{
  InterZip<Label> {
    Vec< l_key: HKey, r_key: HKey, step: LabelBinZipStep<Label> >
  } fn inter
}

mk_zip!{
  MinusZip<Label> {
    Vec< l_key: HKey, r_key: HKey, step: LabelBinZipStep<Label> >
  } fn minus
}


pub trait UnaryLabelZipper<
  Label, Zip: ZipHasLabel<Label> + ZipHasVec<(HKey, LabelUnZipStep<Label>)>
> : FactoryTrait<Label> {
  /// Insert into the cache corresponding to `Zip`.
  #[inline(always)]
  fn cache_insert(& mut self, HKey, & Label, & Zdd<Label>) ;

  /// Zips on `zip`. Returns a `NYet` of the next ZDD to explore if any,
  /// or `Done` of the top ZDD.
  /// Updates the cache as it goes.
  fn zip(
    & mut self, mut zdd: Zdd<Label>, zip: & mut Zip
  ) -> Res<Zdd<Label>, Zdd<Label>> {
    use self::UnZipStep::* ;
    use self::Res::* ;
    loop {
      zdd = match zip.pop() {
        // Can't zip up, done.
        None => return Done(zdd),

        // A right branch hasn't been explored yet.
        Some( (key, Lft(lbl, rgt)) ) => {
          zip.push( (key, Rgt( lbl, zdd)) ) ;
          return NYet(rgt)
        },

        // We were in a right branch, going up.
        Some( (key, Rgt(lbl, lft)) ) => {
          let zdd = self.mk_node(lbl, lft, zdd) ;
          self.cache_insert(key, zip.label(), & zdd) ;
          zdd
        },
      }
    }
  }
}

pub trait UnaryCountZipper<
  Label, Zip: ZipHasVec<(HKey, CountUnZipStep<Label>)>
> : FactoryTrait<Label> {
  /// Insert into the cache corresponding to `Zip`.
  #[inline(always)]
  fn cache_insert(& mut self, HKey, usize) ;

  /// Zips on `zip`. Returns a `NYet` of the next ZDD to explore if any,
  /// or `Done` of the top count.
  /// Updates the cache as it goes.
  ///
  /// Whenever it goes up, the current count and the one retrieved from the
  /// zip are **sumed**. Could have the operation has a parameter but I'm not
  /// sure it would be of any use at this point.
  fn zip(
    & mut self, mut count: usize, zip: & mut Zip
  ) -> Res<usize, Zdd<Label>> {
    use self::UnZipStep::* ;
    use self::Res::* ;
    loop {
      count = match zip.pop() {
        // Can't zip up, done.
        None => return Done(count),

        // A right branch hasn't been explored yet.
        Some( (key, Lft((), rgt)) ) => {
          zip.push( (key, Rgt( (), count)) ) ;
          return NYet(rgt)
        },

        // We were in a right branch, going up.
        Some( (key, Rgt((), l_count)) ) => {
          let count = count + l_count ;
          self.cache_insert(key, count) ;
          count
        },
      }
    }
  }
}

pub trait BinaryLabelZipper<
  Label,
  Zip: ZipHasVec<(HKey, HKey, LabelBinZipStep<Label>)>
> : FactoryTrait<Label> {
  /// Insert into the cache corresponding to `Zip`.
  #[inline(always)]
  fn cache_insert(& mut self, HKey, HKey, & Zdd<Label>) ;


  /// Zips on `zip`. Returns a `NYet` of the two next ZDDs to explore if any,
  /// or `Done` of the top ZDD.
  /// Updates the cache as it goes.
  fn zip(
    & mut self, mut zdd: Zdd<Label>, zip: & mut Zip
  ) -> Res<Zdd<Label>, (Zdd<Label>, Zdd<Label>)> {
    use self::BinZipStep::* ;
    use self::Res::* ;
    loop {
      zdd = match zip.pop() {
        // Can't zip up, done.
        None => return Done(zdd),

        // Some right branches haven't been explored yet.
        Some( (l_key, r_key, Lft(lbl, l_rgt, r_rgt)) ) => {
          zip.push( (l_key, r_key, Rgt( lbl, zdd)) ) ;
          return NYet((l_rgt, r_rgt))
        },

        // A right branch hasn't been explored yet, but we're at a terminal
        // left step in the zipper. Going up.
        Some( (l_key, r_key, TLft(lbl, rgt)) ) => {
          let zdd = self.mk_node(lbl, zdd, rgt) ;
          self.cache_insert(l_key, r_key, & zdd) ;
          zdd
        },

        // We were in a right branch, going up.
        Some( (l_key, r_key, Rgt(lbl, lft)) ) => {
          let zdd = self.mk_node(lbl, lft, zdd) ;
          self.cache_insert(l_key, r_key, & zdd) ;
          zdd
        },
      }
    }
  }
}


