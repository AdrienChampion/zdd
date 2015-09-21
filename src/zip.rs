use ::Zdd ;
use ::cache::{ UnaryCache, BinaryCache } ;

pub enum UnaryStep<Label> {
  Lft(UnaryCache, Label, Zdd<Label>),
  Rgt(UnaryCache, Label, Zdd<Label>),
}

pub enum BinaryStep<Label> {
  Lft(BinaryCache, Label, Zdd<Label>, Zdd<Label>),
  // Terminal left step.
  TLft(BinaryCache, Label, Zdd<Label>),
  Rgt(BinaryCache, Label, Zdd<Label>),
}

pub struct UnaryZip<Label> (
  pub Label,
  // pub Zdd<Label>,
  pub Vec<UnaryStep<Label>>,
) ;
impl<Label: Copy> UnaryZip<Label> {
  #[inline(always)]
  pub fn mk(
    lbl: & Label,
    // zdd: & Zdd<Label>
  ) -> Self {
    UnaryZip(
      * lbl,
      // zdd.clone(),
      vec![]
    )
  }

  #[inline(always)]
  pub fn lbl(& self) -> & Label { & self.0 }
  // #[inline(always)]
  // pub fn zdd(& self) -> & Zdd<Label> { & self.1 }

  // #[inline(always)]
  // pub fn set_zdd(mut self, zdd: Zdd<Label>) -> Self { self.1 = zdd ; self }

  #[inline(always)]
  pub fn pop(& mut self) -> Option<UnaryStep<Label>> {
    self.1.pop()
  }
  #[inline(always)]
  pub fn push(& mut self, step: UnaryStep<Label>) {
    self.1.push(step)
  }
}

pub struct BinaryZip<Label> (
  // pub Zdd<Label>,
  // pub Zdd<Label>,
  pub Vec<BinaryStep<Label>>,
) ;
impl<Label> BinaryZip<Label> {
  #[inline(always)]
  pub fn mk(
    // lhs: & Zdd<Label>,
    // rhs: & Zdd<Label>
  ) -> Self {
    BinaryZip(
      // lhs.clone(),
      // rhs.clone(),
      vec![]
    )
  }

  // #[inline(always)]
  // pub fn lhs(& self) -> & Zdd<Label> { & self.0 }
  // #[inline(always)]
  // pub fn rhs(& self) -> & Zdd<Label> { & self.1 }

  // #[inline(always)]
  // pub fn set_lhs(mut self, zdd: Zdd<Label>) -> Self { self.0 = zdd ; self }
  // #[inline(always)]
  // pub fn set_rhs(mut self, zdd: Zdd<Label>) -> Self { self.1 = zdd ; self }

  #[inline(always)]
  pub fn pop(& mut self) -> Option<BinaryStep<Label>> {
    self.0.pop()
  }
  #[inline(always)]
  pub fn push(& mut self, step: BinaryStep<Label>) {
    self.0.push(step)
  }
}


pub enum ZipResult<Label, Data> {
  Done(Zdd<Label>),
  NYet(Data)
}