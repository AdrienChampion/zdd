use std::fmt ;

use ::Zdd ;

use self::UnaryCache::* ;
use self::BinaryCache::* ;

pub type HKey = u64 ;
pub type UnaryKey<Info> = (HKey, Info) ;
pub type BinaryKey = (HKey, HKey) ;

pub enum UnaryCache {
  Offset(HKey),
  Onnset(HKey),
  Change(HKey),
}
impl fmt::Display for UnaryCache {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    match * self {
      Offset(key) => write!(fmt, "OffsetCache({})", key),
      Onnset(key) => write!(fmt, "OnsetCache({})", key),
      Change(key) => write!(fmt, "ChangeCache({})", key),
    }
  }
}

pub enum BinaryCache {
  Union(HKey, HKey),
  Inter(HKey, HKey),
  Minus(HKey, HKey),
}
impl fmt::Display for BinaryCache {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    match * self {
      Union(l_key,r_key) => write!(fmt, "UnionCache({},{})", l_key, r_key),
      Inter(l_key,r_key) => write!(fmt, "InterCache({},{})", l_key, r_key),
      Minus(l_key,r_key) => write!(fmt, "MinusCache({},{})", l_key, r_key),
    }
  }
}

#[inline(always)]
pub fn offset<Label>(zdd: & Zdd<Label>) -> UnaryCache {
  Offset( zdd.hkey() )
}

#[inline(always)]
pub fn onnset<Label>(zdd: & Zdd<Label>) -> UnaryCache {
  Onnset( zdd.hkey() )
}

#[inline(always)]
pub fn change<Label>(zdd: & Zdd<Label>) -> UnaryCache {
  Change( zdd.hkey() )
}

#[inline(always)]
pub fn union<Label>(lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> BinaryCache {
  Union( lhs.hkey(), rhs.hkey() )
}

#[inline(always)]
pub fn inter<Label>(lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> BinaryCache {
  Inter( lhs.hkey(), rhs.hkey() )
}

#[inline(always)]
pub fn minus<Label>(lhs: & Zdd<Label>, rhs: & Zdd<Label>) -> BinaryCache {
  Minus( lhs.hkey(), rhs.hkey() )
}