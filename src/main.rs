extern crate zdd ;

use zdd::* ;

fn print_and_wtf(
  zdd: & Zdd<& 'static str>, id: usize, name: & 'static str, dir: & 'static str
) {
  use std::fs::OpenOptions ;
  println!("----------------------------") ;
  println!("> [{:>3}] {}", id, name) ;
  println!("  {}", zdd) ;
  println!("----------------------------") ;
  let file = format!("{}/g_{:0>3}_{}.gv", dir, id, name) ;
  match OpenOptions::new().write(true).create(true).truncate(true).open(
    & file
  ) {
    Ok(mut wrt) => zdd.write_as_gv(& mut wrt).unwrap(),
    Err(e) => panic!("{}", e),
  }
}

#[allow(dead_code)]
pub fn run1() {
  use std::process::Command ;
  let dir = "./graphs_1" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\n|===| Test ZDD 1\n") ;

  println!("Creating factory.") ;
  let mut factory = Factory::<& 'static str>::mk() ;
  let one = factory.one() ;

  let zdd1 = factory.change(& one, & "a") ;
  print_and_wtf(& zdd1, 1, "a", dir) ;

  let zdd2 = factory.change(& one, & "b") ;
  print_and_wtf(& zdd2, 2, "b", dir) ;

  let zdd3 = factory.union(& zdd1, & zdd2) ;
  print_and_wtf(& zdd3, 3, "union_1_2", dir) ;

  let zdd4 = factory.union(& one, & zdd3) ;
  print_and_wtf(& zdd4, 4, "union_one_3", dir) ;

  let zdd5 = factory.onset(& zdd4, & "b") ;
  print_and_wtf(& zdd5, 5, "onset_4_b", dir) ;

  let zdd6 = factory.inter(& zdd2, & zdd4) ;
  print_and_wtf(& zdd6, 6, "inter_2_4", dir) ;

  let zdd7 = factory.minus(& zdd4, & zdd2) ;
  print_and_wtf(& zdd7, 7, "minus_4_2", dir) ;

  let zdd8 = factory.inter(& zdd3, & zdd4) ;
  print_and_wtf(& zdd8, 8, "inter_3_4", dir) ;

  let zdd9 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "offset_4_a", dir) ;

  let zdd10 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd10, 10, "offset_4_a", dir) ;

  let zdd11 = factory.change(& zdd4, & "c") ;
  print_and_wtf(& zdd11, 11, "change_4_c", dir) ;

  let zdd12 = factory.offset(& zdd10, & "b") ;
  print_and_wtf(& zdd12, 12, "offset_10_b", dir) ;

  let zdd13 = factory.offset(& zdd11, & "b") ;
  print_and_wtf(& zdd13, 13, "offset_11_b", dir) ;

  let zdd14 = factory.offset(& zdd11, & "b") ;
  print_and_wtf(& zdd14, 14, "offset_11_b", dir) ;

  let zdd15 = factory.offset(& zdd11, & "c") ;
  print_and_wtf(& zdd15, 15, "offset_11_c", dir) ;

  let zdd16 = factory.offset(& zdd11, & "c") ;
  print_and_wtf(& zdd16, 16, "offset_11_c", dir) ;

  println!("Done.\n") ;

  ()
}

#[allow(dead_code)]
pub fn run2() {
  use std::process::Command ;
  let dir = "./graphs_2" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\n|===| Test ZDD 2\n") ;

  println!("Creating factory.") ;
  let mut factory = Factory::<& 'static str>::mk() ;
  let one = factory.one() ;

  let zdd1 = factory.change(& one, & "a") ;
  print_and_wtf(& zdd1, 1, "a", dir) ;

  let zdd2 = factory.change(& one, & "b") ;
  print_and_wtf(& zdd2, 2, "b", dir) ;

  let zdd3 = factory.union(& zdd1, & zdd2) ;
  print_and_wtf(& zdd3, 3, "1_union_2", dir) ;

  let zdd4 = factory.union(& zdd2, & zdd1) ;
  print_and_wtf(& zdd4, 4, "2_union_1", dir) ;

  let zdd5 = factory.offset(& zdd4, & "b") ;
  print_and_wtf(& zdd5, 5, "4_offset_b", dir) ;

  let zdd6 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd6, 6, "4_offset_a", dir) ;

  let zdd7 = factory.onset(& zdd4, & "b") ;
  print_and_wtf(& zdd7, 7, "4_onset_b", dir) ;

  let zdd8 = factory.onset(& zdd4, & "a") ;
  print_and_wtf(& zdd8, 8, "4_onset_a", dir) ;

  let zdd9 = factory.change(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "4_change_a", dir) ;

  let zdd10 = factory.change(& zdd4, & "b") ;
  print_and_wtf(& zdd10, 10, "4_change_b", dir) ;

  let zdd11 = factory.union(& zdd4, & zdd10) ;
  print_and_wtf(& zdd11, 11, "4_union_10", dir) ;

  let zdd12 = factory.inter(& zdd4, & zdd11) ;
  print_and_wtf(& zdd12, 12, "4_inter_11", dir) ;

  let zdd13 = factory.minus(& zdd11, & zdd1) ;
  print_and_wtf(& zdd13, 13, "11_minus_1", dir) ;

  let zdd14 = factory.minus(& zdd11, & zdd2) ;
  print_and_wtf(& zdd14, 14, "11_minus_2", dir) ;

  let zdd15 = factory.minus(& zdd1, & zdd4) ;
  print_and_wtf(& zdd15, 15, "1_minus_4", dir) ;

  let zdd16 = factory.inter(& zdd11, & zdd10) ;
  print_and_wtf(& zdd16, 16, "11_inter_10", dir) ;

  let zdd17 = factory.union(& zdd2, & one) ;
  print_and_wtf(& zdd17, 17, "2_union_1", dir) ;

  let zdd18 = factory.change(& zdd17, & "a") ;
  print_and_wtf(& zdd18, 18, "17_change_a", dir) ;

  let zdd19 = factory.change(& zdd17, & "a") ;
  print_and_wtf(& zdd19, 19, "17_change_a", dir) ;

  println!("count 1: {}", factory.count(& zdd1)) ;
  println!("count 4: {}", factory.count(& zdd4)) ;
  println!("count 11: {}", factory.count(& zdd11)) ;
  println!("count 11: {}", factory.count(& zdd11)) ;

  ()

}

// #[allow(dead_code)]
// fn run_frac() {
//   use zdd::poly::Frac ;

//   println!("\n|===| Test frac\n") ;

//   fn test(num: isize, den: usize) -> Option<Frac> {
//     print!("| {:^3}/{:^3} = ", num, den) ;
//     match Frac::of(num,den) {
//       None => { println!("NaN") ; None },
//       Some(f) => { println!("{}", f) ; Some(f) }
//     }
//   }

//   fn test_fun<F: Fn(Frac, Frac) -> Frac>(
//     lhs: Frac, rhs: Frac, f: F, op: & str
//   ) -> Frac {
//     let res = f(lhs, rhs) ;
//     println!("| {:^7} {} {:^7} = {}", lhs, op, rhs, res) ;
//     res
//   }

//   fn test_add(lhs: Frac, rhs: Frac) -> Frac {
//     test_fun(lhs, rhs, |lhs,rhs| lhs + rhs, "+")
//   }
//   fn test_sub(lhs: Frac, rhs: Frac) -> Frac {
//     test_fun(lhs, rhs, |lhs,rhs| lhs - rhs, "-")
//   }
//   fn test_mul(lhs: Frac, rhs: Frac) -> Frac {
//     test_fun(lhs, rhs, |lhs,rhs| lhs * rhs, "*")
//   }
//   fn test_div(lhs: Frac, rhs: Frac) -> Frac {
//     test_fun(lhs, rhs, |lhs,rhs| (lhs / rhs).unwrap(), "/")
//   }

//   test(3, 0).is_none() ;
//   test(0, 0).is_none() ;
//   let _frac_0 = test(14, 3).unwrap() ;
//   let _frac_1 = test(-7, 9).unwrap() ;
//   let _frac_2 = test(0, 2).unwrap() ;
//   let _frac_3 = test(14, 49).unwrap() ;
//   let _frac_4 = test(1, 14).unwrap() ;

//   let _frac_5 = test_add(_frac_3, _frac_4) ;
//   let _frac_6 = test_div(_frac_0, _frac_3) ;
//   let _frac_7 = test_mul(_frac_0, _frac_4) ;
//   let _frac_8 = test_sub(_frac_5, _frac_7) ;
//   let _frac_9 = test_mul(_frac_2, _frac_3) ;

//   println!("") ;

//   ()
// }

// macro_rules! poly_println {
//   ($string:expr, $poly:expr) => (
//     {
//       print!($string) ;
//       let mut out = io::stdout() ;
//       let poly = & $poly ;
//       poly.write(& mut out).unwrap() ;
//       println!("") ;
//     }
//   ) ;
//   ($string:expr, $($params:expr),+ => $poly:expr) => (
//     {
//       print!($string, $($params),+) ;
//       let mut out = io::stdout() ;
//       let poly = & $poly ;
//       poly.write(& mut out).unwrap() ;
//       println!("") ;
//     }
//   ) ;
// }

// #[allow(dead_code)]
// fn run_poly() {
//   use zdd::poly::* ;
//   use ::std::io::{ self, Write } ;

//   println!("\n|===| Test poly\n") ;

//   let mut factory = PolyFactory::<Frac, & 'static str, Frac>::mk() ;
//   let zero = factory.zero() ;
//   poly_println!("zero: ", zero) ;
//   let frac = Frac::of(-42, 5).unwrap() ;
//   let something = factory.cst(frac) ;
//   poly_println!("cst({}): ", frac => something) ;
//   println!("zdd: {}", something) ;

//   println!("") ;
// }

#[allow(unused)]
fn main() {
  run1() ;
  run2() ;
  // run_frac() ;
  // run_poly() ;
  println!("")
}