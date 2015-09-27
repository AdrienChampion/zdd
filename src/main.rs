extern crate zdd ;

use zdd::* ;


fn print_and_wtf(
  zdd: & Zdd<& 'static str>, id: usize, name: & 'static str, dir: & 'static str
) {
  println!("----------------------------") ;
  println!("\n> [{:>3}] {}", id, name) ;
  zdd.print("".to_string()) ;
  println!("----------------------------") ;
  zdd.graph_to_file(& format!("{}/g_{:0>3}_{}", dir, id, name)).unwrap() ;
}

pub fn run() {
  use std::process::Command ;
  let dir = "./graphs_1" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\nCreating factory.") ;
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

  let zdd9 = factory.offset(& zdd4, & "a") ;
  print_and_wtf(& zdd9, 9, "offset_4_a", dir) ;

  let zdd10 = factory.change(& zdd4, & "c") ;
  print_and_wtf(& zdd10, 10, "change_4_c", dir) ;

  let zdd11 = factory.offset(& zdd10, & "b") ;
  print_and_wtf(& zdd11, 11, "offset_10_b", dir) ;
  println!("Done.\n") ;

  ()
}


pub fn run2() {
  use std::process::Command ;
  let dir = "./graphs_2" ;

  // Creating graph directory.
  let _ = Command::new("mkdir").arg("-p").arg(dir).output().unwrap() ;

  println!("\nCreating factory.") ;
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

  println!("count 1: {}", factory.count(& zdd1)) ;
  println!("count 4: {}", factory.count(& zdd4)) ;
  println!("count 11: {}", factory.count(& zdd11)) ;
  println!("count 11: {}", factory.count(& zdd11)) ;

  ()

}

fn main() {
  run() ;
  run2() ;
  ()
}