`zdd` is a Zero-suppressed binary Decision Diagram library in Rust.

A ZDD library, based on [this paper by Shin-Ichi Minato][zdd paper].

For more details see [the documentation](doc).

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
zdd = "0.1.0"
```

and this to your crate root:

```rust
extern crate zdd ;
```

[zdd paper]: http://link.springer.com/article/10.1007%2Fs100090100038 (Zero-suppressed BDDs and their applications)
[doc]: http://adrienchampion.bitbucket.org/zdd/zdd/ (zdd documentation)