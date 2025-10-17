use std::ops::Range;
use criterion::*;

use hyperon_atom::*;
use hyperon_space::index::*;

struct DataGenerator {
    range: Vec<Atom>,
    sequence: usize,
    atom: Vec<Atom>,
}

impl DataGenerator {
    fn new(n: usize, range: Range<char>) -> Self {
        let mut r = Vec::with_capacity(range.size_hint().0);
        for c in range {
            r.push(Atom::sym(c.to_string()));
        }
        let range_size = r.len();
        let start = range_size.pow(n as u32);
        let c = r[0].clone();
        Self {
            range: r,
            sequence: start,
            atom: vec![c; n],
        }
    }
}

impl Iterator for DataGenerator {
    type Item = Atom;

    fn next(&mut self) -> Option<Self::Item> {
        if self.sequence == 0 {
            None
        } else {
            self.sequence -= 1;
            let mut sequence = self.sequence;
            for i in 0..self.atom.len() {
                let idx = sequence % self.range.len();
                sequence = sequence / self.range.len();
                self.atom[i] = self.range[idx].clone();
            }
            Some(Atom::expr(self.atom.clone()))
        }
    }
}

fn fill(c: &mut Criterion) {
    c.bench_function("fill 100", |b| {
        b.iter_batched(|| {
            let gen = DataGenerator::new(2, 'a'..'k');
            let idx = AtomIndex::<NoDuplication>::new();
            (gen, idx)
        },
        |(gen, mut idx)| {
            let mut i = 0;
            gen.for_each(|a| {
                idx.insert(a);
                i += 1;
            });
            assert_eq!(i, 100);
            idx
        },
        BatchSize::SmallInput)
    });
}

criterion_group!(benches, fill);
criterion_main!(benches);
