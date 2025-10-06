use std::env;
use std::fs::File;
use std::io::BufReader;
use std::time::{SystemTime, Duration};
use ra_ap_profile::memory_usage;

use hyperon_atom::*;
use hyperon::metta::text::*;
use hyperon::space::grounding::*;

#[inline]
fn now() -> SystemTime {
    SystemTime::now()
}

#[inline]
fn since(time: SystemTime) -> Duration {
    SystemTime::now().duration_since(time).unwrap()
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    println!("args passed: {:?}", args);
    let filename = match args.get(1) {
        Some(filename) => filename,
        None => return Err(format!("Please specify MeTTa file as a first argument")),
    };
    let open_error = |err| { format!("Cannot open file: {}, because of error: {}", filename, err) };
    let file = BufReader::new(File::open(filename).map_err(open_error)?);

    let mut parser = SExprParser::new(file);
    let tokenizer = Tokenizer::new();
    let mut space = GroundingSpace::new();

    let before = memory_usage().allocated;
    let start = now();
    loop {
        match parser.parse(&tokenizer)? {
            Some(atom) => space.add(atom),
            None => break,
        }
    }
    let duration = since(start);
    let after = memory_usage().allocated;
    println!("loading time {:?}", duration);
    println!("memory usage: {}", after - before);

    let query = match args.get(2) {
        Some(query) => SExprParser::new(query).parse(&tokenizer)?
            .expect(format!("Incorrect atom: {}", query).as_str()),
        None => expr!("no_match"),
    };

    let start = now();
    let result = space.query(&query);
    let duration = since(start);
    println!("{} -> {}, time {:?}", query, result, duration);

    // FILE: gaf/edges.metta
    // QUERY: (go_gene_product (ontology_term GO:0002377) (protein A0A075B6H8))
    //use hyperon::space::grounding::index::storage::AtomStorage;
    //use hyperon::space::grounding::index::trie::{AllowDuplication, AtomTrie, AtomTrieNode, AtomTrieNodeContent};

    //println!("Atom size {}", std::mem::size_of::<Atom>());
    //println!("AtomTrieNode size {}", std::mem::size_of::<AtomTrieNode>());
    //println!("AtomTrieNodeContent size {}", std::mem::size_of::<AtomTrieNodeContent<AllowDuplication>>());

    //println!("atom storage count: {}", space.index.storage.count());
    //let mut storage = AtomStorage::default();
    //let before = memory_usage().allocated;
    //std::mem::swap(&mut space.index.storage, &mut storage);
    //drop(storage);
    //let after = memory_usage().allocated;
    //println!("atom storage mem: {}", before - after);

    //println!("atom index node count: {:?}", space.index.trie.stats());
    //let mut trie = AtomTrie::default();
    //let before = memory_usage().allocated;
    //std::mem::swap(&mut space.index.trie, &mut trie);
    //drop(trie);
    //let after = memory_usage().allocated;
    //println!("atom index mem: {}", before - after);

    //println!("{}", space.query(&query));

    Ok(())
}
