#[derive(Clone)]
pub struct ListMap<K, V> {
    list: Vec<(K, V)>,
}

impl<K: PartialEq, V> ListMap<K, V> {
    pub fn new() -> Self {
        Self{ list: vec![] }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for (k, v) in &self.list {
            if *k == *key {
                return Some(&v)
            }
        }
        None
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.list.push((key, value))
    }

    pub fn clear(&mut self) {
        self.list.clear()
    }
}

