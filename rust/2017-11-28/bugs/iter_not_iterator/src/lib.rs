use std::iter::FromIterator;
use std::collections::{HashSet, VecDeque};


fn iter_not_iterator() {
    let d = VecDeque::from_iter(vec!["of", "course", "I", "still", "love", "you"]
                                    .iter()
                                    .map(|s| s.to_string()));
    let h: HashSet<String> = HashSet::from_iter(d);
}
