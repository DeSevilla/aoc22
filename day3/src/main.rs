use std::fs;

const ALPHABET: &[u8] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".as_bytes();
const ALPHANUMERIC: &[u8] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".as_bytes();


struct AlphaSet {
    set: u64,
}

impl AlphaSet {
    fn new() -> AlphaSet {
        AlphaSet { set: 0 }
    }

    fn new_full() -> AlphaSet {
        AlphaSet { set: !0 }
    }

    fn from_string(s: &str) -> AlphaSet {
        let mut result = AlphaSet { set: 0 };
        result.add_string(s);
        result
    }

    fn byte_to_index(c: u8) -> u8 {
        assert!(c.is_ascii_alphanumeric());
        if c.is_ascii_lowercase() { 
            c - 'a' as u8 
        } 
        else if c.is_ascii_uppercase() { 
            c - 'A' as u8 + 26 
        } else { //must be numeric
            assert!(c.is_ascii_digit());
            c - '0' as u8 + 52 
        }
    }

    fn add(&mut self, c: u8) {
        self.set |= 1 << Self::byte_to_index(c);
        // println!("added {}, set: {}", c as char, self.as_string());
    }

    fn add_string(&mut self, s: &str) {
        for c in s.chars() {
            self.add(c as u8);
        }
    }

    fn clear(&mut self) {
        self.set = 0;
    }

    fn intersect(&self, other: &AlphaSet) -> AlphaSet {
        AlphaSet { set: self.set & other.set }
    }

    fn union(&self, other: &AlphaSet) -> AlphaSet {
        AlphaSet { set: self.set | other.set }
    }

    fn live_indices(&self) -> Vec<i32> {
        let mut result = Vec::new();
        // println!("set: {:64b}", self.set);
        for i in 0..52 {
            if (self.set & 1 << i) > 0 {
                // println!("i: {i}");
                result.push(i);
            }
        }
        result
    }

    fn as_string(&self) -> String {
        let mut result = String::new();
        for idx in self.live_indices() {
            result.push(ALPHABET[idx as usize] as char);
        }
        result
    }
}

fn main() {
    for b in ALPHANUMERIC {
        println!("{} => {}", *b as char, AlphaSet::byte_to_index(*b));
    }
    let contents = fs::read_to_string("input/day3.txt").expect("Should have been able to open file");
    // let mut set_first = AlphaSet::new();
    // let mut set_second = AlphaSet::new();
    let mut total_priority = 0;
    let mut saved_rows = Vec::new();
    for row in contents.split("\r\n") {
        saved_rows.push(row);
        if saved_rows.len() == 3 {
            let mut result_set = AlphaSet::new_full();
            for sr in &saved_rows {
                result_set = result_set.intersect(&AlphaSet::from_string(sr));
            }
            for index in result_set.live_indices() {
                // println!("Shared character among {:?} is {} (priority {})", saved_rows, ALPHABET[index as usize] as char, index + 1);
                total_priority += index + 1;
            }
            saved_rows.clear();
        }
    //     let (first, second) = row.split_at(row.len() / 2);
    //     // println!("split into ({first}, {second}) ({}, {})", first.len(), second.len());
    //     set_first.add_string(first);
    //     set_second.add_string(second);
    //     // println!("1: {}", set_first.as_string());
    //     // println!("2: {}", set_second.as_string());
    //     let priorities = set_first.intersect(&set_second).live_indices();
    //     for prio in &priorities {
    //         // println!("c: {}", ALPHABET[*prio as usize] as char);
    //         total_priority += prio + 1;
    //     }
    //     // println!("l: {}", priorities.len());
    //     assert!(priorities.len() == 1);
    //     set_first.clear();
    //     set_second.clear();
    }
    println!("Total priority: {total_priority}");
}