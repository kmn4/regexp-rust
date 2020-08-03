#[derive(PartialEq, Eq)]
enum Token {
    Char(char),
    Bar,
    Star,
    LPar,
    RPar,
    EOL,
}

#[derive(Debug)]
// Simple regular expressions.
// Regular expressions (regexp) are built as follows:
// (1) `empty`, `epsilon`, a single character are regexp
// (2) If r1 and r2 are regexps, then
//     a concatination `r1 r2`, disjunction `r1 | r2`,
//     and star closure `r1*` are regexps.
enum RegExp {
    Empty,
    Eps,
    Char(char),
    Or(Box<RegExp>, Box<RegExp>),
    Cat(Box<RegExp>, Box<RegExp>),
    Star(Box<RegExp>),
}

impl RegExp {
    // Perform optimization as follows:
    // empty | r => r
    // eps r => r
    // empty r => empty
    fn opt(re: RegExp) -> RegExp {
        // optimize when subexpressions are optimzed
        fn aux(re: RegExp) -> RegExp {
            match re {
                RegExp::Or(r1, r2) if *r1 == RegExp::Empty => *r2,
                RegExp::Or(r1, r2) if *r2 == RegExp::Empty => *r1,
                RegExp::Cat(r1, r2) if *r1 == RegExp::Eps => *r2,
                RegExp::Cat(r1, r2) if *r2 == RegExp::Eps => *r1,
                RegExp::Cat(r1, _) if *r1 == RegExp::Empty => RegExp::Empty,
                RegExp::Cat(_, r2) if *r2 == RegExp::Empty => RegExp::Empty,
                _ => re,
            }
        }
        match re {
            RegExp::Or(r1, r2) => {
                let r1 = RegExp::opt(*r1);
                let r2 = RegExp::opt(*r2);
                aux(RegExp::Or(Box::new(r1), Box::new(r2)))
            }
            RegExp::Cat(r1, r2) => {
                let r1 = RegExp::opt(*r1);
                let r2 = RegExp::opt(*r2);
                aux(RegExp::Cat(Box::new(r1), Box::new(r2)))
            }
            RegExp::Star(r) => {
                let r = RegExp::opt(*r);
                aux(RegExp::Star(Box::new(r)))
            }
            _ => re,
        }
    }
}

impl PartialEq for RegExp {
    fn eq(&self, other: &Self) -> bool {
        use RegExp::*;
        match (self, other) {
            (Empty, Empty) | (Eps, Eps) => true,
            (Char(c), Char(d)) if c == d => true,
            (Or(r11, r12), Or(r21, r22)) if r11 == r21 && r12 == r22 => true,
            (Cat(r11, r12), Cat(r21, r22)) if r11 == r21 && r12 == r22 => true,
            (Star(r1), Star(r2)) if r1 == r2 => true,
            _ => false,
        }
    }
}
impl Eq for RegExp {}

// Only works on strings over ASCII characters
struct Tokenizer {
    input: String,
}

impl Tokenizer {
    fn with(input: String) -> Tokenizer {
        let mut v = input.into_bytes();
        v.reverse();
        let rev = String::from_utf8(v);
        if let Ok(rev) = rev {
            Tokenizer { input: rev }
        } else {
            // TODO: Fail
            Tokenizer {
                input: String::from(""),
            }
        }
    }
    fn next(&mut self) -> Token {
        match self.input.pop() {
            None => Token::EOL,
            Some('|') => Token::Bar,
            Some('*') => Token::Star,
            Some('(') => Token::LPar,
            Some(')') => Token::RPar,
            Some(c) => Token::Char(c),
        }
    }
}

struct Parser {
    token: Token,
    tokenizer: Tokenizer,
}

use Token::*;
impl Parser {
    fn with(mut tokenizer: Tokenizer) -> Parser {
        let token = tokenizer.next();
        Parser { token, tokenizer }
    }
    fn parse(mut self) -> RegExp {
        self.parse_i()
    }
    fn parse_opt(self) -> RegExp {
        let re = self.parse();
        RegExp::opt(re)
    }
    fn advance(&mut self) {
        self.token = self.tokenizer.next();
    }
    fn eat(&mut self, t: Token) {
        if self.token == t {
            self.advance();
        }
    }
    fn parse_i(&mut self) -> RegExp {
        match self.token {
            Char(_) | LPar | EOL => {
                let r = self.parse_o();
                self.eat(EOL);
                r
            }
            _ => panic!("Unexpected token"),
        }
    }
    fn parse_o(&mut self) -> RegExp {
        match self.token {
            Char(_) | LPar => {
                let c = self.parse_c();
                self.parse_oprime(vec![Box::new(c)])
            }
            EOL => RegExp::Eps,
            _ => panic!("Unexpected token"),
        }
    }
    fn parse_oprime(&mut self, mut cs: Vec<Box<RegExp>>) -> RegExp {
        match self.token {
            Bar => {
                self.eat(Bar);
                let c = self.parse_c();
                cs.push(Box::new(c));
                self.parse_oprime(cs)
            }
            RPar | EOL => cs
                .into_iter()
                .fold(RegExp::Empty, |re, c| RegExp::Or(Box::new(re), c)),
            _ => panic!("Unexpected token"),
        }
    }
    fn parse_c(&mut self) -> RegExp {
        match self.token {
            Char(_) | LPar => {
                let s = self.parse_s();
                self.parse_cprime(vec![Box::new(s)])
            }
            _ => panic!("Unexpected token"),
        }
    }
    fn parse_cprime(&mut self, mut ss: Vec<Box<RegExp>>) -> RegExp {
        match self.token {
            Char(_) | LPar => {
                let s = self.parse_s();
                ss.push(Box::new(s));
                self.parse_cprime(ss)
            }
            Bar | RPar | EOL => ss
                .into_iter()
                .fold(RegExp::Eps, |re, s| RegExp::Cat(Box::new(re), s)),
            _ => panic!("Unexpected token"),
        }
    }
    fn parse_s(&mut self) -> RegExp {
        match self.token {
            Char(_) | LPar => {
                let u = self.parse_u();
                match self.token {
                    Star => {
                        self.eat(Star);
                        RegExp::Star(Box::new(u))
                    }
                    _ => u,
                }
            }
            _ => panic!("Unexpected token"),
        }
    }
    fn parse_u(&mut self) -> RegExp {
        match self.token {
            Char(c) => {
                self.eat(Char(c));
                RegExp::Char(c)
            }
            LPar => {
                self.eat(LPar);
                let re = self.parse_o();
                self.eat(RPar);
                re
            }
            _ => panic!("Unexpected token"),
        }
    }
}

fn parse_str_opt(s: &str) -> RegExp {
    Parser::with(Tokenizer::with(String::from(s))).parse_opt()
}

use std::cell::RefCell;
use std::rc::Rc;
// Each tree in union-find data structures.
#[derive(Debug)]
struct UFTree<Q>
where
    Q: std::hash::Hash + Eq,
{
    val: Q,
    parent: Option<Rc<RefCell<UFTree<Q>>>>,
    rank: u32,
}
impl<Q> UFTree<Q>
where
    Q: std::cmp::Eq + std::hash::Hash,
{
    fn root(t: &Rc<RefCell<UFTree<Q>>>) -> Rc<RefCell<UFTree<Q>>> {
        let mut res = Rc::clone(t);
        loop {
            let mut changed = false;
            res = {
                let r = res.borrow();
                if let Some(ref s) = r.parent {
                    changed = true;
                    Rc::clone(s)
                } else {
                    Rc::clone(&res)
                }
            };
            if !changed {
                break;
            }
        }
        res
    }
}
// Data structure that can deal with equivalent classes.
// It supports two operations `union` and `find`.
// Each class is represented by a tree and two of that can be
// merged to make a single class by `union` operation.
// Given a element q, `find` finds the representative of
// its equivalent class and returns it.
#[derive(Debug)]
struct UnionFind<Q>
where
    Q: std::hash::Hash + Eq,
{
    m: HashMap<Q, Rc<RefCell<UFTree<Q>>>>,
}
impl<Q> UnionFind<Q>
where
    Q: std::hash::Hash + Eq + Copy,
{
    // Creates a new union-find structure that deal with
    // elements of a given vector.
    fn from(qs: Vec<Q>) -> UnionFind<Q> {
        let m = qs
            .into_iter()
            .map(|q| {
                (
                    q,
                    Rc::new(RefCell::new(UFTree {
                        val: q,
                        parent: None,
                        rank: 0,
                    })),
                )
            })
            .collect();
        UnionFind { m }
    }
    fn find_root(&self, q: Q) -> Rc<RefCell<UFTree<Q>>> {
        let t = &self.m[&q];
        UFTree::root(&t)
    }
    fn find(&self, q: Q) -> Q {
        let t = self.find_root(q);
        let t = t.borrow();
        t.val
    }
    fn union(&self, q1: Q, q2: Q) {
        let r1 = &self.find_root(q1);
        let r2 = &self.find_root(q2);
        if r1.borrow().val == r2.borrow().val {
            return;
        }
        let mut c1 = r1.borrow_mut();
        let mut c2 = r2.borrow_mut();
        if c1.rank > c2.rank {
            c2.parent = Some(Rc::clone(r1));
        } else if c1.rank < c2.rank {
            c1.parent = Some(Rc::clone(r2));
        } else {
            c1.rank += 1;
            c2.parent = Some(Rc::clone(r1));
        }
    }
}
use std::collections::{HashMap, HashSet};
#[derive(Debug)]
struct DFA<Q>
where
    Q: std::hash::Hash + Eq + Copy + Ord,
{
    trans: HashMap<(Q, char), Q>,
    initial: Q,
    finals: HashSet<Q>,
}

impl<Q> DFA<Q>
where
    Q: std::hash::Hash + Eq + Copy + Ord,
{
    fn accepts(&self, s: &str) -> bool {
        self.finals.contains(&self.transition(s))
    }
    fn transition(&self, s: &str) -> Q {
        self.transition_from(self.initial, s)
    }
    fn transition_from(&self, mut q: Q, s: &str) -> Q {
        for c in s.chars() {
            if let Some(r) = self.trans.get(&(q, c)) {
                q = *r
            }
        }
        q
    }
    fn intersection<R>(d1: DFA<Q>, d2: DFA<R>) -> DFA<(Q, R)>
    where
        R: std::hash::Hash + Eq + Copy + Ord,
    {
        let mut finals = HashSet::new();
        for p in d1.finals.iter() {
            for q in d2.finals.iter() {
                finals.insert((*p, *q));
            }
        }
        let trans = HashMap::new();
        DFA {
            initial: (d1.initial, d2.initial),
            finals,
            trans,
        }
    }
    // Two states p and q are "equivalent" when the following holds:
    // for all string w, the DFA transitions to
    // state s (resp. t) from p (resp. q) by w,
    // then both of s and t are final state or not final state.
    // This equivalence relation can be used to minimize
    // the number of states of given DFA.
    fn minimized(&self) -> DFA<Q> {
        // (1) Two states p and q are NOT equivalent in the above sense
        // if p is a final state and q is not.
        let states: HashSet<_> = self.trans.iter().map(|((p, _), _)| *p).collect();
        let states: Vec<_> = states.into_iter().collect();
        let n = states.len();
        let mut equiv = HashMap::with_capacity(n * n);
        for &p in &states {
            for &q in &states {
                let bp = self.finals.contains(&p);
                let bq = self.finals.contains(&q);
                let b = (bp && bq) || (!bp && !bq);
                equiv.insert((p, q), b);
            }
        }
        // (2) If t and s are not equivalent to each other,
        // and if p gets to t and q to s by some alphabet a,
        // then p is not equivalent to q.
        let alpha: Vec<char> = self.trans.iter().map(|((_, a), _)| *a).collect();
        loop {
            let mut changed = false;
            for &p in &states {
                for &q in &states {
                    if !equiv[&(p, q)] {
                        continue;
                    }
                    for &a in &alpha {
                        let t = self.trans[&(p, a)];
                        let s = self.trans[&(q, a)];
                        if !equiv[&(t, s)] {
                            changed = true;
                            equiv.insert((p, q), false);
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }
        // Construct a minimum (quotient) automaton.
        // New states are equivalent classes of
        // quotient of old states by the above relation.
        // Here we take representatives of each classes.
        let uf = UnionFind::from(states.clone());
        for &p in &states {
            for &q in &states {
                if equiv[&(p, q)] {
                    uf.union(p, q);
                }
            }
        }
        let representatives: Vec<_> = states.into_iter().map(|q| uf.find(q)).collect();
        // A new transition function maps a pair (p, a)
        // to the representative of image of (p, a) by the old function.
        let mut trans = HashMap::with_capacity(representatives.len() * alpha.len());
        for &p in &representatives {
            for &a in &alpha {
                trans.insert((p, a), uf.find(self.trans[&(p, a)]));
            }
        }
        DFA {
            trans,
            initial: uf.find(self.initial),
            finals: self.finals.iter().map(|&p| uf.find(p)).collect(),
        }
    }
}

#[derive(Debug)]
struct NFA<Q> {
    alpha: Vec<char>,
    trans: HashMap<(Q, char), HashSet<Q>>,
    initial: Q,
    finals: HashSet<Q>,
}

impl<Q> NFA<Q>
where
    Q: std::hash::Hash + Eq + Copy + Ord + Clone,
{
    // Convert NFA to DFA (subset construction).
    fn to_dfa(&self) -> DFA<u32> {
        use std::collections::BTreeSet as BSet;
        use std::iter::FromIterator;
        let mut i: u32 = 0;
        let mut fresh_state = move || {
            let j = i;
            i += 1;
            j
        };
        // Maps a set of old states representing a new state
        // to some number; and these numbers become new states.
        let mut new_states = HashMap::new();
        let mut initial = BSet::new();
        initial.insert(self.initial);
        // A vector of sets of old states whose elements
        // are not yet used to construct new states.
        let mut srcs = vec![initial.clone()];
        new_states.insert(initial, fresh_state());
        let mut trans = HashMap::new();
        // Construct new states and transition function
        // by repeatedly transition all the elements of
        // the head of srcs by each alphabet.
        while let Some(src) = srcs.pop() {
            let i = new_states[&src];
            for &a in self.alpha.iter() {
                let mut dst = BSet::new(); // states reachable from src by a
                for &p in src.iter() {
                    if let Some(qs) = self.trans.get(&(p, a)) {
                        for &q in qs {
                            dst.insert(q);
                        }
                    }
                }
                if let Some(&j) = new_states.get(&dst) {
                    trans.insert((i, a), j);
                } else {
                    // If constructed set of states is fresh one,
                    // then map it to fresh number.
                    let j = fresh_state();
                    new_states.insert(dst.clone(), j);
                    trans.insert((i, a), j);
                    srcs.push(dst);
                }
            }
        }
        // A new state (set of old states) is in new final states iff
        // its intersection with old final states is nonempty.
        let mut finals = HashSet::new();
        let old_finals = BSet::from_iter(self.finals.clone().into_iter());
        for (ss, &i) in new_states.iter() {
            let mut intersection = ss.intersection(&old_finals);
            if let Some(_) = intersection.next() {
                finals.insert(i);
            }
        }
        DFA {
            trans,
            initial: 0,
            finals,
        }
    }
}

#[derive(Debug)]
struct ENFA<Q> {
    states: Vec<Q>,
    alpha: Vec<char>,
    trans: HashMap<(Q, Option<char>), HashSet<Q>>,
    initial: Q,
    finals: HashSet<Q>,
}

impl<Q> ENFA<Q>
where
    Q: std::hash::Hash + std::cmp::Eq + Copy,
{
    fn to_nfa(&self) -> NFA<Q> {
        let mut eclos: HashMap<Q, HashSet<Q>> = HashMap::new();
        // calculate epsilon closure using Warshall-Froyd
        let l = self.states.len();
        let mut d: HashMap<(Q, Q), bool> = HashMap::with_capacity(l * l);
        for &p in &self.states {
            let mut reachables = HashSet::new();
            if let Some(qs0) = self.trans.get(&(p, None)) {
                reachables = qs0.clone();
            }
            reachables.insert(p);
            for &q in &self.states {
                d.insert((p, q), reachables.contains(&q));
            }
        }
        for &r in &self.states {
            for &p in &self.states {
                for &q in &self.states {
                    let pq = d[&(p, q)];
                    d.insert((p, q), pq || (d[&(p, r)] && d[&(r, q)]));
                }
            }
        }
        for &p in &self.states {
            let mut from_p = HashSet::new();
            for &q in &self.states {
                if d[&(p, q)] {
                    from_p.insert(q);
                }
            }
            eclos.insert(p, from_p);
        }
        // For each p in states, new transition function allows it
        // to transition by alphabet a to state q iff
        // there exist some state r s.t. p -> r by eps and r -> q by a.
        let mut trans = HashMap::new();
        for &p in &self.states {
            let rs = &eclos[&p];
            for &a in &self.alpha {
                let mut dst = HashSet::new();
                for r in rs.clone() {
                    if let Some(qs) = self.trans.get(&(r, Some(a))) {
                        dst = dst.union(&qs).map(|q| q.clone()).collect();
                    }
                }
                trans.insert((p, a), dst.clone());
            }
        }
        // State p is in a set of new final states iff
        // there exists old final state q to which p can eps-transition.
        let mut finals = HashSet::new();
        for &p in &self.states {
            let qs = eclos[&p].clone();
            if let Some(_) = qs.intersection(&self.finals).next() {
                finals.insert(p);
            }
        }
        NFA {
            alpha: self.alpha.clone(),
            trans,
            initial: self.initial.clone(),
            finals,
        }
    }
}

impl ENFA<()> {
    fn from_regexp(re: &RegExp) -> ENFA<u32> {
        use RegExp::*;
        fn aux<T>(re: &RegExp, fresh_state: &mut T) -> ENFA<u32>
        where
            T: FnMut() -> u32,
        {
            fn trans_from(
                v: Vec<((u32, Option<char>), Vec<u32>)>,
            ) -> HashMap<(u32, Option<char>), HashSet<u32>> {
                v.into_iter()
                    .map(|(k, v)| (k, v.into_iter().collect()))
                    .collect()
            }
            match re {
                Empty => {
                    let p = fresh_state();
                    ENFA {
                        alpha: vec![],
                        states: vec![p],
                        trans: HashMap::new(),
                        initial: p,
                        finals: HashSet::new(),
                    }
                }
                Eps => {
                    let p = fresh_state();
                    ENFA {
                        alpha: vec![],
                        states: vec![p],
                        trans: HashMap::new(),
                        initial: p,
                        finals: vec![p].into_iter().collect(),
                    }
                }
                &Char(c) => {
                    let (p, q) = (fresh_state(), fresh_state());
                    ENFA {
                        alpha: vec![c].into_iter().collect(),
                        states: vec![p, q],
                        trans: trans_from(vec![((p, Some(c)), vec![q])]),
                        initial: p,
                        finals: vec![q].into_iter().collect(),
                    }
                }
                Or(r1, r2) => {
                    let n1 = aux(r1, fresh_state);
                    let mut n2 = aux(r2, fresh_state);
                    let p = fresh_state();
                    let states = {
                        let mut states = n1.states;
                        states.append(&mut n2.states);
                        states.push(p);
                        states
                    };
                    let alpha = {
                        let mut alpha = n1.alpha;
                        alpha.append(&mut n2.alpha);
                        alpha
                    };
                    let finals = {
                        let finals = n1.finals;
                        let union = finals.union(&mut n2.finals);
                        union.into_iter().map(|&p| p).collect()
                    };
                    let trans = {
                        let i1 = n1.initial;
                        let i2 = n2.initial;
                        let mut trans = trans_from(vec![((p, None), vec![i1, i2])]);
                        for (k, v) in n1.trans {
                            trans.insert(k, v);
                        }
                        for (k, v) in n2.trans {
                            trans.insert(k, v);
                        }
                        trans
                    };
                    ENFA {
                        states,
                        alpha,
                        trans,
                        initial: p,
                        finals,
                    }
                }
                Cat(r1, r2) => {
                    let n1 = aux(r1, fresh_state);
                    let mut n2 = aux(r2, fresh_state);
                    let states = {
                        let mut states = n1.states;
                        states.append(&mut n2.states);
                        states
                    };
                    let alpha = {
                        let mut alpha = n1.alpha;
                        alpha.append(&mut n2.alpha);
                        alpha
                    };
                    let finals = n2.finals;
                    let trans = {
                        let i2 = n2.initial;
                        let mut trans: HashMap<_, HashSet<_>> = n1
                            .finals
                            .iter()
                            .map(|&p| ((p, None), vec![i2].into_iter().collect()))
                            .collect();
                        for (k, v) in n1.trans {
                            trans.entry(k).and_modify(|s| s.extend(&v)).or_insert(v);
                        }
                        for (k, v) in n2.trans {
                            trans.entry(k).and_modify(|s| s.extend(&v)).or_insert(v);
                        }
                        trans
                    };
                    ENFA {
                        states,
                        alpha,
                        trans,
                        initial: n1.initial,
                        finals,
                    }
                }
                Star(r) => {
                    let n = aux(r, fresh_state);
                    let p = fresh_state();
                    let trans = {
                        let mut trans = n.trans;
                        trans.insert((p, None), vec![n.initial].into_iter().collect());
                        for f in n.finals {
                            trans.insert((f, None), vec![p].into_iter().collect());
                        }
                        trans
                    };
                    ENFA {
                        states: n.states.into_iter().chain(vec![p].into_iter()).collect(),
                        alpha: n.alpha,
                        trans,
                        initial: p,
                        finals: vec![p].into_iter().collect(),
                    }
                }
            }
        }
        aux(re, {
            let mut i = 0;
            &mut move || {
                let j = i;
                i += 1;
                j
            }
        })
    }
}

fn main() {
    use RegExp::*;
    {
        let re = parse_str_opt("a(b|c)*");
        assert_eq!(
            re,
            Cat(
                Box::new(Char('a')),
                Box::new(Star(Box::new(Or(Box::new(Char('b')), Box::new(Char('c'))))))
            )
        );
    }
    let mut trans = std::collections::HashMap::new();
    let mut dst = std::collections::HashSet::new();
    dst.insert(1);
    trans.insert((0, 'a'), dst.clone());
    trans.insert((1, 'b'), dst.clone());
    trans.insert((1, 'c'), dst.clone());
    let mut finals = std::collections::HashSet::new();
    finals.insert(1);
    let n = NFA {
        alpha: vec!['a', 'b', 'c'],
        trans,
        initial: 0,
        finals,
    };
    let d = n.to_dfa();
    assert!(d.accepts("a"));
    assert!(d.accepts("abcbccbb"));
    assert!(!d.accepts("b"));
    let states = vec![0, 1, 2, 3, 4, 5].into_iter().collect();
    let alpha = vec!['a', 'b'].into_iter().collect();
    let initial = 0;
    let finals = vec![3].into_iter().collect();
    let trans = vec![
        ((0, None), vec![1, 3]),
        ((1, Some('a')), vec![2]),
        ((2, None), vec![0]),
        ((3, None), vec![4]),
        ((4, Some('b')), vec![5]),
        ((5, None), vec![3]),
    ]
    .into_iter()
    .map(|(k, v)| (k, v.into_iter().collect()))
    .into_iter()
    .collect();
    let e = ENFA {
        alpha,
        states,
        trans,
        initial,
        finals,
    };
    let n = e.to_nfa();
    let d = n.to_dfa();
    assert!(d.accepts("aabb"));
    assert!(d.accepts("bb"));
    assert!(d.accepts("a"));
    assert!(!d.accepts("aba"));
    assert!(!d.accepts("bab"));

    {
        let re = parse_str_opt("a(b|c)*");
        let d = ENFA::from_regexp(&re).to_nfa().to_dfa();
        assert!(d.accepts("a"));
        assert!(d.accepts("abcbccbb"));
        assert!(!d.accepts("b"));
    }

    {
        let u = UnionFind::from(vec![1, 2, 3, 4, 5]);
        u.union(1, 3);
        println!("{:#?}", u);
        assert_eq!(u.find(1), u.find(3));
        u.union(3, 2);
        println!("{:#?}", u);
        assert_eq!(u.find(1), u.find(2));
    }

    {
        let re = parse_str_opt("a(b|c)*");
        println!("DFA that accepts a(b|c)*:");
        let d = ENFA::from_regexp(&re).to_nfa().to_dfa();
        println!("Before minimization: {:#?}", d);
        let d = d.minimized();
        println!("After minimization: {:#?}", d);
    }
}
