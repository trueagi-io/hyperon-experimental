use crate::*;
use crate::space::grounding::GroundingSpace;
use crate::Atom;
use std::fmt::{Display, Debug};
use std::{fmt, clone};
use ndarray::Array1;

pub const SEGMENT:usize = 10;
pub const MATCH_RATIO: f32 = 0.95;

static mut NODE_ID:u32 = 0;
static mut LINK_ID:u32 = 0;

pub enum NodeEdge{
    Node,
    Edge,
    Other,
}

pub enum SensoryType{
    OTHER,
    SENSORY,
    ACTION,
    REACTION,
}


#[derive(PartialEq, Clone, Debug)]
pub struct MemoryNode {
    Node_ID: u32,
    Link_IDs: Vec<u32>,
    Key_Code: Vec<f32>,
    Name: String,
}
impl MemoryNode {
    /// Constructs new empty node.
    pub fn new() -> Self {
        Self{
            Node_ID: 0,
            Link_IDs: Vec::new(),
            Key_Code: Vec::new(),
            Name: String::from(""),
        }
    }
    pub fn node_edge_() -> NodeEdge {
        NodeEdge::Node
    }
}
impl Grounded for MemoryNode {
    fn type_(&self) -> Atom {
        rust_type_atom::<MemoryNode>()
    }
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError>{
        execute_not_executable(self)
    }
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match_by_equality(self, other)
    }
}
impl fmt::Display for MemoryNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.Node_ID)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LinksEdge {
    Edge_ID: u32,
    Source_Node_ID: u32,
    Dist_Node_ID: u32,
    Weights: Vec<f32>,
}
impl LinksEdge {
    /// Constructs new empty node.
    pub fn new() -> Self {
        Self{
            Edge_ID: 0,
            Source_Node_ID: 0,
            Dist_Node_ID: 0,
            Weights: Vec::new(),
        }
    }
    pub fn node_edge_() -> NodeEdge {
        NodeEdge::Edge
    }
}
impl Grounded for LinksEdge {
    fn type_(&self) -> Atom {
        rust_type_atom::<LinksEdge>()
    }
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError>{
        execute_not_executable(self)
    }
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match_by_equality(self, other)
    }
}
impl fmt::Display for LinksEdge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.Edge_ID)
    }
}

pub struct GraphKB {
    Nodes: Vec<MemoryNode>,
    Links: Vec<LinksEdge>,
}

impl GraphKB {

pub fn find_linked_edges(memory_space: & GroundingSpace, find_node_atom: &Atom) -> Vec<Atom> {
    let mut vec_atoms = Vec::new();
    let vec_link_ids =  find_node_atom.as_gnd::<MemoryNode>().unwrap().Link_IDs.clone();
    let temp_node = LinksEdge::new();
    let mut temp_atom = Atom::gnd(temp_node.clone());
    for edge_id in vec_link_ids{
        println!("id {}", edge_id);
        for next_edge in &(*memory_space.borrow_vec()) {
            temp_atom = (*next_edge).clone();
            let temp_type_atom = temp_atom.as_gnd::<LinksEdge>();
            match temp_type_atom {
            Some(temp_type_atom)=> {
            if temp_atom.as_gnd::<LinksEdge>().unwrap().Edge_ID == edge_id {
                
                vec_atoms.push(temp_atom.clone());
            }
            },
            None => { },
            }
        }
    }
    vec_atoms.sort_by(|a, b| a.as_gnd::<LinksEdge>().unwrap().Weights[0].partial_cmp(&b.as_gnd::<LinksEdge>().unwrap().Weights[0]).unwrap());
    return vec_atoms
}


pub fn identify_code_in_memory(memory_space: & mut GroundingSpace, new_node_atom: &Atom) -> Atom {
    let mut temp_atom_new = (*new_node_atom).clone();
    let temp_node = MemoryNode::new();
    let mut temp_atom_next_old = Atom::gnd(temp_node.clone());
    let mut temp_atom_next_new = Atom::gnd(temp_node.clone());
    let mut is_found = false;
    let mut node_id = 0;
    let mut temp_type_node = NodeEdge::Node;
    for next_atom in & (*memory_space.borrow_vec()) {
        temp_atom_next_old = (*next_atom).clone();
        let temp_type_atom = temp_atom_next_old.as_gnd::<MemoryNode>();
        match temp_type_atom {
        Some(temp_type_atom)=> {
        temp_type_node = NodeEdge::Node;
        if GraphKB::match_ratio(next_atom, new_node_atom) >= MATCH_RATIO {
            temp_atom_next_new = (*next_atom).clone();
            GraphKB::calc_wieght_increase(& mut temp_atom_next_new.as_gnd_mut::<MemoryNode>().unwrap().Key_Code, 1.01);
            is_found = true;
            temp_type_node = NodeEdge::Node;
            break;
        }
        },
        None => { temp_type_node = NodeEdge::Other; },
    }
    }
    let temp_type_atom = temp_atom_new.as_gnd::<MemoryNode>();
    match temp_type_atom {
        Some(temp_type_atom)=> {temp_type_node = NodeEdge::Node;},
        None => { temp_type_node = NodeEdge::Other; },
    }
    match temp_type_node {
        NodeEdge::Node => {
        if is_found {
            node_id = temp_atom_next_new.as_gnd::<MemoryNode>().unwrap().Node_ID;
            memory_space.replace(&temp_atom_next_old, temp_atom_next_new.clone());
            return temp_atom_next_new
        }
        node_id = GraphKB::plus_NODE_ID();
        temp_atom_new.as_gnd_mut::<MemoryNode>().unwrap().Node_ID = node_id;
        GraphKB::calc_wieght_increase(& mut temp_atom_new.as_gnd_mut::<MemoryNode>().unwrap().Key_Code, 1.01);
        memory_space.add(temp_atom_new.clone());
        return temp_atom_new
        },
        _ => {return (*new_node_atom).clone()},
    }
}

pub fn set_link_for_nodes(link_space: &mut GroundingSpace, prev: & Atom, next: & Atom) -> Vec<Atom> {
    let mut vec_atoms = Vec::new(); //first is Link, second is Prev Node, third is Next Node
    let mut temp_atom_prev = (*prev).clone();
    let mut temp_atom_next = (*next).clone();

    let temp_node = LinksEdge::new();
    let mut temp_atom_next_old = Atom::gnd(temp_node.clone());
    let mut temp_atom_next_new = Atom::gnd(temp_node.clone());

    let mut is_found = false;
    let mut edge_id = 0;
    let mut temp_type_node = NodeEdge::Edge;

    for next_edge in &(*link_space.borrow_vec()) {
        temp_atom_next_old = (*next_edge).clone();
        let temp_type_atom = temp_atom_next_old.as_gnd::<LinksEdge>();
        match temp_type_atom {
        Some(temp_type_atom)=> {
        temp_type_node = NodeEdge::Edge;
        if (next_edge.as_gnd::<LinksEdge>().unwrap().Source_Node_ID == prev.as_gnd::<MemoryNode>().unwrap().Node_ID) && (next_edge.as_gnd::<LinksEdge>().unwrap().Dist_Node_ID == next.as_gnd::<MemoryNode>().unwrap().Node_ID) {
            temp_atom_next_new = (*next_edge).clone();
            GraphKB::calc_wieght_increase(&mut temp_atom_next_new.as_gnd_mut::<LinksEdge>().unwrap().Weights, 1.01);
            is_found = true;
            break;
        }
        },
        None => { temp_type_node = NodeEdge::Other; },
        }
    }
    match temp_type_node {
        NodeEdge::Edge => {
        if is_found {
            edge_id = temp_atom_next_new.as_gnd::<LinksEdge>().unwrap().Edge_ID;
            link_space.replace(&temp_atom_next_old, temp_atom_next_new.clone());
            vec_atoms.push(temp_atom_next_new);
            vec_atoms.push(temp_atom_prev);
            vec_atoms.push(temp_atom_next);
            return vec_atoms
        }
        },
        _ => { },
    }
    let mut Link_new_node = LinksEdge::new();
    edge_id = GraphKB::plus_LINK_ID();
    Link_new_node.Edge_ID = edge_id;
    Link_new_node.Source_Node_ID = prev.as_gnd::<MemoryNode>().unwrap().Node_ID;
    Link_new_node.Dist_Node_ID = next.as_gnd::<MemoryNode>().unwrap().Node_ID;
    Link_new_node.Weights= vec![0.01; SEGMENT];
    let Link_new_node_atom = Atom::gnd(Link_new_node);
    link_space.add(Link_new_node_atom.clone());

    temp_atom_prev.as_gnd_mut::<MemoryNode>().unwrap().Link_IDs.push(edge_id);
    temp_atom_next.as_gnd_mut::<MemoryNode>().unwrap().Link_IDs.push(edge_id);

    link_space.replace(prev, temp_atom_prev.clone());
    link_space.replace(next, temp_atom_next.clone());

    vec_atoms.push(Link_new_node_atom);
    vec_atoms.push(temp_atom_prev);
    vec_atoms.push(temp_atom_next);
    return vec_atoms
}

pub fn calc_wieght_increase(weights: & mut Vec<f32>, update_value: f32){
    for i in weights {
        *i *= update_value;
    }
}

pub fn plus_NODE_ID() ->u32 {
    unsafe { NODE_ID += 1; NODE_ID }
}

pub fn plus_LINK_ID() ->u32 {
    unsafe { LINK_ID += 1; LINK_ID }
}

pub fn match_ratio(data: &Atom, pattern: &Atom) -> f32 {
    let a = Array1::from(data.as_gnd::<MemoryNode>().unwrap().Key_Code.clone());
    let b = Array1::from(pattern.as_gnd::<MemoryNode>().unwrap().Key_Code.clone());
    let diff = &a-&b;
    let match_ratio = 1.-(diff.sum()/(diff.len() as f32)).abs();
    match_ratio
}

}



#[cfg(test)]
mod tests {
 
use crate::space::grounding::GroundingSpace;
use crate::child_ai::graph_kb::*;

    #[test]
    pub fn test_save_node_edge(){
        let mut Memory_space = GroundingSpace::new();

  
        let code1: Vec<f32> = vec![0.1; SEGMENT];
        let mut new_node = MemoryNode::new();
        new_node.Name = String::from("Name1");
        new_node.Key_Code = code1;
        let mut new_node_atom = Atom::gnd(new_node.clone());
        let mut result_atom = GraphKB::identify_code_in_memory(&mut Memory_space, & new_node_atom);
        let result_atom1 = result_atom.clone();
    
    
        let code2: Vec<f32> = vec![0.2; SEGMENT];
        new_node.Name = String::from("Name2");
        new_node.Key_Code = code2;
        new_node_atom = Atom::gnd(new_node.clone());
        result_atom = GraphKB::identify_code_in_memory(&mut Memory_space, & new_node_atom);
        let result_atom2 = result_atom.clone();
    
    
        let mut vec_atoms = Vec::new();
        vec_atoms = GraphKB::set_link_for_nodes(&mut Memory_space, & result_atom1, & result_atom2);
        vec_atoms = GraphKB::set_link_for_nodes(&mut Memory_space, vec_atoms.get(1).unwrap(), vec_atoms.get(1).unwrap());
        
        let result_atom_find = vec_atoms.get(1).unwrap();
        let node_id = result_atom_find.as_gnd::<MemoryNode>().unwrap().Node_ID;
        let vec_link_ids =  result_atom_find.as_gnd::<MemoryNode>().unwrap().Link_IDs.clone();
        vec_atoms = GraphKB::find_linked_edges(& Memory_space,  & result_atom_find);
        let atom1 =  vec_atoms.get(0).unwrap();
        let source_id = atom1.as_gnd::<LinksEdge>().unwrap().Source_Node_ID;
        println!("source_id {}", source_id);
        let dist_id = atom1.as_gnd::<LinksEdge>().unwrap().Dist_Node_ID;
        println!("dist_id {}", dist_id);
    
    }
}