use crate::*;
use crate::space::grounding::GroundingSpace;
//use crate::Atom;
use std::fmt::{Display, Debug};
use std::{fmt, clone};
use ndarray::Array1;

pub const SEGMENT:usize = 10;
pub const MATCH_RATIO: f32 = 0.95;

static mut NODE_ID:u32 = 0;
static mut LINK_ID:u32 = 0;

#[derive(PartialEq)]
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
        write!(f, "{}", &self.Name)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LinksEdge {
    Edge_ID: u32,
    Source_Node_ID: u32,
    Dist_Node_ID: u32,
    Weights: Vec<f32>,
    Name: String,
    Source_Name: String,
    Dist_Name: String,
}
impl LinksEdge {
    /// Constructs new empty node.
    pub fn new() -> Self {
        Self{
            Edge_ID: 0,
            Source_Node_ID: 0,
            Dist_Node_ID: 0,
            Weights: Vec::new(),
            Name: String::from(""),
            Source_Name: String::from(""),
            Dist_Name: String::from(""),
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
        write!(f, "{}", &self.Name)
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

pub fn set_link_for_nodes(link_space: &mut GroundingSpace, prev: & Atom, next: & Atom, link_name: &String) -> Vec<Atom> {
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
    Link_new_node.Name = (*link_name).clone();
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

pub fn find_link_by_name(memory_space: & GroundingSpace, link_name: &String) -> Result<Atom, String> {
    let temp_link = LinksEdge::new();
    let mut temp_atom = Atom::gnd(temp_link.clone());
    for next_edge in &(*memory_space.borrow_vec()) {
        temp_atom = (*next_edge).clone();
        let temp_type_atom = temp_atom.as_gnd::<LinksEdge>();
        match temp_type_atom {
        Some(temp_type_atom)=> {
            println!("(*link_name) {}", (*link_name));
            println!("Name {}", temp_atom.as_gnd::<LinksEdge>().unwrap().Name);
            if temp_atom.as_gnd::<LinksEdge>().unwrap().Name == (*link_name) {
                return Ok(temp_atom.clone())
            }
        }, None => { },
        }
    }
    return Err(String::from("Link not found!"))
}

pub fn find_node_by_name(memory_space: & GroundingSpace, node_name: &String) -> Result<Atom, String> {
    let temp_node = MemoryNode::new();
    let mut temp_atom = Atom::gnd(temp_node.clone());
    for next_node in &(*memory_space.borrow_vec()) {
        temp_atom = (*next_node).clone();
        let temp_type_atom = temp_atom.as_gnd::<MemoryNode>();
        match temp_type_atom {
        Some(temp_type_atom)=> {
            if temp_atom.as_gnd::<MemoryNode>().unwrap().Name == (*node_name) {
                return Ok(temp_atom.clone())
            }
        }, None => { },
        }
    }
    return Err(String::from("Node not found!"))
}

pub fn parse_link_node(input_atom: &Atom, input_type: NodeEdge) -> Result<Vec<Atom>, String> {
    if let Atom::Expression(expr_atom) = input_atom {
        let exr_atom_vec = expr_atom.children();
        if exr_atom_vec.len() == 2 {
            let atom_type = exr_atom_vec.get(0).unwrap();
            println!("atom_type {}", atom_type);
            if (atom_type.to_string() == "LinksEdge" && input_type == NodeEdge::Edge) || (atom_type.to_string() == "MemoryNode" && input_type == NodeEdge::Node) {
                let atom_link_node = exr_atom_vec.get(1).unwrap();
                println!("atom_link_node {}", atom_link_node);
                let mut vec_atoms = Vec::new();
                vec_atoms.push((*atom_type).clone());
                vec_atoms.push((*atom_link_node).clone());
                return Ok(vec_atoms)
            }
        }
    }
    return Err(String::from("Atom not Link Node expression!"))
}

pub fn create_link_nodes_by_names(memory_space: & mut GroundingSpace, input_vec: &Vec<String>) -> Result<Vec<Atom>, String,> {
    let mut vec_atoms_temp = Vec::new();
    let mut error_mes = String::from("Could not create Link with Nodes.");
    if input_vec.len() == 3 {
        let node1_name = input_vec.get(1).unwrap();
        let res_node1_find = GraphKB::find_node_by_name(memory_space, &node1_name);
        match res_node1_find {
            Ok(node1_atom_find)=>{
                vec_atoms_temp.push(node1_atom_find);
            }, Err(error)=>{
                let mut new_node1 = MemoryNode::new();
                let code1: Vec<f32> = vec![11.; SEGMENT];
                new_node1.Name = (*node1_name).clone();
                new_node1.Key_Code = code1;
                println!("new_node1.Name {:?}", new_node1.Name);
                let mut new_atom_node1 = Atom::gnd(new_node1.clone());
                new_atom_node1 = GraphKB::identify_code_in_memory(memory_space, & new_atom_node1);
                vec_atoms_temp.push(new_atom_node1.clone());
                println!("new_atom_node1 {:?}", new_atom_node1);
                }
        }
        let node2_name = input_vec.get(2).unwrap();
        let res_node2_find = GraphKB::find_node_by_name(memory_space, &node2_name);
        match res_node2_find {
            Ok(node2_atom_find)=>{
                vec_atoms_temp.push(node2_atom_find);
            }, Err(error)=>{
                let mut new_node2 = MemoryNode::new();
                let code2: Vec<f32> = vec![22.; SEGMENT];
                new_node2.Name = (*node2_name).clone();
                new_node2.Key_Code = code2;
                println!("new_node2.Name {:?}", new_node2.Name);
                let mut new_atom_node2 = Atom::gnd(new_node2.clone());
                new_atom_node2 = GraphKB::identify_code_in_memory(memory_space, & new_atom_node2);
                vec_atoms_temp.push(new_atom_node2.clone());
                println!("new_atom_node2 {:?}", new_atom_node2);
                }
        }
    let link_name = input_vec.get(0).unwrap();
    let vec_atoms = GraphKB::set_link_for_nodes(memory_space, & vec_atoms_temp.get(0).unwrap(), & vec_atoms_temp.get(1).unwrap(), link_name);
    if vec_atoms.len() == 3 {
        println!("new_atom_link {:?}", vec_atoms.get(0).unwrap().as_gnd::<LinksEdge>().unwrap().Name);
        return Ok(vec_atoms)
    } else {return Err(error_mes)}
    }
    else {return Err(error_mes)}
}

pub fn parse_graph(memory_space: & GroundingSpace, input_atom: &Atom, output_vec: &mut Vec<String>) -> Result<Vec<Atom>, String,> {
    let mut vec_atoms_temp = Vec::new();
    let mut error_mes = String::from("No Link with nodes found! Please use create_link_nodes_by_names() function.");
    if let Atom::Expression(expr_atom) = input_atom {
        println!("expr_atom {:?}", expr_atom);
        let expr_atom_vec = expr_atom.children();
        if expr_atom_vec.len() == 3 {
            let res_link = GraphKB::parse_link_node(expr_atom_vec.get(0).unwrap(),NodeEdge::Edge);
            match res_link {
                Ok(link_atom_vec)=>{ 
                    let link_name = (link_atom_vec.get(1).unwrap()).to_string();
                    output_vec.push(link_name.clone());
                    let res_link_find = GraphKB::find_link_by_name(memory_space, &link_name);
                    match res_link_find {
                        Ok(link_atom_find)=>{ 
                            vec_atoms_temp.push(link_atom_find.clone());
                        }, Err(error)=>{}
                    }
                }, Err(error)=>{
                    error_mes = String::from("Link Expression is not valid.");
                    return Err(error_mes)
                }
            }
            let res_node1 = GraphKB::parse_link_node(expr_atom_vec.get(1).unwrap(),NodeEdge::Node);
            match res_node1 {
                Ok(node1_atom_vec)=>{
                    let node1_name = (node1_atom_vec.get(1).unwrap()).to_string();
                    output_vec.push(node1_name.clone());
                    let res_node1_find = GraphKB::find_node_by_name(memory_space, &node1_name);
                    match res_node1_find {
                        Ok(node1_atom_find)=>{ 
                            vec_atoms_temp.push(node1_atom_find.clone());
                        }, Err(error)=>{}
                    }
                    }, Err(error)=>{
                        error_mes = String::from("Node1 Expression is not valid.");
                        return Err(error_mes)}
                }
            let res_node2 = GraphKB::parse_link_node(expr_atom_vec.get(2).unwrap(),NodeEdge::Node);
            match res_node2 {
                Ok(node2_atom_vec)=>{
                    let node2_name = (node2_atom_vec.get(1).unwrap()).to_string();
                    output_vec.push(node2_name.clone());
                    let res_node2_find = GraphKB::find_node_by_name(memory_space, &node2_name);
                    match res_node2_find {
                        Ok(node2_atom_find)=>{ 
                            vec_atoms_temp.push(node2_atom_find.clone());
                            
                        }, Err(error)=>{}
                    }
                }, Err(error)=>{
                    error_mes = String::from("Node2 Expression is not valid.");
                    return Err(error_mes)}
            }
            if vec_atoms_temp.len() == 3{
                return Ok(vec_atoms_temp) 
            }
        } else {error_mes = String::from("This is not Link with Nodes Atom.");}
    } else {error_mes = String::from("This is not Expression Atom.");}
    return Err(error_mes)
    }

}

#[cfg(test)]
mod tests {
use super::*;
use crate::child_ai::graph_kb::GraphKB;
use crate::metta::interpreter;
use crate::metta::text::*;
    #[test]
    pub fn test_save_node_edge(){
        // Create test Space, Grounded Atoms for Link, Nodes. Save Atoms in Space and set Link.
        let mut Memory_space = GroundingSpace::new();
        let code1: Vec<f32> = vec![1.; SEGMENT];
        let mut new_node = MemoryNode::new();
        new_node.Name = String::from("Node1");
        new_node.Key_Code = code1;
        println!("new_node {}", new_node.type_());
        let mut new_node_atom = Atom::gnd(new_node.clone());
        let mut result_atom = GraphKB::identify_code_in_memory(&mut Memory_space, & new_node_atom);
        let result_atom1 = result_atom.clone();
        let code2: Vec<f32> = vec![2.; SEGMENT];
        new_node.Name = String::from("Node2");
        new_node.Key_Code = code2;
        new_node_atom = Atom::gnd(new_node.clone());
        result_atom = GraphKB::identify_code_in_memory(&mut Memory_space, & new_node_atom);
        let result_atom2 = result_atom.clone();
        let mut vec_atoms = Vec::new();
        let link_name = result_atom1.as_gnd::<MemoryNode>().unwrap().Name.clone() +(&result_atom2.as_gnd::<MemoryNode>().unwrap().Name);
        vec_atoms = GraphKB::set_link_for_nodes(&mut Memory_space, & result_atom1, & result_atom2, &link_name);
        let mut link_atom = vec_atoms.get_mut(0).unwrap();
        let old_atom = link_atom.clone();
        (*link_atom).as_gnd_mut::<LinksEdge>().unwrap().Name = String::from("Link1");
        Memory_space.replace(&old_atom, (*link_atom).clone());


        // Test for "Expression Atoms for Graph knowledge base" Pull Request.
        let mut space = GroundingSpace::new();
        space = Memory_space.clone();

        // test case when Grounded Atoms (Link1, Node1, Node2) exist.
        //let program = "((LinksEdge Link1) (MemoryNode Node1) (MemoryNode Node2))";
        // test case when Grounded Atoms (Link11, Node11, Node22) don't exist.
        let program = "((LinksEdge Link11) (MemoryNode Node11) (MemoryNode Node22))";

        let parse_res = parse_atoms(program);
        let parse_atom = parse_res.get(0).unwrap();

        let mut names_vec = Vec::new();
        let res_graph = GraphKB::parse_graph(&space, &parse_atom, &mut names_vec); 
        match res_graph {
            Ok(vec_atom_graph)=>{ 
                let expr_atom_graph = Atom::expr(vec_atom_graph.clone());
                space.add(expr_atom_graph.clone());
                println!("Expression Grounded Atom Found and added to Space: {:?}", expr_atom_graph);
            },
            Err(error_graph)=>{
                println!("parse_graph message: {:?}", error_graph);
                let res_vec_atoms =  GraphKB::create_link_nodes_by_names(& mut space, &names_vec);
                match res_vec_atoms {
                    Ok(vec_atom_graph)=>{
                        let expr_atom_graph = Atom::expr(vec_atom_graph.clone());
                        space.add(expr_atom_graph.clone());
                        println!("Expression Grounded Atom Created and added to Space: {:?}", expr_atom_graph);
                    }, Err(error_graph)=>{
                        println!("create_link_nodes_by_names: {:?}", error_graph);
                    }
                }
            }
        }
        

        // Test findning linked nodes.
        let link_name = String::from("link_name");
        vec_atoms = GraphKB::set_link_for_nodes(&mut Memory_space, vec_atoms.get(1).unwrap(), vec_atoms.get(1).unwrap(), &link_name);
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

    // function for parsing testing from text.rs, analog of similar in Python
    fn parse_atoms(program: &str) -> Vec<Atom> {
        let tokenizer = Tokenizer::new();
        let mut parser = SExprParser::new(program);
        let mut result = Vec::new();
        while let Some(atom) = parser.parse(&tokenizer) {
            result.push(atom.clone());
            println!("parse_atoms {}", atom);
        }
        result
    }

    fn get_atom_type(atom: &Atom)->String{
        match atom {
            Atom::Expression(atom)=>{return String::from("Expression")},
            Atom::Symbol(atom)=>{return String::from("Symbol")},
            Atom::Variable(atom)=>{return String::from("Variable")},
            Atom::Grounded(atom)=>{return String::from("Grounded")},
            _=>{return String::from("Other")}
        }
    }
    

}