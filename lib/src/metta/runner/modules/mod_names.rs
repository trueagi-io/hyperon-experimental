//! Implements logic for working with module names, including a low-allocation layered tree structure 

use crate::metta::runner::*;
use crate::common::owned_or_borrowed::OwnedOrBorrowed;

/// The name of the top module in a runner
pub const TOP_MOD_NAME: &'static str = "top";

/// The name to refer to the current module in the module name hierarchy
pub const SELF_MOD_NAME: &'static str = "self";

/// The separator between parent and child module names in a module name path
pub const MOD_NAME_SEPARATOR: char = ':';

/// Private struct, A node in a tree to locate loaded mods by name
///
/// # Name Resolution Behavior
/// `top` is a reserved module name for the module at the top of the runner
/// `top.some_mod` and `some_mod` are equivalent.  In other words, `top` is optional in a path
/// `self` is an alias for the current module
/// `self.some_mod` is a private sub-module of the current module
///
#[derive(Clone, Debug, Default)]
pub(crate) struct ModNameNode {
    pub mod_id: ModId,
    children: Option<HashMap<String, ModNameNode>>
}

/// Returns `Some(path)`, with `path` being the relative portion following `self:`, if the name
/// begins with "self:". Returns "" if the name exactly equals "self".  Otherwise returns None
pub(crate) fn mod_name_relative_path(name: &str) -> Option<&str> {
    mod_name_remove_prefix(name, SELF_MOD_NAME)
}

pub(crate) fn mod_name_remove_prefix<'a>(name: &'a str, prefix: &str) -> Option<&'a str> {
    if name.starts_with(prefix) {
        if name.len() == prefix.len() {
            Some("")
        } else {
            if name.as_bytes()[prefix.len()] == MOD_NAME_SEPARATOR as u8 {
                Some(&name[(prefix.len()+1)..])
            } else {
                None
            }
        }
    } else {
        None
    }
}

//TODO-NEXT: This function is likely unneeded, and can be removed
// /// Returns the portion of `name` that does not match the beginning of `other`, starting
// /// at the point where the strings diverge.  For example, is `name == "top:mod_a:hello"`
// /// and `other == "top:mod_a:goodbye"`, then this function would return "hello".
// pub(crate) fn remove_common_prefix<'a>(name: &'a str, other: &str) -> &'a str {
//     let mut start = 0;
//     let mut other_iter = other.chars();
//     for (idx, c_name) in name.char_indices() {
//         if c_name == MOD_NAME_SEPARATOR {
//             start = idx+1;
//         }
//         match other_iter.next() {
//             Some(c_other) => {
//                 if c_name != c_other {
//                     return &name[start..]
//                 }
//             },
//             None => return &name[start..]
//         }
//     }
//     ""
// }

/// Returns the part of a module name path after the last separator, or the entire path if it does not
/// contain a separator.  May panic if the mod-name-path is invalid
pub(crate) fn mod_name_from_path(path: &str) -> &str {
    let mut start_idx = 0;
    for (idx, the_char) in path.char_indices() {
        if the_char == MOD_NAME_SEPARATOR {
            start_idx = idx+1;
        }
    }
    &path[start_idx..]
}

/// Join a relative module path as a sub-module to `&self`
pub(crate) fn concat_relative_module_path(base_path: &str, relative_path: &str) -> String {
    if relative_path.len() > 0 {
        format!("{base_path}:{relative_path}")
    } else {
        base_path.to_string()
    }
}

/// Returns the portion of `mod_path` that is a sub-path on top of `base_path`.  The reverse
/// of `concat_relative_module_path`
pub(crate) fn get_module_sub_path<'a>(mod_path: &'a str, base_path: &str) -> Option<&'a str> {
    if mod_path.starts_with(base_path) {
        if (&mod_path[base_path.len()..]).starts_with(':') {
            Some(&mod_path[base_path.len()+1..])
        } else {
            None
        }
    } else {
        None
    }
}

/// Normalize a module name into a canonical name-path form, and expanding a relative module-path
///
/// On input, module names that don't begin with either `top` nor `self` will be assumed to be
/// relative to the current module.  On output, the result will be an absolute path beginning
/// with `top`.
pub(crate) fn normalize_relative_module_name(base_path: &str, mod_name: &str) -> Result<String, String> {
    let full_name_path = match mod_name_relative_path(mod_name) {
        Some(remainder) => concat_relative_module_path(base_path, remainder),
        None => {
            match mod_name_remove_prefix(mod_name, TOP_MOD_NAME) {
                Some(remainder) => format!("{}:{}", TOP_MOD_NAME, remainder),
                None => concat_relative_module_path(base_path, mod_name),
            }
        },
    };
    Ok(full_name_path)
}

/// Decomposes name path components into individual module names.  Reverse of [compose_name_path]
pub(crate) fn decompose_name_path(name: &str) -> Result<Vec<&str>, String> {
    let mut components: Vec<&str> = vec![];
    let (_, _, last) = ModNameNode::parse_parent_generic(ModNameNode::top(), name, &OverlayMap::none(),
        |node, _| Some(node),
        |_, component| {components.push(component)})?;
    if last.len() > 0 {
        components.push(last);
    }
    Ok(components)
}

/// Composes a name path from a slice of individual module names.  Reverse of [decompose_name_path]
pub(crate) fn compose_name_path(components: &[&str]) -> Result<String, String> {
    let mut new_name = TOP_MOD_NAME.to_string();
    for component in components {
        new_name.push_str(":");
        new_name.push_str(component);
    }
    Ok(new_name)
}

/// Internal map to allow subtrees to be overlaid and effectively merged
#[derive(Debug)]
struct OverlayMap<'a>(Option<smallvec::SmallVec<[(*const ModNameNode, &'a str, usize); 4]>>);

impl<'a> OverlayMap<'a> {
    fn none() -> Self {
        Self(None)
    }
    fn with_capacity(capacity: usize) -> Self {
        Self(Some(smallvec::SmallVec::with_capacity(capacity)))
    }
    fn push(&mut self, parent_node: *const ModNameNode, name: &'a str, subtree_idx: usize) {
        match &mut self.0 {
            Some(map) => map.push((parent_node, name, subtree_idx)),
            None => panic!()
        }
    }
    fn get(&self, parent_node: *const ModNameNode, name: &str) -> Option<usize> {
        match &self.0 {
            Some(map) => for (map_parent_node, map_name, subtree_idx) in map.iter() {
                if std::ptr::eq(parent_node, *map_parent_node) && name == *map_name {
                    return Some(*subtree_idx);
                }
            }
            None => {}
        }
        None
    }
}

impl ModNameNode {

    /// Returns the node corresponding to the runner's "top" mod
    pub fn top() -> Self {
        Self::new(ModId::TOP)
    }

    pub fn new(mod_id: ModId) -> Self {
        Self {
            mod_id,
            children: None,
        }
    }

    /// Updates the ModId of an existing node, or creates the node if it doesn't exist
    /// Does NOT recursively add multiple nodes
    pub fn update(&mut self, name: &str, mod_id: ModId) -> Result<(), String> {
        self.update_in_layered(&mut[], name, mod_id)
    }

    /// Layered version of [Self::update].  See [Self::resolve_layered] for details
    pub fn update_in_layered(&mut self, subtrees: &mut[(&str, &mut Self)], name: &str, mod_id: ModId) -> Result<(), String> {
        if name == TOP_MOD_NAME || name.len() == 0 {
            self.mod_id = mod_id;
            return Ok(());
        }
        let (parent_node, mod_name) = self.parse_parent_layered_mut(subtrees, name)?;
        if mod_name == TOP_MOD_NAME || mod_name == SELF_MOD_NAME || mod_name.len() == 0 {
            return Err(format!("illegal module name: {name}"));
        }
        match parent_node.get_child_mut(mod_name) {
            Some(node) => node.mod_id = mod_id,
            None => parent_node.add_child_node(mod_name.to_string(), Self::new(mod_id))
        }
        Ok(())
    }

    /// Adds a single new node to the tree.  Does NOT recursively add multiple nodes
    ///
    /// If an entry already exists at that name, the existing entry will be replaced
    pub fn add(&mut self, name: &str, mod_id: ModId) -> Result<(), String> {
        self.merge_subtree_into(name, Self::new(mod_id))
    }

    /// Adds a single new node to a layered tree.  See [Self::resolve_layered] for details
    #[allow(dead_code)] //NOTE: This function "feels" like it belongs in a complete API, and might be used in the future.  See discussion on [ModuleInitFrame::add_module_to_name_tree]
    pub fn add_to_layered(&mut self, subtrees: &mut[(&str, &mut Self)], name: &str, mod_id: ModId) -> Result<(), String> {
        self.merge_subtree_into_layered(subtrees, name, Self::new(mod_id))
    }

    /// Merges `new_subtree` into `&self`, starting at `root_name`
    pub fn merge_subtree_into(&mut self, root_name: &str, new_subtree: Self) -> Result<(), String> {
        self.merge_subtree_into_layered(&mut[], root_name, new_subtree)
    }

    /// Layered version of [Self::merge_subtree_into].  See [Self::resolve_layered] for details
    pub fn merge_subtree_into_layered(&mut self, layered_subtrees: &mut[(&str, &mut Self)], root_name: &str, new_subtree: Self) -> Result<(), String> {
        let (parent_node, mod_name) = self.parse_parent_layered_mut(layered_subtrees, root_name)?;
        if mod_name == TOP_MOD_NAME || mod_name == SELF_MOD_NAME || mod_name.len() == 0 {
            return Err(format!("illegal module name: {root_name}"));
        }
        parent_node.add_child_node(mod_name.to_string(), new_subtree);
        Ok(())
    }

    /// Returns a child node from a name
    fn get_child(&self, child_name: &str) -> Option<&Self> {
        match &self.children {
            Some(children) => children.get(child_name),
            None => None
        }
    }

    /// Mutable version of [get_child]
    fn get_child_mut(&mut self, child_name: &str) -> Option<&mut Self> {
        match &mut self.children {
            Some(children) => children.get_mut(child_name),
            None => None
        }
    }

    /// Adds a child node, operates on a single parent node
    fn add_child_node(&mut self, child_name: String, child_node: Self) {
        if self.children.is_none() {
            self.children = Some(HashMap::new());
        }
        self.children.as_mut().unwrap().insert(child_name, child_node);
    }

    /// Walks a tree running the provided function on each node, including the root
    pub fn visit_mut<F: FnMut(&str, &mut Self)>(&mut self, root_node_name: &str, mut f: F) {
        self.visit_mut_internal(root_node_name, &mut f);
    }

    fn visit_mut_internal<F: FnMut(&str, &mut Self)>(&mut self, root_node_name: &str, f: &mut F) {
        f(root_node_name, self);
        if let Some(children) = &mut self.children {
            for (child_name, child_node) in children.iter_mut() {
                child_node.visit_mut_internal(child_name, f);
            }
        }
    }

    /// Returns the [ModId] of a module name/path within `&self`, or None if it can't be resolved
    pub fn resolve(&self, name: &str) -> Option<ModId> {
        self.name_to_node::<&Self>(&[], name).map(|node| node.mod_id)
    }

    /// Returns the [ModId] of a module name/path within a tree composed of `&self` and additional
    /// layered subtrees, or None if it can't be resolved.
    ///
    /// Each subtree is oriented with respect to the tree defined by &self and prior elements of the
    /// 'subtrees` argument. For example, if &self is `"top:root:trunk:branch"` and `subtrees` is the
    /// following: `[("top:root:trunk", "twig")]`,  then resolving "top:root:trunk:twig:leaf" will
    /// return a sub-node from `subtrees[0]`.
    pub fn resolve_layered<SelfRefT: core::borrow::Borrow<Self>>(&self, subtrees: &[(&str, SelfRefT)], name: &str) -> Option<ModId> {
        self.name_to_node(subtrees, name).map(|node| node.mod_id)
    }

    /// Resolves a module name/path within &self, and additional subtrees layered in
    pub fn name_to_node<'a, 'slice, 'out, SelfRefT: core::borrow::Borrow<Self>>(&'a self, subtrees: &'slice[(&str, SelfRefT)], name: &str) -> Option<&'out Self>
    where 'a: 'out, 'slice: 'out
    {
        if name == TOP_MOD_NAME || name.len() == 0 {
            Some(self)
        } else {
            let map = self.build_overlay_map(subtrees).ok()?;
            let (parent_node, mod_name) = self.parse_parent_layered_internal(name, &map, subtrees).ok()?;

            if let Some(subtree_idx) = map.get(parent_node, mod_name) {
                Some(subtrees.get(subtree_idx).unwrap().1.borrow())
            } else {
                parent_node.get_child(mod_name)
            }
        }
    }

    /// Mutable version of [Self::name_to_node]
    #[allow(dead_code)] //NOTE: This function "feels" like it belongs in a complete API, but it's currently not needed in the upstream implementation
    pub fn name_to_node_mut<'a, 'b, 'slice, 'out>(&'a mut self, subtrees: &'slice mut[(&str, &'b mut Self)], name: &str) -> Option<&'out mut Self>
    where 'a: 'out, 'slice: 'out, 'b: 'out
    {
        _ = subtrees;
        _ = name;
        panic!("Need Polonius or Rust 2024 edition");
        // TODO: The below function should work unmodified in Rust 2024 edition, once the Polonius borrow checker is enabled by default
        // You can try it now on nightly by setting the RUSTFLAGS="-Zpolonius" environment variable
        // let subtree_idx = if name == TOP_MOD_NAME || name.len() == 0 {
        //     return Some(self);
        // } else {
        //     let map = self.build_overlay_map(subtrees).ok()?;
        //     let (parent_node, mod_name) = self.parse_parent_layered_internal_mut(name, &map, subtrees).ok()?;

        //     if let Some(subtree_idx) = map.get(parent_node, mod_name) {
        //         subtree_idx
        //     } else {
        //         return parent_node.get_child_mut(mod_name);
        //     }
        // };
        // Some(subtrees.get_mut(subtree_idx).unwrap().1)
    }

    /// Returns the node corresponding to any part of module name path before the last separator character,
    /// and the remaining substring
    #[allow(dead_code)] //NOTE: This function "feels" like it belongs in a complete API, but it's currently not needed in the upstream implementation because the upstream implementation goes straight to `parse_parent_layered_internal`
    pub fn parse_parent<'a, 'b>(&'a self, name: &'b str) -> Result<(&'a Self, &'b str), String> {
        self.parse_parent_layered::<&Self>(&[], name)
    }

    /// Same behavior as [parse_parent], but takes and returns a mutable reference
    pub fn parse_parent_mut<'a, 'b>(&'a mut self, name: &'b str) -> Result<(&'a mut Self, &'b str), String> {
        //NOTE: could also be implemented with parse_parent_layered_mut, but this way is less compiled code
        Self::parse_parent_generic(self, name, &OverlayMap::none(),
            |node, name| node.get_child_mut(name),
            |_, _| {}).map(|(node, _subtree_idx, remaining)| (node, remaining))
    }

    // //TODO-NOW This function is probably not needed
    // /// Parses a module name path into a canonical representation beginning with `top`
    // ///
    // /// Does not handle paths beginning with `self`
    // pub fn normalize_name_path(name: &str) -> Result<String, String> {
    //     if name == TOP_MOD_NAME {
    //         return Ok(TOP_MOD_NAME.to_string());
    //     }
    //     let mut new_name = TOP_MOD_NAME.to_string();
    //     let (_, _, last) = Self::parse_parent_generic(Self::top(), name, &OverlayMap::none(),
    //         |node, _| Some(node),
    //         |_, component| {new_name.push_str(":"); new_name.push_str(component)})?;
    //     if last.len() > 0 {
    //         new_name.push_str(":");
    //         new_name.push_str(last);
    //     }
    //     Ok(new_name)
    // }

    /// Internal generic `parse_parent` that can expand to mutable and const versions.
    /// Return valus is (parent_node, subtree_idx, remaining_name_path)
    /// If subtree_idx is None, the path was parsed to the end.  If it is Some, then the overlay_map
    ///  indicated we need to follow a subtree.
    fn parse_parent_generic<'a, SelfT, GetF, PushParentF>(gen_self: SelfT, name: &'a str, overlay_map: &OverlayMap, get_f: GetF, mut push_f: PushParentF) -> Result<(SelfT, Option<usize>, &'a str), String>
        where
        SelfT: core::borrow::Borrow<Self>,
        GetF: Fn(SelfT, &str) -> Option<SelfT>,
        PushParentF: FnMut(&mut SelfT, &'a str),
    {
        if name.len() == 0 {
            return Ok((gen_self, None, &name))
        }
        let mut cur_node = gen_self;
        let mut sym_start = 0;
        for (idx, the_char) in name.char_indices() {
            match the_char {
                MOD_NAME_SEPARATOR => {
                    let sym = &name[sym_start..idx];
                    if sym == TOP_MOD_NAME && sym_start == 0 {
                        sym_start = idx+1;
                        continue;
                    }
                    push_f(&mut cur_node, sym);
                    match overlay_map.get(cur_node.borrow(), sym) {
                        Some(subtree_idx) => {
                            sym_start = idx+1;
                            return if sym_start < name.len() {
                                Ok((cur_node, Some(subtree_idx), &name[sym_start..]))
                            } else {
                                Err(format!("Invalid module name format: {name}"))
                            };
                        },
                        None => {
                            match get_f(cur_node, sym) {
                                Some(new_node) => {
                                    cur_node = new_node;
                                    sym_start = idx+1;
                                },
                                None => return Err(format!("Parent module not loaded: {sym}"))
                            }
                        }
                    }
                },
                _ => {},
            }
        }
        if sym_start < name.len() {
            Ok((cur_node, None, &name[sym_start..]))
        } else {
            Err(format!("Invalid module name format: {name}"))
        }
    }

    /// Returns the node corresponding to any part of module name path before the last separator character,
    /// and the remaining substring, searching the composit tree
    pub fn parse_parent_layered<'a, 'slice, 'out, 'str, SelfRefT: core::borrow::Borrow<Self>>(&'a self, subtrees: &'slice[(&str, SelfRefT)], name: &'str str) -> Result<(&'out Self, &'str str), String>
    where 'a: 'out, 'slice: 'out
    {
        let map = self.build_overlay_map(subtrees)?;
        self.parse_parent_layered_internal(name, &map, subtrees)
    }

    /// Internal method to Build the map connecting each subtree branch to its location in the main tree
    fn build_overlay_map<'str, SelfRefT: core::borrow::Borrow<Self>>(&self, subtrees: &[(&'str str, SelfRefT)]) -> Result<OverlayMap<'str>, String> {
        let mut map = OverlayMap::with_capacity(subtrees.len());
        for (idx, (subtree_root_name, _subtree_root_node)) in subtrees.iter().enumerate() {
            let (parent_node, remaining_name) = self.parse_parent_layered_internal(subtree_root_name, &map, subtrees)?;
            map.push(parent_node, remaining_name, idx);
        }
        Ok(map)
    }

    /// Internal function to retry the parse, each time we move to a new sub-tree
    fn parse_parent_layered_internal<'a, 'slice, 'out, 'str, SelfT: core::borrow::Borrow<Self>>(&'a self, name: &'str str, overlay_map: &OverlayMap, subtrees: &'slice[(&str, SelfT)]) -> Result<(&'out Self, &'str str), String>
    where 'a: 'out, 'slice: 'out
    {
        let mut cur_node = self;
        let mut remaining_name = name;
        loop {
            let (parent_node, subtree_idx, remainder) = Self::parse_parent_generic(cur_node, remaining_name, overlay_map,
                |node, name| node.get_child(name),
                |_, _| {})?;

            match subtree_idx {
                None => {return Ok((parent_node, remainder))},
                Some(subtree_idx) => {
                    cur_node = subtrees.get(subtree_idx).unwrap().1.borrow();
                    remaining_name = remainder;
                }
            }
        }
    }

    /// Mutable version of [Self::parse_parent_layered]
    pub fn parse_parent_layered_mut<'a, 'b, 'slice, 'out, 'str>(&'a mut self, subtrees: &'slice mut[(&str, &'b mut Self)], name: &'str str) -> Result<(&'out mut Self, &'str str), String>
    where 'a: 'out, 'b: 'out, 'slice: 'out
    {
        let map = self.build_overlay_map(subtrees)?;
        self.parse_parent_layered_internal_mut(name, &map, subtrees)
    }

    /// Mutable version of [Self::parse_parent_layered_internal].  More backflips needed to keep mutable borrow
    fn parse_parent_layered_internal_mut<'a, 'b, 'slice, 'out, 'str>(&'a mut self, name: &'str str, overlay_map: &OverlayMap, subtrees: &'slice mut[(&str, &'b mut Self)]) -> Result<(&'out mut Self, &'str str), String>
    where 'a: 'out, 'b: 'out, 'slice: 'out
    {
        let mut cur_node = &*self;
        let mut remaining_name = name;
        let mut last_subtree_idx = None;
        let (subtree_idx, local_name_path) = loop {
            let (_parent_node, subtree_idx, remainder) = Self::parse_parent_generic(&*cur_node, remaining_name, overlay_map,
                |node, name| node.get_child(name),
                |_, _| {})?;

            match subtree_idx {
                None => {break (last_subtree_idx, remaining_name)},
                Some(subtree_idx) => {
                    cur_node = subtrees.get(subtree_idx).unwrap().1;
                    last_subtree_idx = Some(subtree_idx);
                    remaining_name = remainder;
                }
            }
        };

        let subtree_root = match subtree_idx {
            None => self,
            Some(subtree_idx) => subtrees.get_mut(subtree_idx).unwrap().1
        };
        subtree_root.parse_parent_mut(local_name_path)
    }
}

/// A wrapper that allows for parameterized Display of a ModNameNode
pub(crate) struct ModNameNodeDisplayWrapper<'a, F> {
    node_name: Option<&'a str>,
    node: &'a ModNameNode,
    indent: &'a str,
    mod_id_renderer: OwnedOrBorrowed<'a, F>,
}

impl<'a, F> ModNameNodeDisplayWrapper<'a, F>
    where F: Fn(ModId, &mut std::fmt::Formatter<'_>) -> std::fmt::Result
{
    const TEE: &'static str = " ├─";
    const CORNER: &'static str = " └─";
    const PIPE: &'static str = " │ ";
    const SPACE: &'static str = "   ";

    /// Make a new ModNameNodeDisplayWrapper, to display a name tree
    pub fn new(root_name: &'a str, node: &'a ModNameNode, mod_id_renderer: F) -> Self {
        Self {
            node_name: Some(root_name),
            node,
            indent: "",
            mod_id_renderer: OwnedOrBorrowed::from(mod_id_renderer),
        }
    }

    /// Internal function to make a ModNameNodeDisplayWrapper to display a subtree within a larger tree
    fn new_subtree(child_node: &'a ModNameNode, child_indent: &'a str, mod_id_renderer: &'a F) -> Self {
        Self {
            node_name: None,
            node: child_node,
            indent: child_indent,
            mod_id_renderer: OwnedOrBorrowed::from(mod_id_renderer),
        }
    }
}

impl<F> std::fmt::Display for ModNameNodeDisplayWrapper<'_, F> 
    where F: Fn(ModId, &mut std::fmt::Formatter<'_>) -> std::fmt::Result
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.node_name {
            Some(node_name) => {
                let subtree = ModNameNodeDisplayWrapper::new_subtree(self.node, self.indent, self.mod_id_renderer.as_ref());
                write!(f, "{node_name} = {subtree}")
            },
            None => {
                (self.mod_id_renderer.as_ref())(self.node.mod_id, f)?;
                writeln!(f)?;
                if let Some(children) = &self.node.children {
                    let mut iter = children.iter().peekable();
                    while let Some((child_name, child_node)) = iter.next() {
                        write!(f, "{}", self.indent)?;
                        let child_indent = if iter.peek().is_some() {
                            write!(f, "{}", Self::TEE)?;
                            format!("{}{}", self.indent, Self::PIPE)
                        } else {
                            write!(f, "{}", Self::CORNER)?;
                            format!("{}{}", self.indent, Self::SPACE)
                        };
                        let subtree = ModNameNodeDisplayWrapper::new_subtree(child_node, &child_indent, self.mod_id_renderer.as_ref());
                        write!(f, "{child_name} = {subtree}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for ModNameNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let wrapper = ModNameNodeDisplayWrapper::new(SELF_MOD_NAME, self, |mod_id: ModId, f: &mut std::fmt::Formatter| write!(f, "{}", mod_id.0));
        write!(f, "{wrapper}")
    }
}

/// Returns `true` if a str is a legal name for a module
///
/// A module name must be an ascii string, containing only alpha-numeric characters plus [`_`, `-`]
pub(crate) fn module_name_is_legal(name: &str) -> bool {
    for the_char in name.chars() {
        if !the_char.is_ascii() {
            return false;
        }
        if !the_char.is_ascii_alphanumeric() &&
            the_char != '-' &&
            the_char != '_' {
            return false;
        }
    }
    return true;
}

/// This test is narrowly focussed on the module namespace path parsing behavior implemented in
/// [ModNameNode], but it does not test any module operations
#[test]
fn module_name_parse_test() {
    let mut top = ModNameNode::top();

    assert!(top.add("top:sub1", ModId(1)).is_ok());
    assert!(top.add("sub2", ModId(2)).is_ok());
    assert!(top.add("sub2:suba", ModId(3)).is_ok());
    assert!(top.add("sub2:suba:subA", ModId(4)).is_ok());
    assert!(top.add("top:sub1:subb", ModId(5)).is_ok());

    assert!(top.add("", ModId(6)).is_err());
    assert!(top.add("top", ModId(6)).is_err());
    assert!(top.add("sub2::suba:subB", ModId(7)).is_err());
    assert!(top.add("sub2:suba:subB:", ModId(7)).is_err());

    assert_eq!(top.resolve("top").unwrap(), top.mod_id);
    assert_eq!(top.resolve("").unwrap(), top.mod_id);
    assert!(top.resolve("top:").is_none());
    assert!(top.resolve(":").is_none());

    assert_eq!(top.resolve("sub1").unwrap(), ModId(1));
    assert_eq!(top.resolve("top:sub2:suba:subA").unwrap(), ModId(4));
    assert!(top.resolve("sub2:suba:subA:").is_none());
    assert!(top.resolve("sub1:suba").is_none());

// //NOTE: HashMap internals make the order unpredictable so this is hard to test
// assert_eq!(format!("\n{top}"), r#"
// self = 0
//  ├─sub2 = 2
//  │  └─suba = 3
//  │     └─subA = 4
//  └─sub1 = 1
//     └─subb = 5
// "#);

}

/// This test tests multiple [ModNameNode] layered together to form a single tree
#[test]
fn module_name_layered_parse_test() {
    let mut top = ModNameNode::top();

    assert!(top.add("A", ModId(1)).is_ok());
    assert!(top.add("A:B", ModId(2)).is_ok());
    assert!(top.add("A:B:C", ModId(3)).is_ok());

    let mut branch = ModNameNode::new(ModId(10));
    assert!(branch.add("b", ModId(11)).is_ok());
    assert!(branch.add("b:c", ModId(12)).is_ok());
    assert!(branch.add("b:c:d", ModId(13)).is_ok());

    let mut leaf = ModNameNode::new(ModId(100));
    assert!(leaf.add("1", ModId(101)).is_ok());
    assert!(leaf.add("2", ModId(102)).is_ok());
    assert!(leaf.add("3", ModId(103)).is_ok());

    //Test one level of layering
    let subtrees = [("A:B:a", &branch)];
    assert_eq!(ModNameNode::resolve_layered(&top, &subtrees, "A:B:a").unwrap(), ModId(10));
    assert_eq!(ModNameNode::resolve_layered(&top, &subtrees, "A:B:a:b").unwrap(), ModId(11));
    assert_eq!(ModNameNode::resolve_layered(&top, &subtrees, "A:B:C").unwrap(), ModId(3));

    //Test two levels
    let subtrees = [("A:B:a", &branch), ("A:B:a:b:c:d:digits", &leaf)];
    assert_eq!(ModNameNode::resolve_layered(&top, &subtrees, "A:B:a:b:c:d:digits:2").unwrap(), ModId(102));

    //Test resolving to get mutable access
    let mut subtrees = [("A:B:a", &mut branch), ("A:B:a:b:c:d:digits", &mut leaf)];
    let (parent, _remaining_name) = ModNameNode::parse_parent_layered_mut(&mut top, &mut subtrees, "A:B:a:b:c:d:digits:1").unwrap();
    assert!(parent.add("1:one", ModId(1001)).is_ok());
    assert_eq!(ModNameNode::resolve_layered(&top, &subtrees, "A:B:a:b:c:d:digits:1:one").unwrap(), ModId(1001));
}

