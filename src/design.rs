use std::{collections::HashMap, fmt::format, io::Write, path::Path};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct Port {
    pub is_output : bool
}

#[derive(Serialize, Deserialize)]
pub struct Instance {
    pub module_name : String,
    pub instance_name : String,
    pub connections : Vec<(String, String)>
}

#[derive(Serialize, Deserialize)]
pub struct Module {
    pub name : String,
    pub ports : HashMap<String, Port>,
    pub instances : Vec<Instance>
}

#[derive(Serialize, Deserialize)]
pub struct Design {
    pub modules : HashMap<String, Module>,
}

pub mod design_build {
    use std::collections::HashMap;
    use std::path::{Path, PathBuf};
    use core::panic;
    use sv_parser::{parse_sv, unwrap_node, Expression, ListOfPortConnections, Locate, NamedPortConnection, PortDirection, Primary, RefNode, SyntaxTree};
    use super::*;

    fn get_identifier(node: RefNode) -> Option<Locate> {
        // unwrap_node! can take multiple types
        match unwrap_node!(node, SimpleIdentifier, EscapedIdentifier) {
            Some(RefNode::SimpleIdentifier(x)) => {
                return Some(x.nodes.0);
            }
            Some(RefNode::EscapedIdentifier(x)) => {
                return Some(x.nodes.0);
            }
            _ => None,
        }
    }

    fn parse_module_declaration<'a>(syntax_tree : &SyntaxTree, node : RefNode<'_>) -> Module {
        let mod_ident = unwrap_node!(node.clone(), ModuleIdentifier).unwrap();
        let mod_ident = get_identifier(mod_ident).unwrap();
        let module_name = syntax_tree.get_str(&mod_ident).unwrap().to_string();
        let list_ports = unwrap_node!(node.clone(), ListOfPortDeclarations);
        let mut ports = HashMap::new();
        if let Some(RefNode::ListOfPortDeclarations(list_ports)) = list_ports {
            let list_ports = list_ports.nodes.0.nodes.1.as_ref();
            if let Some(list_ports) = list_ports {
                for port_decl in list_ports.contents() {
                    let port_dir = unwrap_node!(RefNode::from(&port_decl.1), PortDirection);
                    let port_is_output = port_dir.is_some_and(|port_dir| matches!(port_dir, RefNode::PortDirection(PortDirection::Output(_))));
                    let port_ident = unwrap_node!(RefNode::from(&port_decl.1), PortIdentifier).unwrap();
                    let port_ident = get_identifier(port_ident).unwrap();
                    ports.insert(syntax_tree.get_str(&port_ident).unwrap().to_string(), Port { is_output : port_is_output });
                }
            } else {
                eprintln!("Warning: no ports found!");
            }

        } else {
            eprintln!("Warning: no ports found!");
        }
        Module {
            name : module_name,
            ports : ports,
            instances : vec![]
        }
    }

    fn try_add_module(design : &mut Design, module : &mut Option<Module>) {
        if let Some(module) = module.take() {
            design.modules.insert(module.name.clone(), module);
        }
    }

    fn parse_module_instantiation(syntax_tree : &SyntaxTree, node : RefNode<'_>) -> Instance {
        let module_name = syntax_tree.get_str(&get_identifier(node.clone()).unwrap()).unwrap().to_string();
        let instance_ident = get_identifier(unwrap_node!(node.clone(), InstanceIdentifier).unwrap()).unwrap();
        let instance_ident = syntax_tree.get_str(&instance_ident).unwrap().to_string();
        let port_conns = unwrap_node!(node.clone(), ListOfPortConnections).unwrap();
        let mut connected_ports = vec![];
        match port_conns {
            RefNode::ListOfPortConnections(port_conns_list) => {
                match port_conns_list {
                    ListOfPortConnections::Named(named) => {
                        for port_conn in named.nodes.0.contents() {
                            match port_conn {
                                NamedPortConnection::Identifier(named_port_ident) => {
                                    let named_port_ident_node = RefNode::from(&named_port_ident.nodes.2);
                                    let port_ident = get_identifier(named_port_ident_node).unwrap();
                                    let port_ident = syntax_tree.get_str(&port_ident).unwrap().to_string();
                                    let wire_ident = {
                                        let expr = named_port_ident.nodes.3.as_ref();
                                        let expr = expr.and_then(|v| v.nodes.1.as_ref());
                                        match expr {
                                            Some(Expression::Primary(primary_expr)) => {
                                                match primary_expr.as_ref() {
                                                    Primary::Hierarchical(h) => {
                                                        let ident = get_identifier(RefNode::from(h.as_ref())).unwrap();
                                                        syntax_tree.get_str(&ident).unwrap().to_string()
                                                    }
                                                    _ => port_ident.clone()
                                                }
                                            }
                                            _ => port_ident.clone()
                                        }
                                    };
                                    connected_ports.push((port_ident, wire_ident));
                                }
                                NamedPortConnection::Asterisk(_) => {
                                    eprintln!("Warning: Asterisk unsupported!")
                                }
                            }
                        }
                    }
                    ListOfPortConnections::Ordered(_) => eprintln!("Warning: Ordered port connections unsupported!")
                }
            }
            _ => panic!("Something wrong!")
        }
        Instance {
            module_name : module_name,
            instance_name : instance_ident,
            connections : connected_ports
        }
    }

    pub fn parse_file<T : AsRef<Path>, U : AsRef<Path>>(design : &mut Design, includes : &[U], path: T) {
        match parse_sv(path, &HashMap::new(), includes,
                            false, false) {
            Ok((syntax_tree, _)) => {
                let mut current_module : Option<Module> = None;
                for node in &syntax_tree {
                    match node {
                        RefNode::ModuleDeclarationAnsi(_) => {
                            try_add_module(design, &mut current_module);
                            current_module = Some(parse_module_declaration(&syntax_tree, node));
                        }
                        RefNode::ModuleInstantiation(_mod_inst) => {
                            if let Some(module) = current_module.as_mut() {
                                let instance = parse_module_instantiation(&syntax_tree, node);
                                module.instances.push(instance);
                            } else {
                                panic!("Bad!")
                            }
                        }
                        _ => ()
                    }
                }
                try_add_module(design, &mut current_module);
            }
            Err(err) => {
                eprintln!("Failed to parse!");
                eprintln!("{}", err);
            }
        }

    }

}

pub fn print_design(design : &Design) {
    for (module_name, module) in &design.modules {
        println!("Module {}:", module_name);
        for (port_name, port) in &module.ports {
            let direction = if port.is_output { "output" } else { "input" };
            println!("- {} {}", direction, port_name);
        }
        for instance in &module.instances {
            println!("* {} {}", instance.module_name, instance.instance_name);
            for (port_name, conn_name) in &instance.connections {
                println!(" > {} - {}", conn_name, port_name);
            }
        }
    }
}

pub use design_build::parse_file;

pub fn new_design() -> Design {
    Design {
        modules : HashMap::new()
    }
}

struct Wire<'a> {
    to : Vec<(u32, &'a str, &'a str)>,
    from : Vec<(u32, &'a str, &'a str)>
}

struct GraphGenContext<'a, T : Write> {
    out : &'a mut T,
    last_index_cluster : u32,
    ports : Vec<Wire<'a>>,
    indent_level : u32,
    current_path : String,
    path_segs : Vec<usize>
}

impl<'a, T : Write> GraphGenContext<'a, T> {
    pub fn new(out : &'a mut T, current_path : String) -> Self {
        Self {
            out,
            ports : vec![],
            last_index_cluster : 0,
            indent_level : 0,
            current_path,
            path_segs : vec![]
        }
    }

    pub fn alloc_node(&mut self) -> u32 {
        let r = self.ports.len();
        self.ports.push(Wire { to : vec![], from : vec![] });
        r as u32
    }

    pub fn get_node(&self, id : u32) -> &Wire<'a> {
        &self.ports[id as usize]
    }

    pub fn get_node_mut(&mut self, id : u32) -> &mut Wire<'a> {
        &mut self.ports[id as usize]
    }

    pub fn ports_iter(&self) -> std::slice::Iter<'_, Wire> {
        self.ports.iter()
    }

    pub fn connect_nodes(&mut self, id_from : u32, instance_from : &'a str, port_from : &'a str,
                                    id_to : u32, instance_to : &'a str, port_to : &'a str)
    {
        self.get_node_mut(id_from).to.push((id_to, instance_to, port_to));
        self.get_node_mut(id_to).from.push((id_from, instance_from, port_from));
    }

    pub fn alloc_cluster(&mut self) -> u32 {
        let r = self.last_index_cluster;
        self.last_index_cluster += 1;
        r
    }

    pub fn print_line(&mut self, s : &str) {
        for _ in 0..self.indent_level {
            write!(self.out, " ").unwrap();
        }
        writeln!(self.out, "{}", s).unwrap();
    }

    pub fn inc_indent(&mut self, delta : u32) {
        self.indent_level += delta;
    }

    pub fn dec_indent(&mut self, delta : u32) {
        self.indent_level -= delta;
    }

    pub fn push_path_seg(&mut self, seg : &str) {
        self.path_segs.push(self.current_path.len());
        self.current_path.push('.');
        self.current_path.push_str(seg);
    }

    pub fn pop_path_seg(&mut self) {
        let len = self.path_segs.pop().unwrap();
        self.current_path.truncate(len);
    }

    pub fn last_seg(&self, offset : usize) -> Option<&str> {
        let slen = self.path_segs.len();
        if slen <= offset {
            None
        } else if offset == 0 {
            // + 1 to remove the period (.)
            Some(&self.current_path[(self.path_segs[slen - 1] + 1)..])
        } else {
            Some(&self.current_path[(self.path_segs[slen - offset - 1] + 1)..self.path_segs[slen - offset]])
        }
    }
}

fn generate_module_instance<'d, T : Write>(context : &mut GraphGenContext<'d, T>, design : &'d Design, module : &'d Module, instance_name : &'d str, depth_lim : u32) -> HashMap<String, u32>{
    if depth_lim == 0 {
        return HashMap::new();
    }

    let cluster_id = context.alloc_cluster();
    context.print_line(&format!("subgraph cluster_{} {{", cluster_id));
    context.inc_indent(1);

    context.print_line(&format!("URL = \"javascript:scheme_view_goto('{}')\";", &context.current_path));
    context.print_line("target = \"_top\";");
    context.print_line("cluster = true;");
    context.print_line("style = filled;");
    let fill_color = if depth_lim % 2 == 0 { "fillcolor = lightgrey;" } else { "fillcolor = white;" };
    context.print_line(fill_color);
    context.print_line(&format!("label = <<b>{}</b>: {}>;", module.name, instance_name));
    context.print_line(&format!("tooltip = \"{}: {}\";", &module.name, &context.current_path));

    /* Generate the ports */
    let mut input_cnt = 0;
    let mut output_cnt = 0;

    let mut input_ports = vec![];
    let mut output_ports = vec![];
    let port_nodes : HashMap<String, u32> = module.ports.iter().map(
        |(port_name, port)| {
            let id = context.alloc_node();
            if port.is_output {
                output_ports.push((port_name, port));
            } else {
                input_ports.push((port_name, port));
            }
            let (color, col, row) =
                if port.is_output {
                    let r = output_cnt;
                    output_cnt += 1;
                    ("orange", 10, r)
                } else {
                    let r = input_cnt;
                    input_cnt += 1;
                    ("lightblue", 0, r)
                };
            // context.print_line(&format!("w{} [label=\"{}\" shape={} pos=\"{},{}!\"];", id, port_name, shape, col, row));
            (port_name.clone(), id)
        }
    ).collect();

    for (color, ports) in [("orange", output_ports), ("lightblue", input_ports)] {
        let cluster_id = context.alloc_cluster();
        context.print_line(&format!("subgraph cluster_{} {{", cluster_id));
        context.inc_indent(1);
        context.print_line("style = invis;");
        for (port_name, _port) in ports {
            let id = *port_nodes.get(port_name).unwrap();
            context.print_line(&format!("w{} [label=\"{}\" shape=rect color=none fillcolor={} style=filled];", id, port_name, color));
        }

        context.dec_indent(1);
        context.print_line("}");
    }

    let mut wires : HashMap<String, Wire> = HashMap::new();
    for instance in &module.instances {
        if let Some(instance_module) = design.modules.get(&instance.module_name) {
            context.push_path_seg(&instance.instance_name);
            let instance_ports = generate_module_instance(context, design, instance_module, &instance.instance_name, depth_lim - 1);
            context.pop_path_seg();
            /* connect the ports */
            for (port_name, conn_name) in &instance.connections {
                if let Some(port_id) = instance_ports.get(port_name) {
                    /* check if conn_name matches a port of the outside module */
                    if let Some(ex_port_id) = port_nodes.get(conn_name) {
                        if instance_module.ports.get(port_name).unwrap().is_output {
                            context.print_line(&format!("w{} -> w{} [tooltip=\"{}.{} -> {}.{}\"];",
                                *port_id, *ex_port_id, &instance.instance_name, port_name, instance_name, conn_name));
                            context.connect_nodes(*port_id, &instance.instance_name, port_name,
                                *ex_port_id, instance_name, conn_name);
                        } else {
                            context.print_line(&format!("w{} -> w{} [tooltip=\"{}.{} -> {}.{}\"];",
                                *ex_port_id, *port_id, instance_name, conn_name, &instance.instance_name, port_name));
                            context.connect_nodes(*ex_port_id, instance_name, conn_name,
                                *port_id, &instance.instance_name, port_name);
                        }
                    } else if let Some(wire) = wires.get_mut(conn_name) {
                        if instance_module.ports.get(port_name).unwrap().is_output {
                            wire.from.push((*port_id, &instance.instance_name, port_name));
                        } else {
                            wire.to.push((*port_id, &instance.instance_name, port_name));
                        }
                    } else {
                        /* create new wire */
                        wires.insert(conn_name.clone(), Wire { from : vec![], to : vec![] });
                    };
                }
            }
        }
    }
    for (_, wire) in wires {
        if !wire.to.is_empty() && !wire.from.is_empty() {
            // let id = context.alloc_node();
            // context.print_line(&format!("w{} [shape=point];", id));
            for (from_node, from_instance, from_name) in wire.from {
                // context.print_line(&format!("w{} -> w{};", *from_node, id));
                for (to_node, to_instance, to_name) in &wire.to {
                    context.print_line(&format!("w{} -> w{} [tooltip=\"{}.{} -> {}.{}\"];",
                        from_node, to_node, from_instance, from_name, to_instance, to_name));
                    context.connect_nodes(from_node, from_instance, from_name,
                        *to_node, *to_instance, *to_name);
                }
            }
        }
    }

    context.dec_indent(1);
    context.print_line("}");

    port_nodes
}

pub fn generate_graph_part<'d, 'm, T : Write>(design : &'d Design, top_module : &'m Module, module_path : &str, depth_lim : u32, out : &mut T) where 'm : 'd {
    let mut context = GraphGenContext::new(out, module_path.to_string());
    let top_instance_name = module_path.rsplitn(2, ".").last().unwrap();
    context.print_line("digraph {");
    context.inc_indent(1);
    context.print_line("layout = fdp;");
    context.print_line("splines = true;");
    let _ = generate_module_instance(&mut context, design, top_module, top_instance_name, depth_lim);
    // generate tool tips
    let ports = std::mem::take(&mut context.ports);
    for (id, port) in ports.iter().enumerate() {
        fn build_instance_list<'a, 'b, T : Iterator<Item = &'b (u32, &'a str, &'a str)>>(mut v : T) -> String where 'a : 'b {
            let mut res = String::new();
            if let Some((_, inst_name, _)) = v.next() {
                res.push_str(*inst_name);
                for (_, inst_name, _) in v {
                    res.push_str(", ");
                    res.push_str(*inst_name);
                }
            }
            res
        }
        let tooltip_s =
            match (port.from.is_empty(), port.to.is_empty()) {
                (true, true) => "(unconnected)",
                (true, false) => &format!("To: {}", build_instance_list(port.to.iter())),
                (false, true) => &format!("From: {}", build_instance_list(port.from.iter())),
                (false, false) => &format!("To: {}; From: {}",
                    build_instance_list(port.to.iter()),
                    build_instance_list(port.from.iter()))
            };
        context.print_line(&format!("w{} [tooltip=\"{}\"]", id, tooltip_s));
    }
    context.dec_indent(1);
    context.print_line("}");
}

pub fn locate_module<'a>(design : &'a Design, top_module_name : &str, module_path : &str) -> Option<&'a Module> {
    if module_path == "Top" || module_path.starts_with("Top.") {
        let path_segments = module_path.split(".");
        match design.modules.get(top_module_name) {
            Some(module) => {
                let mut module = module;
                for path_segment in path_segments.skip(1) {
                    let next_module = module.instances.iter()
                        .find_map(|instance|
                            if instance.instance_name == path_segment {
                                design.modules.get(&instance.module_name)
                            } else {
                                None
                            }
                        );
                    match next_module {
                        Some(next_module) => module = next_module,
                        None => return None
                    }
                }
                Some(module)
            }
            None => None
        }
    } else {
        None
    }
}


// Serialisation

pub fn save_to_archive<T : AsRef<Path>>(design : &Design, out_file : T) -> std::io::Result<()> {
    let mut out = std::fs::File::create(out_file)?;
    serde_json::to_writer(&mut out, &design)?;
    Ok(())
}

pub fn load_from_archive<T : AsRef<Path>>(in_file : T) -> std::io::Result<Design> {
    let mut in_f = std::fs::File::open(in_file)?;
    let v = serde_json::from_reader(&mut in_f)?;
    Ok(v)
}
