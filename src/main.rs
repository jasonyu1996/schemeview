
mod sv_design {
    use std::{collections::HashMap, fmt::format};
    pub struct Port {
        pub is_output : bool
    }

    pub struct Instance {
        pub module_name : String,
        pub instance_name : String,
        pub connections : Vec<(String, String)>
    }

    pub struct Module {
        pub name : String,
        pub ports : HashMap<String, Port>,
        pub instances : Vec<Instance>
    }

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

        pub fn parse_file<T : AsRef<Path>>(design : &mut Design, path: T) {
            match parse_sv(path, &HashMap::new(), &[PathBuf::from("../capstone-ariane/core/include")] as &[PathBuf],
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

    struct GraphGenContext {
        last_index_cluster : u32,
        last_index : u32,
        indent_level : u32
    }

    impl GraphGenContext {
        pub fn alloc_node(&mut self) -> u32 {
            let r = self.last_index;
            self.last_index += 1;
            r
        }

        pub fn alloc_cluster(&mut self) -> u32 {
            let r = self.last_index_cluster;
            self.last_index_cluster += 1;
            r
        }

        pub fn print_line(&self, s : &str) {
            for _ in 0..self.indent_level {
                print!(" ");
            }
            println!("{}", s);
        }

        pub fn inc_indent(&mut self, delta : u32) {
            self.indent_level += delta;
        }

        pub fn dec_indent(&mut self, delta : u32) {
            self.indent_level -= delta;
        }
    }

    struct Wire {
        to : Vec<u32>,
        from : Vec<u32>
    }

    fn generate_module_instance(context : &mut GraphGenContext, design : &Design, module : &Module, instance_name : &str, depth_lim : u32) -> HashMap<String, u32>{
        if depth_lim == 0 {
            return HashMap::new();
        }

        let cluster_id = context.alloc_cluster();
        context.print_line(&format!("subgraph cluster_{} {{", cluster_id));
        context.inc_indent(1);

        context.print_line("cluster = true;");
        context.print_line("style = filled;");
        let fill_color = if depth_lim % 2 == 0 { "fillcolor = lightgrey;" } else { "fillcolor = white;" };
        context.print_line(fill_color);
        context.print_line(&format!("label = <<b>{}</b>: {}>;", module.name, instance_name));

        /* Generate the ports */
        let mut input_cnt = 0;
        let mut output_cnt = 0;
        let port_nodes : HashMap<String, u32> = module.ports.iter().map(
            |(port_name, port)| {
                let id = context.alloc_node();
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
                context.print_line(&format!("w{} [label=\"{}\" shape=rect color=none fillcolor={} style=filled];", id, port_name, color));
                (port_name.clone(), id)
            }
        ).collect();

        let mut wires : HashMap<String, Wire> = HashMap::new();
        for instance in &module.instances {
            if let Some(instance_module) = design.modules.get(&instance.module_name) {
                let instance_ports = generate_module_instance(context, design, instance_module, &instance.instance_name, depth_lim - 1);
                /* connect the ports */
                for (port_name, conn_name) in &instance.connections {
                    if let Some(port_id) = instance_ports.get(port_name) {
                        /* check if conn_name matches a port of the outside module */
                        if let Some(ex_port_id) = port_nodes.get(conn_name) {
                            if instance_module.ports.get(port_name).unwrap().is_output {
                                context.print_line(&format!("w{} -> w{};", *port_id, *ex_port_id));
                            } else {
                                context.print_line(&format!("w{} -> w{};", *ex_port_id, *port_id));
                            }
                        } else if let Some(wire) = wires.get_mut(conn_name) {
                            if instance_module.ports.get(port_name).unwrap().is_output {
                                wire.from.push(*port_id);
                            } else {
                                wire.to.push(*port_id);
                            }
                        } else {
                            /* create new wire */
                            wires.insert(conn_name.clone(), Wire { from : vec![], to : vec![] });
                        };
                    }
                }
            }
        }
        for (_, wire) in &wires {
            if !wire.to.is_empty() && !wire.from.is_empty() {
                // let id = context.alloc_node();
                // context.print_line(&format!("w{} [shape=point];", id));
                for from_node in &wire.from {
                    // context.print_line(&format!("w{} -> w{};", *from_node, id));
                    for to_node in &wire.to {
                        context.print_line(&format!("w{} -> w{};", *from_node, *to_node));
                    }
                }
            }
        }

        context.dec_indent(1);
        context.print_line("}");

        port_nodes
    }

    const GRAPH_DEPTH_LIM : u32 = 8;
    pub fn generate_graph(design : &Design, top_module_name : &str) {
        match design.modules.get(top_module_name) {
            Some(top_module) => {
                let mut context = GraphGenContext { last_index_cluster : 0, last_index : 0, indent_level : 0 };
                context.print_line("digraph {");
                context.inc_indent(1);
                context.print_line("layout = osage;");
                let _ = generate_module_instance(&mut context, design, top_module, "Top", GRAPH_DEPTH_LIM);
                context.dec_indent(1);
                context.print_line("}");
            }
            None => eprintln!("Top module {} not found!", top_module_name)
        }
    }

}

fn main() {
    let args : Vec<String> = std::env::args().collect();

    let mut design = sv_design::new_design();

    for arg in &args[2..] {
        eprintln!("Parsing {}", arg);
        sv_design::parse_file(&mut design, &std::path::PathBuf::from(arg));
    }

    // sv_design::print_design(&design);

    sv_design::generate_graph(&design, &args[1]);
}
