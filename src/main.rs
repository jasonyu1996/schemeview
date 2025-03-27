mod design;
mod serve;

fn main() {
    let args : Vec<String> = std::env::args().collect();

    let mut design = design::new_design();

    for arg in &args[2..] {
        eprintln!("Parsing {}", arg);
        design::parse_file(&mut design, &std::path::PathBuf::from(arg));
    }

    // sv_design::print_design(&design);

    // design::generate_graph(&design, &args[1], &mut std::io::stdout());

    let server = serve::Server::new("127.0.0.1:4002".to_string(), design, args[1].to_string());
    server.start();
}
