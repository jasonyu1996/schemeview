use std::path::PathBuf;

use clap::Parser;

mod design;
mod serve;

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct CliArgs {
    #[command(subcommand)]
    command : CliCommand
}

#[derive(clap::Subcommand)]
enum CliCommand {
    Generate {
        /// include paths
        #[arg(short = 'I')]
        includes : Vec<PathBuf>,
        #[arg(short)]
        out_file : PathBuf,
        /// source files
        sources : Vec<PathBuf>
    },
    Serve {
        #[arg(long, default_value = "127.0.0.1:8000")]
        listen : String,
        #[arg(long, default_value_t = false)]
        keep_cache : bool,
        in_file : PathBuf,
        top_module_name : String
    },
    ClearCache
}

fn main() {
    let cli_args = CliArgs::parse();
    match cli_args.command {
        CliCommand::Generate { includes, out_file, sources } => {
            let mut design = design::new_design();

            for src_file in &sources {
                eprintln!("Parsing {}", src_file.to_str().unwrap());
                design::parse_file(&mut design, &includes, src_file);
            }

            eprintln!("Saving to file {} ...", out_file.to_str().unwrap());
            design::save_to_archive(&design, &out_file).expect("Failed to save to file!");
            eprintln!("Done");
        }
        CliCommand::Serve { listen, keep_cache, in_file, top_module_name } => {
            eprintln!("Loading from {} ...", in_file.to_str().unwrap());
            let design = design::load_from_archive(&in_file).expect("Failed to load from file!");
            eprintln!("Done");
            let server = serve::Server::new(listen, design, top_module_name)
                .expect("Failed to start server!");
            server.start(keep_cache);
        }
        CliCommand::ClearCache => {
            let _ = std::fs::remove_dir_all("./cache");
        }
    }
}
