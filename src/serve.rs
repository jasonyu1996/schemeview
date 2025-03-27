extern crate rouille;

use std::cell::RefCell;
use std::io::BufWriter;
use std::process::Command;
use std::sync::{Arc, Mutex, RwLock};
use std::{collections::HashSet, fs::File, io::Read};
use std::path::{Path, PathBuf};

use rouille::{Response, Request};

use crate::design::Design;

pub struct Server {
    top_module_name : String,
    listen_at : String,
    design : Design,
}


impl Server {
    pub fn new(listen_at : String, design : Design, top_module_name : String) -> Self {
        Self {
            top_module_name,
            listen_at,
            design
        }
    }

    fn read_file<T : AsRef<Path>>(path : T) -> Vec<u8> {
        let mut file = File::open(path).expect("Failed to open file!");
        let mut res = vec![];
        file.read_to_end(&mut res).expect("Error reading file!");
        res
    }

    pub fn start(self) {
        let cached = Arc::new(Mutex::new(HashSet::new()));
        std::fs::create_dir_all("./cache").expect("Failed to create cache directory!");
        let ui_data = Self::read_file("ui/index.html");
        let css_data = Self::read_file("ui/style.css");
        rouille::start_server(self.listen_at, move |request| {
            let url = request.url();
            if url == "/" {
                Response::from_data("text/html", ui_data.clone())
            } else if url == "/style.css" {
                Response::from_data("text/css", css_data.clone())
            } else if url.starts_with("/graph/") {
                let module_path = &url[7..];
                let cache_path : PathBuf = ["./cache", module_path].iter().collect();
                let mut cache_lock = cached.lock().unwrap();
                if !cache_lock.contains(module_path) {
                    /* add to cache */
                    eprintln!("Cache miss: {}", module_path);
                    let module = crate::design::locate_module(&self.design, &self.top_module_name, module_path);
                    if let Some(module) = module {
                        let mut out = BufWriter::new(File::create("tmp.dot").expect("Failed to open temp file"));
                        crate::design::generate_graph_part(&self.design, module, module_path, 2, &mut out);
                        drop(out);
                        let dot_cmd_status = Command::new("dot")
                            .args(["-O", "-Tsvg", "tmp.dot"])
                            .status()
                            .expect("Fail to run graphviz!");
                        if dot_cmd_status.success() {
                            /* copy the output */
                            std::fs::copy("tmp.dot.svg", &cache_path).expect("Failed to copy output file!");
                            /* add to cache */
                            cache_lock.insert(module_path.to_string());
                        } else {
                            eprintln!("Module {} process failed!", module_path);
                            return Response::text("").with_status_code(500)
                        }
                    } else {
                        eprintln!("Module {} not found!", module_path);
                        return Response::empty_404()
                    }
                } else {
                    eprintln!("Cache hit: {}", module_path);
                }
                let data = Self::read_file(&cache_path);
                let resp = Response::from_data("image/svg+xml", data);
                resp
            } else {
                Response::empty_404()
            }
        });
    }
}

