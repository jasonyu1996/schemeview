extern crate rouille;

use std::cell::RefCell;
use std::io::{BufWriter, Write};
use std::net::{SocketAddr, ToSocketAddrs};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};
use std::{collections::HashSet, fs::File, io::Read};
use std::path::{Path, PathBuf};

use rouille::{Response, Request};

use crate::design::Design;

pub struct Server {
    top_module_name : String,
    listen_at : SocketAddr,
    design : Design,
}


impl Server {
    pub fn new<T : ToSocketAddrs>(listen_at : T, design : Design, top_module_name : String) -> std::io::Result<Self> {
        let listen_at = listen_at.to_socket_addrs()?.next()
            .ok_or(std::io::Error::new(std::io::ErrorKind::Other, "Invalid socket address!"))?;
        Ok(
            Self {
                top_module_name,
                listen_at,
                design
            }
        )
    }

    fn read_file<T : AsRef<Path>>(path : T) -> Vec<u8> {
        let mut file = File::open(path).expect("Failed to open file!");
        let mut res = vec![];
        file.read_to_end(&mut res).expect("Error reading file!");
        res
    }

    pub fn start(self, keep_cache : bool) {
        let is_stopping = Arc::new(AtomicBool::new(false));
        let is_stopping_clone = is_stopping.clone();

        ctrlc::set_handler(move || {
            is_stopping_clone.store(true, Ordering::SeqCst);
        }).unwrap();
        std::fs::create_dir_all("./cache").expect("Failed to create cache directory!");

        let mut cached = HashSet::new();
        // check the existing files
        for f in Path::new("./cache").read_dir()
            .expect("Failed to read cache directory!")
        {
            if let Ok(f) = f {
                if let Ok(f_metadata) = f.metadata() {
                    if f_metadata.is_file() {
                        if let Ok(s) = f.file_name().into_string() {
                            cached.insert(s);
                        }
                    }
                }
            }
        }
        let cached = Arc::new(Mutex::new(cached));

        let ui_data = Self::read_file("ui/index.html");
        let css_data = Self::read_file("ui/style.css");
        let server = rouille::Server::new(self.listen_at, move |request| {
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
                        let mut dot_proc = Command::new("dot")
                            .args(["-o", cache_path.to_str().unwrap(), "-Tsvg", "/dev/stdin"])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::null())
                            .spawn()
                            .expect("Failed to spawn graphviz!");
                        let dot_proc_stdin = dot_proc.stdin.take().expect("Failed to open pipe to graphviz!");
                        let mut dot_proc_stdin = BufWriter::new(dot_proc_stdin);
                        crate::design::generate_graph_part(&self.design, module, module_path, 2, &mut dot_proc_stdin);
                        dot_proc_stdin.flush().unwrap();
                        drop(dot_proc_stdin);
                        let dot_proc_status = dot_proc.wait().expect("Failed to run graphviz!");
                        if dot_proc_status.success() {
                            eprintln!("Module {} processed!", module_path);
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
        }).unwrap();

        eprintln!("Server started at {}", server.server_addr());

        while !is_stopping.load(Ordering::SeqCst) {
            server.poll();
        }

        eprintln!("Shutting down server ...");

        // clear up cache
        if !keep_cache {
            let _ = std::fs::remove_dir_all("./cache");
        }
    }
}

