use std::{path::PathBuf, time::Instant};
use clap::{Parser, Subcommand};
use null::{lex::Tokenizer, source, Error};

#[derive(Debug, Subcommand)]
enum Command {
    Tokenize {
        #[arg()]
        path: PathBuf 
    },
}


/// Full toolkit for the null programming language
#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Null {
    #[command(subcommand)]
    command: Command,
}

fn main() -> Result<(), Error> {
    let cli = Null::parse();

    match cli.command {
        Command::Tokenize { path } => {
            let source = std::fs::read_to_string(&path).unwrap();
            println!("PATH: {}", path.display());

            let tokenizer = Tokenizer::new(source.as_str());
            
            let lines = source.lines().count() as u32;

            let now = Instant::now();
            for token in tokenizer {
                if let Err(e) = token {
                    println!("{:?}", e.with_source_code(source!(&path, &source)))
                }
            }
            let duration = now.elapsed();
            println!("{} lines ~ {}s {:0>3}ms {:0>3}ns", lines, duration.as_secs() % 60, duration.as_millis() % 1000, duration.as_nanos() % 1000);
        }
    }
    Ok(())
}
