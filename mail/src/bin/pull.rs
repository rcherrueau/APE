use std::io;

use async_process::{Child, Command, ExitStatus, Stdio};
use futures::executor::block_on;

/// Call mbsync to synchronize one email store
async fn sync(store: &str) -> () {
    // io::Result<ExitStatus> {
    let cmd = format!("mbsync {}-store", store);

    let proc = Command::new(cmd)
        .stderr(Stdio::piped())
        .output()
        .await
        .expect("Failed to execute mbsync");

    // There is an error
    if !proc.status.success() {
        let stderr = proc.stderr;
        // Display the error, call libnotify
        println!("stderr: {}", String::from_utf8_lossy(&stderr));
    }


    // proc.and_then(|child| -> () {})

}

fn main() {
    let future = sync("IMT");

    block_on(future);
}
