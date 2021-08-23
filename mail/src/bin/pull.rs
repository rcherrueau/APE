use std::{convert::TryInto, error::Error, future::Future, mem::MaybeUninit, path::PathBuf, process::{Command, Stdio}};

use futures::{FutureExt, StreamExt};

use std::io;
use notify_rust::{Notification, Timeout, Urgency, Hint};
use tokio::join;
use which::which;

fn find_exe_path(exe: &str) -> io::Result<PathBuf> {
    which(exe).map_err(|err| io::Error::new(
        io::ErrorKind::Other,
        format!("Could not find {} on your machine: {}", exe, err.to_string())))

}

/// Call mbsync to synchronize an email store
///
/// *Always succeed except if mbsync does not exists*
fn sync(store: &str) -> io::Result<()> {
    // mbsync executable path
    let mbsync_path = find_exe_path("mbsync")?;

    // Call mbsync on `store`
    let output = Command::new(mbsync_path.clone())
        .arg("--verbose")
        .arg(store)
        .output()?;

    if output.status.success() { // Sync runs smoothly
        // FIXME: Get the stdout and log it
        let stdout = String::from_utf8_lossy(&output.stdout);
        println!("Sync of {}: {}", store, stdout);

        // TODO: parse stdout to get Inbox changes and notify the user
        // ...

        return Ok(());

    } else { // An error occurs during sync
        // Get the error message and log it
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("Sync error for {}: {}", store, stderr);

        // Notify user about the synchronization error.
        Notification::new()
            .summary(&format!("📪 Sync error for {}", store))
            .body(&format!(
                "{}\n\
             Try '{} -V {}' to debug.",
                stderr, mbsync_path.as_path().display().to_string(), store))
            .hint(Hint::Category("email".to_owned()))
            .urgency(Urgency::Critical)
            .timeout(Timeout::Never)
            .show()
            .unwrap();

        return Ok(());
    }
}

async fn index_mail() -> io::Result<()> {
    // notmuch executable path
    let notmuch_path = find_exe_path("notmuch")?;

    // Call notmuch new to index new mails
    let output = Command::new(notmuch_path.clone())
        .arg("neww")
        .output()?;

    if output.status.success() { // Indexing runs smoothly
        // FIXME: Get the stdout and log it
        let stdout = String::from_utf8_lossy(&output.stdout);
        println!("Notmuch {}", stdout);
        return Ok(())
    } else {
        // Get the error message and log it let stderr = String::from_utf8_lossy(&output.stderr);
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("Err Notmuch {}", stderr);

        // TODO: return error
        // return Err(Box::new(std::io::Error::new(std::io::ErrorKind::Other, stderr)))
        // return io::Error::new(io::ErrorKind::Other, "notmuch failes")
        return Err(io::Error::last_os_error())
    }


}

/// Call
async fn tag_file(file: &str) -> io::Result<()> {
    // notmuch executable path
    let notmuch_path = find_exe_path("notmuch")?;

    // Call notmuch tag with the batch file
    let output = Command::new(notmuch_path.clone())
        .arg("tag")
        .arg("--batch")
        .arg(format!("--input={}", file))
        .output()?;

    if !output.status.success() {
        // Get the error message and log it
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("Err Notmuch {}", stderr);
    }


    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("Notmuch {}", stdout);

    return Ok(());
}

async fn sequence2<I, T, O>(xs: I) -> Vec<O>
where
    I: IntoIterator<Item = T>,
    T: Future<Output = O>,
    Vec<O>: Extend<T>,
{

    futures::stream::iter(xs).collect::<Vec<O>>().await

        // futures::stream::iter(xs).fo
    // let res = xs.into_iter().rfold(
    //     async { vec![] },
    //     move |acc, x| {
    //         let xx: io::Result<()> = x.await;
    //         let y: Vec<io::Result<()>> = acc.await;
    //         y.push(xx)
    // });

    // res

}


async fn traverse<I, T, F, Fut, O>(xs: I, f: F) -> Vec<O>
where
    I: IntoIterator<Item = T>,
    F: Fn(T) -> Fut,
    Fut: Future<Output = O>,
{
    futures::stream::iter(xs).then(f).collect().await
}

async fn sequence<I, T, O>(xs: I) -> Vec<O>
where
    I: IntoIterator<Item = T>,
    T: futures::Future<Output = O>,
{
    traverse(xs, |a|{a}).await
}


async fn app() {
    let syncs = sequence(vec![
        tokio::task::spawn_blocking(move || sync("IMT-inbox")),
        tokio::task::spawn_blocking(move || sync("Inria-inbox")),
        tokio::task::spawn_blocking(move || sync("IMT-store")),
        tokio::task::spawn_blocking(move || sync("Inria-store")),
    ]);




    // let elems = syncs.into_iter().map()

    // syncs.and_then(|_| index_mail())


    // let handles = syncs.into_iter().map(async_std::task::spawn);

    // join_all(handles).await
    //     .and_then(|_| index_mail())


}

#[tokio::main]
async fn main() {
    let syncs = sequence(vec![
        tokio::task::spawn_blocking(move || sync("IMT-inbox")),
        tokio::task::spawn_blocking(move || sync("Inria-inbox")),
        tokio::task::spawn_blocking(move || sync("IMT-store")),
        tokio::task::spawn_blocking(move || sync("Inria-store")),
    ]);

    // let syncs = vec![
    //     sync("IMT-inbox"),
    //     sync("Inria-inbox"),
    //     sync("IMT-store"),
    //     sync("Inria-store"),
    // ].into_iter().map(|f| {
    //     tokio::task::spawn_blocking(move || f)
    // }).collect::<Vec<_>>();

    // join()

    // let syncs = vec![
    //     sync("IMT-inbox"),
    //     sync("Inria-inbox"),
    //     sync("IMT-store"),
    //     sync("Inria-store"),
    // ];

    // let handles = syncs.into_iter().map(async_std::task::spawn);

    // // let app =
    // //     // Sync all inbox concurrently
    // //     join_all(handles)
    // //     // join_all(syncs)
    // //     // Tag new mails with notmuch
    // //     .then(|_| index_mail())
    // //     .then(|_| tag_file("/nix/store/z1azzwys1y9cx2lk9xznc2xcs72h0yyr-notmuch-tag-mail"));

    // block_on(app);
}
