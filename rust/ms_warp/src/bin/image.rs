#![feature(iter_intersperse)]

use warp::Filter;
use ms_warp::auth;

// Mimic https://docs.openstack.org/api-ref/image/v2/index.html

// The database for images
const IMGS_DB: &'static[&'static str]  = &["cirros", "debian-10",];

/// Accepts and returns the image name if it exists, reject the
/// request with a 404 Not found otherwise.
async fn image_exists(img: String) -> Result<String, warp::Rejection> {
    let exists = IMGS_DB.iter().any(|&img| img == img);

    if exists {
        Ok(img)
    } else {
        Err(warp::reject::not_found())
    }
}

#[tokio::main]
async fn main() {
    // List available images
    //
    // API:
    // - / => 200 OK, with images as a json list.
    //     => 401 Unauthorized, for bad Bearer token
    //     => 400 Bad request, for missing Bearer token
    //
    // Example:
    // $ curl --oauth2-bearer the-secret-token "127.0.0.1:4202/images/" -v
    // < HTTP/1.1 200 OK
    // < ...
    // ["cirros","debian-10"]%
    let list = warp::path::end()
        .and(warp::get())
        // Collect image names and returns them as a json
        .map(|| warp::reply::json(&IMGS_DB));

    // Show an image info
    //
    // API:
    // - /:img => 200 OK, with images as a json list.
    //         => 404 Not found, for an unexisting image.
    //         => 401 Unauthorized, for bad Bearer token
    //         => 400 Bad request, for missing Bearer token
    //
    // Example:
    // $ curl --oauth2-bearer the-secret-token "127.0.0.1:4202/images/debian-10" -v
    // < HTTP/1.1 200 OK
    // < ...
    // ...
    let show = warp::path!(String)
        .and(warp::get())
        .and_then(image_exists)
        .map(|_img: String| {
            // TODO: build `_img` info
            let img_info = "";
            warp::reply::json(&img_info)
        });

    // Get image data stream
    //
    // API:
    // - /:img/file => 200 OK, with images as a json list.
    //              => 404 Not found, for an unexisting image.
    //              => 401 Unauthorized, for bad Bearer token
    //              => 400 Bad request, for missing Bearer token
    //
    // Example:
    // curl --oauth2-bearer the-secret-token http://localhost:4202/images/debian-10/file -v
    // < HTTP/1.1 200 OK
    // < ...
    // ...
    let get_data = warp::path!(String / "file")
        .and(warp::get())
        .and_then(image_exists)
        .map(|_img: String| {
            // TODO: get `_img` data
            let img_data: &'static [u8] = &[];
            warp::reply::html(img_data)
        });

    // Define the API (prefix every path with "images")
    let api = warp::path("images")
        // Check token validity
        .and(auth::authorized())
        // Compose endpoints
        .and(list
             .or(show)
             .or(get_data))
        // Return good response code for authorization
        .recover(auth::handle_not_authorized);

    // Start the server
    warp::serve(api).run(([0,0,0,0], 4202)).await;
}
