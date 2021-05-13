use rocket::http::Status;
use rocket::config::{Config, Environment};

use rocket_contrib::json::Json;

use crate::auth;

type Res<T> = Result<T, Status>;

// use serde::{Deserialize,Serialize};

// Mimic https://docs.openstack.org/api-ref/image/v2/index.html
// The database for images
const IMGS_DB: &'static[&'static str]  = &["cirros", "debian-10",];

/// Accepts and returns the image name if it exists, reject the
/// request with a 404 Not found otherwise.
fn image_exists(img_ref: String) -> Result<String, Status> {
    let exists = IMGS_DB.iter().any(|&img| img == img_ref);

    if exists {
        Ok(img_ref)
    } else {
        Err(Status::new(Status::NotFound.code, "Image does not exists"))
    }
}

// List available images
//
// API:
// - /images => 200 OK, with images as a json list.
//           => 401 Unauthorized, for bad Bearer token
//           => 400 Bad request, for missing Bearer token
//
// Example:
// $ curl --oauth2-bearer the-secret-token "127.0.0.1:4202/images/" -v
// < HTTP/1.1 200 OK
// < ...
// ["cirros","debian-10"]%
#[get("/images")]
fn list(_auth: auth::Auth) -> Res<Json<&'static[&'static str]>> {
    Ok(Json(IMGS_DB))
}

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
#[get("/images/<image_ref>")]
fn show(image_ref: String, _auth: auth::Auth) -> Res<Json<&'static str>> {
    // Test the image exists
    image_exists(image_ref)?;

    // TODO: build `img_info` from `image_ref`
    let img_info = "";
    Ok(Json(img_info))
}

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
#[get("/images/<image_ref>/file")]
fn data(image_ref: String, _auth: auth::Auth) -> Res<&'static [u8]> {
    // Test the image exists
    image_exists(image_ref)?;

    // TODO: build the image data from `image_ref`
    Ok(&[])
}

// Microservice configuration
pub fn rocket(port: u16) -> rocket::Rocket {
    let config = Config::build(Environment::Development)
        .address("0.0.0.0")
        .port(port)
        .finalize()
        .unwrap();

    rocket::custom(config)
        .mount("/", routes![list, show, data])
}
