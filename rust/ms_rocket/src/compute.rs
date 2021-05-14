use std::{collections::HashMap, sync::Mutex};

use rocket::http::Status;
use rocket::State;
use rocket::config::{Config, Environment};

use rocket_contrib::json::Json;
use serde::{Deserialize,Serialize};

use crate::{auth, utils};

type ID = String;
type Res<T> = Result<T, Status>;
type DB = Mutex<HashMap<ID, VMRef>>;

// Mimic https://docs.openstack.org/api-ref/compute/

#[derive(Deserialize, Serialize, Clone)]
struct VMRef {
    id: ID,
    image_ref: ID,
    created: Option<String>,
}

fn do_get_img_data(img_ref: &ID, token: &str) -> Res<Vec<u8>> {
    // Rest call to the image service
    let rest_res: reqwest::blocking::Response = reqwest::blocking::Client::new()
        .get(format!("http://localhost:4202/images/{}/file", img_ref))
         // Forward auth token
        .header(reqwest::header::AUTHORIZATION, format!("Bearer {}", token))
        .send()
        .map_err(|err| {
            eprintln!("REST request error: {:?}", err.to_string());
            Status::InternalServerError
        })?;

    // If image not found, returns image service error
    if !rest_res.status().is_success() {
        return Err(utils::reqwest_to_rocket_status(rest_res.status()))
    }

    // Returns image data as Vec
    rest_res.bytes()
        .map(|b| b.to_vec())
        .map_err(|err| {
            eprintln!("Getting image bytes error: {:?}", err.to_string());
            Status::InternalServerError
        })
}

// Create a new VM
//
// API:
// - /:vm-name => 200 OK, with images as a json list.
//             => 401 Unauthorized, for bad Bearer token
//             => 400 Bad request, for missing Bearer token
//
// Example:
// curl --oauth token-$HOSTNAME localhost:4203/servers -v -d '{ "id": "foo", "image_ref": "debian-10" }' -H "Content-Type: application/json"
// < HTTP/1.1 200 OK
// < ...
// ["cirros","debian-10"]%
#[post("/servers", format="application/json", data="<vm_ref>")]
fn new(vm_ref: Json<VMRef>, db: State<DB>, auth: auth::Auth) -> Res<Status> {
    // Get image data.
    let img_data = do_get_img_data(&vm_ref.image_ref, auth.0)?;
    println!("img-data {:?}", img_data);

    // XXX: create the VM at the hypervisor level
    let created_vm_ref = VMRef {
        created: Some(String::from("uuid-in-virsh...")),
        ..vm_ref.0
    };

    // Store the ref in the db
    let id = created_vm_ref.id.clone();
    let mut db = db.lock().expect("db lock.");

    db.insert(id, created_vm_ref);
    Ok(Status::Ok)
}

#[get("/servers")]
fn list(db: State<DB>, _auth: auth::Auth) -> Res<Json<Vec<VMRef>>> {
    //
    // let id = created_vm_ref.id;
    let vm_refs: Vec<VMRef> = db.lock()
        .expect("db lock.")
        .values()
        .cloned()
        .collect();

    Ok(Json(vm_refs))
}

// Microservice configuration
pub fn rocket(port: u16) -> rocket::Rocket {
    let config = Config::build(Environment::Development)
        .address("0.0.0.0")
        .port(port)
        .finalize()
        .unwrap();

    rocket::custom(config)
        .manage(Mutex::new(HashMap::<ID, VMRef>::new()))
        .mount("/", routes![new, list])
}
