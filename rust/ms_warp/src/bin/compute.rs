use warp::Filter;
use serde::{Deserialize, Serialize};

use ms_warp::auth;

#[derive(Debug, Deserialize, Serialize, Clone)]
struct VMRef {
    id: String,
    image_name: String,
    created: Option<String>,
}

// Mimic https://docs.openstack.org/api-ref/compute/

#[tokio::main]
async fn main() {
    // Create a new VM
    //
    // API:
    // - /:vm-name
    let create_vm = warp::path::end()
        .and(warp::post())
        .and(warp::body::json::<VMRef>())
        .map(|vm_to_create_ref| {
            // TODO get image data.
            // let img = ...

            // XXX: create the VM at the hypervisor level
            let vm_created_ref = VMRef {
               created: Some(String::from("uuid-in-virsh")),
               ..vm_to_create_ref
            };

            warp::reply::json(&vm_created_ref)
        });


    // Define the API (prefix every path with "servers")
    let api = warp::path("servers")
    // Check token validity
        .and(auth::authorized())
    // Compose endpoints
        .and(create_vm)
    // Return good response code for authorization
        .recover(auth::handle_not_authorized);

    // Start the server
    warp::serve(api).run(([0,0,0,0], 4203)).await;
}
