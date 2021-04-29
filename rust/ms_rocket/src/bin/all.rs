use std::thread;

use ms_rocket::identity;
use ms_rocket::images;
use ms_rocket::compute;

// Start all microservices
fn main() {
    let ms_identity = thread::spawn(move || {
        identity::rocket(4201).launch();
    });

    let ms_images = thread::spawn(move || {
        images::rocket(4202).launch();
    });

    let ms_compute = thread::spawn(move || {
        compute::rocket(4203).launch();
    });

    ms_identity.join();
    ms_images.join();
    ms_compute.join();
}
