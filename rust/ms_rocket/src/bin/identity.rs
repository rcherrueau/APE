use ms_rocket::identity;

// Start the identity microservice
fn main() {
    identity::rocket(4201).launch();
}
