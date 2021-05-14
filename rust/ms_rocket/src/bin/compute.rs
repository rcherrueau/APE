use ms_rocket::compute;

// Start the compute microservice
fn main() {
    compute::rocket(4203).launch();
}
