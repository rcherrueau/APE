use ms_rocket::images;

// Start the image microservice
fn main() {
    images::rocket(4202).launch();
}
