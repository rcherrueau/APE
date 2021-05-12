use hostname;
use rocket::http::Status;
use rocket::State;
use rocket::config::{Config, Environment};

// Bearer token of the service
struct Token(String);

// Returns a token for latter authentication.
//
// API:
// - /auth/token => 200 OK, with the `TOKEN` in the body
//
// Example:
// $ curl "127.0.0.1:4201/auth/token" -v
// < HTTP/1.1 200 OK
// < ...
// the-secret-token%
#[get("/auth/token")]
fn get_token(host_token: State<Token>) -> String {
    format!("{}", host_token.0)
}

// Checks the validity of a token.
//
// API:
// Head /auth/token => 200 OK, if token is valide
//                  => 401 Unauthorized, otherwise
// Get /auth/token  => 200 OK, if token is valid
//                  => 401 Unauthorized otherwise
//
// Example:
// $ curl "127.0.0.1:4201/auth/check/the-secret-token" -I
// HTTP/1.1 401 Unauthorized
// ...
//
// $ curl "127.0.0.1:4201/auth/check/foo" -I
// HTTP/1.1 401 Unauthorized
// ...
#[get("/auth/check/<user_token>")]
fn check_token(user_token: String, host_token: State<Token>) -> Status {
    if user_token == host_token.0 {
        Status::Ok
    } else {
        Status::Unauthorized
    }
}

// Declare the microservice
pub fn rocket(port: u16) -> rocket::Rocket {
    let hostname: String = hostname::get()
        .unwrap().into_string()
        .unwrap_or(String::from("nohost"));

    // Make the token specific to the machine
    let token: String = format!("token-{}", hostname);

    let config = Config::build(Environment::Development)
        .address("0.0.0.0")
        .port(port)
        .finalize()
        .unwrap();

    rocket::custom(config)
        .manage(Token(token))
        .mount("/", routes![get_token, check_token])
}
