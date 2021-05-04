use warp::{Filter, http::StatusCode};

// Bearer token of the service
static TOKEN: &str = "the-secret-token";

// Returns true if `token` is valid.
fn is_valid(token: &str) -> bool {token == TOKEN}

#[tokio::main]
async fn main() {
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
    let get_token = warp::get()
        .and(warp::path("token"))
        .map(|| TOKEN);

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
    let check_token = warp::path!("check" / String)
        .map(|token: String| match is_valid(&token) {
            true => StatusCode::OK,
            false => StatusCode::UNAUTHORIZED,
        });

    // Define the API (prefix every path with "auth")
    let api = warp::path("auth").and(
        get_token.or(check_token));

    // Start the server
    warp::serve(api).run(([0,0,0,0], 4201)).await;
}
