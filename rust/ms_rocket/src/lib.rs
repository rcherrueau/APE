#![feature(proc_macro_hygiene, decl_macro)]
#[macro_use] extern crate rocket;

// Microservices
pub mod identity;
pub mod compute;
pub mod images;


// Utils
pub mod auth {
    use rocket::http::Status;
    use rocket::request::{FromRequest, Request};
    use rocket::Outcome;

    use reqwest;

    // Authorization token
    pub struct Auth<'r>(pub &'r str);

    // Authorization errors
    static ERR_MISSING_AUTH_HEADER: (u16, &'static str) = (
        Status::BadRequest.code,
        "Missing request header \"Authorization\"",
    );

    static ERR_AUTH_HEADER_NOT_BEARER: (u16, &'static str) = (
        Status::BadRequest.code,
        "Parsing the token failed \
          should be 'Authorization: Bearer <token>'",
    );

    static ERR_REST_CHECK_AUTH: (u16, &'static str) = (
        Status::InternalServerError.code,
        "Contacting the identity service failed",
    );

    /// Parses authorization header and returns the Bearer token.
    ///
    /// The header should be of the form "Authorization: Bearer
    /// <the-token>".
    fn parse_bearer_token(token: &str) -> Option<&str> {
        match token.split_once(' ') {
            Some(("Bearer", token)) => Some(token),
            _ => None,
        }
    }

    /// Rest request to Check the auth status.
    ///
    /// AuthError (500 -- Internal server error) if contacting
    /// identity service to check the token failed.
    fn do_check_auth_status(token: &str) -> Result<reqwest::StatusCode, reqwest::Error> {
        reqwest::blocking::get(format!("http://localhost:4201/auth/check/{}", token))
            .map(|rest_check| rest_check.status())
    }

    /// Extracts the authorization token and check if it is a valid
    /// one, or failed otherwise.
    ///
    /// Cause of rejection:
    /// - No "Authorization" header
    ///   ╰> code: 400 , msg: Missing request header "Authorization"
    /// - Parsing the token failed, e.g.,: "Authorization: Bearer <the-token>"
    ///   ╰> AuthError (400 Bad request)
    /// - Contacting identity service to check the token failed
    ///   ╰> AuthError (500 -- Internal server error)
    /// - Identity service returns wrong authentication
    ///   ╰> AuthError (401 Unauthorized)
    fn auhtenticate<'a, 'r>(req: &'a Request<'r>) -> Result<Auth<'a>, (u16, &'static str)> {
        // Get the "Authorization" header
        let auth_header: &str = req
            .headers()
            .get_one("Authorization")
            .ok_or(ERR_MISSING_AUTH_HEADER)?;

        // Parse the bearer token
        let token = parse_bearer_token(auth_header).ok_or(ERR_AUTH_HEADER_NOT_BEARER)?;

        // Check the token with a REST request and get the status
        let auth_status: reqwest::StatusCode = do_check_auth_status(token)
            .map_err(|err: reqwest::Error| {
                // An error occurred during the REST request
                eprintln!("REST request error: {:?}", err.to_string());
                ERR_REST_CHECK_AUTH
            })?;

        // A successful status means authentication succeeded.
        if auth_status.is_success() {
            Ok(Auth(token))
        } else {
            let code = auth_status.as_u16();
            let msg = auth_status
                .canonical_reason()
                .unwrap_or("Identity service returns a wrong authentication");
            Err((code, msg))
        }
    }

    /// Extracts the authorization token and check if it is a valid
    /// one, or failed otherwise.
    ///
    /// Cause of rejection:
    /// - No "Authorization" header
    ///   ╰> 400 Missing request header "Authorization"
    /// - Parsing the token failed, e.g.,: "Authorization: Bearer <the-token>"
    ///   ╰> AuthError (400 Bad request)
    /// - Contacting identity service to check the token failed
    ///   ╰> AuthError (500 -- Internal server error)
    /// - Identity service returns wrong authentication
    ///   ╰> AuthError (401 Unauthorized)
    impl<'a, 'r> FromRequest<'a, 'r> for Auth<'a> {
        type Error = &'a str;

        fn from_request(req: &'a Request<'r>) -> Outcome<Self, (Status, Self::Error), ()> {
            match auhtenticate(req) {
                Ok(token) => Outcome::Success(token),
                Err((code, msg)) => Outcome::Failure((Status::new(code, msg), msg)),
            }
        }
    }
}

pub mod utils {
    use rocket::http::Status;

    /// Transforms a reqwest::StatusCode into a rocket::http:Status
    pub fn reqwest_to_rocket_status(sc: reqwest::StatusCode) -> Status {
        let code = sc.as_u16();
        let reason = sc.canonical_reason().unwrap_or("");

        Status::new(code, reason)
    }
}
