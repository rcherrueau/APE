#![feature(fn_traits)]
#![feature(unboxed_closures)]


// Check authentication token
pub mod auth {
  use std::convert::Infallible;

  use warp::Filter;
  use reqwest;
  use reqwest::StatusCode;

  // Error report
  #[derive(Debug)]
  struct AuthError<'r> {
      code: StatusCode,
      msg: &'r str,
  }
  impl warp::reject::Reject for AuthError<'static> {}

  /// Extracts the authorization token and check if it is a valid one, or
  /// reject otherwise.
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
  pub fn authorized() -> impl Filter<Extract = (), Error = warp::Rejection> + Copy {

      /// Parses authorization header and returns the Bearer token.
      ///
      /// AuthError (400 Bad request) if the parsing failed. The header
      /// should be of the form "Authorization: Bearer <the-token>".
      fn parse_bearer_token(token: String) -> Result<String, warp::Rejection> {
          match token.split_once(' ') {
              Some(("Bearer", token)) => Ok(String::from(token)),
              _ => {
                  let code = StatusCode::BAD_REQUEST;
                  let msg = "Parsing the token failed \
                    should be 'Authorization: Bearer <token>'";

                  Err(warp::reject::custom(AuthError { code, msg }))
              },
          }
      }

      /// Rest request to Check the auth status.
      ///
      /// AuthError (500 -- Internal server error) if contacting
      /// identity service to check the token failed.
      async fn check_auth_status(token: String) -> Result<StatusCode, warp::Rejection> {
          let rest_check =
              reqwest::get(format!("http://localhost:4201/auth/check/{}", token))
              .await
              .map_err(|err: reqwest::Error| {
                  eprintln!("REST request error: {:?}", err.to_string());

                  let code = err.status().unwrap_or(StatusCode::INTERNAL_SERVER_ERROR);
                  let msg = "Checking token status failed";
                  AuthError { code, msg }
              })?;

          Ok(rest_check.status())
      }

      // Get the "Authorization" header
      warp::header("Authorization")
          .and_then(|auth: String| async move {
              // Parse the Bearer token from the "Authorization" header
              let token = parse_bearer_token(auth)?;

              // Check token with a REST request and get the status
              let status = check_auth_status(token).await?;

              // Check the status
              if status.is_success() {
                  Ok(())
              } else {
                  Err(warp::reject::custom(AuthError {
                      code: status,
                      msg: "Identity service returns a wrong authentication"
                  }))
              }
          })
          // Remove the result of the previous filter in case of Ok
          .untuple_one()
  }

  pub async fn handle_not_authorized(err: warp::Rejection) -> Result<
          impl warp::Reply, Infallible> {
      let code;
      let msg;

      if let Some(err) = err.find::<warp::reject::MissingHeader>() {
          code = StatusCode::INTERNAL_SERVER_ERROR;
          msg = err.to_string();
      } else if let Some(err) = err.find::<AuthError>() {
          code = err.code;
          msg = err.msg.to_string();
      } else {
          eprintln!("unhandled rejection: {:?}", err);
          code = StatusCode::INTERNAL_SERVER_ERROR;
          msg = String::from("UNHANDLED_REJECTION");
      }

      let html = warp::reply::html(msg);
      Ok(warp::reply::with_status(html, code))
  }

}


// Utils function
pub mod utils {
    use std::ops::{FnMut, FnOnce};

    // Equivalent to Haskell const: a -> (b -> a)
    #[derive(Clone, Copy)]
    pub struct Always<A>(A);

    impl<A,B> FnOnce<B> for Always<A> {
        type Output = A;
        extern "rust-call" fn call_once(self, _args: B) -> Self::Output {
            self.0
        }
    }

    impl<A: Copy,B> FnMut<B> for Always<A> {
        extern "rust-call" fn call_mut(&mut self, _args: B) -> Self::Output {
            self.0
        }
    }

    impl<A: Copy,B> Fn<B> for Always<A> {
        extern "rust-call" fn call(&self, _args: B) -> Self::Output {
            self.0
        }
    }

    // always("Hello") => _ -> "Hello"
    pub fn always<A>(a: A) -> Always<A> {Always(a)}
}
