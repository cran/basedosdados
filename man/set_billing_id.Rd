% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/billing.R
\name{set_billing_id}
\alias{set_billing_id}
\title{Define your Project Id}
\usage{
set_billing_id(billing_project_id = NULL)
}
\arguments{
\item{billing_project_id}{a single character value containing the string. Vectors with longer lengths and non-vectors will trigger an error.}
}
\value{
No return.
}
\description{
Define your project billing ids here so all your queries are authenticated and return data, not errors.
If using in production or leaving code available at public repositories, \code{dotenv} is highly recommended.
}
\examples{

\dontrun{
set_billing_id("my_billing_project_id")

# or load from an .env file

library(dotenv)

load_dot_env("keys.env")
print(Sys.getenv("billing_project_id"))

set_billing_id(Sys.getenv("billing_project_id"))

}

}
