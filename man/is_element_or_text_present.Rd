% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_element_or_text_present}
\alias{is_element_or_text_present}
\title{is_element_or_text_present}
\usage{
is_element_or_text_present(
  remDr,
  element,
  using = "css selector",
  text_element,
  desired_text,
  num_of_iterations = 10,
  wait_time = 0.25
)
}
\arguments{
\item{remDr}{Selenium client used to control the browser.}

\item{element}{A css element as a string.}

\item{using}{type of selector}

\item{text_element}{text_element}

\item{desired_text}{desired_text}

\item{num_of_iterations}{num_of_iterations}

\item{wait_time}{wait_time}
}
\value{
found_element
}
\description{
is_element_or_text_present
}
