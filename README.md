
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rstripe

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of rstripe is to interact with the [Stripe
API](https://docs.stripe.com/api), from R.

It is an independent, community-developed R package for
[Stripe](https://stripe.com/) (not created by or affiliated with
Stripe).

## Installation

You can install the development version of rstripe like so:

``` r
pak::pak("nutrivetpet/rstripe")
```

## Getting Started

### API Key Setup

The package requires environment variables for API authentication:

- `STRIPE_API_KEY_TEST` for test mode operations
- `STRIPE_API_KEY_LIVE` for live mode operations

Set these in your `.Renviron` file with:

``` r
# pak::pak("usethis")
usethis::edit_r_environ()
```

Don’t forget to restart R afterwards.

## Example

### Current API Coverage

The package currently provides limited but useful coverage of the Stripe
API. You can fetch data from these Stripe resources:

``` r
library(rstripe)

# Fetch charges (payments)
charges <- list_charges("test", limit = 10)

# Fetch customers
customers <- list_customers("test", limit = 10)

# Fetch products from your catalog
products <- list_products("test", limit = 10)

# Fetch pricing information
prices <- list_prices("test", limit = 10)

# Fetch invoices
invoices <- list_invoices("test", limit = 10)

# Fetch invoice line items
invoice_items <- list_invoice_items("test", limit = 10)

# Fetch balance transactions
balance_transactions <- list_balance_transactions("test", limit = 10)

# For unlimited results, use limit = Inf
all_charges <- list_charges("test", limit = Inf)
```

All functions support both `"test"` and `"live"` modes, and return data
as data frames (tibbles if the tibble package is installed).
