# build_req() works

    Code
      build_req(api_key = "TEST", endpoint = "test", limit = 1L)
    Output
      <httr2_request>
      GET https://api.stripe.com/v1/test?limit=1
      Headers:
      * Authorization: <REDACTED>
      Body: empty

