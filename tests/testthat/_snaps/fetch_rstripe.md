# build_req() works

    Code
      build_req(client, endpoint = "test", limit = 1L)
    Output
      <httr2_request>
      GET https://api.stripe.com/v1/test?limit=1
      Headers:
      * Authorization: <REDACTED>
      Body: empty
      Options:
      * timeout_ms    : 30000
      * connecttimeout: 0

