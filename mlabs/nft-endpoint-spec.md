# Endpoints Documentation

This document describes endpoints available in NFT markterplace application.

## Admin endpoints

### App Init (`/api/app-init`)

prerequisite:
- none

input:
```json
"argument": {
  "contents": {
    "contents": [
      [
        "getUserId",
        {
          "contents": [
            [
              "getPubKeyHash",
              {
                "tag": "FormSchemaString"
              }
            ]
          ],
          "tag": "FormSchemaObject"
        }
      ]
    ],
    "tag": "FormSchemaObject"
  },
  "tag": "FormSchemaArray"
},
```

behaviour: Starts NFT marketplace application, mintes HEAD and unique token.

## User endpoints

### Mint (`/api/mint`)

prerequisite:
- App is initialised
- NFT was not minted before

input: Mlabs.NFT.Types.MintParams
```json
"argument": {
  "contents": [
    [
      "mp'content",
      {
        "contents": [
          [
            "getContent",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ],
    [
      "mp'title",
      {
        "contents": [
          [
            "getTitle",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ],
    [
      "mp'share",
      {
        "contents": [
          {
            "tag": "FormSchemaInteger"
          },
          {
            "tag": "FormSchemaInteger"
          }
        ],
        "tag": "FormSchemaTuple"
      }
    ],
    [
      "mp'price",
      {
        "contents": {
          "tag": "FormSchemaInteger"
        },
        "tag": "FormSchemaMaybe"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour: Mints new NFT. If the price is `Nothing` then the NFT is not for sale
and the owner must call `set-price` to allow sales.

### Set price (`/api/set-price`)

prerequisite:
- App is initialised
- User must be current owner
- NFT is not on auction

input: Mlabs.NFT.Types.SetPriceParams
```json
"argument": {
  "contents": [
    [
      "sp'nftId",
      {
        "contents": [
          [
            "nftId'contentHash",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ],
    [
      "sp'price",
      {
        "contents": {
          "tag": "FormSchemaInteger"
        },
        "tag": "FormSchemaMaybe"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour: updates the `info'price` parameter

### Buy (`/api/buy`)

prerequisite:
- App is initialised
- User must have necessary ADA in wallet
- `info'price` parameter is not `Nothing`

input: Mlabs.NFT.Types.BuyRequestUser
```json
"argument": {
  "contents": [
    [
      "ur'nftId",
      {
        "contents": [
          [
            "nftId'contentHash",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ],
    [
      "ur'price",
      {
        "tag": "FormSchemaInteger"
      }
    ],
    [
      "ur'newPrice",
      {
        "contents": {
          "tag": "FormSchemaInteger"
        },
        "tag": "FormSchemaMaybe"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour:

If the `BuyRequestUser.ur'price` is greater than or equal to the asking price,
the user's wallet will be reduced by Buy.Price ADA (the contract must fail if
the user has less than the specified Buy.price) the funds sent by the caller
('the buyer') are split such that (`share` * `price` parameter amount) is sent
to the author, and the remainder is sent to the current owner.

For example, if the author set a share to 1/10, and the buyer paid 100 ADA, the
author would receive 10 ADA and the owner would receive the rest. The owner is
set to the caller if the above is successful the asking price is set to the
`BuyRequestUser.ur'newPrice`.

### Auction open (`/api/auction-open`)

prerequisite:
- App is initialised
- User must be current owner
- NFT is not on auction
- `as'minBid` is greater or equal to 2 ADA

input: Mlabs.NFT.Types.AuctionOpenParams
```json
"argument": {
  "contents": [
    [
      "op'nftId",
      {
        "contents": [
          [
            "nftId'contentHash",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ],
    [
      "op'deadline",
      {
        "tag": "FormSchemaInteger"
      }
    ],
    [
      "op'minBid",
      {
        "tag": "FormSchemaInteger"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour:

Sets the `info'price` parameter to `Nothing` (NFT is no longer for sale), and sets `info'auctionState` to `Just` starting an auction.

### Auction bid (`/api/auction-bid`)

prerequisite:
- App is initialised
- NFT is on auction
- Bid (`bp'bidAmount`) is higher than `as'minBid`
- Bid is higher than `as'highesBid`, when `as'highesBid` is `Just`
- `as'deadline` is not reached

input: Mlabs.NFT.Types.AuctionBidParams
```json
"argument": {
  "contents": [
    [
      "bp'nftId",
      {
        "contents": [
          [
            "nftId'contentHash",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ],
    [
      "bp'bidAmount",
      {
        "tag": "FormSchemaInteger"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour:
Bid amount is lock in the script, previous bid is sent back, updates `as'highesBid`

### Auction close (`/api/auction-close`)

prerequisite:
- App is initialised
- NFT is on auction
- User is auction winner

input: Mlabs.NFT.Types.AuctionCloseParams
```json
"argument": {
  "contents": [
    [
      "cp'nftId",
      {
        "contents": [
          [
            "nftId'contentHash",
            {
              "tag": "FormSchemaString"
            }
          ]
        ],
        "tag": "FormSchemaObject"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour: NFT is sent to user, Highest bid is unlocked from script and paid to
previous owner and author, as described in `buy` endpoint.

## Query endpoints

### Query Current Price (`/api/query-current-owner`)

prerequisite:
- App is initialised

input: Mlabs.NFT.Types.NftId
```json
"argument": {
  "contents": [
    [
      "nftId'contentHash",
      {
        "tag": "FormSchemaString"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour: Returns current price of NFT.

### Query Current Owner (`/api/query-current-price`)

prerequisite:
- App is initialised

input: Mlabs.NFT.Types.NftId
```json
"argument": {
  "contents": [
    [
      "nftId'contentHash",
      {
        "tag": "FormSchemaString"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour: Returns current owner of NFT.

### Query List Nfts (`/api/query-list-nfts`)

prerequisite:
- App is initialised

input:
```json
"argument": {
  "tag": "FormSchemaUnit"
},
```

behaviour: Returns list of all NFTs available in the app.

### Query Content (`/api/query-content`)

prerequisite:
- App is initialised

input: Mlabs.NFT.Types.Content
```json
"argument": {
  "contents": [
    [
      "getContent",
      {
        "tag": "FormSchemaString"
      }
    ]
  ],
  "tag": "FormSchemaObject"
},
```

behaviour: Returns status of NFT given content.
