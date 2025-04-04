(namespace (read-msg 'ns))

(module wrapper GOVERNANCE
  (defschema liquidity-position
      "Tracks liquidity positions of users of the wrapper contract."
    key:string                          ;; key for this row, used for convenience
    pair:string                         ;; pair-key for this position, returned by exchange.get-pair-key
    account:string                      ;; user account that controls the liquidity tokens (`to` parameter from `add-liquidity`)
    guard:guard                         ;; guard for controlling the liquidity tokens
    liquidity-tokens:decimal            ;; amount of liquidity tokens the user can remove with `remove-liquidity`
    tokenA-pooled:decimal               ;; amount of tokenAs initially added (or later IL-adjusted)
    tokenB-pooled:decimal               ;; amount of tokenBs initially added (or later IL-adjusted)

    updated-at:time                     ;; last time the settled fee values were updated
    settled-multiplier:decimal          ;; the accumulated multiplier for the settled fees
    settled-liquidity-for-fees:decimal) ;; the amount of liquidity set aside for settled fees with the settled-multiplier
  (deftable liquidity-positions:{liquidity-position})

  (defschema liquidity-account
      "Tracks liquidity positions owned by the wrapper contract in the underlying pools."
    pair:string                         ;; pair-key for this account, returned by exchange.get-pair-key
    account:string                      ;; account created and managed by the wrapper to hold liquidity tokens
    guard:guard                         ;; module guard used to protect the held liquidity tokens
    tokenA:module{fungible-v2}          ;; module references to the two tokens of the pair
    tokenB:module{fungible-v2}
    liquidity-tokens:decimal)           ;; total amount of liquidity tokens held by the wrapper for the pair
  (deftable liquidity-accounts:{liquidity-account})

  (defschema trading-pair
      "Tracks information about supported wrapped trading pairs."
    pair:string                             ;; pair-key for this pair
    account:string                          ;; account created and managed by the wrapper to hold both tokenA/tokenB temporarily
    guard:guard                             ;; module guard used to protect the account
    fee-multiplier:decimal                  ;; the currently active KDX boosted fee multiplier for this pair
    last-fee-multiplier:decimal             ;; the last active fee multiplier for this pair
    updated-at:time                         ;; the time when the multiplier was last updated
    tokenA:module{fungible-v2}              ;; module reference to the pair token
    tokenA-base-path:[module{fungible-v2}]  ;; swap path for tokenA into KDX, e.g. [token-abc coin kaddex.kdx]
    tokenB:module{fungible-v2}
    tokenB-base-path:[module{fungible-v2}])
  (deftable trading-pairs:{trading-pair})

  (defschema reward-claim-request
      "Tracks information for pending boosted KDX reward claims."
    request-id:string  ;; key for this row, for convenience
    account:string     ;; user account claiming rewards
    to:string          ;; user account to receive the rewards
    to-guard:guard     ;; guard to receive the rewards

    liquidity-position-key:string ;; key into the liquidity-positions-table
    pair-key:string               ;; key into trading-pairs table
    tokenA-fees:decimal           ;; amount of tokenA collected as fees (not counting settled fees)
    tokenB-fees:decimal           ;; amount of tokenB collected as fees (not counting settled fees)

    tokenA-observations:[object{exchange.observation}] ;; list of TWAP observations for the tokenA swap path
    tokenB-observations:[object{exchange.observation}] ;; list of TWAP observations for the tokenB swap path
    total-kdx-swapped:decimal                          ;; total amount of KDX received from selling the fees

    fee-multiplier:decimal ;; the active multiplier the user is gonna get (not counting settled fees)

    settled-tokenA-fees:decimal ;; amount of tokenA collected as settled fees only
    settled-tokenB-fees:decimal ;; amount of tokenB collected as settled fees only
    settled-multiplier:decimal  ;; the multiplier for the settled fee amounts

    start-time:time ;; when the user initiated the request
    end-time:time   ;; when the request is available to be claimed
    status:string)  ;; current status of the request, see below for possible values
  (deftable reward-claim-requests:{reward-claim-request})


  (defschema pending-request-schema
      requests:[string])
  (deftable pending-requests:{pending-request-schema})
  ;; if the key is account name, then requests is a list of pending requests for that account

  ;; this is a simple global lock that can be toggled by the operators to pause the contract if necessary
  (defschema contract-lock-status
      lock:bool)
  (deftable contract-lock:{contract-lock-status})
  (defconst CONTRACT_LOCK_KEY 'lock)

  (defcap RESERVE_ACCESS
    ( pair:string )
    true)
  (defcap LIQUIDITY_ACCESS
    ( token:string )
    true)
  (defcap BANK_ACCESS
    ( token:string )
    true)
  (defcap MINTING
    ( token:string )
    true)
  (defcap BURNING
    ( token:string )
    true)

  (defun format-token:string (token:module{fungible-v2})
    (format "{}" [token]))

  (defun enforce-bank-access (token:string)
    (require-capability (BANK_ACCESS token)))
  (defun enforce-reserve-access (pair:string)
    (require-capability (RESERVE_ACCESS pair)))
  (defun enforce-minting (token:string)
    (require-capability (MINTING token)))
  (defun enforce-burning (token:string)
    (require-capability (BURNING token)))
  (defun enforce-liquidity-access (pair:string)
    (require-capability (LIQUIDITY_ACCESS pair)))

  (defun create-reserve-guard (pair-key:string)
    (create-user-guard (enforce-reserve-access pair-key)))
  (defun create-mint-guard (token:module{fungible-v2})
    (create-user-guard (enforce-minting (format-token token))))
  (defun create-burn-guard (token:module{fungible-v2})
    (create-user-guard (enforce-burning (format-token token))))
  (defun create-liquidity-guard (pair:string)
    (create-user-guard (enforce-liquidity-access pair)))
  (defun create-bank-guard ()
    (create-user-guard (enforce-bank-access (format-token (get-base-token)))))

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'kaddex-wrapper-admin)))

  (defcap OPS ()
    (enforce-guard (keyset-ref-guard 'kaddex-wrapper-ops)))

  (defcap LIQUIDITY_POS_GUARD
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      account:string
    )
    "Capability for enforcing the user's liquidity-position guard"
    (enforce-guard (at 'guard (get-liquidity-position token0 token1 account)))
  )

  (defcap FEE_BUYBACK
    ( token:module{fungible-v2}
      token-accrued:decimal
    )
    "Private capability for processing KDX buybacks."
    true
  )

  (defcap SWAPPING_FEES ()
    "Private capability for swapping KDX."
    true
  )

  (defcap FEE_REWARD
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      token0-accrued:decimal
      token1-accrued:decimal
      base-token-paid:decimal
      reward-multiplier:decimal
      bonus-reward:decimal
      to:string
    )
    "Event for reporting fee payouts."
    @event
    true
  )

  ;; account name for the wrapper KDX holding account, holds the KDX from swapping user fees
  (defconst WRAPPER_KDX_BANK 'kaddex-kdx-wrapper-bank)

  ;; account name for holding pre-minted KDX for booster distribution. needs to be regularly monitored and possibly refilled
  ;; this is used instead of giving mint permissions to the wrapper to minimize attack surface
  (defconst WRAPPER_KDX_MINT_BANK 'kaddex-kdx-wrapper-mint-bank)


  ;; utility function to get a module reference to KDX
  (defun get-base-token:module{fungible-v2} () kaddex.kdx)

  (defun tokens-equal:bool
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Token equality defined by comparing the fully qualified contract names."
    ;; Module references are equal if and only if they implement exactly the same interfaces.
    ;; Updates to tokens (such as implementing the new fungible-xchain-v1 interface) can
    ;; break this, so we only check if the fully qualified name is the same.
    (= (format "{}" [tokenA])
       (format "{}" [tokenB]))
  )

  (defun get-pair-account:string
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (at 'account (read trading-pairs (exchange.get-pair-key tokenA tokenB) ['account]))
  )

  (defun get-pair-multiplier:decimal
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (at 'fee-multiplier (read trading-pairs (exchange.get-pair-key tokenA tokenB) ['fee-multiplier]))
  )

  (defun get-liquidity-position-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
    )
    (format "{}:{}" [(exchange.get-pair-key tokenA tokenB) account])
  )

  (defun get-liquidity-position-key*
    ( pair-key:string
      account:string
    )
    (format "{}:{}" [pair-key account])
  )

  (defun get-liquidity-account:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (read liquidity-accounts (exchange.get-pair-key tokenA tokenB))
  )

  (defun get-liquidity-position:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
    )
    (read liquidity-positions (get-liquidity-position-key tokenA tokenB account))
  )

  (defun pair-registered:bool
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (with-default-read trading-pairs (exchange.get-pair-key tokenA tokenB)
      { 'pair: "" }
      { 'pair := read-pair-key }
      (!= 0 (length read-pair-key))
    )
  )

  (defun enforce-disabled:bool ()
    (enforce false "Wrapper module is disabled"))

  (defun register-pair-only
    ( tokenAA:module{fungible-v2}
      tokenBB:module{fungible-v2}
      tokenAA-base-path:[module{fungible-v2}]
      tokenBB-base-path:[module{fungible-v2}]
      hint:string
    )
    (enforce-disabled)
  )

  (defun register-pair
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      tokenA-base-path:[module{fungible-v2}]
      tokenB-base-path:[module{fungible-v2}]
      amountA-desired:decimal
      amountB-desired:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      hint:string
    )
    (enforce-disabled)
  )

  (defun set-fee-multiplier-for-pair
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      new-multiplier:decimal
    )
    (enforce-disabled)
  )

  (defun update-single-position-for-new-multiplier
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
    )
    (enforce-disabled)
  )

  (defun update-positions-for-new-multiplier
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      accounts:[string]
    )
    (enforce-disabled)
  )


  (defun remove-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      requested-liquidity:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      to:string
      to-guard:guard
      wants-kdx-rewards:bool
    )
    (enforce-disabled)
    )

  (defun remove-liquidity-extended:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      requested-liquidity:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      to:string
      to-guard:guard
      wants-kdx-rewards:bool
      rewards-to:string
      rewards-to-guard:guard
    )
  (enforce-disabled)
  )



  (defun add-liquidity-one-sided:object
    ( token-in:module{fungible-v2}
      token-other:module{fungible-v2}
      amount-in-total:decimal
      amount-in-min:decimal
      amount-other-min:decimal
      sender:string
      sender-guard:guard
      to:string
      to-guard:guard
    )
  (enforce-disabled)
  )

  (defun add-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      amountA-desired:decimal
      amountB-desired:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      to:string
      to-guard:guard
    )
    (enforce-disabled)
  )

  (defun dump-positions () ;; CLEANUP: testing function, do we want to remove?
    (select liquidity-positions (constantly true)))

  (defun dump-liquidity () ;; CLEANUP: testing function, do we want to remove?
    (select liquidity-accounts (constantly true)))

  (defun withdraw-claim:string
    ( sender:string
      request-id:string
    )
    (enforce-disabled)
  )


  (defun process-claim-request-if-necessary:bool
    ( request-id:string
    )
    (enforce-disabled)
  )

  (defun init (initial-lock:bool)
    (enforce-disabled)
  )

  (defun transfer-from-bank (dst:string dst-guard:guard)
    (with-capability (OPS)
      (install-capability (kdx.TRANSFER WRAPPER_KDX_BANK dst (kdx.get-balance WRAPPER_KDX_BANK)))
      (install-capability (kdx.TRANSFER WRAPPER_KDX_MINT_BANK dst (kdx.get-balance WRAPPER_KDX_MINT_BANK)))

      (with-capability (BANK_ACCESS "kaddex.kdx")
        (kdx.transfer-create WRAPPER_KDX_BANK dst dst-guard (kdx.get-balance WRAPPER_KDX_BANK))
        (kdx.transfer-create WRAPPER_KDX_MINT_BANK dst dst-guard (kdx.get-balance WRAPPER_KDX_MINT_BANK))))
  )

  (defun unwrap-account (pair-key:string account:string)
    (with-capability (OPS)
      (with-read liquidity-positions (get-liquidity-position-key* pair-key account) {'liquidity-tokens:=amount,
                                                                                    'account:=owner-account,
                                                                                    'guard:=owner-guard}
        (with-read liquidity-accounts pair-key {'liquidity-tokens:=total-wrapped, 'account:=wrapper-account}
          (if (> amount 0.0)
              (do (install-capability (tokens.TRANSFER pair-key wrapper-account owner-account amount))
                  (with-capability (LIQUIDITY_ACCESS pair-key)
                    (tokens.transfer-create pair-key wrapper-account owner-account owner-guard amount)))

              "")
          (update liquidity-accounts pair-key {'liquidity-tokens: (- total-wrapped amount)})))

        (update liquidity-positions (get-liquidity-position-key* pair-key account)
                {'liquidity-tokens:0.0,
                 'tokenA-pooled:0.0,
                 'tokenB-pooled:0.0,
                 'updated-at:(at 'block-time (chain-data)),
                 'settled-multiplier:0.0,
                 'settled-liquidity-for-fees:0.0}))
  )

  (defun unwrap-account-object (data:object)
    (bind data {'a:=account, 'p:=pair}
      (unwrap-account pair account))
  )
)
