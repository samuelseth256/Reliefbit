(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_VERIFIED (err u103))
(define-constant ERR_NOT_VERIFIED (err u104))
(define-constant ERR_ALREADY_DISBURSED (err u105))
(define-constant ERR_INVALID_EMERGENCY (err u106))
(define-constant ERR_EMERGENCY_INACTIVE (err u107))
(define-constant ERR_VERIFICATION_EXPIRED (err u108))

(define-data-var next-emergency-id uint u1)
(define-data-var next-victim-id uint u1)
(define-data-var contract-balance uint u0)

(define-map emergencies
  { emergency-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    total-fund: uint,
    disbursed-amount: uint,
    is-active: bool,
    created-by: principal,
    created-at: uint
  }
)

(define-map verified-victims
  { victim-id: uint }
  {
    victim-address: principal,
    emergency-id: uint,
    verification-amount: uint,
    is-disbursed: bool,
    verified-by: principal,
    verified-at: uint,
    disbursed-at: (optional uint)
  }
)

(define-map victim-emergency-lookup
  { victim: principal, emergency-id: uint }
  { victim-id: uint }
)

(define-map emergency-stats
  { emergency-id: uint }
  {
    total-victims: uint,
    total-disbursed: uint,
    remaining-fund: uint
  }
)

(define-public (create-emergency (title (string-ascii 100)) (description (string-ascii 500)))
  (let
    (
      (emergency-id (var-get next-emergency-id))
      (current-block-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> (len title) u0) ERR_INVALID_AMOUNT)
    
    (map-set emergencies
      { emergency-id: emergency-id }
      {
        title: title,
        description: description,
        total-fund: u0,
        disbursed-amount: u0,
        is-active: true,
        created-by: tx-sender,
        created-at: current-block-height
      }
    )
    
    (map-set emergency-stats
      { emergency-id: emergency-id }
      {
        total-victims: u0,
        total-disbursed: u0,
        remaining-fund: u0
      }
    )
    
    (var-set next-emergency-id (+ emergency-id u1))
    (ok emergency-id)
  )
)

(define-public (fund-emergency (emergency-id uint) (amount uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
      (current-stats (unwrap! (map-get? emergency-stats { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active emergency) ERR_EMERGENCY_INACTIVE)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set emergencies
      { emergency-id: emergency-id }
      (merge emergency { total-fund: (+ (get total-fund emergency) amount) })
    )
    
    (map-set emergency-stats
      { emergency-id: emergency-id }
      (merge current-stats { remaining-fund: (+ (get remaining-fund current-stats) amount) })
    )
    
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok true)
  )
)

(define-public (verify-victim (victim-address principal) (emergency-id uint) (amount uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
      (victim-id (var-get next-victim-id))
      (current-stats (unwrap! (map-get? emergency-stats { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
      (lookup-key { victim: victim-address, emergency-id: emergency-id })
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (get is-active emergency) ERR_EMERGENCY_INACTIVE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? victim-emergency-lookup lookup-key)) ERR_ALREADY_VERIFIED)
    (asserts! (<= amount (get remaining-fund current-stats)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set verified-victims
      { victim-id: victim-id }
      {
        victim-address: victim-address,
        emergency-id: emergency-id,
        verification-amount: amount,
        is-disbursed: false,
        verified-by: tx-sender,
        verified-at: stacks-block-height,
        disbursed-at: none
      }
    )
    
    (map-set victim-emergency-lookup
      lookup-key
      { victim-id: victim-id }
    )
    
    (map-set emergency-stats
      { emergency-id: emergency-id }
      (merge current-stats 
        { 
          total-victims: (+ (get total-victims current-stats) u1),
          remaining-fund: (- (get remaining-fund current-stats) amount)
        }
      )
    )
    
    (var-set next-victim-id (+ victim-id u1))
    (ok victim-id)
  )
)

(define-public (disburse-aid (victim-id uint))
  (let
    (
      (victim-data (unwrap! (map-get? verified-victims { victim-id: victim-id }) ERR_NOT_VERIFIED))
      (emergency (unwrap! (map-get? emergencies { emergency-id: (get emergency-id victim-data) }) ERR_INVALID_EMERGENCY))
      (current-stats (unwrap! (map-get? emergency-stats { emergency-id: (get emergency-id victim-data) }) ERR_INVALID_EMERGENCY))
      (disbursement-amount (get verification-amount victim-data))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get is-disbursed victim-data)) ERR_ALREADY_DISBURSED)
    (asserts! (get is-active emergency) ERR_EMERGENCY_INACTIVE)
    (asserts! (>= (var-get contract-balance) disbursement-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? disbursement-amount tx-sender (get victim-address victim-data))))
    
    (map-set verified-victims
      { victim-id: victim-id }
      (merge victim-data 
        { 
          is-disbursed: true,
          disbursed-at: (some stacks-block-height)
        }
      )
    )
    
    (map-set emergencies
      { emergency-id: (get emergency-id victim-data) }
      (merge emergency { disbursed-amount: (+ (get disbursed-amount emergency) disbursement-amount) })
    )
    
    (map-set emergency-stats
      { emergency-id: (get emergency-id victim-data) }
      (merge current-stats { total-disbursed: (+ (get total-disbursed current-stats) disbursement-amount) })
    )
    
    (var-set contract-balance (- (var-get contract-balance) disbursement-amount))
    (ok true)
  )
)

(define-public (toggle-emergency-status (emergency-id uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set emergencies
      { emergency-id: emergency-id }
      (merge emergency { is-active: (not (get is-active emergency)) })
    )
    (ok (not (get is-active emergency)))
  )
)

(define-read-only (get-emergency (emergency-id uint))
  (map-get? emergencies { emergency-id: emergency-id })
)

(define-read-only (get-victim (victim-id uint))
  (map-get? verified-victims { victim-id: victim-id })
)

(define-read-only (get-victim-by-address (victim-address principal) (emergency-id uint))
  (match (map-get? victim-emergency-lookup { victim: victim-address, emergency-id: emergency-id })
    lookup-data (map-get? verified-victims { victim-id: (get victim-id lookup-data) })
    none
  )
)

(define-read-only (get-emergency-stats (emergency-id uint))
  (map-get? emergency-stats { emergency-id: emergency-id })
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-next-emergency-id)
  (var-get next-emergency-id)
)

(define-read-only (get-next-victim-id)
  (var-get next-victim-id)
)

(define-read-only (is-victim-verified (victim-address principal) (emergency-id uint))
  (is-some (map-get? victim-emergency-lookup { victim: victim-address, emergency-id: emergency-id }))
)

(define-read-only (get-emergency-remaining-funds (emergency-id uint))
  (match (map-get? emergency-stats { emergency-id: emergency-id })
    stats (some (get remaining-fund stats))
    none
  )
)