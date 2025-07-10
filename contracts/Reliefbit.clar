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

(define-constant ERR_MILESTONE_NOT_FOUND (err u109))
(define-constant ERR_BADGE_ALREADY_CLAIMED (err u110))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u111))
(define-constant ERR_INVALID_MILESTONE (err u112))

(define-data-var next-milestone-id uint u1)
(define-data-var total-donors uint u0)
(define-data-var total-donations-count uint u0)

(define-map emergency-milestones
  { emergency-id: uint }
  {
    milestone-25: uint,
    milestone-50: uint,
    milestone-75: uint,
    milestone-100: uint,
    current-progress: uint,
    milestone-25-reached: bool,
    milestone-50-reached: bool,
    milestone-75-reached: bool,
    milestone-100-reached: bool,
    milestone-25-at: (optional uint),
    milestone-50-at: (optional uint),
    milestone-75-at: (optional uint),
    milestone-100-at: (optional uint)
  }
)

(define-map donor-profiles
  { donor-address: principal }
  {
    total-donated: uint,
    donation-count: uint,
    reputation-points: uint,
    first-donation-at: uint,
    last-donation-at: uint,
    badge-level: uint,
    consecutive-months: uint,
    largest-donation: uint,
    favorite-cause: (optional uint)
  }
)

(define-map donor-emergency-history
  { donor-address: principal, emergency-id: uint }
  {
    total-contribution: uint,
    donation-count: uint,
    first-donation-at: uint,
    last-donation-at: uint,
    milestone-rewards-earned: uint
  }
)

(define-map milestone-rewards
  { reward-id: uint }
  {
    donor-address: principal,
    emergency-id: uint,
    milestone-level: uint,
    reward-points: uint,
    earned-at: uint,
    badge-earned: (optional (string-ascii 50))
  }
)

(define-map donor-badges
  { donor-address: principal, badge-type: (string-ascii 50) }
  {
    earned-at: uint,
    badge-level: uint,
    requirements-met: (string-ascii 200)
  }
)

(define-public (set-emergency-milestone-goals (emergency-id uint) (goal-amount uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
      (milestone-25 (/ goal-amount u4))
      (milestone-50 (/ goal-amount u2))
      (milestone-75 (/ (* goal-amount u3) u4))
      (milestone-100 goal-amount)
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> goal-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active emergency) ERR_EMERGENCY_INACTIVE)
    
    (map-set emergency-milestones
      { emergency-id: emergency-id }
      {
        milestone-25: milestone-25,
        milestone-50: milestone-50,
        milestone-75: milestone-75,
        milestone-100: milestone-100,
        current-progress: u0,
        milestone-25-reached: false,
        milestone-50-reached: false,
        milestone-75-reached: false,
        milestone-100-reached: false,
        milestone-25-at: none,
        milestone-50-at: none,
        milestone-75-at: none,
        milestone-100-at: none
      }
    )
    (ok true)
  )
)

(define-public (fund-emergency-with-tracking (emergency-id uint) (amount uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
      (current-stats (unwrap! (map-get? emergency-stats { emergency-id: emergency-id }) ERR_INVALID_EMERGENCY))
      (milestone-data (map-get? emergency-milestones { emergency-id: emergency-id }))
      (donor-profile (map-get? donor-profiles { donor-address: tx-sender }))
      (donor-history (map-get? donor-emergency-history { donor-address: tx-sender, emergency-id: emergency-id }))
      (current-block stacks-block-height)
      (reputation-earned (calculate-reputation-points amount))
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
    
    (match donor-profile
      existing-profile (map-set donor-profiles
        { donor-address: tx-sender }
        {
          total-donated: (+ (get total-donated existing-profile) amount),
          donation-count: (+ (get donation-count existing-profile) u1),
          reputation-points: (+ (get reputation-points existing-profile) reputation-earned),
          first-donation-at: (get first-donation-at existing-profile),
          last-donation-at: current-block,
          badge-level: (get badge-level existing-profile),
          consecutive-months: (get consecutive-months existing-profile),
          largest-donation: (if (> amount (get largest-donation existing-profile)) amount (get largest-donation existing-profile)),
          favorite-cause: (get favorite-cause existing-profile)
        }
      )
      (begin
        (map-set donor-profiles
          { donor-address: tx-sender }
          {
            total-donated: amount,
            donation-count: u1,
            reputation-points: reputation-earned,
            first-donation-at: current-block,
            last-donation-at: current-block,
            badge-level: u0,
            consecutive-months: u1,
            largest-donation: amount,
            favorite-cause: (some emergency-id)
          }
        )
        (var-set total-donors (+ (var-get total-donors) u1))
      )
    )
    
    (match donor-history
      existing-history (map-set donor-emergency-history
        { donor-address: tx-sender, emergency-id: emergency-id }
        {
          total-contribution: (+ (get total-contribution existing-history) amount),
          donation-count: (+ (get donation-count existing-history) u1),
          first-donation-at: (get first-donation-at existing-history),
          last-donation-at: current-block,
          milestone-rewards-earned: (get milestone-rewards-earned existing-history)
        }
      )
      (map-set donor-emergency-history
        { donor-address: tx-sender, emergency-id: emergency-id }
        {
          total-contribution: amount,
          donation-count: u1,
          first-donation-at: current-block,
          last-donation-at: current-block,
          milestone-rewards-earned: u0
        }
      )
    )
    
    (match milestone-data
      existing-milestones (let ((update-result (update-milestone-progress emergency-id existing-milestones amount))) (unwrap! update-result ERR_INVALID_MILESTONE))
      true
    )
    
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (var-set total-donations-count (+ (var-get total-donations-count) u1))
    (ok true)
  )
)

(define-private (calculate-reputation-points (amount uint))
  (let
    (
      (base-points (/ amount u1000000))
      (bonus-multiplier (if (> amount u10000000) u3 (if (> amount u5000000) u2 u1)))
    )
    (if (> base-points u0) (* base-points bonus-multiplier) u1)
  )
)

(define-private (update-milestone-progress (emergency-id uint) (milestone-data (tuple (milestone-25 uint) (milestone-50 uint) (milestone-75 uint) (milestone-100 uint) (current-progress uint) (milestone-25-reached bool) (milestone-50-reached bool) (milestone-75-reached bool) (milestone-100-reached bool) (milestone-25-at (optional uint)) (milestone-50-at (optional uint)) (milestone-75-at (optional uint)) (milestone-100-at (optional uint)))) (donation-amount uint))
  (let
    (
      (new-progress (+ (get current-progress milestone-data) donation-amount))
      (current-block stacks-block-height)
      (milestone-25-target (get milestone-25 milestone-data))
      (milestone-50-target (get milestone-50 milestone-data))
      (milestone-75-target (get milestone-75 milestone-data))
      (milestone-100-target (get milestone-100 milestone-data))
      (milestone-25-reached (or (get milestone-25-reached milestone-data) (>= new-progress milestone-25-target)))
      (milestone-50-reached (or (get milestone-50-reached milestone-data) (>= new-progress milestone-50-target)))
      (milestone-75-reached (or (get milestone-75-reached milestone-data) (>= new-progress milestone-75-target)))
      (milestone-100-reached (or (get milestone-100-reached milestone-data) (>= new-progress milestone-100-target)))
    )
    (map-set emergency-milestones
      { emergency-id: emergency-id }
      {
        milestone-25: milestone-25-target,
        milestone-50: milestone-50-target,
        milestone-75: milestone-75-target,
        milestone-100: milestone-100-target,
        current-progress: new-progress,
        milestone-25-reached: milestone-25-reached,
        milestone-50-reached: milestone-50-reached,
        milestone-75-reached: milestone-75-reached,
        milestone-100-reached: milestone-100-reached,
        milestone-25-at: (if (and milestone-25-reached (is-none (get milestone-25-at milestone-data))) (some current-block) (get milestone-25-at milestone-data)),
        milestone-50-at: (if (and milestone-50-reached (is-none (get milestone-50-at milestone-data))) (some current-block) (get milestone-50-at milestone-data)),
        milestone-75-at: (if (and milestone-75-reached (is-none (get milestone-75-at milestone-data))) (some current-block) (get milestone-75-at milestone-data)),
        milestone-100-at: (if (and milestone-100-reached (is-none (get milestone-100-at milestone-data))) (some current-block) (get milestone-100-at milestone-data))
      }
    )
    (ok true)
  )
)

(define-public (claim-milestone-badge (emergency-id uint) (milestone-level uint))
  (let
    (
      (milestone-data (unwrap! (map-get? emergency-milestones { emergency-id: emergency-id }) ERR_MILESTONE_NOT_FOUND))
      (donor-history (unwrap! (map-get? donor-emergency-history { donor-address: tx-sender, emergency-id: emergency-id }) ERR_NOT_VERIFIED))
      (badge-name (get-milestone-badge-name milestone-level))
      (badge-key { donor-address: tx-sender, badge-type: badge-name })
      (reward-points (get-milestone-reward-points milestone-level))
      (current-block stacks-block-height)
      (donor-profile (unwrap! (map-get? donor-profiles { donor-address: tx-sender }) ERR_NOT_VERIFIED))
    )
    (asserts! (> (get total-contribution donor-history) u0) ERR_NOT_VERIFIED)
    (asserts! (is-milestone-reached milestone-data milestone-level) ERR_INVALID_MILESTONE)
    (asserts! (is-none (map-get? donor-badges badge-key)) ERR_BADGE_ALREADY_CLAIMED)
    
    (map-set donor-badges
      badge-key
      {
        earned-at: current-block,
        badge-level: milestone-level,
        requirements-met: (get-milestone-requirements milestone-level)
      }
    )
    
    (map-set milestone-rewards
      { reward-id: (var-get next-milestone-id) }
      {
        donor-address: tx-sender,
        emergency-id: emergency-id,
        milestone-level: milestone-level,
        reward-points: reward-points,
        earned-at: current-block,
        badge-earned: (some badge-name)
      }
    )
    
    (map-set donor-profiles
      { donor-address: tx-sender }
      (merge donor-profile { reputation-points: (+ (get reputation-points donor-profile) reward-points) })
    )
    
    (var-set next-milestone-id (+ (var-get next-milestone-id) u1))
    (ok true)
  )
)

(define-private (get-milestone-badge-name (milestone-level uint))
  (if (is-eq milestone-level u25) "Bronze Supporter"
    (if (is-eq milestone-level u50) "Silver Supporter"
      (if (is-eq milestone-level u75) "Gold Supporter"
        (if (is-eq milestone-level u100) "Platinum Supporter"
          "Unknown Badge"
        )
      )
    )
  )
)

(define-private (get-milestone-reward-points (milestone-level uint))
  (if (is-eq milestone-level u25) u50
    (if (is-eq milestone-level u50) u100
      (if (is-eq milestone-level u75) u200
        (if (is-eq milestone-level u100) u500
          u0
        )
      )
    )
  )
)

(define-private (get-milestone-requirements (milestone-level uint))
  (if (is-eq milestone-level u25) "Contributed to 25% milestone completion"
    (if (is-eq milestone-level u50) "Contributed to 50% milestone completion"
      (if (is-eq milestone-level u75) "Contributed to 75% milestone completion"
        (if (is-eq milestone-level u100) "Contributed to 100% milestone completion"
          "Unknown requirements"
        )
      )
    )
  )
)

(define-private (is-milestone-reached (milestone-data (tuple (milestone-25 uint) (milestone-50 uint) (milestone-75 uint) (milestone-100 uint) (current-progress uint) (milestone-25-reached bool) (milestone-50-reached bool) (milestone-75-reached bool) (milestone-100-reached bool) (milestone-25-at (optional uint)) (milestone-50-at (optional uint)) (milestone-75-at (optional uint)) (milestone-100-at (optional uint)))) (milestone-level uint))
  (if (is-eq milestone-level u25) (get milestone-25-reached milestone-data)
    (if (is-eq milestone-level u50) (get milestone-50-reached milestone-data)
      (if (is-eq milestone-level u75) (get milestone-75-reached milestone-data)
        (if (is-eq milestone-level u100) (get milestone-100-reached milestone-data)
          false
        )
      )
    )
  )
)

(define-read-only (get-emergency-milestone-progress (emergency-id uint))
  (map-get? emergency-milestones { emergency-id: emergency-id })
)

(define-read-only (get-donor-profile (donor-address principal))
  (map-get? donor-profiles { donor-address: donor-address })
)

(define-read-only (get-donor-emergency-history (donor-address principal) (emergency-id uint))
  (map-get? donor-emergency-history { donor-address: donor-address, emergency-id: emergency-id })
)

(define-read-only (get-donor-badge (donor-address principal) (badge-type (string-ascii 50)))
  (map-get? donor-badges { donor-address: donor-address, badge-type: badge-type })
)

(define-read-only (get-milestone-reward (reward-id uint))
  (map-get? milestone-rewards { reward-id: reward-id })
)

(define-read-only (get-total-donors)
  (var-get total-donors)
)

(define-read-only (get-total-donations-count)
  (var-get total-donations-count)
)

(define-read-only (calculate-milestone-percentage (emergency-id uint))
  (match (map-get? emergency-milestones { emergency-id: emergency-id })
    milestone-data (let
      (
        (current-progress (get current-progress milestone-data))
        (target-amount (get milestone-100 milestone-data))
      )
      (if (> target-amount u0)
        (some (/ (* current-progress u100) target-amount))
        none
      )
    )
    none
  )
)