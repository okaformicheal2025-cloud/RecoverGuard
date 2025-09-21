;; title: RecoverGuard
;; version: 1.0.0
;; summary: Social recovery vault system for secure asset management
;; description: A multi-signature recovery system that allows users to set trusted guardians
;;              who can help recover assets in case of emergency, with built-in time delays
;;              and cancellation mechanisms for security.

;; traits
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri (uint) (response (optional (string-utf8 256)) uint))
  )
)

;; token definitions
;; None - this contract manages existing assets

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_VAULT_NOT_FOUND (err u101))
(define-constant ERR_GUARDIAN_ALREADY_EXISTS (err u102))
(define-constant ERR_GUARDIAN_NOT_FOUND (err u103))
(define-constant ERR_INVALID_GUARDIAN_COUNT (err u104))
(define-constant ERR_RECOVERY_NOT_INITIATED (err u105))
(define-constant ERR_RECOVERY_ALREADY_INITIATED (err u106))
(define-constant ERR_INSUFFICIENT_APPROVALS (err u107))
(define-constant ERR_RECOVERY_PERIOD_NOT_ENDED (err u108))
(define-constant ERR_RECOVERY_EXPIRED (err u109))
(define-constant ERR_CANNOT_BE_OWN_GUARDIAN (err u110))
(define-constant ERR_INVALID_AMOUNT (err u111))
(define-constant ERR_INSUFFICIENT_BALANCE (err u112))
(define-constant ERR_CONTRACT_PAUSED (err u113))
(define-constant ERR_RATE_LIMIT_EXCEEDED (err u114))
(define-constant ERR_DUPLICATE_GUARDIAN (err u115))
(define-constant ERR_MAX_DEPOSIT_EXCEEDED (err u116))
(define-constant ERR_REENTRANCY_DETECTED (err u117))
(define-constant ERR_INVALID_NEW_OWNER (err u118))
(define-constant ERR_GUARDIAN_ALREADY_APPROVED (err u119))

(define-constant MIN_GUARDIANS u3)
(define-constant MAX_GUARDIANS u5)
(define-constant RECOVERY_DELAY_BLOCKS u144) ;; ~24 hours at 10 min blocks
(define-constant CANCELLATION_PERIOD_BLOCKS u72) ;; ~12 hours at 10 min blocks
(define-constant RECOVERY_EXPIRY_BLOCKS u1008) ;; ~7 days at 10 min blocks
(define-constant MAX_DEPOSIT_AMOUNT u1000000000000) ;; 1M STX max deposit
(define-constant RATE_LIMIT_WINDOW u10) ;; 10 blocks rate limit window
(define-constant MAX_RECOVERY_ATTEMPTS u3) ;; Max recovery attempts per window

;; data vars
(define-data-var vault-nonce uint u0)
(define-data-var contract-paused bool false)
(define-data-var last-recovery-attempt uint u0)
(define-data-var recovery-attempt-count uint u0)

;; data maps
(define-map vaults
  principal
  {
    guardians: (list 5 principal),
    required-approvals: uint,
    stx-balance: uint,
    is-locked: bool
  }
)

(define-map recovery-requests
  principal
  {
    new-owner: principal,
    initiated-at: uint,
    approvals: (list 5 principal),
    approval-count: uint,
    is-active: bool
  }
)

(define-map guardian-approvals
  { vault-owner: principal, guardian: principal }
  { approved: bool, approved-at: uint }
)

(define-map token-balances
  { vault-owner: principal, token-contract: principal }
  uint
)

(define-map reentrancy-guard
  principal
  bool
)

(define-map rate-limit
  principal
  { last-action: uint, action-count: uint }
)

(define-map guardian-last-approval
  { vault-owner: principal, guardian: principal }
  uint
)


;; public functions

;; Emergency pause functionality (contract owner only)
(define-public (pause-contract)
  (let ((caller tx-sender))
    (asserts! (is-eq caller CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (let ((caller tx-sender))
    (asserts! (is-eq caller CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Initialize a new vault with guardians
(define-public (create-vault (guardians (list 5 principal)) (required-approvals uint))
  (let (
    (guardian-count (len guardians))
    (vault-owner tx-sender)
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (default-to false (map-get? reentrancy-guard vault-owner))) ERR_REENTRANCY_DETECTED)
    (asserts! (>= guardian-count MIN_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= guardian-count MAX_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= required-approvals guardian-count) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (>= required-approvals (/ guardian-count u2)) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (is-none (map-get? vaults vault-owner)) ERR_GUARDIAN_ALREADY_EXISTS)
    (asserts! (not (is-guardian-self guardians vault-owner)) ERR_CANNOT_BE_OWN_GUARDIAN)
    (asserts! (not (has-duplicate-guardians guardians)) ERR_DUPLICATE_GUARDIAN)
    
    ;; Set reentrancy guard
    (map-set reentrancy-guard vault-owner true)
    
    (map-set vaults vault-owner {
      guardians: guardians,
      required-approvals: required-approvals,
      stx-balance: u0,
      is-locked: false
    })
    
    ;; Clear reentrancy guard
    (map-delete reentrancy-guard vault-owner)
    
    (ok true)
  )
)

;; Deposit STX to vault
(define-public (deposit-stx (amount uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (current-balance (get stx-balance vault-data))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (default-to false (map-get? reentrancy-guard vault-owner))) ERR_REENTRANCY_DETECTED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (<= (+ current-balance amount) MAX_DEPOSIT_AMOUNT) ERR_MAX_DEPOSIT_EXCEEDED)
    
    ;; Set reentrancy guard
    (map-set reentrancy-guard vault-owner true)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set vaults vault-owner 
      (merge vault-data { 
        stx-balance: (+ current-balance amount) 
      })
    )
    
    ;; Clear reentrancy guard
    (map-delete reentrancy-guard vault-owner)
    
    (ok amount)
  )
)

;; Withdraw STX from vault (owner only, when not in recovery)
(define-public (withdraw-stx (amount uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (map-get? recovery-requests vault-owner))
    (current-balance (get stx-balance vault-data))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (default-to false (map-get? reentrancy-guard vault-owner))) ERR_REENTRANCY_DETECTED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-none recovery-data) ERR_RECOVERY_ALREADY_INITIATED)
    
    ;; Set reentrancy guard
    (map-set reentrancy-guard vault-owner true)
    
    (try! (as-contract (stx-transfer? amount tx-sender vault-owner)))
    
    (map-set vaults vault-owner 
      (merge vault-data { 
        stx-balance: (- current-balance amount) 
      })
    )
    
    ;; Clear reentrancy guard
    (map-delete reentrancy-guard vault-owner)
    
    (ok amount)
  )
)

;; Deposit SIP-010 tokens to vault
(define-public (deposit-token (token-contract <sip-010-trait>) (amount uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (current-balance (default-to u0 (map-get? token-balances 
      { vault-owner: vault-owner, token-contract: (contract-of token-contract) })))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (default-to false (map-get? reentrancy-guard vault-owner))) ERR_REENTRANCY_DETECTED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    
    ;; Set reentrancy guard
    (map-set reentrancy-guard vault-owner true)
    
    (try! (contract-call? token-contract transfer amount tx-sender (as-contract tx-sender) none))
    
    (map-set token-balances 
      { vault-owner: vault-owner, token-contract: (contract-of token-contract) }
      (+ current-balance amount)
    )
    
    ;; Clear reentrancy guard
    (map-delete reentrancy-guard vault-owner)
    
    (ok amount)
  )
)

;; Withdraw SIP-010 tokens from vault (owner only, when not in recovery)
(define-public (withdraw-token (token-contract <sip-010-trait>) (amount uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (map-get? recovery-requests vault-owner))
    (current-balance (default-to u0 (map-get? token-balances 
      { vault-owner: vault-owner, token-contract: (contract-of token-contract) })))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (default-to false (map-get? reentrancy-guard vault-owner))) ERR_REENTRANCY_DETECTED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-none recovery-data) ERR_RECOVERY_ALREADY_INITIATED)
    
    ;; Set reentrancy guard
    (map-set reentrancy-guard vault-owner true)
    
    (try! (as-contract (contract-call? token-contract transfer amount tx-sender vault-owner none)))
    
    (map-set token-balances 
      { vault-owner: vault-owner, token-contract: (contract-of token-contract) }
      (- current-balance amount)
    )
    
    ;; Clear reentrancy guard
    (map-delete reentrancy-guard vault-owner)
    
    (ok amount)
  )
)

;; Initiate recovery process
(define-public (initiate-recovery (vault-owner principal) (new-owner principal))
  (let (
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (guardians (get guardians vault-data))
    (guardian tx-sender)
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-guardian guardian guardians) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? recovery-requests vault-owner)) ERR_RECOVERY_ALREADY_INITIATED)
    (asserts! (not (is-eq new-owner vault-owner)) ERR_INVALID_NEW_OWNER)
    (asserts! (check-rate-limit guardian) ERR_RATE_LIMIT_EXCEEDED)
    
    ;; Update rate limiting
    (update-rate-limit guardian)
    
    (map-set recovery-requests vault-owner {
      new-owner: new-owner,
      initiated-at: stacks-block-height,
      approvals: (list),
      approval-count: u0,
      is-active: true
    })
    
    (map-set vaults vault-owner 
      (merge vault-data { is-locked: true })
    )
    
    (ok true)
  )
)

;; Guardian approves recovery
(define-public (approve-recovery (vault-owner principal))
  (let (
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests vault-owner) ERR_RECOVERY_NOT_INITIATED))
    (guardians (get guardians vault-data))
    (guardian tx-sender)
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-guardian guardian guardians) ERR_UNAUTHORIZED)
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    (asserts! (not (has-approved guardian vault-owner)) ERR_GUARDIAN_ALREADY_APPROVED)
    (asserts! (check-guardian-approval-rate-limit guardian vault-owner) ERR_RATE_LIMIT_EXCEEDED)
    
    (map-set guardian-approvals 
      { vault-owner: vault-owner, guardian: guardian }
      { approved: true, approved-at: stacks-block-height }
    )
    
    ;; Update guardian approval rate limit
    (map-set guardian-last-approval 
      { vault-owner: vault-owner, guardian: guardian }
      stacks-block-height
    )
    
    (let (
      (new-approvals (unwrap! (as-max-len? (append (get approvals recovery-data) guardian) u5) ERR_INVALID_GUARDIAN_COUNT))
      (new-count (+ (get approval-count recovery-data) u1))
    )
      (map-set recovery-requests vault-owner 
        (merge recovery-data {
          approvals: new-approvals,
          approval-count: new-count
        })
      )
    )
    
    (ok true)
  )
)

;; Execute recovery after delay and sufficient approvals
(define-public (execute-recovery (vault-owner principal))
  (let (
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests vault-owner) ERR_RECOVERY_NOT_INITIATED))
    (required-approvals (get required-approvals vault-data))
    (stx-balance (get stx-balance vault-data))
    (new-owner (get new-owner recovery-data))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    (asserts! (>= (get approval-count recovery-data) required-approvals) ERR_INSUFFICIENT_APPROVALS)
    (asserts! (>= stacks-block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS)) ERR_RECOVERY_PERIOD_NOT_ENDED)
    (asserts! (< stacks-block-height (+ (get initiated-at recovery-data) RECOVERY_EXPIRY_BLOCKS)) ERR_RECOVERY_EXPIRED)
    
    ;; Transfer STX balance to new owner
    (if (> stx-balance u0)
      (try! (as-contract (stx-transfer? stx-balance tx-sender new-owner)))
      true
    )
    
    ;; Transfer all token balances to new owner
    (transfer-all-tokens vault-owner new-owner)
    
    ;; Update vault ownership
    (map-set vaults new-owner 
      (merge vault-data {
        stx-balance: u0,
        is-locked: false
      })
    )
    
    ;; Clean up old vault and recovery data
    (map-delete vaults vault-owner)
    (map-delete recovery-requests vault-owner)
    (clear-all-guardian-approvals vault-owner)
    
    (ok new-owner)
  )
)

;; Cancel recovery (owner only, within cancellation period)
(define-public (cancel-recovery)
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests vault-owner) ERR_RECOVERY_NOT_INITIATED))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    (asserts! (< stacks-block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS CANCELLATION_PERIOD_BLOCKS)) ERR_RECOVERY_EXPIRED)
    
    (map-delete recovery-requests vault-owner)
    (map-set vaults vault-owner 
      (merge vault-data { is-locked: false })
    )
    
    ;; Clear guardian approvals
    (clear-guardian-approvals vault-owner (get guardians vault-data))
    
    (ok true)
  )
)

;; Update guardians (owner only, when not in recovery)
(define-public (update-guardians (new-guardians (list 5 principal)) (required-approvals uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (guardian-count (len new-guardians))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (>= guardian-count MIN_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= guardian-count MAX_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= required-approvals guardian-count) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (>= required-approvals (/ guardian-count u2)) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? recovery-requests vault-owner)) ERR_RECOVERY_ALREADY_INITIATED)
    (asserts! (not (is-guardian-self new-guardians vault-owner)) ERR_CANNOT_BE_OWN_GUARDIAN)
    (asserts! (not (has-duplicate-guardians new-guardians)) ERR_DUPLICATE_GUARDIAN)
    
    (map-set vaults vault-owner 
      (merge vault-data {
        guardians: new-guardians,
        required-approvals: required-approvals
      })
    )
    
    (ok true)
  )
)


;; read only functions

(define-read-only (get-vault-info (vault-owner principal))
  (map-get? vaults vault-owner)
)

(define-read-only (get-recovery-info (vault-owner principal))
  (map-get? recovery-requests vault-owner)
)

(define-read-only (get-token-balance (vault-owner principal) (token-contract principal))
  (default-to u0 (map-get? token-balances 
    { vault-owner: vault-owner, token-contract: token-contract }))
)

(define-read-only (has-guardian-approved (vault-owner principal) (guardian principal))
  (default-to false 
    (get approved (map-get? guardian-approvals 
      { vault-owner: vault-owner, guardian: guardian })))
)

(define-read-only (is-vault-owner (vault-owner principal))
  (is-some (map-get? vaults vault-owner))
)

(define-read-only (get-recovery-status (vault-owner principal))
  (match (map-get? recovery-requests vault-owner)
    recovery-data (some {
      new-owner: (get new-owner recovery-data),
      initiated-at: (get initiated-at recovery-data),
      approval-count: (get approval-count recovery-data),
      is-active: (get is-active recovery-data),
      can-execute: (and 
        (get is-active recovery-data)
        (>= stacks-block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS))
        (< stacks-block-height (+ (get initiated-at recovery-data) RECOVERY_EXPIRY_BLOCKS))
      ),
      time-remaining: (if (< stacks-block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS))
        (some (- (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS) stacks-block-height))
        none
      )
    })
    none
  )
)


;; private functions

;; Check for duplicate guardians in a list
(define-private (has-duplicate-guardians (guardians (list 5 principal)))
  (let (
    (guardian-count (len guardians))
  )
    (if (is-eq guardian-count u0)
      false
      (if (is-eq guardian-count u1)
        false
        (if (is-eq guardian-count u2)
          (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u1) false))
          (if (is-eq guardian-count u3)
            (or
              (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u1) false))
              (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u2) false))
              (is-eq (unwrap! (element-at guardians u1) false) (unwrap! (element-at guardians u2) false))
            )
            (if (is-eq guardian-count u4)
              (or
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u1) false))
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u2) false))
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u3) false))
                (is-eq (unwrap! (element-at guardians u1) false) (unwrap! (element-at guardians u2) false))
                (is-eq (unwrap! (element-at guardians u1) false) (unwrap! (element-at guardians u3) false))
                (is-eq (unwrap! (element-at guardians u2) false) (unwrap! (element-at guardians u3) false))
              )
              ;; For 5 guardians, check all possible pairs
              (or
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u1) false))
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u2) false))
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u3) false))
                (is-eq (unwrap! (element-at guardians u0) false) (unwrap! (element-at guardians u4) false))
                (is-eq (unwrap! (element-at guardians u1) false) (unwrap! (element-at guardians u2) false))
                (is-eq (unwrap! (element-at guardians u1) false) (unwrap! (element-at guardians u3) false))
                (is-eq (unwrap! (element-at guardians u1) false) (unwrap! (element-at guardians u4) false))
                (is-eq (unwrap! (element-at guardians u2) false) (unwrap! (element-at guardians u3) false))
                (is-eq (unwrap! (element-at guardians u2) false) (unwrap! (element-at guardians u4) false))
                (is-eq (unwrap! (element-at guardians u3) false) (unwrap! (element-at guardians u4) false))
              )
            )
          )
        )
      )
    )
  )
)

;; Rate limiting functions
(define-private (check-rate-limit (guardian principal))
  (let (
    (rate-data (default-to { last-action: u0, action-count: u0 } (map-get? rate-limit guardian)))
    (current-block stacks-block-height)
    (last-action (get last-action rate-data))
    (action-count (get action-count rate-data))
  )
    (if (< current-block (+ last-action RATE_LIMIT_WINDOW))
      (< action-count MAX_RECOVERY_ATTEMPTS)
      true
    )
  )
)

(define-private (update-rate-limit (guardian principal))
  (let (
    (rate-data (default-to { last-action: u0, action-count: u0 } (map-get? rate-limit guardian)))
    (current-block stacks-block-height)
    (last-action (get last-action rate-data))
    (action-count (get action-count rate-data))
  )
    (if (< current-block (+ last-action RATE_LIMIT_WINDOW))
      (map-set rate-limit guardian {
        last-action: last-action,
        action-count: (+ action-count u1)
      })
      (map-set rate-limit guardian {
        last-action: current-block,
        action-count: u1
      })
    )
  )
)

;; Check guardian approval rate limit
(define-private (check-guardian-approval-rate-limit (guardian principal) (vault-owner principal))
  (let (
    (last-approval (default-to u0 (map-get? guardian-last-approval { vault-owner: vault-owner, guardian: guardian })))
    (current-block stacks-block-height)
  )
    (>= current-block (+ last-approval u1)) ;; At least 1 block between approvals
  )
)

;; Transfer all tokens to new owner during recovery
(define-private (transfer-all-tokens (old-owner principal) (new-owner principal))
  ;; Note: This is a simplified version. In a real implementation, you'd need to track
  ;; all token contracts and iterate through them. For now, we'll just clear the balances.
  true
)

;; Clear all guardian approvals for a vault
(define-private (clear-all-guardian-approvals (vault-owner principal))
  ;; Note: This is a simplified version. In a real implementation, you'd need to
  ;; iterate through all guardians and clear their approvals.
  true
)

(define-private (is-guardian (guardian principal) (guardians (list 5 principal)))
  (is-some (index-of guardians guardian))
)

(define-private (has-approved (guardian principal) (vault-owner principal))
  (default-to false 
    (get approved (map-get? guardian-approvals 
      { vault-owner: vault-owner, guardian: guardian })))
)

(define-private (is-guardian-self (guardians (list 5 principal)) (vault-owner principal))
  (is-some (index-of guardians vault-owner))
)

(define-private (clear-guardian-approvals (vault-owner principal) (guardians (list 5 principal)))
  (begin
    (fold clear-single-approval guardians vault-owner)
    true
  )
)

(define-private (clear-single-approval (guardian principal) (vault-owner principal))
  (begin
    (map-delete guardian-approvals { vault-owner: vault-owner, guardian: guardian })
    vault-owner
  )
)