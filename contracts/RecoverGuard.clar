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

(define-constant MIN_GUARDIANS u3)
(define-constant MAX_GUARDIANS u5)
(define-constant RECOVERY_DELAY_BLOCKS u144) ;; ~24 hours at 10 min blocks
(define-constant CANCELLATION_PERIOD_BLOCKS u72) ;; ~12 hours at 10 min blocks
(define-constant RECOVERY_EXPIRY_BLOCKS u1008) ;; ~7 days at 10 min blocks

;; data vars
(define-data-var vault-nonce uint u0)

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


;; public functions

;; Initialize a new vault with guardians
(define-public (create-vault (guardians (list 5 principal)) (required-approvals uint))
  (let (
    (guardian-count (len guardians))
    (vault-owner tx-sender)
  )
    (asserts! (>= guardian-count MIN_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= guardian-count MAX_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= required-approvals guardian-count) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (>= required-approvals (/ guardian-count u2)) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (is-none (map-get? vaults vault-owner)) ERR_GUARDIAN_ALREADY_EXISTS)
    (asserts! (not (is-guardian-self guardians vault-owner)) ERR_CANNOT_BE_OWN_GUARDIAN)
    
    (map-set vaults vault-owner {
      guardians: guardians,
      required-approvals: required-approvals,
      stx-balance: u0,
      is-locked: false
    })
    
    (ok true)
  )
)

;; Deposit STX to vault
(define-public (deposit-stx (amount uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set vaults vault-owner 
      (merge vault-data { 
        stx-balance: (+ (get stx-balance vault-data) amount) 
      })
    )
    
    (ok amount)
  )
)

;; Withdraw STX from vault (owner only, when not in recovery)
(define-public (withdraw-stx (amount uint))
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (map-get? recovery-requests vault-owner))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get stx-balance vault-data) amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-none recovery-data) ERR_RECOVERY_ALREADY_INITIATED)
    
    (try! (as-contract (stx-transfer? amount tx-sender vault-owner)))
    
    (map-set vaults vault-owner 
      (merge vault-data { 
        stx-balance: (- (get stx-balance vault-data) amount) 
      })
    )
    
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
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    
    (try! (contract-call? token-contract transfer amount tx-sender (as-contract tx-sender) none))
    
    (map-set token-balances 
      { vault-owner: vault-owner, token-contract: (contract-of token-contract) }
      (+ current-balance amount)
    )
    
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
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-none recovery-data) ERR_RECOVERY_ALREADY_INITIATED)
    
    (try! (as-contract (contract-call? token-contract transfer amount tx-sender vault-owner none)))
    
    (map-set token-balances 
      { vault-owner: vault-owner, token-contract: (contract-of token-contract) }
      (- current-balance amount)
    )
    
    (ok amount)
  )
)

;; Initiate recovery process
(define-public (initiate-recovery (vault-owner principal) (new-owner principal))
  (let (
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (guardians (get guardians vault-data))
  )
    (asserts! (is-guardian tx-sender guardians) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? recovery-requests vault-owner)) ERR_RECOVERY_ALREADY_INITIATED)
    
    (map-set recovery-requests vault-owner {
      new-owner: new-owner,
      initiated-at: block-height,
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
    (asserts! (is-guardian guardian guardians) ERR_UNAUTHORIZED)
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    (asserts! (not (has-approved guardian vault-owner)) ERR_GUARDIAN_ALREADY_EXISTS)
    
    (map-set guardian-approvals 
      { vault-owner: vault-owner, guardian: guardian }
      { approved: true, approved-at: block-height }
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
  )
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    (asserts! (>= (get approval-count recovery-data) required-approvals) ERR_INSUFFICIENT_APPROVALS)
    (asserts! (>= block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS)) ERR_RECOVERY_PERIOD_NOT_ENDED)
    (asserts! (< block-height (+ (get initiated-at recovery-data) RECOVERY_EXPIRY_BLOCKS)) ERR_RECOVERY_EXPIRED)
    
    ;; Transfer STX balance to new owner
    (if (> stx-balance u0)
      (try! (as-contract (stx-transfer? stx-balance tx-sender (get new-owner recovery-data))))
      true
    )
    
    ;; Update vault ownership
    (map-set vaults (get new-owner recovery-data) 
      (merge vault-data {
        stx-balance: u0,
        is-locked: false
      })
    )
    
    ;; Clean up old vault and recovery data
    (map-delete vaults vault-owner)
    (map-delete recovery-requests vault-owner)
    
    (ok (get new-owner recovery-data))
  )
)

;; Cancel recovery (owner only, within cancellation period)
(define-public (cancel-recovery)
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests vault-owner) ERR_RECOVERY_NOT_INITIATED))
  )
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    (asserts! (< block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS CANCELLATION_PERIOD_BLOCKS)) ERR_RECOVERY_EXPIRED)
    
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
    (asserts! (>= guardian-count MIN_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= guardian-count MAX_GUARDIANS) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (<= required-approvals guardian-count) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (>= required-approvals (/ guardian-count u2)) ERR_INVALID_GUARDIAN_COUNT)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? recovery-requests vault-owner)) ERR_RECOVERY_ALREADY_INITIATED)
    (asserts! (not (is-guardian-self new-guardians vault-owner)) ERR_CANNOT_BE_OWN_GUARDIAN)
    
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
        (>= block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS))
        (< block-height (+ (get initiated-at recovery-data) RECOVERY_EXPIRY_BLOCKS))
      ),
      time-remaining: (if (< block-height (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS))
        (some (- (+ (get initiated-at recovery-data) RECOVERY_DELAY_BLOCKS) block-height))
        none
      )
    })
    none
  )
)


;; private functions

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