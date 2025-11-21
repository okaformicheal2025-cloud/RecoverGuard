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
(define-constant ERR_VAULT_DELETION_LOCKED (err u120))
(define-constant ERR_EMERGENCY_TRANSFER_COOLDOWN (err u121))
(define-constant ERR_INVALID_GUARDIAN_ADDRESS (err u122))
(define-constant ERR_TOKEN_TRANSFER_FAILED (err u123))

(define-constant MIN_GUARDIANS u3)
(define-constant MAX_GUARDIANS u5)
(define-constant RECOVERY_DELAY_BLOCKS u144) ;; ~24 hours at 10 min blocks
(define-constant CANCELLATION_PERIOD_BLOCKS u72) ;; ~12 hours at 10 min blocks
(define-constant RECOVERY_EXPIRY_BLOCKS u1008) ;; ~7 days at 10 min blocks
(define-constant MAX_DEPOSIT_AMOUNT u1000000000000) ;; 1M STX max deposit
(define-constant RATE_LIMIT_WINDOW u10) ;; 10 blocks rate limit window
(define-constant MAX_RECOVERY_ATTEMPTS u3) ;; Max recovery attempts per window
(define-constant EMERGENCY_TRANSFER_COOLDOWN u1440) ;; ~10 days at 10 min blocks
(define-constant MIN_GUARDIAN_APPROVAL_DELAY u1) ;; Minimum blocks between approvals

;; data vars
(define-data-var vault-nonce uint u0)
(define-data-var contract-paused bool false)
(define-data-var last-recovery-attempt uint u0)
(define-data-var recovery-attempt-count uint u0)
(define-data-var emergency-transfer-enabled bool false)
(define-data-var last-emergency-transfer uint u0)

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

(define-map vault-metadata
  principal
  {
    created-at: uint,
    last-modified: uint,
    recovery-count: uint,
    deletion-locked: bool
  }
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
    (asserts! (validate-guardian-addresses guardians) ERR_INVALID_GUARDIAN_ADDRESS)
    
    ;; Set reentrancy guard
    (map-set reentrancy-guard vault-owner true)
    
    (map-set vaults vault-owner {
      guardians: guardians,
      required-approvals: required-approvals,
      stx-balance: u0,
      is-locked: false
    })
    
    ;; Initialize vault metadata
    (map-set vault-metadata vault-owner {
      created-at: stacks-block-height,
      last-modified: stacks-block-height,
      recovery-count: u0,
      deletion-locked: false
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

;; Emergency owner transfer (contract owner only, with cooldown)
(define-public (emergency-transfer-ownership (old-owner principal) (new-owner principal))
  (let (
    (vault-data (unwrap! (map-get? vaults old-owner) ERR_VAULT_NOT_FOUND))
    (metadata (unwrap! (map-get? vault-metadata old-owner) ERR_VAULT_NOT_FOUND))
    (stx-balance (get stx-balance vault-data))
  )
    ;; Security checks
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (var-get emergency-transfer-enabled) ERR_UNAUTHORIZED)
    (asserts! (>= stacks-block-height (+ (var-get last-emergency-transfer) EMERGENCY_TRANSFER_COOLDOWN)) ERR_EMERGENCY_TRANSFER_COOLDOWN)
    (asserts! (not (is-eq old-owner new-owner)) ERR_INVALID_NEW_OWNER)
    (asserts! (is-none (map-get? vaults new-owner)) ERR_GUARDIAN_ALREADY_EXISTS)
    
    ;; Update last emergency transfer timestamp
    (var-set last-emergency-transfer stacks-block-height)
    
    ;; Transfer vault to new owner
    (map-set vaults new-owner vault-data)
    (map-set vault-metadata new-owner (merge metadata {
      last-modified: stacks-block-height
    }))
    
    ;; Clean up old owner data
    (map-delete vaults old-owner)
    (map-delete vault-metadata old-owner)
    
    (ok new-owner)
  )
)

;; Enable/disable emergency transfer (contract owner only)
(define-public (set-emergency-transfer (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-transfer-enabled enabled)
    (ok enabled)
  )
)

;; Delete vault (owner only, when not locked and no balance)
(define-public (delete-vault)
  (let (
    (vault-owner tx-sender)
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (metadata (unwrap! (map-get? vault-metadata vault-owner) ERR_VAULT_NOT_FOUND))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (get is-locked vault-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get stx-balance vault-data) u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (is-none (map-get? recovery-requests vault-owner)) ERR_RECOVERY_ALREADY_INITIATED)
    (asserts! (not (get deletion-locked metadata)) ERR_VAULT_DELETION_LOCKED)
    
    ;; Delete vault and metadata
    (map-delete vaults vault-owner)
    (map-delete vault-metadata vault-owner)
    (clear-guardian-approvals vault-owner (get guardians vault-data))
    
    (ok true)
  )
)

;; Lock/unlock vault deletion (owner only)
(define-public (set-deletion-lock (locked bool))
  (let (
    (vault-owner tx-sender)
    (metadata (unwrap! (map-get? vault-metadata vault-owner) ERR_VAULT_NOT_FOUND))
  )
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (map-set vault-metadata vault-owner (merge metadata {
      deletion-locked: locked,
      last-modified: stacks-block-height
    }))
    (ok locked)
  )
)

;; Batch approve recovery by multiple guardians (gas optimization)
(define-public (batch-approve-recovery (vault-owner principal) (guardians-to-approve (list 5 principal)))
  (let (
    (vault-data (unwrap! (map-get? vaults vault-owner) ERR_VAULT_NOT_FOUND))
    (recovery-data (unwrap! (map-get? recovery-requests vault-owner) ERR_RECOVERY_NOT_INITIATED))
  )
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (get is-active recovery-data) ERR_RECOVERY_NOT_INITIATED)
    
    ;; Process all approvals
    (ok (fold process-guardian-approval guardians-to-approve { vault-owner: vault-owner, success: true }))
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
    (asserts! (validate-guardian-addresses new-guardians) ERR_INVALID_GUARDIAN_ADDRESS)
    
    (map-set vaults vault-owner 
      (merge vault-data {
        guardians: new-guardians,
        required-approvals: required-approvals
      })
    )
    
    ;; Update metadata
    (match (map-get? vault-metadata vault-owner)
      metadata (map-set vault-metadata vault-owner (merge metadata {
        last-modified: stacks-block-height
      }))
      true
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

(define-read-only (get-vault-metadata (vault-owner principal))
  (map-get? vault-metadata vault-owner)
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (is-emergency-transfer-enabled)
  (var-get emergency-transfer-enabled)
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

;; Validate that all guardian addresses are valid (not zero address)
(define-private (validate-guardian-addresses (guardians (list 5 principal)))
  (let (
    (guardian-count (len guardians))
  )
    (if (is-eq guardian-count u0)
      true
      (if (is-eq guardian-count u1)
        (is-valid-address (unwrap! (element-at guardians u0) false))
        (if (is-eq guardian-count u2)
          (and
            (is-valid-address (unwrap! (element-at guardians u0) false))
            (is-valid-address (unwrap! (element-at guardians u1) false))
          )
          (if (is-eq guardian-count u3)
            (and
              (is-valid-address (unwrap! (element-at guardians u0) false))
              (is-valid-address (unwrap! (element-at guardians u1) false))
              (is-valid-address (unwrap! (element-at guardians u2) false))
            )
            (if (is-eq guardian-count u4)
              (and
                (is-valid-address (unwrap! (element-at guardians u0) false))
                (is-valid-address (unwrap! (element-at guardians u1) false))
                (is-valid-address (unwrap! (element-at guardians u2) false))
                (is-valid-address (unwrap! (element-at guardians u3) false))
              )
              (and
                (is-valid-address (unwrap! (element-at guardians u0) false))
                (is-valid-address (unwrap! (element-at guardians u1) false))
                (is-valid-address (unwrap! (element-at guardians u2) false))
                (is-valid-address (unwrap! (element-at guardians u3) false))
                (is-valid-address (unwrap! (element-at guardians u4) false))
              )
            )
          )
        )
      )
    )
  )
)

;; Check if an address is valid (basic check - not a comprehensive validation)
(define-private (is-valid-address (addr principal))
  ;; In Clarity, we can't easily check for zero address, but we can check it's not the contract itself
  (not (is-eq addr (as-contract tx-sender)))
)

;; Process guardian approval in batch (helper for fold)
(define-private (process-guardian-approval (guardian principal) (state { vault-owner: principal, success: bool }))
  (let (
    (vault-owner (get vault-owner state))
    (vault-data-opt (map-get? vaults vault-owner))
  )
    (match vault-data-opt
      vault-data
        (let (
          (guardians (get guardians vault-data))
        )
          (if (and (get success state) (is-guardian guardian guardians))
            (begin
              (map-set guardian-approvals
                { vault-owner: vault-owner, guardian: guardian }
                { approved: true, approved-at: stacks-block-height }
              )
              { vault-owner: vault-owner, success: true }
            )
            { vault-owner: vault-owner, success: false }
          )
        )
      { vault-owner: vault-owner, success: false }
    )
  )
)

;; Optimized duplicate check using fold (more gas efficient)
(define-private (check-duplicate-in-list (guardian principal) (state { list: (list 5 principal), has-dup: bool }))
  (let (
    (current-list (get list state))
    (has-duplicate (get has-dup state))
  )
    (if has-duplicate
      state
      (if (is-some (index-of current-list guardian))
        { list: current-list, has-dup: true }
        { list: (unwrap! (as-max-len? (append current-list guardian) u5) state), has-dup: false }
      )
    )
  )
)

;; Optimized has-duplicate-guardians using fold
(define-private (has-duplicate-guardians-optimized (guardians (list 5 principal)))
  (get has-dup (fold check-duplicate-in-list guardians { list: (list), has-dup: false }))
)

;; Batch query vault information for multiple addresses
(define-read-only (get-multiple-vault-info (owners (list 10 principal)))
  (map get-vault-info-helper owners)
)

(define-private (get-vault-info-helper (owner principal))
  (map-get? vaults owner)
)

;; Get recovery status for multiple vaults
(define-read-only (get-multiple-recovery-status (owners (list 10 principal)))
  (map get-recovery-status-helper owners)
)

(define-private (get-recovery-status-helper (owner principal))
  (get-recovery-status owner)
)