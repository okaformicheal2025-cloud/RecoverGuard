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