;; BlockHarbor Protocol - Digital Resource Management Framework
;; This is a detailed Blockchain system for conditional digital resource transfers with multi-party verification

;; Storage architecture for digital resource lockers
(define-map LockerRepository
  { locker-identifier: uint }
  {
    originator: principal,
    beneficiary: principal,
    resource-type: uint,
    quantity: uint,
    status-flag: (string-ascii 10),
    genesis-block: uint,
    termination-block: uint
  }
)

;; Sequence tracker for locker identifiers
(define-data-var locker-sequence uint u0)

;; Error code definitions
(define-constant ADMIN_ONLY_ERROR (err u100))
(define-constant LOCKER_NOT_FOUND_ERROR (err u101))
(define-constant STATUS_TRANSITION_ERROR (err u102))
(define-constant RESOURCE_MOVEMENT_ERROR (err u103))
(define-constant INVALID_IDENTIFIER_ERROR (err u104))
(define-constant INVALID_QUANTITY_ERROR (err u105))
(define-constant ORIGINATOR_MISMATCH_ERROR (err u106))
(define-constant LIFECYCLE_EXPIRY_ERROR (err u107))
(define-constant PROTOCOL_CONTROLLER tx-sender)
(define-constant LOCKER_LIFECYCLE_SPAN u1008)


;; Protocol utility functions

(define-private (verify-locker-exists (locker-identifier uint))
  (<= locker-identifier (var-get locker-sequence))
)

(define-private (verify-beneficiary-eligibility (target-beneficiary principal))
  (and 
    (not (is-eq target-beneficiary tx-sender))
    (not (is-eq target-beneficiary (as-contract tx-sender)))
  )
)

;; Protocol operations

;; Finalize resource transfer to beneficiary
(define-public (execute-locker-transfer (locker-identifier uint))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (beneficiary (get beneficiary locker-record))
        (quantity (get quantity locker-record))
        (resource-type (get resource-type locker-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_CONTROLLER) (is-eq tx-sender (get originator locker-record))) ADMIN_ONLY_ERROR)
      (asserts! (is-eq (get status-flag locker-record) "pending") STATUS_TRANSITION_ERROR)
      (asserts! (<= block-height (get termination-block locker-record)) LIFECYCLE_EXPIRY_ERROR)
      (match (as-contract (stx-transfer? quantity tx-sender beneficiary))
        success
          (begin
            (map-set LockerRepository
              { locker-identifier: locker-identifier }
              (merge locker-record { status-flag: "completed" })
            )
            (print {event: "transfer_executed", locker-identifier: locker-identifier, beneficiary: beneficiary, resource-type: resource-type, quantity: quantity})
            (ok true)
          )
        error RESOURCE_MOVEMENT_ERROR
      )
    )
  )
)

;; Repatriate resources to originator
(define-public (repatriate-resources (locker-identifier uint))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (quantity (get quantity locker-record))
      )
      (asserts! (is-eq tx-sender PROTOCOL_CONTROLLER) ADMIN_ONLY_ERROR)
      (asserts! (is-eq (get status-flag locker-record) "pending") STATUS_TRANSITION_ERROR)
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set LockerRepository
              { locker-identifier: locker-identifier }
              (merge locker-record { status-flag: "returned" })
            )
            (print {event: "resources_repatriated", locker-identifier: locker-identifier, originator: originator, quantity: quantity})
            (ok true)
          )
        error RESOURCE_MOVEMENT_ERROR
      )
    )
  )
)

;; Originator-initiated termination
(define-public (terminate-locker (locker-identifier uint))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (quantity (get quantity locker-record))
      )
      (asserts! (is-eq tx-sender originator) ADMIN_ONLY_ERROR)
      (asserts! (is-eq (get status-flag locker-record) "pending") STATUS_TRANSITION_ERROR)
      (asserts! (<= block-height (get termination-block locker-record)) LIFECYCLE_EXPIRY_ERROR)
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set LockerRepository
              { locker-identifier: locker-identifier }
              (merge locker-record { status-flag: "cancelled" })
            )
            (print {event: "locker_terminated", locker-identifier: locker-identifier, originator: originator, quantity: quantity})
            (ok true)
          )
        error RESOURCE_MOVEMENT_ERROR
      )
    )
  )
)

;; Prolong locker lifecycle
(define-public (prolong-locker-lifecycle (locker-identifier uint) (additional-blocks uint))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (asserts! (> additional-blocks u0) INVALID_QUANTITY_ERROR)
    (asserts! (<= additional-blocks u1440) INVALID_QUANTITY_ERROR) ;; Maximum extension ~10 days
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record)) 
        (beneficiary (get beneficiary locker-record))
        (current-termination (get termination-block locker-record))
        (new-termination (+ current-termination additional-blocks))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_CONTROLLER)) ADMIN_ONLY_ERROR)
      (asserts! (or (is-eq (get status-flag locker-record) "pending") (is-eq (get status-flag locker-record) "accepted")) STATUS_TRANSITION_ERROR)
      (map-set LockerRepository
        { locker-identifier: locker-identifier }
        (merge locker-record { termination-block: new-termination })
      )
      (print {event: "lifecycle_prolonged", locker-identifier: locker-identifier, requestor: tx-sender, new-termination-block: new-termination})
      (ok true)
    )
  )
)

;; Reclaim expired locker resources
(define-public (reclaim-expired-resources (locker-identifier uint))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (quantity (get quantity locker-record))
        (termination (get termination-block locker-record))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_CONTROLLER)) ADMIN_ONLY_ERROR)
      (asserts! (or (is-eq (get status-flag locker-record) "pending") (is-eq (get status-flag locker-record) "accepted")) STATUS_TRANSITION_ERROR)
      (asserts! (> block-height termination) (err u108)) ;; Must be expired
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set LockerRepository
              { locker-identifier: locker-identifier }
              (merge locker-record { status-flag: "expired" })
            )
            (print {event: "expired_resources_reclaimed", locker-identifier: locker-identifier, originator: originator, quantity: quantity})
            (ok true)
          )
        error RESOURCE_MOVEMENT_ERROR
      )
    )
  )
)

;; Initiate contested locker procedure
(define-public (contest-locker (locker-identifier uint) (justification (string-ascii 50)))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (beneficiary (get beneficiary locker-record))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ADMIN_ONLY_ERROR)
      (asserts! (or (is-eq (get status-flag locker-record) "pending") (is-eq (get status-flag locker-record) "accepted")) STATUS_TRANSITION_ERROR)
      (asserts! (<= block-height (get termination-block locker-record)) LIFECYCLE_EXPIRY_ERROR)
      (map-set LockerRepository
        { locker-identifier: locker-identifier }
        (merge locker-record { status-flag: "disputed" })
      )
      (print {event: "locker_contested", locker-identifier: locker-identifier, contesting-party: tx-sender, justification: justification})
      (ok true)
    )
  )
)

;; Cryptographic validation mechanism
(define-public (register-cryptographic-validation (locker-identifier uint) (cryptographic-proof (buff 65)))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (beneficiary (get beneficiary locker-record))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ADMIN_ONLY_ERROR)
      (asserts! (or (is-eq (get status-flag locker-record) "pending") (is-eq (get status-flag locker-record) "accepted")) STATUS_TRANSITION_ERROR)
      (print {event: "validation_registered", locker-identifier: locker-identifier, validator: tx-sender, cryptographic-proof: cryptographic-proof})
      (ok true)
    )
  )
)

;; Register recovery destination
(define-public (register-recovery-destination (locker-identifier uint) (recovery-destination principal))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
      )
      (asserts! (is-eq tx-sender originator) ADMIN_ONLY_ERROR)
      (asserts! (not (is-eq recovery-destination tx-sender)) (err u111)) ;; Recovery destination must differ
      (asserts! (is-eq (get status-flag locker-record) "pending") STATUS_TRANSITION_ERROR)
      (print {event: "recovery_destination_registered", locker-identifier: locker-identifier, originator: originator, recovery-destination: recovery-destination})
      (ok true)
    )
  )
)

;; Adjudicate contested locker
(define-public (adjudicate-contested-locker (locker-identifier uint) (originator-allocation uint))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (asserts! (is-eq tx-sender PROTOCOL_CONTROLLER) ADMIN_ONLY_ERROR)
    (asserts! (<= originator-allocation u100) INVALID_QUANTITY_ERROR) ;; Must be percentage 0-100
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (beneficiary (get beneficiary locker-record))
        (quantity (get quantity locker-record))
        (originator-share (/ (* quantity originator-allocation) u100))
        (beneficiary-share (- quantity originator-share))
      )
      (asserts! (is-eq (get status-flag locker-record) "disputed") (err u112)) ;; Must be in disputed state
      (asserts! (<= block-height (get termination-block locker-record)) LIFECYCLE_EXPIRY_ERROR)

      ;; Distribute originator's share
      (unwrap! (as-contract (stx-transfer? originator-share tx-sender originator)) RESOURCE_MOVEMENT_ERROR)

      ;; Distribute beneficiary's share
      (unwrap! (as-contract (stx-transfer? beneficiary-share tx-sender beneficiary)) RESOURCE_MOVEMENT_ERROR)

      (map-set LockerRepository
        { locker-identifier: locker-identifier }
        (merge locker-record { status-flag: "resolved" })
      )
      (print {event: "contest_adjudicated", locker-identifier: locker-identifier, originator: originator, beneficiary: beneficiary, 
              originator-share: originator-share, beneficiary-share: beneficiary-share, originator-allocation: originator-allocation})
      (ok true)
    )
  )
)

;; Register additional oversight for high-value lockers
(define-public (register-additional-oversight (locker-identifier uint) (oversight-authority principal))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (quantity (get quantity locker-record))
      )
      ;; Only for high-value lockers (> 1000 STX)
      (asserts! (> quantity u1000) (err u120))
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_CONTROLLER)) ADMIN_ONLY_ERROR)
      (asserts! (is-eq (get status-flag locker-record) "pending") STATUS_TRANSITION_ERROR)
      (print {event: "oversight_registered", locker-identifier: locker-identifier, oversight-authority: oversight-authority, requestor: tx-sender})
      (ok true)
    )
  )
)

;; Suspend anomalous locker
(define-public (suspend-anomalous-locker (locker-identifier uint) (justification (string-ascii 100)))
  (begin
    (asserts! (verify-locker-exists locker-identifier) INVALID_IDENTIFIER_ERROR)
    (let
      (
        (locker-record (unwrap! (map-get? LockerRepository { locker-identifier: locker-identifier }) LOCKER_NOT_FOUND_ERROR))
        (originator (get originator locker-record))
        (beneficiary (get beneficiary locker-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_CONTROLLER) (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ADMIN_ONLY_ERROR)
      (asserts! (or (is-eq (get status-flag locker-record) "pending") 
                   (is-eq (get status-flag locker-record) "accepted")) 
                STATUS_TRANSITION_ERROR)
      (map-set LockerRepository
        { locker-identifier: locker-identifier }
        (merge locker-record { status-flag: "frozen" })
      )
      (print {event: "locker_suspended", locker-identifier: locker-identifier, reporter: tx-sender, justification: justification})
      (ok true)
    )
  )
)

