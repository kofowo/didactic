;; Multi-Send Contract with Gas Limit Per Transfer
;; This contract enables sending tokens to multiple recipients with gas protection

;; Constants
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-AMOUNT (err u402))
(define-constant ERR-INSUFFICIENT-BALANCE (err u403))
(define-constant ERR-TRANSFER-FAILED (err u404))
(define-constant ERR-EMPTY-RECIPIENTS (err u405))
(define-constant ERR-TOO-MANY-RECIPIENTS (err u406))
(define-constant ERR-GAS-LIMIT-EXCEEDED (err u407))
(define-constant ERR-PAUSED (err u408))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Maximum recipients per batch
(define-constant MAX-RECIPIENTS u200)

;; Data Variables
(define-data-var max-gas-per-transfer uint u10000)
(define-data-var total-gas-used uint u0)
(define-data-var contract-paused bool false)
(define-data-var total-transfers uint u0)
(define-data-var total-volume uint u0)

;; Maps
(define-map gas-usage-by-sender principal uint)
(define-map transfer-count-by-sender principal uint)

;; Read-only functions
(define-read-only (get-max-gas-per-transfer)
  (ok (var-get max-gas-per-transfer))
)

(define-read-only (get-total-gas-used)
  (ok (var-get total-gas-used))
)

(define-read-only (get-contract-stats)
  (ok {
    total-transfers: (var-get total-transfers),
    total-volume: (var-get total-volume),
    total-gas-used: (var-get total-gas-used),
    contract-paused: (var-get contract-paused)
  })
)

(define-read-only (get-sender-stats (sender principal))
  (ok {
    gas-used: (default-to u0 (map-get? gas-usage-by-sender sender)),
    transfer-count: (default-to u0 (map-get? transfer-count-by-sender sender))
  })
)

(define-read-only (is-contract-paused)
  (ok (var-get contract-paused))
)

;; Private functions
(define-private (check-gas-limit (gas-estimate uint))
  (if (<= gas-estimate (var-get max-gas-per-transfer))
    (ok true)
    ERR-GAS-LIMIT-EXCEEDED)
)

(define-private (update-gas-usage (sender principal) (gas-used uint))
  (begin
    (var-set total-gas-used (+ (var-get total-gas-used) gas-used))
    (map-set gas-usage-by-sender sender 
      (+ (default-to u0 (map-get? gas-usage-by-sender sender)) gas-used))
    (ok true)
  )
)

(define-private (update-transfer-count (sender principal))
  (begin
    (var-set total-transfers (+ (var-get total-transfers) u1))
    (map-set transfer-count-by-sender sender 
      (+ (default-to u0 (map-get? transfer-count-by-sender sender)) u1))
    (ok true)
  )
)

;; Transfer function with gas estimation
(define-private (execute-transfer (transfer {to: principal, amount: uint}) (context {sender: principal, running-total: uint, error: (optional uint)}))
  (match (get error context)
    some-error context  ;; If there's already an error, pass it through
    (let (
      (sender (get sender context))
      (recipient (get to transfer))
      (amount (get amount transfer))
      (running-total (get running-total context))
      ;; Simplified gas estimation based on transfer complexity
      (gas-estimate (+ u1000 (* amount u1)))
    )
      (match (check-gas-limit gas-estimate)
        success-check
          (match (stx-transfer? amount sender recipient)
            success-transfer
              (begin
                (unwrap-panic (update-gas-usage sender gas-estimate))
                (unwrap-panic (update-transfer-count sender))
                {sender: sender, running-total: (+ running-total amount), error: none})
            error-transfer
              {sender: sender, running-total: running-total, error: (some u404)})
        error-check
          {sender: sender, running-total: running-total, error: (some u407)}))
  )
)

;; Public functions
(define-public (multi-send (recipients (list 200 {to: principal, amount: uint})))
  (let (
    (sender tx-sender)
    (recipient-count (len recipients))
    (total-amount (fold + (map get-amount recipients) u0))
  )
    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    
    ;; Validate inputs
    (asserts! (> recipient-count u0) ERR-EMPTY-RECIPIENTS)
    (asserts! (<= recipient-count MAX-RECIPIENTS) ERR-TOO-MANY-RECIPIENTS)
    (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Check sender balance
    (asserts! (>= (stx-get-balance sender) total-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Execute transfers
    (let (
      (final-context (fold execute-transfer recipients {sender: sender, running-total: u0, error: none}))
    )
      (match (get error final-context)
        some-error-code
          (err some-error-code)
        (begin
          (var-set total-volume (+ (var-get total-volume) (get running-total final-context)))
          (ok {
            total-sent: (get running-total final-context),
            recipients: recipient-count,
            sender: sender
          })))
    )
  )
)

;; Admin functions
(define-public (set-max-gas-per-transfer (new-limit uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> new-limit u0) ERR-INVALID-AMOUNT)
    (var-set max-gas-per-transfer new-limit)
    (ok new-limit)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Helper function to get amount from transfer object
(define-private (get-amount (transfer {to: principal, amount: uint}))
  (get amount transfer)
)

;; Emergency withdraw function (only owner)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER))
  )
)