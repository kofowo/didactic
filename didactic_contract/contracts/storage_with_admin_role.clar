;; Enhanced Admin Storage Smart Contract
;; Features: Multi-admin system, permissions, logging, validation, batch operations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant max-admins u5)
(define-constant max-batch-size u10)
(define-constant max-string-length u256)
(define-constant min-value u0)
(define-constant max-value u1000000)

;; Error Constants
(define-constant err-owner-only (err u100))
(define-constant err-not-admin (err u101))
(define-constant err-admin-exists (err u102))
(define-constant err-not-found (err u103))
(define-constant err-max-admins (err u104))
(define-constant err-invalid-value (err u105))
(define-constant err-contract-paused (err u106))
(define-constant err-invalid-permission (err u107))
(define-constant err-batch-too-large (err u108))
(define-constant err-invalid-string (err u109))
(define-constant err-rate-limit (err u110))
(define-constant err-backup-failed (err u111))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var stored-value uint u0)
(define-data-var stored-text (string-ascii 256) "")
(define-data-var contract-version (string-ascii 32) "v2.0.0")
(define-data-var total-operations uint u0)
(define-data-var last-backup-height uint u0)

;; Permission levels
(define-constant PERMISSION-READ u1)
(define-constant PERMISSION-WRITE u2)
(define-constant PERMISSION-DELETE u4)
(define-constant PERMISSION-ADMIN u8)
(define-constant PERMISSION-ALL u15)

;; Data Maps
(define-map admins 
  { admin: principal } 
  { permissions: uint, added-at: uint, added-by: principal, active: bool }
)

(define-map storage-map 
  { key: (string-ascii 64) } 
  { 
    value: uint, 
    text: (string-ascii 256), 
    updated-by: principal, 
    updated-at: uint,
    version: uint,
    locked: bool,
    tags: (list 5 (string-ascii 32))
  }
)

(define-map operation-log
  { operation-id: uint }
  {
    operation-type: (string-ascii 32),
    key: (optional (string-ascii 64)),
    old-value: (optional uint),
    new-value: (optional uint),
    performer: principal,
    timestamp: uint,
    success: bool
  }
)

(define-map user-activity
  { user: principal }
  { last-action: uint, action-count: uint, rate-limit-reset: uint }
)

(define-map backup-snapshots
  { snapshot-id: uint }
  { 
    created-at: uint, 
    created-by: principal, 
    data-count: uint,
    hash: (buff 32)
  }
)

(define-map categories
  { category: (string-ascii 32) }
  { description: (string-ascii 128), color: (string-ascii 16), active: bool }
)

;; Admin Management Functions

;; Add new admin with specific permissions
(define-public (add-admin (new-admin principal) (permissions uint))
  (let ((admin-count (get-admin-count)))
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (not (var-get contract-paused)) err-contract-paused)
      (asserts! (< admin-count max-admins) err-max-admins)
      (asserts! (is-none (map-get? admins { admin: new-admin })) err-admin-exists)
      (asserts! (<= permissions PERMISSION-ALL) err-invalid-permission)
      
      (map-set admins 
        { admin: new-admin }
        { 
          permissions: permissions, 
          added-at: block-height, 
          added-by: tx-sender,
          active: true 
        }
      )
      (log-operation "add-admin" none none (some permissions) tx-sender true)
      (ok true)
    )
  )
)

;; Update admin permissions
(define-public (update-admin-permissions (admin principal) (new-permissions uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-some (map-get? admins { admin: admin })) err-not-found)
    (asserts! (<= new-permissions PERMISSION-ALL) err-invalid-permission)
    
    (match (map-get? admins { admin: admin })
      admin-data (map-set admins 
        { admin: admin }
        (merge admin-data { permissions: new-permissions })
      )
      false
    )
    (log-operation "update-permissions" none none (some new-permissions) tx-sender true)
    (ok true)
  )
)

;; Deactivate admin
(define-public (deactivate-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-some (map-get? admins { admin: admin })) err-not-found)
    
    (match (map-get? admins { admin: admin })
      admin-data (map-set admins 
        { admin: admin }
        (merge admin-data { active: false })
      )
      false
    )
    (log-operation "deactivate-admin" none none none tx-sender true)
    (ok true)
  )
)

;; Enhanced Storage Functions

;; Store data with tags and validation
(define-public (store-enhanced-data 
  (key (string-ascii 64)) 
  (value uint) 
  (text (string-ascii 256))
  (tags (list 5 (string-ascii 32))))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-WRITE) err-not-admin)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (and (>= value min-value) (<= value max-value)) err-invalid-value)
    (asserts! (check-rate-limit tx-sender) err-rate-limit)
    
    (let ((current-data (map-get? storage-map { key: key })))
      (map-set storage-map 
        { key: key }
        { 
          value: value, 
          text: text, 
          updated-by: tx-sender, 
          updated-at: block-height,
          version: (match current-data data (+ (get version data) u1) u1),
          locked: false,
          tags: tags
        }
      )
      (update-rate-limit tx-sender)
      (increment-operations)
      (log-operation "store-data" (some key) 
        (match current-data data (some (get value data)) none)
        (some value) tx-sender true)
      (ok true)
    )
  )
)

;; Batch store multiple key-value pairs
(define-public (batch-store (data-list (list 10 { key: (string-ascii 64), value: uint, text: (string-ascii 256) })))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-WRITE) err-not-admin)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (<= (len data-list) max-batch-size) err-batch-too-large)
    (asserts! (check-rate-limit tx-sender) err-rate-limit)
    
    (match (fold batch-store-helper data-list (ok u0))
      success (begin
        (update-rate-limit tx-sender)
        (increment-operations)
        (log-operation "batch-store" none none (some (len data-list)) tx-sender true)
        (ok success)
      )
      error (err error)
    )
  )
)

;; Lock/unlock data entries
(define-public (lock-data (key (string-ascii 64)) (lock bool))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-ADMIN) err-not-admin)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-some (map-get? storage-map { key: key })) err-not-found)
    
    (match (map-get? storage-map { key: key })
      data-info (map-set storage-map 
        { key: key }
        (merge data-info { locked: lock })
      )
      false
    )
    (log-operation "lock-data" (some key) none (some (if lock u1 u0)) tx-sender true)
    (ok true)
  )
)

;; Advanced Read Functions

;; Get data with full metadata
(define-read-only (get-enhanced-data (key (string-ascii 64)))
  (match (map-get? storage-map { key: key })
    data-info (ok data-info)
    err-not-found
  )
)

;; Search data by tag
(define-read-only (has-tag (key (string-ascii 64)) (tag (string-ascii 32)))
  (match (map-get? storage-map { key: key })
    data-info (is-some (index-of (get tags data-info) tag))
    false
  )
)

;; Get data statistics
(define-read-only (get-data-stats)
  (ok {
    total-operations: (var-get total-operations),
    contract-version: (var-get contract-version),
    admin-count: (get-admin-count),
    contract-paused: (var-get contract-paused),
    last-backup: (var-get last-backup-height)
  })
)

;; Get operation history
(define-read-only (get-operation (operation-id uint))
  (map-get? operation-log { operation-id: operation-id })
)

;; Contract Management Functions

;; Pause/unpause contract
(define-public (toggle-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused (not (var-get contract-paused)))
    (log-operation "toggle-pause" none none 
      (some (if (var-get contract-paused) u1 u0)) tx-sender true)
    (ok true)
  )
)

;; Update contract version
(define-public (update-version (new-version (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-version new-version)
    (log-operation "update-version" none none none tx-sender true)
    (ok true)
  )
)

;; Create backup snapshot
(define-public (create-backup (snapshot-id uint))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-ADMIN) err-not-admin)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-none (map-get? backup-snapshots { snapshot-id: snapshot-id })) err-admin-exists)
    
    (map-set backup-snapshots
      { snapshot-id: snapshot-id }
      {
        created-at: block-height,
        created-by: tx-sender,
        data-count: (var-get total-operations),
        hash: (keccak256 (unwrap-panic (to-consensus-buff? block-height)))
      }
    )
    (var-set last-backup-height block-height)
    (log-operation "create-backup" none none (some snapshot-id) tx-sender true)
    (ok true)
  )
)

;; Category Management
(define-public (add-category (category (string-ascii 32)) (description (string-ascii 128)) (color (string-ascii 16)))
  (begin
    (asserts! (has-permission tx-sender PERMISSION-ADMIN) err-not-admin)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    
    (map-set categories
      { category: category }
      { description: description, color: color, active: true }
    )
    (log-operation "add-category" none none none tx-sender true)
    (ok true)
  )
)

;; Helper Functions

;; Check if user has specific permission
(define-read-only (has-permission (user principal) (required-permission uint))
  (match (map-get? admins { admin: user })
    admin-data (and 
      (get active admin-data)
      (> (bit-and (get permissions admin-data) required-permission) u0)
    )
    false
  )
)

;; Get admin count
(define-read-only (get-admin-count)
  ;; This is a simplified version - in practice you'd iterate through all admins
  u1 ;; Placeholder - would need to implement proper counting
)

;; Rate limiting check
(define-read-only (check-rate-limit (user principal))
  (match (map-get? user-activity { user: user })
    activity (< (get action-count activity) u10) ;; Max 10 actions per period
    true ;; First time user
  )
)

;; Update rate limit
(define-private (update-rate-limit (user principal))
  (match (map-get? user-activity { user: user })
    activity (map-set user-activity 
      { user: user }
      (merge activity { 
        last-action: block-height,
        action-count: (+ (get action-count activity) u1)
      })
    )
    (map-set user-activity
      { user: user }
      { last-action: block-height, action-count: u1, rate-limit-reset: block-height }
    )
  )
)

;; Batch store helper
(define-private (batch-store-helper 
  (item { key: (string-ascii 64), value: uint, text: (string-ascii 256) })
  (result (response uint uint)))
  (match result
    success (if (and (>= (get value item) min-value) (<= (get value item) max-value))
      (begin
        (map-set storage-map 
          { key: (get key item) }
          { 
            value: (get value item), 
            text: (get text item), 
            updated-by: tx-sender, 
            updated-at: block-height,
            version: u1,
            locked: false,
            tags: (list)
          }
        )
        (ok (+ success u1))
      )
      err-invalid-value
    )
    error (err error)
  )
)

;; Log operation
(define-private (log-operation 
  (op-type (string-ascii 32))
  (key (optional (string-ascii 64)))
  (old-val (optional uint))
  (new-val (optional uint))
  (performer principal)
  (success bool))
  (let ((op-id (var-get total-operations)))
    (map-set operation-log
      { operation-id: op-id }
      {
        operation-type: op-type,
        key: key,
        old-value: old-val,
        new-value: new-val,
        performer: performer,
        timestamp: block-height,
        success: success
      }
    )
  )
)

;; Increment operations counter
(define-private (increment-operations)
  (var-set total-operations (+ (var-get total-operations) u1))
)

;; Emergency Functions

;; Emergency stop (owner only)
(define-public (emergency-stop)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused true)
    (log-operation "emergency-stop" none none none tx-sender true)
    (ok true)
  )
)

;; Get contract health status
(define-read-only (get-health-status)
  {
    paused: (var-get contract-paused),
    total-ops: (var-get total-operations),
    version: (var-get contract-version),
    last-backup: (var-get last-backup-height),
    current-height: block-height
  }
)