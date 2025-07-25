;; Enhanced Multi-Role Voting Smart Contract
;; Advanced features: Multiple elections, delegate voting, quorum, weighted votes, 
;; audit trails, emergency controls, and comprehensive analytics

;; Constants
(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-admin-only (err u101))
(define-constant err-voter-only (err u102))
(define-constant err-not-found (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-election-not-active (err u106))
(define-constant err-election-ended (err u107))
(define-constant err-invalid-candidate (err u108))
(define-constant err-unauthorized (err u109))
(define-constant err-insufficient-funds (err u110))
(define-constant err-quorum-not-met (err u111))
(define-constant err-invalid-delegation (err u112))
(define-constant err-emergency-stop (err u113))
(define-constant err-invalid-weight (err u114))
(define-constant err-proposal-exists (err u115))
(define-constant err-voting-closed (err u116))
(define-constant err-invalid-timelock (err u117))

;; Role definitions
(define-constant role-admin u1)
(define-constant role-voter u2)
(define-constant role-observer u3)
(define-constant role-delegate u4)
(define-constant role-auditor u5)

;; Election types
(define-constant election-type-simple u1)
(define-constant election-type-weighted u2)
(define-constant election-type-delegated u3)
(define-constant election-type-proposal u4)

;; Election status
(define-constant status-pending u0)
(define-constant status-active u1)
(define-constant status-ended u2)
(define-constant status-cancelled u3)
(define-constant status-emergency-stopped u4)

;; Vote types
(define-constant vote-yes u1)
(define-constant vote-no u2)
(define-constant vote-abstain u3)

;; Data Variables
(define-data-var next-election-id uint u1)
(define-data-var emergency-stop bool false)
(define-data-var platform-fee uint u1000) ;; Fee in microSTX
(define-data-var min-quorum-percentage uint u20) ;; 20% minimum
(define-data-var max-delegation-depth uint u3)
(define-data-var voting-token-supply uint u1000000)

;; Data Maps - Elections
(define-map elections uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    election-type: uint,
    status: uint,
    start-block: uint,
    end-block: uint,
    min-quorum: uint,
    total-eligible-voters: uint,
    total-votes-cast: uint,
    requires-fee: bool,
    timelock-blocks: uint,
    execution-block: uint
})

;; Data Maps - Candidates & Proposals
(define-map candidates {election-id: uint, candidate-id: uint} {
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint,
    weighted-vote-count: uint,
    proposal-data: (optional (string-ascii 1000))
})

(define-map election-candidate-count uint uint)

;; Data Maps - Users & Roles
(define-map user-roles {user: principal, election-id: uint} uint)
(define-map global-user-roles principal uint)
(define-map user-weights principal uint)
(define-map user-reputation principal uint)

;; Data Maps - Voting
(define-map user-votes {user: principal, election-id: uint} {
    candidate-id: uint,
    vote-type: uint,
    block-height: uint,
    weight-used: uint,
    delegated-from: (optional principal)
})

(define-map vote-delegations {delegator: principal, election-id: uint} principal)
(define-map delegation-chains {delegate: principal, election-id: uint} (list 10 principal))

;; Data Maps - Analytics & Audit
(define-map election-analytics uint {
    participation-rate: uint,
    avg-vote-time: uint,
    delegation-count: uint,
    audit-hash: (buff 32)
})

(define-map vote-audit-trail {election-id: uint, vote-index: uint} {
    voter: principal,
    timestamp: uint,
    action: (string-ascii 50),
    details: (string-ascii 200)
})

(define-map election-vote-count uint uint)

;; Data Maps - Financial
(define-map election-fees uint uint)
(define-map user-fee-payments {user: principal, election-id: uint} uint)

;; Initialize contract
(map-set global-user-roles contract-owner role-admin)
(map-set user-weights contract-owner u100)
(map-set user-reputation contract-owner u1000)

;; Helper Functions (defined early to avoid unresolved function errors)
(define-read-only (is-global-admin (user principal))
    (is-eq (default-to u0 (map-get? global-user-roles user)) role-admin)
)

(define-read-only (is-election-creator (user principal) (election-id uint))
    (match (map-get? elections election-id)
        election (is-eq user (get creator election))
        false
    )
)

(define-read-only (can-vote (user principal) (election-id uint))
    (let ((role (default-to u0 (map-get? user-roles {user: user, election-id: election-id})))
          (global-role (default-to u0 (map-get? global-user-roles user))))
        (or (is-eq role role-voter) (is-eq global-role role-admin))
    )
)

(define-read-only (can-receive-delegation (user principal) (election-id uint))
    (let ((role (default-to u0 (map-get? user-roles {user: user, election-id: election-id}))))
        (is-eq role role-delegate)
    )
)

(define-read-only (get-user-weight (user principal))
    (default-to u1 (map-get? user-weights user))
)

(define-read-only (get-user-reputation (user principal))
    (default-to u0 (map-get? user-reputation user))
)

(define-read-only (has-voted (user principal) (election-id uint))
    (is-some (map-get? user-votes {user: user, election-id: election-id}))
)

(define-read-only (is-election-active (election-id uint))
    (match (map-get? elections election-id)
        election (and (is-eq (get status election) status-active)
                     (>= block-height (get start-block election))
                     (< block-height (get end-block election)))
        false
    )
)

(define-read-only (is-election-ended (election-id uint))
    (match (map-get? elections election-id)
        election (or (is-eq (get status election) status-ended)
                    (>= block-height (get end-block election)))
        false
    )
)

(define-read-only (is-election-pending (election-id uint))
    (match (map-get? elections election-id)
        election (is-eq (get status election) status-pending)
        false
    )
)

(define-read-only (is-quorum-met (election-id uint))
    (match (map-get? elections election-id)
        election (let ((required-votes (/ (* (get total-eligible-voters election) (get min-quorum election)) u100)))
                    (>= (get total-votes-cast election) required-votes))
        false
    )
)

;; Private Helper Functions
(define-private (add-audit-entry (election-id uint) (action (string-ascii 50)) (details (string-ascii 200)))
    (let ((vote-count (+ (default-to u0 (map-get? election-vote-count election-id)) u1)))
        (map-set vote-audit-trail {election-id: election-id, vote-index: vote-count} {
            voter: tx-sender,
            timestamp: block-height,
            action: action,
            details: details
        })
        (map-set election-vote-count election-id vote-count)
        true
    )
)

(define-private (pay-voting-fee (election-id uint))
    (let ((fee (var-get platform-fee)))
        (try! (stx-transfer? fee tx-sender contract-owner))
        (map-set user-fee-payments {user: tx-sender, election-id: election-id} fee)
        (map-set election-fees election-id 
            (+ (default-to u0 (map-get? election-fees election-id)) fee))
        (ok true)
    )
)

(define-private (calculate-delegation-weight (delegator principal) (current-weight uint))
    (+ current-weight (get-user-weight delegator))
)

(define-private (verify-delegation (delegator principal) (is-valid bool))
    (and is-valid (can-vote delegator u1)) ;; Simplified for example
)

(define-private (creates-delegation-cycle (delegator principal) (delegate principal) (election-id uint))
    false ;; Simplified - would need recursive checking in production
)

(define-private (record-delegated-vote (election-id uint) (candidate-id uint) (vote-type uint) (weight uint) (delegators (list 10 principal)))
    true ;; Simplified implementation
)

(define-private (calculate-avg-vote-time (election-id uint))
    u100 ;; Simplified - would calculate based on vote timestamps
)

(define-private (count-delegations (election-id uint))
    u0 ;; Simplified - would count delegation entries
)

(define-private (generate-audit-hash (election-id uint))
    0x0000000000000000000000000000000000000000000000000000000000000000 ;; Simplified
)

(define-private (uint-to-string (value uint))
    "1" ;; Simplified - would convert uint to string
)

(define-private (principal-to-string (user principal))
    "user" ;; Simplified - would convert principal to string
)

(define-private (string-append (str1 (string-ascii 50)) (str2 (string-ascii 50)))
    str1 ;; Simplified - would concatenate strings
)

;; View Helper Functions
(define-private (get-user-vote-for-election (election-id uint))
    (map-get? user-votes {user: tx-sender, election-id: election-id})
)

(define-private (get-candidate-by-id-with-election (candidate-id uint))
    (map-get? candidates {election-id: u1, candidate-id: candidate-id}) ;; Simplified - would use actual election-id
)

(define-private (get-all-candidates (election-id uint) (count uint))
    (map get-candidate-by-id-with-election (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
)

(define-private (get-candidate-by-id (candidate-info {election-id: uint, candidate-id: uint}))
    (map-get? candidates candidate-info)
)

(define-private (generate-index-list (start uint) (count uint))
    (list start (+ start u1) (+ start u2) (+ start u3) (+ start u4))
)

(define-private (get-audit-entry-by-index (index uint))
    (map-get? vote-audit-trail {election-id: u1, vote-index: index})
)

;; Enhanced Role Management
(define-public (assign-global-role (user principal) (role uint))
    (begin
        (asserts! (is-global-admin tx-sender) err-admin-only)
        (asserts! (<= role role-auditor) err-not-found)
        (map-set global-user-roles user role)
        (ok true)
    )
)

(define-public (assign-election-role (user principal) (election-id uint) (role uint))
    (begin
        (asserts! (or (is-global-admin tx-sender) 
                      (is-election-creator tx-sender election-id)) err-admin-only)
        (asserts! (<= role role-auditor) err-not-found)
        (map-set user-roles {user: user, election-id: election-id} role)
        (ok true)
    )
)

(define-public (set-user-weight (user principal) (weight uint))
    (begin
        (asserts! (is-global-admin tx-sender) err-admin-only)
        (asserts! (and (> weight u0) (<= weight u1000)) err-invalid-weight)
        (map-set user-weights user weight)
        (ok true)
    )
)

(define-public (update-reputation (user principal) (change int))
    (let ((current-rep (get-user-reputation user)))
        (asserts! (is-global-admin tx-sender) err-admin-only)
        (map-set user-reputation user 
            (if (> change 0) 
                (+ current-rep (to-uint change))
                (if (> current-rep (to-uint (- 0 change)))
                    (- current-rep (to-uint (- 0 change)))
                    u0)))
        (ok true)
    )
)

;; Enhanced Election Management
(define-public (create-advanced-election 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (election-type uint)
    (start-block uint)
    (end-block uint)
    (min-quorum uint)
    (requires-fee bool)
    (timelock-blocks uint))
    (let ((election-id (var-get next-election-id)))
        (asserts! (is-global-admin tx-sender) err-admin-only)
        (asserts! (not (var-get emergency-stop)) err-emergency-stop)
        (asserts! (> end-block start-block) err-not-found)
        (asserts! (<= election-type election-type-proposal) err-not-found)
        (asserts! (and (>= min-quorum u1) (<= min-quorum u100)) err-not-found)
        
        (map-set elections election-id {
            title: title,
            description: description,
            creator: tx-sender,
            election-type: election-type,
            status: status-pending,
            start-block: start-block,
            end-block: end-block,
            min-quorum: min-quorum,
            total-eligible-voters: u0,
            total-votes-cast: u0,
            requires-fee: requires-fee,
            timelock-blocks: timelock-blocks,
            execution-block: (+ end-block timelock-blocks)
        })
        
        (map-set election-candidate-count election-id u0)
        (map-set election-vote-count election-id u0)
        (var-set next-election-id (+ election-id u1))
        
        (add-audit-entry election-id "ELECTION_CREATED" "Election created")
        (ok election-id)
    )
)

(define-public (add-proposal-candidate 
    (election-id uint) 
    (name (string-ascii 50)) 
    (description (string-ascii 200))
    (proposal-data (string-ascii 1000)))
    (let ((candidate-id (+ (default-to u0 (map-get? election-candidate-count election-id)) u1)))
        (asserts! (or (is-global-admin tx-sender) 
                      (is-election-creator tx-sender election-id)) err-admin-only)
        (asserts! (is-election-pending election-id) err-election-not-active)
        
        (map-set candidates {election-id: election-id, candidate-id: candidate-id} {
            name: name,
            description: description,
            vote-count: u0,
            weighted-vote-count: u0,
            proposal-data: (some proposal-data)
        })
        
        (map-set election-candidate-count election-id candidate-id)
        (add-audit-entry election-id "CANDIDATE_ADDED" name)
        (ok candidate-id)
    )
)

;; Delegation System
(define-public (delegate-vote (election-id uint) (delegate principal))
    (begin
        (asserts! (can-vote tx-sender election-id) err-voter-only)
        (asserts! (not (is-eq tx-sender delegate)) err-invalid-delegation)
        (asserts! (can-receive-delegation delegate election-id) err-invalid-delegation)
        (asserts! (not (creates-delegation-cycle tx-sender delegate election-id)) err-invalid-delegation)
        
        (map-set vote-delegations {delegator: tx-sender, election-id: election-id} delegate)
        (add-audit-entry election-id "VOTE_DELEGATED" 
              (string-append "Delegated to " (principal-to-string delegate)))
        (ok true)
    )
)

(define-public (revoke-delegation (election-id uint))
    (begin
        (asserts! (can-vote tx-sender election-id) err-voter-only)
        (map-delete vote-delegations {delegator: tx-sender, election-id: election-id})
        (add-audit-entry election-id "DELEGATION_REVOKED" "Delegation revoked")
        (ok true)
    )
)

;; Enhanced Voting System
(define-public (cast-weighted-vote (election-id uint) (candidate-id uint) (vote-type uint))
    (let ((election (unwrap! (map-get? elections election-id) err-not-found))
          (candidate (unwrap! (map-get? candidates {election-id: election-id, candidate-id: candidate-id}) err-invalid-candidate))
          (user-weight (get-user-weight tx-sender))
          (fee-required (get requires-fee election)))
        
        (asserts! (can-vote tx-sender election-id) err-voter-only)
        (asserts! (is-election-active election-id) err-election-not-active)
        (asserts! (not (has-voted tx-sender election-id)) err-already-voted)
        (asserts! (<= vote-type vote-abstain) err-not-found)
        
        ;; Handle fee payment if required
        (if fee-required
            (try! (pay-voting-fee election-id))
            true)
        
        ;; Record vote
        (map-set user-votes {user: tx-sender, election-id: election-id} {
            candidate-id: candidate-id,
            vote-type: vote-type,
            block-height: block-height,
            weight-used: user-weight,
            delegated-from: none
        })
        
        ;; Update candidate vote counts
        (map-set candidates {election-id: election-id, candidate-id: candidate-id}
            (merge candidate {
                vote-count: (+ (get vote-count candidate) u1),
                weighted-vote-count: (+ (get weighted-vote-count candidate) user-weight)
            }))
        
        ;; Update election totals
        (map-set elections election-id
            (merge election {
                total-votes-cast: (+ (get total-votes-cast election) u1)
            }))
        
        (add-audit-entry election-id "VOTE_CAST" 
              (concat "Vote for candidate " (uint-to-string candidate-id)))
        (ok true)
    )
)

(define-public (cast-delegated-vote (election-id uint) (candidate-id uint) (vote-type uint) (delegators (list 10 principal)))
    (let ((total-weight (fold calculate-delegation-weight delegators u0)))
        (asserts! (can-receive-delegation tx-sender election-id) err-unauthorized)
        (asserts! (is-election-active election-id) err-election-not-active)
        (asserts! (> total-weight u0) err-invalid-delegation)
        
        ;; Verify all delegations are valid
        (asserts! (fold verify-delegation delegators true) err-invalid-delegation)
        
        ;; Cast vote with combined weight
        (record-delegated-vote election-id candidate-id vote-type total-weight delegators)
        (ok true)
    )
)

;; Emergency Controls
(define-public (activate-emergency-stop)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set emergency-stop true)
        (ok true)
    )
)

(define-public (deactivate-emergency-stop)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set emergency-stop false)
        (ok true)
    )
)

(define-public (emergency-cancel-election (election-id uint))
    (let ((election (unwrap! (map-get? elections election-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set elections election-id (merge election {status: status-cancelled}))
        (add-audit-entry election-id "EMERGENCY_CANCELLED" "Election cancelled by owner")
        (ok true)
    )
)

;; Financial Functions
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

;; Analytics & Reporting
(define-public (finalize-election-analytics (election-id uint))
    (let ((election (unwrap! (map-get? elections election-id) err-not-found))
          (total-eligible (get total-eligible-voters election))
          (total-voted (get total-votes-cast election)))
        (asserts! (is-global-admin tx-sender) err-admin-only)
        (asserts! (is-election-ended election-id) err-election-not-active)
        
        (map-set election-analytics election-id {
            participation-rate: (if (> total-eligible u0) 
                                  (/ (* total-voted u100) total-eligible) u0),
            avg-vote-time: (calculate-avg-vote-time election-id),
            delegation-count: (count-delegations election-id),
            audit-hash: (generate-audit-hash election-id)
        })
        (ok true)
    )
)

;; Enhanced View Functions
(define-read-only (get-election-details (election-id uint))
    (map-get? elections election-id)
)

(define-read-only (get-election-results (election-id uint))
    (let ((candidate-count (default-to u0 (map-get? election-candidate-count election-id))))
        {
            election: (map-get? elections election-id),
            candidates: (get-all-candidates election-id candidate-count),
            analytics: (map-get? election-analytics election-id),
            quorum-met: (is-quorum-met election-id)
        }
    )
)

(define-read-only (get-user-voting-history (user principal))
    (let ((election-ids (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
        (map get-user-vote-for-election election-ids)
    )
)

(define-read-only (get-delegation-chain (user principal) (election-id uint))
    (default-to (list) (map-get? delegation-chains {delegate: user, election-id: election-id}))
)

(define-read-only (get-audit-trail (election-id uint) (start-index uint) (count uint))
    (map get-audit-entry-by-index 
         (generate-index-list start-index count))
)