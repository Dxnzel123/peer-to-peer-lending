;; Loan Dispute Resolution System
;; Handles conflicts between lenders and borrowers through decentralized arbitration

;; Dispute categories and statuses
(define-constant DISPUTE-PAYMENT "PAYMENT")
(define-constant DISPUTE-TERMS "TERMS")
(define-constant DISPUTE-COLLATERAL "COLLATERAL")
(define-constant DISPUTE-INTEREST "INTEREST")

(define-constant STATUS-OPEN "OPEN")
(define-constant STATUS-UNDER-REVIEW "UNDER_REVIEW")
(define-constant STATUS-RESOLVED "RESOLVED")
(define-constant STATUS-DISMISSED "DISMISSED")

;; Main dispute tracking map
(define-map disputes
    (tuple (loan-id principal) (dispute-id uint))
    (tuple
        (initiator principal)
        (respondent principal)
        (category (string-ascii 20))
        (description (string-ascii 200))
        (evidence-hash (string-ascii 64))
        (status (string-ascii 20))
        (filed-at uint)
        (resolution-deadline uint)
        (arbitrator (optional principal))
        (ruling (optional (string-ascii 100)))
        (compensation-amount uint)
    )
)

;; Arbitrator registry
(define-map arbitrators
    principal
    (tuple
        (active bool)
        (cases-handled uint)
        (success-rate uint)
        (deposit-amount uint)
        (registered-at uint)
    )
)

;; Dispute counter for unique IDs
(define-map dispute-counters principal uint)

;; System parameters
(define-data-var admin principal tx-sender)
(define-data-var arbitrator-deposit uint u1000)
(define-data-var resolution-period uint u1440) ;; 1440 blocks (~1 day)
(define-data-var dispute-fee uint u50)

;; Register as an arbitrator
(define-public (register-arbitrator (deposit-amount uint))
    (begin
        (asserts! (>= deposit-amount (var-get arbitrator-deposit)) (err "Insufficient deposit"))
        (map-set arbitrators tx-sender
            (tuple
                (active true)
                (cases-handled u0)
                (success-rate u100)
                (deposit-amount deposit-amount)
                (registered-at block-height)
            ))
        (ok "Arbitrator registered successfully")
    )
)

;; File a new dispute
(define-public (file-dispute 
    (loan-id principal)
    (respondent principal)
    (category (string-ascii 20))
    (description (string-ascii 200))
    (evidence-hash (string-ascii 64))
)
    (let 
        ((current-count (default-to u0 (map-get? dispute-counters loan-id)))
         (dispute-id (+ current-count u1)))
        (begin
            (asserts! (or (is-eq category DISPUTE-PAYMENT)
                         (is-eq category DISPUTE-TERMS)
                         (is-eq category DISPUTE-COLLATERAL)
                         (is-eq category DISPUTE-INTEREST))
                     (err "Invalid dispute category"))
            (asserts! (not (is-eq tx-sender respondent)) (err "Cannot dispute with yourself"))
            (map-set dispute-counters loan-id dispute-id)
            (map-set disputes (tuple (loan-id loan-id) (dispute-id dispute-id))
                (tuple
                    (initiator tx-sender)
                    (respondent respondent)
                    (category category)
                    (description description)
                    (evidence-hash evidence-hash)
                    (status STATUS-OPEN)
                    (filed-at block-height)
                    (resolution-deadline (+ block-height (var-get resolution-period)))
                    (arbitrator none)
                    (ruling none)
                    (compensation-amount u0)
                ))
            (ok dispute-id)
        )
    )
)

;; Assign arbitrator to a dispute
(define-public (assign-arbitrator (loan-id principal) (dispute-id uint) (arbitrator principal))
    (let 
        ((dispute-key (tuple (loan-id loan-id) (dispute-id dispute-id)))
         (dispute (unwrap! (map-get? disputes dispute-key) (err "Dispute not found")))
         (arbitrator-data (unwrap! (map-get? arbitrators arbitrator) (err "Arbitrator not found"))))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) (err "Only admin can assign arbitrators"))
            (asserts! (is-eq (get status dispute) STATUS-OPEN) (err "Dispute not available for assignment"))
            (asserts! (get active arbitrator-data) (err "Arbitrator not active"))
            (map-set disputes dispute-key
                (merge dispute (tuple 
                    (arbitrator (some arbitrator))
                    (status STATUS-UNDER-REVIEW)
                )))
            (ok "Arbitrator assigned successfully")
        )
    )
)

;; Submit evidence (for both parties)
(define-public (submit-additional-evidence 
    (loan-id principal) 
    (dispute-id uint) 
    (evidence-hash (string-ascii 64))
)
    (let 
        ((dispute-key (tuple (loan-id loan-id) (dispute-id dispute-id)))
         (dispute (unwrap! (map-get? disputes dispute-key) (err "Dispute not found"))))
        (begin
            (asserts! (or (is-eq tx-sender (get initiator dispute))
                         (is-eq tx-sender (get respondent dispute)))
                     (err "Not authorized to submit evidence"))
            (asserts! (is-eq (get status dispute) STATUS-UNDER-REVIEW) (err "Dispute not under review"))
            ;; For simplicity, just update the evidence hash (in a real system, you'd append)
            (map-set disputes dispute-key
                (merge dispute (tuple (evidence-hash evidence-hash))))
            (ok "Additional evidence submitted")
        )
    )
)

;; Resolve dispute (arbitrator only)
(define-public (resolve-dispute 
    (loan-id principal) 
    (dispute-id uint) 
    (ruling (string-ascii 100))
    (winner principal)
    (compensation-amount uint)
)
    (let 
        ((dispute-key (tuple (loan-id loan-id) (dispute-id dispute-id)))
         (dispute (unwrap! (map-get? disputes dispute-key) (err "Dispute not found")))
         (arbitrator-addr (unwrap! (get arbitrator dispute) (err "No arbitrator assigned")))
         (arbitrator-data (unwrap! (map-get? arbitrators arbitrator-addr) (err "Arbitrator not found"))))
        (begin
            (asserts! (is-eq tx-sender arbitrator-addr) (err "Only assigned arbitrator can resolve"))
            (asserts! (is-eq (get status dispute) STATUS-UNDER-REVIEW) (err "Dispute not under review"))
            (asserts! (or (is-eq winner (get initiator dispute))
                         (is-eq winner (get respondent dispute))) 
                     (err "Winner must be one of the parties"))
            (map-set disputes dispute-key
                (merge dispute (tuple 
                    (status STATUS-RESOLVED)
                    (ruling (some ruling))
                    (compensation-amount compensation-amount)
                )))
            ;; Update arbitrator stats
            (map-set arbitrators arbitrator-addr
                (merge arbitrator-data (tuple 
                    (cases-handled (+ (get cases-handled arbitrator-data) u1))
                )))
            (ok "Dispute resolved successfully")
        )
    )
)

;; Dismiss dispute (admin or arbitrator)
(define-public (dismiss-dispute (loan-id principal) (dispute-id uint) (reason (string-ascii 100)))
    (let 
        ((dispute-key (tuple (loan-id loan-id) (dispute-id dispute-id)))
         (dispute (unwrap! (map-get? disputes dispute-key) (err "Dispute not found")))
         (arbitrator-addr (get arbitrator dispute)))
        (begin
            (asserts! (or (is-eq tx-sender (var-get admin))
                         (and (is-some arbitrator-addr) (is-eq tx-sender (unwrap-panic arbitrator-addr))))
                     (err "Not authorized to dismiss"))
            (asserts! (not (is-eq (get status dispute) STATUS-RESOLVED)) (err "Cannot dismiss resolved dispute"))
            (map-set disputes dispute-key
                (merge dispute (tuple 
                    (status STATUS-DISMISSED)
                    (ruling (some reason))
                )))
            (ok "Dispute dismissed")
        )
    )
)

;; Get dispute information
(define-read-only (get-dispute (loan-id principal) (dispute-id uint))
    (map-get? disputes (tuple (loan-id loan-id) (dispute-id dispute-id)))
)

;; Get arbitrator information
(define-read-only (get-arbitrator (arbitrator principal))
    (map-get? arbitrators arbitrator)
)

;; Check if address is qualified arbitrator
(define-read-only (is-qualified-arbitrator (arbitrator principal))
    (let ((arbitrator-data (map-get? arbitrators arbitrator)))
        (if (is-some arbitrator-data)
            (get active (unwrap-panic arbitrator-data))
            false
        )
    )
)

;; Get disputes count for a loan
(define-read-only (get-dispute-count (loan-id principal))
    (default-to u0 (map-get? dispute-counters loan-id))
)

;; Update system parameters (admin only)
(define-public (update-dispute-parameters 
    (new-admin principal)
    (new-arbitrator-deposit uint)
    (new-resolution-period uint)
    (new-dispute-fee uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err "Only admin can update parameters"))
        (var-set admin new-admin)
        (var-set arbitrator-deposit new-arbitrator-deposit)
        (var-set resolution-period new-resolution-period)
        (var-set dispute-fee new-dispute-fee)
        (ok "Parameters updated successfully")
    )
)

;; Deactivate arbitrator
(define-public (deactivate-arbitrator (arbitrator principal))
    (let ((arbitrator-data (unwrap! (map-get? arbitrators arbitrator) (err "Arbitrator not found"))))
        (begin
            (asserts! (or (is-eq tx-sender (var-get admin)) (is-eq tx-sender arbitrator)) 
                     (err "Not authorized"))
            (map-set arbitrators arbitrator
                (merge arbitrator-data (tuple (active false))))
            (ok "Arbitrator deactivated")
        )
    )
)
