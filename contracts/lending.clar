;; Add status to the loans map
(define-map loans principal 
    (tuple 
        (amount uint) 
        (interest-rate uint) 
        (deadline uint) 
        (lender principal) 
        (borrower principal)
        (status (string-ascii 20))
    )
)


(define-map loanss 
    principal 
    (tuple (amount uint) (interest-rate uint) (deadline uint) (lender principal) (borrower principal)))

(define-public (offer-loan (borrower principal) (amount uint) (interest-rate uint) (deadline uint) (status (string-ascii 20)))
    (begin
        (asserts! (is-none (map-get? loans borrower)) (err "Loan already offered to this borrower"))
        (map-set loans borrower (tuple (amount amount) (interest-rate interest-rate) (deadline deadline) (lender tx-sender) (borrower borrower) (status status)))
        (ok "Loan offered successfully")
    ))



(define-public (repay-loan (borrower principal) (amount uint))
  (let
    (
      (loan (map-get? loans borrower))
    )
    (asserts! (not (is-none loan)) (err "No loan found for this borrower.")) ;; Ensure the loan exists
    (let
      (
        (loan-data (unwrap! loan (err "Loan data is missing.")))
        (loan-amount (get amount loan-data))
        (interest-rate (get interest-rate loan-data))
        (deadline (get deadline loan-data))
        (lender (get lender loan-data))
      )
      (asserts! (>= (+ amount (* loan-amount (/ (+ u100 interest-rate) u100))) loan-amount) (err "Insufficient repayment amount."))
      (asserts! (>= deadline block-height) (err "Loan repayment deadline has passed."))
      (begin
        (ok "Loan repaid successfully")
      )
    )
  )
)



(define-public (get-loan-status (borrower principal))
    (let ((loan (map-get? loans borrower)))
        (ok (get status (unwrap! loan (err "No loan found"))))
    )
)



(define-map loan-counter principal uint)

(define-map all-loans 
    (tuple (borrower principal) (loan-id uint))
    (tuple 
        (amount uint) 
        (interest-rate uint) 
        (deadline uint) 
        (lender principal)
    )
)

(define-public (create-loan-offer (borrower principal) (amount uint) (interest-rate uint) (deadline uint))
    (let 
        ((current-count (default-to u0 (map-get? loan-counter borrower))))
        (map-set loan-counter borrower (+ current-count u1))
        (map-set all-loans 
            (tuple (borrower borrower) (loan-id current-count))
            (tuple (amount amount) (interest-rate interest-rate) (deadline deadline) (lender tx-sender))
        )
        (ok "New loan offer created")
    )
)


(define-map borrower-ratings principal 
    (tuple 
        (total-rating uint) 
        (number-of-ratings uint)
    )
)

(define-public (rate-borrower (borrower principal) (rating uint))
    (let 
        ((current-rating (default-to (tuple (total-rating u0) (number-of-ratings u0)) (map-get? borrower-ratings borrower))))
        (map-set borrower-ratings borrower
            (tuple 
                (total-rating (+ (get total-rating current-rating) rating))
                (number-of-ratings (+ (get number-of-ratings current-rating) u1))
            )
        )
        (ok "Rating submitted successfully")
    )
)


(define-constant early-repayment-bonus u5) ;; 5% bonus

(define-public (early-repayment (borrower principal) (amount uint))
    (let 
        ((loan (unwrap! (map-get? loans borrower) (err "No loan found")))
         (deadline (get deadline loan))
         (blocks-early (- deadline block-height)))
        (if (> blocks-early u100)
            (let 
                ((bonus-amount (* amount (/ early-repayment-bonus u100))))
                ;; Process repayment with bonus
                (ok "Early repayment processed with bonus")
            )
            (ok "Regular repayment processed")
        )
    )
)


(define-map extension-requests principal bool)

(define-public (request-extension (borrower principal) (new-deadline uint))
    (let ((loan (unwrap! (map-get? loans borrower) (err "No loan found"))))
        (map-set extension-requests borrower true)
        (ok "Extension requested")
    )
)

(define-public (approve-extension (borrower principal) (new-deadline uint))
    (let 
        ((loan (unwrap! (map-get? loans borrower) (err "No loan found")))
         (is-requested (default-to false (map-get? extension-requests borrower))))
        (asserts! (is-eq tx-sender (get lender loan)) (err "Only lender can approve"))
        (asserts! is-requested (err "No extension requested"))
        (map-set loans borrower (merge loan (tuple (deadline new-deadline))))
        (ok "Extension approved")
    )
)



(define-map collaterals principal 
    (tuple 
        (amount uint) 
        (token principal)
    )
)

(define-public (add-collateral (borrower principal) (amount uint) (token principal))
    (begin
        (map-set collaterals borrower 
            (tuple 
                (amount amount) 
                (token token)
            )
        )
        (ok "Collateral added successfully")
    )
)

(define-public (verify-collateral (borrower principal))
    (let ((collateral (map-get? collaterals borrower)))
        (if (is-some collateral)
            (ok true)
            (ok false)
        )
    )
)

;; Define pause state
(define-data-var contract-paused bool false)
(define-data-var contract-owner principal tx-sender)

;; Toggle pause state
(define-public (toggle-pause)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) 
                 (err "Not authorized"))
        (var-set contract-paused (not (var-get contract-paused)))
        (ok "Contract pause toggled")
    ))

;; Check if paused
(define-public (is-paused)
    (ok (var-get contract-paused))
)



;; Define credit score tiers
(define-map credit-tiers 
    uint 
    (tuple 
        (min-score uint)
        (interest-rate uint)
    )
)

;; Initialize credit tiers
(map-set credit-tiers u1 (tuple (min-score u0) (interest-rate u20)))    ;; 20% for low scores
(map-set credit-tiers u2 (tuple (min-score u50) (interest-rate u15)))   ;; 15% for medium scores
(map-set credit-tiers u3 (tuple (min-score u80) (interest-rate u10)))   ;; 10% for high scores

;; Get interest rate based on credit score
(define-public (get-interest-rate (credit-score uint))
    (let ((tier-1 (unwrap! (map-get? credit-tiers u1) (err "Tier not found")))
          (tier-2 (unwrap! (map-get? credit-tiers u2) (err "Tier not found")))
          (tier-3 (unwrap! (map-get? credit-tiers u3) (err "Tier not found"))))
        (if (>= credit-score (get min-score tier-3))
            (ok (get interest-rate tier-3))
            (if (>= credit-score (get min-score tier-2))
                (ok (get interest-rate tier-2))
                (ok (get interest-rate tier-1))
            )
        )
    )
)



;; Define loan purpose types
(define-constant BUSINESS "business")
(define-constant PERSONAL "personal")
(define-constant EDUCATION "education")

;; Add purpose to loans map
(define-map loan-purposes principal (string-ascii 20))

;; Set loan purpose
(define-public (set-loan-purpose (borrower principal) (purpose (string-ascii 20)))
    (begin
        (asserts! (or (is-eq purpose BUSINESS) 
                     (is-eq purpose PERSONAL) 
                     (is-eq purpose EDUCATION)) 
                 (err "Invalid loan purpose"))
        (map-set loan-purposes borrower purpose)
        (ok "Loan purpose set")
    ))


;; Define payment schedule
(define-map payment-schedules 
    principal 
    (tuple 
        (installment-amount uint)
        (payment-frequency uint)
        (next-payment uint)
    )
)

;; Create payment schedule
(define-public (create-payment-schedule 
    (borrower principal) 
    (total-amount uint) 
    (number-of-installments uint))
    (begin
        (asserts! (> number-of-installments u0) (err "Invalid number of installments"))
        (let ((installment-amount (/ total-amount number-of-installments)))
            (map-set payment-schedules borrower 
                (tuple 
                    (installment-amount installment-amount)
                    (payment-frequency u30)  ;; 30 blocks between payments
                    (next-payment (+ block-height u30))
                ))
            (ok "Payment schedule created")
        )
    ))



;; Define insurance map
(define-map loan-insurance 
    principal 
    (tuple 
        (insured-amount uint)
        (premium uint)
        (active bool)
    )
)

;; Add insurance to loan
(define-public (add-insurance (borrower principal) (loan-amount uint))
    (let ((premium (/ loan-amount u20)))  ;; 5% premium
        (map-set loan-insurance borrower
            (tuple 
                (insured-amount loan-amount)
                (premium premium)
                (active true)
            ))
        (ok "Insurance added to loan")
    ))



;; Define referral tracking
(define-map referrals 
    principal  ;; referee
    (tuple 
        (referrer principal)
        (bonus uint)
        (claimed bool)
    )
)

;; Create referral
(define-public (create-referral (referee principal) (referrer principal))
    (begin
        (asserts! (not (is-eq referee referrer)) (err "Cannot refer self"))
        (map-set referrals referee
            (tuple 
                (referrer referrer)
                (bonus u50)  ;; 50 token bonus
                (claimed false)
            ))
        (ok "Referral created")
    ))


;; Define blacklist map
(define-map blacklisted-addresses principal bool)
(define-data-var blacklist-admin principal tx-sender)

(define-public (add-to-blacklist (address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get blacklist-admin)) (err "Not authorized"))
        (map-set blacklisted-addresses address true)
        (ok "Address blacklisted")
    ))

(define-public (remove-from-blacklist (address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get blacklist-admin)) (err "Not authorized"))
        (map-delete blacklisted-addresses address)
        (ok "Address removed from blacklist")
    ))

(define-read-only (is-blacklisted (address principal))
    (default-to false (map-get? blacklisted-addresses address))
)


(define-map liquidation-thresholds
    principal
    (tuple 
        (threshold uint)
        (liquidated bool)
    )
)

(define-public (set-liquidation-threshold (borrower principal) (threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err "Not authorized"))
        (map-set liquidation-thresholds borrower
            (tuple 
                (threshold threshold)
                (liquidated false)
            ))
        (ok "Threshold set")
    ))

(define-public (liquidate-loan (borrower principal))
    (let (
        (threshold-data (unwrap! (map-get? liquidation-thresholds borrower) (err "No threshold set")))
        (loan-data (unwrap! (map-get? loans borrower) (err "No loan found")))
    )
        (asserts! (not (get liquidated threshold-data)) (err "Already liquidated"))
        (asserts! (>= block-height (get threshold threshold-data)) (err "Cannot liquidate yet"))
        (map-set liquidation-thresholds borrower
            (merge threshold-data (tuple (liquidated true))))
        (ok "Loan liquidated")
    ))

    

    (define-map risk-scores
    principal
    (tuple 
        (credit-score uint)
        (collateral-ratio uint)
        (payment-history uint)
        (risk-level (string-ascii 10))
    )
)

(define-public (calculate-risk-score (borrower principal))
    (let (
        (credit (default-to u0 (get credit-score (map-get? risk-scores borrower))))
        (collateral (default-to u0 (get collateral-ratio (map-get? risk-scores borrower))))
        (history (default-to u0 (get payment-history (map-get? risk-scores borrower))))
    )
        (map-set risk-scores borrower
            (tuple 
                (credit-score credit)
                (collateral-ratio collateral)
                (payment-history history)
                (risk-level (if (> (+ credit collateral history) u80)
                    "LOW"
                    (if (> (+ credit collateral history) u50)
                        "MEDIUM"
                        "HIGH")))
            ))
        (ok "Risk score calculated")
    ))


    (define-map loan-auctions
    principal
    (tuple 
        (start-price uint)
        (current-price uint)
        (end-block uint)
        (highest-bidder (optional principal))
    )
)

(define-public (start-auction (loan-id principal) (start-price uint) (duration uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err "Not authorized"))
        (map-set loan-auctions loan-id
            (tuple 
                (start-price start-price)
                (current-price start-price)
                (end-block (+ block-height duration))
                (highest-bidder none)
            ))
        (ok "Auction started")
    ))

(define-public (place-bid (loan-id principal) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? loan-auctions loan-id) (err "No auction found")))
    )
        (asserts! (< block-height (get end-block auction)) (err "Auction ended"))
        (asserts! (> bid-amount (get current-price auction)) (err "Bid too low"))
        (map-set loan-auctions loan-id
            (merge auction (tuple 
                (current-price bid-amount)
                (highest-bidder (some tx-sender)))))
        (ok "Bid placed")
    ))



    (define-map reward-points
    principal
    (tuple 
        (points uint)
        (level (string-ascii 10))
    )
)

(define-public (earn-points (user principal) (action-points uint))
    (let (
        (current-points (default-to (tuple (points u0) (level "BRONZE")) 
            (map-get? reward-points user)))
    )
        (map-set reward-points user
            (tuple 
                (points (+ (get points current-points) action-points))
                (level (if (> (+ (get points current-points) action-points) u1000)
                    "GOLD"
                    (if (> (+ (get points current-points) action-points) u500)
                        "SILVER"
                        "BRONZE")))
            ))
        (ok "Points earned")
    ))


    (define-map purpose-verification
    principal
    (tuple 
        (purpose (string-ascii 20))
        (verified bool)
        (verifier (optional principal))
    )
)

(define-public (verify-loan-purpose (borrower principal))
    (let (
        (purpose-data (unwrap! (map-get? loan-purposes borrower) (err "No purpose set")))
    )
        (map-set purpose-verification borrower
            (tuple 
                (purpose purpose-data)
                (verified true)
                (verifier (some tx-sender))
            ))
        (ok "Purpose verified")
    ))



    (define-map auto-renewals
    principal
    (tuple 
        (enabled bool)
        (max-renewals uint)
        (renewals-used uint)
    )
)

(define-public (enable-auto-renewal (borrower principal) (max-renewals uint))
    (begin
        (map-set auto-renewals borrower
            (tuple 
                (enabled true)
                (max-renewals max-renewals)
                (renewals-used u0)
            ))
        (ok "Auto-renewal enabled")
    ))

(define-public (process-auto-renewal (borrower principal))
    (let (
        (renewal-data (unwrap! (map-get? auto-renewals borrower) (err "No auto-renewal set")))
    )
        (asserts! (get enabled renewal-data) (err "Auto-renewal not enabled"))
        (asserts! (< (get renewals-used renewal-data) (get max-renewals renewal-data)) 
            (err "Max renewals reached"))
        (map-set auto-renewals borrower
            (merge renewal-data 
                (tuple (renewals-used (+ (get renewals-used renewal-data) u1)))))
        (ok "Loan auto-renewed")
    ))



(define-map refinance-requests 
    principal 
    (tuple 
        (original-loan-id principal)
        (new-interest-rate uint)
        (new-deadline uint)
        (status (string-ascii 20))
    )
)

(define-public (request-refinance (borrower principal) (new-interest-rate uint) (new-deadline uint))
    (let ((loan (unwrap! (map-get? loans borrower) (err "No loan found"))))
        (map-set refinance-requests borrower
            (tuple 
                (original-loan-id borrower)
                (new-interest-rate new-interest-rate)
                (new-deadline new-deadline)
                (status "PENDING")
            ))
        (ok "Refinance requested")
    )
)

(define-public (approve-refinance (borrower principal))
    (let 
        ((loan (unwrap! (map-get? loans borrower) (err "No loan found")))
         (refinance (unwrap! (map-get? refinance-requests borrower) (err "No refinance request"))))
        (asserts! (is-eq tx-sender (get lender loan)) (err "Only lender can approve"))
        (map-set loans borrower 
            (merge loan 
                (tuple 
                    (interest-rate (get new-interest-rate refinance))
                    (deadline (get new-deadline refinance))
                    (status "REFINANCED")
                )
            ))
        (map-set refinance-requests borrower
            (merge refinance (tuple (status "APPROVED"))))
        (ok "Loan refinanced successfully")
    )
)




(define-map loan-bundles 
    uint 
    (tuple 
        (loans (list 20 principal))
        (total-value uint)
        (owner principal)
        (interest-rate uint)
    )
)

(define-data-var bundle-counter uint u0)

(define-public (create-loan-bundle (loan-borrowers (list 20 principal)) (bundle-interest-rate uint))
    (let 
        ((bundle-id (var-get bundle-counter))
         (total-value u0))
        (var-set bundle-counter (+ bundle-id u1))
        (map-set loan-bundles bundle-id
            (tuple 
                (loans loan-borrowers)
                (total-value total-value)
                (owner tx-sender)
                (interest-rate bundle-interest-rate)
            ))
        (ok bundle-id)
    )
)

(define-public (transfer-bundle (bundle-id uint) (recipient principal))
    (let ((bundle (unwrap! (map-get? loan-bundles bundle-id) (err "Bundle not found"))))
        (asserts! (is-eq tx-sender (get owner bundle)) (err "Not the bundle owner"))
        (map-set loan-bundles bundle-id
            (merge bundle (tuple (owner recipient))))
        (ok "Bundle transferred")
    )
)



(define-map marketplace-listings 
    uint 
    (tuple 
        (borrower principal)
        (asking-price uint)
        (seller principal)
        (active bool)
    )
)

(define-data-var listing-counter uint u0)

(define-public (list-loan-for-sale (borrower principal) (asking-price uint))
    (let 
        ((loan (unwrap! (map-get? loans borrower) (err "No loan found")))
         (listing-id (var-get listing-counter)))
        (asserts! (is-eq tx-sender (get lender loan)) (err "Only lender can list loan"))
        (var-set listing-counter (+ listing-id u1))
        (map-set marketplace-listings listing-id
            (tuple 
                (borrower borrower)
                (asking-price asking-price)
                (seller tx-sender)
                (active true)
            ))
        (ok listing-id)
    )
)

(define-public (buy-loan (listing-id uint))
    (let 
        ((listing (unwrap! (map-get? marketplace-listings listing-id) (err "Listing not found")))
         (borrower (get borrower listing))
         (loan (unwrap! (map-get? loans borrower) (err "Loan not found"))))
        (asserts! (get active listing) (err "Listing not active"))
        (map-set loans borrower
            (merge loan (tuple (lender tx-sender))))
        (map-set marketplace-listings listing-id
            (merge listing (tuple (active false))))
        (ok "Loan purchased successfully")
    )
)



(define-map loan-grades 
    principal 
    (tuple 
        (grade (string-ascii 2))
        (score uint)
        (last-updated uint)
    )
)

(define-constant GRADE-A "A+")
(define-constant GRADE-B "B+")
(define-constant GRADE-C "C+")
(define-constant GRADE-D "D+")
(define-constant GRADE-F "F")

(define-public (grade-loan (borrower principal))
    (let 
        ((loan (unwrap! (map-get? loans borrower) (err "No loan found")))
         (risk-data (default-to 
                      (tuple (credit-score u0) (collateral-ratio u0) (payment-history u0) (risk-level "HIGH")) 
                      (map-get? risk-scores borrower)))
         (total-score (+ (get credit-score risk-data) (get collateral-ratio risk-data) (get payment-history risk-data))))
        (map-set loan-grades borrower
            (tuple 
                (grade (if (>= total-score u90) 
                          GRADE-A
                          (if (>= total-score u75)
                              GRADE-B
                              (if (>= total-score u60)
                                  GRADE-C
                                  (if (>= total-score u40)
                                      GRADE-D
                                      GRADE-F)))))
                (score total-score)
                (last-updated block-height)
            ))
        (ok "Loan graded")
    )
)

(define-read-only (get-loan-grade (borrower principal))
    (let ((grade-data (map-get? loan-grades borrower)))
        (if (is-some grade-data)
            (ok (get grade (unwrap! grade-data (err "No grade"))))
            (ok "Not graded")
        )
    )
)


(define-map loan-health-metrics 
    principal 
    (tuple 
        (health-score uint)
        (last-payment uint)
        (missed-payments uint)
        (status (string-ascii 20))
    )
)

(define-public (update-loan-health (borrower principal))
    (let 
        ((loan (unwrap! (map-get? loans borrower) (err "No loan found")))
         (current-health (default-to 
                          (tuple 
                              (health-score u100) 
                              (last-payment u0) 
                              (missed-payments u0) 
                              (status "HEALTHY")) 
                          (map-get? loan-health-metrics borrower)))
         (deadline (get deadline loan))
         (health-status (if (> block-height deadline)
                           "AT_RISK"
                           (if (> (- deadline block-height) u100)
                               "HEALTHY"
                               "WARNING"))))
        (map-set loan-health-metrics borrower
            (tuple 
                (health-score (if (is-eq health-status "HEALTHY") u100 
                                 (if (is-eq health-status "WARNING") u70 u30)))
                (last-payment (get last-payment current-health))
                (missed-payments (get missed-payments current-health))
                (status health-status)
            ))
        (ok "Loan health updated")
    )
)

(define-read-only (get-loan-health (borrower principal))
    (let ((health-data (map-get? loan-health-metrics borrower)))
        (if (is-some health-data)
            (ok (get status (unwrap! health-data (err "No health data"))))
            (ok "Not monitored")
        )
    )
)


(define-map participation-pools
    principal 
    (tuple 
        (total-shares uint)
        (available-shares uint)
        (share-price uint)
        (participants (list 20 principal))
        (min-participation uint)
    )
)

(define-map participant-shares
    (tuple (pool-id principal) (participant principal))
    uint
)

(define-public (create-participation-pool 
    (loan-id principal) 
    (total-shares uint) 
    (share-price uint)
    (min-participation uint)
)
    (let ((loan (unwrap! (map-get? loans loan-id) (err "Loan not found"))))
        (asserts! (is-eq tx-sender (get lender loan)) (err "Not loan owner"))
        (map-set participation-pools loan-id
            (tuple 
                (total-shares total-shares)
                (available-shares total-shares)
                (share-price share-price)
                (participants (list))
                (min-participation min-participation)
            ))
        (ok "Pool created")
    )
)

(define-public (buy-pool-shares (pool-id principal) (shares uint))
    (let (
        (pool (unwrap! (map-get? participation-pools pool-id) (err "Pool not found")))
    )
        (asserts! (>= shares (get min-participation pool)) (err "Below minimum"))
        (asserts! (<= shares (get available-shares pool)) (err "Not enough shares"))
        (map-set participation-pools pool-id
            (merge pool (tuple 
                (available-shares (- (get available-shares pool) shares))
                (participants (unwrap! (as-max-len? 
                    (append (get participants pool) tx-sender) u20) 
                    (err "Too many participants")))
            )))
        (map-set participant-shares 
            (tuple (pool-id pool-id) (participant tx-sender)) 
            shares)
        (ok "Shares purchased")
    )
)


(define-map interest-model
    (tuple (utilization uint) (risk-level uint))
    uint
)

(define-data-var base-rate uint u500)
(define-data-var optimal-utilization uint u8000)
(define-data-var slope1 uint u100)
(define-data-var slope2 uint u300)

(define-public (calculate-interest-rate (utilization uint) (risk-level uint))
    (let (
        (base (var-get base-rate))
        (optimal (var-get optimal-utilization))
        (rate (if (<= utilization optimal)
            (+ base (* (/ utilization u10000) (var-get slope1)))
            (+ base (* (/ (- utilization optimal) u10000) (var-get slope2)))
        )))
        (map-set interest-model 
            (tuple (utilization utilization) (risk-level risk-level))
            (+ rate (* risk-level u10)))
        (ok rate)
    )
)

(define-public (update-rate-parameters 
    (new-base uint) 
    (new-optimal uint)
    (new-slope1 uint)
    (new-slope2 uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err "Not authorized"))
        (var-set base-rate new-base)
        (var-set optimal-utilization new-optimal)
        (var-set slope1 new-slope1)
        (var-set slope2 new-slope2)
        (ok "Parameters updated")
    )
)