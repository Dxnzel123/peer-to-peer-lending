;; Dynamic Loan Refinancing System
;; Allows borrowers to refinance existing loans with improved terms

;; Local reference maps - these mirror the lending contract data for refinancing operations
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

(define-map borrower-ratings principal 
    (tuple 
        (total-rating uint) 
        (number-of-ratings uint)
    )
)

(define-map borrower-impact-scores
    principal
    (tuple
        (total-score uint)
        (environmental-score uint)
        (social-score uint)
        (education-score uint)
        (healthcare-score uint)
        (verified bool)
        (last-updated uint)
    )
)

(define-map refinancing-applications
    principal  ;; original borrower
    (tuple
        (original-loan-id principal)
        (current-amount uint)
        (current-rate uint)
        (current-deadline uint)
        (requested-new-rate uint)
        (requested-new-deadline uint)
        (justification (string-ascii 100))
        (status (string-ascii 20))
        (application-date uint)
        (creditworthiness-score uint)
        (payment-history-score uint)
        (market-rate-improvement uint)
    )
)

(define-map refinancing-offers
    (tuple (borrower principal) (offer-id uint))
    (tuple
        (lender principal)
        (new-interest-rate uint)
        (new-deadline uint)
        (refinancing-fee uint)
        (offer-expires uint)
        (terms-improvement-score uint)
        (accepted bool)
        (offer-date uint)
    )
)

(define-map borrower-refinancing-history
    principal
    (tuple
        (total-refinanced uint)
        (successful-refinances uint)
        (average-improvement uint)
        (last-refinance-date uint)
        (refinancing-eligibility-score uint)
    )
)

(define-map market-rate-tracker
    uint  ;; time period (blocks)
    (tuple
        (average-rate uint)
        (volatility uint)
        (trend (string-ascii 10))
        (sample-size uint)
    )
)

(define-data-var refinancing-admin principal tx-sender)
(define-data-var base-refinancing-fee uint u50)
(define-data-var min-refinancing-improvement uint u100)
(define-data-var max-refinancing-applications uint u5)
(define-data-var refinancing-cooldown uint u1440)

(define-constant REFINANCING-PENDING "PENDING")
(define-constant REFINANCING-APPROVED "APPROVED")
(define-constant REFINANCING-REJECTED "REJECTED")
(define-constant REFINANCING-COMPLETED "COMPLETED")
(define-constant REFINANCING-CANCELLED "CANCELLED")

(define-public (apply-for-refinancing
    (original-loan-id principal)
    (requested-new-rate uint)
    (requested-new-deadline uint)
    (justification (string-ascii 100))
)
    (let (
        (original-loan (unwrap! (map-get? loans original-loan-id) (err "Original loan not found")))
        (current-rate (get interest-rate original-loan))
        (current-amount (get amount original-loan))
        (current-deadline (get deadline original-loan))
        (existing-app (map-get? refinancing-applications tx-sender))
        (history (default-to (tuple (total-refinanced u0) (successful-refinances u0) (average-improvement u0) (last-refinance-date u0) (refinancing-eligibility-score u50)) (map-get? borrower-refinancing-history tx-sender)))
    )
        (asserts! (is-eq tx-sender (get borrower original-loan)) (err "Not loan borrower"))
        (asserts! (is-none existing-app) (err "Application already pending"))
        (asserts! (>= (- current-rate requested-new-rate) (var-get min-refinancing-improvement)) (err "Insufficient rate improvement"))
        (asserts! (>= (get refinancing-eligibility-score history) u30) (err "Low eligibility score"))
        (let (
            (creditworthiness (unwrap! (calculate-creditworthiness-score tx-sender) (err "Credit calculation failed")))
            (payment-history (unwrap! (calculate-payment-history-score tx-sender) (err "Payment history calculation failed")))
            (market-improvement (unwrap! (calculate-market-rate-improvement current-rate) (err "Market analysis failed")))
        )
            (map-set refinancing-applications tx-sender
                (tuple
                    (original-loan-id original-loan-id)
                    (current-amount current-amount)
                    (current-rate current-rate)
                    (current-deadline current-deadline)
                    (requested-new-rate requested-new-rate)
                    (requested-new-deadline requested-new-deadline)
                    (justification justification)
                    (status REFINANCING-PENDING)
                    (application-date block-height)
                    (creditworthiness-score creditworthiness)
                    (payment-history-score payment-history)
                    (market-rate-improvement market-improvement)
                ))
            (ok "Refinancing application submitted")
        )
    )
)

(define-public (make-refinancing-offer
    (borrower principal)
    (offer-id uint)
    (new-interest-rate uint)
    (new-deadline uint)
    (refinancing-fee uint)
    (offer-duration uint)
)
    (let (
        (application (unwrap! (map-get? refinancing-applications borrower) (err "No application found")))
        (existing-offer (map-get? refinancing-offers (tuple (borrower borrower) (offer-id offer-id))))
    )
        (asserts! (is-eq (get status application) REFINANCING-PENDING) (err "Application not pending"))
        (asserts! (is-none existing-offer) (err "Offer ID already exists"))
        (asserts! (<= new-interest-rate (get current-rate application)) (err "Rate not improved"))
        (let (
            (improvement-score (calculate-terms-improvement 
                (get current-rate application) 
                new-interest-rate 
                (get current-deadline application) 
                new-deadline))
        )
            (map-set refinancing-offers (tuple (borrower borrower) (offer-id offer-id))
                (tuple
                    (lender tx-sender)
                    (new-interest-rate new-interest-rate)
                    (new-deadline new-deadline)
                    (refinancing-fee refinancing-fee)
                    (offer-expires (+ block-height offer-duration))
                    (terms-improvement-score improvement-score)
                    (accepted false)
                    (offer-date block-height)
                ))
            (ok "Refinancing offer submitted")
        )
    )
)

(define-public (accept-refinancing-offer (offer-id uint))
    (let (
        (offer-key (tuple (borrower tx-sender) (offer-id offer-id)))
        (offer (unwrap! (map-get? refinancing-offers offer-key) (err "Offer not found")))
        (application (unwrap! (map-get? refinancing-applications tx-sender) (err "No application found")))
        (original-loan-id (get original-loan-id application))
    )
        (begin
            (asserts! (not (get accepted offer)) (err "Offer already accepted"))
            (asserts! (< block-height (get offer-expires offer)) (err "Offer expired"))
            (asserts! (is-eq (get status application) REFINANCING-PENDING) (err "Application not pending"))
            (try! (execute-refinancing original-loan-id offer application))
            (map-set refinancing-offers offer-key (merge offer (tuple (accepted true))))
            (map-set refinancing-applications tx-sender 
                (merge application (tuple (status REFINANCING-COMPLETED))))
            (unwrap! (update-refinancing-history tx-sender offer application) (err "Failed to update history"))
            (ok "Refinancing offer accepted and executed")
        )
    )
)

(define-private (execute-refinancing 
    (original-loan-id principal) 
    (offer (tuple (lender principal) (new-interest-rate uint) (new-deadline uint) (refinancing-fee uint) (offer-expires uint) (terms-improvement-score uint) (accepted bool) (offer-date uint)))
    (application (tuple (original-loan-id principal) (current-amount uint) (current-rate uint) (current-deadline uint) (requested-new-rate uint) (requested-new-deadline uint) (justification (string-ascii 100)) (status (string-ascii 20)) (application-date uint) (creditworthiness-score uint) (payment-history-score uint) (market-rate-improvement uint)))
)
    (let (
        (new-rate (get new-interest-rate offer))
        (new-deadline (get new-deadline offer))
        (current-amount (get current-amount application))
        (refinancing-fee (get refinancing-fee offer))
        (total-new-amount (+ current-amount refinancing-fee))
    )
        (begin
            ;; Close the original loan by updating status
            (map-set loans original-loan-id 
                (merge (unwrap! (map-get? loans original-loan-id) (err "Loan not found")) 
                       (tuple (status "REFINANCED"))))
            ;; Create new loan with updated terms
            (map-set loans tx-sender 
                (tuple 
                    (amount total-new-amount) 
                    (interest-rate new-rate) 
                    (deadline new-deadline) 
                    (lender (get lender offer)) 
                    (borrower tx-sender)
                    (status "ACTIVE")))
            (ok true)
        )
    )
)

(define-private (update-refinancing-history 
    (borrower principal) 
    (offer (tuple (lender principal) (new-interest-rate uint) (new-deadline uint) (refinancing-fee uint) (offer-expires uint) (terms-improvement-score uint) (accepted bool) (offer-date uint)))
    (application (tuple (original-loan-id principal) (current-amount uint) (current-rate uint) (current-deadline uint) (requested-new-rate uint) (requested-new-deadline uint) (justification (string-ascii 100)) (status (string-ascii 20)) (application-date uint) (creditworthiness-score uint) (payment-history-score uint) (market-rate-improvement uint)))
)
    (let (
        (current-history (default-to (tuple (total-refinanced u0) (successful-refinances u0) (average-improvement u0) (last-refinance-date u0) (refinancing-eligibility-score u50)) (map-get? borrower-refinancing-history borrower)))
        (rate-improvement (- (get current-rate application) (get new-interest-rate offer)))
        (new-total (+ (get total-refinanced current-history) u1))
        (new-successful (+ (get successful-refinances current-history) u1))
        (new-avg-improvement (/ (+ (* (get average-improvement current-history) (get total-refinanced current-history)) rate-improvement) new-total))
        (new-eligibility (if (> (+ (get refinancing-eligibility-score current-history) u5) u100) u100 (+ (get refinancing-eligibility-score current-history) u5)))
    )
        (map-set borrower-refinancing-history borrower
            (tuple
                (total-refinanced new-total)
                (successful-refinances new-successful)
                (average-improvement new-avg-improvement)
                (last-refinance-date block-height)
                (refinancing-eligibility-score new-eligibility)
            ))
        (ok true)
    )
)

(define-private (calculate-creditworthiness-score (borrower principal))
    (let (
        (rating-data (default-to (tuple (total-rating u0) (number-of-ratings u0)) (map-get? borrower-ratings borrower)))
        (avg-rating (if (> (get number-of-ratings rating-data) u0)
            (/ (get total-rating rating-data) (get number-of-ratings rating-data))
            u50))
        (impact-data (default-to (tuple (total-score u0) (environmental-score u0) (social-score u0) (education-score u0) (healthcare-score u0) (verified false) (last-updated u0)) (map-get? borrower-impact-scores borrower)))
        (impact-score (get total-score impact-data))
        (composite-score (/ (+ (* avg-rating u7) (* impact-score u3)) u10))
    )
        (ok (if (> composite-score u100) u100 composite-score))
    )
)

(define-private (calculate-payment-history-score (borrower principal))
    (let (
        (refinancing-history (default-to (tuple (total-refinanced u0) (successful-refinances u0) (average-improvement u0) (last-refinance-date u0) (refinancing-eligibility-score u50)) (map-get? borrower-refinancing-history borrower)))
        (base-score u70)
        (success-rate (if (> (get total-refinanced refinancing-history) u0)
            (/ (* (get successful-refinances refinancing-history) u100) (get total-refinanced refinancing-history))
            u100))
        (adjusted-score (/ (+ (* base-score u7) (* success-rate u3)) u10))
    )
        (ok (if (> adjusted-score u100) u100 adjusted-score))
    )
)

(define-private (calculate-market-rate-improvement (current-rate uint))
    (let (
        (recent-market-data (default-to (tuple (average-rate u1500) (volatility u200) (trend "STABLE") (sample-size u0)) (map-get? market-rate-tracker (- block-height u100))))
        (market-average (get average-rate recent-market-data))
        (improvement (if (> current-rate market-average) (- current-rate market-average) u0))
    )
        (ok improvement)
    )
)

(define-private (calculate-terms-improvement (old-rate uint) (new-rate uint) (old-deadline uint) (new-deadline uint))
    (let (
        (rate-improvement (if (> old-rate new-rate) (- old-rate new-rate) u0))
        (deadline-improvement (if (> new-deadline old-deadline) 
            (if (> (/ (- new-deadline old-deadline) u100) u50) u50 (/ (- new-deadline old-deadline) u100)) u0))
        (total-improvement (+ (* rate-improvement u8) (* deadline-improvement u2)))
    )
        (/ total-improvement u10)
    )
)

(define-public (reject-refinancing-application (borrower principal) (reason (string-ascii 50)))
    (let (
        (application (unwrap! (map-get? refinancing-applications borrower) (err "No application found")))
    )
        (asserts! (is-eq tx-sender (var-get refinancing-admin)) (err "Not authorized"))
        (asserts! (is-eq (get status application) REFINANCING-PENDING) (err "Application not pending"))
        (map-set refinancing-applications borrower
            (merge application (tuple (status REFINANCING-REJECTED))))
        (ok "Application rejected")
    )
)

(define-public (cancel-refinancing-application)
    (let (
        (application (unwrap! (map-get? refinancing-applications tx-sender) (err "No application found")))
    )
        (asserts! (is-eq (get status application) REFINANCING-PENDING) (err "Application not pending"))
        (map-delete refinancing-applications tx-sender)
        (ok "Application cancelled")
    )
)

(define-public (update-market-rates (time-period uint) (average-rate uint) (volatility uint) (trend (string-ascii 10)) (sample-size uint))
    (begin
        (asserts! (is-eq tx-sender (var-get refinancing-admin)) (err "Not authorized"))
        (map-set market-rate-tracker time-period
            (tuple
                (average-rate average-rate)
                (volatility volatility)
                (trend trend)
                (sample-size sample-size)
            ))
        (ok "Market rates updated")
    )
)

(define-read-only (get-refinancing-application (borrower principal))
    (map-get? refinancing-applications borrower)
)

(define-read-only (get-refinancing-offer (borrower principal) (offer-id uint))
    (map-get? refinancing-offers (tuple (borrower borrower) (offer-id offer-id)))
)

(define-read-only (get-borrower-refinancing-stats (borrower principal))
    (map-get? borrower-refinancing-history borrower)
)

(define-read-only (calculate-estimated-savings (borrower principal) (new-rate uint) (new-deadline uint))
    (let (
        (application (unwrap! (map-get? refinancing-applications borrower) (err "No application found")))
        (current-amount (get current-amount application))
        (current-rate (get current-rate application))
        (current-deadline (get current-deadline application))
        (current-interest (/ (* current-amount current-rate) u100))
        (new-interest (/ (* current-amount new-rate) u100))
        (interest-savings (if (> current-interest new-interest) (- current-interest new-interest) u0))
        (deadline-extension-cost (if (> new-deadline current-deadline) 
            (/ (* new-interest (- new-deadline current-deadline)) u365) u0))
        (net-savings (if (> interest-savings deadline-extension-cost) 
            (- interest-savings deadline-extension-cost) u0))
    )
        (ok (tuple (interest-savings interest-savings) (deadline-cost deadline-extension-cost) (net-savings net-savings)))
    )
)

(define-public (update-refinancing-parameters 
    (new-admin principal)
    (new-base-fee uint)
    (new-min-improvement uint)
    (new-max-applications uint)
    (new-cooldown uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get refinancing-admin)) (err "Not authorized"))
        (var-set refinancing-admin new-admin)
        (var-set base-refinancing-fee new-base-fee)
        (var-set min-refinancing-improvement new-min-improvement)
        (var-set max-refinancing-applications new-max-applications)
        (var-set refinancing-cooldown new-cooldown)
        (ok "Parameters updated")
    )
)

