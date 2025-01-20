

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
