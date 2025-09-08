;; contracts/subwallet.clar
(define-constant ERR_INSUFFICIENT u100)
(define-constant ERR_NO_CATEGORY u101)

(define-map balances
  { owner: principal, category: (string-ascii 32) }
  { amount: uint })

;; read-only: get balance for an owner+category
(define-read-only (get-balance (owner principal) (category (string-ascii 32)))
  (default-to u0 (get amount (map-get? balances { owner: owner, category: category }))))

;; deposit STX into a category (user must transfer STX to the contract principal)
(define-public (deposit (category (string-ascii 32)) (amount uint))
  (begin
    ;; transfer STX from tx-sender -> contract principal
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; update internal ledger
    (let ((prev (default-to u0 (get amount (map-get? balances { owner: tx-sender, category: category })))))
      (map-set balances { owner: tx-sender, category: category } { amount: (+ prev amount) }))
    (ok true)))

;; move internal balance between categories for same owner (virtual move)
(define-public (move (from (string-ascii 32)) (to (string-ascii 32)) (amt uint))
  (let ((row (map-get? balances { owner: tx-sender, category: from })))
    (match row
      entry
        (let ((bal (get amount entry)))
          (if (>= bal amt)
            (begin
              (map-set balances { owner: tx-sender, category: from } { amount: (- bal amt) })
              ;; credit target
              (let ((prev (default-to u0 (get amount (map-get? balances { owner: tx-sender, category: to })))))
                (map-set balances { owner: tx-sender, category: to } { amount: (+ prev amt) }))
              (ok true))
            (err ERR_INSUFFICIENT)))
      (err ERR_NO_CATEGORY))))

;; spend from a category: reduces internal balance, then contract sends STX out
(define-public (spend (category (string-ascii 32)) (recipient principal) (amt uint))
  (let ((row (map-get? balances { owner: tx-sender, category: category })))
    (match row
      entry
        (let ((bal (get amount entry)))
          (if (>= bal amt)
            (begin
              ;; reduce virtual balance
              (map-set balances { owner: tx-sender, category: category } { amount: (- bal amt) })
              ;; transfer from contract principal to recipient
              (try! (as-contract (stx-transfer? amt tx-sender recipient)))
              (ok true))
            (err ERR_INSUFFICIENT)))
      (err ERR_NO_CATEGORY))))
