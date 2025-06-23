(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_VOTING_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_INVALID_DURATION (err u107))
(define-constant ERR_NOT_CITIZEN (err u108))
(define-constant ERR_ALREADY_CITIZEN (err u109))
(define-constant ERR_INVALID_CATEGORY (err u110))

(define-data-var proposal-counter uint u0)
(define-data-var total-treasury uint u0)
(define-data-var min-stake uint u1000)

(define-map proposals
    uint
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        category: (string-ascii 50),
        amount: uint,
        proposer: principal,
        votes-for: uint,
        votes-against: uint,
        start-block: uint,
        end-block: uint,
        executed: bool,
        priority: uint,
    }
)

(define-map votes
    {
        proposal-id: uint,
        voter: principal,
    }
    {
        vote: bool,
        weight: uint,
    }
)

(define-map citizens
    principal
    {
        stake: uint,
        voting-power: uint,
        last-vote: uint,
        reputation: uint,
    }
)

(define-map categories
    (string-ascii 50)
    {
        budget: uint,
        spent: uint,
        priority: uint,
    }
)

(define-public (register-citizen)
    (let ((caller tx-sender))
        (asserts! (>= (stx-get-balance caller) (var-get min-stake))
            ERR_INSUFFICIENT_FUNDS
        )
        (asserts! (is-none (map-get? citizens caller)) ERR_ALREADY_CITIZEN)
        (try! (stx-transfer? (var-get min-stake) caller (as-contract tx-sender)))
        (map-set citizens caller {
            stake: (var-get min-stake),
            voting-power: u1,
            last-vote: u0,
            reputation: u100,
        })
        (ok true)
    )
)

(define-public (create-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (category (string-ascii 50))
        (amount uint)
        (duration uint)
    )
    (let (
            (caller tx-sender)
            (proposal-id (+ (var-get proposal-counter) u1))
        )
        (asserts! (is-some (map-get? citizens caller)) ERR_NOT_CITIZEN)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (var-get total-treasury)) ERR_INSUFFICIENT_FUNDS)
        (asserts! (and (>= duration u144) (<= duration u4320))
            ERR_INVALID_DURATION
        )
        (asserts! (is-valid-category category) ERR_INVALID_CATEGORY)
        (map-set proposals proposal-id {
            title: title,
            description: description,
            category: category,
            amount: amount,
            proposer: caller,
            votes-for: u0,
            votes-against: u0,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration),
            executed: false,
            priority: (get-category-priority category),
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote
        (proposal-id uint)
        (support bool)
    )
    (let (
            (caller tx-sender)
            (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
            (citizen (unwrap! (map-get? citizens caller) ERR_NOT_CITIZEN))
            (voting-power (get voting-power citizen))
        )
        (asserts!
            (is-none (map-get? votes {
                proposal-id: proposal-id,
                voter: caller,
            }))
            ERR_ALREADY_VOTED
        )
        (asserts! (<= stacks-block-height (get end-block proposal))
            ERR_VOTING_ENDED
        )
        (map-set votes {
            proposal-id: proposal-id,
            voter: caller,
        } {
            vote: support,
            weight: voting-power,
        })
        (map-set proposals proposal-id
            (merge proposal {
                votes-for: (if support
                    (+ (get votes-for proposal) voting-power)
                    (get votes-for proposal)
                ),
                votes-against: (if support
                    (get votes-against proposal)
                    (+ (get votes-against proposal) voting-power)
                ),
            })
        )
        (map-set citizens caller
            (merge citizen { last-vote: stacks-block-height })
        )
        (ok true)
    )
)

(define-private (update-treasury (amount uint))
    (begin
        (var-set total-treasury (- (var-get total-treasury) amount))
        (ok true)
    )
)

(define-private (update-category-budget
        (category (string-ascii 50))
        (amount uint)
    )
    (match (map-get? categories category)
        category-data (begin
            (map-set categories category
                (merge category-data { spent: (+ (get spent category-data) amount) })
            )
            (ok true)
        )
        (err ERR_INVALID_CATEGORY)
    )
)

(define-private (is-valid-category (category (string-ascii 50)))
    (is-some (map-get? categories category))
)

(define-private (get-category-priority (category (string-ascii 50)))
    (match (map-get? categories category)
        category-data (get priority category-data)
        u1
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-citizen (who principal))
    (map-get? citizens who)
)

(define-read-only (get-category (category (string-ascii 50)))
    (map-get? categories category)
)

(define-read-only (get-vote
        (proposal-id uint)
        (voter principal)
    )
    (map-get? votes {
        proposal-id: proposal-id,
        voter: voter,
    })
)

(define-read-only (get-treasury-balance)
    (var-get total-treasury)
)

(define-read-only (get-active-proposals)
    (filter is-active-proposal
        (map uint-to-proposal (generate-sequence u1 (var-get proposal-counter)))
    )
)

(define-private (is-active-proposal (proposal (optional {
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: (string-ascii 50),
    amount: uint,
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    priority: uint,
})))
    (match proposal
        p (and
            (<= stacks-block-height (get end-block p))
            (not (get executed p))
        )
        false
    )
)

(define-private (uint-to-proposal (id uint))
    (map-get? proposals id)
)

(define-private (generate-sequence
        (start uint)
        (end uint)
    )
    (list start)
)
