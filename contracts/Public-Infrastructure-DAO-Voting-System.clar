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
(define-data-var contract-initialized bool false)

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

(define-private (is-valid-category (category (string-ascii 50)))
    (is-some (map-get? categories category))
)

(define-private (get-category-priority (category (string-ascii 50)))
    (match (map-get? categories category)
        category-data (get priority category-data)
        u1
    )
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
    (let ((current-treasury (var-get total-treasury)))
        (if (>= current-treasury amount)
            (begin
                (var-set total-treasury (- current-treasury amount))
                (ok true)
            )
            ERR_INSUFFICIENT_FUNDS
        )
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
        ERR_INVALID_CATEGORY
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

(define-public (initialize-contract)
    (begin
        (asserts! (not (var-get contract-initialized)) ERR_UNAUTHORIZED)
        (map-set categories "infrastructure" {
            budget: u100000,
            spent: u0,
            priority: u1,
        })
        (map-set categories "governance" {
            budget: u50000,
            spent: u0,
            priority: u2,
        })
        (map-set categories "community" {
            budget: u75000,
            spent: u0,
            priority: u3,
        })
        (map-set categories "development" {
            budget: u80000,
            spent: u0,
            priority: u4,
        })
        (var-set contract-initialized true)
        (ok true)
    )
)

(define-public (fund-treasury (amount uint))
    (let ((caller tx-sender))
        (try! (stx-transfer? amount caller (as-contract tx-sender)))
        (var-set total-treasury (+ (var-get total-treasury) amount))
        (ok true)
    )
)

(define-private (uint-to-proposal (id uint))
    (map-get? proposals id)
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

(define-private (generate-sequence
        (start uint)
        (end uint)
    )
    (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
)

(define-read-only (get-active-proposals)
    (filter is-active-proposal
        (map uint-to-proposal (generate-sequence u1 (var-get proposal-counter)))
    )
)
(define-constant ERR_EXECUTION_FAILED (err u200))
(define-constant ERR_PROPOSAL_REJECTED (err u201))
(define-constant ERR_ALREADY_EXECUTED (err u202))
(define-constant ERR_VOTING_NOT_ENDED (err u203))

(define-map proposal-recipients
    uint
    {
        recipient: principal,
        amount: uint,
    }
)

(define-public (set-proposal-recipient
        (proposal-id uint)
        (recipient principal)
        (amount uint)
    )
    (let (
            (caller tx-sender)
            (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
        )
        (asserts! (is-eq caller (get proposer proposal)) ERR_UNAUTHORIZED)
        (asserts! (is-eq amount (get amount proposal)) ERR_INVALID_AMOUNT)
        (map-set proposal-recipients proposal-id {
            recipient: recipient,
            amount: amount,
        })
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
            (recipient-data (unwrap! (map-get? proposal-recipients proposal-id)
                ERR_EXECUTION_FAILED
            ))
            (votes-for (get votes-for proposal))
            (votes-against (get votes-against proposal))
            (total-votes (+ votes-for votes-against))
        )
        (asserts! (> stacks-block-height (get end-block proposal))
            ERR_VOTING_NOT_ENDED
        )
        (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
        (asserts! (> total-votes u0) ERR_EXECUTION_FAILED)
        (asserts! (> votes-for votes-against) ERR_PROPOSAL_REJECTED)
        (asserts! (>= (* votes-for u100) (* total-votes u51))
            ERR_PROPOSAL_REJECTED
        )
        (try! (as-contract (stx-transfer? (get amount recipient-data) tx-sender
            (get recipient recipient-data)
        )))
        (try! (update-treasury (get amount proposal)))
        (try! (update-category-budget (get category proposal) (get amount proposal)))
        (map-set proposals proposal-id (merge proposal { executed: true }))
        (ok true)
    )
)

(define-public (batch-execute-proposals (proposal-ids (list 10 uint)))
    (let ((results (map execute-single-proposal proposal-ids)))
        (ok results)
    )
)

(define-private (execute-single-proposal (proposal-id uint))
    (match (execute-proposal proposal-id)
        success
        proposal-id
        error
        u0
    )
)

(define-read-only (get-proposal-recipient (proposal-id uint))
    (map-get? proposal-recipients proposal-id)
)

(define-read-only (is-proposal-executable (proposal-id uint))
    (match (get-proposal proposal-id)
        proposal (let (
                (votes-for (get votes-for proposal))
                (votes-against (get votes-against proposal))
                (total-votes (+ votes-for votes-against))
            )
            (and
                (> stacks-block-height (get end-block proposal))
                (not (get executed proposal))
                (> votes-for votes-against)
                (>= (* votes-for u100) (* total-votes u51))
                (is-some (map-get? proposal-recipients proposal-id))
            )
        )
        false
    )
)

(define-read-only (get-executable-proposals)
    (filter is-executable-proposal-wrapper
        (map uint-to-proposal-id (generate-sequence-for-execution u1 u100))
    )
)

(define-private (is-executable-proposal-wrapper (proposal-id uint))
    (is-proposal-executable proposal-id)
)

(define-private (uint-to-proposal-id (id uint))
    id
)

(define-private (generate-sequence-for-execution
        (start uint)
        (end uint)
    )
    (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
)
(define-constant ERR_CANNOT_DELEGATE_TO_SELF (err u300))
(define-constant ERR_DELEGATION_NOT_FOUND (err u301))
(define-constant ERR_CIRCULAR_DELEGATION (err u302))
(define-constant ERR_MAX_DELEGATION_DEPTH (err u303))

(define-data-var max-delegation-depth uint u3)

(define-map delegations
    principal
    {
        delegate: principal,
        delegated-power: uint,
        active: bool,
    }
)

(define-map delegation-received
    principal
    {
        total-delegated-power: uint,
        delegator-count: uint,
    }
)

(define-map voting-history
    principal
    {
        total-votes: uint,
        successful-votes: uint,
        last-activity: uint,
    }
)

(define-public (delegate-voting-power (delegate principal))
    (let (
            (caller tx-sender)
            (citizen (unwrap! (get-citizen caller) ERR_NOT_CITIZEN))
            (delegate-citizen (unwrap! (get-citizen delegate) ERR_NOT_CITIZEN))
            (voting-power (get voting-power citizen))
        )
        (asserts! (not (is-eq caller delegate)) ERR_CANNOT_DELEGATE_TO_SELF)
        (asserts! (not (has-circular-delegation caller delegate))
            ERR_CIRCULAR_DELEGATION
        )
        (match (map-get? delegations caller)
            existing-delegation (begin
                (unwrap-panic (remove-delegation-from-previous-delegate caller
                    (get delegate existing-delegation) voting-power
                ))
                true
            )
            true
        )
        (map-set delegations caller {
            delegate: delegate,
            delegated-power: voting-power,
            active: true,
        })
        (match (map-get? delegation-received delegate)
            existing-received (map-set delegation-received delegate {
                total-delegated-power: (+ (get total-delegated-power existing-received) voting-power),
                delegator-count: (+ (get delegator-count existing-received) u1),
            })
            (map-set delegation-received delegate {
                total-delegated-power: voting-power,
                delegator-count: u1,
            })
        )
        (ok true)
    )
)

(define-public (revoke-delegation)
    (let (
            (caller tx-sender)
            (delegation (unwrap! (map-get? delegations caller) ERR_DELEGATION_NOT_FOUND))
            (delegate (get delegate delegation))
            (delegated-power (get delegated-power delegation))
        )
        (unwrap-panic (remove-delegation-from-previous-delegate caller delegate delegated-power))
        (map-delete delegations caller)
        (ok true)
    )
)

(define-public (vote-with-delegation
        (proposal-id uint)
        (support bool)
    )
    (let (
            (caller tx-sender)
            (total-power (calculate-total-voting-power caller))
        )
        (try! (vote proposal-id support))
        (unwrap-panic (update-voting-history caller proposal-id support))
        (ok total-power)
    )
)

(define-public (update-voting-power-based-on-activity (citizen principal))
    (let (
            (history (default-to {
                total-votes: u0,
                successful-votes: u0,
                last-activity: u0,
            }
                (map-get? voting-history citizen)
            ))
            (base-power u1)
            (activity-bonus (/ (get total-votes history) u10))
            (success-bonus (/ (get successful-votes history) u5))
            (new-power (+ base-power activity-bonus success-bonus))
        )
        (ok new-power)
    )
)

(define-private (remove-delegation-from-previous-delegate
        (delegator principal)
        (delegate principal)
        (power uint)
    )
    (match (map-get? delegation-received delegate)
        existing-received (begin
            (map-set delegation-received delegate {
                total-delegated-power: (- (get total-delegated-power existing-received) power),
                delegator-count: (- (get delegator-count existing-received) u1),
            })
            (ok true)
        )
        (ok true)
    )
)

(define-private (has-circular-delegation
        (delegator principal)
        (potential-delegate principal)
    )
    (is-eq delegator potential-delegate)
)

(define-private (calculate-total-voting-power (citizen principal))
    (let (
            (base-citizen (unwrap-panic (get-citizen citizen)))
            (base-power (get voting-power base-citizen))
            (delegated-power (match (map-get? delegation-received citizen)
                received (get total-delegated-power received)
                u0
            ))
        )
        (+ base-power delegated-power)
    )
)

(define-private (update-voting-history
        (voter principal)
        (proposal-id uint)
        (support bool)
    )
    (let ((current-history (default-to {
            total-votes: u0,
            successful-votes: u0,
            last-activity: u0,
        }
            (map-get? voting-history voter)
        )))
        (map-set voting-history voter {
            total-votes: (+ (get total-votes current-history) u1),
            successful-votes: (get successful-votes current-history),
            last-activity: stacks-block-height,
        })
        (ok true)
    )
)

(define-read-only (get-delegation (delegator principal))
    (map-get? delegations delegator)
)

(define-read-only (get-delegation-received (delegate principal))
    (map-get? delegation-received delegate)
)

(define-read-only (get-total-voting-power (citizen principal))
    (calculate-total-voting-power citizen)
)

(define-read-only (get-voting-history (citizen principal))
    (map-get? voting-history citizen)
)

(define-read-only (is-active-delegate (delegate principal))
    (match (map-get? delegation-received delegate)
        received (> (get delegator-count received) u0)
        false
    )
)

(define-read-only (get-delegation-chain (citizen principal))
    (list citizen)
)

(define-constant ERR_SCHEDULE_TOO_EARLY (err u400))
(define-constant ERR_TIMELOCK_NOT_EXPIRED (err u401))
(define-constant ERR_SCHEDULE_NOT_FOUND (err u402))
(define-constant ERR_ALREADY_SCHEDULED (err u403))

(define-data-var timelock-delay uint u144)

(define-map proposal-schedules
    uint
    {
        scheduled-at: uint,
        execution-block: uint,
        scheduled-by: principal,
        cancelled: bool,
    }
)

(define-map execution-queue
    uint
    {
        proposal-id: uint,
        execution-block: uint,
        priority: uint,
    }
)

(define-data-var queue-counter uint u0)

(define-public (schedule-proposal-execution
        (proposal-id uint)
        (execution-delay uint)
    )
    (let (
            (caller tx-sender)
            (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
            (min-delay (var-get timelock-delay))
            (execution-block (+ stacks-block-height execution-delay))
        )
        (asserts! (is-proposal-executable proposal-id) ERR_EXECUTION_FAILED)
        (asserts! (>= execution-delay min-delay) ERR_SCHEDULE_TOO_EARLY)
        (asserts! (is-none (map-get? proposal-schedules proposal-id))
            ERR_ALREADY_SCHEDULED
        )
        (map-set proposal-schedules proposal-id {
            scheduled-at: stacks-block-height,
            execution-block: execution-block,
            scheduled-by: caller,
            cancelled: false,
        })
        (let ((queue-id (+ (var-get queue-counter) u1)))
            (map-set execution-queue queue-id {
                proposal-id: proposal-id,
                execution-block: execution-block,
                priority: (get priority proposal),
            })
            (var-set queue-counter queue-id)
            (ok queue-id)
        )
    )
)

(define-public (execute-scheduled-proposal (proposal-id uint))
    (let (
            (schedule (unwrap! (map-get? proposal-schedules proposal-id)
                ERR_SCHEDULE_NOT_FOUND
            ))
            (execution-block (get execution-block schedule))
        )
        (asserts! (>= stacks-block-height execution-block)
            ERR_TIMELOCK_NOT_EXPIRED
        )
        (asserts! (not (get cancelled schedule)) ERR_EXECUTION_FAILED)
        (try! (execute-proposal proposal-id))
        (map-delete proposal-schedules proposal-id)
        (ok true)
    )
)

(define-public (cancel-scheduled-proposal (proposal-id uint))
    (let (
            (caller tx-sender)
            (schedule (unwrap! (map-get? proposal-schedules proposal-id)
                ERR_SCHEDULE_NOT_FOUND
            ))
        )
        (asserts!
            (or
                (is-eq caller (get scheduled-by schedule))
                (is-eq caller CONTRACT_OWNER)
            )
            ERR_UNAUTHORIZED
        )
        (map-set proposal-schedules proposal-id
            (merge schedule { cancelled: true })
        )
        (ok true)
    )
)

(define-public (batch-execute-scheduled-proposals (proposal-ids (list 5 uint)))
    (let ((results (map execute-scheduled-single proposal-ids)))
        (ok results)
    )
)

(define-private (execute-scheduled-single (proposal-id uint))
    (match (execute-scheduled-proposal proposal-id)
        success
        proposal-id
        error
        u0
    )
)

(define-public (update-timelock-delay (new-delay uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set timelock-delay new-delay)
        (ok true)
    )
)

(define-read-only (get-proposal-schedule (proposal-id uint))
    (map-get? proposal-schedules proposal-id)
)

(define-read-only (get-queue-item (queue-id uint))
    (map-get? execution-queue queue-id)
)

(define-read-only (get-timelock-delay)
    (var-get timelock-delay)
)

(define-read-only (is-proposal-scheduled (proposal-id uint))
    (match (map-get? proposal-schedules proposal-id)
        schedule (not (get cancelled schedule))
        false
    )
)

(define-read-only (get-ready-for-execution)
    (filter is-ready-for-execution-wrapper
        (map get-queue-proposal-id (generate-execution-sequence u1 u20))
    )
)

(define-private (is-ready-for-execution-wrapper (proposal-id uint))
    (match (map-get? proposal-schedules proposal-id)
        schedule (and
            (>= stacks-block-height (get execution-block schedule))
            (not (get cancelled schedule))
        )
        false
    )
)

(define-private (get-queue-proposal-id (queue-id uint))
    (match (map-get? execution-queue queue-id)
        queue-item (get proposal-id queue-item)
        u0
    )
)

(define-private (generate-execution-sequence
        (start uint)
        (end uint)
    )
    (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
)
