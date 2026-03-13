;; OpenDeltaVote - DAO Governance Platform
;; Implements reputation-based voting with delta scoring and quadratic mechanics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-proposal-closed (err u104))
(define-constant err-insufficient-reputation (err u105))
(define-constant err-invalid-amount (err u106))

;; Minimum reputation required to create proposals
(define-constant min-reputation-proposal u100)

;; Proposal status constants
(define-constant status-active u1)
(define-constant status-passed u2)
(define-constant status-rejected u3)
(define-constant status-executed u4)

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var total-reputation uint u0)

;; Data Maps

;; Member reputation scores
(define-map member-reputation
    principal
    {
        reputation-score: uint,
        total-votes: uint,
        successful-votes: uint,
        last-updated: uint
    }
)

;; Proposals
(define-map proposals
    uint
    {
        proposer: principal,
        title: (string-ascii 256),
        description: (string-ascii 1024),
        voting-start: uint,
        voting-end: uint,
        yes-votes: uint,
        no-votes: uint,
        total-weight: uint,
        status: uint,
        executed-at: (optional uint)
    }
)

;; Voting records
(define-map votes
    { proposal-id: uint, voter: principal }
    {
        vote-weight: uint,
        vote-choice: bool,
        voted-at: uint
    }
)

;; Contribution tracking for reputation mining
(define-map contributions
    principal
    {
        proposals-created: uint,
        votes-cast: uint,
        engagement-score: uint,
        last-contribution: uint
    }
)

;; Read-only functions

(define-read-only (get-member-reputation (member principal))
    (default-to
        { reputation-score: u0, total-votes: u0, successful-votes: u0, last-updated: u0 }
        (map-get? member-reputation member)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-contributions (member principal))
    (default-to
        { proposals-created: u0, votes-cast: u0, engagement-score: u0, last-contribution: u0 }
        (map-get? contributions member)
    )
)

(define-read-only (calculate-voting-power (member principal))
    (let (
        (reputation (get reputation-score (get-member-reputation member)))
        (base-power u100)
    )
    ;; Quadratic voting: square root approximation using reputation
    ;; Simplified calculation: voting-power = base + (reputation / 10)
    (+ base-power (/ reputation u10))
    )
)

(define-read-only (get-proposal-nonce)
    (var-get proposal-nonce)
)

;; Private functions

(define-private (update-reputation-internal (member principal) (delta int) (is-positive bool))
    (let (
        (current-rep (get-member-reputation member))
        (current-score (get reputation-score current-rep))
        (new-score (if is-positive
            (+ current-score (to-uint delta))
            (if (> current-score (to-uint (- delta)))
                (- current-score (to-uint (- delta)))
                u0
            )
        ))
    )
    (map-set member-reputation member
        (merge current-rep {
            reputation-score: new-score,
            last-updated: block-height
        })
    )
    new-score
    )
)

;; Public functions

;; Initialize member reputation
(define-public (initialize-member)
    (let (
        (existing (map-get? member-reputation tx-sender))
    )
    (if (is-none existing)
        (begin
            (map-set member-reputation tx-sender {
                reputation-score: u100,
                total-votes: u0,
                successful-votes: u0,
                last-updated: block-height
            })
            (map-set contributions tx-sender {
                proposals-created: u0,
                votes-cast: u0,
                engagement-score: u0,
                last-contribution: block-height
            })
            (ok true)
        )
        (ok false)
    )
    )
)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 256)) (description (string-ascii 1024)) (voting-duration uint))
    (let (
        (proposer-rep (get reputation-score (get-member-reputation tx-sender)))
        (proposal-id (var-get proposal-nonce))
        (current-height block-height)
    )
    (asserts! (>= proposer-rep min-reputation-proposal) err-insufficient-reputation)
    (asserts! (> voting-duration u0) err-invalid-amount)
    
    ;; Create proposal
    (map-set proposals proposal-id {
        proposer: tx-sender,
        title: title,
        description: description,
        voting-start: current-height,
        voting-end: (+ current-height voting-duration),
        yes-votes: u0,
        no-votes: u0,
        total-weight: u0,
        status: status-active,
        executed-at: none
    })
    
    ;; Update proposer contributions
    (let (
        (contrib (get-contributions tx-sender))
    )
    (map-set contributions tx-sender
        (merge contrib {
            proposals-created: (+ (get proposals-created contrib) u1),
            last-contribution: current-height
        })
    )
    )
    
    ;; Increment nonce
    (var-set proposal-nonce (+ proposal-id u1))
    
    ;; Award reputation for creating proposal
    (update-reputation-internal tx-sender 10 true)
    
    (ok proposal-id)
    )
)

;; Cast vote on proposal
(define-public (cast-vote (proposal-id uint) (vote-choice bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (voter-power (calculate-voting-power tx-sender))
        (existing-vote (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
    )
    ;; Validations
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (is-eq (get status proposal) status-active) err-proposal-closed)
    (asserts! (<= block-height (get voting-end proposal)) err-proposal-closed)
    (asserts! (>= block-height (get voting-start proposal)) err-proposal-closed)
    
    ;; Record vote
    (map-set votes { proposal-id: proposal-id, voter: tx-sender } {
        vote-weight: voter-power,
        vote-choice: vote-choice,
        voted-at: block-height
    })
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id
        (merge proposal {
            yes-votes: (if vote-choice 
                (+ (get yes-votes proposal) voter-power)
                (get yes-votes proposal)
            ),
            no-votes: (if vote-choice
                (get no-votes proposal)
                (+ (get no-votes proposal) voter-power)
            ),
            total-weight: (+ (get total-weight proposal) voter-power)
        })
    )
    
    ;; Update voter contributions
    (let (
        (contrib (get-contributions tx-sender))
        (member-rep (get-member-reputation tx-sender))
    )
    (map-set contributions tx-sender
        (merge contrib {
            votes-cast: (+ (get votes-cast contrib) u1),
            engagement-score: (+ (get engagement-score contrib) u1),
            last-contribution: block-height
        })
    )
    (map-set member-reputation tx-sender
        (merge member-rep {
            total-votes: (+ (get total-votes member-rep) u1)
        })
    )
    )
    
    ;; Award reputation for voting
    (update-reputation-internal tx-sender 5 true)
    
    (ok true)
    )
)

;; Finalize proposal after voting period
(define-public (finalize-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
    )
    (asserts! (> block-height (get voting-end proposal)) err-proposal-closed)
    (asserts! (is-eq (get status proposal) status-active) err-proposal-closed)
    
    (let (
        (yes-votes (get yes-votes proposal))
        (no-votes (get no-votes proposal))
        (passed (> yes-votes no-votes))
        (new-status (if passed status-passed status-rejected))
    )
    (map-set proposals proposal-id
        (merge proposal {
            status: new-status
        })
    )
    
    ;; Award bonus reputation to proposer if passed
    (if passed
        (begin
            (update-reputation-internal (get proposer proposal) 50 true)
            (ok passed)
        )
        (ok passed)
    )
    ))
)

;; Update member reputation manually (governance function)
(define-public (adjust-reputation (member principal) (delta int) (is-positive bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (update-reputation-internal member delta is-positive))
    )
)

;; Slash reputation for malicious behavior
(define-public (slash-reputation (member principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (update-reputation-internal member (to-int amount) false))
    )
)
