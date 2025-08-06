;; =============================================================================
;; SIMPLE BABYSITTER NETWORK
;; =============================================================================
;; A comprehensive childcare provider matching system with availability tracking,
;; rate management, background verification, and parent review system.
;;
;; Features:
;; - Provider and parent profile management
;; - Availability and rate tracking
;; - Background verification system
;; - Review and rating system
;; - Booking management
;; =============================================================================

;; Error constants
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-INPUT (err u400))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INSUFFICIENT-FUNDS (err u402))
(define-constant ERR-BOOKING-CONFLICT (err u410))
(define-constant ERR-INVALID-STATUS (err u411))
(define-constant ERR-VERIFICATION-REQUIRED (err u412))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Status constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-CONFIRMED u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-CANCELLED u3)

(define-constant VERIFICATION-PENDING u0)
(define-constant VERIFICATION-APPROVED u1)
(define-constant VERIFICATION-REJECTED u2)

;; Data structures
(define-map providers
  { provider: principal }
  {
    name: (string-ascii 64),
    bio: (string-utf8 256),
    hourly-rate: uint,
    experience-years: uint,
    specialties: (list 5 (string-ascii 32)),
    available: bool,
    verification-status: uint,
    verification-date: (optional uint),
    rating: uint, ;; Average rating * 100 (e.g., 450 = 4.5 stars)
    review-count: uint,
    created-at: uint
  }
)

(define-map parents
  { parent: principal }
  {
    name: (string-ascii 64),
    children-count: uint,
    children-ages: (list 10 uint),
    special-needs: (string-utf8 128),
    preferred-rate-max: uint,
    rating: uint, ;; Parent rating from providers
    review-count: uint,
    created-at: uint
  }
)

(define-map availability-slots
  { provider: principal, date: (string-ascii 10), start-hour: uint }
  {
    end-hour: uint,
    is-available: bool,
    rate-override: (optional uint)
  }
)

(define-map bookings
  { booking-id: uint }
  {
    provider: principal,
    parent: principal,
    date: (string-ascii 10),
    start-hour: uint,
    end-hour: uint,
    total-hours: uint,
    hourly-rate: uint,
    total-cost: uint,
    status: uint,
    created-at: uint,
    confirmed-at: (optional uint),
    completed-at: (optional uint)
  }
)

(define-map reviews
  { review-id: uint }
  {
    booking-id: uint,
    reviewer: principal, ;; Either provider or parent
    reviewee: principal, ;; The person being reviewed
    rating: uint, ;; 1-5 stars
    comment: (string-utf8 512),
    is-provider-review: bool, ;; true if provider reviewing parent, false if parent reviewing provider
    created-at: uint
  }
)

(define-map background-checks
  { provider: principal }
  {
    check-type: (string-ascii 32), ;; "criminal", "reference", "certification"
    status: uint,
    verified-by: (optional principal),
    verification-date: (optional uint),
    expiry-date: (optional uint),
    notes: (string-utf8 256)
  }
)

;; Counter variables
(define-data-var next-booking-id uint u1)
(define-data-var next-review-id uint u1)

;; Platform fee (basis points: 250 = 2.5%)
(define-data-var platform-fee-bp uint u250)

;; =============================================================================
;; PROVIDER FUNCTIONS
;; =============================================================================

(define-public (register-provider
  (name (string-ascii 64))
  (bio (string-utf8 256))
  (hourly-rate uint)
  (experience-years uint)
  (specialties (list 5 (string-ascii 32))))
  (begin
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> hourly-rate u0) ERR-INVALID-INPUT)
    (asserts! (is-none (map-get? providers { provider: tx-sender })) ERR-ALREADY-EXISTS)

    (map-set providers
      { provider: tx-sender }
      {
        name: name,
        bio: bio,
        hourly-rate: hourly-rate,
        experience-years: experience-years,
        specialties: specialties,
        available: true,
        verification-status: VERIFICATION-PENDING,
        verification-date: none,
        rating: u0,
        review-count: u0,
        created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (update-provider-profile
  (name (string-ascii 64))
  (bio (string-utf8 256))
  (hourly-rate uint)
  (specialties (list 5 (string-ascii 32))))
  (let ((provider-data (unwrap! (map-get? providers { provider: tx-sender }) ERR-NOT-FOUND)))
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> hourly-rate u0) ERR-INVALID-INPUT)

    (map-set providers
      { provider: tx-sender }
      (merge provider-data {
        name: name,
        bio: bio,
        hourly-rate: hourly-rate,
        specialties: specialties
      })
    )
    (ok true)
  )
)

(define-public (set-availability
  (date (string-ascii 10))
  (start-hour uint)
  (end-hour uint)
  (rate-override (optional uint)))
  (begin
    (asserts! (is-some (map-get? providers { provider: tx-sender })) ERR-NOT-FOUND)
    (asserts! (and (>= start-hour u0) (< start-hour u24)) ERR-INVALID-INPUT)
    (asserts! (and (> end-hour start-hour) (<= end-hour u24)) ERR-INVALID-INPUT)

    (map-set availability-slots
      { provider: tx-sender, date: date, start-hour: start-hour }
      {
        end-hour: end-hour,
        is-available: true,
        rate-override: rate-override
      }
    )
    (ok true)
  )
)

(define-public (toggle-provider-availability (available bool))
  (let ((provider-data (unwrap! (map-get? providers { provider: tx-sender }) ERR-NOT-FOUND)))
    (map-set providers
      { provider: tx-sender }
      (merge provider-data { available: available })
    )
    (ok true)
  )
)

;; =============================================================================
;; PARENT FUNCTIONS
;; =============================================================================

(define-public (register-parent
  (name (string-ascii 64))
  (children-count uint)
  (children-ages (list 10 uint))
  (special-needs (string-utf8 128))
  (preferred-rate-max uint))
  (begin
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> children-count u0) ERR-INVALID-INPUT)
    (asserts! (is-none (map-get? parents { parent: tx-sender })) ERR-ALREADY-EXISTS)

    (map-set parents
      { parent: tx-sender }
      {
        name: name,
        children-count: children-count,
        children-ages: children-ages,
        special-needs: special-needs,
        preferred-rate-max: preferred-rate-max,
        rating: u0,
        review-count: u0,
        created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (update-parent-profile
  (name (string-ascii 64))
  (children-count uint)
  (children-ages (list 10 uint))
  (special-needs (string-utf8 128))
  (preferred-rate-max uint))
  (let ((parent-data (unwrap! (map-get? parents { parent: tx-sender }) ERR-NOT-FOUND)))
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> children-count u0) ERR-INVALID-INPUT)

    (map-set parents
      { parent: tx-sender }
      (merge parent-data {
        name: name,
        children-count: children-count,
        children-ages: children-ages,
        special-needs: special-needs,
        preferred-rate-max: preferred-rate-max
      })
    )
    (ok true)
  )
)

;; =============================================================================
;; BOOKING FUNCTIONS
;; =============================================================================

(define-public (create-booking
  (provider principal)
  (date (string-ascii 10))
  (start-hour uint)
  (end-hour uint))
  (let (
    (booking-id (var-get next-booking-id))
    (provider-data (unwrap! (map-get? providers { provider: provider }) ERR-NOT-FOUND))
    (parent-data (unwrap! (map-get? parents { parent: tx-sender }) ERR-NOT-FOUND))
    (total-hours (- end-hour start-hour))
    (slot-key { provider: provider, date: date, start-hour: start-hour })
    (availability (map-get? availability-slots slot-key))
  )
    ;; Validation
    (asserts! (> total-hours u0) ERR-INVALID-INPUT)
    (asserts! (get available provider-data) ERR-NOT-FOUND)
    (asserts! (is-eq (get verification-status provider-data) VERIFICATION-APPROVED) ERR-VERIFICATION-REQUIRED)

    ;; Check availability if slot exists
    (match availability
      slot-data (asserts! (and
                    (get is-available slot-data)
                    (>= (get end-hour slot-data) end-hour)) ERR-BOOKING-CONFLICT)
      true ;; No specific slot means general availability
    )

    (let (
      (hourly-rate (match availability
                     slot-data (default-to (get hourly-rate provider-data) (get rate-override slot-data))
                     (get hourly-rate provider-data)))
      (total-cost (* total-hours hourly-rate))
    )
      ;; Create booking
      (map-set bookings
        { booking-id: booking-id }
        {
          provider: provider,
          parent: tx-sender,
          date: date,
          start-hour: start-hour,
          end-hour: end-hour,
          total-hours: total-hours,
          hourly-rate: hourly-rate,
          total-cost: total-cost,
          status: STATUS-PENDING,
          created-at: stacks-block-height,
          confirmed-at: none,
          completed-at: none
        }
      )

      ;; Update availability slot if it exists
      (match availability
        slot-data (map-set availability-slots slot-key
                    (merge slot-data { is-available: false }))
        true
      )

      (var-set next-booking-id (+ booking-id u1))
      (ok booking-id)
    )
  )
)

(define-public (confirm-booking (booking-id uint))
  (let ((booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq (get provider booking-data) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status booking-data) STATUS-PENDING) ERR-INVALID-STATUS)

    (map-set bookings
      { booking-id: booking-id }
      (merge booking-data {
        status: STATUS-CONFIRMED,
        confirmed-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (complete-booking (booking-id uint))
  (let ((booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND)))
    (asserts! (or (is-eq (get provider booking-data) tx-sender)
                  (is-eq (get parent booking-data) tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status booking-data) STATUS-CONFIRMED) ERR-INVALID-STATUS)

    (map-set bookings
      { booking-id: booking-id }
      (merge booking-data {
        status: STATUS-COMPLETED,
        completed-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (cancel-booking (booking-id uint))
  (let ((booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND)))
    (asserts! (or (is-eq (get provider booking-data) tx-sender)
                  (is-eq (get parent booking-data) tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (not (is-eq (get status booking-data) STATUS-COMPLETED)) ERR-INVALID-STATUS)

    ;; Restore availability if slot exists
    (let ((slot-key {
            provider: (get provider booking-data),
            date: (get date booking-data),
            start-hour: (get start-hour booking-data)
          }))
      (match (map-get? availability-slots slot-key)
        slot-data (map-set availability-slots slot-key
                    (merge slot-data { is-available: true }))
        true
      )
    )

    (map-set bookings
      { booking-id: booking-id }
      (merge booking-data { status: STATUS-CANCELLED })
    )
    (ok true)
  )
)

;; =============================================================================
;; REVIEW FUNCTIONS
;; =============================================================================

(define-public (submit-review
  (booking-id uint)
  (rating uint)
  (comment (string-utf8 512)))
  (let (
    (review-id (var-get next-review-id))
    (booking-data (unwrap! (map-get? bookings { booking-id: booking-id }) ERR-NOT-FOUND))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-INPUT)
    (asserts! (is-eq (get status booking-data) STATUS-COMPLETED) ERR-INVALID-STATUS)
    (asserts! (or (is-eq (get provider booking-data) tx-sender)
                  (is-eq (get parent booking-data) tx-sender)) ERR-UNAUTHORIZED)

    (let (
      (is-provider-review (is-eq (get provider booking-data) tx-sender))
      (reviewee (if is-provider-review
                   (get parent booking-data)
                   (get provider booking-data)))
    )
      ;; Create review
      (map-set reviews
        { review-id: review-id }
        {
          booking-id: booking-id,
          reviewer: tx-sender,
          reviewee: reviewee,
          rating: rating,
          comment: comment,
          is-provider-review: is-provider-review,
          created-at: stacks-block-height
        }
      )

      ;; Update reviewee's rating
      (update-rating reviewee rating is-provider-review)

      (var-set next-review-id (+ review-id u1))
      (ok review-id)
    )
  )
)

(define-private (update-rating (reviewee principal) (new-rating uint) (is-provider-review bool))
  (if is-provider-review
    ;; Update parent rating
    (match (map-get? parents { parent: reviewee })
      parent-data
      (let (
        (current-total (* (get rating parent-data) (get review-count parent-data)))
        (new-count (+ (get review-count parent-data) u1))
        (new-avg (/ (+ current-total (* new-rating u100)) new-count))
      )
        (map-set parents { parent: reviewee }
          (merge parent-data {
            rating: new-avg,
            review-count: new-count
          })
        )
        true
      )
      false
    )
    ;; Update provider rating
    (match (map-get? providers { provider: reviewee })
      provider-data
      (let (
        (current-total (* (get rating provider-data) (get review-count provider-data)))
        (new-count (+ (get review-count provider-data) u1))
        (new-avg (/ (+ current-total (* new-rating u100)) new-count))
      )
        (map-set providers { provider: reviewee }
          (merge provider-data {
            rating: new-avg,
            review-count: new-count
          })
        )
        true
      )
      false
    )
  )
)

;; =============================================================================
;; BACKGROUND VERIFICATION FUNCTIONS
;; =============================================================================

(define-public (submit-background-check
  (provider principal)
  (check-type (string-ascii 32))
  (expiry-date (optional uint))
  (notes (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? providers { provider: provider })) ERR-NOT-FOUND)

    (map-set background-checks
      { provider: provider }
      {
        check-type: check-type,
        status: VERIFICATION-APPROVED,
        verified-by: (some tx-sender),
        verification-date: (some stacks-block-height),
        expiry-date: expiry-date,
        notes: notes
      }
    )

    ;; Update provider verification status
    (match (map-get? providers { provider: provider })
      provider-data
      (map-set providers { provider: provider }
        (merge provider-data {
          verification-status: VERIFICATION-APPROVED,
          verification-date: (some stacks-block-height)
        })
      )
      false
    )
    (ok true)
  )
)

(define-public (reject-background-check
  (provider principal)
  (reason (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? providers { provider: provider })) ERR-NOT-FOUND)

    (map-set background-checks
      { provider: provider }
      {
        check-type: "rejected",
        status: VERIFICATION-REJECTED,
        verified-by: (some tx-sender),
        verification-date: (some stacks-block-height),
        expiry-date: none,
        notes: reason
      }
    )

    ;; Update provider verification status
    (match (map-get? providers { provider: provider })
      provider-data
      (map-set providers { provider: provider }
        (merge provider-data {
          verification-status: VERIFICATION-REJECTED,
          verification-date: (some stacks-block-height)
        })
      )
      false
    )
    (ok true)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-provider (provider principal))
  (map-get? providers { provider: provider })
)

(define-read-only (get-parent (parent principal))
  (map-get? parents { parent: parent })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

(define-read-only (get-availability
  (provider principal)
  (date (string-ascii 10))
  (start-hour uint))
  (map-get? availability-slots { provider: provider, date: date, start-hour: start-hour })
)

(define-read-only (get-background-check (provider principal))
  (map-get? background-checks { provider: provider })
)

(define-read-only (is-provider-verified (provider principal))
  (match (map-get? providers { provider: provider })
    provider-data (is-eq (get verification-status provider-data) VERIFICATION-APPROVED)
    false
  )
)

(define-read-only (get-provider-rating (provider principal))
  (match (map-get? providers { provider: provider })
    provider-data (get rating provider-data)
    u0
  )
)

(define-read-only (get-parent-rating (parent principal))
  (match (map-get? parents { parent: parent })
    parent-data (get rating parent-data)
    u0
  )
)

(define-read-only (calculate-booking-cost
  (provider principal)
  (date (string-ascii 10))
  (start-hour uint)
  (end-hour uint))
  (let (
    (provider-data (unwrap! (map-get? providers { provider: provider }) ERR-NOT-FOUND))
    (total-hours (- end-hour start-hour))
    (slot-key { provider: provider, date: date, start-hour: start-hour })
  )
    (asserts! (> total-hours u0) ERR-INVALID-INPUT)

    (let (
      (hourly-rate (default-to (get hourly-rate provider-data)
                              (match (map-get? availability-slots slot-key)
                                slot-data (get rate-override slot-data)
                                none
                              )))
    )
      (ok (* total-hours hourly-rate))
    )
  )
)

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

(define-public (set-platform-fee (new-fee-bp uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee-bp u1000) ERR-INVALID-INPUT) ;; Max 10%
    (var-set platform-fee-bp new-fee-bp)
    (ok true)
  )
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-bp)
)
