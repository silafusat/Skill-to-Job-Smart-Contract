;; Skill-to-Job Smart Contract
;; Connects certified learning programs with conditional job offers

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u1001))
(define-constant err-not-found (err u1002))
(define-constant err-already-exists (err u1003))
(define-constant err-invalid-program (err u1004))
(define-constant err-not-certified (err u1005))
(define-constant err-expired (err u1006))
(define-constant err-already-claimed (err u1007))
(define-constant err-invalid-offer (err u1008))

(define-data-var next-program-id uint u1)
(define-data-var next-offer-id uint u1)
(define-data-var platform-fee uint u100)

(define-map learning-programs
  { program-id: uint }
  {
    name: (string-ascii 100),
    provider: principal,
    certification-authority: principal,
    duration-blocks: uint,
    skill-tags: (list 10 (string-ascii 50)),
    active: bool,
    created-at: uint
  }
)

(define-map student-certifications
  { student: principal, program-id: uint }
  {
    completed-at: uint,
    grade: uint,
    verified: bool,
    certification-hash: (buff 32)
  }
)

(define-map job-offers
  { offer-id: uint }
  {
    employer: principal,
    required-program-id: uint,
    position-title: (string-ascii 100),
    salary-stx: uint,
    duration-blocks: uint,
    max-positions: uint,
    filled-positions: uint,
    expires-at: uint,
    active: bool,
    created-at: uint
  }
)

(define-map job-applications
  { student: principal, offer-id: uint }
  {
    applied-at: uint,
    status: (string-ascii 20),
    employer-response: (optional (string-ascii 200))
  }
)

(define-map program-providers
  { provider: principal }
  {
    name: (string-ascii 100),
    verified: bool,
    programs-created: uint,
    reputation-score: uint
  }
)

(define-map certification-authorities
  { authority: principal }
  {
    name: (string-ascii 100),
    verified: bool,
    certifications-issued: uint
  }
)

(define-public (register-provider (name (string-ascii 100)))
  (let
    ((provider tx-sender))
    (asserts! (is-none (map-get? program-providers { provider: provider })) err-already-exists)
    (map-set program-providers
      { provider: provider }
      {
        name: name,
        verified: false,
        programs-created: u0,
        reputation-score: u100
      })
    (ok provider)))

(define-public (register-certification-authority (name (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (is-none (map-get? certification-authorities { authority: tx-sender })) err-already-exists)
    (map-set certification-authorities
      { authority: tx-sender }
      {
        name: name,
        verified: true,
        certifications-issued: u0
      })
    (ok tx-sender)))

(define-public (create-learning-program 
  (name (string-ascii 100))
  (certification-authority principal)
  (duration-blocks uint)
  (skill-tags (list 10 (string-ascii 50))))
  (let
    ((program-id (var-get next-program-id))
     (provider tx-sender))
    (asserts! (is-some (map-get? program-providers { provider: provider })) err-unauthorized)
    (asserts! (is-some (map-get? certification-authorities { authority: certification-authority })) err-invalid-program)
    (map-set learning-programs
      { program-id: program-id }
      {
        name: name,
        provider: provider,
        certification-authority: certification-authority,
        duration-blocks: duration-blocks,
        skill-tags: skill-tags,
        active: true,
        created-at: stacks-block-height
      })
    (map-set program-providers
      { provider: provider }
      (merge 
        (unwrap-panic (map-get? program-providers { provider: provider }))
        { programs-created: (+ (get programs-created (unwrap-panic (map-get? program-providers { provider: provider }))) u1) }))
    (var-set next-program-id (+ program-id u1))
    (ok program-id)))

(define-public (certify-student 
  (student principal)
  (program-id uint)
  (grade uint)
  (certification-hash (buff 32)))
  (let
    ((program (unwrap! (map-get? learning-programs { program-id: program-id }) err-not-found))
     (authority tx-sender))
    (asserts! (is-eq authority (get certification-authority program)) err-unauthorized)
    (asserts! (>= grade u60) err-invalid-program)
    (asserts! (is-none (map-get? student-certifications { student: student, program-id: program-id })) err-already-exists)
    (map-set student-certifications
      { student: student, program-id: program-id }
      {
        completed-at: stacks-block-height,
        grade: grade,
        verified: true,
        certification-hash: certification-hash
      })
    (map-set certification-authorities
      { authority: authority }
      (merge
        (unwrap-panic (map-get? certification-authorities { authority: authority }))
        { certifications-issued: (+ (get certifications-issued (unwrap-panic (map-get? certification-authorities { authority: authority }))) u1) }))
    (ok true)))

(define-public (create-job-offer
  (required-program-id uint)
  (position-title (string-ascii 100))
  (salary-stx uint)
  (duration-blocks uint)
  (max-positions uint)
  (expires-blocks uint))
  (let
    ((offer-id (var-get next-offer-id))
     (employer tx-sender))
    (asserts! (is-some (map-get? learning-programs { program-id: required-program-id })) err-invalid-program)
    (asserts! (> max-positions u0) err-invalid-offer)
    (asserts! (> salary-stx u0) err-invalid-offer)
    (map-set job-offers
      { offer-id: offer-id }
      {
        employer: employer,
        required-program-id: required-program-id,
        position-title: position-title,
        salary-stx: salary-stx,
        duration-blocks: duration-blocks,
        max-positions: max-positions,
        filled-positions: u0,
        expires-at: (+ stacks-block-height expires-blocks),
        active: true,
        created-at: stacks-block-height
      })
    (var-set next-offer-id (+ offer-id u1))
    (ok offer-id)))

(define-public (apply-for-job (offer-id uint))
  (let
    ((offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (student tx-sender))
    (asserts! (get active offer) err-invalid-offer)
    (asserts! (< stacks-block-height (get expires-at offer)) err-expired)
    (asserts! (< (get filled-positions offer) (get max-positions offer)) err-invalid-offer)
    (asserts! (is-some (map-get? student-certifications { student: student, program-id: (get required-program-id offer) })) err-not-certified)
    (asserts! (is-none (map-get? job-applications { student: student, offer-id: offer-id })) err-already-exists)
    (map-set job-applications
      { student: student, offer-id: offer-id }
      {
        applied-at: stacks-block-height,
        status: "pending",
        employer-response: none
      })
    (ok true)))

(define-public (approve-application (student principal) (offer-id uint))
  (let
    ((offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (application (unwrap! (map-get? job-applications { student: student, offer-id: offer-id }) err-not-found))
     (employer tx-sender))
    (asserts! (is-eq employer (get employer offer)) err-unauthorized)
    (asserts! (is-eq (get status application) "pending") err-invalid-offer)
    (asserts! (< (get filled-positions offer) (get max-positions offer)) err-invalid-offer)
    (map-set job-applications
      { student: student, offer-id: offer-id }
      (merge application { status: "approved" }))
    (map-set job-offers
      { offer-id: offer-id }
      (merge offer { filled-positions: (+ (get filled-positions offer) u1) }))
    (ok true)))

(define-public (reject-application (student principal) (offer-id uint) (reason (string-ascii 200)))
  (let
    ((offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (application (unwrap! (map-get? job-applications { student: student, offer-id: offer-id }) err-not-found))
     (employer tx-sender))
    (asserts! (is-eq employer (get employer offer)) err-unauthorized)
    (asserts! (is-eq (get status application) "pending") err-invalid-offer)
    (map-set job-applications
      { student: student, offer-id: offer-id }
      (merge application { 
        status: "rejected", 
        employer-response: (some reason) 
      }))
    (ok true)))

(define-public (withdraw-offer (offer-id uint))
  (let
    ((offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (employer tx-sender))
    (asserts! (is-eq employer (get employer offer)) err-unauthorized)
    (map-set job-offers
      { offer-id: offer-id }
      (merge offer { active: false }))
    (ok true)))

(define-public (transfer-certification 
  (from-student principal)
  (to-student principal)
  (program-id uint))
  (let
    ((certification (unwrap! (map-get? student-certifications { student: from-student, program-id: program-id }) err-not-found)))
    (asserts! (is-eq tx-sender from-student) err-unauthorized)
    (asserts! (is-none (map-get? student-certifications { student: to-student, program-id: program-id })) err-already-exists)
    (map-delete student-certifications { student: from-student, program-id: program-id })
    (map-set student-certifications
      { student: to-student, program-id: program-id }
      certification)
    (ok true)))

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set platform-fee new-fee)
    (ok new-fee)))

(define-read-only (get-program (program-id uint))
  (map-get? learning-programs { program-id: program-id }))

(define-read-only (get-student-certification (student principal) (program-id uint))
  (map-get? student-certifications { student: student, program-id: program-id }))

(define-read-only (get-job-offer (offer-id uint))
  (map-get? job-offers { offer-id: offer-id }))

(define-read-only (get-application (student principal) (offer-id uint))
  (map-get? job-applications { student: student, offer-id: offer-id }))

(define-read-only (get-provider (provider principal))
  (map-get? program-providers { provider: provider }))

(define-read-only (get-certification-authority (authority principal))
  (map-get? certification-authorities { authority: authority }))

(define-read-only (is-student-certified (student principal) (program-id uint))
  (is-some (map-get? student-certifications { student: student, program-id: program-id })))

(define-read-only (get-active-offers-for-program (program-id uint))
  (filter-active-offers program-id))

(define-private (filter-active-offers (program-id uint))
  (filter is-offer-active-for-program-helper
    (list 
      { offer-id: u1, program-id: program-id }
      { offer-id: u2, program-id: program-id }
      { offer-id: u3, program-id: program-id }
      { offer-id: u4, program-id: program-id }
      { offer-id: u5, program-id: program-id })))

(define-private (is-offer-active-for-program-helper (offer-data { offer-id: uint, program-id: uint }))
  (match (map-get? job-offers { offer-id: (get offer-id offer-data) })
    offer (and 
            (get active offer)
            (is-eq (get required-program-id offer) (get program-id offer-data))
            (< stacks-block-height (get expires-at offer))
            (< (get filled-positions offer) (get max-positions offer)))
    false))

(define-read-only (get-student-eligible-offers (student principal))
  (let
    ((student-certs (list 
      { program-id: u1 }
      { program-id: u2 }
      { program-id: u3 }
      { program-id: u4 }
      { program-id: u5 })))
    (fold check-offer-eligibility student-certs (list))))

(define-read-only (get-contract-info)
  {
    next-program-id: (var-get next-program-id),
    next-offer-id: (var-get next-offer-id),
    platform-fee: (var-get platform-fee),
    contract-owner: contract-owner,
    current-block: stacks-block-height
  })

(define-read-only (get-program-statistics (program-id uint))
  (match (map-get? learning-programs { program-id: program-id })
    program (ok {
      program: program,
      total-certified: (get-program-certification-count program-id),
      active-offers: u0
    })
    err-not-found))



(define-private (check-offer-eligibility 
  (program-data { program-id: uint })
  (acc (list 100 uint)))
  (if (is-student-certified tx-sender (get program-id program-data))
    (unwrap-panic (as-max-len? (append acc (get program-id program-data)) u100))
    acc))

(define-private (get-program-certification-count (program-id uint))
  u0)

(define-public (batch-certify-students
  (students (list 10 principal))
  (program-id uint)
  (grades (list 10 uint))
  (certification-hashes (list 10 (buff 32))))
  (let
    ((program (unwrap! (map-get? learning-programs { program-id: program-id }) err-not-found))
     (authority tx-sender))
    (asserts! (is-eq authority (get certification-authority program)) err-unauthorized)
    (asserts! (is-eq (len students) (len grades)) err-invalid-program)
    (asserts! (is-eq (len students) (len certification-hashes)) err-invalid-program)
    (ok (map batch-certify-single 
         students 
         grades 
         certification-hashes 
         (list program-id program-id program-id program-id program-id program-id program-id program-id program-id program-id)))))

(define-private (batch-certify-single 
  (student principal)
  (grade uint)
  (cert-hash (buff 32))
  (program-id uint))
  (begin
    (if (and (>= grade u60) 
             (is-none (map-get? student-certifications { student: student, program-id: program-id })))
      (begin
        (map-set student-certifications
          { student: student, program-id: program-id }
          {
            completed-at: stacks-block-height,
            grade: grade,
            verified: true,
            certification-hash: cert-hash
          })
        true)
      false)))

(define-public (extend-offer-deadline (offer-id uint) (additional-blocks uint))
  (let
    ((offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (employer tx-sender))
    (asserts! (is-eq employer (get employer offer)) err-unauthorized)
    (map-set job-offers
      { offer-id: offer-id }
      (merge offer { expires-at: (+ (get expires-at offer) additional-blocks) }))
    (ok (+ (get expires-at offer) additional-blocks))))

(define-public (update-offer-positions (offer-id uint) (new-max-positions uint))
  (let
    ((offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (employer tx-sender))
    (asserts! (is-eq employer (get employer offer)) err-unauthorized)
    (asserts! (>= new-max-positions (get filled-positions offer)) err-invalid-offer)
    (map-set job-offers
      { offer-id: offer-id }
      (merge offer { max-positions: new-max-positions }))
    (ok new-max-positions)))

(define-read-only (get-offers-by-employer (employer principal))
  (filter-offers-by-employer employer (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))

(define-private (filter-offers-by-employer (employer principal) (offer-ids (list 10 uint)))
  (filter is-employer-offer-partial offer-ids))

(define-private (is-employer-offer-partial (offer-id uint))
  (match (map-get? job-offers { offer-id: offer-id })
    offer (is-eq tx-sender (get employer offer))
    false))

(define-private (is-employer-offer (employer principal) (offer-id uint))
  (match (map-get? job-offers { offer-id: offer-id })
    offer (is-eq employer (get employer offer))
    false))

(define-read-only (get-student-applications (student principal))
  (filter-applications-by-student student (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))

(define-private (filter-applications-by-student (student principal) (offer-ids (list 10 uint)))
  (filter is-student-application-partial offer-ids))

(define-private (is-student-application-partial (offer-id uint))
  (is-some (map-get? job-applications { student: tx-sender, offer-id: offer-id })))

(define-private (is-student-application (student principal) (offer-id uint))
  (is-some (map-get? job-applications { student: student, offer-id: offer-id })))

(define-read-only (calculate-match-score (student principal) (offer-id uint))
  (match (map-get? job-offers { offer-id: offer-id })
    offer (match (map-get? student-certifications { student: student, program-id: (get required-program-id offer) })
      cert (if (get verified cert)
             (+ u50 (/ (get grade cert) u2))
             u0)
      u0)
    u0))
