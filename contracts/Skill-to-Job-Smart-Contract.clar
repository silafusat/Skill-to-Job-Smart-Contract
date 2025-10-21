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
(define-constant err-skill-not-found (err u1009))
(define-constant err-invalid-assessment (err u1010))
(define-constant err-assessment-exists (err u1011))
(define-constant err-insufficient-competency (err u1012))
(define-constant err-review-not-allowed (err u1013))
(define-constant err-already-reviewed (err u1014))
(define-constant err-invalid-rating (err u1015))
(define-constant err-review-not-found (err u1016))

(define-data-var next-program-id uint u1)
(define-data-var next-offer-id uint u1)
(define-data-var platform-fee uint u100)
(define-data-var next-skill-id uint u1)
(define-data-var next-assessment-id uint u1)
(define-data-var next-review-id uint u1)

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

(define-map program-skills
  { program-id: uint, skill-id: uint }
  {
    skill-name: (string-ascii 50),
    required-level: uint,
    weight: uint,
    assessment-type: (string-ascii 20)
  }
)

(define-map student-skill-assessments
  { student: principal, program-id: uint, skill-id: uint }
  {
    assessment-id: uint,
    score: uint,
    max-score: uint,
    assessed-at: uint,
    assessor: principal,
    competency-level: uint
  }
)

(define-map student-competency-profiles
  { student: principal, program-id: uint }
  {
    overall-score: uint,
    skills-assessed: uint,
    skills-required: uint,
    competency-percentage: uint,
    last-updated: uint,
    ready-for-jobs: bool
  }
)

(define-map employer-reputation
  { employer: principal }
  {
    total-hires: uint,
    total-reviews: uint,
    average-rating: uint,
    response-rate: uint,
    verified: bool,
    registration-block: uint
  }
)

(define-map student-reviews
  { review-id: uint }
  {
    student: principal,
    employer: principal,
    offer-id: uint,
    rating: uint,
    review-text: (string-ascii 500),
    work-environment: uint,
    compensation-fairness: uint,
    growth-opportunities: uint,
    would-recommend: bool,
    created-at: uint,
    verified-hire: bool
  }
)

(define-map student-employer-reviews
  { student: principal, employer: principal }
  uint
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

(define-public (define-program-skill
  (program-id uint)
  (skill-name (string-ascii 50))
  (required-level uint)
  (weight uint)
  (assessment-type (string-ascii 20)))
  (let
    ((program (unwrap! (map-get? learning-programs { program-id: program-id }) err-not-found))
     (provider tx-sender)
     (skill-id (var-get next-skill-id)))
    (asserts! (is-eq provider (get provider program)) err-unauthorized)
    (asserts! (and (> required-level u0) (<= required-level u100)) err-invalid-assessment)
    (asserts! (and (> weight u0) (<= weight u100)) err-invalid-assessment)
    (map-set program-skills
      { program-id: program-id, skill-id: skill-id }
      {
        skill-name: skill-name,
        required-level: required-level,
        weight: weight,
        assessment-type: assessment-type
      })
    (var-set next-skill-id (+ skill-id u1))
    (ok skill-id)))

(define-public (submit-skill-assessment
  (student principal)
  (program-id uint)
  (skill-id uint)
  (score uint)
  (max-score uint))
  (let
    ((program (unwrap! (map-get? learning-programs { program-id: program-id }) err-not-found))
     (skill (unwrap! (map-get? program-skills { program-id: program-id, skill-id: skill-id }) err-skill-not-found))
     (assessor tx-sender)
     (assessment-id (var-get next-assessment-id))
     (competency-level (calculate-competency-level score max-score)))
    (asserts! (is-eq assessor (get certification-authority program)) err-unauthorized)
    (asserts! (and (> max-score u0) (<= score max-score)) err-invalid-assessment)
    (asserts! (is-none (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: skill-id })) err-assessment-exists)
    (map-set student-skill-assessments
      { student: student, program-id: program-id, skill-id: skill-id }
      {
        assessment-id: assessment-id,
        score: score,
        max-score: max-score,
        assessed-at: stacks-block-height,
        assessor: assessor,
        competency-level: competency-level
      })
    (var-set next-assessment-id (+ assessment-id u1))
    (let ((profile-update (update-student-competency-profile student program-id)))
      (ok assessment-id))))

(define-private (calculate-competency-level (score uint) (max-score uint))
  (let ((percentage (/ (* score u100) max-score)))
    (if (>= percentage u90)
      u5
      (if (>= percentage u80)
        u4
        (if (>= percentage u70)
          u3
          (if (>= percentage u60)
            u2
            u1))))))

(define-private (update-student-competency-profile (student principal) (program-id uint))
  (let
    ((skills-data (get-student-skills-summary student program-id))
     (overall-score (get overall-score skills-data))
     (skills-assessed (get skills-assessed skills-data))
     (skills-required (get-program-skills-count program-id))
     (competency-percentage (if (> skills-required u0) 
                             (/ (* skills-assessed u100) skills-required) 
                             u0))
     (ready-for-jobs (and (>= competency-percentage u80) 
                          (>= overall-score u70))))
    (map-set student-competency-profiles
      { student: student, program-id: program-id }
      {
        overall-score: overall-score,
        skills-assessed: skills-assessed,
        skills-required: skills-required,
        competency-percentage: competency-percentage,
        last-updated: stacks-block-height,
        ready-for-jobs: ready-for-jobs
      })
    (ok true)))

(define-private (get-student-skills-summary (student principal) (program-id uint))
  (let
    ((skill-1 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u1 }))
     (skill-2 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u2 }))
     (skill-3 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u3 }))
     (skill-4 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u4 }))
     (skill-5 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u5 })))
    {
      overall-score: (/ (+ 
        (get-skill-weighted-score skill-1 program-id u1)
        (get-skill-weighted-score skill-2 program-id u2)
        (get-skill-weighted-score skill-3 program-id u3)
        (get-skill-weighted-score skill-4 program-id u4)
        (get-skill-weighted-score skill-5 program-id u5)) u5),
      skills-assessed: (+ 
        (if (is-some skill-1) u1 u0)
        (if (is-some skill-2) u1 u0)
        (if (is-some skill-3) u1 u0)
        (if (is-some skill-4) u1 u0)
        (if (is-some skill-5) u1 u0))
    }))

(define-private (get-skill-weighted-score (skill-opt (optional { assessment-id: uint, score: uint, max-score: uint, assessed-at: uint, assessor: principal, competency-level: uint })) (program-id uint) (skill-id uint))
  (match skill-opt
    skill-data
    (match (map-get? program-skills { program-id: program-id, skill-id: skill-id })
      skill-config
      (let ((percentage (/ (* (get score skill-data) u100) (get max-score skill-data)))
            (weight (get weight skill-config)))
        (/ (* percentage weight) u100))
      u0)
    u0))

(define-private (get-program-skills-count (program-id uint))
  (+ 
    (if (is-some (map-get? program-skills { program-id: program-id, skill-id: u1 })) u1 u0)
    (if (is-some (map-get? program-skills { program-id: program-id, skill-id: u2 })) u1 u0)
    (if (is-some (map-get? program-skills { program-id: program-id, skill-id: u3 })) u1 u0)
    (if (is-some (map-get? program-skills { program-id: program-id, skill-id: u4 })) u1 u0)
    (if (is-some (map-get? program-skills { program-id: program-id, skill-id: u5 })) u1 u0)))

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
    offer (match (map-get? student-competency-profiles { student: student, program-id: (get required-program-id offer) })
      profile (if (get ready-for-jobs profile)
                (+ u60 (/ (get overall-score profile) u3))
                (/ (get competency-percentage profile) u2))
      (match (map-get? student-certifications { student: student, program-id: (get required-program-id offer) })
        cert (if (get verified cert)
               (+ u30 (/ (get grade cert) u3))
               u0)
        u0))
    u0))

(define-read-only (get-student-skill-profile (student principal) (program-id uint))
  (let
    ((profile (map-get? student-competency-profiles { student: student, program-id: program-id }))
     (skill-1 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u1 }))
     (skill-2 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u2 }))
     (skill-3 (map-get? student-skill-assessments { student: student, program-id: program-id, skill-id: u3 })))
    {
      competency-profile: profile,
      skill-assessments: {
        skill-1: skill-1,
        skill-2: skill-2,
        skill-3: skill-3
      }
    }))

(define-read-only (get-program-skill-requirements (program-id uint))
  (let
    ((skill-1 (map-get? program-skills { program-id: program-id, skill-id: u1 }))
     (skill-2 (map-get? program-skills { program-id: program-id, skill-id: u2 }))
     (skill-3 (map-get? program-skills { program-id: program-id, skill-id: u3 }))
     (skill-4 (map-get? program-skills { program-id: program-id, skill-id: u4 }))
     (skill-5 (map-get? program-skills { program-id: program-id, skill-id: u5 })))
    {
      total-skills: (get-program-skills-count program-id),
      skills: {
        skill-1: skill-1,
        skill-2: skill-2,
        skill-3: skill-3,
        skill-4: skill-4,
        skill-5: skill-5
      }
    }))

(define-read-only (get-student-competency-status (student principal) (program-id uint))
  (match (map-get? student-competency-profiles { student: student, program-id: program-id })
    profile {
      ready-for-jobs: (get ready-for-jobs profile),
      overall-score: (get overall-score profile),
      completion-rate: (get competency-percentage profile),
      skills-completed: (get skills-assessed profile),
      skills-required: (get skills-required profile),
      last-updated: (get last-updated profile)
    }
    {
      ready-for-jobs: false,
      overall-score: u0,
      completion-rate: u0,
      skills-completed: u0,
      skills-required: (get-program-skills-count program-id),
      last-updated: u0
    }))

(define-read-only (get-enhanced-contract-info)
  {
    next-program-id: (var-get next-program-id),
    next-offer-id: (var-get next-offer-id),
    next-skill-id: (var-get next-skill-id),
    next-assessment-id: (var-get next-assessment-id),
    platform-fee: (var-get platform-fee),
    contract-owner: contract-owner,
    current-block: stacks-block-height
  })

(define-public (register-employer (employer principal))
  (begin
    (asserts! (is-none (map-get? employer-reputation { employer: employer })) err-already-exists)
    (map-set employer-reputation
      { employer: employer }
      {
        total-hires: u0,
        total-reviews: u0,
        average-rating: u0,
        response-rate: u100,
        verified: false,
        registration-block: stacks-block-height
      })
    (ok employer)))

(define-public (verify-employer (employer principal))
  (let
    ((reputation (unwrap! (map-get? employer-reputation { employer: employer }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (map-set employer-reputation
      { employer: employer }
      (merge reputation { verified: true }))
    (ok true)))

(define-public (submit-employer-review
  (employer principal)
  (offer-id uint)
  (rating uint)
  (review-text (string-ascii 500))
  (work-environment uint)
  (compensation-fairness uint)
  (growth-opportunities uint)
  (would-recommend bool))
  (let
    ((student tx-sender)
     (review-id (var-get next-review-id))
     (offer (unwrap! (map-get? job-offers { offer-id: offer-id }) err-not-found))
     (application (unwrap! (map-get? job-applications { student: student, offer-id: offer-id }) err-not-found))
     (reputation (default-to
                   { total-hires: u0, total-reviews: u0, average-rating: u0, response-rate: u100, verified: false, registration-block: stacks-block-height }
                   (map-get? employer-reputation { employer: employer }))))
    (asserts! (is-eq employer (get employer offer)) err-unauthorized)
    (asserts! (is-eq (get status application) "approved") err-review-not-allowed)
    (asserts! (is-none (map-get? student-employer-reviews { student: student, employer: employer })) err-already-reviewed)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (and (>= work-environment u1) (<= work-environment u5)) err-invalid-rating)
    (asserts! (and (>= compensation-fairness u1) (<= compensation-fairness u5)) err-invalid-rating)
    (asserts! (and (>= growth-opportunities u1) (<= growth-opportunities u5)) err-invalid-rating)
    (map-set student-reviews
      { review-id: review-id }
      {
        student: student,
        employer: employer,
        offer-id: offer-id,
        rating: rating,
        review-text: review-text,
        work-environment: work-environment,
        compensation-fairness: compensation-fairness,
        growth-opportunities: growth-opportunities,
        would-recommend: would-recommend,
        created-at: stacks-block-height,
        verified-hire: true
      })
    (map-set student-employer-reviews
      { student: student, employer: employer }
      review-id)
    (let
      ((new-total-reviews (+ (get total-reviews reputation) u1))
       (current-total-rating (* (get average-rating reputation) (get total-reviews reputation)))
       (new-average-rating (/ (+ current-total-rating rating) new-total-reviews)))
      (map-set employer-reputation
        { employer: employer }
        (merge reputation {
          total-reviews: new-total-reviews,
          average-rating: new-average-rating
        })))
    (var-set next-review-id (+ review-id u1))
    (ok review-id)))

(define-public (update-employer-hire-count (employer principal))
  (let
    ((reputation (unwrap! (map-get? employer-reputation { employer: employer }) err-not-found)))
    (asserts! (is-eq tx-sender employer) err-unauthorized)
    (map-set employer-reputation
      { employer: employer }
      (merge reputation { total-hires: (+ (get total-hires reputation) u1) }))
    (ok true)))

(define-read-only (get-employer-reputation (employer principal))
  (map-get? employer-reputation { employer: employer }))

(define-read-only (get-employer-reviews (employer principal))
  (let
    ((review-1 (map-get? student-reviews { review-id: u1 }))
     (review-2 (map-get? student-reviews { review-id: u2 }))
     (review-3 (map-get? student-reviews { review-id: u3 }))
     (review-4 (map-get? student-reviews { review-id: u4 }))
     (review-5 (map-get? student-reviews { review-id: u5 })))
    (filter-employer-reviews employer (list review-1 review-2 review-3 review-4 review-5))))

(define-private (filter-employer-reviews (employer principal) (reviews (list 5 (optional { student: principal, employer: principal, offer-id: uint, rating: uint, review-text: (string-ascii 500), work-environment: uint, compensation-fairness: uint, growth-opportunities: uint, would-recommend: bool, created-at: uint, verified-hire: bool }))))
  (filter is-employer-review-partial reviews))

(define-private (is-employer-review-partial (review-opt (optional { student: principal, employer: principal, offer-id: uint, rating: uint, review-text: (string-ascii 500), work-environment: uint, compensation-fairness: uint, growth-opportunities: uint, would-recommend: bool, created-at: uint, verified-hire: bool })))
  (match review-opt
    review (is-eq (get employer review) tx-sender)
    false))

(define-read-only (get-review (review-id uint))
  (map-get? student-reviews { review-id: review-id }))

(define-read-only (get-student-review-for-employer (student principal) (employer principal))
  (match (map-get? student-employer-reviews { student: student, employer: employer })
    review-id (map-get? student-reviews { review-id: review-id })
    none))

(define-read-only (calculate-employer-trust-score (employer principal))
  (match (map-get? employer-reputation { employer: employer })
    reputation
    (let
      ((verified-bonus (if (get verified reputation) u20 u0))
       (rating-score (* (get average-rating reputation) u10))
       (review-count-bonus (if (>= (get total-reviews reputation) u10) u15 (/ (* (get total-reviews reputation) u15) u10)))
       (response-bonus (/ (get response-rate reputation) u10))
       (hire-count-bonus (if (>= (get total-hires reputation) u5) u10 (/ (* (get total-hires reputation) u10) u5))))
      (+ verified-bonus rating-score review-count-bonus response-bonus hire-count-bonus))
    u0))

(define-read-only (get-employer-detailed-stats (employer principal))
  (match (map-get? employer-reputation { employer: employer })
    reputation
    (ok {
      reputation: reputation,
      trust-score: (calculate-employer-trust-score employer),
      active-offers: (count-employer-active-offers employer),
      recommendation-rate: (calculate-recommendation-rate employer)
    })
    err-not-found))

(define-private (count-employer-active-offers (employer principal))
  (+ 
    (if (is-active-employer-offer employer u1) u1 u0)
    (if (is-active-employer-offer employer u2) u1 u0)
    (if (is-active-employer-offer employer u3) u1 u0)
    (if (is-active-employer-offer employer u4) u1 u0)
    (if (is-active-employer-offer employer u5) u1 u0)))

(define-private (is-active-employer-offer (employer principal) (offer-id uint))
  (match (map-get? job-offers { offer-id: offer-id })
    offer (and
            (is-eq (get employer offer) employer)
            (get active offer)
            (< stacks-block-height (get expires-at offer))
            (< (get filled-positions offer) (get max-positions offer)))
    false))

(define-private (calculate-recommendation-rate (employer principal))
  (let
    ((review-1 (map-get? student-reviews { review-id: u1 }))
     (review-2 (map-get? student-reviews { review-id: u2 }))
     (review-3 (map-get? student-reviews { review-id: u3 }))
     (review-4 (map-get? student-reviews { review-id: u4 }))
     (review-5 (map-get? student-reviews { review-id: u5 })))
    (let
      ((total-reviews (+ 
                        (if (and (is-some review-1) (is-eq employer (get employer (unwrap-panic review-1)))) u1 u0)
                        (if (and (is-some review-2) (is-eq employer (get employer (unwrap-panic review-2)))) u1 u0)
                        (if (and (is-some review-3) (is-eq employer (get employer (unwrap-panic review-3)))) u1 u0)
                        (if (and (is-some review-4) (is-eq employer (get employer (unwrap-panic review-4)))) u1 u0)
                        (if (and (is-some review-5) (is-eq employer (get employer (unwrap-panic review-5)))) u1 u0)))
       (recommended-count (+ 
                            (if (and (is-some review-1) (is-eq employer (get employer (unwrap-panic review-1))) (get would-recommend (unwrap-panic review-1))) u1 u0)
                            (if (and (is-some review-2) (is-eq employer (get employer (unwrap-panic review-2))) (get would-recommend (unwrap-panic review-2))) u1 u0)
                            (if (and (is-some review-3) (is-eq employer (get employer (unwrap-panic review-3))) (get would-recommend (unwrap-panic review-3))) u1 u0)
                            (if (and (is-some review-4) (is-eq employer (get employer (unwrap-panic review-4))) (get would-recommend (unwrap-panic review-4))) u1 u0)
                            (if (and (is-some review-5) (is-eq employer (get employer (unwrap-panic review-5))) (get would-recommend (unwrap-panic review-5))) u1 u0))))
      (if (> total-reviews u0)
        (/ (* recommended-count u100) total-reviews)
        u0))))

(define-read-only (get-top-rated-employers)
  (let
    ((employer-1 (get-employer-if-exists u1))
     (employer-2 (get-employer-if-exists u2))
     (employer-3 (get-employer-if-exists u3))
     (employer-4 (get-employer-if-exists u4))
     (employer-5 (get-employer-if-exists u5)))
    (list employer-1 employer-2 employer-3 employer-4 employer-5)))

(define-private (get-employer-if-exists (index uint))
  none)
