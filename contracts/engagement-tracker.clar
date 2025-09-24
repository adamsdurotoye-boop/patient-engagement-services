;; Patient Engagement Services Contract
;; Communication platform with appointment reminders and satisfaction surveys

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-PATIENT-NOT-FOUND (err u101))
(define-constant ERR-APPOINTMENT-NOT-FOUND (err u102))
(define-constant ERR-SURVEY-NOT-FOUND (err u103))
(define-constant ERR-INVALID-RATING (err u104))

;; Data structures
(define-map patients
  { patient-id: uint }
  {
    name: (string-ascii 64),
    contact-info: (string-ascii 128),
    preferred-communication: (string-ascii 16),
    engagement-score: uint,
    total-appointments: uint,
    provider: principal
  }
)

(define-map appointments
  { appointment-id: uint }
  {
    patient-id: uint,
    appointment-date: uint,
    appointment-type: (string-ascii 32),
    status: (string-ascii 16),
    reminder-sent: bool,
    outcome-recorded: bool,
    provider: principal
  }
)

(define-map satisfaction-surveys
  { survey-id: uint }
  {
    patient-id: uint,
    appointment-id: uint,
    satisfaction-rating: uint,
    feedback-text: (string-ascii 256),
    survey-date: uint,
    response-collected: bool
  }
)

(define-map health-education
  { education-id: uint }
  {
    patient-id: uint,
    content-type: (string-ascii 32),
    topic: (string-ascii 64),
    delivery-date: uint,
    engagement-status: (string-ascii 16),
    provider: principal
  }
)

(define-data-var next-patient-id uint u1)
(define-data-var next-appointment-id uint u1)
(define-data-var next-survey-id uint u1)
(define-data-var next-education-id uint u1)

;; Register new patient
(define-public (register-patient (name (string-ascii 64)) (contact-info (string-ascii 128)) (preferred-communication (string-ascii 16)))
  (let ((patient-id (var-get next-patient-id)))
    (map-set patients
      { patient-id: patient-id }
      {
        name: name,
        contact-info: contact-info,
        preferred-communication: preferred-communication,
        engagement-score: u50,
        total-appointments: u0,
        provider: tx-sender
      }
    )
    (var-set next-patient-id (+ patient-id u1))
    (ok patient-id)
  )
)

;; Schedule appointment
(define-public (schedule-appointment (patient-id uint) (appointment-date uint) (appointment-type (string-ascii 32)))
  (let 
    (
      (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND))
      (appointment-id (var-get next-appointment-id))
    )
    (asserts! (is-eq (get provider patient) tx-sender) ERR-UNAUTHORIZED)
    (map-set appointments
      { appointment-id: appointment-id }
      {
        patient-id: patient-id,
        appointment-date: appointment-date,
        appointment-type: appointment-type,
        status: "scheduled",
        reminder-sent: false,
        outcome-recorded: false,
        provider: tx-sender
      }
    )
    (map-set patients
      { patient-id: patient-id }
      (merge patient { total-appointments: (+ (get total-appointments patient) u1) })
    )
    (var-set next-appointment-id (+ appointment-id u1))
    (ok appointment-id)
  )
)

;; Send appointment reminder
(define-public (send-reminder (appointment-id uint))
  (let ((appointment (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR-APPOINTMENT-NOT-FOUND)))
    (asserts! (is-eq (get provider appointment) tx-sender) ERR-UNAUTHORIZED)
    (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment { reminder-sent: true })
    )
    (ok true)
  )
)

;; Create satisfaction survey
(define-public (create-survey (patient-id uint) (appointment-id uint))
  (let 
    (
      (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND))
      (survey-id (var-get next-survey-id))
    )
    (asserts! (is-eq (get provider patient) tx-sender) ERR-UNAUTHORIZED)
    (map-set satisfaction-surveys
      { survey-id: survey-id }
      {
        patient-id: patient-id,
        appointment-id: appointment-id,
        satisfaction-rating: u0,
        feedback-text: "",
        survey-date: u0,
        response-collected: false
      }
    )
    (var-set next-survey-id (+ survey-id u1))
    (ok survey-id)
  )
)

;; Submit survey response
(define-public (submit-survey-response (survey-id uint) (rating uint) (feedback (string-ascii 256)))
  (let ((survey (unwrap! (map-get? satisfaction-surveys { survey-id: survey-id }) ERR-SURVEY-NOT-FOUND)))
    (asserts! (<= rating u10) ERR-INVALID-RATING)
    (asserts! (> rating u0) ERR-INVALID-RATING)
    (map-set satisfaction-surveys
      { survey-id: survey-id }
      (merge survey { 
        satisfaction-rating: rating,
        feedback-text: feedback,
        survey-date: u0,
        response-collected: true 
      })
    )
    (ok true)
  )
)

;; Deliver health education content
(define-public (deliver-education (patient-id uint) (content-type (string-ascii 32)) (topic (string-ascii 64)))
  (let 
    (
      (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND))
      (education-id (var-get next-education-id))
    )
    (asserts! (is-eq (get provider patient) tx-sender) ERR-UNAUTHORIZED)
    (map-set health-education
      { education-id: education-id }
      {
        patient-id: patient-id,
        content-type: content-type,
        topic: topic,
        delivery-date: u0,
        engagement-status: "delivered",
        provider: tx-sender
      }
    )
    (var-set next-education-id (+ education-id u1))
    (ok education-id)
  )
)

;; Update engagement score
(define-public (update-engagement-score (patient-id uint) (new-score uint))
  (let ((patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND)))
    (asserts! (is-eq (get provider patient) tx-sender) ERR-UNAUTHORIZED)
    (map-set patients
      { patient-id: patient-id }
      (merge patient { engagement-score: new-score })
    )
    (ok true)
  )
)

;; Get patient information
(define-read-only (get-patient-info (patient-id uint))
  (map-get? patients { patient-id: patient-id })
)

;; Get appointment details
(define-read-only (get-appointment-details (appointment-id uint))
  (map-get? appointments { appointment-id: appointment-id })
)

;; Get survey response
(define-read-only (get-survey-response (survey-id uint))
  (map-get? satisfaction-surveys { survey-id: survey-id })
)

;; Get education content
(define-read-only (get-education-content (education-id uint))
  (map-get? health-education { education-id: education-id })
)
