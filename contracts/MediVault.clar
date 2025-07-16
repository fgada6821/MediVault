(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PATIENT-EXISTS (err u101))
(define-constant ERR-NO-PATIENT (err u102))
(define-constant ERR-NO-RECORD (err u103))
(define-constant ERR-NOT-DOCTOR (err u104))
(define-constant ERR-NOT-INSURANCE-PROVIDER (err u105))
(define-constant ERR-CLAIM-NOT-FOUND (err u106))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u107))
(define-constant ERR-INVALID-CLAIM-AMOUNT (err u108))
(define-constant ERR-PROVIDER-NOT-VERIFIED (err u109))

(define-data-var contract-owner principal tx-sender)

(define-map Patients 
  { patient-id: principal }
  {
    name: (string-ascii 64),
    dob: uint,
    blood-type: (string-ascii 3),
    emergency-contact: (string-ascii 64),
    registered-at: uint
  }
)

(define-map DoctorRegistry
  { doctor-id: principal }
  {
    name: (string-ascii 64),
    specialty: (string-ascii 64),
    license-number: (string-ascii 32),
    verified: bool
  }
)

(define-map MedicalRecords
  { 
    patient-id: principal,
    record-id: uint
  }
  {
    doctor-id: principal,
    timestamp: uint,
    diagnosis: (string-ascii 256),
    prescription: (string-ascii 256),
    notes: (string-ascii 512),
    confidential: bool
  }
)

(define-map PatientDoctorAccess
  {
    patient-id: principal,
    doctor-id: principal
  }
  {
    granted-at: uint,
    access-level: uint
  }
)

(define-map InsuranceProviders
  { provider-id: principal }
  {
    name: (string-ascii 64),
    license-number: (string-ascii 32),
    coverage-types: (string-ascii 128),
    verified: bool,
    registered-at: uint
  }
)

(define-map PatientInsurance
  { patient-id: principal }
  {
    provider-id: principal,
    policy-number: (string-ascii 32),
    coverage-start: uint,
    coverage-end: uint,
    deductible: uint,
    copay-percentage: uint
  }
)

(define-map InsuranceClaims
  { claim-id: uint }
  {
    patient-id: principal,
    provider-id: principal,
    doctor-id: principal,
    record-id: uint,
    claim-amount: uint,
    submitted-at: uint,
    status: uint,
    processed-at: uint,
    approved-amount: uint,
    rejection-reason: (string-ascii 256)
  }
)

(define-data-var record-counter uint u0)
(define-data-var claim-counter uint u0)

(define-public (register-patient (name (string-ascii 64)) (dob uint) (blood-type (string-ascii 3)) (emergency-contact (string-ascii 64)))
  (let ((patient-data { patient-id: tx-sender }))
    (if (is-none (map-get? Patients patient-data))
      (ok (map-set Patients 
        patient-data
        {
          name: name,
          dob: dob,
          blood-type: blood-type,
          emergency-contact: emergency-contact,
          registered-at: stacks-block-height
        }))
      ERR-PATIENT-EXISTS)))

(define-public (register-doctor (name (string-ascii 64)) (specialty (string-ascii 64)) (license-number (string-ascii 32)))
  (ok (map-set DoctorRegistry
    { doctor-id: tx-sender }
    {
      name: name,
      specialty: specialty,
      license-number: license-number,
      verified: false
    })))

(define-public (verify-doctor (doctor-id principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set DoctorRegistry
      { doctor-id: doctor-id }
      (merge (unwrap! (map-get? DoctorRegistry { doctor-id: doctor-id }) ERR-NOT-DOCTOR)
      { verified: true })))))

(define-public (grant-access (doctor-id principal))
  (ok (map-set PatientDoctorAccess
    {
      patient-id: tx-sender,
      doctor-id: doctor-id
    }
    {
      granted-at: stacks-block-height,
      access-level: u1
    })))

(define-public (revoke-access (doctor-id principal))
  (ok (map-delete PatientDoctorAccess
    {
      patient-id: tx-sender,
      doctor-id: doctor-id
    })))

(define-public (add-medical-record 
    (patient-id principal)
    (diagnosis (string-ascii 256))
    (prescription (string-ascii 256))
    (notes (string-ascii 512))
    (confidential bool))
  (let ((current-record-id (var-get record-counter)))
    (asserts! (is-some (map-get? DoctorRegistry { doctor-id: tx-sender })) ERR-NOT-DOCTOR)
    (asserts! (is-some (map-get? PatientDoctorAccess { patient-id: patient-id, doctor-id: tx-sender })) ERR-NOT-AUTHORIZED)
    (var-set record-counter (+ current-record-id u1))
    (ok (map-set MedicalRecords
      {
        patient-id: patient-id,
        record-id: current-record-id
      }
      {
        doctor-id: tx-sender,
        timestamp: stacks-block-height,
        diagnosis: diagnosis,
        prescription: prescription,
        notes: notes,
        confidential: confidential
      }))))

(define-read-only (get-patient-info (patient-id principal))
  (map-get? Patients { patient-id: patient-id }))

(define-read-only (get-doctor-info (doctor-id principal))
  (map-get? DoctorRegistry { doctor-id: doctor-id }))

(define-read-only (get-medical-record (patient-id principal) (record-id uint))
  (let ((record (map-get? MedicalRecords { patient-id: patient-id, record-id: record-id })))
    (if (and
      (is-some record)
      (or
        (is-eq tx-sender patient-id)
        (is-some (map-get? PatientDoctorAccess { patient-id: patient-id, doctor-id: tx-sender }))
      ))
      record
      none)))

(define-read-only (check-access (patient-id principal) (doctor-id principal))
  (map-get? PatientDoctorAccess { patient-id: patient-id, doctor-id: doctor-id }))

(define-public (register-insurance-provider (name (string-ascii 64)) (license-number (string-ascii 32)) (coverage-types (string-ascii 128)))
  (ok (map-set InsuranceProviders
    { provider-id: tx-sender }
    {
      name: name,
      license-number: license-number,
      coverage-types: coverage-types,
      verified: false,
      registered-at: stacks-block-height
    })))

(define-public (verify-insurance-provider (provider-id principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? InsuranceProviders { provider-id: provider-id })) ERR-NOT-INSURANCE-PROVIDER)
    (ok (map-set InsuranceProviders
      { provider-id: provider-id }
      (merge (unwrap! (map-get? InsuranceProviders { provider-id: provider-id }) ERR-NOT-INSURANCE-PROVIDER)
      { verified: true })))))

(define-public (add-patient-insurance 
    (provider-id principal)
    (policy-number (string-ascii 32))
    (coverage-start uint)
    (coverage-end uint)
    (deductible uint)
    (copay-percentage uint))
  (begin
    (asserts! (is-some (map-get? Patients { patient-id: tx-sender })) ERR-NO-PATIENT)
    (asserts! (is-some (map-get? InsuranceProviders { provider-id: provider-id })) ERR-NOT-INSURANCE-PROVIDER)
    (ok (map-set PatientInsurance
      { patient-id: tx-sender }
      {
        provider-id: provider-id,
        policy-number: policy-number,
        coverage-start: coverage-start,
        coverage-end: coverage-end,
        deductible: deductible,
        copay-percentage: copay-percentage
      }))))

(define-public (submit-insurance-claim 
    (patient-id principal)
    (record-id uint)
    (claim-amount uint))
  (let 
    ((current-claim-id (var-get claim-counter))
     (patient-insurance (unwrap! (map-get? PatientInsurance { patient-id: patient-id }) ERR-NO-PATIENT))
     (provider-id (get provider-id patient-insurance))
     (provider-info (unwrap! (map-get? InsuranceProviders { provider-id: provider-id }) ERR-NOT-INSURANCE-PROVIDER)))
    (asserts! (is-some (map-get? DoctorRegistry { doctor-id: tx-sender })) ERR-NOT-DOCTOR)
    (asserts! (is-some (map-get? MedicalRecords { patient-id: patient-id, record-id: record-id })) ERR-NO-RECORD)
    (asserts! (get verified provider-info) ERR-PROVIDER-NOT-VERIFIED)
    (asserts! (> claim-amount u0) ERR-INVALID-CLAIM-AMOUNT)
    (var-set claim-counter (+ current-claim-id u1))
    (ok (map-set InsuranceClaims
      { claim-id: current-claim-id }
      {
        patient-id: patient-id,
        provider-id: provider-id,
        doctor-id: tx-sender,
        record-id: record-id,
        claim-amount: claim-amount,
        submitted-at: stacks-block-height,
        status: u1,
        processed-at: u0,
        approved-amount: u0,
        rejection-reason: ""
      }))))

(define-public (process-insurance-claim 
    (claim-id uint)
    (approved bool)
    (approved-amount uint)
    (rejection-reason (string-ascii 256)))
  (let ((claim (unwrap! (map-get? InsuranceClaims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND)))
    (asserts! (is-some (map-get? InsuranceProviders { provider-id: tx-sender })) ERR-NOT-INSURANCE-PROVIDER)
    (asserts! (is-eq (get provider-id claim) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status claim) u1) ERR-CLAIM-ALREADY-PROCESSED)
    (ok (map-set InsuranceClaims
      { claim-id: claim-id }
      (merge claim
        {
          status: (if approved u2 u3),
          processed-at: stacks-block-height,
          approved-amount: approved-amount,
          rejection-reason: rejection-reason
        })))))

(define-read-only (get-insurance-provider (provider-id principal))
  (map-get? InsuranceProviders { provider-id: provider-id }))

(define-read-only (get-patient-insurance (patient-id principal))
  (map-get? PatientInsurance { patient-id: patient-id }))

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? InsuranceClaims { claim-id: claim-id }))

(define-read-only (get-claim-status (claim-id uint))
  (let ((claim (map-get? InsuranceClaims { claim-id: claim-id })))
    (if (is-some claim)
      (some (get status (unwrap-panic claim)))
      none)))