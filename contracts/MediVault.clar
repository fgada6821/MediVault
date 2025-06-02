(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PATIENT-EXISTS (err u101))
(define-constant ERR-NO-PATIENT (err u102))
(define-constant ERR-NO-RECORD (err u103))
(define-constant ERR-NOT-DOCTOR (err u104))

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

(define-data-var record-counter uint u0)

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