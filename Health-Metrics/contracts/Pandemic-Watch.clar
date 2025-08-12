;; Global Health Data Oracle - Pandemic Intelligence Network
;; A decentralized oracle system for collecting, validating, and distributing 
;; real-time pandemic and health crisis data across global regions with 
;; reputation-based data integrity and multi-oracle consensus mechanisms

;; ERROR CONSTANTS - Validation and Authorization Failures

(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-INVALID-INPUT-DATA (err u101))
(define-constant ERR-HEALTH-DATA-NOT-FOUND (err u102))
(define-constant ERR-ORACLE-NOT-AUTHORIZED (err u103))
(define-constant ERR-INVALID-TIMESTAMP-FORMAT (err u104))
(define-constant ERR-OUTDATED-DATA-SUBMISSION (err u105))
(define-constant ERR-INVALID-LOCATION-FORMAT (err u106))
(define-constant ERR-INSUFFICIENT-ORACLE-REPUTATION (err u107))
(define-constant ERR-DATA-VALIDATION-FAILED (err u108))
(define-constant ERR-INVALID-NUMERICAL-VALUE (err u109))
(define-constant ERR-INVALID-PRINCIPAL (err u110))

;; CONTRACT CONFIGURATION CONSTANTS

(define-constant initial-contract-deployer tx-sender)
(define-constant default-oracle-submission-fee u1000000) ;; 1 STX in microSTX
(define-constant health-data-freshness-window u86400) ;; 24 hours validity
(define-constant maximum-data-age-limit u604800) ;; 7 days maximum age
(define-constant minimum-oracle-reputation-threshold u10)
(define-constant maximum-bulk-submissions-per-transaction u10)
(define-constant initial-oracle-reputation-score u50)
(define-constant contract-owner-reputation-score u100)
(define-constant maximum-numerical-value u4294967295) ;; Max uint32 value for safety
(define-constant minimum-numerical-value u0)

;; STATE MANAGEMENT VARIABLES

(define-data-var current-contract-administrator principal initial-contract-deployer)
(define-data-var oracle-submission-fee-amount uint default-oracle-submission-fee)
(define-data-var data-validity-timeframe uint health-data-freshness-window)
(define-data-var total-data-submissions uint u0)
(define-data-var contract-creation-timestamp uint u0)

;; CORE DATA STRUCTURES - Health Data Storage and Management

;; Primary health data registry mapping location and metric type to comprehensive data record
(define-map global-health-data-registry 
    {geographic-location: (string-ascii 50), health-metric-type: (string-ascii 30)}
    {
        numerical-value: uint,
        data-submission-timestamp: uint,
        submitting-oracle-address: principal,
        verification-status: bool,
        blockchain-block-height: uint,
        data-source-confidence-score: uint
    }
)

;; Oracle authorization and management system
(define-map authorized-health-data-oracles principal bool)

;; Reputation scoring system for oracle reliability tracking  
(define-map oracle-reputation-scores principal uint)

;; Geographic region validation mapping for location-based data integrity
(define-map geographic-region-validators (string-ascii 50) (list 5 principal))

;; Historical data submission tracking for analytics and auditing
(define-map oracle-submission-history 
    principal 
    {
        total-submissions: uint,
        verified-submissions: uint,
        last-submission-timestamp: uint,
        accuracy-percentage: uint
    }
)

;; CONTRACT INITIALIZATION

;; Initialize contract deployer with full permissions and maximum reputation
(map-set authorized-health-data-oracles initial-contract-deployer true)
(map-set oracle-reputation-scores initial-contract-deployer contract-owner-reputation-score)
(map-set oracle-submission-history initial-contract-deployer 
    {total-submissions: u0, verified-submissions: u0, last-submission-timestamp: u0, accuracy-percentage: u100})

;; READ-ONLY QUERY FUNCTIONS - Public Data Access Interface

(define-read-only (get-current-contract-administrator)
    (var-get current-contract-administrator)
)

(define-read-only (get-oracle-submission-fee)
    (var-get oracle-submission-fee-amount)
)

(define-read-only (is-oracle-authorized (oracle-address principal))
    (default-to false (map-get? authorized-health-data-oracles oracle-address))
)

(define-read-only (get-oracle-reputation-score (oracle-address principal))
    (default-to u0 (map-get? oracle-reputation-scores oracle-address))
)

(define-read-only (retrieve-health-data-entry (geographic-location (string-ascii 50)) (health-metric-type (string-ascii 30)))
    (map-get? global-health-data-registry {geographic-location: geographic-location, health-metric-type: health-metric-type})
)

(define-read-only (verify-data-freshness (geographic-location (string-ascii 50)) (health-metric-type (string-ascii 30)))
    (match (map-get? global-health-data-registry {geographic-location: geographic-location, health-metric-type: health-metric-type})
        health-data-entry 
        (let ((submission-timestamp (get data-submission-timestamp health-data-entry))
              (current-blockchain-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
              (freshness-window (var-get data-validity-timeframe)))
            (<= (- current-blockchain-time submission-timestamp) freshness-window))
        false
    )
)

(define-read-only (get-confirmed-case-count (geographic-location (string-ascii 50)))
    (match (map-get? global-health-data-registry {geographic-location: geographic-location, health-metric-type: "confirmed-cases"})
        health-data-entry (some (get numerical-value health-data-entry))
        none
    )
)

(define-read-only (get-mortality-statistics (geographic-location (string-ascii 50)))
    (match (map-get? global-health-data-registry {geographic-location: geographic-location, health-metric-type: "mortality-count"})
        health-data-entry (some (get numerical-value health-data-entry))
        none
    )
)

(define-read-only (get-vaccination-coverage-rate (geographic-location (string-ascii 50)))
    (match (map-get? global-health-data-registry {geographic-location: geographic-location, health-metric-type: "vaccination-coverage"})
        health-data-entry (some (get numerical-value health-data-entry))
        none
    )
)

(define-read-only (get-hospitalization-metrics (geographic-location (string-ascii 50)))
    (match (map-get? global-health-data-registry {geographic-location: geographic-location, health-metric-type: "hospitalization-rate"})
        health-data-entry (some (get numerical-value health-data-entry))
        none
    )
)

(define-read-only (get-oracle-performance-statistics (oracle-address principal))
    (map-get? oracle-submission-history oracle-address)
)

(define-read-only (get-contract-operational-statistics)
    {
        total-submissions: (var-get total-data-submissions),
        contract-administrator: (var-get current-contract-administrator),
        current-submission-fee: (var-get oracle-submission-fee-amount),
        data-validity-window: (var-get data-validity-timeframe)
    }
)

;; PRIVATE UTILITY FUNCTIONS - Internal Validation and Processing

(define-private (validate-geographic-location-format (geographic-location (string-ascii 50)))
    (and 
        (> (len geographic-location) u0)
        (<= (len geographic-location) u50)
        ;; Additional validation could include character set restrictions
    )
)

(define-private (validate-health-metric-type (health-metric-type (string-ascii 30)))
    (or 
        (is-eq health-metric-type "confirmed-cases")
        (is-eq health-metric-type "mortality-count")
        (is-eq health-metric-type "hospitalization-rate")
        (is-eq health-metric-type "vaccination-coverage")
        (is-eq health-metric-type "testing-positivity-rate")
        (is-eq health-metric-type "recovery-statistics")
        (is-eq health-metric-type "icu-occupancy-rate")
    )
)

(define-private (validate-numerical-value (value uint))
    (and 
        (>= value minimum-numerical-value)
        (<= value maximum-numerical-value)
    )
)

(define-private (validate-principal-address (address principal))
    ;; Check if the principal is not the zero principal and is a valid format
    (not (is-eq address 'SP000000000000000000002Q6VF78))
)

(define-private (calculate-updated-oracle-reputation (oracle-address principal) (reputation-increase bool))
    (let ((current-reputation-score (get-oracle-reputation-score oracle-address)))
        (if reputation-increase
            (map-set oracle-reputation-scores oracle-address (+ current-reputation-score u2))
            (if (> current-reputation-score u1)
                (map-set oracle-reputation-scores oracle-address (- current-reputation-score u1))
                (map-set oracle-reputation-scores oracle-address u0)
            )
        )
    )
)

(define-private (update-oracle-submission-statistics (oracle-address principal) (verification-successful bool))
    (match (map-get? oracle-submission-history oracle-address)
        existing-statistics
        (let ((updated-total-submissions (+ (get total-submissions existing-statistics) u1))
              (updated-verified-submissions (if verification-successful 
                  (+ (get verified-submissions existing-statistics) u1)
                  (get verified-submissions existing-statistics)))
              (current-blockchain-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
              (new-accuracy-percentage (if (> updated-total-submissions u0)
                  (/ (* updated-verified-submissions u100) updated-total-submissions)
                  u0)))
            (map-set oracle-submission-history oracle-address
                {
                    total-submissions: updated-total-submissions,
                    verified-submissions: updated-verified-submissions,
                    last-submission-timestamp: current-blockchain-time,
                    accuracy-percentage: new-accuracy-percentage
                }
            )
        )
        ;; Initialize statistics for new oracle
        (map-set oracle-submission-history oracle-address
            {
                total-submissions: u1,
                verified-submissions: (if verification-successful u1 u0),
                last-submission-timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height)),
                accuracy-percentage: (if verification-successful u100 u0)
            }
        )
    )
)

(define-private (validate-timestamp-requirements (submission-timestamp uint))
    (let ((current-blockchain-time (unwrap-panic (get-stacks-block-info? time stacks-block-height))))
        (and 
            (<= submission-timestamp current-blockchain-time)
            (>= submission-timestamp (- current-blockchain-time maximum-data-age-limit))
        )
    )
)

;; ADMINISTRATIVE FUNCTIONS - Contract Management and Oracle Authorization

(define-public (grant-oracle-authorization (new-oracle-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-principal-address new-oracle-address) ERR-INVALID-PRINCIPAL)
        (asserts! (not (is-eq new-oracle-address (var-get current-contract-administrator))) ERR-INVALID-INPUT-DATA)
        (map-set authorized-health-data-oracles new-oracle-address true)
        (map-set oracle-reputation-scores new-oracle-address initial-oracle-reputation-score)
        (ok true)
    )
)

(define-public (revoke-oracle-authorization (oracle-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-principal-address oracle-address) ERR-INVALID-PRINCIPAL)
        (asserts! (not (is-eq oracle-address (var-get current-contract-administrator))) ERR-INVALID-INPUT-DATA)
        (map-delete authorized-health-data-oracles oracle-address)
        (ok true)
    )
)

(define-public (update-oracle-submission-fee (new-fee-amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (> new-fee-amount u0) ERR-INVALID-INPUT-DATA)
        (var-set oracle-submission-fee-amount new-fee-amount)
        (ok true)
    )
)

(define-public (modify-data-validity-timeframe (new-timeframe-seconds uint))
    (begin
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (and (> new-timeframe-seconds u3600) (<= new-timeframe-seconds u604800)) ERR-INVALID-INPUT-DATA)
        (var-set data-validity-timeframe new-timeframe-seconds)
        (ok true)
    )
)

;; CORE DATA SUBMISSION FUNCTIONS - Oracle Data Management

(define-public (submit-health-data-entry 
    (geographic-location (string-ascii 50))
    (health-metric-type (string-ascii 30))
    (numerical-value uint)
    (data-timestamp uint))
    (let ((current-blockchain-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
          (oracle-reputation (get-oracle-reputation-score tx-sender))
          (validated-location geographic-location)
          (validated-metric-type health-metric-type)
          (validated-value numerical-value))
        
        ;; Comprehensive validation checks
        (asserts! (is-oracle-authorized tx-sender) ERR-ORACLE-NOT-AUTHORIZED)
        (asserts! (>= oracle-reputation minimum-oracle-reputation-threshold) ERR-INSUFFICIENT-ORACLE-REPUTATION)
        (asserts! (validate-geographic-location-format validated-location) ERR-INVALID-LOCATION-FORMAT)
        (asserts! (validate-health-metric-type validated-metric-type) ERR-INVALID-INPUT-DATA)
        (asserts! (validate-numerical-value validated-value) ERR-INVALID-NUMERICAL-VALUE)
        (asserts! (validate-timestamp-requirements data-timestamp) ERR-INVALID-TIMESTAMP-FORMAT)
        
        ;; Process oracle submission fee payment
        (try! (stx-transfer? (var-get oracle-submission-fee-amount) tx-sender (as-contract tx-sender)))
        
        ;; Store comprehensive health data entry with validated inputs
        (map-set global-health-data-registry 
            {geographic-location: validated-location, health-metric-type: validated-metric-type}
            {
                numerical-value: validated-value,
                data-submission-timestamp: data-timestamp,
                submitting-oracle-address: tx-sender,
                verification-status: false,
                blockchain-block-height: stacks-block-height,
                data-source-confidence-score: oracle-reputation
            }
        )
        
        ;; Update oracle performance tracking
        (update-oracle-submission-statistics tx-sender false)
        (calculate-updated-oracle-reputation tx-sender true)
        
        ;; Update contract-wide statistics
        (var-set total-data-submissions (+ (var-get total-data-submissions) u1))
        
        (ok true)
    )
)

(define-public (verify-submitted-health-data 
    (geographic-location (string-ascii 50))
    (health-metric-type (string-ascii 30))
    (data-accuracy-confirmed bool))
    (let ((validated-location geographic-location)
          (validated-metric-type health-metric-type))
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-geographic-location-format validated-location) ERR-INVALID-LOCATION-FORMAT)
        (asserts! (validate-health-metric-type validated-metric-type) ERR-INVALID-INPUT-DATA)
        
        (match (map-get? global-health-data-registry {geographic-location: validated-location, health-metric-type: validated-metric-type})
            existing-health-data-entry
            (let ((submitting-oracle (get submitting-oracle-address existing-health-data-entry)))
                
                ;; Update data verification status
                (map-set global-health-data-registry 
                    {geographic-location: validated-location, health-metric-type: validated-metric-type}
                    (merge existing-health-data-entry {verification-status: true})
                )
                
                ;; Update oracle performance metrics
                (update-oracle-submission-statistics submitting-oracle data-accuracy-confirmed)
                (calculate-updated-oracle-reputation submitting-oracle data-accuracy-confirmed)
                
                (ok true)
            )
            ERR-HEALTH-DATA-NOT-FOUND
        )
    )
)

(define-public (bulk-submit-health-data-entries 
    (health-data-batch (list 10 {geographic-location: (string-ascii 50), health-metric-type: (string-ascii 30), numerical-value: uint, data-timestamp: uint})))
    (begin
        (asserts! (is-oracle-authorized tx-sender) ERR-ORACLE-NOT-AUTHORIZED)
        (asserts! (>= (get-oracle-reputation-score tx-sender) minimum-oracle-reputation-threshold) ERR-INSUFFICIENT-ORACLE-REPUTATION)
        
        ;; Calculate and process total submission fees
        (let ((total-submission-fees (* (var-get oracle-submission-fee-amount) (len health-data-batch))))
            (try! (stx-transfer? total-submission-fees tx-sender (as-contract tx-sender)))
        )
        
        ;; Process each health data entry in the batch
        (fold process-individual-health-data-entry health-data-batch (ok true))
    )
)

(define-private (process-individual-health-data-entry 
    (health-data-entry {geographic-location: (string-ascii 50), health-metric-type: (string-ascii 30), numerical-value: uint, data-timestamp: uint})
    (batch-processing-result (response bool uint)))
    (if (is-ok batch-processing-result)
        (let ((location (get geographic-location health-data-entry))
              (metric-type (get health-metric-type health-data-entry))
              (value (get numerical-value health-data-entry))
              (timestamp (get data-timestamp health-data-entry)))
            
            ;; Validate individual entry requirements
            (if (and 
                (validate-geographic-location-format location)
                (validate-health-metric-type metric-type)
                (validate-numerical-value value)
                (validate-timestamp-requirements timestamp))
                (begin
                    ;; Store validated health data entry
                    (map-set global-health-data-registry 
                        {geographic-location: location, health-metric-type: metric-type}
                        {
                            numerical-value: value,
                            data-submission-timestamp: timestamp,
                            submitting-oracle-address: tx-sender,
                            verification-status: false,
                            blockchain-block-height: stacks-block-height,
                            data-source-confidence-score: (get-oracle-reputation-score tx-sender)
                        }
                    )
                    
                    ;; Update contract statistics
                    (var-set total-data-submissions (+ (var-get total-data-submissions) u1))
                    (ok true)
                )
                ERR-DATA-VALIDATION-FAILED
            )
        )
        batch-processing-result
    )
)

;; EMERGENCY MANAGEMENT FUNCTIONS - Crisis Response Capabilities

(define-public (emergency-health-data-update 
    (geographic-location (string-ascii 50))
    (health-metric-type (string-ascii 30))
    (emergency-value uint))
    (let ((current-blockchain-time (unwrap-panic (get-stacks-block-info? time stacks-block-height)))
          (validated-location geographic-location)
          (validated-metric-type health-metric-type)
          (validated-emergency-value emergency-value))
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-geographic-location-format validated-location) ERR-INVALID-LOCATION-FORMAT)
        (asserts! (validate-health-metric-type validated-metric-type) ERR-INVALID-INPUT-DATA)
        (asserts! (validate-numerical-value validated-emergency-value) ERR-INVALID-NUMERICAL-VALUE)
        
        ;; Store emergency health data with immediate verification using validated inputs
        (map-set global-health-data-registry 
            {geographic-location: validated-location, health-metric-type: validated-metric-type}
            {
                numerical-value: validated-emergency-value,
                data-submission-timestamp: current-blockchain-time,
                submitting-oracle-address: tx-sender,
                verification-status: true,
                blockchain-block-height: stacks-block-height,
                data-source-confidence-score: contract-owner-reputation-score
            }
        )
        
        (ok true)
    )
)

;; FINANCIAL MANAGEMENT FUNCTIONS - Contract Treasury Operations

(define-public (withdraw-accumulated-fees (withdrawal-amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (<= withdrawal-amount (stx-get-balance (as-contract tx-sender))) ERR-INVALID-INPUT-DATA)
        (as-contract (stx-transfer? withdrawal-amount tx-sender (var-get current-contract-administrator)))
    )
)

(define-public (transfer-contract-administration (new-administrator-address principal))
    (let ((validated-new-admin new-administrator-address))
        (asserts! (is-eq tx-sender (var-get current-contract-administrator)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-principal-address validated-new-admin) ERR-INVALID-PRINCIPAL)
        (asserts! (not (is-eq validated-new-admin (var-get current-contract-administrator))) ERR-INVALID-INPUT-DATA)
        
        (var-set current-contract-administrator validated-new-admin)
        (map-set authorized-health-data-oracles validated-new-admin true)
        (map-set oracle-reputation-scores validated-new-admin contract-owner-reputation-score)
        (ok true)
    )
)

;; ANALYTICS AND REPORTING FUNCTIONS - Data Intelligence Capabilities

(define-public (generate-regional-health-summary (geographic-locations-list (list 10 (string-ascii 50))))
    (ok (map compile-location-health-metrics geographic-locations-list))
)

(define-private (compile-location-health-metrics (geographic-location (string-ascii 50)))
    {
        geographic-location: geographic-location,
        confirmed-cases: (get-confirmed-case-count geographic-location),
        mortality-statistics: (get-mortality-statistics geographic-location),
        vaccination-coverage: (get-vaccination-coverage-rate geographic-location),
        hospitalization-metrics: (get-hospitalization-metrics geographic-location),
        data-freshness: (verify-data-freshness geographic-location "confirmed-cases")
    }
)

(define-read-only (calculate-regional-health-trends (geographic-location (string-ascii 50)))
    (let ((case-data (get-confirmed-case-count geographic-location))
          (mortality-data (get-mortality-statistics geographic-location))
          (vaccination-data (get-vaccination-coverage-rate geographic-location)))
        {
            location: geographic-location,
            case-mortality-ratio: (if (and (is-some case-data) (is-some mortality-data))
                (some (/ (* (unwrap-panic mortality-data) u100) (unwrap-panic case-data)))
                none),
            vaccination-status: vaccination-data,
            data-availability-score: (+ (if (is-some case-data) u1 u0)
                                      (+ (if (is-some mortality-data) u1 u0)
                                         (if (is-some vaccination-data) u1 u0)))
        }
    )
)