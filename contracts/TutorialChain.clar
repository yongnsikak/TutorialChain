;; TutorialChain: Educational Tutorial and Learning Content Exchange Platform
;; Version: 1.0.0

(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-TUTORIAL-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-PUBLISHED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-LESSON-COUNT (err u5))
(define-constant ERR-INVALID-SUBJECT (err u6))
(define-constant ERR-INVALID-LEVEL (err u7))
(define-constant ERR-INVALID-TUTORIAL-TITLE (err u8))
(define-constant ERR-INVALID-CONTENT (err u9))

(define-constant MIN-LESSON-COUNT u1)

(define-data-var next-tutorial-id uint u1)

(define-map learning-repository
    uint
    {
        educator: principal,
        tutorial-title: (string-utf8 50),
        content: (string-utf8 200),
        subject: (string-utf8 15),
        level: (string-utf8 10),
        access-status: (string-utf8 15),
        lesson-count: uint
    })

(define-private (validate-subject (subject (string-utf8 15)))
    (or 
        (is-eq subject u"Mathematics")
        (is-eq subject u"Science")
        (is-eq subject u"Programming")
        (is-eq subject u"Language")
        (is-eq subject u"Arts")
        (is-eq subject u"Business")
    ))

(define-private (validate-level (level (string-utf8 10)))
    (or 
        (is-eq level u"Beginner")
        (is-eq level u"Basic")
        (is-eq level u"Intermediate")
        (is-eq level u"Advanced")
        (is-eq level u"Expert")
    ))

(define-private (validate-text-structure (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-public (publish-tutorial 
    (tutorial-title (string-utf8 50))
    (content (string-utf8 200))
    (subject (string-utf8 15))
    (level (string-utf8 10))
    (lesson-count uint))
    (let
        (
            (tutorial-id (var-get next-tutorial-id))
        )
        (asserts! (validate-text-structure tutorial-title u3 u50) ERR-INVALID-TUTORIAL-TITLE)
        (asserts! (validate-text-structure content u10 u200) ERR-INVALID-CONTENT)
        (asserts! (>= lesson-count MIN-LESSON-COUNT) ERR-INVALID-LESSON-COUNT)
        (asserts! (validate-subject subject) ERR-INVALID-SUBJECT)
        (asserts! (validate-level level) ERR-INVALID-LEVEL)
        
        (map-set learning-repository tutorial-id {
            educator: tx-sender,
            tutorial-title: tutorial-title,
            content: content,
            subject: subject,
            level: level,
            access-status: u"open",
            lesson-count: lesson-count
        })
        (var-set next-tutorial-id (+ tutorial-id u1))
        (ok tutorial-id)
    ))

(define-public (restrict-tutorial (tutorial-id uint))
    (let
        (
            (tutorial (unwrap! (map-get? learning-repository tutorial-id) ERR-TUTORIAL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get educator tutorial)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get access-status tutorial) u"open") ERR-INVALID-STATUS)
        (ok (map-set learning-repository tutorial-id (merge tutorial { access-status: u"restricted" })))
    ))

(define-read-only (get-tutorial (tutorial-id uint))
    (ok (map-get? learning-repository tutorial-id)))

(define-read-only (get-educator (tutorial-id uint))
    (ok (get educator (unwrap! (map-get? learning-repository tutorial-id) ERR-TUTORIAL-NOT-FOUND))))

(define-read-only (get-total-tutorials)
    (ok (- (var-get next-tutorial-id) u1)))

(define-read-only (get-access-status (tutorial-id uint))
    (ok (get access-status (unwrap! (map-get? learning-repository tutorial-id) ERR-TUTORIAL-NOT-FOUND))))