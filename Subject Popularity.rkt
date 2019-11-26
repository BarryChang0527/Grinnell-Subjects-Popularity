#lang racket
(require loudhum)
(require plot)

;;; Name:
;;;   data
;;; Purpose:
;;;   Compile all the files in /home/rebelsky/Desktop/SandB/ into a
;;;    list of directory strings.
;;; Produces:
;;;;  files, a list of strings

(define data
  (map (section string-append "/home/rebelsky/Desktop/SandB/" <>)
       (map path->string
            (directory-list "/home/rebelsky/Desktop/SandB/"))))

;;; Procedure:
;;;   file-to-string-by-year
;;; Parameters:
;;;   datum, a string
;;;   year, a natural number
;;; Purpose:
;;;   Convert a string url datum to a string, if it fits within the given
;;;    year between 1961-1970 in two-digit form (61).
;;; Produces:
;;;   string, a string
;;;OR false, a boolean


(define file-to-string-by-year
  (lambda (datum year)
    (if (regexp-match? (pregexp
                        (string-append "/home/rebelsky/Desktop/SandB/usiagrc_scb_19"
                                       (number->string year))) datum)
        (file->string datum)
        #f)))

;;; Name
;;;   texts-table
;;; Purpose:
;;;   Convert a string url to a string, sorting hash values into a list.
;;; Produces:
;;;;  Converted, a list of strings

(define texts-table
  (let ([years (map (section + 61 <>) (range 10))]
        [texts-table (make-hash)])
    (for-each (lambda (year)
                (hash-set! texts-table year
                           (filter-map
                            (section file-to-string-by-year <> year) data))) years)
    texts-table))

;;; Name
;;;   reference-hash
;;; Purpose:
;;;   Produces a hash table with regexp keywords for each subject used as
;;;    reference when determining the subject of words
;;; Produces:
;;;;  reference, a hash table

(define reference-hash
  (make-hash (list
              (cons 'other-humanities '("[Cc]lassics" "[Pp]hilosophy" "[Rr]eligio(n|(us))"
                                                      "[Ll]iterature" "[Ee]nglish"))
              
              (cons 'art '("[Aa]rt" "[Mm]usic*?" "[Tt]heat*?" "[Pp]aint" "[Dd]raw(ing)*?"
                                    "[Ss]culpture" "[Cc]oncert"))
              
              (cons 'social-sciences '("[Pp]olitic*?" "[Pp]sycholog*?" "[Hh]istor*?"
                                                      "[Aa]nthropolog*?" "[Ee]conomic*?"
                                                      "[Ss]ociolog*?"))
              
              (cons 'sciences '("[Mm]ath*?" "[Bb]iolog*?" "[Cc]hemi*" "[Ss]cience"
                                            "[Pp]hysics"))
              
              (cons 'languages '("[Cc]hinese" "[Jj]apanese" "[Ff]rench"
                                              "[Gg]erman" "[Rr]ussian" "[Ff]oreign"
                                              "[Ss]panish" "[Ll]anguage" "[Ll]inguistics")))))

;;; Name
;;;   (none)
;;; Purpose:
;;;   Adds required regexp tags
;;; Produces:
;;;  (nothing)

(for-each (lambda (key)
            (hash-set! reference-hash key (map (section string-append "[^A-Za-z]" <> "[^A-Za-z]")
                                               (hash-ref reference-hash key))))
          (hash-keys reference-hash))

;;; Procedure:
;;;   update-values!
;;; Parameters:
;;;   hash, a hash table
;;;   key, a string
;;;   value, a natural number
;;; Purpose:
;;;   given a hash, hash key, and value, updates the hash key of
;;;    the hash by adding the value to it.
;;; Produces:
;;;   (none)
(define update-values!
  (lambda (hash key value)
    (hash-set! hash key (+ (hash-ref hash key) value))))

;;; Procedure:
;;;   tally-matches
;;; Parameters:
;;;   str, a  string
;;;   match, a natural number
;;; Purpose:
;;;   tallies the number of matches of a match string with a
;;;    string of text, up to 5
;;; Produces:
;;;   counts, a natural number


(define tally-matches
  (lambda (str match)
    (min 5 (length (regexp-match* (pregexp match) str)))))


;;; Procedure:
;;;   file-to-string-by-year
;;; Parameters:
;;;   hash, a hash table
;;;   counts, a hash table
;;;   str, a string
;;; Purpose:
;;;   tallies the number of matches for each topic in a single string of text,
;;;    updating a hash table of counts
;;; Produces:
;;;   (none)


(define add-counts!
  (lambda (hash counts str)
    (for-each (lambda (topic)
                (update-values! counts topic (reduce + (map (lambda (match)
                                                              (tally-matches str match))
                                                            (hash-ref hash topic)))))
              (hash-keys hash))))

;;; Procedure:
;;;   count-occurences
;;; Parameters:
;;;   strings, a list of strings
;;; Purpose:
;;;   applies add-counts! to all the strings a list of strings,
;;;    creating a hash table logging the counts
;;; Produces:
;;;   counts, a hash table

(define count-occurences
  (lambda (strings)
    (let ([counts (make-hash (map (section cons <> 0) (hash-keys reference-hash)))])
      (for-each (lambda (str)
                  (add-counts! reference-hash counts str)) strings)
      counts)))

;;; Procedure:
;;;   create-periods
;;; Parameters:
;;;   sections, one or more two-item lists
;;; Purpose:
;;;   Convert one or more two-digit year pairs into a list of years
;;; Produces:
;;;   periods, a list of lists
;;; Preconditions:
;;;   Each value of sections must be:
;;;   * A pair, '(x . y) where x <= y
;;;   * '(x . y) represents the time period x-(y-1), where x and (y-1) are
;;;      natural numbers between 61 and 70
;;; Postconditions
;;;   * periods is a list of lists
;;;   * (= (length periods) [number of inputs in section])
;;;   * each element of period is the result (range x y) of a pair ('x . y)

(define create-periods
  (lambda sections 
    (cond [(null? sections)
           null]
          [else
           (cons (range (caar sections) (cadar sections))
                 (apply create-periods (cdr sections)))])))

;;; Procedure:
;;;   compile-occurences
;;; Parameters:
;;;   periods, a list of lists
;;; Purpose:
;;;   Tally the occurences of each subject within
;;;    each time period in a corresponding hash table
;;; Produces:
;;;   list-of-counts, a list of hash tables

(define compile-occurences
  (lambda (periods)
    (map (lambda (period)
           (count-occurences (reduce append (map (section hash-ref texts-table <>) period))))
         periods)))

;;; Procedure:
;;;   occurences->percents
;;; Parameters:
;;;   counts, a hash table
;;; Purpose:
;;;   Convert the tally counts of each key in a hash table to a percent of the total tallies
;;; Produces:
;;;   percents, a hash table

(define occurences->percents
  (lambda (counts)
    (let ([sum (apply + (map (section hash-ref counts <>) (hash-keys counts)))]
          [hash (hash-copy counts)]) ;so that counts isn't modified
      (for-each (lambda (key)
                  (hash-set! hash key (/ (hash-ref hash key) (* sum .01)))) (hash-keys hash))
      hash)))




;;; Name: scale
;;; Purpose: Reference scale to sort histogram vectors
;;; Type: hash table

(define scale
  (make-hash (list (cons 'social-sciences 0)
                   (cons 'sciences 1)
                   (cons 'other-humanities 2)
                   (cons 'languages 3)
                   (cons 'art 4))))

;;; Procedure:
;;;   in-order?
;;; Parameters:
;;;   vec1, a vector
;;;   vec2, a vector
;;;   scale, a hash table
;;; Purpose:
;;;   Determines whether one vector is greater than the other, according to a scale
;;; Produces:
;;;   status, a boolean
(define in-order?
  (lambda (vec1 vec2)
    (< (hash-ref scale (vector-ref vec1 0)) (hash-ref scale (vector-ref vec2 0)))))
  
;;; Procedure:
;;;   make-histogram
;;; Parameters:
;;;   period-string, a string
;;;   percents, a hash
;;;   num, a natural number
;;;   skip, an exact positive integer
;;; Purpose:
;;;   Creates a histogram object based on parameters
;;;    defined to create an interwoven plot.
;;; Produces:
;;;   histogram, a chart

(define make-histogram
  (lambda (period-string percents num skip)
    
    (discrete-histogram
     (sort (map (lambda (key)
                  (vector key (hash-ref percents key))) (hash-keys percents)) in-order?)
                 
     #:skip skip #:x-min (add1 num)
     #:label period-string #:color (add1 num) #:line-color (add1 num))))


;;; Procedure:
;;;   visualize
;;; Parameters:
;;;   period-strings, a list
;;;   list-of-percents, a list
;;;   index, a hash table
;;;   skip, a number
;;; Purpose:
;;;   Analyze text based on time periods then create an
;;;    interwoven chart representing the data
;;; Produces:
;;;   (none)

(define visualize
  (lambda (period-strings list-of-percents skip)

      
      (parameterize ([plot-x-tick-label-anchor 'top-right] ;taken from Racket documentation for plot
                     [plot-x-tick-label-angle 30])
        (plot (let rc ([num 0]
                       [remaining-percents list-of-percents]
                       [remaining-period-strings period-strings])
                
                (cond [(null? remaining-percents)
                       null]
                      [else
                       (cons (make-histogram (car remaining-period-strings)
                                             (car remaining-percents) num skip)
                             (rc (add1 num) (cdr remaining-percents) (cdr remaining-period-strings)))]))
              #:x-label "Subjects" #:y-label "%"
              
              ))))

;;; Procedure:
;;;   analyze-subject-trend
;;; Parameters:
;;;   period-strings, a list
;;;   list-of-percents, a list
;;;   subject, a symbol
;;;   sensitivity, a number
;;; Purpose:
;;;   Analyzes the trend of a given subject across a list of hash tables, displaying
;;;    a generated analysis
;;; Produces:
;;;   statement, a string

(define analyze-subject-trend
  (lambda (period-strings list-of-percents subject sensitivity)
    (let ([values (map (section hash-ref <> subject) list-of-percents)])

      (let rc ([remaining values]
               [flag #f]
               [i 0]
               [messages ""])
        
        (if (null? (cdr remaining))
            (string-append messages "\n")
            (let ([percent (/ (- (cadr remaining) (car remaining))
                              (car remaining))])
              (cond [(> (abs percent) sensitivity)
                     (rc (cdr remaining) #t (add1 i)
                         (string-append (string-append messages
                                                       (if (not flag)
                                                           (string-append (symbol->string subject) " experienced:\n")
                                                           "")
                                                       "a " (number->string (exact-round (* 100 percent))) "% change from "
                                                       (list-ref period-strings i) " to " (list-ref period-strings (add1 i))
                                                       " (" (number->string (list-ref values i)) "-"
                                                       (number->string (list-ref values (add1 i)))
                                                       ")\n")))]
                    [else
                     (rc (cdr remaining) flag (add1 i) messages)])))))))

;;; Procedure:
;;;   analyze-trends
;;; Parameters:
;;;   period-strings, a list
;;;   list-of-percents, a list
;;;   sensitivity, a number
;;; Purpose:
;;;   Analyzes the trend of all subjects across a list of hash tables, displaying
;;;    a generated analysis
;;; Produces:
;;;   (none)
(define analyze-trends
  (lambda (period-strings list-of-percents sensitivity)
    (let ([subjects (hash-keys (car list-of-percents))])
      (for-each (o display
                   (section analyze-subject-trend period-strings list-of-percents <> sensitivity))
                subjects))))




;;; Procedure:
;;;   analyze
;;; Parameters:
;;;   sections, a list of lists
;;;   sensitivity, a number
;;; Purpose:
;;;   Analyze text based on time periods then create an
;;;    interwoven chart representing the data, as well as text
;;;    analysis according to sensitivity, a decimal less than 1
;;; Produces:
;;;   (none)
    

(define analyze
  (lambda (sections sensitivity)
    (let* ([periods (apply create-periods sections)]
           [list-of-counts (compile-occurences periods)]           
           [period-strings (map (lambda (period)
                                  (string-append (number->string (car period)) "-"
                                                 (number->string (last period)))) periods)] ;provides x-axis names for the histogram
           [skip (* 1.25 (length list-of-counts))]  ;makes sure skip length allows for interwoven lists, see Racket plot documentation
           [list-of-percents (map occurences->percents list-of-counts)])


      ;visualization
      (display (visualize period-strings list-of-percents skip))
      (newline)
      (analyze-trends period-strings list-of-percents sensitivity)
      )))
