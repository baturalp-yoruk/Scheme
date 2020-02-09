#lang scheme
;2015400036

;At first, finds the list which contains the input and then returns that list's third element which is the connected cities list
(define (RAILWAY-CONNECTION y) (if (equal? (assoc y LOCATIONS) #f) '() (car(cdr(cdr(assoc y LOCATIONS)))) ) )

;At first, finds the list which contains the input and then returns that list's second element which is the accommodation cost
(define (ACCOMMODATION-COST y) (if (equal? (assoc y LOCATIONS) #f) '0 (car(cdr(assoc y LOCATIONS))) ) )

;At first, finds the list which contains the input and then returns that list's second element which is the interested cities list
(define (INTERESTED-CITIES y) (if (equal? (assoc y TRAVELERS) #f) '() (car(cdr(assoc y TRAVELERS))) ) )

;At first, finds the list which contains the input and then returns that list's third element which is the interested activities list
(define (INTERESTED-ACTIVITIES y) (if (equal? (assoc y TRAVELERS) #f) '() (car(cdr(cdr(assoc y TRAVELERS)))) ) )

;At first, finds the list which contains the input and then returns that list's last element which is the hometown city of the input
(define (HOME y) (if (equal? (assoc y TRAVELERS) #f) '() (car(cdr(cdr(cdr(assoc y TRAVELERS))))) ) )

;This helper function takes 1 argument and returns true if this is an atom, false otherwise
(define (atom? x) (not (or (pair? x) (null? x))))

;This function does the almost all of the necessary things done. It recursively calls itself until it finds the desired result.
(define (base_TRAVELER-FROM x y)
  (cond
    ((null? y) '())
    ((equal? (last (car y)) x) (cons (first (car y)) (base_TRAVELER-FROM x (cdr y))) )
    (else (base_TRAVELER-FROM x (cdr y)))
    
    )
  )

;This function simply call the upper function with TRAVELERS. So it decreases the input number from 2 to 1.
(define (TRAVELER-FROM y) (base_TRAVELER-FROM y TRAVELERS))

;This function does the almost all of the necessary things done. It recursively calls itself until it finds the desired result.
(define (base_INTERESTED-IN-CITY x y)
  (cond
    ((null? y) '())
    ((equal? (not (member x (second (car y)))) #f) (cons (first (car y)) (base_INTERESTED-IN-CITY x (cdr y))) )
    (else (base_INTERESTED-IN-CITY x (cdr y)))
  )
)

;This function simply call the upper function with TRAVELERS. So it decreases the input number from 2 to 1.
(define (INTERESTED-IN-CITY y) (base_INTERESTED-IN-CITY y TRAVELERS))

;This function does the almost all of the necessary things done. It recursively calls itself until it finds the desired result.
(define (base_INTERESTED-IN-ACTIVITY x y)
  (cond
    ((null? y) '())
    ((equal? (not (member x (third (car y)))) #f) (cons (first (car y)) (base_INTERESTED-IN-ACTIVITY x (cdr y))) )
    (else (base_INTERESTED-IN-ACTIVITY x (cdr y)))
  )
)

;This function simply call the upper function with TRAVELERS. So it decreases the input number from 2 to 1.
(define (INTERESTED-IN-ACTIVITY y) (base_INTERESTED-IN-ACTIVITY y TRAVELERS))

;Takes a city and LOCATIONS as arguments and returns connected cities list which connected with railway.
(define (loll x y)
  (cond
    ((null? y) '())
    ((equal? x (first (car y))) (third (car y)))
    (else (loll x (cdr y))) 
  )
)

;Calls the upper function by it's results again and again until no element left.
(define (lolz l)
  (cond
    ((empty? l) '())
    (#t (append (loll (car l) LOCATIONS) (lolz (cdr l))))))

;This function finds all the connected cities by going deeper and deeper, and returns a huge list with some duplicates. Will remove duplicates at next function. 
(define (together x)
  (append (append (loll x LOCATIONS) (lolz (loll x LOCATIONS))) (lolz (lolz (loll x LOCATIONS)))))

;This simply removes the duplicates of the upper function's list and returns the desired list.
(define (RAILWAY-NETWORK x)
  (remove x (remove-duplicates (together x))))

;Takes a person, a city and a list as arguments. Returns true if that city hosts an activity which our person loves. False otherwise.
(define (etkinlik m n z)
  (cond
    ((null? z) #f)
    ((null? (third (assoc m TRAVELERS))) #f)
    ((null? (fourth (assoc n LOCATIONS))) #f)
    (else (or (not (equal? (member (car z) (fourth (assoc n LOCATIONS))) #f)) (etkinlik m n (cdr z))))))

;This function simply call the upper function with a list which contains our person's favorite activities.
(define (etkinlik2 m n)
  (cond
    ((equal? (assoc m TRAVELERS) #f) #f)
    ((equal? (assoc n LOCATIONS) #f) #f)
   (else (etkinlik m n (third (assoc m TRAVELERS))))))

;This function takes a person and a city as arguments and returns 0 if this city is the hometown of our person,
;Else if that city hosts an activity which our person likes, then multiplies the accommodation expense with 3,
;Else simply returns that city's accommodation cost.
(define (ACCOMMODATION-EXPENSES m n)
  (cond
    ((equal? (assoc m TRAVELERS) #f) '())
    ((not(equal? (assoc m TRAVELERS) #f)) (cond
                                            ((equal? (last (assoc m TRAVELERS)) n) 0)
                                            ((etkinlik2 m n) (* 3 (second (assoc n LOCATIONS)))  )
                                            (else (if (not (equal? (assoc n LOCATIONS) #f))  (second (assoc n LOCATIONS)) '() ))))))

;This function simply looks whether given cities are connected or not. If they are, returns 100; else returns 200.
(define (TRAVEL-EXPENSES m n)
  (cond
    ((equal? (assoc m TRAVELERS) #f) '())
    ((not(equal? (assoc m TRAVELERS) #f)) (cond
                                            ((equal? (last (assoc m TRAVELERS)) n) 0)
                                            ((not (equal? (member n (RAILWAY-NETWORK (last (assoc m TRAVELERS)))) #f)) 100)
                                            (else 200)))))

;It simply sums the Travel Expenses with the Accommodation Expenses.
(define (EXPENSES m n)
  (cond
    ((equal? (assoc m TRAVELERS) #f) '())
    ((equal? (assoc n LOCATIONS) #f) '())
    (else (+ (ACCOMMODATION-EXPENSES m n) (TRAVEL-EXPENSES m n) ))))

;This function looks all of the expenses and determines whether they are in-between the given numbers or not. If it is, then those cities appended to list.
(define (helper-IN-BETWEEN m n z)
  (cond
    ((null? z) '())
    ((and (equal? (>= n (second (car z))) #t) (equal? (<= m (second (car z))) #t) ) (append (list (first (car z))) (helper-IN-BETWEEN m n (cdr z))))
    (else (helper-IN-BETWEEN m n (cdr z)))))

;This function simply calls the upper function with LOCATIONS. So it decreases the input number from 3 to 2.
(define (IN-BETWEEN m n)
  (helper-IN-BETWEEN m n LOCATIONS))
