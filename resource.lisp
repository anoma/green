(defpackage :resource-machine
  (:shadow :@)
  (:use :common-lisp :serapeum :ironclad :intbytes :trivia))

(in-package :resource-machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *hash-functions*
    (let ((hash (make-hash-table)))
      hash))

  ;; generate out destructure bind for function,

  ;; also generate out a `make-',function-name-resource for easy construction

  ;; (map 'list #'gensym (range 0 2))

  (defun parse-predicate-arguments (argument-list)
    "parse the toplevel define-predicate arguments"
    (assert (<= 1 (length argument-list)))
    (let ((car (car argument-list)))
      (fill-arguments (and (listp car) (cdr car)) "PUBLIC" "PRIVATE")))

  (-> fill-arguments ((or cons atom) string string) (cons symbol (cons symbol null)))
  (defun fill-arguments (argument name1 name2)
    "Takes an argument which may be an atom or a list and returns a list"
    (let ((arg1 (gensym name1))
          (arg2 (gensym name2))
          (list-arg (if (listp argument) argument (list argument))))
      (destructuring-bind (&optional (a1 arg1) (a2 arg2)) list-arg
        (list a1 a2))))

  (defun parse-inner (argument-list)
    "parse the inner arguments for a predicate declaration

Some good examples look like this

RESOURCE-MACHINE> (parse-inner '((mode pub priv)
                                 :created (c-res commitments)
                                 :nullified (n-res nullifiers)
                                 :data (nullifier-key resource-wanted amount)))
((MODE COMMITMENTS NULLIFIERS)
 ((NULLIFIER-KEY RESOURCE-WANTED AMOUNT) C-RES N-RES))

RESOURCE-MACHINE> (parse-inner '((mode pub priv)
                                 :created (c-res commitments)
                                 :nullified n-res))
((MODE COMMITMENTS #:NULLIFIERS607) (#:DATA608 C-RES N-RES))

RESOURCE-MACHINE> (parse-inner '(mode))
((MODE #:COMMITMENTS610 #:NULLIFIERS612)
 (#:DATA613 #:RESOURCE609 #:RESOURCE611))
"
    (assert (<= 1 (length argument-list)))
    (destructuring-bind (mode &key nullified created data) argument-list
      (mvlet ((c-res commitments
                     (values-list (fill-arguments created "RESOURCE" "COMMITMENTS")))
              (n-res nullifiers
                     (values-list (fill-arguments nullified "RESOURCE" "NULLIFIERS"))))
        (list (list (if (listp mode) (car mode) mode)
                    commitments
                    nullifiers)
              (list (or data (gensym "DATA")) c-res n-res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicate (function-name (mode-pub-priv &key nullified created data) &body body)
  "We define out a predicate, this constitutes a few things:

If the function is named f, then we define the following
f: this is the prove function for the given predicate
*f*: the hash of the function
make-resource-f: creating the resource
"
  ;; reconstruct the argument list for better parsing
  (let* ((unparsed (list mode-pub-priv :nullified nullified
                                       :created created
                                       :data data))
         (top-argument-names (parse-predicate-arguments unparsed))
         (inner-argument-list (parse-inner unparsed))

         (hash-value (sxhash (list* unparsed body)))
         (make-fn    (intern (concat "MAKE-RESOURCE-" (symbol-name function-name))
                             (symbol-package function-name)))
         (data-list   (if (listp data) data (list data)))
         (result-name (gensym "RESULT"))

         ;; Ι know these by position in my inner-argument-list
         ;; basically 2nd and 3rd argument of the public arguments
         (commitments-variable (cadar inner-argument-list))
         (nullifier-variable   (caddar inner-argument-list))
         (public-arguments     (car top-argument-names)))
    `(prog1
         (defun ,function-name ,top-argument-names
           (destructuring-bind ,inner-argument-list
               (list ,@top-argument-names)
             (declare (ignorable ,@(alexandria:flatten inner-argument-list)))

             (let ((,result-name ,@body))
               (assert (typep ,result-name 'boolean))
               (make-instance
                'proof-output
                :commitments ,commitments-variable
                :nullifiers ,nullifier-variable
                :program (lambda (public-again)
                           (bool->int
                            (and (check-proof-correspodence ,public-arguments
                                                            public-again)
                                 ,result-name)))))))

       (defparameter ,(intern (concat "*" (symbol-name function-name) "*")
                              (symbol-package function-name))
         ,hash-value)

       (defun ,make-fn
           ,(append data-list
             `(&key (eph 0) (nonce 0) (npk 0) (rseed 0) (quantity 0) (label 0)))
         (make-instance 'resource :eph eph :nonce nonce :npk npk :rseed rseed
                                  :quantity quantity
                                  :label label
                                  :hash ,hash-value
                                  :data (list ,@data-list)))

       (setf (gethash ,hash-value *hash-functions*)
             (symbol-function ',function-name)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-predicate test-example ((mode))
    (= 0 mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Type declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (ty)
  `(or (cons ,ty (or null cons)) null))

(defclass resource ()
  (;; 𝔽ₗ
   (pred-hash      :accessor hash     :type integer       :initarg :hash :initform 0)
   ;; 𝔽ₗₐₑₗ
   (label          :accessor label    :type integer       :initarg :label :initform 0)
   ;; 𝔽Q
   (quantity       :accessor quantity :type integer       :initarg :quantity :initform 0)
   ;; {𝔽ᵥ}
   (data           :accessor data     :type (list-of integer) :initarg :data :initform nil)
   ;; 𝔽₂
   (ephemorability :accessor eph      :type (integer 0 1) :initarg :eph :initform 0)
   ;; 𝔽ₙₒₙₑ
   (nonce          :accessor nonce    :type integer       :initarg :nonce :initform 0)
   ;; 𝔽ₙₚₖ
   (nullifier-key  :accessor npk      :type integer       :initarg :npk :initform 0)
   ;; Fᵣₛₑₑ
   (random-seed    :accessor rseed    :type integer       :initarg :rseed :initform 0)))

(defclass partial-transaction ()
  (;; {𝔽ₐₙ}
   (merkle-roots :accessor ans   :type (list-of integer) :initarg :ans   :initform nil)
   ;; {𝔽ₘ}
   (commitments  :accessor cms   :type (list-of integer) :initarg :cms   :initform nil)
   ;; {𝔽ₙ}
   (nullifiers   :accessor nfs   :type (list-of integer) :initarg :nfs   :initform nil)
   ;; 𝔽ₚ
   (proof-record :accessor pr    :type list :initarg :pr    :initform nil)
   ;; 𝔽
   (delta        :accessor delta :type list :initarg :delta :initform nil)
   ;; {(𝔽ₖₑ, d)}
   (extra        :accessor extra :type (list-of integer) :initarg :extra :initform nil)
   ;; Φ
   (preference   :accessor pref  :type function          :initarg :pref :initform #'identity)))

;; A resource with a specified hash function
(defclass resource-hash (resource)
  ((hash-commitment :accessor hcm   :initform #'sxhash)
   (hash-nullifier  :accessor hnf   :initform #'sxhash)
   (hash-kind       :accessor hkind :initform #'sxhash)
   (hash-delta      :accessor hΔ    :initform #'sxhash)))

(defclass proof-output ()
  ((program     :accessor program :initarg :program :initform 0)
   (commitments :accessor commitments :initarg :commitments :initform 0)
   (nullifiers  :accessor nullifiers :initarg :nullifiers :initform 0)))


(defmacro define-generic-print (type)
  `(defmethod print-object ((obj ,type) stream)
     (pprint-logical-block (stream nil)
       (print-unreadable-object (obj stream :type t)
         (format stream "~2I~{~_~A~^ ~}" (to-list obj))))))

(define-generic-print resource)
(define-generic-print partial-transaction)
(define-generic-print proof-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prove and Verify functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> prove ((-> (list list) proof-output) list list) proof-output)
;; set of commitments and nullifiers
;; custom witnesses, what logics, I.E. for fib the value we wish to compute really
;; hash(resource) --> commitments

;; resources themselves are the private input (witnesses)
;;   + we also send circuit specific data here as well (this will be the first)
;; commitment and nullifiers are the public inputs (instances)
;; the output should have commitments that we pass around publicly
(defun prove (program instance witness)
  ;; create assertions on length for better error messages
  (values
   (funcall program instance witness)))

;; user creates: 1x --> 1y into the intent pool.
;;  - Special resource
;;    + set the ephermal flag to true
;;    + we don't need to check existence will consume it
;;    + it is not on the merkle tree (we can not create a valid merkle path)
;;    + User needs to create a proof of creating this intent (special resource)
;;       * don't need the resources the intent wants
;;       * input: resources that will be consumed (nullifiers)
;;       * output: resources that will be created (resource commits, added to merkle tree at end)
;;  - Can be created without enough resource
;;  - reality format: nock --> {special-resource, include_these_logics_into_transaction ....}
;; solver {1y --> 1x, 1x --> 1y}
;; solver will call prove
;;  - will consume the intent resource (and it's proof)
;;  - satisfy the intent


;; alice wants: x -> y, bob wants: y -> x
;; when x is consumed, create the ephemeral resource, that is the commitment that goes to x
;;   - solver is doing this
;;   - additional public input: nothing
;;   - additional private input: ephemeral resource (I can pass a kind) that confirms the counter increment
;; with the swap intent
;; total_number_of_swaps: +1
;; baked into the intent for x
;; resource_name: resorce_total_swaps(value: 10)
;; there must be another resource called increase_1
;; bond the two resource, every the swap intent is consumed

;; transaction is made that bob x, and alice y
;; resource_name: resorce_total_swaps(value: 11)

;; anchor check, check it existed in a previous computed at the end of
;; the last block. Or created in the partial transaction

;; 5 swaps
;; we have to get the correct values before hand


(-> verify (list list proof-output) boolean)
(defun verify (key instance proof)
  (declare (ignore key))
  (values
   (funcall (program proof) instance)))


;; garbage unfinished
(defun check-proof-correspondence (valid given)
  (every (lambda (x y)
           (and (= (commitment x) (commitment y))))
         valid
         given))

;; garbage unfinished
(-> check-proof-correspodence ((list-of integer) (list-of integer)) boolean)
(defun check-proof-correspodence (x y)
  (equalp x y))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computed data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric to-list (class)
  (:documentation "Turns a class into a struct")
  (:method ((object standard-object))
    (mapcar (lambda (x) (slot-value object x))
            (mapcar #'c2mop:slot-definition-name
                    (c2mop:compute-slots (class-of object))))))

;; in the null proof case we can just return the object itself, and
;; this would help the balance check
(defgeneric commitment (resource)
  (:documentation "A resource commitment key")
  (:method ((o standard-object))
    ;; We compute it by the whole object
    (sxhash (to-list o))))

(defgeneric address (resource)
  (:documentation "A resource address key")
  (:method ((o standard-object))
    (commitment o)))

(defgeneric nullifier (resource)
  (:documentation "A resource commitment key"))

(defgeneric kind (resource)
  (:documentation "The kind of resource"))

;; binding signature
;; sum(qᵢ₁ * kindᵢ₁ + rᵢ₁ * base_point … qᵢₙ * kindᵢₙ + rᵢₙ * base_point)
;; - sum(qⱼ₁ * kindⱼ₁ + rⱼ₁ * base_point … qⱼₙ * kindⱼₙ + rⱼₙ * base_point)
;; = r * base_point
(defgeneric delta (resource)
  (:documentation "Total quantity of kinds. This is the binding signature implementation. See "))

(defgeneric note-cm (resource)
  (:documentation "Total quantity of kinds"))

;; Overload it so any class that subclasses this gets the right
;; to-list value of a resource
(defmethod to-list ((r resource))
  (mapcar (lambda (x) (slot-value r x))
            (mapcar #'c2mop:slot-definition-name
                    (c2mop:compute-slots (find-class-safe 'resource)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data to be hashed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; || means cons
;; note_cm = poseidon_hash(l || label || v || npk || nonce || \psi || eph || q || rcm)
;; nf = poseidon_hash(nullifier_key || nonce || \psi || note_cm)
;; \psi and rcm are intermediate variables derived from rseed.
;; nullifier_key is the secret key of npk

(defun note-cm-data (r)
  (append (to-list r) (list (commitment r))))

(defun commitment-data (resource)
  (to-list resource))

;; specs says this should be the same as `commitment-data'
;; Xuyang would say otherwise
;; Make a request about this (forum post)
(defun nullifier-data (resource)
  (list (npk resource) (nonce resource) (rseed resource) (note-cm resource)))

(defun kind-data (resource)
  (list (hash resource) (label resource)))

(defun delta-data (resource)
  (list (kind resource) (quantity resource)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific versions for resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not needed but w/e
(defmethod commitment ((r resource))
  (sxhash (commitment-data r)))

(defmethod nullifier ((r resource))
  (sxhash (nullifier-data r)))

(defmethod kind ((r resource))
  (sxhash (kind-data r)))

(defmethod delta ((r resource))
  (sxhash (delta-data r)))

(defmethod note-cm ((r resource))
  (sxhash (note-cm-data r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific versions for resource-hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod commitment ((r resource-hash))
  (funcall (hcm r) (commitment-data r)))

(defmethod nullifier ((r resource-hash))
  (funcall (hnf r) (nullifier-data r)))

(defmethod kind ((r resource-hash))
  (funcall (hkind r) (kind-data r)))

(defmethod delta ((r resource-hash))
  (funcall (hΔ r) (delta-data r)))

(defmethod note-cm ((r resource-hash))
  (funcall (hcm r) (note-cm r)))

;;; PROVING SYSTEM
;; idk how to mock this exactly, but there is a rust api for when data
;; is in miden/lurk


;; Partial Transactions


;; j is output, i is input
;; sum(qᵢ * hash_to_point(label_i, logic_i)) : by definition
;; = sum(qᵢ * kind)
;; sum(qᵢ₁ * kindᵢ₁ … qᵢₙ * kindᵢₙ) = sum(qⱼ₁ * kindⱼ₁ … qⱼₙ * kindⱼₙ)
;; qᵢ₁ * kindᵢ₁ ≡ qⱼ₁ * kindⱼ₁

;; binding signature
;; sum(qᵢ₁ * kindᵢ₁ + rᵢ₁ * base_point … qᵢₙ * kindᵢₙ + rᵢₙ * base_point)
;; - sum(qⱼ₁ * kindⱼ₁ + rⱼ₁ * base_point … qⱼₙ * kindⱼₙ + rⱼₙ * base_point)
;; = r * base_point


;; q * kind + rseed * base_point

;; Compliance proof 5.2 point 3
;; check:
;;   + merkle check
;;   + binding signature (Basically how to achieve the balance proof)
;;   + commitment and nullifier derivation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crypto Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(-> nullifier-key (ed25519-private-key) integer)
(defun nullifier-key (key)
  (values (octets->uint (ed25519-key-x key) 32)))

(-> reconstruct-private (integer) ed25519-private-key)
(defun reconstruct-private (number)
  (values (make-private-key :ed25519 :x (int->octets number 32))))

(-> reconstruct-public (integer) ed25519-public-key)
(defun reconstruct-public (number)
  (values (make-public-key :ed25519 :y (int->octets number 32))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> bool->int (boolean) (integer 0 1))
(defun bool->int (bool)
  (if bool 1 0))

(defun nullified? (x)
  (zerop x))

(defun created? (x)
  (not (nullified? x)))

(-> resources ((list-of resource) &key (:of t) (:owned-by t)) (list-of resource))
(defun resources (resources &key of owned-by)
  "returns a resource list with the specified credentials

:of specifies the kinds that are accepted
:owned-by specifies who owns it"
  (remove-if (lambda (r)
               (or (and owned-by (not (= owned-by (npk r))))
                   (and of       (not (member (kind r) of :test #'=)))))
             resources))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compliance program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> compliance-proof ((list-of integer) (list-of resource)) proof-output)
(defun compliance-proof (instance witness)
  (declare (ignore instance witness))
  (values
   (make-instance 'proof-output)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the car of all private inputs is always the created or destroyed


;; Predicate format
;; in real protocol the lists can be separated via 0 before consume,
;; and 1 before a created
;; for private data we know how many arguments we expect
;; public: (created-commitments created-nullifiers)
;; created-commitments: (list 𝔽ₒₘₘᵢₜₛ)
;; created-nullifiers: (list 𝔽ₙᵤₗᵢₑᵣₛ)
;; private: (private-inputs* created consumed)
;; private-inputs: 𝔽
;; created: (list resource)
;; consumed: (list resource)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-predicate x-for-y ((mode pub priv)
                             :created (c-resources commitments)
                             :data (nullifier-key resource-wanted amount))
    ;; mode represents if it's created
    (and (or (created? mode)
             (<= amount
                 (~>> (resources c-resources
                                 :owned-by nullifier-key
                                 :of (list resource-wanted))
                      (mapcar #'quantity)
                      sum)))
         ;; need to check label as well
         ;;
         ;; further I need to ensure there is only
         ;; 1 of this kind in a transaction, or else I can steal
         t))

  ;; unfinished
  (define-predicate x-resource ((mode public private))
    (equalp t private))

  ;; unfinished
  (define-predicate y-resource ((mode public private))
    (equalp t private)))



;; resource-logic ---> x-for-y
;; an x is coming in a y is coming out

;; we have an x, we also have a y
;; we now call prove

(defparameter *special-label-x* 1)
(defparameter *special-label-y* 2)

;; we can potentially cut this, or keep this as resource x's keys
(defparameter *private* (generate-key-pair :ed25519))
(defparameter *public*
  (make-public-key :ed25519 :y (ed25519-public-key (ed25519-key-x *private*))))

;; yes, it is in the transparent situation.
;; But in the shielded situation, the encryption of resources is
;; public. Users can fetch and decrypt.

;; public key is useful for concealing the private key when someone
;; sends you funds
(defun generate-resoruce-x ()
  "Randomly generate out a resource `*x*', making a fresh private key"
  (let ((private (generate-key-pair :ed25519)))
    (make-instance 'resource
                   :quantity 1
                   :label *special-label-x*
                   :hash *x-resource*
                   :nonce 1
                   :npk (nullifier-key private))))

(defun generate-resoruce-y ()
  "Randomly generate out a resource `*y*', making a fresh private key"
  (let ((private (generate-key-pair :ed25519)))
    (make-instance 'resource
                   :quantity 1
                   :label *special-label-y*
                   :hash *y-resource*
                   :nonce 1
                   :npk (nullifier-key private))))

;; swap x for y
(defun example-intent ()
  (let* ((private            (generate-key-pair :ed25519))
         (resource-x         (generate-resoruce-x))
         ;; replace with make-x-for-y-resource
         (ephemeral-resource (make-resource-x-for-y
                              ;; TODO generate this
                              1 2 1
                              :quantity 1
                              :eph 1
                              :npk (nullifier-key private)))
         (public-data  (list (nullifier resource-x)
                             (commitment ephemeral-resource)))
         (private-data (list resource-x ephemeral-resource)))
    (list
     (make-instance 'partial-transaction
                    :cms (list (commitment ephemeral-resource))
                    :nfs (list (nullifier resource-x))
                    ;; we need the root for x, Ι don't need to pass
                    ;; in the fake root for ephemeral, because it's not
                    ;; being consumed
                    :delta (list (list *x-resource* -1) (list *x-for-y* 1))
                    :ans (list 0)
                    :pr (list
                         ;; prove the resource logic for x
                         (prove (gethash *x-resource* *hash-functions*)
                                ;; 0 for consumed
                                (cons 0 public-data)
                                (cons (data resource-x) private-data))
                         ;; compliance proof
                         ;; this ensures that we have ran the VPS
                         (prove #'compliance-proof public-data private-data)

                         (prove (gethash *x-for-y* *hash-functions*)
                                ;; 1 for creating
                                (cons 1 public-data)
                                (cons (data ephemeral-resource) private-data))))
     ;; remember to grab the `*x-for-y*' out of this
     private
     ephemeral-resource)))

(-> example-proof ((list-of resource) (list-of resource)) proof-output)
(defun example-proof (instance-1 witness)
  (let* ((only-arg (car instance-1))
         (result (and (= 5 (kind only-arg))
                      (> (quantity only-arg) 5))))
    (values
     (make-instance 'proof-output
                    :program
                    (lambda (instance-2)
                      (and (check-proof-correspodence instance-1 instance-2) result))
                    :nullifiers
                    (mapcar #'npk witness)
                    :commitments
                    ;; ??
                    nil))))
