(in-package :resource-machine)

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

(define-generic-print partial-transaction)


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
;;   + commitment and nullifier-hash derivation
