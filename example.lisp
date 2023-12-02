(in-package :resource-machine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating test resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yes, it is in the transparent situation.
;; But in the shielded situation, the encryption of resources is
;; public. Users can fetch and decrypt.

;; public key is useful for concealing the private key when someone
;; sends you funds
(defun generate-resoruce-x ()
  "Randomly generate out a resource `*x*', making a fresh private key"
  (let ((private (generate-key-pair :ed25519)))
    (make-instance 'nullifyable-resouce
                   :resource (make-resource-x-resource
                              :quantity 1
                              :nonce 1
                              :npk (nullifier-key private))
                   :npr-key private)))

(defun generate-resoruce-y ()
  "Randomly generate out a resource `*y*', making a fresh private key"
  (let ((private (generate-key-pair :ed25519)))
    (make-instance 'nullifyable-resouce
                   :resource (make-resource-y-resource
                              :quantity 1
                              :nonce 1
                              :npk (nullifier-key private))
                   :npr-key private)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; resource-logic ---> x-for-y
;; an x is coming in a y is coming out

;; we have an x, we also have a y
;; we now call prove

;; we can potentially cut this, or keep this as resource x's keys
(defparameter *private* (generate-key-pair :ed25519))
(defparameter *public*
  (make-public-key :ed25519 :y (ed25519-public-key (ed25519-key-x *private*))))

;; swap x for y
(defun example-intent ()
  (let* ((resource-x (generate-resoruce-x))
         (private    (generate-key-pair :ed25519))
         (ephemeral-resource
           (make-resource-x-for-y (npk (resource resource-x))
                                  (kind (make-resource-y-resource))
                                  1
                                  :quantity 1
                                  :eph 1
                                  :npk (nullifier-key private))))
    (list (create-intent :commited (list ephemeral-resource)
                         :nullified (list resource-x)
                         :anchors (list 0)) ; figure out what I ought to put here
          (make-instance 'nullifyable-resouce
                         :npr-key private
                         :resource ephemeral-resource))))
