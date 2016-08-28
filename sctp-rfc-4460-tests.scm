;;; 
;;; Copyright (C) 2006, 2007 M. Tuexen tuexen@fh-muenster.de
;;; 
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the
;;; following conditions are met:
;;; 1. Redistributions of source code must retain the above
;;;    copyright notice, this list of conditions and the
;;;    following disclaimer.
;;; 2. Redistributions in binary form must reproduce the
;;;    above copyright notice, this list of conditions and
;;;    the following disclaimer in the documentation and/or
;;;    other materials provided with the distribution.
;;; 3. Neither the name of the project nor the names of
;;;    its contributors may be used to endorse or promote
;;;    products derived from this software without specific
;;;    prior written permission.
;;;  
;;; THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS
;;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
;;; OF SUCH DAMAGE.

;;;
;;; Coding convention:
;;; A test is implemented as a function. If the function
;;; #t, the tests was passed successfully. If anything else
;;; is returned, the test might be passed or not.
;;;


(define sut-addr (make-ipv4-address "192.168.1.189"))
(define sut-port 5001)
(define sut-closed-port (+ sut-port 1))
(define tester-addr (make-ipv4-address "192.168.1.17"))
(define tester-port 1234)
(define tester-is 1)
(define tester-os 1)
(define unknown-chunk-type-01 #x4f)

(define (vector-find vector predicate)
  (if (vector? vector)
      (do ((index 0 (+ index 1)))
	  ((or (= index (vector-length vector))
	       (predicate (vector-ref vector index)))
	   (if (= index (vector-length vector))
	       #f
	       (vector-ref vector index))))
      #f))

(define (vector-append v1 v2)
  (list->vector (append (vector->list v1)
			(vector->list v2))))
;;; (vector-append (vector 1 2) (vector 3 4))

(define (sctp-receive-chunk predicate)
  (let* ((result (sctp-receive 1000))
	 (chunks (cadr result)))
    (if (or (equal? result (list #f #f #f #f))
	    (vector-find chunks predicate))
	result
	(sctp-receive-chunk predicate))))


(define (associate peer-addr peer-port local-addr local-port)
  (let ((local-tag  1)
	(initial-tsn  0))
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk local-tag 1500 tester-os tester-is initial-tsn #()))
	       peer-addr)
    (let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	   (init-ack     (vector-ref (cadr answer) 0))
	   (parameters   (get-parameters init-ack))
	   (state-cookie (vector-find parameters cookie-parameter?))
	   (peer-tag     (get-initiate-tag init-ack))
	   (header       (make-common-header local-port peer-port peer-tag)))
      (sctp-send header 
		 (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		 peer-addr)
      (sctp-receive-chunk cookie-ack-chunk?)
      (list header local-tag peer-tag initial-tsn (get-initial-tsn init-ack)))))


;;;
;;; Section 2.1
;;;



;;;
;;; This SUT establishes an association and sends an unknown chunk with
;;; upper bits 01 and expects an ERROR chunk back with the correct error
;;; cause included.
;;;

(define (sctp-2-1-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (peer-tag (caddr answer))
	 (unknown-chunk (make-chunk unknown-chunk-type-01 0 #())))
    (sctp-send header
	       (vector unknown-chunk)
	       peer-addr)
    (let* ((result (sctp-receive-chunk error-chunk?))
	   (chunks (cadr result))
	   (error-chunk (vector-find chunks error-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (and error-chunk 
	   (equal? (get-causes error-chunk)
		   (vector (make-cause 6 (vector unknown-chunk-type-01 0 0 4))))))))

;;; (sctp-2-1-01 sut-addr sut-port tester-addr tester-port)



;;;
;;; Section 2,2 superseeded by 2.27, 2.32 and 2.42
;;;



;;;
;;; Section 2.3
;;;



;;;
;;; This test sends an INIT chunk with a not 32-bit aligned parameter   
;;; to the system under test. The final padding of the parameter is
;;; not included in the chunk length. The SUT MUST accept this packet.
;;;

(define (sctp-2-3-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 0))
	 (parameters (vector (make-supported-address-type-parameter (vector 5))))
	 (init-chunk (make-init-chunk 1          ;;; initiate tag
				      1500       ;;; a_rwnd
				      1          ;;; outbound streams
				      1          ;;; inbound streams
				      0          ;;; initial TSN
				      parameters ;;; Parameters
				      )))
    (sctp-send header (vector init-chunk) peer-addr))
  (not (equal? (sctp-receive-chunk init-ack-chunk?)
	       (list #f #f #f #f))))

;;; (sctp-2-3-01 sut-addr sut-port tester-addr tester-port)


;;;
;;; This test sends an INIT chunk with a not 32-bit aligned parameter   
;;; to the system under test. The final padding of the parameter is
;;; included in the chunk length. The SUT should accept this packet.
;;;

(define (sctp-2-3-02 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 0))
	 (parameters (vector (make-supported-address-type-parameter (vector 5))))
	 (init-chunk (make-init-chunk 1          ;;; initiate tag
				      1500       ;;; a_rwnd
				      1          ;;; outbound streams
				      1          ;;; inbound streams
				      0          ;;; initial TSN
				      parameters ;;; Parameters
				      ))
	 (wrong-init-chunk (make-chunk (get-chunk-type init-chunk)
				       (get-chunk-flags init-chunk)
				       (vector-append (get-chunk-data init-chunk)
						      (vector 0 0)))))
    (sctp-send header (vector wrong-init-chunk) peer-addr))
  (not (equal? (sctp-receive-chunk init-ack-chunk?)
	       (list #f #f #f #f))))

;;; (sctp-2-3-02 sut-addr sut-port tester-addr tester-port)

;;;
;;; FIXME: How to test the sending path of the SUT?
;;;


;;;
;;; Section 2.4 is not testable.
;;;


;;;
;;; Section 2.5
;;;


;;;
;;; This test tries to send an INIT with MIS smaller than
;;; the default OS of the SUT. The SUT MUST NOT return a
;;; OS larger than the provided MIS.
;;;



(define (sctp-2-5-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 0))
	 (init-chunk (make-init-chunk 1          ;;; initiate tag
				      1500       ;;; a_rwnd
				      #xffff     ;;; outbound streams
				      #xffff     ;;; inbound streams
				      0          ;;; initial TSN
				      (vector)   ;;; Parameters
				      )))
    (sctp-send header (vector init-chunk) peer-addr)
    (let* ((answer (sctp-receive-chunk init-ack-chunk?))
	   (init-ack-chunk  (vector-ref (cadr answer) 0))
	   (mos (get-mos init-ack-chunk)))
      (if (> mos 1)
	  (let ((new-init-chunk (make-init-chunk 1          ;;; initiate tag
						 1500       ;;; a_rwnd
						 #xffff     ;;; outbound streams
						 (- mos 1)  ;;; inbound streams
						 0          ;;; initial TSN
						 (vector)   ;;; Parameters
						 )))
	    (sctp-send header (vector new-init-chunk) peer-addr)
	    (let* ((answer (sctp-receive-chunk init-ack-chunk?))
		   (init-ack-chunk  (vector-ref (cadr answer) 0)))
	      (< (get-mos init-ack-chunk) mos)))
	  #f))))

;;; (sctp-2-5-01 sut-addr sut-port tester-addr tester-port)

;;;
;;; Section 2.11
;;;



;;;
;;; This test sends an INIT chunk bundled with an PAD chunk
;;; to a listening socket.
;;; The receiver MUST silently discard the packet.
;;;

(define (sctp-2-11-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 0))
	 (init-chunk (make-init-chunk 1         ;;; initiate tag
				      1500      ;;; a_rwnd
				      1         ;;; outbound streams
				      1         ;;; inbound streams
				      0         ;;; initial TSN
				      (vector)  ;;; Parameters
				      ))
	 (pad-chunk (make-chunk #x84 0 (vector))))
    (sctp-send header (vector init-chunk pad-chunk) peer-addr))
  (equal? (sctp-receive 1000) (list #f #f #f #f)))

;;; (sctp-2-11-01 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk bundled with an PAD chunk
;;; to a non-listening socket.
;;; The receiver MUST silently discard the packet.
;;;

(define sctp-2-11-02 sctp-2-11-01)

;;; (sctp-2-11-02 sut-addr sut-closed-port tester-addr tester-port)



;;;
;;; This test sends an PAD chunk bundled with an INIT chunk
;;; to a listening socket.
;;; The receiver MUST silently discard the packet.
;;;

(define (sctp-2-11-03 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 0))
	 (init-chunk (make-init-chunk 1         ;;; initiate tag
				      1500      ;;; a_rwnd
				      1         ;;; outbound streams
				      1         ;;; inbound streams
				      0         ;;; initial TSN
				      (vector)  ;;; Parameters
				      ))
	 (pad-chunk (make-chunk #x84 0 (vector))))
    (sctp-send header (vector pad-chunk init-chunk) peer-addr))
  (equal? (sctp-receive 1000) (list #f #f #f #f)))

;;; (sctp-2-11-03 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an PAD chunk bundled with an INIT chunk
;;; to a non-listening socket.
;;; The receiver MUST silently discard the packet.
;;;

(define sctp-2-11-04 sctp-2-11-03)

;;; (sctp-2-11-04 sut-addr sut-closed-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk with a non-zero verification tag
;;; to a listening socket.
;;; The receiver MUST silently discard the INIT chunk.
;;;

(define (sctp-2-11-05 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 2))
	 (init-chunk (make-init-chunk 1         ;;; initiate tag
				      1500      ;;; a_rwnd
				      1         ;;; outbound streams
				      1         ;;; inbound streams
				      0         ;;; initial TSN
				      (vector)  ;;; Parameters
				      )))
    (sctp-send header (vector init-chunk) peer-addr))
  (equal? (sctp-receive 1000) (list #f #f #f #f)))

;;; (sctp-2-11-05 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk with a non-zero verification tag
;;; to a non-listening socket.
;;; The receiver MUST silently discard the INIT chunk.
;;;

(define sctp-2-11-06 sctp-2-11-05)

;;; (sctp-2-11-06 sut-addr sut-closed-port tester-addr tester-port)



;;;
;;; Section 2.13 superseeded by Section 2.41
;;;



;;;
;;; Section 2.40
;;;



;;;
;;; This tests send an INIT chunk from port 0 to the listening
;;; port of the SUT. It is expected that neither an ABORT nor
;;; an INIT-ACK is returned.
;;;

(define (sctp-2-40-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header 0 peer-port 0))
	 (init-chunk (make-init-chunk 1         ;;; initiate tag
				      1500      ;;; a_rwnd
				      1         ;;; outbound streams
				      1         ;;; inbound streams
				      0         ;;; initial TSN
				      (vector)  ;;; Parameters
				      )))
    (sctp-send header (vector init-chunk) peer-addr))
  (equal? (sctp-receive 1000) (list #f #f #f #f)))

;;; (sctp-2-40-01 sut-addr sut-port tester-addr tester-port)



;;;
;;; This tests send an INIT chunk to port 0 to of the SUT.
;;;  It is expected that an ABORT is not returned.
;;;

(define (sctp-2-40-02 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port 0 0))
	 (init-chunk (make-init-chunk 1         ;;; initiate tag
				      1500      ;;; a_rwnd
				      1         ;;; outbound streams
				      1         ;;; inbound streams
				      0         ;;; initial TSN
				      (vector)  ;;; Parameters
				      )))
    (sctp-send header (vector init-chunk) peer-addr))
  (equal? (sctp-receive 1000) (list #f #f #f #f)))

;;; (sctp-2-40-02 sut-addr sut-port tester-addr tester-port)



;;;
;;; Section 2.41
;;;



;;;
;;; This test sends an INIT chunk to a non-listening
;;; socket and expects back an ABORT chunk using the
;;; initiate tag as the verification tag and the T-bit
;;; not set.
;;;

(define (sctp-2-41-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((initiate-tag 1))
    (let* ((header (make-common-header local-port peer-port 0))
	   (init-chunk (make-init-chunk initiate-tag ;;; initiate tag
					1500         ;;; a_rwnd
					1            ;;; outbound streams
					1            ;;; inbound streams
					0            ;;; initial TSN
					(vector)     ;;; Parameters
					)))
    (sctp-send header (vector init-chunk) peer-addr))
  (let* ((answer (sctp-receive-chunk abort-chunk?))
	 (header (car answer))
	 (chunk  (vector-ref (cadr answer) 0)))
    (and (eqv? initiate-tag (get-verification-tag header))
	 (eqv? (get-t-bit chunk) #f)))))

;;; (sctp-2-41-01 sut-addr sut-closed-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk to a listening
;;; socket and expects back an ABORT chunk using the
;;; initiate tag as the verification tag and the T-bit
;;; not set because the a_rwnd is too small.
;;;

(define (sctp-2-41-02 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((initiate-tag 1))
    (let* ((header (make-common-header local-port peer-port 0))
	   (init-chunk (make-init-chunk initiate-tag ;;; initiate tag
					0            ;;; a_rwnd
					1            ;;; outbound streams
					1            ;;; inbound streams
					0            ;;; initial TSN
					(vector)     ;;; Parameters
					)))
    (sctp-send header (vector init-chunk) peer-addr))
  (let* ((answer (sctp-receive-chunk abort-chunk?))
	 (header (car answer))
	 (chunk  (vector-ref (cadr answer) 0)))
    (and (eqv? initiate-tag (get-verification-tag header))
	 (eqv? (get-t-bit chunk) #f)))))

;;; (sctp-2-41-02 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk to a listening
;;; socket and expects back an ABORT chunk using the
;;; initiate tag as the verification tag and the T-bit
;;; not set because the number of outbound streams is
;;; too small.
;;;

(define (sctp-2-41-03 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((initiate-tag 1))
    (let* ((header (make-common-header local-port peer-port 0))
	   (init-chunk (make-init-chunk initiate-tag ;;; initiate tag
					1500         ;;; a_rwnd
					0            ;;; outbound streams
					1            ;;; inbound streams
					0            ;;; initial TSN
					(vector)     ;;; Parameters
					)))
    (sctp-send header (vector init-chunk) peer-addr))
  (let* ((answer (sctp-receive-chunk abort-chunk?))
	 (header (car answer))
	 (chunk  (vector-ref (cadr answer) 0)))
    (and (eqv? initiate-tag (get-verification-tag header))
	 (eqv? (get-t-bit chunk) #f)))))

;;; (sctp-2-41-03 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk to a listening
;;; socket and expects back an ABORT chunk using the
;;; initiate tag as the verification tag and the T-bit
;;; not set because the number of inbound streams is
;;; too small.
;;;

(define (sctp-2-41-04 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((initiate-tag 1))
    (let* ((header (make-common-header local-port peer-port 0))
	   (init-chunk (make-init-chunk initiate-tag ;;; initiate tag
					1500         ;;; a_rwnd
					1            ;;; outbound streams
					0            ;;; inbound streams
					0            ;;; initial TSN
					(vector)     ;;; Parameters
					)))
    (sctp-send header (vector init-chunk) peer-addr))
  (let* ((answer (sctp-receive-chunk abort-chunk?))
	 (header (car answer))
	 (chunk  (vector-ref (cadr answer) 0)))
    (and (eqv? initiate-tag (get-verification-tag header))
	 (eqv? (get-t-bit chunk) #f)))))

;;; (sctp-2-41-04 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an INIT chunk to a listening
;;; socket and expects back an ABORT chunk using the
;;; initiate tag as the verification tag and the T-bit
;;; not set because the initiate tag is zero.
;;;

(define (sctp-2-41-05 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((initiate-tag 0))
    (let* ((header (make-common-header local-port peer-port 0))
	   (init-chunk (make-init-chunk initiate-tag ;;; initiate tag
					1500         ;;; a_rwnd
					1            ;;; outbound streams
					0            ;;; inbound streams
					0            ;;; initial TSN
					(vector)     ;;; Parameters
					)))
    (sctp-send header (vector init-chunk) peer-addr))
  (let* ((answer (sctp-receive-chunk abort-chunk?))
	 (header (car answer))
	 (chunk  (vector-ref (cadr answer) 0)))
    (and (eqv? initiate-tag (get-verification-tag header))
	 (eqv? (get-t-bit chunk) #f)))))

;;; (sctp-2-41-05 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test sends an OOTB SHUTDOWN-ACK chunk and expects
;;; back an ABORT chunk using the verification tag of the
;;; OOTB packet.
;;;

(define (sctp-2-41-06 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((verification-tag 1))
    (let* ((header (make-common-header local-port peer-port verification-tag))
	   (shutdown-ack-chunk (make-shutdown-ack-chunk)))
      (sctp-send header (vector shutdown-ack-chunk) peer-addr))
    (let* ((answer (sctp-receive-chunk shutdown-complete-chunk?))
	   (header (car answer))
	   (chunk  (vector-ref (cadr answer) 0)))
      (and (eqv? verification-tag (get-verification-tag header))
	   (eqv? (get-t-bit chunk) #t)))))

;;; (sctp-2-41-06 sut-addr sut-port tester-addr tester-port)



;;; This test sends an OOTB HEARTBEAT chunk and expects
;;; back an ABORT chunk using the verification tag of the
;;; OOTB packet.
;;;

(define (sctp-2-41-07 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let ((verification-tag 1))
    (let* ((header (make-common-header local-port peer-port verification-tag))
	   (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector)))))
      (sctp-send header (vector heartbeat-chunk) peer-addr))
    (let* ((answer (sctp-receive-chunk abort-chunk?))
	   (header (car answer))
	   (chunk  (vector-ref (cadr answer) 0)))
      (and (eqv? verification-tag (get-verification-tag header))
	   (eqv? (get-t-bit chunk) #t)))))

;;; (sctp-2-41-07 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends an ABORT
;;; with the expected verification and the T-bit set. It
;;; is expected that the peer silently discards the packet. 
;;; 

(define (sctp-2-41-08 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (peer-tag (caddr answer))
	 (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector)))))
    (sctp-send (make-common-header local-port peer-port peer-tag)
	       (vector (make-abort-chunk #t))
	       peer-addr) 
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive-chunk heartbeat-ack-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (not (equal? result (list #f #f #f #f))))))

;;; (sctp-2-41-08 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends an ABORT
;;; with the unexpected verification and the T-bit not set. It
;;; is expected that the peer silently discards the packet. 
;;; 

(define (sctp-2-41-09 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (local-tag (cadr answer))
	 (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector)))))
    (sctp-send (make-common-header local-port peer-port local-tag)
	       (vector (make-abort-chunk #f))
	       peer-addr) 
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive-chunk heartbeat-ack-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (not (equal? result (list #f #f #f #f))))))

;;; (sctp-2-41-09 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends an ABORT
;;; with the unexpected verification and the T-bit set. It
;;; is expected that the peer removes the TCB. 
;;; 

(define (sctp-2-41-10 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (local-tag (cadr answer))
	 (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector)))))
    (sctp-send (make-common-header local-port peer-port local-tag)
	       (vector (make-abort-chunk #t))
	       peer-addr) 
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive-chunk abort-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (not (equal? result (list #f #f #f #f))))))

;;; (sctp-2-41-10 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends an ABORT
;;; with the expected verification and the T-bit not set. It
;;; is expected that the peer removes the TCB. 
;;; 

(define (sctp-2-41-11 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (peer-tag (caddr answer))
	 (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector)))))
    (sctp-send (make-common-header local-port peer-port peer-tag)
	       (vector (make-abort-chunk #f))
	       peer-addr) 
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive-chunk abort-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (not (equal? result (list #f #f #f #f))))))

;;; (sctp-2-41-11 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends a SHUTODWN.
;;; On recption of the SHUTDOWN-ACK it sends a SHUTDOWN-COMPLETE
;;; with the expected verification tag and the T-bit set.
;;; It is expected that the peer silently discards the
;;; SHUTDOWN-COMPLETE and retransmits the SHUTDOWN-ACK.
;;; 

(define (sctp-2-41-12 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (peer-tag (caddr answer))
	 (peer-initial-tsn (cadddr (cdr answer)))
	 (shutdown-chunk (make-shutdown-chunk (1-mod32 peer-initial-tsn))))
    (sctp-send header
	       (vector shutdown-chunk)
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send (make-common-header local-port peer-port peer-tag)
	       (vector (make-shutdown-complete-chunk #t))
	       peer-addr)
    (let ((result (sctp-receive-chunk shutdown-ack-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (not (equal? result (list #f #f #f #f))))))

;;; (sctp-2-41-12 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends a SHUTODWN.
;;; On recption of the SHUTDOWN-ACK it sends a SHUTDOWN-COMPLETE
;;; with the unexpected verification tag and the T-bit not set.
;;; It is expected that the peer silently discards the
;;; SHUTDOWN-COMPLETE and retransmits the SHUTDOWN-ACK.
;;; 

(define (sctp-2-41-13 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (local-tag (cadr answer))
	 (peer-initial-tsn (cadddr (cdr answer)))
	 (shutdown-chunk (make-shutdown-chunk (1-mod32 peer-initial-tsn))))
    (sctp-send header
	       (vector shutdown-chunk)
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send (make-common-header local-port peer-port local-tag)
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    (let ((result (sctp-receive-chunk shutdown-ack-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (not (equal? result (list #f #f #f #f))))))

;;; (sctp-2-41-13 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends a SHUTODWN.
;;; On recption of the SHUTDOWN-ACK it sends a SHUTDOWN-COMPLETE
;;; with the expected verification tag and the T-bit not set.
;;; It is expected that the peer removes the TCB.
;;; 

(define (sctp-2-41-14 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (peer-tag (caddr answer))
	 (peer-initial-tsn (cadddr (cdr answer)))
	 (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector))))
	 (shutdown-chunk (make-shutdown-chunk (1-mod32 peer-initial-tsn))))
    (sctp-send header
	       (vector shutdown-chunk)
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send (make-common-header local-port peer-port peer-tag)
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive-chunk abort-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (and (not (equal? result (list #f #f #f #f)))
	   (equal? (get-t-bit (vector-ref (cadr result) 0)) #t)))))

;;; (sctp-2-41-14 sut-addr sut-port tester-addr tester-port)



;;;
;;; This test establishes an association and sends a SHUTODWN.
;;; On recption of the SHUTDOWN-ACK it sends a SHUTDOWN-COMPLETE
;;; with the unexpected verification tag and the T-bit set.
;;; It is expected that the peer removes the TCB.
;;; 

(define (sctp-2-41-15 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((answer (associate peer-addr peer-port local-addr local-port))
	 (header (car answer))
	 (local-tag (cadr answer))
	 (peer-initial-tsn (cadddr (cdr answer)))
	 (heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (vector))))
	 (shutdown-chunk (make-shutdown-chunk (1-mod32 peer-initial-tsn))))
    (sctp-send header
	       (vector shutdown-chunk)
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send (make-common-header local-port peer-port local-tag)
	       (vector (make-shutdown-complete-chunk #t))
	       peer-addr)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive-chunk abort-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (and (not (equal? result (list #f #f #f #f)))
	   (equal? (get-t-bit (vector-ref (cadr result) 0)) #t)))))

;;; (sctp-2-41-15 sut-addr sut-port tester-addr tester-port)



;;;
;;; Section 2.42
;;;

;;;
;;; This test sends an INIT chunk to a non-listening
;;; socket and expects back an ABORT chunk using the
;;; initiate tag as the verification tag and the T-bit
;;; not set.
;;;

(define (sctp-2-42-01 peer-addr peer-port local-addr local-port)
  (sctp-cleanup)
  (let* ((header (make-common-header local-port peer-port 0))
	 (parameter (make-parameter))
	 (init-chunk (make-init-chunk 1                  ;;; initiate tag
				      1500               ;;; a_rwnd
				      1                  ;;; outbound streams
				      1                  ;;; inbound streams
				      0                  ;;; initial TSN
				      (vector parameter) ;;; Parameters
				      )))
    (sctp-send header (vector init-chunk) peer-addr))
  (let* ((answer (sctp-receive-chunk init-ack-chunk?)))
    (equal? answer (list #f #f #f #f))))

;;; (sctp-2-42-01 sut-addr sut-closed-port tester-addr tester-port)
