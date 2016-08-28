;;; 
;;; Copyright (C) 2003, 2004, 2005 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: sctp-imh-tests.scm,v 1.6 2010/03/02 12:08:22 tuexen Exp $


;;; Version 1.0

;;;
;;; Invalid Message Handling (IMH)
;;;


(define (sctp-imh-i-3-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (begin
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-chunk 1 0 (vector 1)))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (abort-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-imh-i-3-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not send an INIT-ACK
;;; and sends possibly an ABORT.



(define (sctp-imh-i-3-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((tcb (sctp-receive-chunk init-chunk?))
	     (common-header (get-header tcb))
	     (local-port (get-destination-port common-header))
	     (peer-port (get-source-port common-header))
	     (init-chunk (vector-ref (cadr tcb) 0)))
	(sctp-send (make-common-header local-port peer-port (get-initiate-tag init-chunk))
		   (vector (make-chunk 2 0 (vector 1 2 3 4 5)))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (or (init-chunk? chunk)
			(abort-chunk? chunk))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client
;;; (sctp-imh-i-3-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not send a COOKIE-ECHO
;;; chunk and possibly sends an ABORT.



(define (sctp-imh-i-3-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  ;; Send the COOKIE-ECHO with wrong V-tag.
	  (sctp-send (make-common-header local-port peer-port (1+mod32 (get-verification-tag header))) 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (let ((result (sctp-receive 1000)))
	    (sctp-send header
		       (vector (make-abort-chunk #f))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		stt-test-result-failed))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-imh-i-3-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the peer returns no packet in response to the COOKIE-ECHO.



(define (sctp-imh-i-3-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send-without-crc32c (make-common-header local-port peer-port 0)
				  (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
				  peer-addr)
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (pktdrop-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-imh-i-3-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if there is no INIT-ACK sent by the SUT
;;; or a PKTDROP report is sent.



(define (sctp-imh-i-3-5 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (random (expt 2 32)))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag))
	       (cookie       (get-cookie-parameter-cookie state-cookie)))
	  ;; Send the COOKIE-ECHO with modified cookie.
	  (vector-set! cookie 2 (modulo (+ (vector-ref cookie 2) 1) 256))
	  (sctp-send header
		     (vector (make-cookie-echo-chunk cookie))
		     peer-addr)
	  (let ((result (sctp-receive 1000)))
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		stt-test-result-failed))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;;(sctp-imh-i-3-5 sut-is=server sut-addr tester-port sut-port)
;;; The test is passed if the SUT returns no packet in response to the COOKIE-ECHO.



(define (sctp-imh-i-3-6 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  ;; delay the sending of the cookie echo.
	  (sleep tester-long-wait)
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (sctp-receive-chunk error-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-imh-i-3-6 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the COOKIE-ECHO is responded with
;;; an ERROR chunk.



(define (sctp-imh-i-3-7 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (local-port (get-source-port header)))
    ;; Sending an ABORT chunk with incorrect vtag.
    (sctp-send (make-common-header local-port peer-port (1+mod32 (get-verification-tag header))) 
	       (vector (make-abort-chunk #f))
	       peer-addr)
    ;; send a HEARTBEAT to see if the peer has removed the TCB.
    (sctp-send header 
	       (vector (make-heartbeat-chunk (make-heartbeat-parameter (make-ascii-bytes "Test"))))
	       peer-addr)
    (sctp-receive-chunk heartbeat-ack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-imh-i-3-7 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an HEARTBEAT-ACK is returned.



(define (sctp-imh-i-3-8 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (begin
	(sctp-send-raw (make-common-header local-port peer-port 0)
		       (vector 01 00 00 255 ;; tag=1; flags=0; length=255
			       00 00 00 01  ;; initiate_tag = 1
			       00 00 05 220 ;; a_rwnd = 1500
			       00 02 00 02  ;; os = 2; mis = 2
			       00 00 00 00) ;; initial_tsn = 0
		       peer-addr)
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (abort-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-imh-i-3-8 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if there is no INIT-ACK sent by the peer.



(define (sctp-imh-i-3-9 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    ;; Send a DATA chunk to the echo server.
    (if (not (= peer-port 13))
	(sctp-send header 
		   (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		   peer-addr))
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Acknowledge it.
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 0 #() #()))
	       peer-addr)
    (sctp-receive-chunk shutdown-chunk?)
    ;; send the SHUTDOWN-ACK with wrong v-tag.
    (sctp-send (make-common-header local-port peer-port (1+mod32 (get-verification-tag header)))
	       (vector (make-shutdown-ack-chunk))
	       peer-addr)
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The SUT needs to be a daytime server.
;;; (sctp-imh-i-3-9 sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWN is retransmitted.



(define (sctp-imh-i-3-10 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send (make-common-header local-port peer-port (1+mod32 (get-verification-tag header)))
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-imh-i-3-10 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if SHUTDOWN-ACK chunks are being retransmitted.

