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

;;; $Id: sctp-fh-tests.scm,v 1.8 2012/08/27 19:25:13 tuexen Exp $


;;; Version 1.1

;;; History
;;; 26.01.2005 Rename sctp-fh-o-5-2 to sctp-fh-v-5-2
;;; 30.01.2005 Make tester in sctp-fh-i-5-1-1 multihomed.
;;; 13.02.2005 sctp-fh-i-5-1-1 now answers the first heartbeats.
;;; 17.02.2005 sctp-fh-i-5-1-2 now sends an M3UA BEAT message.
;;;
;;; Fault Handling (FH)
;;;



(define (sctp-fh-i-5-1-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 
					    1500 
					    tester-os 
					    tester-mis
					    local-tsn 
					    (if (equal? tester-addr-1 tester-addr-2)
						(vector)
						(vector (make-ipv4-address-parameter tester-addr-1)
							(make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (peer-tsn     (get-initial-tsn init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
          ;;; wait for the heartbeats verifying the paths.
	  (let* ((answer    (sctp-receive-chunk heartbeat-chunk?))
		 (heartbeat (vector-ref (cadr answer) 0))
		 (peer-addr (cadddr answer))
		 (local-addr (caddr answer)))
	    (sctp-send header
		       (vector (make-heartbeat-ack-chunk (get-heartbeat-parameter heartbeat)))
		       peer-addr
		       local-addr))
	  (let* ((answer    (sctp-receive-chunk heartbeat-chunk?))
		 (heartbeat (vector-ref (cadr answer) 0))
		 (peer-addr (cadddr answer))
		 (local-addr (caddr answer)))
	    (sctp-send header
		       (vector (make-heartbeat-ack-chunk (get-heartbeat-parameter heartbeat)))
		       peer-addr
		       local-addr))
	  (sctp-send header
		     (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		     peer-addr)
	  (sleep tester-long-wait)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-unknown))
	stt-test-result-not-applicable))
;;; The peer needs to be an echo server. This is an implementation limitation.
;;; (sctp-fh-i-5-1-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the DATA chunk is retransmitted by the SUT.



(define (sctp-fh-i-5-1-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    (dotimes (i 10)
	     (sctp-send header
			(vector (make-data-chunk (+mod32 local-tsn i) 1 i test-ppid test-message))
			peer-addr)
	     (sctp-receive-chunk data-chunk?)
	     (sctp-receive-chunk data-chunk?)
	     (sctp-receive-chunk data-chunk?)
	     (sctp-send header
			(vector (make-sack-chunk (+mod32 peer-tsn i) 1500 #() #()))
			peer-addr))
    (sleep tester-short-wait)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to provide an echo service.
;;; (sctp-fh-i-5-1-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if eventually all DATA was transmitted succesfully.


(define (sctp-fh-v-5-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (peer-addr (get-peer-addr tcb))
	 (header (get-header tcb)))
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((heartbeat-ack-chunk (vector-ref (cadr (sctp-receive-chunk heartbeat-ack-chunk?)) 0)))
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  (if (equal? (get-heartbeat-parameter heartbeat-chunk)
		      (get-heartbeat-parameter heartbeat-ack-chunk))
	      stt-test-result-passed
	      stt-test-result-failed))))
;;; The peer needs to be a server.
;;; (sctp-fh-v-5-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if a HEARTBEAT-ACK is sent back.
;;; What about large ones???



(define (sctp-fh-o-5-3-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((verification-tag (random (expt 2 32)))
	 (header (make-common-header local-port (if (= peer-port 0) 1 peer-port) verification-tag)))
    (sctp-send header
	       (vector (make-data-chunk 0 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive 500)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-fh-o-5-3-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if ABORTs are returned.



(define (sctp-fh-o-5-3-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((verification-tag (random (expt 2 32)))
	 (header (make-common-header local-port peer-port verification-tag)))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  stt-test-result-failed))))
;;; (sctp-fh-o-5-3-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not respond.



(define (sctp-fh-o-5-3-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((verification-tag (random (expt 2 32)))
	 (header (make-common-header local-port peer-port verification-tag)))
    (sctp-send header
	       (vector (make-shutdown-ack-chunk))
	       peer-addr)
    (let ((result (sctp-receive 500)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (and (shutdown-complete-chunk? chunk)
		     (= (get-chunk-flags chunk) #x01))
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-fh-o-5-3-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT send SHUTDOWN-COMPLETE.



(define (sctp-fh-o-5-3-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((verification-tag (random (expt 2 32)))
	 (header (make-common-header local-port peer-port verification-tag)))
    (sctp-send header
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  stt-test-result-failed))))
;;; (sctp-fh-o-5-3-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not respond.



(define (sctp-fh-o-5-3-5 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let ((local-tag  (choose-local-tag))
	(local-tsn  (random (expt 2 32))))
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn  (if (equal? tester-addr-1 tester-addr-2)
											   (vector)
											   (vector (make-ipv4-address-parameter tester-addr-1)
												   (make-ipv4-address-parameter tester-addr-2)))))
	       peer-addr)
    (let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	   (init-ack     (vector-ref (cadr answer) 0))
	   (parameters   (get-parameters init-ack))
	   (state-cookie (vector-find parameters cookie-parameter?))
	   (peer-tag     (get-initiate-tag init-ack))
	   (header       (make-common-header local-port peer-port peer-tag))
	   (cookie       (get-cookie-parameter-cookie state-cookie)))
      (sctp-send header 
		 (vector (make-cookie-echo-chunk cookie))
		 peer-addr
		 (make-ipv4-address "224.0.0.1"))
      (let ((result (sctp-receive 1000)))
	(sctp-send header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	(if (equal? result (list #f #f #f #f #f))
	    stt-test-result-passed
	    stt-test-result-failed)))))
;;; The peer needs to be a server.
;;; (sctp-fh-o-5-3-5 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not respond to the COOKIE-chunk.
