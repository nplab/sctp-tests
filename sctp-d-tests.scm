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

;;; $Id: sctp-d-tests.scm,v 1.16 2012/08/28 17:29:23 tuexen Exp $


;;; Version 1.1


;;; History
;;; 26.01.2005 sctp-d-v-8-1 sends now an M3UA HB message.
;;; 26.01.2005 sctp-d-o-8-8 fixed.
;;; 06.02.2005 sctp-d-v-8-2 sends now SACKs.
;;; 06.02.2005 sctp-d-v-8-9 implemented.
;;; 06.02.2005 sctp-d-v-8-1[234] implemented.
;;; 12.02.2005 sctp-d-o-8-8 does not increment SSN for segments
;;; 12.02.2005 sctp-d-v-8-9 does increment the SSN
;;; 13.02.2005 sctp-d-v-8-15 does not expect the data to be reflected.
;;; 19.07.2006 sctp-d-v-8-2 use an M3UA BEAT message.
;;; 19.07.2006 sctp-d-v-8-1[345] use an M3UA BEAT message.

;;;
;;; Data (D)
;;;



(define (sctp-d-v-8-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-port (get-source-port header))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Acknowledge it.
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 0 #() #()))
	       peer-addr)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))

;;; The peer needs to be an echo server. .
;;; (sctp-d-v-8-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT send the messsage back.
  


(define (sctp-d-v-8-2-help peer-server? peer-addr local-port peer-port mtu total-user-data-size)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb))
	 (max-data-per-chunk (- mtu (+ 20 12 16)))
	 (user-data-fits (= (remainder total-user-data-size max-data-per-chunk) 0)) 
	 (number-of-middle-chunks (if user-data-fits
				      (- (quotient total-user-data-size max-data-per-chunk) 2)
				      (- (quotient total-user-data-size max-data-per-chunk) 1)))
	 (data-of-last-chunk (if user-data-fits
				 max-data-per-chunk
				 (remainder total-user-data-size max-data-per-chunk))))
    (if (<= total-user-data-size max-data-per-chunk)
	(sctp-send header 
		   (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid (make-test-message total-user-data-size total-user-data-size) #f #t #t))
		   peer-addr)
	(begin
	  (sctp-send header 
		     (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid (make-test-message max-data-per-chunk total-user-data-size) #f #t #f))
		     peer-addr)
	  (do ((i 0 (+ i 1)))
	      ((= i number-of-middle-chunks))
	    (sctp-send header
		       (vector (make-data-chunk (+mod32 local-tsn (+ 1 i)) 1 0 test-ppid (make-random-bytes max-data-per-chunk) #f #f #f))
		       peer-addr))
	  (sctp-send header
		     (vector (make-data-chunk (+mod32 local-tsn (+ number-of-middle-chunks 1)) 1 0 test-ppid (make-random-bytes data-of-last-chunk) #f #f #t))
		     peer-addr)))
    (if (<= total-user-data-size max-data-per-chunk)
	(begin
	  (sctp-receive-chunk data-chunk?)
	  (sctp-send header
		     (vector (make-sack-chunk peer-tsn mtu #() #()))
		     peer-addr))
	(do ((i 0 (+ i 1)))
	    ((= i (+ 2 number-of-middle-chunks)))
	  (sctp-receive-chunk data-chunk?)
	  (sctp-send header
		     (vector (make-sack-chunk (+mod32 peer-tsn i) mtu #() #()))
		     peer-addr)))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))

(define (sctp-d-v-8-2 peer-server? peer-addr local-port peer-port)
  (sctp-d-v-8-2-help peer-server? peer-addr local-port peer-port 1500 4000))
;;; The peer needs to be an echo server. The but last argument is the MTU and the last one is the user data size.
;;; (sctp-d-v-8-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back the DATA.



(define (sctp-d-v-8-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb))
	 (first-part (list->vector (list (car (vector->list test-message)))))
	 (middle-part (list->vector (list (cadr (vector->list test-message)))))
	 (end-part (list->vector (cddr (vector->list test-message)))))
    (sctp-send header 
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid first-part #f #t #f)
		       (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid middle-part #f #f #f)
		       (make-data-chunk (+mod32 local-tsn 2) 1 0 test-ppid end-part #f #f #t))
	       peer-addr)
    (let* ((result (sctp-receive-chunk data-chunk?))
	   (data-chunk (vector-find (cadr result) data-chunk?)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      stt-test-result-passed)))
;;; The peer needs to be an echo server.
;;; (sctp-d-v-8-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back the user message.



(define (sctp-d-v-8-4 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 1500 #() #()))
	       peer-addr)
    (let ((result (sctp-receive 4000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (and (data-chunk? chunk) (= (get-tsn chunk) peer-tsn))
		stt-test-result-failed
		stt-test-result-passed))))))
;;; The peer needs to be an echo server.
;;; (sctp-d-v-8-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if you do not see any retransmissions of the DATA chunk



(define (sctp-d-v-8-4-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (if (not peer-server?)
      (let* ((tcb (accept-association tester-os tester-mis))
	     (header (get-header tcb))
	     (peer-addr (get-peer-addr tcb))
	     (local-tsn (get-local-tsn tcb))
	     (peer-tsn (get-remote-tsn tcb)))
;;;    (sctp-send header 
;;;	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
;;;	       peer-addr)
	(sctp-receive-chunk data-chunk?)
	(sctp-send header
		   (vector (make-sack-chunk peer-tsn 1500 #() #()))
		   peer-addr)
	(let ((result (sctp-receive 4000)))
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (data-chunk? chunk)
		    stt-test-result-failed
		    stt-test-result-passed)))))
      stt-test-result-not-applicable))
;;; The peer needs to be an echo client.
;;; (sctp-d-v-8-4-alt sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if you do not see any retransmissions of the DATA chunk



(define (sctp-d-i-8-5 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    (sctp-receive-chunk data-chunk? header)
    (sctp-send header 
	       (vector (make-sack-chunk peer-tsn 1500 #() #())
	               (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (dotimes (i (1+ sut-maximum-assoc-retransmits))
	     (sctp-receive-chunk data-chunk? header))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to provide an echo service.
;;; (sctp-d-i-8-5 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if you do see retransmissions of the DATA chunk



(define (sctp-d-i-8-5-alt peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (dotimes (i sut-maximum-assoc-retransmits)
	     (sctp-receive-chunk data-chunk?))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to be an echo server.
;;; (sctp-d-i-8-5-alt sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if you do see retransmissions of the DATA chunk



(define (sctp-d-o-8-6 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message)
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sleep tester-short-wait)
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sleep tester-short-wait)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-unknown))
;;; The peer needs to provide an echo service. 
;;; (sctp-d-o-8-6 sut-is-server sut-addr tester-port sut-port)
;;; See ETSI TS.



(define (sctp-d-o-8-7 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    ;; Close receiver window.
    (sctp-send header
	       (vector (make-sack-chunk (1-mod32 peer-tsn) 0 #() #()))
	       peer-addr)
    ;; Send first DATA chunk such that it fills up a packet.
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    ;; do not SACK it.
    (sctp-send header
	       (vector (make-sack-chunk (1-mod32 peer-tsn) 0 #() #()))
	       peer-addr)
    (sctp-send header 
	       (vector (make-data-chunk (1+mod32 local-tsn) 1 1 test-ppid test-message))
	       peer-addr)
    (let ((result stt-test-result-passed))
      (dotimes (i 4)
	       (let* ((chunks (cadr (sctp-receive-chunk data-chunk?)))
		      (data-chunk (vector-ref chunks (1- (vector-length chunks)))))
		 (sctp-send header
			    (vector (make-sack-chunk (1-mod32 peer-tsn) 0 #() #()))
			    peer-addr)
		 (if (not (= (get-tsn data-chunk) peer-tsn))
		     (set! result stt-test-result-failed))))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      result)))
;;; The peer needs to be an echo server.
;;; (sctp-d-o-8-7 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if only one DATA chunk is used for zero window probing.



(define (sctp-d-o-8-8-help peer-server? peer-addr local-port peer-port size)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    ;; Close receiver window.
    (sctp-send header
	       (vector (make-sack-chunk (1-mod32 peer-tsn) 0 #() #()))
	       peer-addr)
    ;; Send first DATA chunk such that it fills up a packet.
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 0 (make-random-bytes size) #f #t #f))
	       peer-addr)
    (sctp-send header 
	       (vector (make-data-chunk (1+mod32 local-tsn) 1 0 0 (make-random-bytes size) #f #f #t))
	       peer-addr)
    (sleep tester-long-wait)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-unknown))

(define (sctp-d-o-8-8 peer-server? peer-addr local-port peer-port)
  (sctp-d-o-8-8-help peer-server? peer-addr local-port peer-port 1000))
;;; The peer needs to be an echo server.
;;; (sctp-d-o-8-8 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if only one DATA chunk is used for zero window probing.



(define (sctp-d-v-8-9-help peer-server? peer-addr local-port peer-port nr)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (car tcb))
	 (local-tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb))
	 (remote-tsn (caddr tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)    
    (sctp-receive-chunk data-chunk?)
    (do ((i 0 (+ i 1)))
	((= i nr))
      (sctp-send header 
		 (vector (make-data-chunk (+mod32 local-tsn (+ 1 i)) 1 (+ 1 i) test-ppid test-message))
		 peer-addr)
      (sctp-receive-chunk data-chunk?)
      (sctp-send header 
		 (vector (make-sack-chunk (1-mod32 remote-tsn) 1500 (vector (vector 2 (+ i 2))) #())) 
		 peer-addr))
    (sleep tester-long-wait)
    (sctp-send header 
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-unknown))
(define (sctp-d-v-8-9 peer-server? peer-addr local-port peer-port)
  (sctp-d-v-8-9-help peer-server? peer-addr local-port peer-port 5))
;;; The peer needs to provide an echo service.
;;; (sctp-d-v-8-9 sut-addr tester-port sut-port 5)
;;; The test is passed if retransmissions are sent before transmissions.



(define (sctp-d-v-8-10-help peer-server? peer-addr local-port peer-port tester-addr-1 tester-addr-2)
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
	  (sctp-send header
		     (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		     peer-addr)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-send header
		     (vector (make-sack-chunk peer-tsn 1500 #() #()))
		     peer-addr
		     tester-addr-2)
	  (sleep tester-long-wait)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-unknown))
      stt-test-result-not-applicable))

(define (sctp-d-v-8-10 peer-server? peer-addr local-port peer-port)
  (sctp-d-v-8-10-help peer-server? peer-addr local-port peer-port tester-addr-1 tester-addr-2))
;;; The peer needs to be a server.
;;; (sctp-d-v-8-10 sut-addr tester-port sut-port)
;;; The test is passed if not DATA chunks are retransmitted.



(define (sctp-d-i-8-11 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk local-tsn 1 0 0 #()))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-d-i-8-11 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an ABORT is sent back.



(define (sctp-d-o-8-12 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    (sctp-send header
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 1500 #() #()))
	       peer-addr)
    (sctp-send header
	       (vector (make-data-chunk (1+mod32 local-tsn) 1 1 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (sctp-send header
	       (vector (make-sack-chunk (1-mod32 peer-tsn) 1500 (vector (vector 2 2)) #()))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (sctp-receive-chunk data-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-unknown))
;;; The peer needs to be a server.
;;; (sctp-d-o-8-12 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT retranmits the second DATA chunk.



(define sctp-d-v-8-13 sctp-d-v-8-2)
;;; The peer needs to be an echo server. The but last argument is the MTU and the last one is the maximum userdata size.
;;; (sctp-d-v-8-13 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back the DATA.



(define sctp-d-v-8-14 sctp-d-v-8-2)
;;; The peer needs to be an echo server. The but last argument is the MTU and the last one is the maximum userdata size.
;;; (sctp-d-v-8-14 sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back the DATA.



(define (sctp-d-v-8-15-help peer-server? peer-addr local-port peer-port mtu total-user-data-size)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb))
	 (max-data-per-chunk (- mtu (+ 20 12 16)))
	 (user-data-fits (= (remainder total-user-data-size max-data-per-chunk) 0)) 
	 (number-of-middle-chunks (if user-data-fits
				      (- (quotient total-user-data-size max-data-per-chunk) 2)
				      (- (quotient total-user-data-size max-data-per-chunk) 1)))
	 (data-of-last-chunk (if user-data-fits
				 max-data-per-chunk
				 (remainder total-user-data-size max-data-per-chunk))))
    (if (<= total-user-data-size max-data-per-chunk)
	(sctp-send header 
		   (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid (make-test-message total-user-data-size total-user-data-size) #f #t #t))
		   peer-addr)
	(begin
	  (sctp-send header 
		     (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid (make-test-message max-data-per-chunk total-user-data-size) #f #t #f))
		     peer-addr)
	  (do ((i 0 (+ i 1)))
	      ((= i number-of-middle-chunks))
	    (sctp-send header
		       (vector (make-data-chunk (+mod32 local-tsn (+ 1 i)) 1 0 test-ppid (make-random-bytes max-data-per-chunk) #f #f #f))
		       peer-addr))
	  (sctp-send header
		     (vector (make-data-chunk (+mod32 local-tsn (+ number-of-middle-chunks 1)) 1 0 test-ppid (make-random-bytes data-of-last-chunk) #f #f #t))
		     peer-addr)))
    (sleep tester-short-wait)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-unknown))

(define (sctp-d-v-8-15 peer-server? peer-addr local-port peer-port)
  (sctp-d-v-8-15-help peer-server? peer-addr local-port peer-port 1500 4000))

;;; The peer needs to be an echo server. The but last argument is the MTU and the last one is the maximum userdata size.
;;; (sctp-d-v-8-15 sut-addr tester-port sut-port 1500 4000)
;;; The test is passed if the SUT sends back an ABORT.

