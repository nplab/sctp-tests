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

;;; $Id: sctp-a-tests.scm,v 1.7 2012/08/25 21:43:53 tuexen Exp $

;;; Version 1.0


;;;
;;; Acknowledgement (A)
;;;



(define (sctp-a-v-9-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-remote-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (start-time (get-internal-real-time)))
    (sctp-send header
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    (if (= upper-layer-protocol ulp-echo)
	(if (<= (- (get-internal-real-time) start-time)
		(* 20/1000 internal-time-units-per-second))
	    stt-test-result-passed
	    stt-test-result-failed)
	stt-test-result-unknown)))
;;; (sctp-a-v-9-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends the SACK without delay. 



(define (sctp-a-v-9-2 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message)
		       (make-data-chunk (+mod32 local-tsn 1) 1 1 test-ppid test-message))
	       peer-addr)
    (let ((sack (vector-ref (cadr (sctp-receive-chunk sack-chunk?)) 0)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (= (get-cumulative-tsn-ack sack)
	     (+mod32 local-tsn 1))
	  stt-test-result-passed
	  stt-test-result-failed))))
;;; (sctp-a-v-9-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends one SACK for the packet.



(define (sctp-a-o-9-3 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (let ((start-time (get-internal-real-time)))
      (sctp-send header
		 (vector (make-data-chunk (+mod32 local-tsn 2) 1 2 test-ppid test-message))
		 peer-addr)
      (sctp-receive-chunk sack-chunk?)
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (<= (- (get-internal-real-time) start-time)
	      (* 20/1000 internal-time-units-per-second))
	  stt-test-result-passed
	  stt-test-result-failed))))
;;; (sctp-a-o-9-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends a SACK with a gap without delay.
