;;; 
;;; Copyright (C) 2014 I. Ruengeler ruengeler@fh-muenster.de
;;; Copyright (C) 2014 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: sctp-data-tests.scm,v 1.4 2014/08/30 19:49:35 tuexen Exp $


(define (sctp-data-send-ndata peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header 
	       (vector (make-ndata-chunk local-tsn 1 0 test-ppid 1 1 test-message #f #t #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (error-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-data-send-ndata sut-is-server sut-addr tester-port sut-port)
;;; An NDATA chunk is sent instead of a DATA chunk. An ERROR chunk is expected.
;;; Ony valid once the NDATA chunk type is 64.


(define (sctp-data-send-on-different-streams peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 0) 0 1 test-ppid test-message #f #t #f)) peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 1 test-ppid test-message #f #f #t)) peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		  (let ((causes (get-causes chunk)))
		    (if (equal? (get-cause-code (vector-ref causes 0)) 13)
			stt-test-result-passed
			stt-test-result-failed))
		  stt-test-result-failed))))))
;;; (sctp-data-send-on-different-streams sut-is-server sut-addr tester-port sut-port)
;;; The two fragments of a message are sent on different streams. An ABORT chunk is expected.
;;; FAILED with NDATA stack


(define (sctp-data-no-user-data peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid #() #f #t #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		  (let ((causes (get-causes chunk)))
		    (if (equal? (get-cause-code (vector-ref causes 0)) 9)
			stt-test-result-passed
			stt-test-result-failed))
		  stt-test-result-failed))))))
;;; (sctp-data-no-user-data sut-is-server sut-addr tester-port sut-port)
;;; A DATA chunk with payload length = 0 is sent. An ABORT chunk with "No User Data" cause is expected.


(define (sctp-data-double-begin peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message #f #t #f))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header 
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #f #t #f))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-double-begin sut-is-server sut-addr tester-port sut-port)
;;; Two fragments, both with the B-bit set are sent. An ABORT chunk with protocol violation cause is expected.
;;; Crashes with NDATA stack.


(define (sctp-data-start-with-end peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message #f #t #t))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header 
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 1 test-ppid test-message #f #f #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-start-with-end sut-is-server sut-addr tester-port sut-port)
;;; A complete chunk and a fragment with the E-bit set are sent. An ABORT chunk with protocol violation cause is expected.
;;; FAILED!!!  SACK is sent.


(define (sctp-data-ssn-not-same peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message #f #t #f))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 1 test-ppid test-message #f #f #f))
	       peer-addr)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 2) 1 2 test-ppid test-message #f #f #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-ssn-not-same sut-is-server sut-addr tester-port sut-port)
;;; The SSN of the fragments is different. An ABORT chunk with protocol violation cause is expected.
;;; FAILED with NDATA stack.


(define (sctp-data-wrap-TSN peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (- (expt 2 32) 2)))
	(sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
	       peer-addr)
	(let* ((answer (sctp-receive-chunk init-ack-chunk?))
	       (init-ack (vector-ref (cadr answer) 0))
	       (init-ack-parameters   (get-parameters init-ack))
	       (state-cookie (vector-find init-ack-parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  (sctp-send header
		     (vector (make-data-chunk  (- (expt 2 32) 2) 1 0 test-ppid test-message #f #t #t))
		     peer-addr)
	  (sctp-receive-chunk sack-chunk?)
	  (sctp-send header
		     (vector (make-data-chunk (- (expt 2 32) 1) 1 1 test-ppid test-message #f #t #t))
		     peer-addr)
	  (sctp-send header
		     (vector (make-data-chunk 0 1 2 test-ppid test-message #f #t #t))
		     peer-addr)
	  (let ((result (sctp-receive 1000)))
	    (sctp-send header
		       (vector (make-abort-chunk #f))
		       peer-addr)
	    (let ((chunk (vector-ref (cadr result) 0)))
	      (if (sack-chunk? chunk)   
		  (let ((cum (get-cumulative-tsn-ack chunk)))
		    (if (eq? cum 0)
			stt-test-result-passed
			stt-test-result-failed))
		  stt-test-result-failed)))))))
;;; (sctp-data-wrap-TSN sut-is-server sut-addr tester-port sut-port)
;;; TSN wrap around is tested. A SACK with cumTsnAck is expected


(define (sctp-data-unordered-fragmented peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message #t #t #f))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #t #f #f))
	       peer-addr)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 2) 1 0 test-ppid test-message #f #f #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-unordered-fragmented sut-is-server sut-addr tester-port sut-port)
;;; Fragments of an unordered message must all be set to U=1. Here the last U-Bit is false. An ABORT chunk with
;;; protocol violation cause is expected.
;;; FAILED!!!!  SACKs are sent.


(define (sctp-data-unordered-incomplete-message peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message #t #t #f))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #t #t #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-unordered-incomplete-message sut-is-server sut-addr tester-port sut-port)
;;; The middle fragment has the B/E bits set to 11 instead of 00. An ABORT chunk with protocol violation cause is expected.
;;;    FAILED!!!  SACKs are sent.


(define (sctp-data-ordered-incomplete-message peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message #f #t #f))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #f #t #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-ordered-incomplete-message sut-is-server sut-addr tester-port sut-port)
;;; The middle fragment has the B/E bits set to 11 instead of 00. An ABORT chunk with protocol violation cause is expected.
;;;    FAILED!!!  SACKs are sent.


(define (sctp-data-complete-after-middle-fragment peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #f #f #f))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 2) 1 1 test-ppid test-message #f #t #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-complete-after-middle-fragment sut-is-server sut-addr tester-port sut-port)
;;; Send a complete packet after a middle one.


(define (sctp-data-bundle-data-abort peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message #f #t #t))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #f #t #t) (make-abort-chunk #f))
	       peer-addr)
     (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed)))))) 
;;; (sctp-data-bundle-data-abort sut-is-server sut-addr tester-port sut-port)
;;; DATA chunks MUST NOT be bundled with ABORT. An ABORT chunk with protocol violation cause is expected.


(define (sctp-data-invalid-stream peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port 1 1)
		  (accept-association 1 1)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (tsn (cadr tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message #f #t #t))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 1) 2 0 test-ppid test-message #f #t #t))
	       peer-addr)
     (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (error-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 1)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-invalid-stream sut-is-server sut-addr tester-port sut-port)
;;; A DATA chunk with a non existent SID is sent. An ERROR chunk with cause "Invalid Stream Identifier" is expected


(define (sctp-data-data-before-cookie-echo  peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (init-ack-parameters   (get-parameters init-ack))
	       (state-cookie (vector-find init-ack-parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-data-chunk local-tsn 1 0 test-ppid test-message #f #t #t)
			     (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
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
      stt-test-result-not-applicable))
;;; (sctp-data-data-before-cookie-echo sut-is-server sut-addr tester-port sut-port)
;;; A DATA chunk is bundled with a COOKIE-ECHO chunk in the wrong sequence. An ABORT chunk with protocol violation cause is expected. 
;;; FAILED!!!!  Nothing is sent. 


(define (sctp-data-data-oob peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (init-ack-parameters   (get-parameters init-ack))
	       (state-cookie (vector-find init-ack-parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-data-chunk local-tsn 1 0 test-ppid test-message #f #t #t))
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
      stt-test-result-not-applicable))
;;; (sctp-data-data-oob sut-is-server sut-addr tester-port sut-port)
;;; A DATA chunk is sent after INIT-ACK is received. An ABORT chunk with protocol violation cause is expected. 


(define (sctp-data-only-end peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-tsn (get-peer-tsn tcb))
	 (peer-addr (get-peer-addr tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 1 test-ppid test-message #f #f #t))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		(let ((causes (get-causes chunk)))
		  (if (equal? (get-cause-code (vector-ref causes 0)) 13)
		      stt-test-result-passed
		      stt-test-result-failed))
		stt-test-result-failed))))))
;;; (sctp-data-only-end sut-is-server sut-addr tester-port sut-port)
;;; A fragment with the E-Bit set is sent.
