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

;;; $Id: sctp-m-tests.scm,v 1.11 2010/03/03 18:46:11 tuexen Exp $


;;; Version 1.0

;;;
;;; Miscellaneous Test Cases (M)
;;;



(define (sctp-m-i-10-1 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-chunk #xf5 0 (make-ascii-bytes "This is a test"))
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive-chunk-with-timeout
		    (lambda (x) (or (error-chunk? x) (sack-chunk? x)))
		    1000)))
	   (if (equal? result (list #f #f #f #f #f))
	       (begin (sctp-send header
				 (vector (make-abort-chunk #f))
				 peer-addr)
		      stt-test-result-failed)
	       (let ((control-chunks (vector-filter (cadr result) (lambda(x) (not (data-chunk? x))))))
		 (if (= (vector-length control-chunks) 2)
		     (begin (sctp-send header
				       (vector (make-abort-chunk #f))
				       peer-addr)
			    (if (and (error-chunk? (vector-ref control-chunks 0))
				     (sack-chunk? (vector-ref control-chunks 1))
				     (= (get-cumulative-tsn-ack  (vector-ref control-chunks 1)) local-tsn))
				stt-test-result-passed
				stt-test-result-failed))
		     (if (and (= (vector-length control-chunks) 1)
			      (error-chunk? (vector-ref control-chunks 0)))
			 (let ((result (sctp-receive-chunk-with-timeout sack-chunk? 1000)))
			   (if (equal? result (list #f #f #f #f #f))
			       (begin (sctp-send header
						 (vector (make-abort-chunk #f))
						 peer-addr)
				      stt-test-result-failed)
			       (let ((control-chunks (vector-filter (cadr result) (lambda(x) (not (data-chunk? x))))))
				 (sctp-send header
					    (vector (make-abort-chunk #f))
					    peer-addr)
				 (if (and (= (vector-length control-chunks) 1)
					  (sack-chunk? (vector-ref control-chunks 0))
					  (= (get-cumulative-tsn-ack  (vector-ref control-chunks 0)) local-tsn))
				     stt-test-result-passed
				     stt-test-result-failed))))
			 (begin (sctp-send header
					   (vector (make-abort-chunk #f))
					   peer-addr)
				stt-test-result-failed))))))))
;;; (sctp-m-i-10-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back an ERROR chunk and a SACK chunk.



(define (sctp-m-i-10-2 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-chunk #x35 0 (make-ascii-bytes "This is a test"))
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive-chunk-with-timeout
		   (lambda (x) (or (error-chunk? x) (sack-chunk? x)))
		   1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  stt-test-result-failed))))
;;; (sctp-m-i-10-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not send back an ERROR chunk and does not SACK the DATA chunk.



(define (sctp-m-i-10-3 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-chunk #x75 0 (make-ascii-bytes "This is a test"))
		       (make-data-chunk (+mod32 local-tsn 0) 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive-chunk
		    (lambda (x) (or (error-chunk? x) (sack-chunk? x))))))
      (if (equal? result (list #f #f #f #f #f))
	  (begin (sctp-send header
			    (vector (make-abort-chunk #f))
			    peer-addr)
		 stt-test-result-failed)
	  (let ((control-chunks (vector-filter (cadr result) (lambda(x) (not (data-chunk? x))))))
	    (if (and (= (vector-length control-chunks) 1)
		     (error-chunk? (vector-ref control-chunks 0)))
		(if (equal? (sctp-receive-chunk-with-timeout sack-chunk? 1000)
			    (list #f #f #f #f #f))
		    (begin
			(sctp-send header
				   (vector (make-abort-chunk #f))
				   peer-addr)
			stt-test-result-passed)
		    (begin
			(sctp-send header
				   (vector (make-abort-chunk #f))
				   peer-addr)
			stt-test-result-failed))
		(begin (sctp-send header
				  (vector (make-abort-chunk #f))
				  peer-addr)
		       stt-test-result-failed)))))))
;;; (sctp-m-i-10-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back an ERROR chunk and does not SACK the DATA chunk.



(define (sctp-m-i-10-4 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-chunk #xb5 0 (make-ascii-bytes "This is a test"))
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive-chunk
		    (lambda (x) (or (error-chunk? x) (sack-chunk? x))))))
      (if (equal? result (list #f #f #f #f #f))
	  (begin (sctp-send header
			    (vector (make-abort-chunk #f))
			    peer-addr)
		 stt-test-result-failed)
	  (let ((control-chunks (vector-filter (cadr result) (lambda(x) (not (data-chunk? x))))))
	    (if (and (= (vector-length control-chunks) 1)
		     (sack-chunk? (vector-ref control-chunks 0))
		     (= (get-cumulative-tsn-ack (vector-ref control-chunks 0)) local-tsn))
		(if (equal? (sctp-receive-chunk-with-timeout error-chunk? 1000)
			    (list #f #f #f #f #f))
		    (begin
		      (sctp-send header
				 (vector (make-abort-chunk #f))
			   peer-addr)
		      stt-test-result-passed)
		    (begin
		      (sctp-send header
				 (vector (make-abort-chunk #f))
			   peer-addr)
		      stt-test-result-failed))
		(begin
		  (sctp-send header
			     (vector (make-abort-chunk #f))
			     peer-addr)
		  stt-test-result-failed)))))))
;;; (sctp-m-i-10-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not send back an ERROR chunk and SACK the DATA chunk.



