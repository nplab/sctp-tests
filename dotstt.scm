;;; 
;;; Copyright (C) 2009 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: dotstt.scm,v 1.4 2012/08/25 23:26:39 tuexen Exp $

;;;
;;; This files loads all test scripts.
;;; Please modify the variable dir according to your setup.
;;;

(define dir "/Users/tuexen/Documents/sctp-tests/")
(define files (list "common.scm"
		    "diameter.scm"
		    "sctp.scm"
		    "sctp-param-testtool.scm"
		    "sctp-a-tests.scm"
		    "sctp-as-tests.scm"
		    "sctp-at-tests.scm"
		    "sctp-bdc-tests.scm"
		    "sctp-d-tests.scm"
		    "sctp-dm-tests.scm"
		    "sctp-e-tests.scm"
		    "sctp-fh-tests.scm"
		    "sctp-imh-tests.scm"
		    "sctp-m-tests.scm"
		    "sctp-rt-tests.scm"
		    "sctp-bundling-tests.scm"
		    "sctp-misc-tests.scm"))

(map (lambda (file) (load-from-path (string-append dir file))) files)
