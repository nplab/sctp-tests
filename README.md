# SCTP Testsuite

The tests are based on the ETSI test specification
[ETSI TS 102 369](http://www.etsi.org/deliver/etsi_ts/102300_102399/102369/01.01.01_60/ts_102369v010101p.pdf).

These tests use [guile](https://www.gnu.org/software/guile/) and its extension 
This tool uses [stt](https://github.com/nplab/stt).
Please see [README](https://github.com/nplab/stt#installation) for installation instructions.

## Installation
For downloading the tests run
```
git clone https://github.com/nplab/sctp-tests.git
cd sctp-tests
```
and change the line in `dotstt.scm`
```
(define dir "/Users/tuexen/Documents/sctp-tests/")
```
to reflect the location of the `sctp-tests` directory and run
```
cp dotstt.scm ~/.stt.scm
```

## Configuration
The test tool is configured by editing the file `sctp-param-testtool.scm`. You can change
the IP-address and SCTP port numbers used by the test tool and the system under test by
modifying the entries:
```
(define sut-addr (make-ipv4-address "192.168.1.244"))
(define sut-port 80)
```
and
```
(define tester-port 5001)
(define tester-addr-1 (make-ipv4-address "192.168.1.100"))
(define tester-addr-2 (make-ipv4-address "192.168.1.200"))
```
The upper layer protocol is specified in the line
```
(define upper-layer-protocol ulp-echo)
```
Possible values are

* `ulp-diameter` for the diameter protocol
* `ulp-echo` for the echo protocol
* `ulp-m3ua` for the M3UA protocol
* `ulp-s1ap` for the S1AP protocol

You also need to specify if the system under tests acts as an SCTP client or server:
```
(define sut-is-server #t)
```
You can also specify several other parameters.

## Usage
On Linux, SCTP support is implemented via a loadable kernel module. When using stt, ensure that the kernel
module is not loaded.

You can run specfic tests using the `runsctptest`. For example, to run the `sctp-as-v-1-1-1` test, execute
```
runsctptest sctp-as-v-1-1-1
```
You can also run a selection of client tests by using the `sh` script
```
sctp-tests/sctp-client-tests
```
or a selection of server tests by running the `sh` script
```
sctp-tests/sctp-server-tests
```
Please note that you must verify the verdict by double checking the message flows.
This can be done by using a protocol analyzer like [Wireshark](https://www.wireshark.org).
