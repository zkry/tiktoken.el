;;; tiktoken-test.el --- Tests for tiktoken.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This pacakge contains tests for the tiktoken.el library.  The most
;; important test is to check that the outputs of this command matches
;; that of the official python library.  To this end, I have a test
;; that runs the two side-by-side and fails if the results don't match
;; on random inputs.

(defvar tiktoken-test-length-fuzz-n 10)
(defvar tiktoken-test-length-fuzz-string-size 200)
(defvar tiktoken-test-last-str nil)

(ert-deftest tiktoken-test-length-fuzz ()
  "Tests that encoder encodes with same length as official library."
  (dotimes (i tiktoken-test-length-fuzz-n)
    (let* ((chars (make-vector tiktoken-test-length-fuzz-string-size nil)))
      (dotimes (idx tiktoken-test-length-fuzz-string-size)
        (aset chars idx (+ 32 (random (- 127 32)))))
      (let* ((str (concat chars))
             (_ (setq tiktoken-test-last-str str))
             (my-len (tiktoken-count-tokens (tiktoken-cl100k-base) str))
             (out-buf (generate-new-buffer "*test-out*"))
             (_ (call-process "python" nil out-buf nil "./test.py" str))
             (their-len (string-to-number (with-current-buffer out-buf (buffer-string)))))
        (should (= my-len their-len))
        (kill-buffer out-buf)))))

(ert-deftest tikoken-test-encode-ordinary ()
  "Tests that encoding is correct."
  (should (equal (tiktoken-encode-ordinary (tiktoken-cl100k-base) "This is a test string!")
                 '(2028 374 264 1296 925 0)))
  (should (equal (tiktoken-encode-ordinary (tiktoken-p50k-base) "This is a test string!")
                 '(1212 318 257 1332 4731 0)))
  (should (equal (tiktoken-encode-ordinary (tiktoken-p50k-edit) "This is a test string!")
                 '(1212 318 257 1332 4731 0)))
  (should (equal (tiktoken-encode-ordinary (tiktoken-r50k-base) "This is a test string!")
                 '(1212 318 257 1332 4731 0))))

(ert-deftest tikoken-test-encode ()
  "Tests that encoding is correct with special tokens."
  (should (equal (tiktoken-encode (tiktoken-cl100k-base) "<|fim_prefix|> This is Prefix <|fim_suffix|> This is Suffix <|fim_middle|> This is Middle" 'all)
                 '(100258 1115 374 57583 220 100260 1115 374 328 13866 220 100259 1115 374 12877))))

;;; Peculiar strings:
;; TODO Do more research into why these strings produce off-by-one error.
;; xREq_y!*8K!e55#BlS{Zj5Dl\#TJ):GIM*H8EUR  (dflf(3AS*BfUqg~z a3}9hAiXQE;rN?"zf:.|e(pW`Rj)c`#l(EM;vMQq@_b"RJU3%\W#>wgUF#k1-v%QzL?~?)OA6WaV<odJIhT:6$rHY 2ARM^?/o~7P[[\s#.w<We4IIl}iejja;vC<'hp(4Y21&_j[nqOK

(provide 'tiktoken-test)
;;; tiktoken-test.el ends here
