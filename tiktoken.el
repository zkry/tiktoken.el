;;; tiktoken.el --- Count BPE Tokens -*- lexical-binding: t; -*-

;; Author: Zachary Romero
;; URL: https://github.com/zkry/tiktoken.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.0") (f "0.20.0"))
;;

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a port of OpenAIs tiktoken library, a package for
;; tokenizing utilizing a byte-pair encoder.
;;
;; The first thing required for usrage of this library is an encoding.
;; An encoding object contains the necessary metadata to encode/decode
;; a piece of text.  The aspects of the encoding are:
;;
;; - The ranks of various byte pairs: These are the rules for
;;   combining byte combinations into single tokens.  The file is
;;   fetched over the internet.
;;
;; - The regex splitter: This is a base regular expression that splits
;;   the text into pieces which can be merged.
;;
;; - Special token rules: Special tokens that divide the main text.
;;
;; If you know the encoding you want, you can directly obtain it with
;; the following functions: `tiktoken-cl100k-base',
;; `tiktoken-p50k-edit', `tiktoken-p50k-base', and
;; `tiktoken-r50k-base'.  The initial call to these functions will
;; require parsing of the token-rank files.  These files are fetched
;; over the web and cached.  The caching mechanism saves the fetched
;; file to disk.  You can control where this file is saved with the
;; variable `tiktoken-cache-dir', or set it to nil to disable caching.
;; You may also set `tiktoken-offline-ranks' to read the rank files
;; directly from disk.  Note that these functions are memoized, allowing
;; subsequent calls to complete immediately.
;;
;; If you don't know the encoding you want but know the model you are
;; using you can call the `tiktoken-encoding-for-model' function and
;; pass it the name of the model you are interested in using.
;;
;; Once you have the encoding object, you will pass it to the
;; following functions to encode or decode:
;;
;; - `tiktoken-encode-ordinary': directly encode the text not caring
;;   about special tokens.
;;
;; - `tiktoken-encode': encode text, taking into account special
;;   tokens.
;;
;; - `tiktoken-decode': decode a list of token IDs.
;;
;; - `tiktoken-count-tokens': count the number of tokens in a given
;;    text.  Note that this function is optimized for performance and
;;    should be used if you only care about token length.

;;; Code:

(require 'cl-lib)
(require 'ht)

(defgroup tiktoken nil
  "Byte-pair encoding tokenization for NLP applications."
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/zkry/tiktoken.el"))

(defcustom tiktoken-cache-dir
  user-emacs-directory
  "Directory to save downloaded encoding ranks.

If set to nil or an empty string, caching will be disabled."
  :group 'tiktoken
  :type 'directory)

(defcustom tiktoken-offline-ranks
  nil
  "Alist indicating the file to load for the ranks of a particular model."
  :group 'tiktoken
  :type '(alist :key-type string :value-type file))

(defconst tiktoken-special-endoftext "<|endoftext|>")
(defconst tiktoken-special-fim-prefix "<|fim_prefix|>")
(defconst tiktoken-special-fim-middle "<|fim_middle|>")
(defconst tiktoken-special-fim-suffix "<|fim_suffix|>")
(defconst tiktoken-special-endofprompt "<|endofprompt|>")

(defconst tiktoken-model-cl100k-base "cl100k_base") ; MODEL_CL100K_BASE
(defconst tiktoken-model-p50k-base "p50k_base") ; MODEL_P50K_BASE
(defconst tiktoken-model-p50k-edit "p50k_edit") ; MODEL_P50K_EDIT
(defconst tiktoken-model-r50k-base "r50k_base") ; MODEL_R50K_BASE

(defconst tiktoken-model-urls
  (let ((ht (make-hash-table :test 'equal)))
   (puthash tiktoken-model-cl100k-base "https://openaipublic.blob.core.windows.net/encodings/cl100k_base.tiktoken" ht)
   (puthash tiktoken-model-p50k-edit "https://openaipublic.blob.core.windows.net/encodings/p50k_base.tiktoken" ht)
   (puthash tiktoken-model-p50k-base "https://openaipublic.blob.core.windows.net/encodings/p50k_base.tiktoken" ht)
   (puthash tiktoken-model-r50k-base "https://openaipublic.blob.core.windows.net/encodings/r50k_base.tiktoken" ht)
   ht)
  "Mapping from model name to URL from wich to obtain the token rankings.")

(defconst tiktoken-model-to-encoding
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "gpt-3.5-turbo" tiktoken-model-cl100k-base ht)
    (puthash "text-davinci-003" tiktoken-model-p50k-base ht)
    (puthash "text-davinci-002" tiktoken-model-p50k-base ht)
    (puthash "text-davinci-001" tiktoken-model-r50k-base ht)
    (puthash "text-curie-001" tiktoken-model-r50k-base ht)
    (puthash "text-babbage-001" tiktoken-model-r50k-base ht)
    (puthash "text-ada-001" tiktoken-model-r50k-base ht)
    (puthash "davinci" tiktoken-model-r50k-base ht)
    (puthash "curie" tiktoken-model-r50k-base ht)
    (puthash "babbage" tiktoken-model-r50k-base ht)
    (puthash "ada" tiktoken-model-r50k-base ht)
    (puthash "code-davinci-002" tiktoken-model-p50k-base ht)
    (puthash "code-davinci-001" tiktoken-model-p50k-base ht)
    (puthash "code-cushman-002" tiktoken-model-p50k-base ht)
    (puthash "code-cushman-001" tiktoken-model-p50k-base ht)
    (puthash "davinci-codex" tiktoken-model-p50k-base ht)
    (puthash "cushman-codex" tiktoken-model-p50k-base ht)
    (puthash "text-davinci-edit-001" tiktoken-model-p50k-edit ht)
    (puthash "code-davinci-edit-001" tiktoken-model-p50k-edit ht)
    (puthash "text-embedding-ada-002" tiktoken-model-cl100k-base ht)
    (puthash "text-similarity-davinci-001" tiktoken-model-r50k-base ht)
    (puthash "text-similarity-curie-001" tiktoken-model-r50k-base ht)
    (puthash "text-similarity-babbage-001" tiktoken-model-r50k-base ht)
    (puthash "text-similarity-ada-001" tiktoken-model-r50k-base ht)
    (puthash "text-search-davinci-doc-001" tiktoken-model-r50k-base ht)
    (puthash "text-search-curie-doc-001" tiktoken-model-r50k-base ht)
    (puthash "text-search-babbage-doc-001" tiktoken-model-r50k-base ht)
    (puthash "text-search-ada-doc-001" tiktoken-model-r50k-base ht)
    (puthash "code-search-babbage-code-001" tiktoken-model-r50k-base ht)
    (puthash "code-search-ada-code-001" tiktoken-model-r50k-base ht)
    ht
    "Map of model name to encoder."))

(defconst tiktoken-model-prefix-to-encoding
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "gpt-4-" tiktoken-model-cl100k-base ht)
    (puthash "gpt-3.5-turbo-" tiktoken-model-cl100k-base ht)
    ht)
  "Map of model name prefix to encoding model.")

(defun tiktoken--parse-ranks (text)
  "Given a rank file TEXT, parse it into a map of piece to token number."
  (let* ((ht (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (search-forward " ")
          (let ((str (base64-decode-string
                      (buffer-substring-no-properties start (1- (point)))))
                (val (string-to-number
                      (buffer-substring-no-properties (point) (pos-eol)))))
            (puthash str val ht)
            (forward-line 1)))))
    ht))

(defun tiktoken-load-model-bpe (model)
  "Fetch the MODEL encodings ranks and return it parsed into a hash table.

If `tiktoken-cache-dir' is non-nil and not empy, first look for
the cached file under the name
\"<tiktoken-cache-dir>/<MODEL>.txt\".  If such a file exists,
don't fetch from the external URL and use the file instead.  If
no cached file exists, fetch from the URL and save it to
mentioned filename.

If `tiktoken-offline-ranks' is an alist containing a value for
the key MODEL, then parse and use that file file in place of
fetching the URL or loading from cache."
  (let ((cache-file (and (not (s-blank-p tiktoken-cache-dir))
                         (concat tiktoken-cache-dir "/" model ".txt"))))
    (cond
     ((and cache-file (f-exists-p cache-file))
      (tiktoken--parse-ranks (f-read cache-file)))
     ((and (hash-table-p tiktoken-offline-ranks)
           (gethash model tiktoken-offline-ranks))
      (tiktoken--parse-ranks (f-read (gethash model tiktoken-offline-ranks))))
     (t
      (let* ((url (gethash model tiktoken-model-urls))
             (resp (request url :sync t)))
        (unless (<= 200 (request-response-status-code resp) 299)
          (error "Unexpected result fetching model for %s at %s" model url))
        (when cache-file
          (f-write (request-response-data resp) 'utf-8 cache-file))
        (tiktoken--parse-ranks (request-response-data resp)))))))

(cl-defstruct (tiktoken-encoding
               (:constructor tiktoken-encoding-create)
               (:copier nil))
  "Structure containing all data required to byte-pair encode text."
  name
  pat-str
  mergeable-ranks
  special-tokens)

(defun tiktoken--byte-pair-merge (piece ranks f &optional counts-only)
  "Merge bytes of PIECE according to RANKS.

F is a function taking two parameters, start and end, used to
fetch the token id from the bytes of PIECE between range start
and end.

If COUNTS-ONLY is not-nil, return the length of the encoding, not
the encoding itself."
  (let* ((parts (seq-into (seq-map-indexed
                           (lambda (_ i) (vector i most-positive-fixnum))
                           (make-vector (1+ (length piece)) nil))
                          'vector))
         (get-rank (lambda (start-idx skip)
                     (if (< (+ start-idx skip 2) (length parts))
                         (let* ((b (seq-subseq
                                    piece
                                    (aref (aref parts start-idx) 0)
                                    (aref (aref parts (+ start-idx skip 2)) 0)))
                                (rank (gethash (concat b) ranks)))
                           (or rank -1))
                       -1))))
    (cl-loop for i from 0 below (- (length parts) 2) do
             (let ((rank (funcall get-rank i 0)))
               (when (>= rank 0)
                 (setf (aref (aref parts i) 1) rank))))
    (catch 'done
     (while (> (length parts) 1)
       (let* ((min-rank most-positive-fixnum)
              (min-idx -1))
         (cl-loop for i from 0 below (- (length parts) 1) do
                  (when (< (aref (aref parts i) 1) min-rank)
                    (setq min-rank (aref (aref parts i) 1))
                    (setq min-idx i)))
         (if (< min-rank most-positive-fixnum)
             (let* ((i min-idx)
                    (rank (funcall get-rank i 1)))
               (if (>= rank 0)
                   (setf (aref (aref parts i) 1) rank)
                 (setf (aref (aref parts i) 1) most-positive-fixnum))
               (when (> i 0)
                 (let ((rk (funcall get-rank (1- i) 1)))
                   (if (>= rk 0)
                       (setf (aref (aref parts (1- i)) 1) rk)
                     (setf (aref (aref parts (1- i)) 1) most-positive-fixnum))))
               (setq parts (seq-concatenate 'vector
                                            (seq-subseq parts 0 (1+ i))
                                            (seq-subseq parts (+ i 2)))))
           (throw 'done nil)))))
    (if counts-only
        (1- (length parts))
      (let ((out (make-vector (1- (length parts)) nil)))
        (cl-loop for i from 0 below (length out) do
                 ;; store in reverse order!
                 (setf (aref out (- (length out) i 1))
                       (funcall f
                                (aref (aref parts i) 0)
                                (aref (aref parts (1+ i)) 0))))
        (seq-into out 'list)))))

(defun tiktoken--byte-pair-encode (piece ranks &optional counts-only)
  "Return list of token ids of PIECE split by RANKS.

RANKS is a mapping of unibyte strings to token id.

If COUNTS-ONLY is not-nil, return the length of the encoding, not
the encoding itself."
  (if (eq (length piece) 1)
      (if counts-only
          1
        (vector (gethash (concat piece) ranks)))
    (tiktoken--byte-pair-merge
     piece
     ranks
     (lambda (start end)
       (gethash (concat (seq-subseq piece start end)) ranks))
     counts-only)))

(defun tiktoken-find-regex->string-index (str regexp)
  "Find match of REGEXP in STR, returning start and end indecies."
  (save-match-data
    (let ((idx (string-match regexp str)))
      (when idx
        (cons idx (+ idx (length (match-string 0 str))))))))

(defun tiktoken--find-all-regexp-matches (text regexp)
  "Return all matches of REGEXP in TEXT."
  (let ((matches))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        (push (match-string 0) matches)))
    (nreverse matches)))

(defun tiktoken--encode-native (encoding text allowed-special)
  "Encode TEXT according to ENCODING.

Only special items of ALLOWED-SPECIAL are permitted."
  (let* ((special-tokens (tiktoken-encoding-special-tokens encoding))
         (special-regex (regexp-opt (hash-table-keys special-tokens)))
         (regex (tiktoken-encoding-pat-str encoding))
         (ranks (tiktoken-encoding-mergeable-ranks encoding))
         (ret '())
         (last-piece-token-len 0)
         (start 0))
    (catch 'break2
     (while t
       (let ((next-special nil)
             (start-find start))
         (catch 'break1
           (while t
             ;; Find the next allowed special token, if any
             (let ((temp (substring text start-find (length text))))
               (setq next-special
                     (tiktoken-find-regex->string-index temp special-regex))
               (if next-special
                   (let ((token (substring text
                                           (+ start-find (car next-special))
                                           (+ start-find (cdr next-special)))))
                     (when (gethash token allowed-special)
                       (throw 'break1 nil))
                     (cl-incf start-find (cdr next-special)))
                 (throw 'break1 nil)))))
         (let* ((end (if next-special
                         (+ start (car next-special))
                       (length text)))
                (matches (tiktoken--find-all-regexp-matches
                          (substring text start end)
                          regex)))
           (dolist (piece matches)
             (if-let ((token (gethash piece ranks)))
                 (progn
                   (setq last-piece-token-len 1)
                   (setq ret (cons token ret)))
               (let ((tokens (tiktoken--byte-pair-encode
                              (string-as-unibyte piece) ranks)))
                 (setq last-piece-token-len (length tokens))
                 (setq ret (append tokens ret)))))
           (if next-special
               (let* ((temp (substring text
                                       (+ start (car next-special))
                                       (+ start (cdr next-special))))
                      (token (gethash temp special-tokens)))
                 (setq ret (cons token ret))
                 (cl-incf start (cdr next-special))
                 (setq last-piece-token-len 0))
             (throw 'break2 nil))))))
    (nreverse ret)))

(defun tiktoken-count-tokens (encoding text)
  "Use ENCODING to return the token length of TEXT.

For long texts, counting tokens using this function is much
faster."
  (let* ((regex (tiktoken-encoding-pat-str encoding))
         (ranks (tiktoken-encoding-mergeable-ranks encoding))
         (ret 0))
    (let* ((matches (tiktoken--find-all-regexp-matches text regex)))
      (dolist (piece matches)
        (if-let ((token (gethash piece ranks)))
            ;; TODO try to reverse append, and nreverse the result for better perf
            (cl-incf ret)
          (let ((size (tiktoken--byte-pair-encode (string-as-unibyte piece) ranks t)))
            (cl-incf ret size)))))
    ret))

(defun tiktoken-encode (encoding text allowed-special)
  "Use ENCODING to byte-pair encode TEXT.

If ALLOWED-SPECIAL is the symbol 'all, utilize all special tokens
defined in ENCODING  If ALLOWED-SPECIAL is nil, do not allow any
special tokens.  Otherwise, ALLOWED-SPECIAL should be a list of
special tokens to use."
  (let ((allowed-special-ht
         (cond
          ((eql 'all allowed-special)
           (tiktoken-encoding-special-tokens encoding))
          ((null allowed-special) (make-hash-table :test 'equal))
          ((listp allowed-special)
           (let ((ht (make-hash-table :test 'equal)))
             (dolist (spec allowed-special)
               (puthash spec t  ht)))))))
    (tiktoken--encode-native encoding text allowed-special-ht)))

(defun tiktoken-encode-ordinary (encoding text)
  "Use ENCODING to byte-pair encode TEXT.

No special tokens are taken into account."
  (let* ((regex (tiktoken-encoding-pat-str encoding))
         (ranks (tiktoken-encoding-mergeable-ranks encoding))
         (ret '()))
    (let* ((matches (tiktoken--find-all-regexp-matches text regex)))
      (dolist (piece matches)
        (if-let ((token (gethash piece ranks)))
            ;; TODO try to reverse append, and nreverse the result for better perf
            (setq ret (cons token ret))
          (let ((tokens (tiktoken--byte-pair-encode (string-as-unibyte piece) ranks)))
            (setq ret (append tokens ret))))))
    (nreverse ret)))

(defun tiktoken-decode (encoding ids)
  "Decode a list of number IDS to underlying string using ENCODING."
  (let* ((ranks (tiktoken-encoding-mergeable-ranks encoding)))
    (let* ((inv-ht (make-hash-table :test 'equal)))
      (maphash (lambda (k v)
                (puthash v k  inv-ht))
              ranks)
      (string-to-multibyte
       (string-join
        (seq-map (lambda (id)
                   (gethash id inv-ht))
                 ids))))))


;;; Encoders

(defmemoize tiktoken-cl100k-base ()
  "Load ranks for cl100k_base and return it's encoder object."
  (let ((ranks (tiktoken-load-model-bpe tiktoken-model-cl100k-base))
        (special-tokens (let ((ht (make-hash-table :test 'equal)))
                          (puthash tiktoken-special-endoftext 100257 ht)
                          (puthash tiktoken-special-fim-prefix 100258 ht)
                          (puthash tiktoken-special-fim-middle 100259 ht)
                          (puthash tiktoken-special-fim-suffix 100260 ht)
                          (puthash tiktoken-special-endofprompt 100276 ht)
                          ht)))
    (tiktoken-encoding-create
     :name tiktoken-model-cl100k-base
     :pat-str (rx (or "'s" "'t" "'re" "'ve" "'m" "'ll" "'d"
                      (seq (? (regex "[^\r\n[:alnum:]]"))
                           (+ letter))
                      (seq (repeat 1 3 digit))
                      (seq (? " ")
                           (+ (regex "[^[:blank:][:alnum:]]"))
                           (* (in "\r\n")))
                      (seq (* (in blank))
                           (+ (in "\r\n")))
                      (seq (+ (in blank)))))
     :mergeable-ranks ranks
     :special-tokens special-tokens)))

(defmemoize tiktoken-p50k-edit ()
  "Load ranks for p50k_edit and return it's encoder object."
  (let ((ranks (tiktoken-load-model-bpe tiktoken-model-p50k-edit))
        (special-tokens (let ((ht (make-hash-table :test 'equal)))
                          (puthash tiktoken-special-endoftext 50256 ht)
                          (puthash tiktoken-special-fim-prefix 50281 ht)
                          (puthash tiktoken-special-fim-middle 50282 ht)
                          (puthash tiktoken-special-fim-suffix 50283 ht)
                          ht)))
    (tiktoken-encoding-create
     :name tiktoken-model-p50k-edit
     :pat-str (rx (or "'s" "'t" "'re" "'ve" "'m" "'ll" "'d"
                      (seq (? " ") (+ letter))
                      (seq (? " ") (+ digit))
                      (seq (? " ") (+ (regex "[^[:blank:][:alnum:]]")))
                      (seq (+ blank))))
     :mergeable-ranks ranks
     :special-tokens special-tokens)))

(defmemoize tiktoken-p50k-base ()
  "Load ranks for p50k_edit and return it's encoder object."
  (let ((ranks (tiktoken-load-model-bpe tiktoken-model-p50k-base))
        (special-tokens (let ((ht (make-hash-table :test 'equal)))
                          (puthash tiktoken-special-endoftext 50256 ht)
                          ht)))
    (tiktoken-encoding-create
     :name tiktoken-model-p50k-base
     :pat-str (rx (or "'s" "'t" "'re" "'ve" "'m" "'ll" "'d"
                      (seq (? " ") (+ letter))
                      (seq (? " ") (+ digit))
                      (seq (? " ") (+ (regex "[^[:blank:][:alnum:]]")))
                      (seq (+ blank))))
     :mergeable-ranks ranks
     :special-tokens special-tokens)))

(defmemoize tiktoken-r50k-base ()
  "Load ranks for p50k_edit and return it's encoder object."
  (let ((ranks (tiktoken-load-model-bpe tiktoken-model-r50k-base))
        (special-tokens (let ((ht (make-hash-table :test 'equal)))
                          (puthash tiktoken-special-endoftext 50256 ht))))
    (tiktoken-encoding-create
     :name tiktoken-model-r50k-base
     :pat-str (rx (or "'s" "'t" "'re" "'ve" "'m" "'ll" "'d"
                      (seq (? " ") (+ letter))
                      (seq (? " ") (+ digit))
                      (seq (? " ") (+ (regex "[^[:blank:][:alnum:]]")))
                      (seq (+ blank))))
     :mergeable-ranks ranks
     :special-tokens special-tokens)))


(defun tiktoken--encoding-from-name (encoding-name)
  "Create the model of ENCODING-NAME."
  (cond
   ((equal encoding-name tiktoken-model-cl100k-base)
    (tiktoken-cl100k-base))
   ((equal encoding-name tiktoken-model-p50k-base)
    (tiktoken-p50k-base))
   ((equal encoding-name tiktoken-model-r50k-base)
    (tiktoken-r50k-base))
   ((equal encoding-name tiktoken-model-p50k-edit)
    (tiktoken-p50k-base))
   (t (error "Unrecognized encoding name: %s" encoding-name))))

(defun tiktoken-encoding-for-model (model-name)
  "Return the encoding object of MODEL-NAME."
  (if-let ((encoding-name (gethash model-name tiktoken-model-to-encoding)))
      (tiktoken--encoding-from-name encoding-name)
    (catch 'res
      (maphash
       (lambda (k v)
         (when (string-prefix-p k model-name)
           (throw 'res (tiktoken--encoding-from-name v))))
       tiktoken-model-prefix-to-encoding)
      (error "No encoding for model %s" model-name))))

(provide 'tiktoken)
;;; tiktoken.el ends here
