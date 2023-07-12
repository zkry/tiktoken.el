# tiktoken.el: An Emacs Lisp port of BPE tokenizer tiktoken

tiktoken.el is a port of the widely used BPE tokenizer
[tiktoken](https://github.com/openai/tiktoken).  It is capable of
encoding and decoding using a variety of models.  As many LLM
applications are being build in Emacs, I thought it would be usefull
to have a BPE to count the number of tokens used for these
applications.

# Usage

tiktoken.el provides BPE tokenizers for a variety of OpenAI models.
You can obtain the encoding object for a model as follows:

```lisp
(tiktoken-encoding-for-model "gpt-3.5-turbo")
;; => #s(tiktoken-encoding "cl100_base" ...)
```

The model data is fetched from a URL and saved locally according to
the variable `tiktoken-cache-dir`.

If you know the encoding you need, you can create it directly with the functions `(tiktoken-cl100k-base)`, `(tiktoken-p50k-edit)`, `(tiktoken-p50k-base)`, or `(tiktoken-r50k-base)`

Once you have the encoding object, you can use it to encode or decode text:

```lisp
(let ((enc (tiktoken-encoding-for-model "gpt-3.5-turbo")))

  (tiktoken-encode enc "This is a test!" nil)
  ;; => (2028 374 264 1296 0)

  (tiktoken-decode enc (tiktoken-encode enc "This is a test!" nil))
  ;; => "This is a test!"

  (tiktoken-count-tokens enc "This is a test!")
  ;; => 5
  )
```

If you are using this library to count tokens, I would recommend using the function `tiktoken-count-tokens` as it is much faster.
