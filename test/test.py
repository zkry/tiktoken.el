import sys
import tiktoken
enc = tiktoken.get_encoding("cl100k_base")
encoding = enc.encode(sys.argv[1])
print(len(encoding))
