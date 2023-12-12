# python-pickle

Reading and writing Python pickle files, or at least a best effort to do that.

## Introduction

## Python's pickle protocol

Python's pickle protocol consists out of opcodes and optionally parameters that guide the system through loading a file. The machine has a stack and a dictionary to process data. The protocol has not really a fixed prefix as file, but it normally starts with `0x80` as first opcode to set the protocol version, and then `0x05` or `0x04`, the most common protocol numbers. The last opcode is normally a `0x2e`, or a dot, that stops the parser from reading.

For parsing, the protocol has only some influence with regarding to loading the correct modules.
