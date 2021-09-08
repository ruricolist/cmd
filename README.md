# cmd

A utility for running external programs, built on [UIOP][].

Cmd is designed to:

1. Be natural to use.
2. Protect against shell interpolation.
3. Be usable from multi-threaded programs.
4. Support Windows.

## Argument handling

Arguments to `cmd` are *never* passed to a shell for interpretation.

Arguments can be either strings, keywords, or lists of strings and keywords.

Arguments are handled as follows:

1. A string is tokenized (using [cl-shlex][]) and added to the list of
   arguments.

   ``` lisp
   (cmd "ls -al")
   ≡ (uiop:run-program '("ls" "-al"))

   (cmd "echo 'hello world'")
   ≡ (uiop:run-program '("echo" "hello world"))
   ```

   Redirection operators in the tokenized string (such as `<`, `>`, or
   `|`) are translated into keywords (see below).

   ```lisp
   (cmd "echo 'hello world' > myfile")
   ≡ (cmd '("echo" "hello world" :> "myfile"))
   ≡ (uiop:run-program '("echo" "hello world") :output "myfile")
   ```

2. A list of strings is added directly to the list of arguments (not
   tokenized). (Putting a string in a list is “escaping” it.)

   ``` lisp
   (cmd "bash -c 'exit 1'")
   ≡ (cmd "bash -c" '("exit 1"))
   ```

   Keywords in the list are treated exactly like keywords as
   arguments.

3. Keywords that are subcommand dividers (like `|`) are handled
   internally by `cmd`. Otherwise, a literal keyword, along with the
   next value, is passed through as a keyword argument to UIOP. (Note
   that Cmd supports abbreviations for some UIOP keywords; see below.)

   ``` lisp
   (cmd "bash -c 'exit 1'" :ignore-error-status t)
   ≡ (cmd :ignore-error-status t "bash -c 'exit 1'")
   ≡ (cmd "bash -c" :ignore-error-status t '("exit 1"))
   ≡ (uiop:run-program '("bash" "-c" "exit 1") :ignore-error-status t)
   ```

   Note that unlike normal Lisp functions, keyword arguments can
   appear anywhere, not just at the end.

4. Any character, integer, or pathname is directly added to the list
   of arguments, as if it were a string. (It is an error if a pathname
   begins with `-`.)

## The external program’s working directory

Cmd is designed with multi-threaded programs in mind. It always runs
programs with their working directory relative to
[`*default-pathname-defaults*`][dpd]. This is because the OS-level
working directory a program, on both Windows and Unix, is the working
directory of the entire process, not the individual thread, and
changing it changes it for all threads.

You can also specify the directory for a particular command with the
keyword argument `:in`:

``` lisp
(cmd "ls" :in #p"/")
(cmd :in #p"/" "ls")
=> /bin /home /tmp /usr ...
```

## Entry points

The `cmd` package offers several entry points:

- `cmd` runs an external program synchronously, returning the exit
  code. By default, on a non-zero exit it signals an error.

  ```lisp
  (cmd "cat /etc/os-release")
  NAME="Ubuntu" [...]
  => 0
  ```

- `$cmd` returns the output of the external program as a string,
  stripping any trailing newline. (Much like `$(cmd)` in a shell.)

  ```lisp
  ($cmd "date")
  => "Sun Sep 27 15:43:01 CDT 2020"
  ```

- `cmd?` returns `t` if the external program returned `0`, and `nil`
  otherwise, with the exit code as a second value. As other variants
  by default signal an error if the process exists non-zero, `cmd?` is
  useful for programs that are expected to fail.

  ```lisp
  (cmd? "kill -0" pid)
  => T # PID is a live process
  => NIL # PID is not a live process
  ```

- `cmd&` runs an external program asynchronously (with
  `uiop:launch-program`) and returns a UIOP `process-info` object.

  ```lisp
  (cmd& "cp -a" src dest)
  => #<PROCESS-INFO ...>
  ```

## Redirection

Redirection is accomplished via either tokenized strings or keyword
arguments. These should be self-explanatory to anyone who has used a
shell.

``` lisp
;;; Using keyword arguments.
(cmd "echo 'hello world'" :> "hello.txt")
(cmd "cat hello.txt")
=> hello world
;; Append
(cmd "echo 'goodbye world'" :>> "hello.txt")
(cmd "cat hello.txt")
=> hello world
   goodbye world
(cmd "tar cf - hello.txt" :> #p"hello.tar")
(cmd "rm hello.txt")
(cmd "tar xf hello.tar")
(cmd "cat hello.txt")
=> hello world
goodbye world

;;; Using tokenized strings.
(cmd "echo 'hello world' > hello.txt")
(cmd "cat hello.txt")
=> hello world
;; Append
(cmd "echo 'goodbye world' >> hello.txt")
(cmd "cat hello.txt")
=> hello world
goodbye world
(cmd "tar cf - hello.txt > hello.tar")
(cmd "rm hello.txt")
(cmd "tar xf hello.tar")
(cmd "cat hello.txt")
=> hello world
goodbye world

```

Redirection with keyword arguments is usually more readable when the arguments are computed.

Supported directions include:

- `:<` Redirect stdin.
- `:>`, `:1>` Redirect stdout.
- `:>>`, `:1>>` Append stdout.
- `:2>` Redirect stderr.
- `:2>>` Append stderr.
- `:&>`, `:>&` Redirect stdout and stderr.
- `:&>>`, `:>>&` Append stdout and stderr.
- `:<<<` Provide input from a “here string”.

Note that redirections are interpreted according to the rules for Lisp
keywords (only the first occurrence of a keyword argument matters),
not the side-effecting rules for redirections in POSIX shells.

### Pipelines

The simplest way to set up pipelines is to use tokenized strings:

``` lisp
(cmd "cat /usr/share/dict/words | sort | uniq -c | sort -nr | head -3")
=>    1 études
      1 étude's
      1 étude
```

Alternately you can use keywords. While `:|\||` is acceptable, you can write `:pipeline` instead:

``` lisp
(cmd "cat /usr/share/dict/words"
     :pipeline "sort"
     :pipeline "uniq -c"
     :pipeline "sort -nr"
     :pipeline "head -3")
=>    1 études
      1 étude's
      1 étude
```

Again the keyword syntax is usually more readable when the subcommands are computed.

## Controlling cmd with hooks

There are two hooks you can use to control `cmd`. These are exported from the `cmd/hooks` package (so you can `:use :cmd` without having to worry about them.) Both hooks expect a list of functions of one argument.

The hook `*message-hook*` is called with the external program and its arguments, quoted as a shell command line. This can be useful for logging commands as they are run.

The hook `*proc-hook*` is called with the process object (as returned by `uiop:launch-program`). This can be useful if you want to be able to track what is being run in a particular dynamic extent.

## Windows

On Windows only, the first argument (the program name) has .exe appended to it automatically if it doesn’t already have a file extension.

## Efficiency

While `cmd` does not use a shell to interpret its arguments, it does still have to run a shell (`sh` on Unix, `cmd.exe` on Windows) in order to change the working directory of the program.

How inefficient this is depends on what your distribution uses as a shell; it is faster when `sh` is, say, `dash`, than when it is `bash`.

Recent versions of GNU `env` support a `-C` switch to do this directly. When that is supported (support is detected dynamically) then `env -C` is used in place of a shell and overhead is negligible.

## Past

Cmd is a spinoff of [Overlord][], a Common Lisp build system, and was inspired by the `cmd` function in [Shake][], a Haskell build system.

## Future

I plan to support at least inline redirection (e.g. `(cmd "sth file > other-file")`) and pipelines.

[UIOP]: https://common-lisp.net/project/asdf/uiop.html
[Overlord]: https://github.com/ruricolist/overlord
[Shake]: https://shakebuild.com/
[cl-shlex]: https://github.com/ruricolist/cl-shlex
[dpd]: http://clhs.lisp.se/Body/v_defaul.htm
[Bernstein chaining]: http://www.catb.organization/~eser/writings/taoup/html/ch06s06.html
