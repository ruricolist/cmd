# cmd

A utility for running external programs, built on [UIOP][].

Cmd is designed to:

1. Be natural to use.
2. Protect against shell interpolation.
3. Be usable from multi-threaded programs.
4. Support Windows.

## Argument handling

Arguments to `cmd` are *never* passed to a shell for interpretation.

Arguments are handled as follows:

1. A string is tokenized (using [cl-shlex][]) and added to the list of
   arguments.

   ``` lisp
   (cmd "ls -al")
   ≡ (uiop:run-program '("ls" "-al"))

   (cmd "echo 'hello world'")
   ≡ (uiop:run-program '("echo" "hello world"))
   ```

2. A list of strings is added directly to the list of arguments (not
   tokenized). (Putting a string in a list is “escaping” it.)

   ``` lisp
   (cmd "bash -c 'exit 1'")
   ≡ (cmd "bash -c" '("exit 1"))
   ```

3. A literal keyword, along with the next value, is passed through as
   a keyword argument to UIOP.

   ``` lisp
   (cmd "bash -c 'exit 1'" :ignore-error-status t)
   ≡ (cmd :ignore-error-status t "bash -c 'exit 1'")
   ≡ (cmd "bash -c" :ignore-error-status t '("exit 1"))
   ≡ (uiop:run-program '("bash" "-c" "exit 1") :ignore-error-status t)
   ```

   Note that unlike normal Lisp functions, keyword arguments can
   appear anywhere, not just at the end.

4. Any other string, integer, or pathname is directly added to the list
   of arguments. (It is an error if a pathname begins with `-`.)

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

Redirection is accomplished via keyword arguments. These should be
self-explanatory to anyone who has used a shell.

``` lisp
(cmd "echo 'hello world'" :> #p"hello.txt")
(cmd "cat" #p"hello.txt")
=> hello world
;; Append
(cmd "echo 'goodbye world'" :>> #p"hello.txt")
(cmd "cat" #p"hello.txt")
=> hello world
   goodbye world
(cmd "tar cf -" #p"hello.txt" :> #p"hello.tar")
(cmd "rm" #p"hello.txt")
(cmd "tar xf" #p"hello.tar")
(cmd "cat" #p"hello.txt")
=> hello world
   goodbye world
```

Supported directions are:

- `:<` Redirect stdin.
- `:>`, `:1>` Redirect stdout.
- `:>>`, `:1>>` Append stdout.
- `:2>` Redirect stderr.
- `:2>>` Append stderr.
- `:&>`, `:>&` Redirect stdout and stderr.
- `:&>>`, `:>>&` Append stdout and stderr.

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
