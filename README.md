# cmd

A utility for running external programs, built on
[`uiop:launch-program`][UIOP].

Cmd is designed to:

1. Be natural to use.
2. Protect against shell interpolation.
3. Be usable from multi-threaded programs.
4. Support Windows.

## Argument handling

Arguments to `cmd` are *never* passed to a shell for interpretation.

Arguments are usually:
- strings
- keywords
- lists of strings and keywords

Some other types get special handling. Nested lists are not allowed.

Arguments are handled as follows:

1. A string is tokenized (using [cl-shlex][]) and added to the list of
   arguments.

   ``` lisp
   (cmd "ls -al")
   ≅ (uiop:wait-process (uiop:launch-program '("ls" "-al")))

   (cmd "echo 'hello world'")
   ≅ (uiop:wait-process (uiop:launch-program '("echo" "hello world")))
   ```

   Redirection operators in the tokenized string (such as `<`, `>`, or
   `|`) are translated into keywords (see below).

   ```lisp
   (cmd "echo 'hello world' > myfile")
   ≡ (cmd '("echo" "hello world" :> "myfile"))
   ```

2. A list is added directly to the list of arguments (not tokenized).
   (Putting a string in a list is “escaping” it.)

   ``` lisp
   (cmd "bash -c 'exit 1'")
   ≡ (cmd "bash -c" '("exit 1"))
   ```

   Keywords in the list are treated exactly like keywords as
   arguments.

3. Keywords that are subcommand dividers (like `|`) are handled
   internally by `cmd`. Otherwise, a keyword, along with the next
   value, is used as a keyword argument to UIOP.

   ``` lisp
   (cmd "bash -c 'exit 1'" :ignore-error-status t)
   ≡ (cmd :ignore-error-status t "bash -c 'exit 1'")
   ≡ (cmd :check nil "bash -c 'exit 1'")
   ≡ (cmd "bash -c" :ignore-error-status t '("exit 1"))
   ≡ (cmd "bash -c" :check nil '("exit 1"))
   ```

   Note that unlike normal Lisp functions, keyword arguments can
   appear anywhere, not just at the end.

   Also note `:check` is accepted as an alias for
   `:ignore-error-status`, although the value is negated before being
   passed to UIOP.

4. Any character, integer, or pathname is directly added to the list
   of arguments, as if it were an escaped string. (It is an error if a
   pathname begins with `-`.)

5. Cmd supports a basic form of process substitution, running
   processes as input to commands expecting files. To construct a
   process substitution, use the `psub` Lisp function.

   ``` lisp
   (cmd? "diff" (psub "echo x") (psub "echo x"))
   => T

   (cmd? "diff" (psub "echo x") (psub "echo y"))
   => NIL
   ```

   (For this specific case, however – passing a string to a command
   expecting a file – use `psub-echo` or `psub-format`, which don’t
   actually call an external program.)

### Parsing the `cmd` DSL

You can use the `cmd` DSL in your own programs with `cmd:parse-cmd-dsl`. This takes a list of arguments and returns two values: a fully-tokenized list of command arguments and a list of keyword arguments, both suitable for passsing to `uiop:launch-program`.

``` lisp
(parse-cmd-dsl '("echo 'hello world' > myfile"))
=> ("echo" "hello world"), (:OUTPUT "myfile")
```

## The external program’s working directory

Cmd is designed with multi-threaded programs in mind. It always runs
programs with their working directory relative to
[`*default-pathname-defaults*`][dpd]. This is because the OS-level
working directory of a program, on both Windows and Unix, is the working
directory for the entire process, not the individual thread, and
changing it changes it for all threads.

You can also specify the directory for a particular command with the
keyword argument `:in`:

``` lisp
(cmd "ls" :in #p"/")
(cmd :in #p"/" "ls")
=> /bin /home /tmp /usr ...
```

For convenience Cmd supplies the macro `with-working-directory`:

``` lisp
(with-working-directory (dir)
  (cmd ...)
  (cmd ...))
≡ (progn
    (cmd :in dir ...)
    (cmd :in dir ...))
```

## The external program’s environment

For Unix users only, the variable `*cmd-env*` holds an alist of extra
environment variables to set for each call to `cmd`.

``` lisp

(let ((*cmd-env* (acons "GIT_PAGER" "cat" *cmd-env*)))
  (cmd "git diff" ...))
```

We are currently very restrictive about what we consider a valid
environment variable name.

### Controlling PATH

For controlling the `PATH` environment variable, the Lisp variable
`*cmd-path*` can be used:

``` lisp
(let ((*cmd-path* (cons #p"~/.local/bin" *cmd-path*)))
   ...)
```

Directories in `*cmd-path*` are prepended to `PATH`.

This uses the same mechanism as `*cmd-env*`, so it also only works on
Unix.

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
  stripping any trailing newline. (Much like `$(cmd)` in a shell.) The
  exit code is returned as a second value.

  ```lisp
  ($cmd "date")
  => "Sun Sep 27 15:43:01 CDT 2020", 0
  ```

- `cmd!` runs an external program purely for side effects, discarding
  all output and returning nothing. If the program exits non-zero,
  however, it will still signal an error.

- `cmd?` returns `t` if the external program returned `0`, and `nil`
  otherwise, with the exit code as a second value. As other variants
  by default signal an error if the process exists non-zero, `cmd?` is
  useful for programs expected to fail.

  ```lisp
  (cmd? "kill -0" pid)
  => T, 0   ;; PID is a live process
  => NIL, 1 ;; PID is not a live process
  ```

- `cmd&` runs an external program asynchronously (with
  `uiop:launch-program`) and returns a UIOP `process-info` object.

  ```lisp
  (cmd& "cp -a" src dest)
  => #<PROCESS-INFO ...>
  ```

## Error handling

By default, Cmd stores the stderr of a process, and if there is an
error (due to non-zero exit) it presents the stderr as part of the
error message.

Accordingly `cmd` errors are a subclass of `uiop:subprocess-error`. The
stored stderr can be accessed with `cmd:cmd-error-stderr`.

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

;;; Equivalents using tokenized strings.
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

Note redirections are interpreted according to the rules for Lisp
keywords (only the first occurrence of a keyword argument matters),
not the side-effecting rules for redirections in POSIX shells.

### Pipelines

The simplest way to set up pipelines is to use tokenized strings:

``` lisp
(cmd "cat /usr/share/dict/words | sort | uniq -c | sort -nrs | head -3")
=>    1 a
      1 A
      1 Aachen
```

Alternately you can use keywords. While `:|\||` is acceptable, you can write `"|"` instead. (Remember `"|"` will be tokenized to `'(:|\||)`.)

``` lisp
(cmd "cat /usr/share/dict/words"
     "|" '("sort")
     "|" '("uniq" "-c")
     "|" '("sort" "-nrs")
     "|" '("head" "-3"))
=>    1 a
      1 A
      1 Aachen
```

Again, separating out the pipeline symbols is usually more readable when the subcommands are computed.

## Controlling cmd with hooks

There are two hooks you can use to control `cmd`. These are exported from the `cmd/hooks` package (so you can `:use :cmd` without having to worry about them.) Both hooks expect a list of functions of one argument.

The hook `*message-hook*` is called with the external program and its arguments, quoted as a shell command line. This can be useful for logging commands as they are run.

The hook `*proc-hook*` is called with the process object (as returned by `uiop:launch-program`). This can be useful if you want to be able to track what is being run in a particular dynamic extent.

## Windows

On Windows only, the first argument (the program name) has `.exe` appended to it automatically if it doesn’t already have a file extension.

## Efficiency

While `cmd` does not use a shell to interpret its arguments, it may still have to run a shell (`sh` on Unix, `cmd.exe` on Windows) in order to change the working directory of the program.

How inefficient this is depends on what your distribution uses as `/bin/sh`; it is faster when `/bin/sh` is, say, `dash`, than when it is `bash`.

Recent versions of GNU `env` support a `-C` switch to do this directly. When support is detected dynamically, then `env -C` is used in place of a shell and overhead is negligible.

## Past

Cmd is a spinoff of [Overlord][], a Common Lisp build system, and was
inspired by the `cmd` function in [Shake][], a Haskell build system,
as well as the [Julia][] language’s [shell command
facility][backtick]. The `psub` function is inspired by the
[builtin][psub] of the same name in the [Fish shell][].

## Future

- Pipelines should have “pipefail” behavior.
- Pipelines should support stderr as well (`2|`, `&|`).
- Efferent process substitution should also be supported.
- There should be a special variable holding an alist of extra
  environment variables to set when running a command. (The problem
  here is Windows.)

[UIOP]: https://common-lisp.net/project/asdf/uiop.html
[Overlord]: https://github.com/ruricolist/overlord
[Shake]: https://shakebuild.com/
[cl-shlex]: https://github.com/ruricolist/cl-shlex
[dpd]: http://clhs.lisp.se/Body/v_defaul.htm
[Bernstein chaining]: http://www.catb.organization/~eser/writings/taoup/html/ch06s06.html
[Julia]: https://julialang.org
[backtick]: https://julialang.org/blog/2013/04/put-this-in-your-pipe/
[Fish shell]: https://fishshell.com
[psub]: https://fishshell.com/docs/current/cmds/psub.html
