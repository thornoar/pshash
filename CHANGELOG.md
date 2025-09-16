# Revision history for pshash

## 0.1.16.1

* Improved file encryption (now supports non-text files).

## 0.1.16.0

* Added text encryption functionality.

## 0.1.15.1

* Fixed number formatting in the help message.

## 0.1.15.0

* Rewrote the config file reading logic, + minor stuff

## 0.1.14.7

* Made the `--pure` behavior default, overridable with `--impure`.

## 0.1.14.6

* Split the program into modules, changed help messages.

## 0.1.14.5

* Fixed the combination of patching and file reading.

## 0.1.14.4

* Fixed newlines on Windows.

## 0.1.14.3

* Reversed the repeat functionality, now off by default.

## 0.1.14.2

* Fixed minor bugs, added `--no-repeat` and `--show` options.

## 0.1.14.1

* Added repeat functionality to ensure correct keys.

## 0.1.14.0

* Added private key concealment, added prompts. Implemented exit codes and redirected errors to `stderr`.

## 0.1.13.5

* Option map optimization, better error handling.

## 0.1.13.4

* More compile-time optimizations.

## 0.1.13.3

* Simlified the `factorial` and `factorial'` implementations.

## 0.1.13.2

* Changed `!` to `+` in low-level options, as `!` is used as shell syntax.

## 0.1.13.1

* Minor adjustments for Windows

## 0.1.13.0

* Added `@color` and `@no-color` options, `@all` config file keyword.

## 0.1.12.0

* Added a `--bo-color` flag to disable error message coloring (mostly useful on Windows).

## 0.1.11.0

* Added public key patching, restructured `--help` message.

## 0.1.10.3

* Minor error changes (removed quotes in some places).

## 0.1.10.2

* Reverted error tree style, fixed minor bugs.

## 0.1.10.1

* Changed error tree mechanism, strictified unrecognized option errors, changed some naming.

## 0.1.9.1

* Fixed bugs, cleaned up code.

## 0.1.9.0

* Added configuration files. See `--help`.

## 0.1.8.1

* Cleaned up code.

## 0.1.8.0

* Improved error messages, implemented limit on number of pairs printed using the `-l` option.

## 0.1.7.0

* Redesigned error catching, fixed minor bugs.

## 0.1.6.3

* Improved error message formatting.

## 0.1.6.2

* Added error tracing.

## 0.1.6.1

* Eliminated compiler warnings.

## 0.1.6.0

* More error catching, now the algorithm checks validity of configuration settings

## 0.1.5.0 -- 15.11.2024

* Implemented error handling.

## 0.1.4.0 -- 15.11.2024

* Added query functionality and password cracking functions.

## 0.1.2.1 -- 04.11.2024

* Changed the `--help` information to include version.

## 0.1.0.0 -- dd.mm.YYYY

* First version. Released on an unsuspecting world.
