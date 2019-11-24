## Test environments
On travis:
%* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.5.3 - OK
%* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.6.1 - OK
%* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R devel (2019-10-30 r77341) - OK
%*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.5.3 - OK
%*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.6.1 - OK

On appveyor:
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.6.1 - 2 NOTEs
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R devel (2019-11-23 r77455) - 2 NOTEs
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.5.3 - 2 NOTEs

Local:
 * x86_64-w64-apple-darwin15.6.0 (64-bit); macOS Mojave 10.14.4; R 3.6.1 - 2 NOTEs

check_win:
 % * x86_64-w64_mingw32 (64-bit); R devel (2019-10-29 r77335) - NOTE (DOI, see below)
  %* x86_64-w64_mingw32 (64-bit); R 3.5.3 - NOTE (DOI, see below)
  %* x86_64-w64_mingw32 (64-bit); R 3.6.1 - NOTE (DOI, see below)

R-hub:
% * Fedora Linux, R-devel, clang, gfortran - NOTE (spelling, see below)
 %* Ubuntu Linux 16.04 LTS, R-release, GCC - 1 NOTE (DOI, see below), 1 WARNING (see below)
 %* Debian Linux, R-devel, GCC, no long double - NOTE (imports, see below)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* Undefined global functions or variables: elements lower membership strprint
   These are not undefined. The NOTE is picking up a function that takes in as inputs other functions, but incorrectly thinking these are variables (as they are defined in the package).
   
* Dropping empty section 'Constructor Details'
   This is referring to one of the man-roxygen templates, for several classes a custom section is deliberately ignored as it is not required.
   
## Downstream dependencies
There are currently no downstream dependencies for this package.
