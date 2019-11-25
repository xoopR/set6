## Test environments
On travis:
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.5.3 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.6.1 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R devel (2019-10-30 r77341) - OK
*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.5.3 - OK
*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.6.1 - OK

On appveyor:
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.6.1 - OK
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R devel (2019-11-23 r77455) - OK
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.5.3 - OK

Local:
 * x86_64-w64-apple-darwin15.6.0 (64-bit); macOS Mojave 10.14.4; R 3.6.1 - OK

check_win:
  * x86_64-w64_mingw32 (64-bit); R devel (2019-10-29 r77335) - 1 NOTE
  * x86_64-w64_mingw32 (64-bit); R 3.5.3 - 1 NOTE
  * x86_64-w64_mingw32 (64-bit); R 3.6.1 - 1 NOTE

R-hub:
 * Fedora Linux, R-devel, clang, gfortran - OK
 * Ubuntu Linux 16.04 LTS, R-release, GCC - OK
 * Windows Server 2008 R2 SP1, R-devel, 32/64 bit - OK

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
   
* Possibly mis-spelled words in DESCRIPTION: tuples (14:138)
  * It isn't! 
   
## Downstream dependencies
There are currently no downstream dependencies for this package.
