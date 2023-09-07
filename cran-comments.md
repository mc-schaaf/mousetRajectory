## Test environments

* local: Windows 10 install, R 4.3.0
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran
* R-hub: Windows Server 2022, R-devel, 64 bit
* R-project: macbuilder
* R-project: Windows Server 2022, R-devel, 64 bit



## R CMD check results

0 errors | 0 warnings | 4 notes


### All platforms return this NOTE:
```
Maintainer: 'Roland Pfister <mail@roland-pfister.net>'

New submission

Possibly misspelled words in DESCRIPTION:
  Wirth (25:67)
  al (25:76)
  et (25:73)
```
This is to be expected as this is a new release. The spelling of the surname Wirth is correct, as is the "et al." part of the citation (at least when using APA 7th citation standards)



### Only on R-rub Windows: 
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
and 
```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```
As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560) and [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), both notes seem to be an R-hub issue and can likely be ignored. 


### Only on R-hub Fedora Linux:
```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```
This also seems to be a recurring issue on Rhub [R-hub issue #548](https://github.com/r-hub/rhub/issues/548) and can likely also be ignored.
