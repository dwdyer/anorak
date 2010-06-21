Anorak - A football statistics package.
(C) Copyright 2008-2010 Daniel W. Dyer

SYNOPSIS

  Anorak is a software package for generating football statistics such as
  league tables, form tables and sequences.  It includes a Publisher
  application for generating HTML pages containing a full set of statistics for
  a given football league.


LICENCE

  Anorak is licensed under the terms of the GNU General Public Licence version
  3.0 (http://www.gnu.org/licenses/gpl.html).


DEPENDENCIES

  * Attoparsec (http://hackage.haskell.org/package/attoparsec)
  * HStringTemplate (http://hackage.haskell.org/package/HStringTemplate)
  * XML (http://hackage.haskell.org/package/xml)


BUILDING FROM SOURCE

  Anorak is built using Cabal (http://www.haskell.org/cabal/) and GHC
  (http://www.haskell.org/ghc/).  First make sure that you have GHC and Cabal
  installed - the easiest way is to install the Haskell Platform
  (http://hackage.haskell.org/platform/), then use Cabal to install the above
  library dependencies:

      cabal install attoparsec
      cabal install HStringTemplate
      cabal install xml
  
  Then run the following commands to build Anorak:

      ./setup.hs configure --user
      ./setup.hs build

  This creates an executable called 'anorak' in the dist/build/anorak
  directory.


RUNNING ANORAK

  To use Anorak to generate a set of HTML pages, run the following command:

      ./anorak publish path/to/config.xml

  Input and output configuration options are specified in the XML configuration
  file.

