Enbuske

Ben Swanson 
Brown University
2013

If you are interested in just using Enbuske to get TSGs, please
consider using the current release, available for download at

www.cs.brown.edu/~chonger/enbuske/

If you'd like to hack on the code, then you're in the right place.

--------------------------------------------------------------


Enbuske is written in scala, and here in Github we have the sbt
project.  After cloning this project, please make sure you have
sbt ("simple build tool") installed.  Simply run sbt from the cloned
directory (the one containing "build.sbt") and you'll be ready to go.

The downloadable package contains demo data and shell scripts for
easy use, and to see your local changes reflected in it, please follow
these steps

1) run the sbt target "assembly" to produce some file ???.jar in the
   targets dir

2) overwrite enbuske.jar with ???.jar in the current Enbuske release
   folder


--------------------------------------------------------------


Please note : Many of the core data structures are from a different
project, Multitool (also on github).  You may want to clone this too.
I recommend putting Multitool next to Enbuske, and changing the real
multitool.jar in the lib folder into a symlink into the target folder
of Multitool.  That way you can edit Multitool, run "package", and
your changes will be made available to your Enbuske instance.



Enjoy the Experience.
