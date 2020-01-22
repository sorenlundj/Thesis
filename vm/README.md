user:		neumann
password:	hamster

* start vm with local thesis code
	- run 'startup.sh'
	- run 'setup_rev_mount.sh /Users/Soren/Documents/DIKU/Thesis/code' if not set to right directory
	- run 'rev_mount.sh'
	- run 'login.sh'
	- run 'mount.sh disk'
	- cd into disk and path set in step 2 will be available


###INSTALLS FOR VB

---
sudo apt-get install haskell-platform
cabal update
cabal install row-types
cabal install indexed
cabal install indexed-extras
cabal install tasty
cabal install tasty-hunit