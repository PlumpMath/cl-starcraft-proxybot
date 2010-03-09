# CL-STARCRAFT-PROXYBOT

## A StarCraft ProxyBot client for Common Lisp

### IMPORTANT!

This is a first upload to GitHub mainly for my own convenience.  It
isn't really ready for public consumption yet.

(So don't go whoring karma on HN or Reddit yet!)


### Introduction

This is a client for [ProxyBot](http://code.google.com/p/bwapi-proxy/)
(v2.6.1 as of this writing), which is project to make the
[BWAPI](http://code.google.com/p/bwapi/) for StarCraft available to
other languages than C++.

For this to work you need to following besides my project:

* [BWAPI Beta 2.6.1](http://bwapi.googlecode.com/files/BWAPI_Beta_2.6.1.zip)
* [Chaoslauncher](http://www.teamliquid.net/forum/viewmessage.php?topic_id=65196)
* [ProxyBot 2.6.1](http://bwapi-proxy.googlecode.com/files/ProxyBot-2.6.1.zip)
* [StarCraft 1.16.1](http://www.blizzard.com/store/details.xml?rhtml=y&id=110000124)

(You can upgrade StarCraft to 1.16.1 by connecting to a Battle.net server.)

Check the README for BWAPI for installation instructions except that
you don't need to compile an ExampleAIModule.dll.  We'll be using the
one that comes with ProxyBot (ProxyBot-2.6.1/client/ExampleAIModule.dll).


### License

This project is released under the simplified
[BSD](http://www.opensource.net/licenses/bsd-license.php) license.


### Documentation

None.  There's extensive documentation on the
[BWAPI wiki](http://code.google.com/p/bwapi/wiki/UsingBWAPI).

Also see the code and examples/fatalist.lisp.  If all the dependencies
have been installed correctly you should be able to run
examples/fatalist.sh which will wait for a connection from the
StarCraft ProxyBot.


### Platforms

The latest Git checkout has been tested on the following platforms:

* CCL 1.3-r12775M / Windows Vista SP2
* SBCL 1.0.31.0.debian / Debian Linux 2.6.32-trunk-686

If you don't develop on Windows: StarCraft is an old game and runs
fine in a virtual machine like [VirtualBox](http://www.virtualbox.org/).

I have not been able to run StarCraft with the BWAPI from
Chaoslauncher under Wine.
