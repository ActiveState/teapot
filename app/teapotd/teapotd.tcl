#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}

# ### ### ### ######### ######### #########
## Overview

# Repository Web Server Application. This application talks HTTP/HTML
# using the Repo Web API with any repository web client connecting to
# its listening port.

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# ### ### ### ######### ######### #########
## Requirements

if {[string match -psn* [lindex $::argv 0]]} {
    # Strip Apple's option providing the Processor Serial Number to bundles.
    incr ::argc -1
    set  ::argv [lrange $::argv 1 end]
}

package require repository::sqlitedir
package require repository::localma
package require teapot::wserver
package require teapot::metadata::container
package require logger

# ### ### ### ######### ######### #########
## Main code ...

proc main {} {
    global argv fs

    # Standard methods, retrieval of version, metadata.
    if {([llength $argv] == 1) && [lindex $argv 0] eq "version"} {
	puts \n\t[[md {version unknown}] version]\n
	exit 0
    }
    if {([llength $argv] == 1) && [lindex $argv 0] eq "who"} {
	puts \n[teapot::metadata::write::getStringHumanC [md]]\n
	exit 0
    }

    if {[lindex $argv 0] ne "serve"} usage

    set argv [lrange $argv 1 end]

    if {[llength $argv] < 1} usage

    set caching 0
    set file(-header) {}
    set file(-footer) {}
    while {[string match -* [set opt [lindex $argv 0]]]} {
	if {$opt eq "--cache"} {
	    set caching 1
	    set argv [lrange $argv 1 end]
	} elseif {$opt eq "-debug"} {
	    logger::setlevel debug
	    set argv [lrange $argv 1 end]
	} elseif {($opt eq "-header") || ($opt eq "-footer")} {
	    set c [lindex $argv 1]
	    if {![file exists   $c]} { usageHEADER $c "File does not exist" }
	    if {![file isfile   $c]} { usageHEADER $c "File is no such" }
	    if {![file readable $c]} { usageHEADER $c "File is not readable" }
	    set file($opt) $c
	    set argv [lrange $argv 2 end]
	} else {
	    usage
	}
    }

    if {[llength $argv] > 2} usage

    if {[llength $argv] == 2} {
	foreach {base port} $argv break
	if {![string is integer -strict $port] || ($port <= 0)} \
	    usage
    } else {
	set base [lindex $argv 0]
	set port 46336 ; # 1-800-(TEAPOT) % 65536
    }

    # Note: We will not need a writable repository even if we enable
    # web upload. The plan for that is to put uploads into a separate
    # area for examination. However for caching to be possible we have
    # to be able to create the cache directory, or, if existing, this
    # has to be writable.

    if {[catch {::repository::api typeof $base} msg]} {
	usageRepo $base $msg
    }

    set cachedir ""
    if {$caching} {
	set cachedir [file join $base htmlcache]
	if {[file exists $cachedir]} {
	    if {![file writable $cachedir]} {
		usageRepoCache $base "Cache directory is not writable."
	    }
	} else {
	    if {[catch {file mkdir $cachedir}]} {
		usageRepoCache $base "Unable to create the cache directory."
	    }
	}
    }

    set who \n[teapot::metadata::write::getStringHumanC [md]]\n

    set fs [repository::api open %AUTO% $base -readonly 1]
    set sv [teapot::wserver       %AUTO% \
		-repo     $fs       \
		-port     $port     \
		-cachedir $cachedir \
		-html     1         \
		-md       $who      \
		-header   $file(-header) \
		-footer   $file(-footer) \
	       ]

    vwait ::done
    return
}

proc md {{text {}}} {
    global appmode
    if {$text ne ""} {set text ", $text"}

    if {$appmode eq "unwrapped"} {
	# Fake metadata when running unwrapped for debugging.
	set p [teapot::metadata::container %AUTO%]
	$p define teapot 0.0 application
	$p add platform        tcl
	$p add author          {ActiveState Software Inc}
	return $p
    }

    set mdf [file join $::starkit::topdir teapot.txt]
    if {![file exists $mdf]} {
	puts stderr "No metadata available$text"
	exit 1
    }
    set packages [teapot::metadata::read::fileEx $mdf single errors]
    if {![llength $packages]} {
	puts stderr "Bad meta data$text"
	exit 1
    }
    return [lindex $packages 0]
}

# ### ### ### ######### ######### #########
## Basic helpers ...

proc usage {} {
    global appname
    puts stderr "Usage: $appname serve ?--cache? ?-header HEADERFILE? ?-footer FOOTERFILE? repodir ?port?"
    puts stderr "       $appname version"
    puts stderr "       $appname who"
    exit 1
}

proc usageHEADER {path msg} {
    global appname
    set    blank__ [blankstr $appname]
    puts stderr "Usage: $appname serve ?--cache? ?-header HEADERFILE? ?-footer FOOTERFILE? repodir port"
    puts stderr "       $blank__ \"$path\" is not a valid header file.\n       $blank__ $msg"
    exit 1
}

proc usageRepo {path msg} {
    global appname
    set    blank__ [blankstr $appname]
    puts stderr "Usage: $appname serve ?--cache? ?-header HEADERFILE? ?-footer FOOTERFILE? repodir port"
    puts stderr "       $blank__ \"$path\" is not a valid repository.\n       $blank__ $msg"
    exit 1
}

proc usageRepoCache {path msg} {
    global appname
    set    blank__ [blankstr $appname]
    puts stderr "Usage: $appname serve ?--cache? ?-header HEADERFILE? ?-footer FOOTERFILE? repodir port"
    puts stderr "       $blank__ \"$path\" HTML caching not possible.\n       $blank__ $msg"
    exit 1
}

proc blankstr {text} {
    # Replaces a string with equivalent whitespace.  All characters
    # except tab are replaced with a space.  Tab is whitespace, and
    # has to remain, or the replacement string will have different tab
    # stops.

    regsub -all -- {[^ 	]} $text { } text
    return $text
}

# ### ### ### ######### ######### #########
## Tracing configuration
## (1) Disabled information stuff, allow only errors and above
## (2) Standard logging to stderr, not stdout

#logger::setlevel error
logger::setlevel info
#logger::setlevel debug

proc logger::tree::repository::stdoutcmd {level text} {
    variable service
    puts stderr "\[[clock format [clock seconds]]\] \[$service\] \[$level\] \'$text\'"
}

# ### ### ### ######### ######### #########
## Main processing

main
exit
