#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}

# ### ### ### ######### ######### #########
## Overview

## TEAPOT repository client. Manages a local installation as a
## repository (default), and a list of remote repositories to query
## and search.

## This is a very thin wrapper around the "repository::capp" type.

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# ### ### ### ######### ######### #########
## Early checks and processing

if {[string match -psn* [lindex $::argv 0]]} {
    # Strip Apple's option providing the Processor Serial Number to bundles.
    incr ::argc -1
    set  ::argv [lrange $::argv 1 end]
}

set repodebug 0
if {[string match -debug [lindex $::argv 0]]} {
    # Strip our special -debug option.
    incr ::argc -1
    set  ::argv [lrange $::argv 1 end]
    set repodebug 1
}

if {![info exists ::env(HOME)]} {
    # Prevent operation in an environment without $HOME.
    puts stderr "NOTE: The environment variable 'HOME' is not set."
    puts stderr "      $argv0 cannot operate without this variable."
    puts stderr "      Please ensure that the calling environment"
    puts stderr "      defines it."
    exit 1
}

# ### ### ### ######### ######### #########
## Requirements

package require repository::capp
package require logger
package require textutil
package require pref::teapot ; # TEAPOT preferences.

if {$::tcl_platform(platform) eq "windows"} {
    rename ::exit ::real_exit
    proc ::exit {args} {
	global exitHook
	if {[info exists exitHook]} {
	    uplevel #0 $exitHook
	}
	uplevel 1 [linsert $args 0 ::real_exit]
    }
}

# ### ### ### ######### ######### #########

pref::setGroupOrder [pref::teapot::init]

# ### ### ### ######### ######### #########
## Main code ...

proc main {} {
    global argv appname

    if {![llength $argv]} {
	set blank [string repeat " " [string length $appname]]
	puts stderr "Usage: $appname command arg..."
	puts stderr "       $blank help"
	puts stderr "       ... for more"
	exit 1
    }

    set fun [repository::capp %AUTO%]
    $fun onMD    ::md
    $fun onAbort ::Abort
    $fun onStop  ::Stop

    if {[lsearch -exact [$fun API] [lindex $argv 0]] < 0} {
	$fun abortp "Unknown command \"[lindex $argv 0]\". Use the 'help' command to get more information."
    }

    if {[catch {eval [linsert $argv 0 $fun]} msg]} {
	global errorInfo

	# Bug 67791
	if {[string match "*broken pipe*" $msg]} {
	    exit 0
	}
	if {
	    [string match "*Not a multi-architecture local repo*" $msg]
	} {
	    regsub {Error in constructor: } $msg {} msg
	    regsub {: Journal} $msg ":\n\tJournal" msg
	    $fun abort \n$msg\n
	}
	if {
	    [string match "*[lindex $argv 0]\"*is not defined*" $msg] ||
	    [string match "*method\.info*"                      $msg]
	} {
	    $fun abort "Unknown command \"[lindex $argv 0]\""
	}

	puts stderr \
	    [textutil::indent "______________________\n$errorInfo\n\
                               ______________________" \
		 "INTERNAL ERROR "]
	exit 1
    }
    return
}

proc md {{text {}}} {
    if {$text ne ""} {set text ", $text"}

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
## Application callbacks

proc Abort {text} {
    puts stderr $text
    exit 1
}

proc Stop {text} {
    puts stdout $text
    exit 0
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

if {$repodebug} {
    logger::setlevel debug
} else {
    logger::setlevel error
}

proc logger::tree::stdoutcmd {level text} {
    variable service
    puts stderr "\[[clock format [clock seconds]]\] \[$service\] \[$level\] \'$text\'"
}

# ### ### ### ######### ######### #########
## Helper to report uncaught errors in occuring in event handlers.

proc ::bgerror {args} {
    global errorCode errorInfo
    set prefix {INTERNAL ERROR (BACKGROUND) | }
    puts "${prefix}[join [split $errorInfo \n] "\n${prefix}"]"
    exit 1
    return
}
#interp bgerror {} ::bgerror

# ### ### ### ######### ######### #########
## Main processing

main
exit
