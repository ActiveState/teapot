# TEAPOT Admin Entry
#
# - Initialization of wrap support (VFS, Mk4FS foundation, ...)
# - License check !NONE!
# - Invokation of the actual application.
# - (Inactive code for the debugging of the package management, and file sourcery.)

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

if 0 {
    # Debugging of 'package require statements.
    rename ::package ::__package
    proc ::package {args} {
	if {[lindex $args 0] == "ifneeded"} {
	    if {[string match tcldevkit* [lindex $args 1]]} {
		puts ">>> [info script]"
		puts "    $args"
		puts ""
	    }
	}
	uplevel 1 ::__package $args
    }
}

if 0 {
    # Debugging of 'source' statements.
    rename ::source ::__source
    proc ::source {args} {
	puts SOURCE\ [join $args]
	uplevel 1 ::__source $args
    }
}

package require starkit
switch -exact -- [starkit::startup] {
    unwrapped {
	# Unwrapped call - During development from within the local
	# perforce depot area. Slightly different location of lib dir.
	# Hence we use two stanza's to define an externa lib directory.
	# Debug output is allowed, actually sort of wanted to be sure of
	# package locations.

	lappend auto_path [file join [file dirname [file dirname $starkit::topdir]] devkit lib]
	lappend auto_path [file join [file dirname [file dirname $starkit::topdir]] lib]

	puts unwrapped\n[join $auto_path \n\t]
	set appname $argv0
    }
    starkit {
	set appname $argv0
    }
    starpack {
	set appname [info nameofexecutable]
    }
}
set appname [file tail $appname]

global auto_path
foreach d $auto_path {
    foreach pd [glob -nocomplain -directory $d P-*] {
	lappend auto_path $pd
    }
}

if 0 {
    # This application runs without checking a license.

    # Load parser. This checks the license.
    if {[catch {
	package require parser
	tdk_license user-name
    } err]} {
	# Parser package missing, we cannot work.
	if {![string match "*license*" $err]} {
	    set err "Failed license check"
	}
	if {[catch {
	    package require Tk
	    wm withdraw .
	    tk_messageBox -icon error -title "Invalid License" \
		-type ok -message $err
	}]} {
	    puts stderr $err
	}
	exit 1
    }
}

if 0 {
    rename exit _exit
    proc exit {{status 0}} {
	set tmp {}
	set mlp 0
	set mlv 0

	foreach package [lsort [package names]] {
	    foreach version [package versions $package] {
		if {[catch {package present $package}]} continue
		if {![string equal $version [package provide $package]]} continue

		set ifneeded \
		    [string replace \
			 [string trim \
			      [string map {"\n" " " "\t" " "} \
				   [package ifneeded $package $version]]] \
			 100 end "..."]

		lappend tmp [list $package $version $ifneeded]
		if {[set l [string length $package]] > $mlp} {set mlp $l}
		if {[set l [string length $version]] > $mlv} {set mlv $l}
	    }
	}

	foreach entry $tmp {
	    foreach {package version ifneeded} $entry break

	    puts [format "%-${mlp}s %-${mlv}s %-55s" \
		      $package $version $ifneeded]
	}

	_exit $status
    }
}

# ### ######### ###########################

source [file join $starkit::topdir tpadmin.tcl]
