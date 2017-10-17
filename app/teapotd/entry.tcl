# TEAPOT Server Entry
# - Initialization of wrap support (VFS, Mk4FS foundation, ...)
# - License check !NONE!
# - Invokation of the actual application.
# - (Inactive code for the debugging of the package management, and file sourcery.)

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# Trace exactly which packages are required during execution
#source [file join [pwd] [file dirname [file dirname [info script]]] debug_require.tcl]

# Trace exactly which files are read via source.
#source [file join [pwd] [file dirname [file dirname [info script]]] debug_source.tcl]

# Dump loaded packages when exiting the application
#source [file join [pwd] [file dirname [file dirname [info script]]] dump_packages.tcl]

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
	set appmode unwrapped
    }
    starkit {
	set appname $argv0
	set appmode wrapped
    }
    starpack {
	set appname [info nameofexecutable]
	set appmode wrapped
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

# ### ######### ###########################

source [file join $starkit::topdir teapotd.tcl]
