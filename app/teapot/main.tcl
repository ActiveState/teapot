# Custom main.tcl
# Entry point for unwrapped execution out of AS' source repository.
# This file will be overwritten during wrapping, with a generated
# main.tcl file setting up wrapped execution.

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

package require Tcl 8.5-

# Inactive code for the debugging of the package management, and file sourcery.

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

# Trace exactly which files are read via source.
#source [file join [pwd] [file dirname [file dirname [info script]]] debug_source.tcl]

# Dump loaded packages when exiting the application
#source [file join [pwd] [file dirname [file dirname [info script]]] dump_packages.tcl]

# ### ######### ###########################

package require starkit
starkit::startup

# Unwrapped call - During development from within the local perforce
# depot area. Slightly different location of lib dir.  Hence we use
# two stanza's to define an externa lib directory.  Debug output is
# allowed, actually sort of wanted to be sure of package locations.

lappend auto_path [file join [file dirname [file dirname $starkit::topdir]] devkit lib]
lappend auto_path [file join [file dirname [file dirname $starkit::topdir]] lib]
lappend auto_path ~/TDK/lib

puts unwrapped\n[join $auto_path \n\t]

# ### ######### ###########################

source [file join $starkit::topdir entry.tcl]
