# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# TEAPOT PacKaGe archive Generator ENtry
#
# - Initialization of wrap support (VFS, Mk4FS foundation, ...)
# - License check !NONE!
# - Invokation of the actual application.
# - (Inactive code for the debugging of the package management, and file sourcery.)

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

# Load compiler (for -compile feature), suppress license check via magic
set ::tdk_feature no-license
package require compiler
compiler::tdk_license user-name
unset ::tdk_feature

# ### ######### ###########################

source [file join $starkit::topdir pkggen.tcl]
