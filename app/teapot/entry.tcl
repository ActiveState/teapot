# teacup - TEAPOT Client

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

package require Tcl 8.5-

# - Initialization of wrap support (VFS, Mk4FS foundation, ...)
if {![info exists starkit::mode]} {
    package require starkit
    starkit::startup
}

# - Choosing the application name
if {$starkit::mode eq "starpack"} {
    set appname [info nameofexecutable]
} else {
    set appname $starkit::topdir
}

set appname [file tail $appname]

# - Extending package management to find deeper packages (lib/P-*)
global     auto_path
foreach d $auto_path {
    foreach pd [glob -nocomplain -directory $d P-*] {
	lappend auto_path $pd
    }
}

# ### ######### ###########################

# - Invokation of the actual application.
source [file join $starkit::topdir teapot.tcl]
