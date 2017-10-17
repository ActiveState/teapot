#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}

# ### ### ### ######### ######### #########
## Overview

## TEAPOT package generator application. Generation of package
## archives (Tcl Modules or Zip) from a package directory also
## providing meta data ("teapot.txt").

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# User interface: Command line.
# Syntax:
#
#    teapot-pkg generate ?-o resultpath? ?-type tm|zip|pkgIndex|auto? ?-compile? pkgpath

# ### ### ### ######### ######### #########
## Requirements

if {[string match -psn* [lindex $::argv 0]]} {
    # Strip Apple's option providing the Processor Serial Number to bundles.
    incr ::argc -1
    set  ::argv [lrange $::argv 1 end]
}

package require fileutil
package require fileutil::magic::mimetype
package require pref::teapot
package require teapot::metadata::container
package require teapot::metadata::edit
package require teapot::metadata::read
package require teapot::metadata::write
package require teapot::package::gen
package require textutil
package require zipfile::decode

# ### ### ### ######### ######### #########

pref::setGroupOrder [pref::teapot::init]

package require logger
if 0 {
    puts *\ LOG\ :\ [join [logger::services] "\n* LOG : "]

    set max 0
    foreach s [logger::services] {
	if {[set l [string length $s]] > $max} {set max $l}
	proc logger::tree::${s}::stdoutcmd {level text} {
	    global sx
	    variable service
	    puts stdout "$sx($service) \[$level\] \'$text\'"
	}
    }
    incr max 2
    foreach s [logger::services] {
	set ::sx($s) [format %-*s $max \[$s\]]
    }
} else {
    logger::setlevel error
}

# ### ### ### ######### ######### #########
## Main code ...

proc main {} {
    global argv

    if {[llength $argv] < 1} usage

    set cmd  [lindex $argv 0]
    set argv [lrange $argv 1 end]

    switch -exact -- $cmd {
	version {
	    if {[llength $argv] > 0} usage
	    puts \n\t[[md {version unknown}] version]\n
	    exit 0
	}
	who {
	    if {[llength $argv] > 0} usage
	    puts \n[teapot::metadata::write::getStringHumanC [md]]\n
	    exit 0
	}
	ignore {
	    # add/remove/list

	    if {[llength $argv] < 1} usage
	    set cmd  [lindex $argv 0]
	    set argv [lrange $argv 1 end]

	    switch -exact -- $cmd {
		add    {ignore-add $argv}
		remove {ignore-remove $argv}
		list   {ignore-list $argv}
		default {usage}
	    }
	}
	generate {generate $argv}
	scan     {scandir  $argv}
	show     {show     $argv}
	edit     {edit     $argv}
	expand   {expand   $argv}
	help -
	default {usage}
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

proc ignore-add {argv} {
    if {[llength $argv] < 1} usage
    set patterns [pref::teapot::ignorePatternList]
    foreach p $argv {lappend patterns $p}
    set patterns [lsort -uniq $patterns]
    pref::teapot::ignorePatternList $patterns
    return
}

proc ignore-remove {argv} {
    if {[llength $argv] < 1} usage
    set patterns [pref::teapot::ignorePatternList]
    foreach p $argv {
	if {[set pos [lsearch -exact $patterns $p]] < 0} continue
	set patterns [lreplace $patterns $pos $pos]
    }
    pref::teapot::ignorePatternList $patterns
    return
}

proc ignore-list {argv} {
    if {[llength $argv] > 0} usage

    puts "Directory patterns to ignore during traversal"
    set patterns [pref::teapot::ignorePatternList]
    if {[llength $patterns]} {
	puts ""
	foreach p $patterns {
	    puts "  $p"
	}
	puts ""
    }
    return
}

proc scanlog {l t} {
    puts "$l $t"
    return
}

proc scandir {argv} {
    package require teapot::metadata::scan

    foreach dir $argv {
	if {![teapot::metadata::scan::IsPkgDir $dir]} {
	    scanlog warning "$dir is not a package directory"
	    scanlog warning "  Skipping"
	    continue
	}

	set mds [teapot::metadata::scan %AUTO% $dir -log scanlog]
	$mds hints= {
	    options registry -platform windows
	    options dde      -platform windows
	}

	if {[catch {
	    set scanresult [$mds scan]
	}]} {
	    scanlog error { }
	    scanlog error {ERROR	INTERNAL ERROR}
	    foreach line [split $::errorInfo \n] {
		scanlog error ERROR\t$line
	    }
	    scanlog error {ERROR	INTERNAL ERROR}
	    scanlog error { }

	    $mds destroy
	    continue
	}
	if {[llength $scanresult]} {
	    set md ""
	    foreach p $scanresult {
		append md [::teapot::metadata::write::getStringExternalC $p]\n
		$p destroy
	    }
	    fileutil::writeFile [file join $dir teapot.txt] $md
	}
    }
    return
}

proc generate {argv} {
    set artype   auto
    set mkdirout 1
    set timestamp ""
    set arinfix   ""
    set compile  0

    while {[string match -* [set opt [lindex $argv 0]]]} {
	switch -exact -- $opt {
	    -o -
	    --output {
		if {[llength $argv] < 3} usage
		set output [lindex $argv 1]
		set argv   [lrange $argv 2 end]
	    }
	    -timestamp -
	    --timestamp -
	    -T {
		# We strip leading zeros, this is a possible octal issue for
		# some month names, and a version number must not have
		# leading zeros in its parts anyway.

		set timestamp [string map {.0 .} \
		    [clock format [clock seconds] \
			 -gmt 1 -format {.%Y.%m.%d.%H.%M.%S}]]

		set argv [lrange $argv 1 end]
	    }
	    -arinfix {
		if {[llength $argv] < 3} usage
		set arinfix [lindex $argv 1]
		set argv    [lrange $argv 2 end]
	    }
	    -compile -
	    --compile -
	    -c {
		set compile 1
		set argv [lrange $argv 1 end]
	    }
	    -type -
	    -format -
	    --type -
	    --format -
	    -t -
	    -f {
		if {[llength $argv] < 3} usage
		set artype [string tolower [lindex $argv 1]]
		set argv   [lrange $argv 2 end]
		switch -exact -- $artype {
		    tm - zip - auto {set mkdirout 1}
		    pkgindex {set artype pkgIndex ; set mkdirout 0}
		    default usage
		}
	    }
	    default usage
	}
    }

    if {[llength $argv] != 1} usage
    set top [lindex $argv 0]

    if {![info exists output]} {
	if {$artype eq "pkgIndex"} {
	    set output {}
	} else {
	    set output [pwd]
	}
    } else {
	set output [file normalize $output]
    }

    if {[file exists $output] && [file isfile $output]} {
	puts stderr \
	    "Output path \"$output\" exists, but is not a directory."
	exit 1
    }
    if {$mkdirout} {
	file mkdir $output
    }

    array set config [list \
			  artype    $artype \
			  respath   $output \
			  timestamp $timestamp \
			  arinfix   $arinfix \
			  logcmd    puts \
			  compile   $compile]

    ::teapot::package::gen::ignore [pref::teapot::ignorePatternList]
    ::teapot::package::gen::do     $top config
    return
}

global    fullmode
array set fullmode {
    -h human
    -x external
    -e embedded
}

proc show {argv} {
    global fullmode
    set mode human

    while {[string match -* [set opt [lindex $argv 0]]]} {
	switch -exact -- $opt {
	    --human    -
	    --external -
	    --embedded {
		set mode [string range $opt 2 end]
		set argv [lrange $argv 1 end]
	    }
	    -h -
	    -x -
	    -e {
		set mode $fullmode($opt)
		set argv [lrange $argv 1 end]
	    }
	    -human    -
	    -external -
	    -embedded {
		set mode [string range $opt 1 end]
		set argv [lrange $argv 1 end]
	    }
	    default usage
	}
    }

    if {[llength $argv] != 1} usage
    set archive [lindex $argv 0]

    set errors {}
    set fail [catch {
	set p [lindex [::teapot::metadata::read::file $archive single errors] 0]
    } msg]

    if {!$fail} {set msg [join $errors \n]}

    if {$fail || [llength $errors]} {
	catch {$p destroy}

	puts stdout Error
	puts stdout [textutil::indent $msg "    "]
	return
    }

    switch -exact -- $mode {
	human    {
	    set printout [teapot::metadata::write::getStringHumanC    $p]
	    set printout \n\n[textutil::indent $printout "    "]\n
	}
	external {set printout [teapot::metadata::write::getStringExternalC $p]}
	embedded {set printout [teapot::metadata::write::getStringEmbeddedC $p]}
    }
    puts $printout

    $p destroy
    return
}

proc edit {argv} {
    if {[llength $argv] < 2} usage
    set archive [lindex $argv 0]

    set errors {}

    set errors {}
    set fail [catch {
	::teapot::metadata::edit::archive $archive [lrange $argv 1 end] errors
    } msg]

    if {!$fail} {set msg [join $errors \n]}

    if {$fail || [llength $errors]} {
	puts stdout Error
	puts stdout [textutil::indent $msg "    "]
    }

    return
}

proc expand {argv} {
    if {[llength $argv] != 2} usage
    foreach {src dst} $argv break

    # Expand the archive into the destination directory. The latter is
    # created as needed. This is essentially the same done by the TDK
    # tclapp wrap-engine (lib/wrapengine/tclapp_pkg.tcl, wrapFile).

    # Code snarfed from that location. Modified (always verbose, via
    # puts, not log::log). Also used by TclPE.  => FUTURE: Factor into
    # its own package.

    # Get md first ...

    set errors   {}
    set packages [teapot::metadata::read::file $src single errors {} 1]

    if {[llength $errors]} {
	puts [join $errors \n]
	return
    }

    set p [lindex $packages 0]
    set pname    [$p name]
    set pversion [$p version]

    puts "Expanding   [$p type] $pname $pversion ..."
    puts "Destination $dst"

    # Distinguish 3 cases

    # 1. Zip archive.
    # 2. Tcl Module with attached Metakit filesystem
    # 3. Tcl Module without attached Metakit filesystem

    # (Ad 1) Mount as filesystem, copy all files into the dst.
    # (ad 2) Mount as fs, copy all files into dir. copy prefix code
    #        as separate file, create pkgIndex for it.
    # (Ad 3) Generate pkgIndex for it and copy into dst dir

    set mtypes [fileutil::magic::mimetype $src]
    if {[lsearch -exact $mtypes "application/zip"] >= 0} {
	# Ad (1) Zip archive.

	puts "    Zip archive"
	puts "      Unpacking"

	zipfile::decode::open $src
	set zdict [zipfile::decode::archive]

	puts "      Copying contents"

	file mkdir                    $dst
	zipfile::decode::unzip $zdict $dst
	zipfile::decode::close

    } else {
	# Ad (2,3) Tcl Module.

	set entry implementation.tcl

	puts "    Tcl Module"

	# Strip the block of data which was insert by the package
	# generator. If we do not do this the addition of such a
	# block by the next round of generation will cause the header
	# to continously expand, always with the same code.

	set lines [split [fileutil::cat $src -eofchar \x1A $src] \n]
	set del 0
	set kitsrc $src

	if {($start >= 0) && ($stop >= 0) && ($start < $stop)} {
	    puts "    Stripping teapot-pkg generate'd block"

	    set t [fileutil::tempfile teapot-pkg]
	    fileutil::writeFile $t [join [lreplace $lines $start $stop] \n]\n
	    set src $t
	    set del 1
	}

	if {[lsearch -exact [fileutil::fileType $src] metakit] >= 0} {
	    # (Ad 2) With attached metakit filesystem. Mount and copy.

	    puts "      Copying attached filesystem"

	    # readonly! Because we do not modify, and the temp files
	    # will be set ro too, and writable mounting would fail
	    # due to that.

	    vfs::mk4::Mount  $kitsrc $kitsrc -readonly
	    file mkdir               $dst
	    file copy -force $kitsrc $dst
	    vfs::unmount     $kitsrc

	    if {[file exists [file join $dst $entry]]} {
		set n
		set base implementation
		while {[file exists [file join $dst $base$n.tcl]]} {incr n}
		set entry $base$n.tcl
	    }

	    puts "      Generating entrypoint"

	    fileutil::writeFile [file join $dst $entry] \
		[fileutil::cat -eofchar \x1A $src]

	} else {
	    # (Ad 3) Without attached metakit filesystem.

	    puts "    Copying as is"

	    file mkdir $dst
	    file copy $src [file join $dst $entry]
	}

	if {$del} {
	    # Clean up the temp file needed to strip the teapot-pkg block.
	    file delete $src
	}

	# Generate package index.

	puts "      Generating package index"

	fileutil::writeFile [file join $dst pkgIndex.tcl] \
	    [string map \
		 [list NAME $pname VER $pversion ENTRY $entry] \
		 {package ifneeded {NAME} VER [list source [file join $dir ENTRY]]}]\n
    }

    puts "      Generating metadata ..."

    fileutil::writeFile [file join $dst teapot.txt] \
	[::teapot::metadata::write::getStringExternalC $p]

    puts "    Ok."
    puts " "
    return
}

# ### ### ### ######### ######### #########
## Helpers ...

proc usage {} {
    global appname
    set    blank [blankstr $appname]
    set    appna $appname

    puts stderr "Usage: $appna generate ?--timestamp|-T? ?-o|--output output? ?--type|-t tm|zip|auto|pkgindex? ?--compile|-c? packagepath"
    puts stderr "       $blank show ?--human|-h|--external|-x|--embedded|-e? file"
    puts stderr "       $blank expand file destination"
    puts stderr "       $blank edit file change..."
    puts stderr "       $blank scan dir..."
    puts stderr "       $blank ignore add    pattern..."
    puts stderr "       $blank ignore remove pattern..."
    puts stderr "       $blank ignore list"
    puts stderr "       $blank help"
    puts stderr ""
    puts stderr "       $blank   change <=> set|=     key value"
    puts stderr "       $blank              add|+     key value"
    puts stderr "       $blank              clear|c   key"
    puts stderr "       $blank              unset|!|- key"
    puts stderr "       $blank              name:     value"
    puts stderr "       $blank              version:  value"
    puts stderr "       $blank              type:     value"
    puts stderr ""
    puts stderr "       $blank version"
    puts stderr "       $blank who"
    puts stderr ""
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

logger::setlevel error
#logger::setlevel info

proc logger::tree::stdoutcmd {level text} {
    variable service
    puts stderr "\[[clock format [clock seconds]]\] \[$service\] \[$level\] \'$text\'"
}

# ### ### ### ######### ######### #########
## Main processing

main
exit
