#!/bin/sh
# -*- tcl -*- \
exec tclsh "$0" ${1+"$@"}

# ### ### ### ######### ######### #########
## Overview

## TEAPOT client application. Specialized for entering packages into a
## 'repository::sqlitedir' repository as exported by the 'teapotd'
## server application.

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# ### ### ### ######### ######### #########
## Requirements

if {[string match -psn* [lindex $::argv 0]]} {
    # Strip Apple's option providing the Processor Serial Number to bundles.
    incr ::argc -1
    set  ::argv [lrange $::argv 1 end]
}

package require logger
package require repository::sqlitedir
package require repository::sys
package require teapot::entity
package require teapot::instance
package require teapot::listspec
package require teapot::metadata::read
package require teapot::metadata::write
package require textutil

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
	create {
	    if {[llength $argv] < 1} usage

	    set path [lindex $argv 0]
	    if {[::repository::sqlitedir valid $path ro msg]} {
		usageC
	    }

	    ::repository::sqlitedir new $path

	    puts "Repository @ $path"
	}
	verify {
	    if {[llength $argv] < 1} usage

	    set verbose 0
	    if {[lindex $argv 0] eq "-v"} {
		set verbose 1
		set argv [lrange $argv 1 end]
		if {[llength $argv] < 2} usage
	    }

	    set path [lindex $argv 0]
	    if {![::repository::sqlitedir valid $path rw msg]} {
		usageA $path $msg
	    }

	    set files [lrange $argv 1 end]
	    set repo  [::repository::sqlitedir %AUTO% -location $path]

	    Verify $verbose $repo
	}
	add {
	    if {[llength $argv] < 2} usage

	    set verbose 0
	    if {[lindex $argv 0] eq "-v"} {
		set verbose 1
		set argv [lrange $argv 1 end]
		if {[llength $argv] < 2} usage
	    }

	    set path [lindex $argv 0]
	    if {![::repository::sqlitedir valid $path rw msg]} {
		usageA $path $msg
	    }

	    set files [lrange $argv 1 end]
	    set repo  [::repository::sqlitedir %AUTO% -location $path]

	    Store $verbose $repo $files
	}
	remove {
	    spec 0 usageR path spec

	    if {![::repository::sqlitedir valid $path rw msg]} {
		usageR $path $msg
	    }

	    set repo     [::repository::sqlitedir %AUTO% -location $path]
	    set spectype [teapot::listspec::type $spec]

	    if {$spectype eq "einstance"} {
		# Instance exactly specified, bypass the search.
		Remove $repo [list [teapot::listspec::2instance $spec]]
	    } else {
	        List   $repo $spec [list Remove $repo]
	    }
	}
	get {
	    spec 1 usageG path spec

	    set repo     [::repository::sqlitedir %AUTO% -location $path -readonly 1]
	    set spectype [teapot::listspec::type $spec]

	    if {$spectype eq "einstance"} {
		# Instance exactly specified, bypass the search.
		Get  $repo [list [teapot::listspec::2instance $spec]]
	    } else {
	        List $repo $spec [list Get $repo]
	    }
	}
	describe {
	    spec 0 usageD path spec

	    set repo     [::repository::sqlitedir %AUTO% -location $path -readonly 1]
	    set spectype [teapot::listspec::type $spec]

	    if {$spectype eq "einstance"} {
		# Instance exactly specified, bypass the search.
		Describe  $repo [list [teapot::listspec::2instance $spec]]
	    } else {
	        List $repo $spec [list Describe $repo]
	    }

	}
	list {
	    spec 1 usageL path spec

	    set repo [::repository::sqlitedir %AUTO% -location $path -readonly 1]

	    List $repo $spec PrintInstances
	}
	status {
	    if {[llength $argv] < 1} usage

	    set path [lindex $argv 0]
	    if {![::repository::sqlitedir valid $path ro msg]} {
		usageS $path $msg
	    }

	    set repo [::repository::sqlitedir %AUTO% -location $path -readonly 1]

	    foreach {imtime jmtime jfirst jlast} [$repo IndexStatus] break
	    foreach {jft jfs} $jfirst break
	    foreach {jlt jls} $jlast break

	    puts "INDEX         [clock format $imtime]"
	    puts "Journal       [clock format $jmtime]"
	    if {[catch {
		puts "Journal First [clock format $jft] $jfs"
		puts "Journal Last  [clock format $jlt] $jls"
	    }]} {
		puts "Journal First N/A"
		puts "Journal Last  N/A"
	    }
	    return
	}
	rejournal {
	    if {[llength $argv] < 1} usage

	    set path [lindex $argv 0]
	    if {![::repository::sqlitedir valid $path rw msg]} {
		usageJ $path $msg
	    }

	    set repo [::repository::sqlitedir %AUTO% -location $path]
	    $repo Rejournal ; # Compact the journal.
	    return
	}
	help {
	    usage
	}
	copy {
	    set  usewatch [info exists ::env(TEAPOT_WATCH)]
	    if {!$usewatch} { usage }

	    if {[llength $argv] < 2} usage

	    set verbose 0
	    if {[lindex $argv 0] eq "-v"} {
		set verbose 1
		set argv [lrange $argv 1 end]
		if {[llength $argv] < 2} usage
	    }

	    if {[llength $argv] != 2} usage
	    foreach {pathdst pathsrc} $argv break

	    if {![::repository::sqlitedir valid $pathsrc rw msg]} {
		usage
	    }
	    if {![::repository::sqlitedir valid $pathdst rw msg]} {
		usage
	    }

	    set reposrc [::repository::sqlitedir %AUTO% -location $pathsrc]
	    set repodst [::repository::sqlitedir %AUTO% -location $pathdst]

	    Copy $verbose $reposrc $repodst
	}
	default {
	    set  usewatch [info exists ::env(TEAPOT_WATCH)]
	    if {$usewatch} {
		package require pref::teapot ; # TEAPOT preferences.
		package require teapot::watch::cmdline

		pref::setGroupOrder [pref::teapot::init]

		# Restore the old state of the command line...
		set argv [linsert $argv 0 $cmd]
		try teapot::watch::cmdline 1
		return
	    }
	    usage
	}
    }
    return
}

proc try {type abort} {
    global argv
    set fun [getfun $type]
    if {[lsearch -exact [$fun API] [lindex $argv 0]] >= 0} {
	runvia $fun
	$fun destroy
	return 1
    }

    if {$abort} { usage }
    $fun destroy
    return 0
}

proc getfun {type} {
    set fun [$type %AUTO%]
    $fun onMD    ::md
    $fun onAbort ::Abort
    return $fun
}

proc Abort {text} {
    puts stderr $text
    exit 1
}

proc runvia {fun} {
    global argv

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
	    [textutil::indent "______________________\n\
                               $errorInfo\n\
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
##

proc spec {nullable ucmd pav sv} {
    upvar 1 $pav path $sv spec
    global argv

    # Syntax (nullable):  path ?-entity? ?name ?version ?arch???
    # Syntax (!nullable): path ?-entity?  name ?version ?arch??

    if {[llength $argv] < 1} usage
    set path [lindex $argv 0]
    set argv [lrange $argv 1 end]

    if {![::repository::sqlitedir valid $path ro msg]} {
	$ucmd $path $msg
    }

    set entity {}
    set opt [lindex $argv 0]
    if {[string match -* $opt]} {
	set entity [string range $opt 1 end]
	if {![teapot::entity::valid $entity msg]} {
	    ${ucmd}x $msg
	}
	set argv [lrange $argv 1 end]
    }

    if {[llength $argv] > 3} usage
    if {!$nullable && ([llength $argv] < 1)} usage

    if {[catch {
	set spec [teapot::listspec::cons $entity $argv]
    } msg]} {
	${ucmd}x $msg
    }

    return
}

# ### ### ### ######### ######### #########
## Basic helpers ...

proc usage {} {
    global appname
    set    blank [blankstr $appname]
    set appna $appname
    puts stderr "Usage: $appna create path"
    puts stderr "       $blank add    ?-v? path file..."
    puts stderr "       $blank remove      path ?-entity?  name ?version ?platform??"
    puts stderr "       $blank get         path ?-entity? ?name ?version ?platform???"
    puts stderr "       $blank describe    path ?-entity?  name ?version ?platform??"
    puts stderr "       $blank list        path ?-entity? ?name ?version ?platform???"
    puts stderr "       $blank verify ?-v? path"
    puts stderr "       $blank status      path"
    puts stderr "       $blank rejournal   path"
    puts stderr ""
    puts stderr "       $blank help"
    puts stderr "       $blank version"
    puts stderr "       $blank who"
    puts stderr ""
    exit 1
}

proc usageC {} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname create path"
    puts stderr "       $blank The chosen path is already a valid repository"
    exit 1
}

proc usageA {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname add ?-v? path file..."
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
    exit 1
}

proc usageD {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname describe path ?-entity? name ?version ?platform??"
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
    exit 1
}

proc usageR {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname remove path ?-entity? name ?version ?platform??"
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
    exit 1
}

proc usageRx {msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname remove path ?-entity? name ?version ?platform??"
    puts stderr "       $blank $msg"
    exit 1
}

proc usageG {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname get path ?-entity? ?name ?version ?platform???"
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
    exit 1
}

proc usageGx {msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname get path ?-entity? name ?version ?platform??"
    puts stderr "       $blank $msg"
    exit 1
}

proc usageL {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname list path ?-entity? ?name ?version ?platform???"
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
    exit 1
}

proc usageLx {msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname list path ?-entity? ?name ?version ?platform???"
    puts stderr "       $blank $msg"
    exit 1
}

proc usageS {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname status path"
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
    exit 1
}

proc usageJ {path msg} {
    global appname
    set    blank [blankstr $appname]
    puts stderr "Usage: $appname rejournal path"
    puts stderr "       $blank \"$path\" is not a valid repository.\n       $blank $msg"
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
## Copying a repository to another.

proc Copy {verbose src dst} {
    List $src [teapot::listspec::all] \
	[list GetAndStore $verbose $src $dst]
    return
}

proc GetAndStore {verbose src dst instances} {
    set files {}
    foreach instance $instances {
	# Drop the isprofile boolean flag from incoming data.

	teapot::instance::norm instance

	puts -nonewline stdout "Retrieving $instance ... "
	flush           stdout

	set fname [fileutil::tempfile tpadmin_]

	if {[catch {$src sync get $instance $fname} msg]} {
	    puts stdout Error
	    puts stdout [textutil::indent $msg "    "]
	    file delete $fname
	    continue
	}

	puts stdout Ok
	lappend files $fname
    }

    Store $verbose $dst $files

    foreach f $files { file delete $f }
    return
}

# ### ### ### ######### ######### #########
## Repository verification

proc Verify {verbose r} {
    puts  stdout "Checking [$r cget -location] ..."
    flush stdout

    if {$verbose} {
	package require log
	set log VerifyLog
    } else {
	set log VerifyDevNull
    }

    if {[catch {$r sync verify $log} msg]} {
	puts  stdout "Failed: $msg"
	flush stdout
    } else {
	puts  stdout "Ok. $msg"
	flush stdout
    }
}

proc VerifyDevNull {args} {}
proc VerifyLog {level text} {
    if {$level eq "info"} {
	set show "    "
    } else {
	set show $level
    }
    puts "$show$::log::fill($level) $text"
    return
}

# ### ### ### ######### ######### #########
## File uploading ...

proc Store {verbose r files} {
    set changed 0

    foreach f $files {
	puts -nonewline stdout "Uploading $f ... "
	flush           stdout

	set errors {}
	set fail [catch {
	    set p [lindex [::teapot::metadata::read::file $f single errors] 0]
	} msg]
	if {!$fail} {set msg [join $errors \n]}

	if {$fail || [llength $errors]} {
	    catch {$p destroy}

	    puts stdout Error
	    puts stdout [textutil::indent $msg "    "]
	    break
	}

	if {$verbose} {
	    puts \n\n[textutil::indent \
		  [teapot::metadata::write::getStringHumanC $p] \
		  "    "]\n
	}

	$p destroy
	if {[catch {$r sync put $f} msg]} {
	    puts stdout Error
	    puts stdout [textutil::indent $msg "    "]
	    break
	}

	puts stdout Ok
	set changed 1

	# Continue uploading ...
    }

    if {$changed} {ClearCache [$r cget -location]}
    return
}

# ### ### ### ######### ######### #########
## Instance listing ...

proc List {r spec cmd} {
    puts -nonewline stdout "Locating [teapot::listspec::print $spec] ... "
    flush           stdout

    if {[catch {
	set instances [$r sync list $spec]
    } msg]} {
	puts stdout Error
	puts stdout [textutil::indent $msg "    "]
	return
    }

    puts stdout Ok
    if {[llength $instances] == 1} {
	puts stdout "1 instance found"
    } else {
	puts stdout "[llength $instances] instances found"
    }

    # Run the chosen callback ...
    # Sort by name, version

    lappend cmd [lsort -index 1 -dict \
		     [lsort -index 2 -dict \
			  $instances]]
    uplevel \#0 $cmd
    return
}

proc PrintInstances {instances} {
    set ::done yes

    if {![llength $instances]} return

    set maxt 0
    set maxn 0
    set maxv 0
    set maxp 0

    foreach i $instances {
	teapot::instance::split $i t n v p
	max maxt $t
	max maxn $n
	max maxv $v
	max maxp $p
    }

    # Sort output by name and version.
    puts ""
    foreach i [lsort -dict -index 1 \
		   [lsort -dict -index 2 \
			$instances]] {
	teapot::instance::split $i t n v p
	puts "  [lj $maxt $t]  [lj $maxn $n]  [lj $maxv $v]  $p"
    }
    puts ""
    return
}

proc max {v str} {
    upvar 1 $v max
    set l [string length $str]
    if {$l > $max} {set max $l}
    return
}

proc lj {n s} {format %-*s $n $s}

# ### ### ### ######### ######### #########
## Instance removal ...

proc Remove {r instances} {
    # instances    = list (ext-instance ...)
    # ext-instance = (entity name version arch isprofile)

    set changed 0
    foreach instance $instances {
	# Drop the isprofile boolean flag from incoming data.

	teapot::instance::norm instance

	puts -nonewline stdout "Removing $instance ... "
	flush           stdout

	if {[catch {
	    $r sync del $instance
	} msg]} {
	    puts stdout Error
	    puts stdout [textutil::indent $msg "    "]
	    break
	}

	puts stdout Ok
	set changed 1

	# Continue removing ...
    }

    if {$changed} {ClearCache [$r cget -location]}
    return
}

# ### ### ### ######### ######### #########
## Instance description

proc Describe {r instances} {
    # instances = list (ext-instance ...)
    # ext-instance = (entity name version arch isprofile)

    foreach instance $instances {
	# Drop the isprofile boolean flag from incoming data.

	teapot::instance::norm instance

	set spec     [teapot::instance::2spec  $instance]
	set pkglabel [teapot::listspec::print2 $spec]

	puts "Entity   $pkglabel"
	puts "Origin @ [$r cget -location]"

	if {[catch {$r sync meta $spec} meta]} {
	    puts stdout Error
	    puts stdout [textutil::indent $meta "    "]
	}

	array set md $meta

	set maxlen 0
	foreach name [array names md] {
	    set l [string length $name]
	    if {$l > $maxlen} {set maxlen $l}
	}
	incr maxlen 2
	set  off [expr {72 - $maxlen}]

	foreach name [lsort [array names md]] {
	    set prefix [format "%-*s : " $maxlen $name]
	    puts [bulletpara $prefix $off $md($name)]
	}

	puts ""

	puts stdout Ok

	# Continue describing ...
    }
    return
}

proc bulletpara {bullet len text} {
    bullet $bullet [textutil::adjust $text -length $len]
}

proc bullet {bullet para} {
    set blank  [string repeat " " [string length $bullet]]
    return ${bullet}[textutil::indent $para $blank 1]
}

# ### ### ### ######### ######### #########
## Instance file retrieval ...

proc Get {r instances} {
    # instances = list (ext-instance ...)
    # ext-instance = (entity name version arch isprofile)

    foreach instance $instances {
	# Drop the isprofile boolean flag from incoming data.

	teapot::instance::norm instance

	puts -nonewline stdout "Retrieving $instance ... "
	flush           stdout

	set fname [fileutil::tempfile]

	if {[catch {$r sync get $instance $fname} msg]} {
	    puts stdout Error
	    puts stdout [textutil::indent $msg "    "]
	}

	set mtypes [fileutil::magic::mimetype $fname]
	if {[lsearch -exact $mtypes "application/zip"] >= 0} {
	    set artype zip
	} else {
	    set artype tm
	}

	teapot::instance::split $instance t n v p

	set arfname ${t}-[string map {_ __ :: _} ${n}-${v}-${p}].$artype
	file rename -force $fname $arfname

	puts stdout Ok\t$arfname

	# Continue retrieving ...
    }
    return
}

# ### ### ### ######### ######### #########
## teapotd cache handling.

proc ClearCache {path} {
    set path [file join $path htmlcache]
    if {![file exists $path]} return

    puts stdout {Cache found, clearing ...}

    foreach f [glob -nocomplain -directory $path *] {
	file delete $f
    }

    puts stdout Ok
    return
}

# ### ### ### ######### ######### #########
## Package/Version/Instance removal

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
