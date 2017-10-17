# -*- tcl -*-
# ### ### ### ######### ######### #########
## Overview

## Instances are the teapot client application. Interprets command
## line arguments, with commands matching to methods. Ties into
## configuration database and client functional layer.

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# ### ### ### ######### ######### #########
## Requirements

package require fileutil
package require logger
package require platform::shell
package require repository::client
package require repository::localma
package require repository::proxy
package require repository::sys
package require snit
package require teapot::config
package require teapot::entity       ; # Entity type validation
package require teapot::link         ; # Repository / Shell linkage
package require teapot::listspec     ; # Listspec handling
package require teapot::plat         ; # Shell / Platform packages
package require teapot::tmbackport   ; # Shell / Tcl Modules
package require teapot::version      ; # Version handling

# ### ### ### ######### ######### #########
## Implementation

logger::initNamespace ::repository::capp
snit::type ::repository::capp {
    # ### ### ### ######### ######### #########
    ## API - Construction, set/get default installation.
    ##       Add/remove archives.

    constructor {} {
	set config \
	    [teapot::config ${selfns}::config]

	set client \
	    [::repository::client ${selfns}::client \
		 [mymethod LOG] $config]
	return
    }

    method API {} {
	set res {}
	foreach m [lsort -dict [$self info methods]] {
	    if {[string match {[A-Z]*} $m]} continue
	    if {[string match {on*} $m]} continue
	    if {[string match {abort*} $m]} continue
	    if {[string equal destroy $m]} continue
	    if {[string equal info $m]} continue
	    lappend res $m
	}
	return $res
    }

    # ### ### ### ######### ######### #########
    ## Logging of client actions.

    method LOG {args} {
	eval [linsert $args 0 ::puts]
	flush stdout
	return
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation - Self management

    method who {args} {
	if {[llength $args] != 0} {$self Usage who ""}
	puts \n[teapot::metadata::write::getStringHumanC [$self MD]]\n
	return
    }

    method version {args} {
	if {[llength $args] != 0} {$self Usage version ""}
	puts \n\t[[$self MD {version unknown}] version]\n
	return
    }

    method update-self {args} {
	set  verbose 0
	if {[llength $args] != 0} {
	    if {
		([llength $args] == 1) &&
		([lindex $args 0] eq "-v")
	    } {
		set verbose 1
	    } else {
		$self Usage update-self "?-v?"
	    }
	}

	# Get the newest version of teacup for the same platform as this
	# executable.

	teapot::instance::split [[$self MD] instance] e n v a
	$client updateSelf $e $n $v $a $verbose
	$client ReportInactives
	return
    }

    method regenerate {args} {
	# ?--at DIR?
	$self CheckSyntax/Regenerate $args at
	if {$at eq ""} {set at [$config default get]}
	$client regenerate $at
	return
    }

    # MD callback. Expected result is a MD container object.
    variable mdcmd {}

    method MD {{text {}}} {uplevel \#0 [linsert $mdcmd end $text]}
    method onMD {cmd} {
	set mdcmd $cmd
	return
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation - Package mgmt

    method log {args} {
	# Syntax: log show  ?-l|--last N?   ?-s|--since T?    ?--at DIR?
	#         log purge ?--keep-last N? ?--keep-since T?  ?--at DIR?

	if {[llength $args] < 1} {
	    $self Usage log "show|purge"
	}

	set cmd  [lindex $args 0]
	set args [lrange $args 1 end]

	switch -exact -- $cmd {
	    show    {$self LogShow  $args}
	    purge   {$self LogPurge $args}
	    default {
		$self Usage log "show|purge"
	    }
	}
	return
    }

    method LogShow {a} {
	$self CheckSyntax/LogShow $a n since theinstall
	$self SetInstall $theinstall

	$client log show $n $since
	return
    }

    method LogPurge {a} {
	$self CheckSyntax/LogPurge $a n before theinstall
	$self SetInstall $theinstall

	$client log purge $n $before
	return
    }

    ## General syntax of <PACKAGE-REF>:
    ##           NAME [[-exact] VERSION] [-is ENTITY]
    ## expanded: 1: NAME
    ##           2: NAME VERSION
    ##           3: NAME -exact VERSION
    ##           3: NAME                -is ENTITY
    ##           4: NAME VERSION        -is ENTITY
    ##           5: NAME -exact VERSION -is ENTITY

    ## General syntax of <PACKAGE>:
    ##           NAME [VERSION]
    ## expanded: NAME
    ##           NAME VERSION

    method update {args} {
	# Syntax: ?--dry-run? ?--at DIR? ?--only uninstalled|unknown|newer|update?

	$self CheckSyntax/Update $args dry theinstall only
	$self SetInstall $theinstall
	$client Update $dry $only
	return
    }

    method install {args} {
	# Syntax: ?--dry-run? ?--force? ?--at DIR? ?--arch ARCH? FILE|URL...
	#         ?--dry-run? ?--force? ?--at DIR? ?--arch ARCH? ?--with-recommends? <ENTITY-REF>

	$self CheckSyntax/Install $args immediate dry force theinstall rec ref url arch
	$self SetInstall $theinstall

	# rec :: 0 <=> follow required,
	#        1 <=> follow required+recommended
	#        2 <=> follow none
	# url :: list (file/url...)

	if {$immediate} {
	    $client installUrl    $dry $force $rec $url $arch
	} else {
	    $client installEntity $dry $force $rec $ref $arch
	}
	return
    }

    method remove {args} {
	# Syntax: ?--dry-run? ?--at DIR? <PACKAGE>

	$self CheckSyntax/Remove $args dry pkg theinstall
	$self SetInstall $theinstall

	$client remove $dry $pkg
	return
    }

    method describe {args} {
	# Syntax: ?--at-default? ?--at DIR? ?--all? <PACKAGE>

	log::debug "describe ($args)"

	$self CheckSyntax/Describe $args useinstall theinstall getall pkg
	$self SetInstall $theinstall

	log::debug "         ui=$useinstall ga=$getall p=($pkg)"

	$client describe $useinstall $getall $pkg
	return
    }

    method list {args} {
	# Syntax: ?--at-default? ?--at DIR? ?--as profile|table|csv? ?--entity? ?--all? ?--only uninstalled|unknown|newer|update? ?<PACKAGE>?

	$self CheckSyntax/List $args useinstall theinstall listspec asformat only all
	$self SetInstall $theinstall

	$client list $useinstall $asformat $listspec $only $all
	return
    }

    method get {args} {
	# Syntax: ?--at-default? ?--at DIR? ?--output DIR? ?--entity? ?<PACKAGE+ARCH>?

	$self CheckSyntax/Get $args useinstall theinstall listspec output

	# Bugzilla 67328
	if {![fileutil::test $output edw msg "--output directory"]} {
	    $self Usage get $msg
	}

	$self SetInstall $theinstall

	$client get $useinstall $output $listspec
	return
    }

    method search {args} {
	# Syntax: ?--at-default? ?--at DIR? ?--as profile|table|csv? ?-v? <...query...>

	$self CheckSyntax/Search $args useinstall theinstall query asformat
	$self SetInstall $theinstall

	$client search $useinstall $asformat $query
	return
    }

    method keys {args} {
	# Syntax: ?--at-default? ?--at DIR?

	$self CheckSyntax/Keys $args useinstall theinstall
	$self SetInstall $theinstall

	$client keys $useinstall
	return
    }

    method profiles {args} {
	# Syntax: ?--at-default? ?--at DIR?

	$self CheckSyntax/Profiles $args useinstall theinstall
	$self SetInstall $theinstall

	# Reusing the general search system.

	$client search $useinstall table {or {is profile} {and {is package} {haskey profile}}}
	return
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation - Client mgmt

    method archive {args} {
	if {[llength $args] < 1} {
	    $self Usage archive "list|add|remove"
	}

	set cmd  [lindex $args 0]
	set args [lrange $args 1 end]

	switch -exact -- $cmd {
	    list    {$self ArchiveList $args}
	    add     {$self ArchiveAdd  $args}
	    remove  {$self ArchiveRem  $args}
	    default {
		$self Usage archive "list|add url|remove url"
	    }
	}
	return
    }

    method ArchiveList {arguments} {
	# Syntax: 
	$self CheckSyntax/ArchiveList $arguments

	foreach r [$config archives] {
	    puts "  $r"
	}
	return
    }

    method ArchiveAdd {arguments} {
	# Syntax: url
	$self CheckSyntax/ArchiveAdd $arguments url

	if {[file exists $url]} {
	    set url file://[file join [pwd] $url]
	}

	$config archive add $url

	puts "Added:"
	puts "  $url"
	return
    }

    method ArchiveRem {arguments} {
	# Syntax: url
	$self CheckSyntax/ArchiveRemove $arguments url

	if {[file exists $url]} {
	    set url file://[file join [pwd] $url]
	}

	$config archive remove $url
	return
    }

    method proxy {args} {
	if {([llength $args] != 0) &&
	    ([llength $args] != 2)} {
	    $self Usage proxy "?host port?"
	}

	if {[llength $args] == 2} {
	    foreach {h p} $args break
	    $config proxy set $h $p
	} else {
	    set hp [$config proxy get]
	    if {[llength $hp] != 2} {
		puts "No proxying"
		return
	    }
	    foreach {h p} $hp break
	}

	puts "Proxying through $h @ $p"
	return
    }

    method timeout {args} {
	if {([llength $args] != 0) &&
	    ([llength $args] != 1)} {
	    $self Usage timeout "?seconds?"
	}

	if {[llength $args] == 1} {
	    set seconds [lindex $args 0]
	    if {![string is integer -strict $seconds]} {
		$self Usage timeout "?seconds?"
	    }
	    $config timeout set $seconds
	    puts "Timeout set to [fmt_timeout $seconds]"
	    return
	}

	puts [fmt_timeout [$config timeout get]]
	return
    }

    proc fmt_timeout {n} {
	if {$n <= 0} {return "infinity, no timeout"}
	return [plx $n second]
    }

    proc plx {n s {p {}}} {
	return "$n [pl $n $s $p]"
    }

    proc pl {n s {p {}}} {
	if {$n == 1} {return $s}
	if {$p == {}} {set p ${s}s}
	return $p
    }

    method cache {args} {
	if {[llength $args] < 1} {
	    $self Usage cache "clear|on ?dir?|off|status"
	}

	set cmd  [lindex $args 0]
	set args [lrange $args 1 end]

	switch -exact -- $cmd {
	    clear  {$self CacheClear  $args}
	    on     {$self CacheOn     $args}
	    off    {$self CacheOff    $args}
	    status {$self CacheStatus $args}
	    default {
		$self Usage archive "clear|on ?dir?|off|status"
	    }
	}
	return
    }

    method CacheClear {arguments} {
	# Syntax: 
	$self CheckSyntax/CacheClear $arguments

	if {![$config cache is active]} {
	    $self abortp "Unable to clear cache, location is unknown"
	}
	$config cache clear
	return
    }

    method CacheOn {arguments} {
	# Syntax: ?dir?
	$self CheckSyntax/CacheOn $arguments hasdir dir

	if {$hasdir} {
	    $config cache at $dir
	} else {
	    $config cache on
	}
	return
    }

    method CacheOff {arguments} {
	# Syntax:
	$self CheckSyntax/CacheOff $arguments
	$config cache off
	return
    }

    method CacheStatus {arguments} {
	# Syntax:
	$self CheckSyntax/CacheStatus $arguments
	if {![$config cache is active]} {
	    puts "No local caching of remote meta data"
	} else {
	    puts "Local caching of remote meta data"
	    puts "Cache directory: [$config cache is at]"

	    set urls [$config cache list]

	    if {[llength $urls]} {
		puts ""

		set    maxu 0
		maxstr maxu Archive
		foreach url $urls {
		    maxstr maxu $url
		}

		puts "[lj $maxu Archive] : Last Modified"
		foreach url $urls {
		    set status [$config cache status? $url]
		    if {[llength $status]} {
			set status [clock format [lindex $status 0]]
		    } else {
			set status "--"
		    }
		    puts "[lj $maxu $url] : $status"
		}

		puts ""
	    }
	}
	return
    }

    method default {args} {
	# Syntax: ?directory?
	$self CheckSyntax/Default $args query path

	if {$query} {
	    # Querying the setting for the default installation.

	    set path [$config default get]

	    if {$path eq ""} return

	    # Print location of the current default, check validity first.

	    if {![::repository::localma valid $path ro msg]} {
		$self abortp "The default installation is invalid:\n$msg"
	    }

	    puts $path

	} else {
	    # Set the default installation, if the specified path does
	    # contain such. Empty path => unset the default.

	    if {$path ne ""} {
		if {![repository::localma valid $path ro msg]} {
		    $self abortp $msg
		}
		$config default set $path

		# Print location of the current default. Validity is known.
		puts $path
	    }
	}

	return
    }

    method verify {args} {
	# ?--at DIR? ?-v?
	$self CheckSyntax/Verify $args at verbose

	if {$at eq ""} {set at [$config default get]}
	$client verify $at $verbose
	return
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation - Create/destroy repository
    #
    ## I.  Create an empty transparent repository at a specific path.
    ## II. Create an empty transparent repository at the standard path
    ##     (See "package::sys" [userdir]).

    method create {args} {
	# Syntax: ?PATH?

	if {[llength $args] > 1} {
	    $self Usage create "?path?"
	} elseif {[llength $args] == 1} {
	    set path [lindex $args 0]
	} else {
	    set path [repository::sys::userdir]
	}

	puts "Repository @ $path"

	if {[repository::localma valid $path ro msg]} return

	if {[catch {
	    repository::localma new $path
	} msg]} {
	    $self abortp "Unable to create repository at \"$path\": $msg"
	}

	puts "    Created"
	return
    }

    method delete {args} {
	# Syntax: PATH

	if {[llength $args] != 1} {
	    $self Usage delete "path"
	}

	set path [lindex $args 0]

	if {![repository::localma valid $path ro msg]} {
	    $self abortp "Unable to destroy repository at \"$path\": $msg"
	    return
	}

	foreach sh [$self SHELLS $path] {
	    puts "Unlinking @ $sh ..."
	    teapot::link::disconnect $path $sh
	}

	puts "Deleting  @ $path ..."
	file delete -force $path

	puts "Ok"
	return
    }

    method SHELLS {path} {
	set r [repository::localma %AUTO% -location $path]
	set res {}
	foreach a [$r architecture list] {
	    foreach sh [$r architecture shells $a] {
		lappend res $sh
	    }
	}
	$r destroy
	return $res
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation - (un)linking

    method link {args} {
	if {[llength $args] < 1} {
	    $self Usage link "make DIR SHELL|cut DIR SHELL|info (DIR|SHELL)"
	}

	set cmd  [lindex $args 0]
	set args [lrange $args 1 end]

	switch -exact -- $cmd {
	    make   {$self LinkMake $args}
	    2shell {$self LinkMake $args 2shell}
	    2repo  {$self LinkMake $args 2repo}
	    cut    {$self LinkCut  $args}
	    info   {$self LinkInfo $args}
	    default {
		$self Usage link "(make|2shell|2repo) DIR SHELL|cut DIR SHELL|info (DIR|SHELL)"
	    }
	}
	return
    }

    method LinkMake {argv {direction both}} {
	if {[llength $argv] != 2} {
	    $self Usage "link make" "path shell"
	}
	foreach {r s} $argv break

	if {![repository::localma valid $r ro msg]} {
	    $self abortp "Unable to link repository at \"$r\": $msg"
	}
	if {![teapot::link::shellValid $s]} {
	    $self abortp "Unable to link tcl shell at \"$s\": Not executable"
	}
	if {![teapot::link::shellHasCode $s]} {
	    global appname
	    set msg "Cannot handle teapot repositories.\n\
                     Use '$appname setup $s' to correct this."
	    $self abortp "Unable to link tcl shell at \"$s\": $msg"
	}

	if {[catch {
	    teapot::link::connect $r $s $direction
	} msg]} {
	    if {$::errorCode ne "LINK"} {
		# Rethrow internal error.
		return -code error -errorcode $::errorCode -errorinfo $::errorInfo $msg
	    }
	    # Report access problem more graceful.

	    $self abortp "\n\nOperation aborted.\n\n$msg\n\n"
	    return
	}

	puts "Ok"
	return
    }

    method LinkCut {argv} {
	if {[llength $argv] < 2} {
	    $self Usage "link cut" "path|shell path|shell ..."
	}

	# Separate arguments into the two possible sides.
	set shells {}
	set repos  {}
	foreach path $argv {
	    set type [LinkType $path]
	    switch -exact --  $type {
		shell      {lappend shells $path}
		repository {lappend repos  $path}
		undefined  {
		    # Put on both sides to allow half-cuts.
		    lappend shells $path
		    lappend repos  $path
		}
	    }
	}

	# Verify that we have something to disconnect for both sides.

	if {![llength $shells] || ![llength $repos]} {
	    if {![llength $shells] && ![llength $repos]} {
		$self abortp "Neither shells nor repositories were specified"
	    } elseif {![llength $shells]} {
		$self abortp "No shells specified"
	    } else {
		$self abortp "No repositories specified"
	    }
	}

	# Disconnect all possible pairs.

	foreach s $shells {
	    foreach r $repos {
		if {[catch {
		    teapot::link::disconnect $r $s
		} msg]} {
		    puts stdout "Note: $msg"
		}
	    }
	}

	puts "Ok"
	return
    }

    method LinkInfo {argv} {
	if {[llength $argv] != 1} {
	    $self Usage "link info" "path|shell"
	}
	set x [lindex $argv 0]

	if {[catch {
	    set type [LinkType $x]
	} msg]} {
	    $self abortp $msg
	}

	switch -exact -- $type {
	    shell {
		foreach r [teapot::link::shellInfo $x] {
		    puts "Repository $r"
		}
	    }
	    repository {
		foreach s [$self SHELLS $x] {
		    puts "Shell $s"
		}
	    }
	}
	return
    }

    proc LinkType {path} {
	upvar 1 self self

	if {[file isfile $path]} {
	    if {![teapot::link::shellValid $path]} {
		puts "Note: Unable to query tcl shell at \"$path\": Not executable"
		return undefined
	    }
	    return shell
	} elseif {[file isdirectory $path]} {
	    if {![repository::localma valid $path ro msg]} {
		puts "Note: Unable to query repository at \"$path\": $msg"
		return undefined
	    }
	    return repository
	} else {
	    puts "Note: Path \"$path\" does not exist"
	    return undefined
	}
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation - System setup (User)
    #
    ## I. Create an empty transparent repository at a specific path.
    ##    a. Optionally initialize for a specific shell.
    ##    b. Optionally initialize for all tclsh's and
    ##       wish's in the PATH.
    #
    ## II. Create an empty transparent repository at the standard path
    ##     (See "package::sys" [userdir]).
    ##     a. / b. apply as well.
    #
    ## If the repository had to be crated it will also be set as the
    ## default installation.

    method setup {args} {
	# Syntax: shell

	if {[llength $args] != 1} {
	    $self Usage setup "shell"
	}

	set s [lindex $args 0]

	if {![teapot::link::shellValid $s]} {
	    $self abortp "Tcl shell at \"$s\" is not valid"
	}

	# Make shell able to handle Tcl Modules, Platform, etc., if
	# necessary.
	puts "Looking at tcl shell $s ..."

	if {![teapot::tmbackport::shellHasCode $s]} {
	    puts "  Patching: Adding code to handle Tcl Modules ..."
	    teapot::tmbackport::shellAddCode $s
	} else {
	    puts "  Already able to handle Tcl Modules."
	}

	# Add platform packages to the shell
	if {![teapot::plat::shellHasCode $s]} {
	    puts "  Patching: Adding the platform packages ..."
	    teapot::plat::shellAddCode $s
	} else {
	    puts "  Already has the platform packages."
	}

	if {![teapot::link::shellHasCode $s]} {
	    puts "  Patching: Adding code to handle teapot repositories ..."
	    teapot::link::shellAddCode $s
	} else {
	    puts "  Already handles teapot repositories."
	}

	puts "Done"
	return
    }

    # ### ### ### ######### ######### #########

    method help {args} {
	if {[llength $args] > 1} {
	    $self Usage help "?topic?"
	}

	if {[llength $args] == 1} {
	    puts -nonewline stdout [helpOn [lindex $args 0]]
	    return
	}

	puts stdout [helpAlltopics]
    }

    proc helpOn {topic} {
	::variable here
	global appname
	if {[llength [info commands [namespace current]::helpOn_$topic]]} {
	    return [helpOn_$topic]
	}
	set tfile [file join $here help_${topic}.txt]
	if {[file exists $tfile]} {
	    return [string map [list @@ $appname] [fileutil::cat $tfile]]
	}

	set     help ""
	append  help \n
	append  help "    The topic \"$topic\" is not known." \n
	append  help "    The known topics are:" \n\n
	append  help [helpTopics]
	return $help
    }

    proc helpAlltopics {} {
	global appname

	set     help ""
	append  help \n
	append  help "    $appname" \n
	append  help "    is a tool to access package repositories" \n\n
	append  help [helpTopics]
	return $help
    }

    proc helpTopics {} {
	global appname
	::variable here
	set help ""
	foreach f [lsort [glob -nocomplain -directory $here topic_*.txt]] {
	    append help \t$appname\ help\ [fileutil::cat $f]
	}
	return $help
    }

    method onAbort {cmd} {
	set     onabort $cmd
	$client onAbort $cmd
	$config onAbort $cmd
	return
    }

    method onStop {cmd} {
	$client onStop $cmd

	#set     onstop $cmd
	#$config onStop $cmd
	return
    }

    method abortp {text} {
	$self abort [textutil::indent \
			 [textutil::adjust \
			      $text \
			      -length 64] \
			 \t]
    }

    method abort {text} {
	uplevel \#0 [linsert $onabort end $text]
	return -code error "Abort handler does not exit!"
    }

    # ### ### ### ######### ######### #########
    ## Internals

    proc MergeFlags {ov dict} {
	upvar 1 $ov o
	foreach {k v} $dict {
	    if {[info exists o($k)] && !$v} continue
	    set o($k) $v
	}
	return
    }

    method CheckSyntax/Regenerate {a av} {
	upvar 1 $av at

	set at {}
	set verbose 0
	while 1 {
	    MergeFlags o [$self Options a {--at} lasto]
	    if {$lasto eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    break
	}

	if {[llength $a]} {
	    $self Usage regenerate "?--at DIR?"
	}
	return
    }

    method CheckSyntax/Verify {a av vv} {
	upvar 1 $av at $vv verbose

	set at {}
	set verbose 0
	while 1 {
	    MergeFlags o [$self Options a {--at -v} lasto]
	    if {$lasto eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    } elseif {($lasto eq "-v")} {
		set verbose 1
		continue
	    }
	    break
	}

	if {[llength $a]} {
	    $self Usage verify "?-v? ?--at DIR?"
	}
	return
    }

    proc CheckDateTime {cmd dt} {
	set dt [string trim $dt]
	if {[string is integer -strict $dt]} {
	    return $dt
	}

	if {
	    [regexp {^(\d\d\d\d)-(\d\d)-(\d\d) +(\d\d):(\d\d):(\d\d)$} $dt] ||
	    [regexp {^(\d\d\d\d)-(\d\d)-(\d\d)$} $dt] ||
	    [regexp {^(\d\d):(\d\d):(\d\d)$} $dt]
	} {
	    # No date: clock scan uses today
	    # No time: clock scan uses midnight

	    return [clock scan $dt]
	}

	upvar 1 self self
	$self Usage $cmd "invalid timestamp \"$dt\""
	return
    }

    proc CheckLast {cmd n} {
	if {
	    ![string is integer -strict $n] || ($n <= 0)
	} {
	    upvar 1 self self
	    $self Usage $cmd "invalid count \"$n\""
	}

	return $n
    }

    method CheckSyntax/LogShow {a lv sv iv} {
	upvar 1 $lv last $sv since $iv useinstall
	set last       {}
	set since      {}
	set useinstall {}

	while 1 {
	    MergeFlags o [$self Options a {--at --last -l --since -s} lasto]
	    if {$lasto eq "--at"} {
		set useinstall [lindex $a 0]
		set a        [lrange $a 1 end]
		continue
	    } elseif {($lasto eq "-l") || ($lasto eq "--last")} {
		set last  [CheckLast {log show} [lindex $a 0]]
		set since {}
		set a     [lrange $a 1 end]
		continue
	    } elseif {($lasto eq "-s") || ($lasto eq "--since")} {
		set since [CheckDateTime {log show} [lindex $a 0]]
		set last  {}
		set a     [lrange $a 1 end]
		continue
	    }
	    break
	}

	return
    }

    method CheckSyntax/LogPurge {a lv bv iv} {
	upvar 1 $lv last $bv before $iv useinstall
	set last       {}
	set before      {}
	set useinstall {}

	while 1 {
	    MergeFlags o [$self Options a {--at --keep-last --keep-since} lasto]
	    if {$lasto eq "--at"} {
		set useinstall [lindex $a 0]
		set a        [lrange $a 1 end]
		continue
	    } elseif {$lasto eq "--keep-last"} {
		set last  [CheckLast {log purge} [lindex $a 0]]
		set before {}
		set a     [lrange $a 1 end]
		continue
	    } elseif {$lasto eq "--keep-since"} {
		set before [CheckDateTime {log purge} [lindex $a 0]]
		set last   {}
		set a      [lrange $a 1 end]
		continue
	    }
	    break
	}

	return
    }

    ## Consider cmdline for the option processing ...

    method CheckSyntax/Update {a dv lv ov} {
	upvar 1 $dv dry $lv useinstall $ov only
	set syntax "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--dry-run? ?--at DIR? ?--only uninstalled|unknown|newer|update?"
	set useinstall {}
	set only       all
	while 1 {
	    MergeFlags o [$self Options a {--http-proxy --timeout -v --dry-run --at --only} last]
	    if {$last eq "--at"} {
		set useinstall [lindex $a 0]
		set a        [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--only"} {
		set only [CheckOnly [lindex $a 0] $syntax]
		set a    [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	set dry $o(--dry-run)

	if {![llength $a]} return

	$self Usage update $syntax
	return
    }

    method CheckSyntax/Install {a iv dv fv lv rv pv ov arv} {
	upvar 1 $iv immediate $dv dry $fv force $lv useinstall $rv rec $pv ref $ov file $arv arch

	set useinstall {}
	set arch     {}
	while 1 {
	    MergeFlags o [$self Options a {
		--http-proxy --timeout -v --dry-run --force --with-recommends --at --arch
		--no-follow
	    } last]
	    if {$last eq "--at"} {
		set useinstall [lindex $a 0]
		set a        [lrange $a 1 end]
		continue
	    } elseif {$last eq "--arch"} {
		set arch [lindex $a 0]
		set a    [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    break
	}

	if {$o(-v)} {$client verbose}
	set dry   $o(--dry-run)
	set force $o(--force)

	if {$o(--no-follow)} {
	    set rec 2
	} else {
	    set rec $o(--with-recommends)
	}

	# The remaining arguments can be a set of file names, urls, or
	# a package reference. We detect filenames first (existence),
	# then check for urls (file, http, ftp schemes only), and only
	# at last assume that it is a package reference.

	# Note: We do not allow the mixing of a package reference with
	# files and urls. If there is one file or url, all remaining
	# arguments have to be files and urls.

	set immediate 0
	foreach name $a {
	    if {[file exists $name]} {
		lappend file file:$name
		set immediate 1
	    } elseif {[regexp {^((http)|(ftp)|(file)):/} $name]} {
		lappend file $name
		set immediate 1
	    } elseif {$immediate} {
		$self Usage install "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--dry-run? ?--at-default DIR? ?--force? ?--arch ARCH? ?--with-recommends? NAME ??-exact? VERSION? ?-is ENTITY?"
		# :: no-return
	    } else {
		# The name is neither file nor url nor had we files or
		# urls coming before it => This is the first name and
		# it signals the use of a package reference.
		break
	    }
	}
	if {$immediate} return

	$self CheckSyntax/PackageRef \
	    install "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--dry-run? ?--at-default DIR? ?--force? ?--arch ARCH? ?--with-recommends? NAME ??-exact? VERSION? ?-is ENTITY?" \
	    $a ref
	return
    }

    method CheckSyntax/Remove {a dv pv lv} {
	upvar 1 $pv pkg $dv dry $lv useinstall
	set useinstall {}
	set entity {}
	while 1 {
	    MergeFlags o [$self Options a {--dry-run --at --is} last]
	    if {$last eq "--at"} {
		set useinstall [lindex $a 0]
		set a        [lrange $a 1 end]
		continue
	    } elseif {$last eq "--is"} {
		set entity [lindex $a 0]
		set a  [lrange $a 1 end]
		if {![teapot::entity::valid $entity msg]} {
		    $self Usage remove "?--dry-run? ?--at DIR? ?--is entity? name ?version?"
		}
		continue
	    }
	    break
	}
	set dry $o(--dry-run)

	# No package is ok for remove => Remove all packages.
	if {[llength $a] == 0} {
	    set pkg {0}
	    return
	}

	$self CheckSyntax/PackageA \
	    remove "?--dry-run? ?--at DIR? ?--is entity? name ?version?" \
	    $a $entity pkg
    }

    method CheckSyntax/Describe {a lv av gv pv} {
	upvar 1 $pv pkg $lv useinstall $av at $gv getall

	set at {}
	set entity {}

	while 1 {
	    MergeFlags o [$self Options a {--http-proxy --timeout -v --at-default --all --at --is} last]
	    if {$last eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    } elseif {$last eq "--is"} {
		set entity [lindex $a 0]
		set a  [lrange $a 1 end]
		if {![teapot::entity::valid $entity msg]} {
		    $self Usage describe "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--at-default? ?--at dir? ?--all? ?--is entity? name ?version?" \
		}
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	set getall   $o(--all)
	set useinstall $o(--at-default)
	if {$at ne ""} {set useinstall 1}

	$self CheckSyntax/PackageA \
	    describe "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--at-default? ?--at dir? ?--all? ?--is entity? name ?version ?architecture??" \
	    $a $entity pkg
    }

    proc CheckFormat {format msg} {
	upvar 1 self self
	switch -exact -- $format {
	    table - csv - profile {}
	    default {
		$self Usage list $msg
	    }
	}
	return $format
    }

    proc CheckOnly {format msg} {
	upvar 1 self self
	switch -exact -- $format {
	    uninstalled - unknown {set format uninstalled}
	    newer       - update  {set format newer}
	    
	    default {
		$self Usage list $msg
	    }
	}
	return $format
    }

    method CheckSyntax/List {a lv av pv fv ov allv} {
	upvar 1 $pv pkg $lv useinstall $av at $fv format $ov only $allv all
	set at {}
	set entity {}
	set format table
	set only all
	set all  0
	set syntax "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--all-platforms? ?--at-default? ?--at dir? ?--as profile|csv|table? ?--only uninstalled|unknown|newer|update? ?--is entity? ?name ?version??"

	while 1 {
	    MergeFlags o [$self Options a {
		--http-proxy --timeout -v --at-default --as --at --is
		--only --all-platforms
	    } last]
	    if {$last eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    } elseif {$last eq "--is"} {
		set entity [lindex $a 0]
		set a  [lrange $a 1 end]
		if {![teapot::entity::valid $entity msg]} {
		    $self Usage list $syntax
		}
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--as"} {
		set format [CheckFormat [lindex $a 0] $syntax]
		set a      [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--only"} {
		set only [CheckOnly [lindex $a 0] $syntax]
		set a    [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	if {$o(--all-platforms)} {set all 1}
	set useinstall  $o(--at-default)
	if {$at ne ""} {set useinstall 1}

	# No package is ok for list
	if {[llength $a] == 0} {
	    if {$entity eq ""} {
		set pkg [teapot::listspec::all]
	    } else {
		set pkg [teapot::listspec::eall $entity]
	    }
	    return
	}

	$self CheckSyntax/Package \
	    list "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--all-platforms? ?--at-default? ?--at dir? ?--as profile|csv|table? ?--is entity? ?name ?version??" \
	    $a $entity pkg
    }

    method CheckSyntax/Get {a lv av pv ov} {
	upvar 1 $pv pkg $lv useinstall $av at $ov output
	set at {}
	set entity {}
	set output [pwd]
	while 1 {
	    MergeFlags o [$self Options a {--http-proxy --timeout -v --at-default --output --at --is} last]
	    if {$last eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    } elseif {$last eq "--output"} {
		set output [lindex $a 0]
		set a      [lrange $a 1 end]
		continue
	    } elseif {$last eq "--is"} {
		set entity [lindex $a 0]
		set a  [lrange $a 1 end]
		if {![teapot::entity::valid $entity msg]} {
		    $self Usage get "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--at-default? ?--at dir? ?--output dir? ?--is entity? ?name ?version ?architecture???"
		}
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	set useinstall  $o(--at-default)
	if {$at ne ""} {set useinstall 1}

	# No package is ok for get
	if {[llength $a] == 0} {
	    set pkg {0}
	    return
	}

	$self CheckSyntax/PackageA \
	    get "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--at-default? ?--at dir? ?--output dir? ?--is entity? ?name ?version ?architecture???" \
	    $a $entity pkg
    }

    method CheckSyntax/PackageRef {cmd syntax a rv} {
	upvar 1 $rv ref

	# TODO: Simply use references::cons|valid!
	#       Now accepts this syntax, and more.

	if {[llength $a] == 1} {
	    set ref [teapot::reference::cons [lindex $a 0]]
	    return
	} elseif {[llength $a] == 2} {
	    if {[teapot::version::valid [lindex $a 1]]} {
		foreach {n v} $a break
		set ref [teapot::reference::cons $n -require $v]
		return
	    }
	} elseif {[llength $a] == 3} {
	    if {[lindex $a 1] eq "-exact"} {
		if {[teapot::version::valid [lindex $a 2]]} {
		    foreach {n _ v} $a break
		    set ref [teapot::reference::cons $n -require [list $v $v]]
		    return
		}
	    } elseif {[lindex $a 1] eq "-is"} {
		if {[teapot::entity::valid [lindex $a 2]]} {
		    foreach {n _ e} $a break
		    set ref [teapot::reference::cons $n -is $e]
		    return
		}
	    }
	} elseif {[llength $a] == 4} {
	    if {[lindex $a 2] eq "-is"} {
		if {
		    [teapot::version::valid [lindex $a 1]] &&
		    [teapot::entity::valid  [lindex $a 3]]
		} {
		    foreach {n v _ e} $a break
		    set ref [teapot::reference::cons $n -is $e -require $v]
		    return
		}
	    }
	} elseif {[llength $a] == 5} {
	    if {
		([lindex $a 1] eq "-exact") &&
		([lindex $a 3] eq "-is")
	    } {
		if {
		    [teapot::version::valid [lindex $a 2]] &&
		    [teapot::entity::valid  [lindex $a 4]]
		} {
		    foreach {n _ v _ e} $a break
		    set ref [teapot::reference::cons $n -is $e -require [list $v $v]]
		    return
		}
	    }
	}
	$self Usage $cmd $syntax
	return
    }

    method CheckSyntax/Package {cmd syntax a entity pv} {
	# Syntax: name ?version?

	upvar 1 $pv pkg
	if {
	    ((0 < [llength $a]) && ([llength $a] < 3)) &&
	    (([llength $a] == 1) || [teapot::version::valid [lindex $a 1]])
	} {
	    set pkg [teapot::listspec::cons $entity $a]
	    return
	}
	$self Usage $cmd $syntax
	return
    }

    method CheckSyntax/PackageA {cmd syntax a entity pv} {
	# Syntax: name ?version ?architecture??

	upvar 1 $pv pkg
	if {
	    ((0 < [llength $a]) && ([llength $a] < 4)) &&
	    (([llength $a] == 1) || [teapot::version::valid [lindex $a 1]])
	} {
	    set pkg [teapot::listspec::cons $entity $a]
	    return
	}
	# Allow version == ALL ... work around the listspec validation.
	if {
	    ((0 < [llength $a]) && ([llength $a] < 4)) &&
	    (([llength $a] == 1) || [lindex $a 1] eq "ALL")
	} {
	    set a [lreplace $a 1 1 0]
	    set pkg [lreplace [teapot::listspec::cons $entity $a] 2 2 ALL]
	    return
	}
	$self Usage $cmd $syntax
	return
    }

    method CheckSyntax/Profiles {a lv av} {
	upvar 1 $lv useinstall $av at

	set at {}
	while 1 {
	    MergeFlags o [$self Options a {--http-proxy --timeout -v --at-default --at} last]
	    if {$last eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	set useinstall  $o(--at-default)
	if {$at ne ""} {set useinstall 1}

	return
    }

    method CheckSyntax/Search {a lv av qv fv} {
	upvar 1 $lv useinstall $qv query $av at $fv format
	if {[llength $a] == 0} {
	    $self Usage search "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--at-default? ?-at dir? ?--as profile|csv|table? QUERY"
	}

	# Using a custom option processor, as the options actually
	# specify a complex expression. We have a small SLR parser in
	# here.

	# First strip off the --at-default and --at flags, if any.

	set at {}
	set format table
	while 1 {
	    MergeFlags o [$self Options a {--http-proxy --timeout -v 
		--at-default --at
		--as
		-has -nhas -is -nis
		-eq -re -glob
		-ne -nre -nglob
		-lt -gt -le -ge
		-in -ni
		-a -o -and -or
	    } last]
	    if {$last eq "-v"} {
		continue
	    }
	    if {[regexp {^-[^-]} $last]} {
		# put the option back for query parser
		set a [linsert $a 0 $last]
		break
	    }
	    if {$last eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--as"} {
		set format [CheckFormat [lindex $a 0] "?--timeout seconds? ?--http-proxy Host:Port? ?-v? ?--at-default? ?--at dir? ?--as profile|csv|table? QUERY"]
		set a      [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	set useinstall  $o(--at-default)
	if {$at ne ""} {set useinstall 1}

	# Now parse the query arguments

	if {[llength $a] == 1} {

	    # A single argument is not a valid query, normally. We now
	    # convert this to a simple and easy query: A case
	    # insensitive substring search on the entity name.

	    set query [list key name rex "(?i)[lindex $a 0]"]
	} else {
	    set query [_parse $a]
	}

	log::debug $query
	#puts DONE==========================
	return
    }


    proc _parse {a} {
	upvar 1 self self
	# Hardwired bottom up parsing.

	set values {} ; # Value stack
	set ops    {} ; # Operator stack
	set capture 0
	set op     ""

	#puts ===============================

	foreach e $a {
	    # Debug logging ...
	    #puts V\t([join $values ") ("])
	    #puts O\t([join $ops ") ("])
	    #puts C\t$capture
	    #puts .\t$op
	    #puts ===============================
	    #puts +\t($e)
	    #puts -------------------------------

	    if {$capture} {
		lappend values $e
		incr capture -1
		if {!$capture} {
		    # Compose the simple expression
		    if {$op eq "haskey"} {
			# 1 argument expression
			set values [lreplace $values end end \
					[list \
					     haskey \
					     [string tolower \
						  [lindex $values end]]]]
		    } elseif {$op eq "nhaskey"} {
			# 1 argument expression
			set values [lreplace $values end end \
					[list \
					     nhaskey \
					     [string tolower \
						  [lindex $values end]]]]
		    } elseif {$op eq "is"} {
			# 1 argument expression
			set values [lreplace $values end end \
					[list is [lindex $values end]]]
		    } elseif {$op eq "nis"} {
			# 1 argument expression
			set values [lreplace $values end end \
					[list nis [lindex $values end]]]
		    } else {
			# 2 argument expression, key value

			set attr  [string tolower [lindex $values end-1]]
			set value [lindex $values end]

			# Validate regexp patterns
			if {[string match {*rex} $op] &&
			    [catch {regexp -about $value}]} {
				$self abortp "Bad regular expression \"$value\""
			}

			set values [lreplace $values end-1 end \
					[list key $attr $op $value]]
		    }
		}
		continue
	    }
	    switch -exact -- $e {
		-has - has {
		    set capture 1
		    set op haskey
		}
		-nhas - nhas {
		    set capture 1
		    set op nhaskey
		}
		-is - is {
		    set capture 1
		    set op is
		}
		-nis - nis {
		    set capture 1
		    set op nis
		}
		eq - re  - glob  -
		ne - nre - nglob -
		lt - gt - le - ge - in - ni -
		-eq - -re  - -glob  -
		-ne - -nre - -nglob -
		-lt - -gt - -le - -ge - -in - -ni {
		    set capture 2
		    set op      $opmap($e)
		}
		-or  - -o - or -
		-and - -a - and {
		    set now $opmap($e)
		    if {[llength $ops] > 0} {
			set pre [lindex $ops end]
			if {$opprio($pre) <= $opprio($e)} {
			    # pre binding nearer than current, reduce
			    # before push of new operator

			    _reduce ops values $pre
			}
		    }
		    lappend ops $now
		}
		\{ - \( {
		    # Open grouping
		    lappend ops \(
		}
		\} - \) {
		    # Close grouping, reduce to marker

		    while {[lindex $ops end] ne "("} {
			_reduce ops values [lindex $ops end]
		    }

		    # Remove marker
		    set ops [lrange $ops 0 end-1]
		}
		default {
		    $self abortp "Bad expression element \"$e\""
		}
	    }
	}

	#puts V\t([join $values ") ("])
	#puts O\t([join $ops ") ("])
	#puts C\t$capture
	#puts .\t$op

	while {[llength $ops] > 0} {
	    set op  [lindex $ops end]
	    if {$op eq "("} {
		$self abortp "Bad expression"
	    }
	    _reduce ops values $op
	}

	#puts V\t([join $values ") ("])
	#puts O\t([join $ops ") ("])
	#puts C\t$capture
	#puts .\t$op
	#puts QUERY==========================

	if {[llength $values] != 1} {
	    $self abortp "Bad expression"
	}

	#puts DONE==========================
	return [lindex $values 0]
    }

    proc _reduce {ostack vstack op} {
	upvar 1 $ostack ops $vstack values

	set va [lindex $values end-1]
	set vb [lindex $values end]
	if {[lindex $va 0] eq $op} {
	    # The previous query has the same
	    # operator, just extend it with the
	    # new one. This reduces nesting.

	    set     newquery $va
	    lappend newquery $vb
	} else {
	    # The new query has to add a nesting
	    # layer.

	    set newquery [list $op $va $vb]
	}

	set values [lreplace $values end-1 end $newquery]
	set ops    [lrange $ops 0 end-1]
	return
    }

    typevariable opmap -array {
	-eq    eq	-re    rex
	-glob  glob	-ne    ne
	-nre   !rex	-nglob !glob
	-lt    <	-gt    >
	-le    <=	-ge    >=
	-in    in	-ni    ni
	eq    eq	re    rex
	glob  glob	ne    ne
	nre   !rex	nglob !glob
	lt    <		gt    >
	le    <=	ge    >=
	in    in	ni    ni
	-a and -and and and and
	-o or  -or  or  or  or
    }
    typevariable opprio -array {
	-a 0 -and 0 and 0 ( 2
	-o 1 -or  1 or  1
    }

    method CheckSyntax/Keys {a lv av} {
	upvar 1 $lv useinstall $av at
	set at {}
	while 1 {
	    MergeFlags o [$self Options a {--http-proxy --timeout -v --at-default --at} last]
	    if {$last eq "--at"} {
		set at [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--http-proxy"} {
		$self SetProxy [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    if {$last eq "--timeout"} {
		$self SetTimeout [lindex $a 0]
		set a  [lrange $a 1 end]
		continue
	    }
	    break
	}
	if {$o(-v)} {$client verbose}
	set useinstall $o(--at-default)
	if {$at ne ""} {set useinstall 1}
	return
    }

    method CheckSyntax/CacheOff {a} {
	if {[llength $a] > 0} {
	    $self Usage {cache off} ""
	}
    }

    method CheckSyntax/CacheStatus {a} {
	if {[llength $a] > 0} {
	    $self Usage {cache status} ""
	}
    }

    method CheckSyntax/CacheClear {a} {
	if {[llength $a] > 0} {
	    $self Usage {cache clear} ""
	}
    }

    method CheckSyntax/CacheOn {a hv dv} {
	upvar 1 $hv hasdir $dv dir
	if {[llength $a] > 1} {
	    $self Usage {cache on ?dir?} ""
	} elseif {[llength $a] == 1} {
	    set hasdir 1
	    set dir [lindex $a 0]
	} else {
	    set hasdir 0
	}
	return
    }

    method CheckSyntax/ArchiveList {a} {
	if {[llength $a] > 0} {
	    $self Usage {archive list} ""
	}
    }

    method CheckSyntax/ArchiveAdd {a uv} {
	upvar 1 $uv url
	if {[llength $a] != 1} {
	    $self Usage {archive add} "url"
	}
	set url [string trimright [lindex $a 0] /]
	return
    }

    method CheckSyntax/ArchiveRemove {a uv} {
	upvar 1 $uv url
	if {[llength $a] != 1} {
	    $self Usage {archive remove} "url"
	}
	set url [string trimright [lindex $a 0] /]
	return
    }

    method CheckSyntax/Default {a qv pv} {
	upvar 1 $qv query $pv path
	if {[llength $a] > 1} {
	    $self Usage default " ?directory?"
	} elseif {[llength $a] == 1} {
	    set query 0
	    set path [file join [pwd] [lindex $a 0]]
	} else {
	    set query 1
	}
	return
    }

    method SetProxy {a} {
	set hp [split $a :]
	if {[llength $hp] == 2} {
	    foreach {h p} $hp break
	    $config proxy user set $h $p
	} else {
	    $self abortp "Expected HOST:PORT for proxy setting"
	}
    }

    method SetTimeout {seconds} {
	if {![string is integer -strict $seconds]} {
	    $self abortp "Expected integer for timeout setting"
	}
	$config timeout user set $seconds
	return
    }

    method Usage {cmd syntax} {
	$self abort "Usage: $::appname $cmd $syntax\nTo get more information run the command\n\n\t$::appname help $cmd\n"
    }

    method Options {av accept {lastv {}}} {
	upvar 1 $av a
	if {$lastv ne ""} {upvar 1 $lastv last ; set last {}}
	foreach o $accept {set v($o) 0}
	while {[llength $a] && [string match -* [set o [lindex $a 0]]]} {
	    if {[lsearch -exact $accept $o] >= 0} {
		set v($o) 1
		set a [lrange $a 1 end]
		set last $o
	    } else {
		if {[llength $accept] == 1} {
		    $self abortp \
			"Unknown option \"$o\", should have been\
                         $accept"
		} else {
		    $self abortp \
			"Unknown option \"$o\", should have been one\
                         of [linsert [join [lsort $accept] ", "] end-1 or]"
		}
	    }
	}
	array get v
    }

    method SetInstall {path} {
	# Change the default installation used by the client.

	if {$path eq ""} return

	if {![repository::localma valid $path ro msg]} {
	    # For test situations we may allow the use of opaque
	    # repositories
	    if 0 {
		if {![repository::sqlitedir valid $path ro msgb]} {
		    $self abortp $msg
		}

		$client install \
		    [repository::sqlitedir %AUTO% -location $path]
		return
	    }

	    $self abortp $msg
	}

	$client install \
	    [repository::localma %AUTO% -location $path]
	return
    }

    # ### ### ### ######### ######### #########

    proc maxstr {v str} {
	upvar 1 $v max
	set l [string length $str]
	if {$l > $max} {set max $l}
	return
    }

    proc lj {n s} {format %-*s $n $s}

    # ### ### ### ######### ######### #########
    ## Data structures

    variable config {} ; # Client configuration database
    variable client {} ; # Client control. Not set if there
    #                      are no archives and/or no local
    #                      repository.

    # Callback when aborting operation due to argument errors.

    variable onabort {}

    ##
    # ### ### ### ######### ######### #########
}

# ### ### ### ######### ######### #########
## Data structures

namespace eval ::repository::capp {
    ::variable here [file dirname [info script]]
}

# ### ### ### ######### ######### #########
## Ready

package provide repository::capp 0.1
