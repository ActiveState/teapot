# -*- tcl -*-
# ### ### ### ######### ######### #########
## Overview

## snit::type implementing the functional layer of the TEAPOT client
## application. Functional means here that instances control the
## various repositories while being called with fully expanded
## parameter sets. Another, higher, layer is responsible for parsing
## the command into these sets.

# Copyright (c) 2017 ActiveState Software Inc.
# Released under the BSD-3 license. See LICENSE file for details.

# ### ### ### ######### ######### #########
## Requirements

package require fileutil
package require ftp::geturl
package require http
package require logger
package require repository::localma
package require repository::proxy
package require repository::sqlitedir
package require repository::tap
package require snit
package require teapot::instance          ; # Instance handling
package require teapot::listspec          ; # Handling of listing specs.
package require teapot::metadata
package require teapot::metadata::read
package require teapot::metadata::write
package require teapot::redirect
package require teapot::reference         ; # Reference handling
package require textutil
package require uri
package require vfs
package require csv

# ### ### ### ######### ######### #########
## Implementation

logger::initNamespace ::repository::client
snit::type ::repository::client {
    # ### ### ### ######### ######### #########
    ## API - Package management

    # (1a) install-single ...
    # ---- = Boolean = dry-run
    # ---- = Boolean = force
    # ---- = Boolean = recommends
    # ---- = PkgRef  = package

    # (1b) install-set
    # ---- = Boolean = force
    # ---- = Path    = File listing package instances

    # (2.) remove
    # ---- = Boolean = dry-run
    # ---- = PkgRef  = package

    # (3.) describe
    # ---- = Boolean = useinstall
    # ---- = Boolean = getall
    # ---- = PkgRef  = package

    # (4.) list
    # ---- = Boolean = useinstall
    # ---- = PkgRef  = package

    # (5.) search
    # ---- = Boolean   = useinstall
    # ---- = RepoQuery = query

    # (xx) set configuration - construction
    # ---- = Object  = config accessor object

    # (xx) log/output destination - construction
    # ---- = Cmd prefix

    # ### ### ### ######### ######### #########
    ##

    constructor {thelog theconfig} {
	set log     $thelog
	set config  $theconfig
	return
    }

    # ### ### ### ######### ######### #########
    ##

    method regenerate {r} {
	if {[catch {
	    set r [ArchiveOpen $r ro $config]
	} msg]} {
	    $self abortp $msg
	}

	$r RegenerateCLIPs [mymethod Puts]
	return
    }

    # ### ### ### ######### ######### #########
    ##

    method {log show} {n since} {
	set r [$self Installation ro]

	if {$n == {} && $since == {}} {
	    set logentries [$r log show all]
	} elseif {$n != {}} {
	    set logentries [$r log show n $n]
	} elseif {$since != {}} {
	    set logentries [$r log show since $since]
	} else {
	    error "Bad n/since settings"
	}

	$self GenLogListing $logentries
	return
    }

    method {log purge} {n before} {
	set r [$self Installation ro]

	if {$n == {} && $before == {}} {
	    $r log purge all
	} elseif {$n != {}} {
	    $r log purge keep $n
	} elseif {$before != {}} {
	    $r log purge before $before
	} else {
	    error "Bad n/before settings"
	}
	return
    }

    # ### ### ### ######### ######### #########
    ##


    method updateSelf {e n v a verbose} {
	set test 0
	set A [info nameofexecutable]

	# Support update-self for starkit and unwrapped execution. The
	# latter means that the package directories are not a mounted
	# virtual fs, but native on disk.

	if {$::starkit::mode eq "starpack"} {
	    set A [info nameofexecutable]
	    set mounted 1
	} else {
	    set A $::starkit::topdir
	    set mounted [expr {$::starkit::mode eq "starkit"}]
	}

	# If we are system-distributed teacup we switch the location
	# of the retrieved teacup from /System/Library to /Library as
	# we must not write over the system files. We assume that a
	# facade in /usr/bin/teacup or other place which switches to
	# the teacup under /Library if it is present, with the /System
	# teacup as fallback. In that case we can also say that the
	# package dirs are not in a mounted virtual fs.
	if {
	    $::tcl_platform(os) eq "Darwin" &&
	    [string match /System/* $A]
	} {
	    set A [string range $A [string length /System] end]
	    set mounted 0
	}

	if {$test} {
	    puts $A|\t|$::argv0
	} else {
	    # NOTE: In a starpack the executable A is seen as a
	    # directory, and not a file.  However we can (mostly)
	    # assume that all required files have already been loaded,
	    # and that we exit after the update. So unmounting the
	    # executable is not harmful.
	    #
	    # NOTE 2: We have to use the low-level unmount, because
	    # the starpack was only mounted at that level. The
	    # high-level mount/umount code does not know about this
	    # filesystem!

	    if {$mounted} {
		vfs::filesystem unmount $A
	    }

	    # Note 3: The additional check for existence of A is
	    # needed for the /System to /Library switch. In that case
	    # the chosen destination may not exist, making the checks
	    # moot.

	    set p [platform::identify]
	    if {
		![string match aix-*  $p] &&
		![string match hpux-* $p]
	    } {
		# We do not check our own permissions on AIX and
		# HPUX. We know that our own executable will be
		# reported as 'not writable', simply because these
		# OS's disallow writing to a running executable.

		# Side note: Win32 disallows that too, but is not
		# reporting that through the regular permission checks
		# like 'file writable'. It needs even more band-aid
		# later.

		# Back to AIX/HPUX, we ignore the permissions, as we
		# can rename ourselves and then rename the new
		# executable into our place. That still works. We do
		# have to check the permissions on the directory
		# containing the executable however.

		if {![file writable $A]} {
		    AbortNoWritePrivileges $A
		}
	    } elseif {[file exists [file dirname $A]]} {
		# AIX,HPUX, check directory containing the executable
		# for write permissions, see above for explanations.

		if {![file writable [file dirname $A]]} {
		    AbortNoWritePrivileges $A
		}
	    } else {
		if {![file writable [file dirname $A]]} {
		    AbortNoWritePrivileges $A
		}
	    }

	    # We remount ourselves using the low-level command, as the
	    # http code (for accessing remote repositories) has not
	    # been loaded yet and is now needed. This is why above we
	    # could say only 'mostly'. Only at location (xx) below can
	    # we truly unmount without repercussions. We had to
	    # unmount above however to properly check the permissions
	    # on the executable. Without it would be seen as virtual
	    # and writable directory, regardless of the permissions
	    # seen by the native filesystem.

	    if {$mounted} {
		vfs::filesystem mount $A [list ::vfs::mk4::handler exe]
	    }
	}
	# --

	$self Puts "Updating $A ..."

	# We now force usage of ActiveState's teapot repository, and
	# ignore whatever the user has configured. We take care to
	# __not save__ this change persistently in the
	# configuration. The user may also have asked for verbose
	# output with regard to INDEX retrieval and such.

	if {!$test} {
	    $config archive clear                             !persistent
	    $config archive add http://teapot.activestate.com !persistent
	}
	if {$verbose} {
	    $self verbose
	}

	set ref [teapot::reference::cons $n -version $v -is $e]
	# Bug 73962 dummy data, the actual shell is not relevant here.
	lappend _tcls 8.4

	# Disable pending, shell
	set findstatus [$self Find \
			    [platform::patterns $a] \
			    $ref repo repotype einstance 1 0]
	if {!$findstatus} {
	    $self Puts "Not found in the archive."
	    return
	} elseif {$findstatus == 2} {
	    $self Puts "Cannot resolve ambiguous reference '[teapot::reference::ref2tcl $ref]'.\n"
	    $self Puts "Please contact ActiveState at <support@activestate.com>."
	    # At this point einstance = list (instances)
	    # Transform into the dictionary expected by GenListing.
	    set eitmp {}
	    foreach ei $einstance {
		lappend eitmp $ei {}
	    }
	    $self GenListing $eitmp \t
	    unset eitmp
	    return
	}

	teapot::instance::split $einstance ex nx vx ax

	if {[package vcompare $vx $v] <= 0} {
	    $self Puts "Nothing to do. Found only older or same version"
	    return
	}

	$self Puts "Found newer version $vx ($ax) @ [$repo cget -location]"

	set instances [$self Retrieve 0 1 [list $einstance [list $repo]] 1 1]
	if {[llength $instances] != 2} {
	    $self Puts "Bad retrieval."
	    return
	}

	if {$test} {
	    $self Puts "TEST. No actual update"
	    exit
	}

	foreach {f del link} [lindex $instances 1] break
	# Assert (!link)

	$self Puts "Copying ..."

	# (xx)
	if {$mounted} {
	    vfs::filesystem unmount $A
	}

	if {$::tcl_platform(platform) eq "windows"} {
	    $self win_over $A $f
	} else {
	    $self unix_over $A $f
	}

	$self Puts "  Ok"
	return
    }

    proc AbortNoWritePrivileges {A} {
	upvar 1 self self
	global appname
	$self abortp "Updating $A has been found to be impossible\
              due to insufficient privileges.\n\nYou (and\
              by extension $appname) have no permissions\
              to write its containing directory.\n\nA possible\
              work-around may be to run the update-self command\
              through 'sudo'. Otherwise you will have to contact\
              your system administrator(s)."
	# == Not reached ==
    }

    method unix_over {old new} {
	# Copy or move the files to the application. After the running
	# executable is moved out of the way. Several platforms lock
	# it against overwriting.

	if {[file exists $old]} {
	    catch {file delete -force ${old}.bak}
	    file rename $old ${old}.bak
	} else {
	    # OS X, we are in a switch from /System/Library to
	    # /Library
	    file mkdir [file dirname $old]
	}

	file rename -force $new $old
	catch {file attributes $old -permissions ugo+rx}
	return
    }

    method win_over {old new} {
	# A little hacky, but on Windows, since we can't delete
	# Ourselves, we wait on the "Finish" to launch a batch file
	# which does the needed file copying. And if even that is
	# not possible we simply ask the user to perform the operation.

	catch {file delete -force ${old}.bak}
	file copy $old ${old}.bak

	# Make the needed batch file a sibling of the teacup which
	# updates itself, that way we should be sure to have write
	# access to the directory.
	set uninst [file dirname $old]/teacup-over.bat
	#set uninst "C:/teacup-over.bat"

	if {[catch {open $uninst w} fid]} {
	    $self Puts "Unable to create the bat file \"$uninst\" to"
	    $self Puts "copy the new revision of teacup over the"
	    $self Puts "currently installed revision."
	    $self Puts ""
	    $self Puts "Please copy the file \"$new\""
	    $self Puts "over the file        \"$old\""
	    $self Puts "to complete the update."
	    $self Puts ""
	    $self Puts "Thank you for your help and cooperation."
	    $self Puts ""
	} else {
	    global exitHook
	    set    exitHook "exec [concat [auto_execok start] {"TEAcup Replace"} "\"$uninst\""] &"

	    set new [file nativename [file attributes $new -shortname]]
	    set old [file nativename [file attributes $old -shortname]]

	    puts  $fid "@ECHO OFF"
	    puts  $fid "ECHO This script copies the new teacup over the"
	    puts  $fid "ECHO current installed application. It can be"
	    puts  $fid "ECHO deleted after run."
	    puts  $fid "ECHO ."
	    puts  $fid "ECHO Waiting 2 seconds for the original teacup"
	    puts  $fid "ECHO to go away before trying to overwrite it"
	    puts  $fid "ECHO with the new executable."
	    #puts  $fid "TYPE NUL | CHOICE.COM /N /CY /TY,2 >NUL"
	    # CHOICE doesn't work for Win NT 4+
	    # PING however we can assume to be present (network). The
	    # option -n 3 means to ping three times. And the delay
	    # between two pings is about a second. Discard the output,
	    # and we have our delay. Using the IP for localhost we are
	    # sure too that there won't be a failure.
	    puts  $fid "PING 127.0.0.1 -n 3 >NUL"
	    puts  $fid "CD C:\\"
	    puts  $fid "ECHO Copying ..."
	    puts  $fid "COPY   \"$new\" \"$old\""
	    puts  $fid "IF ERRORLEVEL == 1 GOTO FAILED"
	    puts  $fid "ECHO Deleting tempfile ..."
	    puts  $fid "DEL /F \"$new\""
	    puts  $fid "ECHO Done.  Exiting ..."
	    # We can delete the .bat, but then it won't be able to perform the exit anymore.
	    #puts  $fid  "DEL /F \"[file nativename [file attributes $uninst -shortname]]\""
	    #puts  $fid "ECHO Done. Exiting."
	    #puts  $fid "PAUSE >NUL"
	    puts  $fid "EXIT"
	    puts  $fid ":FAILED"
	    puts  $fid "ECHO The copying failed for unknown reasons."
	    puts  $fid "ECHO You will have to perform his step manually."
	    puts  $fid "ECHO The downloaded executable has been left at"
	    puts  $fid "ECHO \"$old\""
	    puts  $fid "ECHO and has to be copied to"
	    puts  $fid "ECHO \"$new\""
	    puts  $fid "ECHO Press 'Return' to acknowledge and exit ..."
	    puts  $fid "PAUSE > NUL"
	    puts  $fid "EXIT"
	    close $fid
	}
    }

    method verify {r verbose} {
	# r = path ... Convert to actual repository object.

	$self Puts "Checking $r ..."

	if {[catch {
	    set r [ArchiveOpen $r ro $config]
	} msg]} {
	    $self abortp $msg
	}

	if {$verbose} {
	    package require log
	    set xlog [mymethod VerifyLog]
	} else {
	    set xlog [mymethod VerifyDevNull]
	}

	if {[catch {$r sync verify $xlog} msg]} {
	    $self abortp "Failed: $msg"
	} else {
	    $self stop "Ok. $msg"
	}
	return
    }

    method VerifyDevNull {args} {}

    method VerifyLog {level text} {
	if {$level eq "info"} {
	    set show "    "
	} else {
	    set show $level
	}
	$self Puts "$show$::log::fill($level) $text"
	return
    }

    method install {r} {
	# Overide the default installation with a user default. We
	# store it where the system would normally cache the default
	# one, preventing the use of the default.

	# The caller has validated the repository for read-only access
	# only.

	set theinstall $r
	set themode    ro

	# Stash the location into the in-memory configuration, as our
	# default location. Don't make it permanent however.
	$config default setl [$theinstall cget -location]
	return
    }

    method verbose {} {
	set quiet 0
	return
    }

    # ### ### ### ######### ######### #########
    ## API - Implementation.

    method Update {dry only} {
	set spec [teapot::listspec::all]
	set data [$self RunX ro 0 list $spec]

	# data         = dict (repo -> list(ext-instance))
	# ext-instance = {name ver arch isprofile}

	set donewer       [expr {($only eq "all") || ($only eq "newer")}]
	set douninstalled [expr {($only eq "all") || ($only eq "uninstalled")}]

	if {$donewer} {
	    set newer [Invert \
			   [$self FilterRedirections \
				[$self FilterList newer \
				     $data]]]
	} else {
	    set newer {}
	}

	if {$douninstalled} {
	    set uninstalled [Invert \
				 [$self FilterRedirections \
				      [$self FilterList uninstalled \
					   $data]]]
	} else {
	    set uninstalled {}
	}

	if {![llength $newer] && ![llength $uninstalled]} {
	    $self Puts "Nothing to update."
	    $self ReportInactives
	    return
	}

	# newer        = dict (ext-instance -> list(repo))
	# uninstalled  = dict (ext-instance -> list(repo))
	# ext-instance = {type name ver arch isprofile}

	if {$donewer} {
	    set newer       [$self Retrieve $dry 1 [DictSort $newer]       1 0]
	}
	if {$douninstalled} {
	    set uninstalled [$self Retrieve $dry 1 [DictSort $uninstalled] 1 0]
	}

	set nn [expr {[llength $newer]      /2}]
	set nu [expr {[llength $uninstalled]/2}]

	# newer       = dict (instance -> {path delete link})
	# uninstalled = dict (instance -> {path delete link})
	# instance    = {name ver arch}

	if {$nn} {
	    $self Puts ""
	    $self Puts "Installing [nsp $nn {newer package}]"
	    $self Install $dry $newer
	}
	if {$nu} {
	    $self Puts ""
	    $self Puts "Installing [nsp $nu {unknown package}]"
	    $self Install $dry $uninstalled
	}

	$self Puts ""
	$self Puts "Installed [nsp $nn {newer   package}]"
	$self Puts "Installed [nsp $nu {unknown package}]"

	$self ReportInactives
	return
    }

    method installEntity {dry force recommends eref arch} {
	set instances [$self ResolveEntity \
			   $arch $force $recommends $eref]

	# instances    = dict (ext-instance -> list(repo))
	# ext-instance = {type name ver arch isprofile}

	set instances [$self Retrieve $dry $force $instances 0 0]

	# instances = dict (instance -> {path delete link})
	# instance  = {name ver arch}

	$self Install $dry $instances
	$self ReportInactives
	return
    }

    method installUrl {dry force recommends url arch} {
	# Note: Check if the package contained in the file is
	#       installed already.

	# url :: list (file/url...)

	set paths {}
	foreach u $url { lappend paths [GetUrl $u] }
	# above => struct::list map

	set instances  {}
	set references {}
	set arch [$self Arch $arch]
	set local {}

	foreach path $paths {
	    foreach r [$self ResolveFile \
			   $arch $force $recommends $path \
			   isprofile urlinstance already] {
		lappend references $r
	    }
	    lappend local $urlinstance
	    if {!$isprofile && !$already} {
		# GetUrl always returns a temp. file, hence
		# delete(yes).  This then implies linkable(no).

		lappend instances $urlinstance [list $path 1 0]
	    }
	}

	# Resolve the references of all package files together.

	set refinstances [$self DoResolve $arch $force $recommends $references $local]

	# instances    = dict (ext-instance -> list (repo))
	# ext-instance = {name ver arch isprofile}

	foreach i [$self Retrieve $dry $force $refinstances 0 0] {
	    lappend instances $i
	}

	# instances = dict (instance -> {path delete link})
	# instance  = {name ver arch}

	$self Install $dry $instances
	$self ReportInactives
	return
    }


    method remove {dry pkg} {
	# Use rw for list to abort early, not at first delete.

	foreach instance [lsort -dict [$self RunInstall rw list $pkg]] {
	    # instance = {entity name ver arch isprofile} drop the profile flag

	    teapot::instance::norm instance
	    $self Puts " Removing instance $instance"
	    if {$dry} continue
	    $self RunInstall rw del $instance
	}
	return
    }


    method describe {useinstall getall pkg} {
	# pkg = listspec

	# Platform filtering is on.
	set keepall 0
	set afilter {}

	# For instances and einstances (search for name, version,
	# architecture, eventually also entity), platform filtering is
	# off because the platform is specified by the user, and the
	# special platform 'ALL' drops back to not restricting the
	# platform, keeping the filter off (That was the original
	# default behaviour when no platform was specified).

	set ltype [teapot::listspec::type $pkg]
	switch -exact -- $ltype {
	    instance {
		set keepall 1
		teapot::listspec::split $pkg e n v a
		if {$v eq "ALL"} {
		    set pkg [teapot::listspec::name $n]
		    if {$a ne "ALL"} {
			set afilter $a
		    }
		} elseif {$a eq "ALL"} {
		    set pkg [teapot::listspec::version $n $v]
		}
	    }
	    einstance {
		set keepall 1
		teapot::listspec::split $pkg e n v a
		if {$v eq "ALL"} {
		    set pkg [teapot::listspec::ename $e $n]
		    if {$a ne "ALL"} {
			set afilter $a
		    }
		} elseif {$a eq "ALL"} {
		    set pkg [teapot::listspec::eversion $e $n $v]
		}
	    }
	    version {
		teapot::listspec::split $pkg e n v a
		if {$v eq "ALL"} {
		    set pkg [teapot::listspec::name $n]
		}
	    }
	    eversion {
		teapot::listspec::split $pkg e n v a
		if {$v eq "ALL"} {
		    set pkg [teapot::listspec::ename $e $n]
		}
	    }
	}

	set pkglabel [teapot::listspec::print2 $pkg]

	set data [$self RunX ro $useinstall list $pkg]

	if {$afilter ne {}} {
	    # Drop unwanted platforms (currently listspec do not
	    # support (name,arch) searches, only (n,nv, nva).
	    set data [$self FilterArch $data $afilter]
	}
	if {!$keepall} {
	    # Drop instances for unsupported architectures.
	    set data [$self FilterPlatform $data]
	}

	# data         = dict (repo -> list(ext-instance))
	# ext-instance = {name ver arch isprofile}

	set tmp {}
	foreach {r plist} $data {
	    if {![llength $plist]} {
		$self DropArchive $r "$pkglabel is not known"
		continue
	    }
	    lappend tmp $r
	}

	if {![llength $tmp]} {
	    $self Puts "$pkglabel is not known"
	    $self ReportInactives
	    return
	}

	log::debug "Found in <$tmp>"

	# We now have a reduced set of archives which know the
	# package. We ask only them for more information, and ignore
	# all the others.

	# We also use the generated instance information to target the
	# queries, this also changes the output from a merge of all
	# found instances to one section printed per instance found,
	# making the differentiation of free and BE instances easier
	# for the user. (The old merged output looked as if the whole
	# package was BE instead of just some of the platforms.

	# data' = dict (ext-instance -> list(repo))

	foreach {ei _} [Invert $data] {
	    ::teapot::instance::norm ei
	    set ref      [::teapot::instance::2spec $ei]
	    set pkglabel [teapot::listspec::print2 $ref]

	    if {$getall} {
		set data [$self RunSomeArchives ro $tmp meta $ref]
		# data = dict (repo -> list (dict (key -> value)))

		# Consider merging identical package information (just
		# with different origins) into a single paragraph of
		# output.

		foreach {r meta} $data {
		    $self Puts "Entity   $pkglabel"
		    $self Puts "Origin @ [$r cget -location]"
		    $self Puts [join [lrange \
					  [teapot::metadata::write::HumanLines \
					       _dummy_name_ 0 package $meta] \
					  2 end] \n]
		}
	    } else {
		set data [$self RunSomeArchives ro $tmp value description $ref]
		# data = dict (repo -> string), string = description

		$self Puts "Entity        $pkglabel"
		$self Puts ""
		foreach {r desc} $data {
		    $self Puts "  Description @ [$r cget -location]"

		    set desc [string map {--- \n\n} [join $desc]]
		    foreach para [textutil::splitx $desc "\n\n"] {
			$self Puts [indentpara "        " 64 $para]
			$self Puts ""
		    }
		}
	    }
	}

	$self ReportInactives
	return
    }

    method list {useinstall asformat listspec only all} {
	set data \
	    [$self RunX ro $useinstall list $listspec]

	array set L [Invert $data]

	if {![array size L]
	    && ("all"  ne [::teapot::listspec::type $listspec])
	    && ("eall" ne [::teapot::listspec::type $listspec])
	} {
	    # The 'list' did not find anything, and was not asked to
	    # look for everything. That means that very likely the
	    # specified name isn't matching. So we first do a
	    # case-insensitive substring search for this name, and
	    # then re-do the list searches using the names we found,
	    # if any.

	    set data [$self FuzzList $listspec $useinstall]
	}
	unset L

	# data         = dict (repo -> list(ext-instance))
	# ext-instance = {type name ver arch isprofile}

	if {$only ne "all"} {
	    # Drop instances for unsupported architectures, profiles.
	    # May drop instances which are installed.
	    set data [$self FilterList $only $data]
	}
	if {!$all} {
	    # Drop instances for unsupported architectures.
	    set data [$self FilterPlatform $data]
	}

	# Drop all redirection instances shadowed by actual instances.
	set data [$self FilterRedirections $data]

	# Retrieve associated as::note's, if any, for the display.
	set data [$self Annotate $useinstall $data]
	# data               = dict (repo -> list(annotated-instance))
	# annotated-instance = {type name ver arch isprofile note}

	$self Dump $data $asformat
	$self ReportInactives
	return
    }

    method FuzzList {listspec useinstall} {
	::teapot::listspec::split $listspec _ n _ _

	array set L [Invert [$self RunX ro $useinstall \
				 search [list key name rex "(?i)$n"]]]

	if {![array size L]} {return {}}

	set data {}
	array set has {}
	foreach k [lsort -dict -index 1 [array names L]] {
	    if {[info exists has($k)]} continue
	    set has($k) .

	    ::teapot::instance::split $k _ n _ _
	    ::teapot::listspec::changeName listspec $n

	    foreach x [$self RunX ro $useinstall list $listspec] {
		lappend data $x
	    }
	}

	# One problem with data is that it should have all information
	# for a repository together, and due to the possible use of
	# multiple names this is not true. Double-inversion fixes
	# that. The first inversion merges everything by name, the
	# re-inversion then returns everything merged by repository.

	return [Invert [Invert $data]]
    }

    method FilterRedirections {data} {
	# data         = dict (repo -> list(ext-instance))

	array set L [Invert $data]

	foreach ei [array names L] {
	    teapot::instance::split $ei e n v a
	    # Ignore regular entries and redirections which have no
	    # shadows (== only 1 instance in L).
	    if {
		($e ne "redirect") ||
		([llength [array names L [list * $n $v $a *]]] == 1)
	    } continue
	    # Drop shadowed redirection.
	    unset L($ei)
	}

	return [Invert [array get L]]
    }

    method FilterArch {data legal} {
	# Perform additional filtering based on the local
	# platform. Only packages acceptable to the local platform are
	# shown. IOW we ignore everything which cannot be installed
	# because of architecture mismatches

	# data         = dict (repo -> list(ext-instance))

	array set L [Invert $data]

	# L = dict (ext-instance -> list(repo))
	foreach ei [array names L] {
	    teapot::instance::split $ei _ _ _ p
	    if {[struct::set contains $legal $p]} continue
	    unset L($ei)
	}

	return [Invert [array get L]]
    }

    method FilterPlatform {data} {
	# Perform additional filtering based on the local
	# platform. Only packages acceptable to the local platform are
	# shown. IOW we ignore everything which cannot be installed
	# because of architecture mismatches

	set legal [platform::patterns [$self Arch {}]]
	return [$self FilterArch $data $legal]
    }

    method FilterList {only data} {

	# Perform additional filtering based on the contents of the
	# local repository.

	array set L [Invert $data]

	# L = dict (ext-instance -> list(repo))

	# When restrictions apply we ignore everything which cannot be
	# installed, be it because of architecture mismatches, or be
	# it because they are special, like profiles.

	# I. Remove unacceptable architectures

	set legal [platform::patterns [$self Arch {}]]
	foreach ei [array names L] {
	    teapot::instance::split $ei _ _ _ p
	    if {[struct::set contains $legal $p]} continue
	    unset L($ei)
	}

	# II. Remove profiles

	foreach k [array names L [list * * * * 1]] {
	    unset L($k)
	}

	switch -exact -- $only {
	    uninstalled {
		# Determine all installed instances, and allowed
		# platforms, and remove everything from the list which
		# we cannot install and which is already installed.
		# Note: Profiles belong in the category of cannot be
		# installed (even if they are valid arguments to
		# 'install' only their requirements are installed, not
		# themselves)

		# III. Remove installed packages

		set _local [$self LocalPackages]

		# Bug 73962. Determine which versions of Tcl are available.
		foreach {p v} $_local { if {$p ne "Tcl"} continue ; lappend _tcls $v }

		array set local $_local
		foreach n [array names local] {
		    #                              t n    v a profile
		    foreach k [array names L [list * ${n} * * *]] {
			unset L($k)
		    }
		}

		# IV. (Bug 74786) Of the remaining candidates remove
		# everything which is not supported by the shells
		# connected to the destination repository, then remove
		# all instances for which a candidate with a higher
		# version is present, and at last remove everything
		# for which we have a candidate with a better matching
		# architecture.

		# Sort by architecture, version, name. Better
		# architecture first, with exact check later.  Better
		# version first. This causes them to suppress the
		# lesser possibilities coming later.

		array set mark {}
		foreach ci [lsort -decreasing -dict -index 1 \
				 [lsort -decreasing -dict -index 2 \
				      [lsort -decreasing -dict -index 3 \
					   [array names L]]]] {

		    # Get the mininmal version of Tcl required by this
		    # instance and check it against the versions of
		    # Tcl provided by the shells connected to the
		    # destination installation repository (_tcls).
		    # Ignore all instances for which we have no good
		    # Tcl.

		    set ok 0
		    foreach r $L($ci) {
			set i $ci
			teapot::instance::norm i
			if {[MTVok $r $i]} {
			    set ok 1 ; break
			}
		    }
		    if {!$ok} {
			# Not supported by connected shell, squash.
			unset L($ci)
			continue
		    }

		    # Now the filtering based on version and
		    # architecture can begin.

		    teapot::instance::split $ci _ n v _
		    if {![info exists mark($n)]} {
			# Remember first possibility, likely the best
			# candidate.
			set mark($n) [list $ci $v]
			continue
		    }

		    if {[package vcompare $v [lindex $mark($n) 1]] > 0} {
			# Found a better version in a later possibility.
			# (Because lsort -dict is not quite right,
			# only mostly). Squash the old, and remember
			# the new instead of squashing it.
			unset L([lindex $mark($n) 0])
			set mark($n) [list $ci $v]
			continue
		    }

		    # Lesser possibility, squash.
		    unset L($ci)
		}

		# Done.
	    }
	    newer {
		# Determine all installed packages, remove everything
		# from the external list which is not in there. Also
		# remove profiles and everything else which cannot be
		# installed, due to mismatching architectures. Then go
		# over the remains and remove everything whose version
		# number is less or equal to the versions we have.

		# III. Remove uninstalled packages, and with a lesser
		# or equal version number. We propagate better
		# versions into the local array to ensure that only
		# the highest version number is listed.

		array set local [$self LocalPackages]
		foreach lei [lsort -decreasing -dict -index 1 \
				 [lsort -decreasing -dict -index 2 \
				      [array names L]]] {
		    teapot::instance::split $lei _ n v _
		    if {
			![info exists local($n)] ||
			([package vcompare $v $local($n)] <= 0)
		    } {
			unset L($lei)
			continue
		    }
		    set local($n) $v
		}
	    }
	}

	return [Invert [array get L]]
    }

    # result = dict (name -> version)
    method LocalPackages {} {
	set install [$self Installation ro]
	set shells  [$install architecture shells [$self Arch {}]]

	# Packages seen by the shell, added to _installed.
	InitializeWhatIsInstalledAlready

	# _installed = dict (name -> version)
	# Sorted by version, higher is later.

	# Add the packages from the local repository.
	foreach lei [$self RunInstall ro list [::teapot::listspec::all]] {
	    teapot::instance::split $lei t n v p
	    lappend _installed $n $v
	}

	# Resort and merge to keep sorted by version, higher is later
	return [VSort $_installed]
    }

    method get {useinstall output listspec} {
	set instances \
	    [$self RunX ro $useinstall list $listspec]

	# Drop all redirection instances shadowed by actual instances.
	set instances [$self FilterRedirections $instances]

	# instances = dict(repo -> list(instance))

	set instances [Invert $instances]

	# instances = dict(instance -> list(repo))

	set instances [$self Retrieve 0 1 $instances 0 1]

	# instances = dict (instance -> {list delete link})
	# instance  = {name ver arch}

	puts "\nOutput directory $output"

	set npkgs [expr {[llength $instances]/2}]

	if {$npkgs == 1} {
	    puts "1 package found, copying retrieved file ..."
	} elseif {$npkgs == 0} {
	    puts "0 packages found, no copying"
	} else {
	    puts "$npkgs packages found, copying retrieved files ..."
	}
	puts ""

	foreach {i spec} $instances {
	    foreach {f del link} $spec break
	    # Assert (!link)

	    # Copy or move the files to the output directory,
	    # constructing file names on the fly.

	    teapot::instance::split $i t n v p
	    set archive [pfilename $t $n $v $p [artype $f $p]]
	    set dst     [file join $output $archive]

	    puts "Copying $archive"

	    if {$del} {
		file rename -force $f $dst
	    } else {
		file copy   -force $f $dst
	    }
	}
	if {[llength $instances] > 1} {
	    puts ""
	}

	$self ReportInactives
	return
    }


    method search {useinstall asformat query} {
	#puts ($query)

	set data \
	    [$self RunX ro $useinstall search $query]

	# data         = dict (repo -> list(ext-instance))
	# ext-instance = {name ver arch isprofile}

	# Drop all redirection instances shadowed by actual instances.
	set data [$self FilterRedirections $data]

	set data [$self Annotate $useinstall $data]
	# data               = dict (repo -> list(annotated-instance))
	# annotated-instance = {type name ver arch isprofile note}

	$self Dump $data $asformat
	$self ReportInactives
	return
    }

    method Dump {data asformat {prefix {}}} {
	# data = dict (repo -> list(ext-instance))
	switch -exact -- $asformat {
	    profile {
		$self GenProfile $data
	    }
	    table {
		$self GenListing [Invert $data] $prefix
	    }
	    csv {
		$self GenCSV [Invert $data]
	    }
	}
	return
    }

    # data         = dict (repo -> list(ext-instance))
    # ext-instance = {name ver arch isprofile}
    method Annotate {useinstall data} {
	set res {}
	foreach {r instances} $data {
	    set ainstances {}
	    foreach ei $instances {
		set i $ei
		teapot::instance::norm i
		set note [lindex \
			      [$self RunX ro $useinstall \
				   value as::note \
				   [::teapot::instance::2spec $i]] 1]

		lappend ainstances [linsert $ei end $note]
	    }
	    lappend res $r $ainstances
	}

	return $res
    }

    method keys {useinstall} {
	# run:   dict(repo -> list(key))
	# inv:   dict(key -> list(repo)
	# names: list(key)

	foreach key [lsort -dict \
			 [Names [Invert \
				     [$self RunX ro $useinstall keys]]]] {
	    $self Puts  " $key"
	}

	$self ReportInactives
	return
    }


    method onAbort {cmd} {
	set onabort $cmd
	return
    }

    method onStop {cmd} {
	set onstop $cmd
	return
    }

    # ### ### ### ######### ######### #########
    ## Internals - General helpers

    method abortp {text} {
	set paragraphs [textutil::splitx $text "\n\n"]
	set res {}
	foreach para $paragraphs {
	    lappend res [indentpara \t 64 $para]
	}
	$self abort [join $res \n\n]
	return -code error "Abort does not exit!"
    }

    method abort {text} {
	uplevel \#0 [linsert $onabort end $text]
	return -code error "Abort handler does not exit!"
    }

    method stop {text} {
	uplevel \#0 [linsert $onstop end $text]
	return -code error "Stop handler does not exit!"
    }

    method Note {msg} {
	if {$quiet} return
	$self Puts $msg
	return
    }

    method Puts {args} {
	set cmd $log
	foreach a $args {lappend cmd $a}
	uplevel \#0 $cmd
	return
    }

    proc bullet {bullet para} {
	set blank  [string repeat " " [string length $bullet]]
	return ${bullet}[textutil::indent $para $blank 1]
    }

    proc indentpara {prefix len para} {
	return [textutil::indent \
		    [textutil::adjust $para -length $len] \
		    $prefix 0]
    }

    proc bulletpara {bullet len text} {
	bullet $bullet [textutil::adjust $text -length $len]
    }

    proc pfilename {entity name version architecture artype} {
	set f ${entity}-[string map {% %25 : %3a} $name]-${version}-${architecture}
	if {$artype != {}} {append f . $artype}
	return $f
    }

    # Bugzilla 67330
    proc artype {f arch} {
	# Actually file extension for the archive file.
	set types [fileutil::fileType $f]

	if {[lsearch -exact $types zip] >= 0} {
	    # Zip archive, easy
	    return zip
	}

	if {[lsearch -exact $types metakit] < 0} {
	    # No metakit attached, should be plain Tcl Module.
	    return tm
	}

	# Attached metakit, can be starkit or starpack.

	if {[lsearch -exact $types script] >= 0} {
	    # Headed by a script => starkit.
	    return kit
	}

	# Starpack. Extension is platform dependent. Not the local
	# platform of the client however, the platform of the archive
	# itself is relevant.

	if {[string match win32-* $arch]} {
	    # Windows => exe
	    return exe
	}

	# Unixoid => No extension for executable.
	return {}
    }

    # ### ### ### ######### ######### #########
    ## Internals - Talking to the configuration.

    # Cached and other information.

    # - Explicit installation to use.
    #   Also cache for default installation
    #   if there is no override from the
    #   command line.

    variable theinstall {}
    variable themode    {}
    variable quiet      1

    # Array of (still-)active repositories to look at.

    variable archives -array {}

    # Array of repositories with problems, and what.

    variable inactive -array {}
    variable vr {}


    method Installation {mode} {
	# Determine the installation repository we are accessing. An
	# overide by the user (or cache) has precedence, otherwise
	# query the configuration for the default.

	# mode : String flag, legal values {ro, rw}.

	if {$theinstall ne ""} {
	    if {$mode eq $themode} {
		# Requesting mode identical to what is checked for. We
		# can use the cache.

		return $theinstall
	    }

	    if {($mode eq "ro") && ($themode eq "rw")} {
		# The requested mode is less than we already have, so
		# we can use the cache.

		return $theinstall
	    }

	    # The repository is validated for read-only, now rw is
	    # requested. We have to revalidate for the extended
	    # access.

	    if {![$config default valid $mode msg]} {
		$self abort "Bad installation: $msg"
	    }

	    set themode $mode
	    return $theinstall
	}

	# The cache is not valid, check according to the requested
	# mode, then create an accessor object and cache it.

	if {![$config default valid $mode msg]} {
	    $self abort "Bad installation: $msg"
	}

	set themode    $mode
	set theinstall [::repository::localma ${selfns}::INSTALL \
			  -location [$config default get]]
	return $theinstall
    }

    proc ArchiveOpen {location mode config} {
	upvar 1 self self
	log::debug "ArchiveOpen $mode <$location>"

	set pristine $location

	if {[regexp {^file://} $location]} {
	    regsub {^file://} $location {} location
	}

	set t [repository::api typeof $location]

	log::debug "  is-a $t @ $pristine"

	if {![$t valid $location $mode message]} {
	    return -code error $message
	}

	log::debug "  Construct"

	if {$t eq "::repository::proxy"} {
	    return [$t %AUTO% -location $location \
			-config $config \
			-notecmd [list $self Note] \
			-timeout [expr {1000* [$config timeout get]}] \
		       ]
	} elseif {$mode eq "ro"} {
	    return [$t %AUTO% -location $location -readonly 1]
	} else {
	    return [$t %AUTO% -location $location]
	}
    }

    proc ArchiveType {location mode lv} {
	upvar 1 $lv actual

	set pristine $location
	if {[regexp {^(http):} $location]} {
	    set actual $location
	    return ::repository::proxy
	}

	if {[regexp {^file://(.*)$} $location -> path]} {
	    set location $path
	}

	if {[file exists $location]} {
	    foreach t {
		::repository::sqlitedir
		::repository::localma
	    } {
		if {[$t valid $location $mode message]} {
		    set actual $location
		    return $t
		}
	    }
	}

	return -code error \
	    "Unable to determine type of archive \"$pristine\""
    }

    method Archives {mode} {
	log::debug "Archives ($mode) /vs ($vr)"

	if {$vr eq ""} {
	    set thearchives [$config archives]
	    if {![llength $thearchives]} {
		$self abort "Bad configuration\nNo archives known"
	    } else {
		log::debug \t<$thearchives>

		foreach r $thearchives {
		    if {[catch {
			set o [ArchiveOpen $r $mode $config]
		    } msg]} {
			log::warn $msg
			set inactive($r) $msg
		    } else {
			set archives($o) .
		    }
		}
		if {![array size archives]} {
		    $self abort "Bad configuration\n[$self InactiveMessages]"
		}
	    }
	    set vr $mode
	}

	# Revalidate if requested mode is rw, vs. currently used ro.

	if {($vr eq "ro") && ($mode eq "rw")} {
	    foreach r [array names archives] {
		if {![$r valid $mode message]} {
		    unset archives($r)
		    set   inactive([$r cget -location]) $message
		}
	    }
	}

	return [array names archives]
    }

    method InactiveMessages {} {
	set lines {}
	set dropped [lsort -dict [array names inactive]]
	set max 0
	foreach r $dropped {maxstr max $r}
	foreach r $dropped {
	    set res \n
	    foreach para [textutil::splitx \
			      [string map \
				   {--- \n\n} \
				   $inactive($r)] \
			      "\n\n"] {
		append res [indentpara "        " 64 $para]\n\n
	    }
	    set res [string range $res 0 end-1];# chop last \n
	    lappend lines "* [lj $max $r] : $res"
	}
	return [join $lines \n]
    }

    method ReportInactives {} {

	foreach r [$self Archives ro] {
	    #puts $r\t[$r cget -location]\t[$r info type]
	    if {[$r info type] ne "::repository::proxy"} continue
	    set msg [$r cachestatus]
	    if {$msg eq ""} continue

	    if {[string match *timeout* $msg]} {
		global appname

		set t [$config timeout get]
		if {$t <= 0} {
		    set t "infinity (disabled)"
		} elseif {$t == 1} {
		    set t "1 second"
		} else {
		    set t "$t seconds"
		}

		append msg "\n\tPlease use the '$appname timeout' command to adjust\
                            or\n\tdisable the transfer timeout. It is currently set to $t.\n"
	    }

	    $self DropArchive $r $msg
	}

	if {![array size inactive]} return
	$self Puts ""
	$self Puts "Problems which occurred during the operation:"
	$self Puts [$self InactiveMessages]
	return
    }

    method DropArchive {r msg} {
	catch {unset archives($r)}
	set   inactive([$r cget -location]) $msg
	return
    }

    # ### ### ### ######### ######### #########
    ## Internals - Accessing repositories

    method RunX {mode useinstall cmd args} {
	log::debug "RunX mode=$mode $cmd ($args)"

	if {$useinstall} {
	    return [list \
			[$self Installation $mode] \
			[eval [linsert $args 0 $self RunInstall $mode $cmd]] \
		       ]
	} else {
	    return [eval [linsert $args 0 $self RunArchives $mode $cmd]]
	}
    }

    method RunInstall {mode cmd args} {
	$self Run $mode [$self Installation $mode] $cmd $args
    }

    method RunArchives {mode cmd args} {
	set res {}
	foreach r [$self Archives $mode] {
	    if {[catch {
		lappend res $r [$self Run $mode $r $cmd $args]
	    } msg]} {
		#puts $::errorInfo

		$self DropArchive $r $msg
	    }
	}
	return $res
    }

    method RunSomeArchives {mode rlist cmd args} {
	set res {}
	foreach r $rlist {
	    if {[catch {
		lappend res $r [$self Run $mode $r $cmd $args]
	    } msg]} {
		$self DropArchive $r $msg
	    }
	}
	return $res
    }

    method Run {mode repo cmd alist} {
	#puts |[$repo cget -location]|$mode|$cmd|([join $alist ") ("])

	# For the local installation we do not come here, because
	# 'RunInstall' checks earlier, and aborts. For archives errors
	# are caught, and validation happens earlier as well.

	if {![$repo valid $mode message]} {
	    return -code error $message
	}

	uplevel #0 [linsert $alist 0 $repo sync $cmd]
    }

    method Done {code result} {
	set runResult [list $code $result]
	return
    }



    # ### ### ### ######### ######### #########
    ## Internals - API Helpers

    method Retrieve {dry force instances noprofiles dontlink} {
	# instances    = dict (ext-instance -> list(repo))
	# ext-instance = {entity name ver arch isprofile}

	log::debug "Retrieving[expr {$dry ? " dry" : ""}][expr {$force ? " forced" : ""}] instances: [llength $instances]"

	if {![llength $instances]} return
	$self Puts ""

	# tmp = dict (instance -> {path delete link repo})

	set tmp {}
	set ok 1

	set maxt 0
	set maxn 0
	set maxv 0
	set maxp 0
	foreach {i _} $instances {
	    teapot::instance::split $i t n v p
	    maxstr maxt $t
	    maxstr maxn $n
	    maxstr maxv $v
	    maxstr maxp $p
	}

	set timeouts 0
	foreach {i repolist} $instances {
	    foreach {t n v p isprofile} $i break

	    set intro "Retrieving [lj $maxt $t] [lj $maxn $n] [lj $maxv $v] [lj $maxp $p] ..."
	    set blank [blankstr $intro]

	    $self Puts -nonewline $intro

	    if {$noprofiles && $isprofile} {
		# Checked the meta data of the retrieved file for the
		# 'Profile' keyword. If present the package is a
		# profile. Such a package is never installed.  It is
		# only used to specify a set of dependencies under a
		# single name.
		#
		# Therefore we do not even attempt to retrieve the
		# associated file.
		#
		# When retrieving files only however we do want the
		# file in the list, hence the possible override via
		# 'noprofiles' by the caller.

		$self Puts " ... Profile, will be ignored"
		continue
	    }

	    # Remove the isprofile flag, make this a regular instance.
	    teapot::instance::norm i
	    set pfx   ""
	    set gotit 0

	    foreach repo $repolist {
		$self Puts -nonewline $pfx
		$self Puts -nonewline "@ [$repo cget -location]"
		set pfx $blank

		#$self Puts "\t\t [$repo info type]"

		# We do special handling of archives accessed through
		# the local filesystem. For opaque archives we can use
		# the stored file and install it, and for transparent
		# archives we can do direct linking to the package in
		# the archive. At leats if the paths returned are in
		# the native fs, and not virtual. In case of the
		# latter we fall back to the regular way of doing
		# things (temp. file, and regular install).

		# More special handling ... If the instance is a
		# redirect we bypass the special handling for a local
		# filesystem and treat it as basic network retrieval,
		# with associated temporary file. This is then
		# followed by the handling of the redirection (reading
		# the metadata from the temp file), and recursively
		# retrieving the instance we were directed to.

		if {$t ne "redirect"} {
		    if {
			!$dontlink &&
			([$repo info type] eq "::repository::localma")
		    } {
			# Get a path instead of a file. If the path is
			# native remember it as the the retrieved
			# location, and record it as externally owned, and
			# sym-linkable.

			if {[catch {
			    set f [$self Run ro $repo path [list $i]]
			    set m [$self Run ro $repo meta [list [teapot::instance::2spec $i]]]
			    set a [$repo artype $f]
			} msg]} {
			    $self Puts " ... Error: [err $msg]"
			    continue
			} else {
			    if {[lindex [file system $f] 0] eq "native"} {

				$self Puts " ... Ok (Local path, Linkable)"

				# Symlink install: external path, linkable.
				lappend tmp $i [list $f 0 1 $m $a $repo]
				set gotit 1
				break
			    }
			    # Falling through to regular retrieval & install.
			}

		    } elseif {[$repo info type] eq "::repository::sqlitedir"} {
			# Get a path instead of a file. Record it as
			# externally owned. We cannot link to it however.
			# A virtual file is useable however, there is no
			# no need to copy it out.

			if {[catch {
			    set f [$self Run ro $repo path [list $i]]
			} msg]} {
			    $self Puts " ... Error: [err $msg]"
			    continue
			} else {
			    $self Puts " ... Ok (Local path)"

			    # No-copy install: external path, however not
			    # linkable, as repository is opaque.

			    lappend tmp $i [list $f 0 0 $repo]
			    set gotit 1
			    break
			}
		    }
		}

		# Network archive, or local transparent archive in a
		# virtual directory, or redirection. We have to use
		# the regular method of installation: Copy over into
		# temp. file, no linking.

		set f [fileutil::tempfile tpm]

		if {[catch {
		    $self Run ro $repo get [list $i $f]
		} msg]} {
		    $self Puts " ... Error: [err $msg]"
		} else {
		    $self Puts " ... Ok"

		    if {$t eq "redirect"} {
			# Special handling of redirections. Pull the
			# meta data from the just-retrieved file and
			# determine the instance we are directed to,
			# and the repositories to talk to. Then
			# recursively retrieve the actual instance.

			$self Puts -nonewline "\n    Redirection"

			# Get the meta data ..
			set errors {}
			set p [lindex [::teapot::metadata::read::file $f single errors] 0]
			file delete $f

			if {$p eq {}} {
			    $self Puts " ... Bad meta data ($errors), ignored"
			    break
			}

			# Determine origin instance, and where to find it.
			set origin [::teapot::redirect::2instance $p]
			set orepos [$p getfor as::repository]
			$p destroy

			if {![llength $orepos]} {
			    $self Puts " ... No repositories specified, ignored"
			    break
			}

			# Get the necessary repository objects
			set ortmp {}
			foreach r $orepos {
			    if {[catch {
				lappend ortmp [ArchiveOpen $r ro $config]
			    } msg]} {
				$self Puts " ... Bad repository $r"
			    }
			}

			if {![llength $ortmp]} {
			    $self Puts " ... No usable repositories, ignored"
			    break
			}

			# Now retrieve the actual instance ...
			$self Puts " to ($origin) @ $orepos"

			lappend origin 0 ;# Fake isprofile flag, not a profile.
			set oinstances [$self Retrieve $dry $force \
					    [list $origin $ortmp] \
					    $noprofiles $dontlink]

			lappend tmp {*}$oinstances
			set gotit 1

			# Cleanup and automatic extension of the set of
			# configured archives ...

			foreach {i def} $oinstances {
			    lassign $def _ _ _ rx
			    set rlocation [$rx cget -location]

			    if {[$config archive has $rlocation]} continue
			    $config archive add $rlocation

			    $self Puts "\n    Added the redirection destination $rlocation"
			    $self Puts "    to the set of configured archives."
			}

			foreach r $ortmp {
			    $r destroy
			}

		    } else {
			if {$dry} {
			    file delete -force $f
			}

			# Standard install: delete temp. file, no linking.
			lappend tmp $i [list $f 1 0 $repo]
			set gotit 1
		    }
		    break
		}
	    }
	    if {!$gotit} {set ok 0}
	}

	if {$timeouts} {
	    $self Puts ""
	    $self Puts "$timeouts Timeout[expr {$timeouts==1?"":"s"}] occured during retrieval."
	    $self Puts "    Use the option --timeout or the 'timeout' command to either"
	    $self Puts "    extend the timeout period, or disable it altogether."
	    $self Puts ""
	}

	if {!$ok && !$force} {
	    if {!$dry} {
		foreach {i spec} $tmp {
		    foreach {f del link} $spec break
		    if {$del} {
			file delete -force $f
		    }
		}
	    }

	    $self abortp "Unable to install the full list. Aborting."
	}

	# tmp = dict (instance -> {path delete link})
	return $tmp
    }

    proc err {msg} {
	upvar 1 config config timeouts timeouts self self
	if {[string match *timeout* $msg]} {
	    set t [$config timeout get]
	    append msg "($t second[expr {$t == 1 ? "" : "s"}])"
	    incr timeouts
	}
	set msg [string map {--- \n\n} $msg]
	set res \n\n
	foreach para [textutil::splitx $msg "\n\n"] {
	    append res [indentpara "        " 64 $para]\n\n
	}
	return [string range $res 0 end-1];# chop last \n
    }

    method Install {dry instances} {
	# instances = dict (instance -> filespec)
	# instance  = {name ver arch}
	# filespec  = {path delete link ?meta artype?}

	# Incoming files may or may not be owned by the application.
	# The retriever tells us using a flag in the data structure.
	# Incoming files may or may not allow sym-linking. This is again
	# recorded in a flag of the incoming data structure.

	# delete    = boolean, 1 - this command has to delete the file (temp. file)
	#                      0 - do not delete the incoming file.
	# link      = boolean, 1 - install by setting a sym-link to the path 
	#                      0 - install by copying the path (regular install).

	# meta is present for link(yes). It contains the meta data to
	# use for the package. We get it separately because the path
	# we get may be a directory (from a zip archive) and thus
	# without the data itself.

	# Ditto archive type.

	# Possible configurations:
	#
	# d l ok Meaning
	# --- -- -------
	# 0 0  1 regular install, file is owned elsewhere, do not delete
	# 0 1  1 sym-link install, must not delete file.
	# 1 0  1 regular install, file is temp. file.
	# 1 1  0 ERROR, sym-link install of file to be deleted is wrong
	# --- -- -------
	# linkable => !delete   && meta-is-present
	# delete   => !linkable

	# dry(on) implies that all files with delete(on) are already
	# deleted and gone.


	if {![llength $instances]} return

	$self Puts ""
	$self Puts "Installing into [[$self Installation rw] cget -location]"
	$self Puts ""

	set maxt 0
	set maxn 0
	set maxv 0
	set maxp 0
	foreach {i _} $instances {
	    teapot::instance::split $i t n v p
	    maxstr maxt $t
	    maxstr maxn $n
	    maxstr maxv $v
	    maxstr maxp $p
	}

	foreach {i spec} $instances {
	    teapot::instance::split $i t n v p
	    foreach {f del link} $spec break

	    if {$link} {
		$self Puts "Installing [lj $maxt $t] [lj $maxn $n] [lj $maxv $v] [lj $maxp $p] \tSetting a symbolic link"
	    } else {
		$self Puts "Installing [lj $maxt $t] [lj $maxn $n] [lj $maxv $v] $p"
	    }

	    if {$dry} continue

	    #$self Puts \t$f\tdel($del)\tlink($link)

	    if {$link} {
		set meta   [lindex $spec 3]
		set artype [lindex $spec 4]
		$self RunInstall rw link $f $i $meta $artype
	    } else {
		$self RunInstall rw put $f
	    }
	    if {$del} {
		file delete -force $f
	    }
	}
	return
    }

    method ResolveEntity {arch force recommends eref} {
	return [$self DoResolve \
		    [$self Arch $arch] \
		    $force $recommends [list $eref]]

	# Result       = dict (ext-instance -> list (repo))
	# ext-instance = {entity name ver arch isprofile}
    }

    method ResolveFile {arch force recommends path ipv iv av} {
	upvar 1 $ipv isprofile $iv instance $av already

	# Get an initial list of package references from the specified
	# file. We also determine if the file is a profile or not, as
	# that influences the system after retrieval.

	set errors {}
	set packages [::teapot::metadata::read::file $path single errors]
	if {![llength $packages]} {
	    # We have run into meta-data errors. Show them, and abort.
	    $self abortp "Unable to install. [join $errors \n]"
	}

	set pkg [lindex $packages 0]

	# cet = context entity type
	set instance [$pkg instance]
	teapot::instance::split $instance cet n v p
	array set meta [$pkg get]
	$pkg destroy

	set install [$self Installation rw]
	if {![$install architecture valid $p]} {
	    $self abortp "Unable to install package \"$n $v\" for architecture \"$p\".\
                          The repository at \"[$install cget -location]\"\
                          accepts only packages for the architectures \
                          [linsert [join [lsort -dict [$install architecture patterns]] ", "] end-1 and]."
	}

	set ref [teapot::reference::cons $n -version $v -exact 1 -is $cet]
	set already 0
	log::debug "File contains ($ref)"

# Bugzilla 105442. Do not prevent installation when already in repo, or similar in repo.
if 0 {
	# 0 = !usearchives => !use _tcls, no need to define it.
	set findstatus [$self Find \
			    [platform::patterns $p] \
			    $ref repo repotype einstance 0]
	if {$findstatus} {
	    # Note: Ambiguity is no problem here.
	    if {$repotype eq "shell"} {
		$self Puts " ... $instance ... Installed outside repository, probing dependencies"
	    } else {
		$self Puts " ... $instance ... Already installed in repository, probing dependencies"
	    }
	    set already 1
	}
    }
	set isprofile  [info exists meta(profile)]
	set references {}
	# recommends :: 0 <=> follow required,
	#               1 <=> follow required+recommended
	#               2 <=> follow none
	if {$recommends < 2} {
	    if {[info exists meta(require)]} {
		foreach e $meta(require) {
		    teapot::reference::completetype e package ;#$cet
		    lappend references $e
		    log::debug "+Reference ($e)"
		}
	    }
	    if {$recommends && [info exists meta(recommend)]} {
		foreach e $meta(recommend) {
		    teapot::reference::completetype e package ;#$cet
		    lappend references $e
		    log::debug "+Recommend ($e)"
		}
	    }
	}

	return $references
	# Result       = list (reference...)
    }

    method Arch {arch} {
	set install [$self Installation ro]

	if {$arch eq ""} {
	    # Determine the architecture of the host we are on, using
	    # the shells recorded in the chosen installation.

	    set arch [$install architecture default]
	    if {$arch eq ""} {
		# NOTE: '::appname' is (expected to be) set by the
		# NOTE: application using this package.
		set rloc [$install cget -location]
		set    note "Unable to determine a default architecture for "
		append note "the repository at \"$rloc\"."

		set ra [$install architecture list]
		if {![llength $ra]} {
		    append note " This is because your repository has no shells"
		    append note " at all connected to it."
		} else {
		    if {[llength $ra] == 1} {
			set x " '$ra'"
			set y "which does not match"
			set z "a shell"
		    } else {
			set x "s [linsert '[join $ra "', '"]' end-1 and]"
			set y "none of which matches"
			set z "shells"
		    }
		    append note " This is because your repository has only $z"
		    append note " for the architecture$x connected to it, $y"
		    append note " the architecture of the host."
		}

		append note " This situation can however be easily rectified "
		append note "by running the command\n\n"
		append note "$::appname link make \"$rloc\" /path/to/your/tclsh"
		append note "\n\n"

		$self abortp $note
	    }
	} else {
	    # Check that the explicit architecture request is
	    # supported by the chosen installation.

	    if {![$install architecture ok $arch]} {
		$self abortp "The requested architecture \"$arch\"\
                              is not supported by the repository at\
                              \"[$install cget -location]\"."
	    }
	}

	return $arch
    }

    method DoResolve {arch force recommends references {li {}}} {
	set install [$self Installation rw]

	log::debug "RESOLVE ($arch) force/$force recommends/$recommends"
	foreach r $references {log::debug "    REF $r"}

	# 'references' is a set of entity references. Resolve locates
	# matching entities, and then recursively locates all
	# dependencies, be they in an archive, the installation, or
	# waiting on retrieval. It returns a list of all entity
	# instances which have to be retrieved and installed. It
	# prevents retrieval and installation if some of the
	# dependencies cannot be located, except if it is forced to
	# allow that.

	# Result is a list of extended instances and repositories.
	# The extended instance has regular entity information, name,
	# version, and platform, and is then extended with a boolean
	# flag 'isprofile'. The flag is set if the instance is a
	# profile package. Such packages we do not retrieve.

	# Pools (Sets of entities) searched, in the given order:
	#
	# - Pending   - Entities waiting for retrieval
	#
	#               This resolves dependencies to entities
	#               already found through other entity
	#               dependencies. This pool is also the list
	#               of entities to retrieve and install, i.e.
	#               our result.
	#
	# - Install A - If the installation is connected to a shell
	#               then this pool is also initialized with the set
	#               of packages the shell knows about, beyond the
	#               stuff in the repository itself.
	#
	# - Install B - Installation. This resolves dependencies to
	#	        already installed entities.
	#
	#               Usually redundant. Because a connected shell
	#               (s.a.) will likely know the packages in the
	#               installation too, so they will be found in
	#               Pending.
	#
	# - Archive    - Archives. This resolves dependencies to entities
	#  	        not yet installed. The packages found there go
	#               into the pool of pending entities.

	# Iterative algorithm. Manages a list of dependencies
	# to satisfy, initialized with 'references' alone to start at.

	log::debug "    AR* $arch"
	# SkipHandledResolved using: arch self.

	set archlist [platform::patterns $arch]
	set shells   [$install architecture shells $arch]

	foreach a $archlist {log::debug "    ARC $a"}
	set pl unknown
	foreach s $shells   {
	    if {![catch {
		platform::shell::platform $s
	    } res] && ($pl eq {})} {
		set pl $res
	    }
	    log::debug "    SH  $s = $res"
	}
	# SkipHandledResolved using: arch self.

	set _pending {}
	set waiting  $references
	set at       0
	set ok       1
	set ambi     0
	set first    1
	set stoponfirst 0

	# SkipHandledResolved using: handled
	array set    handled {}
	set missing {}    ; # List of references not found during dependency resolution.
	array set user {} ; # Per reference the instances which asked for its inclusion.

	InitializeWhatIsInstalledAlready
	# Local base instances (for installUrl, assume the packages in the files as installed)
	foreach i $li {
	    teapot::instance::split $i e n v p
	    lappend _installed $n $v
	}

	# Bug 73962. Determine which versions of Tcl are available.
        set _tcls {}
	foreach {p v} $_installed { if {$p ne "Tcl"} continue ; lappend _tcls $v }

	# Now iterate through the list of entity references to deal
	# with.

	while {$at < [llength $waiting]} {
	    set here $at
	    incr      at
	    set ref [lindex $waiting $here]
	    log::debug "Processing ($ref)"

	    SkipHandledResolved $ref

	    # When installation is forced there is no need to look at
	    # the local installation and check if the package is
	    # already installed. Yet for sensible output we make the
	    # core packages Tcl and Tk exceptions to even that.

	    set n [::teapot::reference::name $ref]
	    set findstatus [$self Find $archlist $ref repo repotype einstance 1 \
				[expr {$force && ($n ne "Tcl") && ($n ne "Tk")
				       ? 2
				       : 1}]]
	    if {!$findstatus} {
		if {$first} {
		    set stoponfirst 1

		    # For the primary package/profile/... to be
		    # installed to fail indicates a possible typo. Do
		    # a fuzzy search for candidates and propose them
		    # as installation candidates instead of just
		    # erroring out.

		    # Another possibility is that we had candidate
		    # packages indeed, but which were weeded out
		    # because they require a version of the Tcl core
		    # the user doesn't have.

		    # We have to properly distinguish the cases, to
		    # provide each with a fitting message.

		    # The method 'find' puts the flag used in the
		    # condition below into our scope (upvar 1).
		    if {$mtvmismatch} {
			# Get version information out of the reference, if present, for the command
			# we recommend to the user.
			teapot::instance::split [teapot::reference::pseudoinstance $ref] _ _ v _
			if {$v ne {}} { set v " $v" }

			$self Puts " ... Runtime mismatch."
			$self Puts ""
			$self Puts "\tThis package has been found in the archives, but requires a version of Tcl"
			$self Puts "\tas its runtime which is not present in the local installation. Please use"
			$self Puts "\tthe command"
			$self Puts ""
			$self Puts "\t\tteacup describe --all $n$v"
			$self Puts ""
			$self Puts "\tto see which version of Tcl you were expected to have."
			$self Puts "\tLook for the 'Require' lines."
		    } else {
			set data [$self FuzzList [teapot::listspec::name $n] 0]

			# Drop instances for unsupported architectures.
			set data [$self FilterArch $data $archlist]

			# Drop all redirection instances shadowed by actual instances.
			set data [$self FilterRedirections $data]

			if {![llength $data]} {
			    $self Puts " ... Not found in the archives."
			    $self Puts ""
			    $self Puts "\tWhile a more fuzzy search disregarding letter case and accepting"
			    $self Puts "\tsubstrings was done, we are sorry to say that it yielded no possible"
			    $self Puts "\tcandidates for installation either."
			    $self Puts ""
			    $self Puts "\tQuestions to consider:"
			    $self Puts "\t\tHave you spelled the name correctly ?"
			    $self Puts "\t\tIncluding the proper case of characters ?"
			    $self Puts ""
			    $self Puts "\tNote that teacup's 'search' command allows you to locate packages by"
			    $self Puts "\tsubject, categories, and the like."
			} else {
			    $self Puts " ... Not found in the archives."
			    $self Puts ""
			    $self Puts "\tDo you possibly mean any of"
			    $self Puts ""

			    $self Dump $data table \t\t
			    $self Puts "\t?"
			}
		    }
		} else {
		    $self Puts " ... Not found in the archives."
		    lappend missing $ref
		}
		set ok 0
		set first 0
		continue
	    } elseif {$findstatus == 2} {
		# Ambiguity on main reference causes special message
		# later, and --force will be disabled.

		if {$first} { set ambi 1 }

		# Here einstance = list of the instances causing
		#                  the ambiguity.

		# Transform into the dictionary expected by GenListing.
		set eitmp {}
		foreach ei $einstance {
		    lappend eitmp $ei {}
		}

		$self Puts " ... Cannot resolve ambiguous reference '[teapot::reference::ref2tcl $ref]'.\n"
		$self GenListing $eitmp \t
		unset eitmp
		set ok 0
		set first 0
		continue
	    }

	    set first 0

	    # einstance = (t n v p isprofile)
	    # instance  = (t n v p)
	    set instance $einstance
	    teapot::instance::norm   instance
	    teapot::instance::split $instance cet _ _ _

	    if {$repotype eq "pending"} {
		# Found in pool "pending". Nothing more to be done.
		# We can especially assume that its dependencies have
		# already been added to waiting as well, and are maybe
		# already processed too.

		$self Puts " ... $instance ... Already handled."
		continue
	    } elseif {$repo ne $install} {
		# Found in an archive. Mark it as to retrieve.

		$self Puts " ... \[$instance @ [$repo cget -location]\]"

		lappend _pending $einstance [list $repo]
	    } else {
		# repotype in { 'install', 'shell' }

		# The entity was found in the installation, or was
		# visible to a shell connected to it, but from outside
		# of the installation. We probe its dependencies to
		# find if any of them are unsatisfied. This can happen
		# for a package installed '--force'-fully. There is no
		# need to retrieve the entity, but this can be
		# '--force'd. In that case we do not come here because
		# Find never looked at the local installation,
		# i.e. never checked if packages were already
		# installed.

		if {$repotype eq "shell"} {
		    $self Puts " ... \[$instance ... Installed outside repository, probing dependencies\]"
		} else {
		    $self Puts " ... \[$instance ... Already installed in repository, probing dependencies\]"
		}
	    }
	    
	    # Well. The found instance comes from either the
	    # installation or an archive. We retrieve its dependencies
	    # per the flags and add them to the list of references
	    # waiting for resolution.

	    if {$repotype eq "shell"} {
		if {[catch {
		    set rq [$self Requirements $repo $instance $recommends]
		}]} {
		    # Nothing ... Package likely comes from the shell,
		    # and not the installation reflected into it. We
		    # cannot trace its dependencies.
		    set rq {}
		}
	    } else {
		# Otherwise we wish to get all errors.
		set rq [$self Requirements $repo $instance $recommends]
	    }
	    foreach r $rq {
		teapot::reference::completetype r package ;#$cet

		# FUTURE: verbose - print dependencies
		#$self Puts "        * $r"
		lappend waiting $r
		log::debug " DEP ($r)"

		# Record the requestor. Should r not be found this
		# goes into the error message to allow us to poinpoint
		# where the bad dependency came from.
		lappend user($r) $instance
	    }

	    # ####################
	}

	if {!$ok && $ambi} {
	    $self abortp "\n\nAborting installation. Please add the option '-is ENTITYTYPE' to the command line to unambiguously specify which of the found entities you wish to install. You _cannot_ --force this.\n\n"
	}

	if {!$ok && !$force} {
	    if {$stoponfirst} {
		$self abortp "\n\nAborting installation, was not able to locate the requested entity.\n\n"
	    } else {
		$self Puts "\n\nAborting installation, was not able to locate all dependencies."
		$self Puts "The following references could not be found:\n"
		foreach r [lsort -dict $missing] {
		    if {[info exists user($r)]} {
			$self Puts "\t$r"
			$self Puts "\t\trequired by [join $user($r) "\n\t\trequired by"]\n"
		    } else {
			$self Puts \t$r
		    }
		}

		$self abortp "\n\n"
	    }
	}

	# result = dict (ext-instance -> list(repo))
	return [DictSort $_pending]
    }

    proc GetPackages {sh err} {
	upvar 1 inactive inactive
	if {[catch {
	    set p [exec $sh << {
		set     packages {}
		if {[string match {*[ab]*} [info patchlevel]]} {
		    # Unstable Tcl, use plain version.  Patchlevel
		    # will cause trouble later, as it is currently not
		    # an acceptable version number, syntax-wise.
		    # This hack can go when TIP 268 is effect.
		    lappend packages Tcl [info tclversion]
		} else {
		    lappend packages Tcl [info patchlevel]
		}
		catch {package require this/is/a/bogus/package/name}
		foreach p [package names] {
		    if {$p == "Tcl"} continue
		    foreach v [package versions $p] {
			lappend packages $p $v
		    }
		}
		puts [list TEACUP-QUERY $packages]
		exit 0
	    } 2> $err]
	} msg]} {
	    # The shell failed in some way. Recover, report later.
	    # Incorporate contents of file `err` in the report.
	    set err [fileutil::cat $err]
	    set inactive($sh) $msg\n$err[indentpara "  " 70 "To permanently get rid of this message please use $::appname's 'link cut' command to sever the connection between the repository and this missing shell."]
	    return {}
	}
	# Extract the query result from the tclsh output. We are
	# looking for the TEAPOT-QUERY marker to ensure that noise
	# generated by puts in a .tclshrc is ignored.
	foreach line [split $p \n] {
	    if {[string match {TEACUP-QUERY *} $line]} {
		set p [lindex $line 1]
		break
	    }
	}
	return $p
    }

    proc InitializeWhatIsInstalledAlready {} {
	upvar _installed _installed shells shells inactive inactive

	set err [fileutil::tempfile tpm]
	set tmp {}
	foreach sh $shells {
	    set psh [GetPackages $sh $err]
	    foreach {n v} $psh {
		lappend tmp $n $v
		log::debug "    SH has $n $v"
	    }
	}
	file delete $err
	set _installed [VSort $tmp]
	return
    }

    proc VSort {dict} {
	# dict = name version, versions can be listed in any order.
	# We resort so that larger versions of a package come last.

	set tmp {}
	foreach {n v} $dict { lappend tmp [list $n $v] }
	set res {}
        foreach item [lsort \
			  -command {package vcompare} \
			  -index 1 \
			  [lsort -unique $tmp]] {
	    foreach {n v} $item break
	    lappend res $n $v
	}
	return $res
    }

    proc DictSort {dict} {
	array set tmp $dict
	set res {}
	foreach n [lsort -dict [array names tmp]] { lappend res $n $tmp($n) }
	return $res	
    }

    proc SkipHandledResolved {ref} {
	upvar 1 handled handled pl pl arch arch self self

	# Ignore references which are resolved already, one way or
	# other.

	if {[info exists handled($ref)]} {return -code continue}
	set handled($ref) .

	$self Puts -nonewline "Resolving [::teapot::reference::ref2tcl $ref]"

	if {[::teapot::reference::skip? $ref $pl $arch]} {
	    $self Puts " ... Skip."
	    return -code continue
	}

	return
    }

    method Requirements {repo instance recommends} {
	# recommends :: 0 <=> follow required,
	#               1 <=> follow required+recommended
	#               2 <=> follow none

	if {$recommends > 1} { return {} }

	set res {}
	foreach ref [$self Run ro $repo require [list $instance]] {
	    lappend res $ref
	}
	if {$recommends} {
	    foreach ref [$self Run ro $repo recommend [list $instance]] {
		lappend res $ref
	    }
	}
	return $res
    }


    method Find {archlist ref rvar rtvar ivar usearchives {usei 1}} {
	set install [$self Installation rw]

	upvar 1 $rvar repo $rtvar repotype $ivar instance _tcls _tcls mtvmismatch mtvmismatch

	set mtvmismatch 0

	# We search across all repositories for instances matching the
	# reference. We select the best match in general, with some
	# wrinkles thrown in. Like Tcl will not load a package anymore
	# if it already has a good, i.e. matching version in memory so
	# this code here will not consider retrieval from the archives
	# if a good match was found in the installation (or in the
	# list of pending retrievals). Even if the external archives
	# might contain a better match, i.e. higher version.
	#
	# A second optimization can be done when an exact match is
	# requested by a reference. The moment we have found it,
	# wherever, we can stop the search. We cannot get better.

	# At last: We look at the dependencies of all candidates and
	# remove any which cannot be run by any of the shells the
	# repository is linked to, based on their version of Tcl.
	# Bug 73962.

	set pv 0
	set type   [::teapot::reference::type $ref pn pv]
	set entity [::teapot::reference::rawentity $ref]
	set exact  [expr {$type eq "exact"}]

	log::debug "Find ($ref)"

	set rp  {}
	set res {} ; # Extended instance, if one found.

	if {$usei} {
	    SearchPending

	    # We skip the local installation if the caller forces the
	    # install. Otherwise we will not have a repository from
	    # which to get the package, only that it existed locally.

	    if {$usei < 2} {
		# Check the installation before the shell to avoid
		# incorrect inside vs. outside tagging (SearchShell sees
		# the packages installed in the repository too, and if
		# looked at first we would think that these are outside
		# although they are not).
		SearchInstallation
		SearchShell
	    }
	}

	# At this point no matching package instance was found
	# anywhere which can be interpreted as it being
	# "installed". Now we can go through the external
	# archives. And now we select the best match across all of
	# them.

	# III. Archives

	if {$usearchives} {
	    if {!$exact || ($res eq "")} {
		# find result = dict (repo -> list(ext-instance))

		# Bug 73962. We retrieve all possible instances which
		# are matching the reference, not only the highest
		# version. This is necessary to allow us to filter
		# them based on the minimal version of Tcl needed by
		# the entity versus the versions of Tcl available
		# through the shells connected to the destination
		# installation repository.

		set data [$self RunArchives ro findall $archlist $ref]

		# Drop shadowed redirections. This ensures that we use
		# a redirection for retrieval if and only if there is
		# no actual package present.
		set data [$self FilterRedirections $data]

		# final-result :: dict (repository -> list (instance ...))
		array set candidates [Invert $data]
		# candidates :: array (instance -> list(repository))

		# We look at all candidates and check if there are
		# ambiguities (= multiple different primary entity
		# types). Secondary entity types are broken out for
		# separate handling (Future, multi-value return from
		# Find).

		array set primary {}
		set instances {}
		foreach p [array names candidates] {
		    set i $p
		    teapot::instance::norm i
		    teapot::instance::split $i e n v a
		    if {[teapot::entity::primary $e]} {
			lappend primary($e) $i
			lappend instances $p
		    }
		}

		if {![array size primary]} {
		    # Nothing found. (Future: Secondaries do not count).
		    return 0
		} elseif {[array size primary] > 1} {
		    # Multiple different primary types, extract and
		    # return the instances causing that.
		    set instance [Names [Invert [array get primary]]]
		    return 2
		}

		# We have one type of primaries, we can now filter
		# further.

		# We sort the instances so that good instances are
		# likely at the front. Not only with regard to
		# versions, but with regard to architecture as
		# well. So that we will prefer a better matching
		# architecture (*) if all else is equal.
		#
		# (*) linux:   glibc version,
		#     solaris: kernel version

		set instances [lsort -index 2 -decreasing \
				   [lsort -index 3 -decreasing \
					$instances]]

		foreach p $instances {
		    set i $p
		    teapot::instance::norm i

		    # Get the minimal version of Tcl required by this
		    # instance and check it against the versions of
		    # Tcl provided by the shells connected to the
		    # destination installation repository (_tcls).
		    # Ignore all instances for which we have no good
		    # Tcl.

		    set ok 0
		    foreach r $candidates($p) {
			if {[MTVok $r $i]} {
			    set ok 1 ; break
			}
			set mtvmismatch 1
		    }
		    if {!$ok} continue

		    # Now we know that the current instance can be run
		    # by at least one shell, it is time to select the
		    # best version among all which managed to come to
		    # this location.

		    if {[Better $p $res]} {
			log::debug "- * Better $p > $res"
			set res $p
			set rp [lindex $candidates($p) 0]
			log::debug "- Archive @ [$rp cget -location]"
			set repotype archive
			if {$exact} break
		    }
		}
	    }
	}

	if {$res ne ""} {
	    set instance $res
	    set repo     $rp
	    return 1
	}
	return 0
    }

    proc MTVok {r i} {
	upvar 1 _tcls _tcls self self

	set mtv [teapot::metadata::minTclVersionMM \
		     [list require [$self Requirements $r $i 0]]]

	foreach t $_tcls {
	    if {[package vsatisfies $t $mtv]} {
		return 1
	    }
	}
	return 0
    }

    proc SearchPending {} {
	upvar 1 _pending _pending res res repotype repotype exact exact \
	    entity entity pn pn pv pv type type repo repo instance instance

	# I. Pending pool: <instance repo>-dict, ignore the repo
	#    references.

	# NOTE: The contents of pv are type-dependent!
	# exact => exact version, version => list of requirements.

	foreach {pi _repolist} $_pending {
	    # pi = extended instance (t n v p isprofile)

	    if {[Match $entity $pn $pv $type $pi]} {
		log::debug "- Pending already"

		set instance $pi
		set repo     {}
		set repotype pending

		# Abort not only the search here, but also 'Find'.
		# We have a good "installed" package, no further
		# search is required.

		return -code return 1
	    }
	}

	return
    }

    proc SearchShell {} {
	upvar 1 _installed _installed res res repotype repotype exact exact \
	    entity entity pn pn pv pv type type repo repo instance instance \
	    install install self self

	# IIa. Shells connected to chosen installation

	# NOTE: The contents of pv are type-dependent!
	# exact => exact version, version => list of requirements.

	foreach {p _v} $_installed {
	    # pi = instance + isprofile
	    set pi [list package $p $_v _ 0]
	    if {[Match $entity $pn $pv $type $pi]} {
		log::debug "- Installed (Shell): $_v"

		set instance $pi
		set repo     $install
		set repotype shell

		# Abort not only the search here, but also 'Find'.
		# We have a good "installed" package, no further
		# search is required.

		return -code return 1
	    }
	}

	return
    }

    proc SearchInstallation {} {
	upvar 1 res res repotype repotype self self repo repo \
	    instance instance archlist archlist ref ref install install

	# IIb. Chosen installation

	set p [$self RunInstall ro find $archlist $ref]
	if {[llength $p]} {
	    # p = list of (e n v p isprofile)

	    set pi [lindex $p 0]
	    # pi = (e n v p isprofile)

	    log::debug "- Installed (Repository): [lindex $pi 1]"

	    set instance $pi
	    set repo     $install
	    set repotype install

	    # Abort not only the search here, but also 'Find'.  We
	    # have a good "installed" package, no further search is
	    # required.

	    return -code return 1
	}

	return
    }

    proc Better {new old} {
	# new, old = instance     = {entity name ver arch}
	#          | ext-instance = {entity name ver arch isprofile}

	if {$old eq ""} {return 1}
	set nv [lindex $new 2]
	set ov [lindex $old 2]
	if {$nv eq $ov} {return 0}
	expr {[package vcompare $nv $ov] > 0}
    }

    proc Match {rt rn rv rtype instance} {

	# rtype = name, version,      exact
	# rv   -> '0' , requirements, version

	# NOTE
	# Searching in _pending can ignore the platform issue.
	# Because any instance winding up in _pending was already
	# checked for it by the archive and is matching.
	# We do have to check that the entity data matches.

	log::debug "Match A: [list $rt $rn $rv $rtype]"
	log::debug "Match B: $instance"

	# rt, rn, rv, rtype = reference

	teapot::instance::split $instance it in iv ip
	if {$rt ne $it} {return 0}

	switch -exact -- $rtype {
	    name {
		set match [expr {$in eq $rn}]
	    }
	    version {
		set match [expr {
			 ($in eq $rn) &&
			 [eval [linsert [::teapot::reference::req2tcl $rv] \
				    0 package vsatisfies $iv]]
		     }]
	    }
	    exact {
		set match [expr {
			 ($in eq $rn) &&
			 ([package vcompare $iv $rv] == 0)
		     }]
	    }
	}
	log::debug "Match  : $match"
	return $match
    }

    proc maxstr {v str} {
	upvar 1 $v max
	set l [string length $str]
	if {$l > $max} {set max $l}
	return
    }

    proc lj {n s} {format %-*s $n $s}
    proc rj {n s} {format %*s $n $s}

    proc blankstr {text} {
	# Replaces a string with equivalent whitespace.  All characters
	# except tab are replaced with a space.  Tab is whitespace, and
	# has to remain, or the replacement string will have different tab
	# stops.

	regsub -all -- {[^ 	]} $text { } text
	return $text
    }

    proc nsp {n singular {plural {}}} {
	return "$n [sp $n $singular $plural]"
    }

    proc sp {n singular {plural {}}} {
	if {$n == 1} { return $singular }
	if {$plural eq ""} { set plural ${singular}s }
	return $plural
    }

    # ### ### ### ######### ######### #########
    ## Retrieve a file specified via an url.
    ## Result is a path in the filesystem
    ## where the file can be picked up.

    proc GetUrl {url} {
	if {[regexp {^file:(.*)$} $url -> origin]} {
	    # We make a copy we can delete later. This makes it easier
	    # on the upcoming processing steps, as we do not have
	    # carry a boolean flag around telling us if the file can
	    # be deleted or not.

	    set path [fileutil::tempfile tpm]
	    file copy -force $origin $path
	    return $path
	}

	if {[regexp {^ftp:} $url]} {
	    set contents [ftp::geturl $url]

	    set path     [fileutil::tempfile tpm]
	    set ch       [open $path w]
	    fconfigure $ch -translation binary -encoding binary
	    puts -nonewline $ch $contents
	    close $ch
	    return $path
	}

	if {[regexp {^http:} $url]} {
	    set token [http::geturl $url -binary 1]
	    
	    log::debug "Received response ($token)"
	    log::debug [http::status $token]/[http::code $token]

	    if {[http::status $token] ne "ok"} {
		# Network error ...

		set msg  "Network error: [http::error $token]"
		http::cleanup $token

		log::debug $msg
		return -code error $msg

	    } elseif {[http::ncode $token] != 200} {
		# Http error ...

		set msg  "Http error: [http::data $token]"
		http::cleanup $token

		log::debug $msg
		return -code error $msg
	    }

	    set path     [fileutil::tempfile tpm]
	    set ch       [open $path w]
	    fconfigure $ch -translation binary -encoding binary
	    puts -nonewline $ch [http::data $token]
	    close $ch

	    http::cleanup $token
	    return $path
	}

	return -code error "Bad url \"$url\""
    }

    # ### ### ### ######### ######### #########
    ## Code for generating a tabular log listing. Input is a list of
    ## log entries.

    method GenLogListing {logentries} {
	if {![llength $logentries]} return

	set maxc 0 ; # serial/counter
	set maxe 0 ; # entity
	set maxn 0 ; # name
	set maxv 0 ; # version
	#set maxa 0 ; # architecture

	set dt {}
	foreach e $logentries {
	    foreach {sec ser _ type name ver _} $e break
	    maxstr maxc $ser
	    maxstr maxe $type
	    maxstr maxn $name
	    maxstr maxv $ver
	    lappend dt [clock format $sec -format {%Y-%m-%d %H:%M:%S}]
	}

	foreach e $logentries ts $dt {
	    foreach {_ ser act type name ver arch} $e break
	    $self Puts "[lj 19 $ts] [lj $maxc $ser] [lj 7 $act] [lj $maxe $type] [lj $maxn $name] [lj $maxv $ver] $arch"
	}
	return
    }

    # ### ### ### ######### ######### #########
    ## Code for generating a tabular instance listing. Input is a list of
    ## instances.

    method GenListing {instances {prefix {}}} {
	# instances = dict(annotated-instance -> list(repo))
	# annotated-instance = {type name ver arch isprofile note}

	set count [llength $instances]
	if {$count} {
	    set maxt 0
	    set maxn 0
	    set maxv 0
	    set maxp 0

	    maxstr maxt entity
	    maxstr maxn name
	    maxstr maxv version
	    maxstr maxp platform ;# visible name in MD! - architecture

	    set tmp {}
	    foreach {i _} $instances {
		teapot::instance::split $i t n v p
		maxstr maxt $t
		maxstr maxn $n
		maxstr maxv $v
		maxstr maxp $p
		lappend tmp $i
	    }
	    set count [llength $tmp]

	    set sep "[string repeat - $maxt] [string repeat - $maxn] [string repeat - $maxv] [string repeat - $maxp]"

	    $self Puts "$prefix[lj $maxt entity] [lj $maxn name] [lj $maxv version] platform"
	    $self Puts $prefix$sep
	    foreach i [lsort -dict -index 1 \
			   [lsort -dict -index 2 \
				[lsort -dict -index 3 \
				     $tmp]]] {
		teapot::instance::split $i t n v p
		set note [lindex $i 5]

		if {$note ne {}} {
		    $self Puts "$prefix[lj $maxt $t] [lj $maxn $n] [lj $maxv $v] [lj $maxp $p] $note"
		} else {
		    $self Puts "$prefix[lj $maxt $t] [lj $maxn $n] [lj $maxv $v] $p"
		}
	    }
	    $self Puts $prefix$sep
	}
	$self Puts "$prefix$count [expr {$count == 1 ? "entity" : "entities"}] found"
	return
    }

    # ### ### ### ######### ######### #########
    ## Code for generating a CSV instance listing. Input is a list of
    ## instances.

    method GenCSV {instances} {
	# instances = dict(ext-instance -> list(repo))

	$self Puts [csv::join {entity name version platform}] ;# platform is visible in MD. architecture

	set tmp {}
	foreach {i _} $instances {
	    teapot::instance::split $i t n v p
	    lappend tmp $i
	}

	foreach i [lsort -dict -index 1 \
		       [lsort -dict -index 2 \
			    [lsort -dict -index 3 \
				 $tmp]]] {
	    teapot::instance::split $i t n v p
	    $self Puts [csv::join [list $t $n $v $p]]
	}
	return
    }

    # ### ### ### ######### ######### #########
    ## Code for generating a profile. Input is a dictionary mapping
    ## repositories to lists of instances.

    method GenProfile {dict} {
	# dict = dict (repo -> list(ext-instance))
	$self Puts "\# TEAPOT - Profile"
	$self Puts ""
	$self Puts "\# Change the contents of this file as needed or desired,"
	$self Puts "\# especially profile name, version, and platform."
	$self Puts ""
	$self Puts "\# ### ### ### ######### ######### #########"
	$self Puts "\# Generated:   [clock format [clock seconds]]"
	$self Puts "\# User:        $::tcl_platform(user)"
	$self Puts "\# Application: teacup (teapot user client)"

	foreach {r instances} $dict {
	    if {$r eq ""} continue
	    $self Puts "\# Repository:  [$r cget -location]"
	}

	$self Puts "\# ### ### ### ######### ######### #########"
	$self Puts ""
	$self Puts "\# @@ Meta Begin"

	if {[llength $dict] == 2} {
	    # Single-repository profile.

	    set r [lindex $dict 0]
	    if {$r eq ""} {
		set name FOO
	    } else {
		set name [$r cget -location]
	    }

	    $self Puts "\# Profile [list $name 1.0]"

	} else {
	    # Multi-repository profile
	    $self Puts "\# Profile FOO 1.0"
	}

	$self Puts "\# Meta Platform tcl"
	$self Puts "\#"

	array set done {}
	foreach {r instances} $dict {
	    foreach instance [lsort -dict -index 1 \
				  [lsort -dict -index 2 \
				       [lsort -dict -index 3 \
					    $instances]]] {
		teapot::instance::split $instance t n v p
		if {[info exists done($t,$n,$v)]} continue

		$self Puts "\# Meta Require  [list [teapot::reference::cons $n -version $v -exact 1 -is $t]]"
		set done($t,$n,$v) .
	    }
	}

	$self Puts "\# @@ Meta End"
	$self Puts ""
	return
    }

    # ### ### ### ######### ######### #########
    ##

    # Input  = dict(repo -> list(value))
    # Result = dict(value -> list(repo))

    proc Invert {dict} {
	array set res {}
	foreach {r values} $dict {
	    foreach v $values {
		lappend res($v) $r
	    }
	}
	return [array get res]
    }

    proc Names {dict} {
	array set _ $dict
	return [array names _]
    }

    # ### ### ### ######### ######### #########
    ##

    variable log      {} ; # Command prefix to write texts too (x)
    variable config   {} ; # Command to retrieve the configuration from.

    # (x) Has to be syntactically equivalent to 'puts'.
    #     Except that no channel is allowed. Using 'puts'
    #     is perfectly acceptable.

    # Eventloop condition variable to make repository invocations
    # synchronous. Also used to transmit the result of the call.

    variable runResult ""

    # List of instances loaded from a snapshot file.

    variable _loaded ""

    # Pool of packages marked for retrieval. Also searched
    # like a repository to satisfy dependencies, if several
    # dependencies require the same package.

    # _pending = dict (ext-instance -> list(repo))

    variable _pending ""

    # Pool of packages coming from a shell connected to the
    # installation. Because this may provide packages which are
    # not in the installation itself (like standard stuff,
    # i.e. registry, dde, tcltest, http, ...).

    variable _installed ""

    # Callback when aborting operation due to argument errors, or
    # generally.

    variable onabort {}
    variable onstop  {}

    ##
    # ### ### ### ######### ######### #########
}

# ### ### ### ######### ######### #########
## Ready

package provide repository::client 0.1
