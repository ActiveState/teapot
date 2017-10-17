# timer - a timer object
package provide Timer 1.0

package require snit

::snit::type Timer {
    variable cancel ""	;# after timer

    # cancel any timers
    method cancel {} {
	upvar 1 self owner
	#Debug {Timer $self for '$owner' cancel $cancel}
	if {$cancel != ""} {
	    after cancel $cancel
	    set cancel ""
	}
    }

    # start a new timer
    method after {when what} {
	upvar 1 self owner

	uplevel 1 $self cancel
	set cancel [after $when $what]
	#Debug {Timer $self for '$owner' after $when '$what' -> $cancel}
	#Debug {Timer $self [info level -1]}
    }
}
