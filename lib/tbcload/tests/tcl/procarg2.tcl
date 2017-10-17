# procarg2.tcl --
#
#  Test file for compilation.
#  Contains a simple procedure whose argument list contains a malformed
#  element (argument has no name).
#  The compilation should fail here.
#
# Copyright (c) 1998-2000 by Ajuba Solutions.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) $Id: procarg2.tcl,v 1.2 2000/05/30 22:19:12 wart Exp $

proc a { {} } {
    return TEST
}

a TEST
