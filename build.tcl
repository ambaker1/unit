package require tin 1.0
tin import assert from tin
tin import tcltest
set version 0.1.4
set config [dict create VERSION $version]
tin bake src build $config
tin bake doc/template/version.tin doc/template/version.tex $config

source build/unit.tcl
# namespace import unit::*

# Perform tests
test main {
    # earlier tests
} -body {
unit system kip in sec
unit import ft ksi
assert {$ft == 12}
assert {$ksi == 1}

assert {[unit string disp] eq {in}}
assert {[unit string area] eq {in^2}}

assert {[unit reduce 20*kN/m^2] eq {0.0029007547546041853*ksi}}

unit import kg m
assert {[unit new rho [expr {$kg/$m**3}] mass 1 length -3] eq {value 9.357254687402184e-11 dimension {1 -4 2 0}}}
assert {[unit combine kg/m^3 rho] eq {value 9.357254687402184e-11 dimension {1 -4 2 0}}}

unit system default; # restore defaults (and avoids rounding error from switching systems)
unit system N m sec
unit import cm
assert {[unit expr {10*cm}] == [expr {10*$cm}]}
assert {[unit convert 5 lbm kg] == 2.26796185}
unit system kip in sec
assert {[unit convert 5 lbm kg] == 2.26796185}
assert {[unit convert 10 ft^2 in^2] == 1440.0}
assert {[unit convert {10 20 30} lbf*in N*m] eq {1.129848290276167 2.259696580552334 3.3895448708285008}}
} -result {}


# Check number of failed tests
set nFailed $::tcltest::numTests(Failed)

# Clean up and report on tests
cleanupTests

# If tests failed, return error
if {$nFailed > 0} {
    error "$nFailed tests failed"
}

# Tests passed, copy build files to main folder and install
file copy -force {*}[glob -directory build *] [pwd]
exec tclsh install.tcl

# Verify installation
tin forget unit
tin clear
tin import unit -exact $version
