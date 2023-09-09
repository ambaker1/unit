package require tin 1.0
set dir [tin mkdir -force unit 0.1.4]
file copy README.md LICENSE pkgIndex.tcl unit.tcl $dir 
