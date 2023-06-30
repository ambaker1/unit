package require tin 0.7
set dir [tin mkdir -force unit 0.1]
file copy README.md LICENSE pkgIndex.tcl unit.tcl $dir 
