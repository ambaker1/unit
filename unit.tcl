# unit.tcl
################################################################################
# Unit system based on force, length, time, and thermodynamic temperature
# Force is not typically considered a base unit, but is useful for FEA analysis
# to consider it as one. 
# This is not object oriented because there is no reason to work with multiple
# unit systems at the same time.

# Based on the OpenSees command "defaultUnits" and the Tcllib units package

# Copyright (C) 2022 Alex Baker, ambaker1@mtu.edu
# All rights reserved. 

# See the file "LICENSE" in the top level directory for information on usage, 
# redistribution, and for a DISCLAIMER OF ALL WARRANTIES.
################################################################################

# Define namespace
namespace eval ::unit {
    # Variables
    # unitData: Nested dictionary with conversion value and dimension
    variable unitData ""
    # $unitName
    #   value $conversion
    #   dimension $exponents
    #   imported $boolean

    # baseTypes: List of base unit types
    variable baseTypes {force length time temp}
    
    # baseUnits: Dictionary for base units defined by "unit system"
    # Even if the base units are removed, it will still remember the
    # defined base units (for unit string output)
    variable baseUnits ""
    # $baseType $unitName
    
    # typeDims: unit type with vector of base unit exponents
    # Unit types are then used for easy definition and query of units
    # Initialized with constant and base unit types (permanent)
    variable typeDims {
        constant {0 0 0 0}
        force {1 0 0 0}
        length {0 1 0 0}
        time {0 0 1 0}
        temp {0 0 0 1}
    }
    
    # Exported commands
    set subcommands {
        combine
        convert
        exists
        expr
        import
        info
        names
        new
        reduce
        remove
        string
        system
        type
        types
        wipe
    }
    variable subcommandMap ""
    foreach subcommand $subcommands {
        dict set subcommandMap $subcommand _$subcommand
    }
    # Create ensemble for export
    namespace ensemble create -command unit -prefixes 0 -map $subcommandMap
    namespace export unit
    # Breakdown of subcommands:
    ########################################################################
    # combine:  Combine units using unit string notation
    # convert:  Convert between unit strings (with dimensionality check)
    # exists:   Check if unit exists
    # expr:     Get unit conversion value associated with unit string
    # import:   Import units as variables
    # info:     Get dictionary of conversion value and dimension for unit
    # names:    Get list of defined units, optionally with dimensions specified
    # new:      Create a unit, with specified dimensions
    # remove:   Remove units
    # string:   Get unit string for specified dimensions
    # system:   Define or redefine the base units for the system of units
    # type:     Define, remove, or query unit type dimensions
    # types:    Get unit types, optionally with dimensions specified
    # wipe:     Clear unit system
}

# unit type --
# 
# Procedure to access and modify unit types, using GetDimension
# New types must be words (no space, no special characters)
# Returns dimension vector for the unit type
#
# Arguments:
# typeName:     Name of unit type
# args:         Three input modes:
#               1. No inputs - returns dimension vector of unit type
#               2. Blank input - removes unit type
#               3. Single unit type or dimension vector
#               4. Dictionary of defined unit types and exponents
#
# Examples:
# unit type force -> {1 0 0 0}
# unit type area length 2 -> Creates "area" unit type
# unit type Area area -> Creates alias for unit type "area"
# unit type Area "" -> Deletes unit type "Area"

proc ::unit::_type {typeName args} {
    variable typeDims
    variable baseTypes
    # Query case
    if {[llength $args] == 0} {
        if {![dict exists $typeDims $typeName]} {
            return -code error "Unknown unit type"
        }
        return [dict get $typeDims $typeName]
    }
    # Removal or redefinition. Do not allow changes to base types.
    if {$typeName in $baseTypes || $typeName eq {constant}} {
        return -code error "Cannot modify base types"
    }
    if {[llength $args] == 1 && [lindex $args 0] == ""} {
        # Removal case
        dict unset typeDims $typeName
        return ""
    } else {
        # Definition case
        set dimension [GetDimension {*}$args]
        dict set typeDims $typeName $dimension
        return $dimension
    }
}

# GetDimension --
#
# Private procedure to normalize input and return dimension exponents
# 
# Arguments:
# args:         Input arguments. Switches with arity
#   No arguments: Error
#   One argument: Either a unit type or a dimension vector
#   Even arguments > 1: Dictionary of unit types and exponents
#   Odd arguments > 1: Error

proc ::unit::GetDimension {args} {
    variable typeDims
    # User specified type or dimensions
    if {[llength $args] == 0} {
        return -code error "Must provide input for dimension"
    } elseif {[llength $args] == 1} {
        set input [lindex $args 0]
        if {[llength $input] == 1} {
            # Unit type specified
            set unitType $input
            if {![dict exists $typeDims $unitType]} {
                return -code error "Unknown unit type \"$unitType\""
            }
            set dimension [dict get $typeDims $unitType]
        } elseif {[llength $input] == 4} {
            # Dimension vector (validate)
            set dimension $input
            foreach exponent $dimension {
                if {![string is double -strict $exponent]} {
                    return -code error "Invalid exponent in dimension vector"
                }
            }; # end foreach exponent
        } else {
            return -code error "Unknown dimension input"
        }
    } elseif {[llength $args]%2 == 0} {
        # Dictionary-style type input
        set derivation $args
        # Type derivation specified
        set dimension {0 0 0 0}; # Initialize
        set derivation $args
        dict for {input exponent} $derivation {
            # Get dimension of input part
            set dimPart [GetDimension $input]
            if {![string is double -strict $exponent]} {
                return -code error "Invalid exponent"
            }   
            set dimension [lmap a $dimension b $dimPart {
                expr {$a + $b*$exponent}
            }]
        }; # end dict for derivation
    } else {
        return -code error "Incorrect number of dimension arguments"
    }
    return $dimension
}

# unit types --
#
# Returns a list of unit types, with optional dimension input
#
# Arguments:
# args:     Unit dimension arguments (blank, returns all)

proc ::unit::_types {args} {
    variable typeDims
    if {[llength $args] == 0} {
        # All unit types
        return [dict keys $typeDims]
    } else {
        # User specified dimensions. Normalize and find base-compatible types
        set dimension [GetDimension {*}$args]
        return [dict keys [dict filter $typeDims script {key value} {
            BaseCompatible $dimension $value
        }]]
    }
}

# BaseCompatible --
#
# Checks if dimension vectors for two units are compatible.
#
# Arguments:    
# dim1:        Dimensions of first unit
# dim2:        Dimensions of second unit

proc ::unit::BaseCompatible {dim1 dim2} {
    foreach exp1 $dim1 exp2 $dim2 {
        if {$exp1 != $exp2} {
            return 0
        }
    }
    return 1
}

# unit system --
#
# Define or redefine base force, length, time and temperature units
#
# Arguments:
# forceUnit:        Unit for force
# lengthUnit:       Unit for length
# timeUnit:         Unit for time
# tempUnit:         Unit for temperature (temp) (optional, default K)

proc ::unit::_system {args} {
    variable unitData
    variable typeDims
    variable baseUnits
    variable baseTypes
    
    # Check arity
    if {[llength $args] == 0} {
        # Query case
        return [dict values $baseUnits]
    } elseif {[llength $args] == 1 && [lindex $args 0] eq {default}} {
        # Restore defaults (N m sec K)
        return [DefaultUnits]
    } elseif {[llength $args] == 3} {
        # Default temperature unit case
        set unitNames [concat $args K]
    } elseif {[llength $args] == 4} {
        set unitNames $args
    } else {
        return -code error "Incorrect number of input arguments"
    }
    
    # Initialization case
    if {[dict size $unitData] == 0} {
        # Set base unit data
        foreach unitName $unitNames unitType $baseTypes {
            dict set baseUnits $unitType $unitName
            dict set unitData $unitName value 1.0
            dict set unitData $unitName dimension [dict get $typeDims $unitType]
        }
        # Return the base unit names (same as query case)
        return $unitNames
    }
    
    # Redefinition case
    # Check validity of input system units, and get conversion factors
    set factors ""
    foreach unitName $unitNames unitType $baseTypes {
        if {![dict exists $unitData $unitName]} {
            return -code error "Unit \"$unitName\" does not exist"
        }
        set unitDim [dict get $unitData $unitName dimension]
        set baseDim [dict get $typeDims $unitType]
        if {![BaseCompatible $unitDim $baseDim]} {
            return -code error "Unit \"$unitName\" not of type \"$unitType\""
        }
        dict set baseUnits $unitType $unitName
        lappend factors [dict get $unitData $unitName value]
        dict unset unitData $unitName; # temporary
    }
    
    # Convert derived units to new base
    dict for {unitName unitInfo} $unitData {
        set value [dict get $unitInfo value]
        set baseValue 1.0
        foreach factor $factors exponent [dict get $unitInfo dimension] {
            if {$exponent != 0.0} {
                set baseValue [expr {
                    $baseValue*$factor**$exponent
                }]
            }; # end if relevant
        }; # end foreach base and corresponding exponent 
        dict set unitData $unitName value [expr {$value/$baseValue}]
    }
    
    # Set base units to unity
    dict for {baseType unitName} $baseUnits {
        dict set unitData $unitName value 1.0
        dict set unitData $unitName dimension [dict get $typeDims $baseType]
    }
    
    # Return the base unit names (same as query case)
    return $unitNames
}

# unit new --
#
# Define a unit to be used in the units module
# Returns unit info
#
# Arguments:
# unitName:     Unit to add
# value:        Scalar conversion value from unit to base units
# args:         Unit dimension arguments

proc ::unit::_new {unitName value args} {
    variable unitData
    variable baseUnits
    
    # Ensure that unit system is called first
    if {[dict size $baseUnits] == 0} {
        return -code error "Must define base units first"
    }
    
    # Validate unit name
    if {![string is wordchar $unitName] || [string is double $unitName]} {
        return -code error "Unit name invalid: Must be alpha-numeric with at \
                least one alphabetic character"
    }
    
    # Check validity of conversion
    if {![string is double -strict $value]} {
        return -code error "Unit value must be a number"
    }
    
    # Interpret dimension arguments
    set dimension [GetDimension {*}$args]
    
    # Save unit in internal memory
    dict set unitData $unitName value $value 
    dict set unitData $unitName dimension $dimension

    # Return unit info
    return [dict get $unitData $unitName]
}

# unit combine --
#
# Combines units to make new units. 
# Base unit exponents are carried through.
# Only change in offset units will be considered.
# Returns unit info
#
# Arguments:
# unitString: Unit string (no substitution) of combined unit
#   Valid operators: * / ^ ( )
#   e.g. unit combine in^2 in2 
#   e.g. unit combine kip*sec^2/in mass 
# unitName: Unique name to call combined unit. 
#   If left blank, no new unit will be made, but unit info will be returned

proc ::unit::_combine {unitString {unitName ""}} {
    variable unitData
    variable baseTypes
    variable baseUnits
    
    # Ensure unit expression is not list
    set unitString [join $unitString {}]
    
    # Initialize expressions
    set valueExpr ""
    foreach unitType $baseTypes {
        set powerExpr($unitType) ""
    }
    set valueMap {^ **}; # Map to convert to Tcl expression math.
    set powerMap {* + ^ * / -}; # Map to convert to exponent math.
    
    # Parse through expression
    # The idea here is to create a value expression, where the values are equal
    # to the substituted values of the units (no offset), and to create 
    # exponent equations for the individual base units.
    set word ""
    set lastOperator ""
    set units ""
    foreach char [list {*}[split $unitString {}] {}] {
        if {$char in {* / ^ ( ) {}}} {
            # Character is valid operator (or the end), add word to expressions
            if {$word eq ""} {
                # No word. Back-to-back operators
            } elseif {[string is double $word]} {
                # Word is a number. 
                set number [expr {$word}]
                # Add number to value expressions
                append valueExpr $number
                # Check if exponent or scalar
                if {$lastOperator eq {^}} {
                    # Previous value is exponent
                    # Assuming that all unit expressions have raw numbers in 
                    # exponents.
                    set exponent $number
                } else {
                    set exponent 0
                }
                # Add to power expression
                foreach unitType $baseTypes {
                    append powerExpr($unitType) $exponent
                }
            } elseif {[dict exists $unitData $word]} {
                # Word is a unit, get data for unit
                set value [dict get $unitData $word value]
                set dimension [dict get $unitData $word dimension]
                # Add value of unit to value expression, without offset
                append valueExpr $value
                # Add dimension exponents to power expressions
                foreach unitType $baseTypes exponent $dimension {
                    append powerExpr($unitType) $exponent
                }
            } elseif {$word in [dict values $baseUnits]} {
                # Word is base unit that was removed. Still valid tho.
                set value 1.0
                set i [lsearch -exact [dict values $baseUnits] $word]
                set dimension [lreplace {0 0 0 0} $i $i 1.0]
            } else {
                return -code error "Unit \"$word\" not defined"
            }
            
            # Add character to value expression
            append valueExpr [string map $valueMap $char]
            # Add mapped character to power expressions
            foreach unitType $baseTypes {
                append powerExpr($unitType) [string map $powerMap $char]
            }
            
            # Reset word and save operator
            set word ""
            set lastOperator $char
        } else {
            # Keep building the word
            append word $char
        }
    }

    # Evaluate expressions
    set value [expr $valueExpr]
    set dimension [lmap unitType $baseTypes {
        expr $powerExpr($unitType)
    }]
    
    # Add unit if specified
    if {$unitName ne ""} {
        unit new $unitName $value $dimension
    }
    
    # Return the unit info (regardless if unit was created or not)
    return [list value $value dimension $dimension]
}

# unit import --
#
# Import unit conversion values into global variables.
# Must re-import if the unit system changes.
#
# Arguments:
# args:         Units to import. -all for all

proc ::unit::_import {args} {
    variable unitData
    variable baseUnits
    # -all option
    if {[llength $args] == 1 && [lindex $args 0] eq "-all"} {
        set unitNames [dict keys $unitData]
    } else {
        set unitNames $args
    }
    foreach unitName $unitNames {
        # Get unit info (value and dimension)
        if {![dict exists $unitData $unitName]} {
            return -code error "Unit \"$unitName\" does not exist"
        }
        upvar 1 $unitName var
        # Remove existing tracer (does not throw error if none)
        trace remove variable var write ::unit::Tracer
        # Create variable in caller, add tracer
        set var [dict get $unitData $unitName value]
        trace add variable var write ::unit::Tracer
    }
    return
}

# Tracer --
#
# Tracer for imported unit variables. Displays warning when overwritten.
#
# Arguments:
# unitName:         Variable name (unit name)
# args:             Unused

proc ::unit::Tracer {unitName args} {
    puts "WARNING: Unit variable \"$unitName\" was overwritten"
    upvar 1 $unitName var
    trace remove variable var write ::unit::Tracer
    return
}
# unit names --
#
# Get the names of units, with optional dimension input
#
# Arguments:
# args:     Unit dimension arguments (blank, returns all)

proc ::unit::_names {args} {
    variable unitData
    if {[llength $args] == 0} {
        return [dict keys $unitData]
    } else {
        # Get dimension from input, and filter
        set dimension [GetDimension {*}$args]
        return [dict keys [dict filter $unitData script {unitName unitInfo} {
            BaseCompatible $dimension [dict get $unitInfo dimension]
        }]]
    }
}

# unit string --
#
# Get base unit string for specified unit dimensions
#
# args:     Unit dimension arguments

proc ::unit::_string {args} {
    variable typeDims
    variable baseTypes
    variable baseUnits
    variable unitData
    
    # Ensure that unit system is called first
    if {[dict size $baseUnits] == 0} {
        return -code error "Must define base units first"
    }
    
    # Interpret dimension input
    set dimension [GetDimension {*}$args]
    
    # Check simple base unit case
    set derived 0
    set bases 0
    foreach exponent $dimension baseType $baseTypes {
        if {$exponent == 0} {
            continue
        } elseif {$exponent == 1 && $bases == 0} {
            set baseUnit [dict get $baseUnits $baseType]
        } else {
            set derived 1
            break
        }
        incr bases
    }
    if {!$derived} {
        return $baseUnit
    }
    
    # The remainder of this proc deals with derived units. First, try to find
    # a unit with the same dimension and a value of 1.0
    dict for {unitName unitInfo} $unitData {
        if {[dict get $unitInfo value] == 1.0} {
            if {[BaseCompatible [dict get $unitInfo dimension] $dimension]} {
                return $unitName
            }
        }
    }
    
    # No unit exists with that base and value of 1.0. Decompose into base units
    set numerator ""
    set denominator ""
    foreach {baseType baseUnit} $baseUnits exponent $dimension {
        if {$exponent != 0} {
            if {$exponent > 0} {
                if {$exponent == 1} {
                    lappend numerator $baseUnit
                } else {
                    lappend numerator $baseUnit^$exponent
                }
            } elseif {$exponent == -1} {
                lappend denominator $baseUnit
            } else {
                lappend denominator $baseUnit^[expr {-$exponent}]
            }
        }; # end if exponent != 0
    }; # end foreach base exponent
    
    # Other cases
    set unitString ""
    if {[llength $numerator] == 0} {
        append unitString 1
    } else {
        append unitString [join $numerator *]
    }
    if {[llength $denominator] > 0} {
        append unitString /
        if {[llength $denominator] == 1} {
            append unitString [lindex $denominator 0]
        } else {
            append unitString ([join $denominator *])
        }
    }
    return $unitString
}

# unit reduce --
#
# Reduce a unit string to base unit string
#
# Arguments:
# unitString:   Unit string to reduce

proc ::unit::_reduce {unitString} {
    set unitInfo [unit combine $unitString]
    set baseUnitString [unit string [dict get $unitInfo dimension]]
    return "[dict get $unitInfo value]*$baseUnitString"
}

# unit convert --
#
# Convert between units (with unit expressions)
#
# Arguments:
# <values>:         Optional first argument, the values to convert (default 1.0)
# unitString1:      Unit expression to convert from
# unitString2:      Unit expression to convert to

proc ::unit::_convert {args} {
    variable unitData
    variable baseTypes
    
    # Check arity
    if {[llength $args] < 2 || [llength $args] > 3} {
        return -code error "Incorrect number of arguments"
    }
    
    # Get unit strings from arguments
    set unitString(1) [lindex $args end-1]
    set unitString(2) [lindex $args end]

    # Get data about unit strings
    foreach i {1 2} {
        set unitInfo [unit combine $unitString($i)]
        set unitVal($i) [dict get $unitInfo value]
        set unitDim($i) [dict get $unitInfo dimension]
    }
    
    # Check compatibility
    if {![BaseCompatible $unitDim(1) $unitDim(2)]} {
        return -code error "Unit strings \"$unitString(1)\" &\
                \"$unitString(2)\" are not compatible"
    }

    # Get conversion factor
    set factor [expr {$unitVal(1)/$unitVal(2)}]
    
    # Convert values
    if {[llength $args] == 2} {
        # Return conversion factor
        return $factor
    } else {
        # Perform unit conversion on values
        return [lmap value [lindex $args 0] {
            expr {$value*$factor}
        }]
    }
}

# unit remove --
#
# Removes units from data (does not remove any imported variables).
# Preserves base unit system and unit types.
#
# Arguments:
# args:         Names of units to remove (-all for all)

proc ::unit::_remove {args} {
    variable unitData
    # -all option
    if {[llength $args] == 1 && [lindex $args 0] eq "-all"} {
        set unitNames [dict keys $unitData]
    } else {
        set unitNames $args
    }
    foreach unitName $unitNames {
        dict unset unitData $unitName
    }
    return
}

# unit wipe --
#
# Removes all units and baseUnits from data (does not remove imported units).

proc ::unit::_wipe {} {
    variable unitData ""
    variable baseUnits ""
    variable typeDims {
        constant {0 0 0 0}
        force {1 0 0 0}
        length {0 1 0 0}
        time {0 0 1 0}
        temp {0 0 0 1}
    }
}

# unit exists --
#
# Returns whether a specified unit exists (simple call to dict exists)
#
# Arguments:
# unitName:     Unit to check existance of

proc ::unit::_exists {unitName} {
    variable unitData
    return [dict exists $unitData $unitName]
}

# unit info --
#
# Returns info for unit (dictionary with value and base)
# Simply a query of the unitData dictionary
#
# Arguments:
# unitName:     Unit to query

proc ::unit::_info {unitName} {
    variable unitData
    if {![dict exists $unitData $unitName]} {
        return -code error "Unit \"$unitName\" not defined"
    }
    return [dict get $unitData $unitName]
}

# unit expr --
#
# Performs unit expression using unit combine, returns value in base units
# Only multiplication, division, exponents, and parentheses allowed
# Could be expanded in the future to allow for other operations (e.g. addition)
#
# Arguments:
# args:         Unit string, to be substituted

proc ::unit::_expr {args} {
    return [dict get [unit combine [uplevel 1 [list subst $args]]] value]
}

# Define built-in units
# -----------------------------------------------------------------------
# Derivation of conversion value from lbf to Newtons:
## Definition of pound force ##
# 1 lbf = lbm * g
## Conventional standard gravity ##
# Established by 3rd General Conference on Weights and Measures
# g = 9.80665 m/sec^2
## Conversion between lbm and kg ##
# Using the International pound, which has an exact conversion to kilograms
# lbm = 0.45359237 kg
## Definition of Newton ## 
# N = kg*m/sec^2
# So, lbf = 9.80665*0.45359237 N = 4.4482216152605 N
# -----------------------------------------------------------------------
proc ::unit::DefaultUnits {} {
    # Clear and redefine unit system
    unit wipe
    unit system N m sec K
    
    # Selected unit types   
    ####################################################################
    unit type angle constant
    unit type disp length
    unit type displacement length
    unit type temperature temp
    unit type area length 2
    unit type volume length 3
    unit type velocity length 1 time -1
    unit type vel velocity
    unit type acceleration length 1 time -2
    unit type accel acceleration
    unit type mass force 1 acceleration -1
    unit type rotMass mass 1 length 2
    unit type inertia mass 1 length 2
    unit type moment force 1 length 1
    unit type energy force 1 length 1
    unit type stiffness force 1 length -1
    unit type stress force 1 area -1
    unit type pressure force 1 area -1
    unit type massDensity mass 1 volume -1
    unit type forceDensity force 1 volume -1
    unit type frequency time -1

    # Constants
    ####################################################################
    unit new pi 3.141592653589793 constant
    unit new e 2.718281828459045 constant

    # Angles (not quite units)
    ####################################################################
    unit new rad 1.0 angle; # Tcl math uses radians
    unit combine pi*rad/180.0 deg
    unit combine 2*pi*rad cycle

    # Force units
    ####################################################################
    unit combine 1e3*N kN 
    unit combine 1e6*N MN
    unit combine 9.80665*N kgf
    unit combine 1e3*kgf tonf
    unit combine 4.4482216152605*N lbf
    unit combine 1e3*lbf kip

    # Length units
    ####################################################################
    unit combine 1e-3*m mm 
    unit combine 1e-2*m cm 
    unit combine 1e3*m km
    unit combine 2.54*cm in
    unit combine 12.0*in ft
    unit combine 3.0*ft yd

    # Time units
    ####################################################################
    unit combine 1e-3*sec msec
    unit combine 60.0*sec min
    unit combine 60.0*min hr

    # Thermodynamic temperature units
    ####################################################################
    unit combine 0.5555555555555556*K R; # Rankine

    # Gravitational acceleration
    ####################################################################
    unit combine 9.80665*m/sec^2 g; # Conventional value

    # Frequency units
    ####################################################################
    unit combine cycle/sec Hz; # Hertz
    unit combine cycle/min rpm; # Revolutions per minute

    # Mass units
    ####################################################################
    unit combine N*sec^2/m kg; # N = kg*m/sec^2
    unit combine 1e-3*kg gram
    unit combine 1e3*kg Mg
    unit combine lbf*sec^2/ft slug; # lbf = slug*ft/sec^2
    unit combine lbf/g lbm; # lbf = lbm*g. 

    # Pressure/stress units
    ####################################################################
    unit combine kip/in^2 ksi
    unit combine kip/ft^2 ksf
    unit combine lbf/in^2 psi
    unit combine lbf/ft^2 psf
    unit combine N/m^2 Pa; # Pascal
    unit combine 1e3*Pa kPa
    unit combine 1e6*Pa MPa
    unit combine 1e9*Pa GPa
    unit combine 101325*Pa atm; # Standard atmosphere

    # Force density units
    ####################################################################
    unit combine lbf/ft^3 pcf

    # Energy units
    ####################################################################
    unit combine N*m J; # Joule
    unit combine 1e3*J kJ
    
    # Return the system (N m sec K)
    return [unit system]
}

# Call DefaultUnits
::unit::DefaultUnits

# TO DO:
# 1. Imported units should live update when unit system changes, and should be
#       removed if the unit is removed.
# 2. Unit expr should allow for addition and subtraction.

# Finally, provide the package
package provide unit 0.1.2
