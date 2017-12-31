# Overview

This is a library for unit conversions and defining formulas with automated unit consistency
checking and conversions. This is similar to [http://www.cs.utexas.edu/users/novak/units95.html],
but written with ease of implementation over optimization. In other words, I understand how mine
works better. Most of the actual unit data is taken from that program, which is why this is under
GPLv2.

This is not yet well tested. Any comments are welcome.

# Dependencies

- iterate
- alexandria

# Usage
## Unit definition language

Units are described by s-expression. Units are identified by symbols, but interpreted by symbol
name, so the package is irrelevant. There is significant number of units already defined in
unit-data.lisp, check there or keys of `units-formula::*units*` hashtable for a list. Units can be
constructed from those by use of `*` `/` `expt` `sqrt` & `formula` operators. In a list with a unit
name as first element `*` is implied. Numbers might be included in unit definition, they will be
combined into contant factor.

## Reference

Function `reduce-unit unit-spec` turns the list in above format to an unit object, which can be used
in any place where unit definition can be used, to avoid repeated reduction of unit definition.

Function `convert-unit unit-from unit-to` takes two unit objects and returns a conversion factor
between them, or :incorrect-conversion if the units do not match.

Example:

`CL-USER> (unit-formulas:convert-unit '(/ parsec fortnight) '(/ km second))
2.5487764e7`

numbers can be included, as mentioned above, to convert between values, rather than to obtain
conversion factor:

`CL-USER> (unit-formulas:convert-unit '(5 kg) 'pound)
11.023113`

Function `same-unit-p unit1 unit2` takes two units and returns true if they are compatible. If key
argument `:factor` is true, then equality between constant factors is also checked.

Function `dimensionless-p unit` returns t if the unit is dimensionless (ie. only the constant factor
is relevant). It's value can be retrieved with `(convert-unit dimensionless-value nil)`

Macro `define-operators list-of-operators kind-keyword` allows definition of operators allowed in
formulas. Right now only :agree and :dimensionless kinds are present, which require all arguments to
be the same unit or dimensionless respectively.

Macro `defformula name (&rest in-spec) formula-expression` defines a formula. This creates a
function named `name`, which takes a &rest argument forming an association list of form (name value
unit) or (name unit-with-value). Argument in-spec is a list of form (name unit) or (name unit
value). The second form creates a named constant which will be folded into the formula. Note that
this has to literal number because this is folded at macroexpansion stage. Units in in-spec would in
most cases be base units, which have synonym symbols with the name of what it is an unit of.

Formula-expression consists of operators defined in `units-formula::*operators*` hash table, which
must have directly corresponding functions defined. Other allowed expressions are: symbol, naming
first a binding defined in in-spec, which will be replaced either by function argument or constant
value, if provided, a literal constant, either a number, an unit name, or (number unit-definition).

Created function returns an unit object, which can be converted to value in desired units with
`convert-unit`, or queried directly with `query-unit`.

Example:

    CL-USER> (unit-formulas:defformula K-np 
                                       ((effective-mass mass)
                                        (delta-e energy)
                                        (h-bar (/ (m m kg) s) #.(/ 6.62d-34 (* 2 pi)))
                                       (f electric-field))
                   (/ (/ (* 4 (sqrt (* 2 effective-mass (expt delta-e 3))))
                         (abs f))
    		      (* 3 elementary-charge h-bar)))
    K-NP
    CL-USER> (k-np '(effective-mass 0.2 electron-mass) '(delta-e 0.8 eV) '(f 0.09 (/ V (nano m))))
    #<UNIT-FORMULAS::UNIT 24.309902549224955d0 >

Function `query-unit unit` returns a property list with unit value and exponents of base SI units
forming an unit.

Function `identify-unit unit` tries to find a quantity with the same units, and if found returns a
keyword naming it.

Macro `defformula*` defines a formula using positional arguments, with much less error checking.  If
the wrong units are passed it will still fail, because symbols will be checked by the formula.

Macro `defformulae*` operates like defformula*, but formula-expression allows nesting of formulas
defined using defformulae*.  This is useful if you have formulas based on other formulas.

Macro `define-units` accepts unit-definitions as a list of ((unit-names) unit-definition) where
unit-names is a list of synonyms for the unit and unit-definition defines a relationship to a base unit.
The relationship definition can be a base-unit, a multiple of a base unit (ie. * / expt sqrt), or a
list in the form of (formula :convert-to [formula-symbol-name] :convert-from [formula-symbol-name]).
The latter defines the unit in terms of a formula defined by defformulae*, which allows for more
complex unit definitions.

Example:

	CL-USER> (unit-formulas:defformulae* celsius-to-kelvin ((c unity))
	   (* (+ c 273.15) kelvin))
	CELSIUS-TO-KELVIN
	CL-USER> (unit-formulas:defformulae* kelvin-to-celsius ((k kelvin))
	   (- k (273.15 kelvin)))
	KELVIN-TO-CELSIUS


	CL-USER> (unit-formulas:define-units ((celsius centigrade)
	   (unit-formulas:formula :convert-to celsius-to-kelvin
		                  :convert-from kelvin-to-celsius)))
        NIL


	CL-USER> (unit-formulas:convert-unit '(100 celsius) 'kelvin)
	373.15d0
	CL-USER> (unit-formulas:convert-unit '(0 kelvin) 'centigrade)
	-273.15d0

In the above example two formulae were defined.  The first allows a dimensionless unit to be
converted into kelvin.  The second formula converts from kelvin back to celsius.  In the second
case we are not concerned with units since `convert-unit` calls this formula and will be returning
a unitless float.

The third form defines two synonyms `celsius` and `centigrade` that are units that use our formulae
to convert-to and from a base unit instead of the defaults, which are `*` and `/`.

Function `transform-units` defines a generic-function that accepts an `input-unit`, an `output-unit`, and a `unit-bag` list of available units to use in performing dimensional analysis, converting the input-unit to a value in the input unit.  This is a basic implementation and is far from optimized, but should work for all cases.

Example:

	UNIT-FORMULAS> 	(defvar *molar-mass-of-water*
			  (reduce-unit '(18.01528 (/ grams mol))))
			(defvar *density-of-water*
  	  		  (reduce-unit '(1 (/ gram mL))))

			(transform-units '(100 gallons)
		   			 'moles
		   			 (list *density-of-water*
			 		       *molar-mass-of-water*))

	21012.228153913533d0
	UNIT-FORMULAS> 

