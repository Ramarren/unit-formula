;;     unit-formulas - library for unit checked formula definitions
;;     Copyright (C) 2008 Jakub Higersberger

;;     This program is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation; either version 2 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program; if not, write to the Free Software
;;     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


(in-package :unit-formulas)

;;mostly taken from http://www.cs.utexas.edu/users/novak/units95.html , this is GPLed
;; use macros to convert to my format

(defmacro import-novak-simple (unit definitions)
  `(define-factors ,unit
       ,@(iter (for (name factor synonyms) in definitions)
	       (collect (cons name synonyms))
	       (collect factor))))

(defmacro import-novak-derived (definitions)
  `(define-units ,@(iter (for (name unit synonyms) in definitions)
			 (collect (cons name synonyms))
			 (collect unit))))

(import-novak-simple nil
	      ((radian    1.0                (radians))
	       (steradian 1.0                (sr steradians))
	       (degree    0.01745329251994   (deg degrees))
	       (arcminute 0.0002908882086657 (arcmin arcminutes arc-minute
						     arc-minutes))
	       (arcsecond 4.848136811095e-6  (arcsec arcseconds arc-second
						     arc-seconds))
	       (pi             3.1415926535897931 ())
	       (unity          1.0       ())
	       (zero           0         ())
	       (one            1         ())
	       (two            2         ())
	       (three          3         ())
	       (four           4         ())
	       (five           5         ())
	       (six            6         ())
	       (seven          7         ())
	       (eight          8         ())
	       (nine           9         ())
	       (dozen          12.0      ())
	       (gross          144.0     ())
	       (ten            10.0      ())
	       (twenty         20.0      ())
	       (thirty         30.0      ())
	       (forty          40.0      ())
	       (fifty          50.0      ())
	       (sixty          60.0      ())
	       (seventy        70.0      ())
	       (eighty         80.0      ())
	       (ninety         90.0      ())
	       (hundred        100.0     ())
	       (thousand       1000.0    ())
	       (million        1.0e6     ())
	       (billion        1.0e9     ())
	       (trillion       1.0e12    ())
	       (quadrillion    1.0e15    ())
	       (quintillion    1.0e18    ())
	       (percent        0.01      (\% percent))
	       (tenth          0.1       ())
	       (hundredth      0.01      ())
	       (thousandth     0.001     ())
	       (millionth      1.0e-6    ())
	       (billionth      1.0e-9    ())
	       (trillionth     1.0e-12   ())
	       (quadrillionth  1.0e-15   ())
	       (quintillionth  1.0e-18   ())
	       (yotta          1.0e24    (yotta-))
	       (zetta          1.0e21    (zetta-))
	       (exa            1.0e18    (exa-))
	       (peta           1.0e15    (peta-))
	       (tera           1.0e12    (tera-))
	       (giga           1.0e9     (giga-))
	       (mega           1.0e6     (mega-))
	       (kilo           1000.0    (kilo-))
	       (hecto          100.0     (hecto-))
	       (deka           10.0      (deca deka- deca-))
	       (deci           0.1       (deci-))
	       (centi          0.01      (centi-))
	       (milli          0.001     (milli-))
	       (micro          1.0e-6    (micro-))
	       (nano           1.0e-9    (nano-))
	       (pico           1.0e-12   (pico-))
	       (femto          1.0e-15   (femto-))
	       (atto           1.0e-18   (atto-))
	       (zepto          1.0e-21   (zepto-))
	       (yocto          1.0e-24   (yocto-))))

(import-novak-simple meter
	      ((meter         1.0       (m meters metre))
	       (foot          0.3048    (ft feet))
	       (decimeter     0.1       (dm decimeters decimetre))
	       (centimeter    0.01      (cm centimeters centimetre))
	       (millimeter    0.001     (mm millimeters millimetre))
	       (dekameter     10.0      (dam dekameters decameter
					     decameters decametre))
	       (hectometer    100.0     (hm hectometers hectometre))
	       (kilometer     1000.0    (km kilometers kilometre))
	       (micron        1.0e-6    (um micro-meter micrometer
					    micrometers micro-meters
					    microns micrometre))
	       (nanometer     1.0e-9    (nm nanometers nanometre))
	       (angstrom      1.0e-10   (ang angstroms))
	       (inch          0.0254    (in inches))
	       (mile          1609.344  (mi miles))
	       (nautical-mile 1852.0    (nm nauticalmiles
					    nauticalmile nautical-miles))
	       (astronomical-unit 
		1.49598e11 (au))
	       (light-year    9.46e15    (ly light-years
					     lightyear lightyears))
	       (parsec        3.083e16   (parsecs))
	       (fathom        1.8054     (fathoms))
	       (yard          0.9144     (yd yards))
	       (rod           5.0292     (rods))
	       (mil           0.0000254  (mils))
	       (furlong       201.168    (furlongs))))


(import-novak-simple kilogram
	      ((kilogram         1.0           (kg kilograms))
	       (hectogram        0.1           (hg hectograms))
	       (dekagram         0.01     (dag dekagrams decagram decagrams))
	       (gram             0.001         (gm grams))
	       (decigram         0.0001        (dg decigrams))
	       (centigram        0.00001       (cg centigrams))
	       (milligram        1.0e-6        (mg milligrams))
	       (microgram        1.0e-9        (ug micrograms))
	       (metric-ton       1000.0        (metric-tons tonne tonnes))
	       (pound            0.45359237    (lb lbs pounds))    ; exactly
	       (slug             14.593902937  (slugs))
	       ;; derived 02 Jun 95 based on pound, foot, and earth-gravity
	       (atomic-mass-unit 1.6605402e-27 (amu atomic-mass-units))
	       (earth-mass       5.98e24       ())))

(define-factors second (s sec secs seconds) 1.0)

(import-novak-derived 
 ((millisecond (* milli second)     (ms msec millisec
					milliseconds))
  (microsecond (* micro second)     (us usec microsec
					microseconds))
  (nanosecond  (* nano  second)     (ns nsec nanosec
					nanoseconds))
  (picosecond  (* pico  second)     (ps psec picosec
					picoseconds))
  (femtosecond (* femto second)     (femtoseconds femtosec))
  (attosecond  (* atto  second)     (attoseconds attosec))
  (minute      (* 60    second)     (min minutes))
  (hour        (* 3600  second)     (hr hours))
  (day         (* 86400 second)     (days))
  (week        (* 604800 second)    (wk weeks))
  (fortnight   (* 1209600 second)   (fortnights))
  (month       (* 2629728 second)   (mon months))
  (year        (* 31556736 second)  (yr years))
  (century     (* 3155673600 second) (centuries))))

(import-novak-derived 
 ((second-squared (* second second) (s2 s^2))))

(import-novak-derived 
 ((hertz    (/ 1 second) (hz))
  (becquerel (/ 1 second) (bq))
  (kilohertz   (* kilo hertz)       (khz))
  (megahertz   (* mega hertz)       (mhz))
  (gigahertz   (* giga hertz)       (ghz))
  (terahertz   (* tera hertz)       (thz))
  (curie       (* 3.7e10 becquerel) (curies))))

(import-novak-simple ampere
		     ((ampere      1.0       (A amp amps amperes)) ))

(import-novak-derived 
 ((earth-gravity (* 9.80665 (/ meter (* second second))))
  (gravity (* 9.80665 (/ meter (* second second))))
  (feet-per-second-squared (/ foot (* second second))
			   (foot-per-second-squared ft/s/s ft/sec/sec foot/sec/sec
						    ft/s2 ft/sec2 foot/second/second))
  (meters-per-second-squared (/ meter (* second second))
			     (meter-per-second-squared m/s/s m/sec/sec m/second/second
						       m/s2 m/sec2 meter/sec/sec meter/second/second))
	       (centimeters-per-second-squared
		(/ centimeter (* second second))
		(centimeter-per-second-squared cm/s/s cm/sec/sec cm/s2
					       cm/sec2))))

(import-novak-derived
 ((milliampere (* milli ampere)
	       (milliamp milliamps ma milliampere))
  (microampere (* micro ampere)
	       (microamp microamps ua microamperes))
  (abampere    (* 10 ampere) (abamp abamperes))
  (statampere  (* 3.336e-10 ampere) (statamp statamperes))))

(import-novak-derived
 ((volt      (/ (* kilogram meter meter)
		(* ampere second second second))
	     (v volts))
  (millivolt (* milli volt)  (mv millivolts))
  (microvolt (* micro volt)  (uv microvolts))
  (abvolt    (* 1.0e-8 volt) (abvolts))
  (statvolt  (* 299.8 volt)  (statvolts))))

(import-novak-derived
 ((ohm      (/ (* kilogram meter meter)
	       (* ampere ampere second second second))
	    (ohms))
  (kilohm   (* kilo ohm)     (kilohms))
  (megohm   (* mega ohm)     (megohms))
  (abohm    (* nano ohm)     (abohms))
  (statohm  (* 8.987e11 ohm) (statohms))))

(import-novak-derived
 ((siemens      (/ (* ampere ampere second second second)
		   (* kilogram meter meter)) (mho) ) ))

(import-novak-derived
 ((farad   (/ (* ampere ampere second second second second)
	      (* kilogram meter meter))
	   (farads))
  (microfarad (* micro farad)     (uf microfarads))
  (picofarad  (* pico farad)      (pf picofarads))
  (abfarad    (* giga farad)      (abfarads))
  (statfarad  (* 1.113e-12 farad) (statfarads)) ))

(import-novak-derived
 ((henry      (/ (* kilogram meter meter)
		 (* ampere ampere second second))
	      (henrys))
  (millihenry (* milli henry)    (mh millihenrys))
  (microhenry (* micro henry)    (uh microhenrys))
  (abhenry    (* nano henry)     (abhenrys))
  (stathenry  (* 8.987e11 henry) (stathenrys)) ))

(import-novak-derived
 ((weber      (/ (* kilogram meter meter)
		 (* ampere second second))
	      (wb webers))
  (maxwell    (* 1.0e-8 weber)  (maxwells))))

(import-novak-derived
 ((tesla      (/ kilogram (* ampere second second))
	      (teslas T))
  (gauss      (* 1.0e-4 tesla) ())
  (milligauss (* milli gauss)  ()) ))

(import-novak-simple kelvin
		     ((degree-kelvin      1.0       (k kelvin kelvins degreeK))
		      (degree-rankine     5/9       (rankine))))

(import-novak-simple candela
		     ((candela            1.0       (cd candelas))))

(import-novak-simple mole
		     ((mole               1.0       (mol moles))))

(import-novak-derived
 ((pound-force  (/ (* slug foot) (* second second)) (lbf))
  (ounce-force  (/ pound-force 16)        ())
  (newton (/ (* kilogram meter) (* second second))
	  (N nt newtons))
  (dyne   (/ (* gram centimeter) (* second second))
	  (dynes))
  (kilogram-force  (* kilogram earth-gravity)
		   (kgf kilogram-weight))
  (gram-weight     (* gram earth-gravity) (gram-force))))

(import-novak-derived
 ((ounce  (/ pound 16)
	  (oz ounces))
  (ton    (* 2000 pound)
	  (tons short-ton short-tons))
  (long-ton (* 2240 pound)
	    (tons long-ton long-tons))
  (hundredweight (* 100 pound) (hundredweights))
  (dram   (/ ounce 16) (drams))
  (grain  (/ dram 27.344) (grains))
  (troy-pound (* 0.373 kilogram) (troy-pounds))
  (troy-ounce (* 31.103 gram)
	      (troy-ounces ounce-troy ounces-troy))
  (pennyweight (* 1.555 gram) (pennyweights))
  (scruple (* 1.296 gram) (scruples))))

(import-novak-derived
 ((square-meter (* meter meter)
		(m^2 m2 meter-squared meters-squared 
		     metersquared square-meters))
  (square-centimeter (* centimeter centimeter)
		     (cm^2 centimetersquared
			   centimeters-squared
			   centimeter-squared
			   square-centimeters))
  (square-foot (* foot foot)
	       (ft^2 foot-squared feet-squared footsquared
		     feetsquared square-feet))
  (square-yard (* yard yard)
	       (yard^2 yard-squared yardsquared yards-squared
		       square-yards))
  (square-inch (* inch inch)
	       (in^2 inch-squared inchsquared inches-squared
		     square-inches))
  (hectare (* 10000 metersquared)
	   (hectares))
  (are     (* 100 metersquared) (ares))
  (acre (* 43560 footsquared)
	(acres))
  (square-mile (* mile mile)
	       (mile^2 mile-squared miles-squared milesquared
		       square-miles))
  (square-kilometer (* kilometer kilometer)
		    (km^2 kilometer-squared
			  kilometers-squared
			  kilometersquared
			  square-kilometers))
  (square-millimeter (* millimeter millimeter)
		     (mm^2 millimeter-squared
			   millimeters-squared
			   millimetersquared
			   square-millimeters))
  (square-micron (* micrometer micrometer)
		 (um^2 micrometer-squared
		       micrometers-squared
		       micron-squared microns-squared
		       micrometersquared
		       micronsquared square-microns))
  (barn (* 1.0e-28 metersquared) (barns))))

(import-novak-derived
 ((cubic-meter     (* meter meter meter)
		   (m^3 meter-cubed metercubed meters-cubed
			cubic-meters kiloliter kiloliters
			kilolitre))
  (cubic-centimeter (* centimeter centimeter centimeter)
		    (cm^3 centimeter-cubed centimeters-cubed
			  centimetercubed centimeterscubed
			  cubic-centimeters milliliter
			  milliliters ml cc
			  cubic-centimetre millilitre))
  (cubic-millimeter (* millimeter millimeter millimeter)
		    (mm^3 millimeter-cubed millimeters-cubed
			  millimetercubed millimeterscubed
			  cubic-millimeters cubic-millimetre))
  (cubic-micron     (* micron micron micron)
		    (micron-cubed microns-cubed
				  cubic-microns))
  (cubic-kilometer  (* kilometer kilometer kilometer)
		    (km^3 kilometer-cubed kilometers-cubed
			  kilometercubed kilometerscubed
			  cubic-kilometers cubic-kilometre))
  (cubic-inch       (* inch inch inch)
		    (in^3 inch-cubed inchcubed inchescubed
			  cubic-inches))
  (cubic-foot       (* foot foot foot)
		    (ft^3 foot-cubed footcubed feetcubed
			  cubic-feet))
  (cubic-yard       (* yard yard yard)
		    (yd^3 yard-cubed yardcubed yardscubed
			  yards-cubed cubic-yards))
  (cubic-mile      (* mile mile mile)
		   (mile^3 mile-cubed miles-cubed
			   cubic-miles))
  (acre-foot        (* acre foot)
		    (acrefoot acre-feet acrefeet))
  (liter           (* 0.001 metercubed)
		   (l liters litre cubic-decimeter
		      cubic-decimeters))
  (deciliter       (/ liter 10)
		   (dl deciliters decilitre))
  (centiliter      (/ liter 100) (cl centiliters centilitre))
  (dekaliter       (* liter 10)
		   (dekaliters decaliter decaliters decalitre
			       dekalitre))
  (hectoliter      (* 100 liter) (hectoliters hectolitre))
  (gallon          (* 3.785411784 liter) (gal gallons))
  (quart           (/ gallon 4) (qt quarts))
  (peck            (* 8 quart) (pecks))
  (bushel          (* 4 peck) (bushels))
  (fifth           (/ gallon 5) (fifths))
  (pint            (* 0.473 liter) (pt pints))
  (cup             (/ pint 2) (cups))
  (fluid-ounce     (* 0.029573 liter)
		   (floz fluidounce fluidounces fluid-ounces))
  (gill            (* 4 fluid-ounce) (gills))
  (fluidram        (* 3.5516 cubic-centimeter) (fluidrams))
  (minim           (* 0.059194 cubic-centimeter) (minims))
  (tablespoon      (/ fluidounce 2) (tbsp tablespoons))
  (teaspoon        (/ tablespoon 3) (tsp teaspoons))
  (barrel          (* 159 liter) (bbl))	; as in oil
  ))

(import-novak-derived
 ((watt       (/ (* kilogram meter meter)
		 (* second second second))
	      (w watts))
  (milliwatt  (* milli watt)
	      (mw milli-watt milli-watts))
  (microwatt  (* micro watt)
	      (uw micro-watt micro-watts))
  (kilowatt   (* kilo watt)
	      (kw kilowatts))
  (megawatt   (* mega watt)
	      (mw megawatts mega-watt mega-watts))
  (gigawatt   (* giga watt)
	      (gw gigawatts giga-watt giga-watts))
  (horsepower (* 550 (/ (* foot pound-force) second))
	      (hp)) ))

(import-novak-derived
 ((joule (/ (* kilogram meter meter) (* second second))
	 (j joules))
  (foot-pound (* foot pound-force)
	      (ftlb ft-lb footpound footpounds foot-pounds))
  (kilowatt-hour (* kilo watt hour)
		 (kwh kilowatthour kilowatthours
		      kilowatt-hours))
  (watt-hour (* watt hour)
	     (watthour watthours watt-hours))
  (horsepower-hour (* horsepower hour)
		   (hp-hour))
  (electron-volt (* 1.60217733e-19 joule)
		 (ev electronvolt electronvolts
		     electron-volts))
  (mev (* 1.60217733e-13 joule)
       (mega-electron-volts))
  (gev (* 1.60217733e-10 joule)
       (giga-electron-volts))
  (tev (* 1.60217733e-7 joule)
       (tera-electron-volts))
  (calorie (* 4.184 joule)
	   (cal calorie calories))
  (kilocalorie (* 4184.0 joule)
	       (kcal kilo-calorie kilo-calories))
  (british-thermal-unit (* 1055.056 joule)
			(btu btus britishthermalunit
			     britishthermalunits
			     british-thermal-units))
  (erg (* 1.0e-7 joule)
       (ergs))
  (gallon-gasoline (* 114100 BTU) (gallon-gas gal-gas))  ))

(import-novak-derived
 ((coulomb     (* ampere second)     (coul coulombs C))
  (microcoulomb (* micro coulomb)    (micro-coulomb uC))
  (nanocoulomb (* nano coulomb)      (nano-coulomb nC))
  (abcoulomb   (* 10.0 coulomb)      (abcoul abcoulombs))
  (statcoulomb (* 3.336e-10 coulomb) (statcoul statcoulombs))
  (amperehour  (* 3600.0  coulomb)   (amp-hour ampere-hour
					       amperehours ampere-hours)) ))

(import-novak-derived
 ((newton-per-coulomb (/ newton coulomb) (N/C newton/coulomb))
  (volts-per-meter    (/ volt meter)     (v/m volt/meter)) ))

(import-novak-derived
 ((pounds-per-square-inch (/ (* 144 pound-force) (* foot foot)) (psi))
  (pascal     (/ newton (* meter meter)) (pa))
  (kilopascal (* 1000.0 pascal) (kilo-pascal kpa kilopascals))
  (bar        (* 1.0e5 pascal)  (bars))
  (millibar   (* milli bar)     (millibars))
  (torr       (* (/ 101325 760) pascal) ())
  (dynes-per-square-centimeter (/ dyne (* centimeter centimeter)))
  (atmosphere (* 101325 pascal) (atm)) ))

(import-novak-derived
 ((miles-per-hour (/ mile hour) (mph mile-per-hour))
  (miles-per-second (/ mile second) (mile-per-second))
  (kilometers-per-hour (/ kilometer hour)
		       (kph kilometer-per-hour))
  (kilometers-per-second (/ kilometer second)
			 (kps kilometer-per-second))
  (feet-per-second (/ foot second)
		   (foot-per-second fps ft/s ft/sec foot/sec
				    ft/second foot/second))
  (meters-per-second (/ meter second)
		     (meter-per-second m/s m/sec m/second
				       meter/sec meter/second))
  (centimeters-per-second (/ centimeter second)
			  (centimeter-per-second cm/s cm/sec))
  (knot              (/ nautical-mile hour) (knots))
  (speed-of-light    (* 299792458 (/ meter second)))))

(import-novak-derived
 ((gray    (/ joule kilogram)   (gy))
  (sievert (/ joule kilogram)   (sv))
  (rad     (/ gray 100)         ())
  (rem     (/ sievert 100)      ())))

;;; constants as units, formula translator accepts them

(define-units
    gravitational-constant (6.6720e-11  (/ (* meter meter meter)
					   (* kilogram second second)))
    elementary-charge (1.6021892e-19 coulomb)
    electron-mass (9.109534e-31 kilogram))
