'Author: Nikhil Dhanji & Manoj Dhanji
'Date: Feb 06 2021
'Version: 1.0
'Description:
'This program provides an intuitive interface to assemble polynomial functions by creating terms (simple or complex) and linking them to create larger polynomials.
'A ploynomial is of the form a0x^n + a1x^n-1 + ....
'A simple term consists of a coefficient (real number) and an exponent (real number). 2(x)^2 and (x)^0.5 are examples of simple terms. A complex term on the other hand,
'consists of a trigonometric function that accepts an argument and is multiplied by a multiplier. Both the argument and the multiplier could be simple or complex.
'(x)^2 sin^2(cos(x)) is an example of a complex term.
'The user builds the terms and them assembles them to form a larger polynomial.
'The program then prompts for the lower and upper values of the domain to plot the function.

CONST SCALE_FACTOR = 16
CONST LAST_COL = 640
CONST LAST_ROW = 480
CONST MID_COL = LAST_COL / 2
CONST MID_ROW = LAST_ROW / 2
'CONST PI = 3.1415926535897932384626433832795
CONST DX = 0.0000009

CONST BLUE = 1
CONST GREEN = 2
CONST CYAN = 3
CONST WHITE = 7
CONST LIGHT_RED = 12

CONST INTERVAL = 1

TYPE SIMPLE
    coefficient AS _FLOAT
    exponent AS _FLOAT
END TYPE

TYPE COMPLEX
    multiplier_index AS INTEGER
    argument_index AS INTEGER
    function_index AS INTEGER
    exponent AS _FLOAT
END TYPE

TYPE TERM
    simple AS SIMPLE
    complex AS COMPLEX
    discriminator AS INTEGER
END TYPE

DIM SHARED FUNCTIONS(8) AS STRING
FUNCTIONS(1) = "sin"
FUNCTIONS(2) = "cos"
FUNCTIONS(3) = "tan"
FUNCTIONS(4) = "csc"
FUNCTIONS(5) = "sec"
FUNCTIONS(6) = "cot"
FUNCTIONS(7) = "log"
FUNCTIONS(8) = "exp"


'$DYNAMIC
DIM SHARED terms(0) AS TERM
'$DYNAMIC
DIM SHARED polynomial_n(0) AS _UNSIGNED INTEGER
DIM SHARED number_of_polynomial_n_terms AS INTEGER

'$DYNAMIC
DIM SHARED polynomial_d(0) AS _UNSIGNED INTEGER
DIM SHARED number_of_polynomial_d_terms AS INTEGER

DIM SHARED counter AS INTEGER
DIM SHARED lower_x AS _FLOAT
DIM SHARED higher_x AS _FLOAT
DIM area AS _FLOAT

DIM SHARED yes_no AS STRING
DIM derivative_yes_no AS STRING
DIM rational_yes_no AS STRING
DIM area_yes_no AS STRING

counter = 0

'Begin main
DO
    CLS
    _TITLE "Polynomial Functions"
    CALL DESCRIPTION

    PRINT "A rational expression is of the form p/q"
    INPUT "Would you like to create a rational expression (y/n): ", rational_yes_no
    number_of_polynomial_n_terms = 0
    number_of_polynomial_d_terms = 0

    IF PARSE_YES_NO(rational_yes_no) THEN
        PRINT: PRINT "Building numerator"
    END IF
    number_of_polynomial_n_terms = BUILD_POLYNOMIAL(polynomial_n())

    IF PARSE_YES_NO(rational_yes_no) THEN
        PRINT: PRINT "Building denominator"
        number_of_polynomial_d_terms = BUILD_POLYNOMIAL(polynomial_d())
    END IF

    CLS
    DISPLAY_TERMS terms(), counter
    PRINT
    PRINT "The polynomial (p) created:"
    DISPLAY_POLYNOMIAL terms(), polynomial_n(), number_of_polynomial_n_terms
    IF number_of_polynomial_d_terms <> 0 THEN
        PRINT "The polynomial (q) created:"
        DISPLAY_POLYNOMIAL terms(), polynomial_d(), number_of_polynomial_d_terms
    END IF

    PRINT
    PRINT "Select limits for the domain to plot the function"
    INPUT "Lower limit of x-value: ", lower_x
    DO
        INPUT "Higher limit of x-value: ", higher_x
    LOOP UNTIL lower_x < higher_x

    INPUT "Would you like to plot the derivative of the f(x) (y/n): ", derivative_yes_no

    INPUT "Would you like to calculate the area under the curve (y/n): ", area_yes_no

    CLS
    SCREEN 12
    DRAW_AXES
    'DISPLAY_POLYNOMIAL terms(), polynomial_n(), number_of_polynomial_n_terms
    DIM x AS _FLOAT
    DIM y AS _FLOAT


    ON ERROR GOTO err_handler
    'Prompt the user to select a zoom-in or zoom-out
    area = 0
    DIM y1 AS _FLOAT
    DIM y2 AS _FLOAT
    PRINT "Function: f(x)"
    FOR x = lower_x TO higher_x STEP DX
        y1 = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_n(), number_of_polynomial_n_terms, x)
        y2 = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_d(), number_of_polynomial_d_terms, x)
        y = y1 / y2
        PSET (x * SCALE_FACTOR + MID_COL, -(y) * SCALE_FACTOR + MID_ROW)
    NEXT

    IF PARSE_YES_NO(derivative_yes_no) THEN
        DELAY INTERVAL
        DIM delta AS DOUBLE
        delta = 0.00009
        COLOR LIGHT_RED: PRINT "Deriviate: f'(x)": COLOR WHITE

        FOR x = lower_x TO higher_x STEP DX
            y1 = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_n(), number_of_polynomial_n_terms, x) / CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_d(), number_of_polynomial_d_terms, x)
            y2 = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_n(), number_of_polynomial_n_terms, (x + delta)) / CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_d(), number_of_polynomial_d_terms, (x + delta))
            y = (y2 - y1) / delta
            PSET (x * SCALE_FACTOR + MID_COL, -(y) * SCALE_FACTOR + MID_ROW), LIGHT_RED
        NEXT
    END IF

    IF PARSE_YES_NO(area_yes_no) THEN
        DELAY INTERVAL
        FOR x = lower_x TO higher_x STEP DX
            ' y = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_n(), number_of_polynomial_n_terms, x)
            y1 = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_n(), number_of_polynomial_n_terms, x)
            y2 = CALCULATE_Y_FOR_POLYNOMIAL(terms(), polynomial_d(), number_of_polynomial_d_terms, x)
            y = y1 / y2
            area = area + (ABS(y) * DX)
            LINE (x * SCALE_FACTOR + MID_COL, MID_ROW)-(x * SCALE_FACTOR + MID_COL, -(y) * SCALE_FACTOR + MID_ROW), GREEN
        NEXT
        COLOR GREEN
        PRINT USING "Area: " + FMT$(area); area
        COLOR WHITE
    END IF
    INPUT "Run again (y/n): ", yes_no
    ERASE polynomial_n
    ERASE polynomial_d
LOOP WHILE PARSE_YES_NO(yes_no)
err_handler:
IF ERR > 0 THEN
    'PRINT ERR
    RESUME NEXT
END IF

END
'End main

'SUB ROUTINES BEGIN

SUB DESCRIPTION ()
    PRINT
    PRINT "******************************************************************************"
    PRINT "This program provides an intuitive interface to assemble polynomial functions "
    PRINT "by creating terms (simple or complex) and linking them to create larger       "
    PRINT "polynomials. A ploynomial is of the form a0x^n + a1x^n-1 + ....               "
    PRINT "A simple term consists of a (real) coefficient and a (real) exponent.         "
    PRINT "2x"; CHR$(253); " and "; CHR$(251); "x are examples of simple terms.          "
    PRINT "A complex term on the other hand, consists of a trigonometric function        "
    PRINT "that accepts an argument and is multiplied by a multiplier.                   "
    PRINT "Both the argument and the multiplier could be simple or complex.              "
    PRINT "x"; CHR$(253); "sin"; CHR$(253); "(cos(x)) is an example of a complex term.   "
    PRINT "The user builds the terms and assembles them to form a larger polynomial.     "
    PRINT "The program then prompts for the lower and upper values of the domain         "
    PRINT "to plot the polynomial  function.                                             "
    PRINT "******************************************************************************"
    PRINT
END SUB

SUB BUILD_DEFAULT_COMPLEX (terms() AS TERM, t AS TERM, counter AS INTEGER)
    DIM m_i AS INTEGER
    m_i = FIND_SIMPLE_MULTIPLIER(terms(), counter)
    DIM a_i AS INTEGER
    a_i = FIND_SIMPLE_ARGUMENT(terms(), counter)
    IF m_i <> -1 THEN
        t.complex.multiplier_index = m_i
    ELSE
        DIM m AS TERM
        m.discriminator = 1
        m.simple.coefficient = 1
        m.simple.exponent = 0
        counter = counter + 1
        REDIM _PRESERVE terms(counter) AS TERM
        terms(counter) = m
        t.complex.multiplier_index = counter
    END IF

    IF a_i <> -1 THEN
        t.complex.argument_index = a_i
    ELSE
        DIM a AS TERM
        a.discriminator = 1
        a.simple.coefficient = 1
        a.simple.exponent = 1
        counter = counter + 1
        REDIM _PRESERVE terms(counter) AS TERM
        terms(counter) = a
        t.complex.argument_index = counter
    END IF

END SUB

FUNCTION BUILD_POLYNOMIAL (polynomial() AS _UNSIGNED INTEGER)
    begin_process:
    DIM number_of_polynomial_terms AS INTEGER
    DIM i AS INTEGER
    yes_no = "Y"
    IF counter <> 0 THEN
        DISPLAY_TERMS terms(), counter
        INPUT "Continue creating more terms (y/n): ", yes_no
    END IF
    IF PARSE_YES_NO(yes_no) THEN
        DO
            DO
                INPUT "Create Simple or Complex term (1. Simple 2. Complex): ", i
            LOOP WHILE i <> 1 AND i <> 2
            DIM t AS TERM
            IF i = 1 THEN 'create a simple term
                t.discriminator = 1
                INPUT "Enter the coefficient for the simple term: ", t.simple.coefficient
                INPUT "Enter the exponent for the simple term: ", t.simple.exponent
            ELSE 'create a complex term
                DISPLAY_FUNCTIONS FUNCTIONS()
                DO
                    INPUT "Select a function from above (1 - 8): ", i
                LOOP WHILE i < 1 OR i > 8
                t.discriminator = 2
                t.complex.function_index = i

                IF counter = 0 THEN 'no existing terms on scratch pad
                    BUILD_DEFAULT_COMPLEX terms(), t, counter
                ELSE 'existing terms on scratch pad

                    PRINT "1. Create a complex term with a simple argument for function"
                    PRINT "2. Select a term from the ";
                    COLOR LIGHT_RED: PRINT "list": COLOR WHITE
                    PRINT
                    DO
                        INPUT "Your option (1 or 2): ", i
                    LOOP WHILE i < 1 OR i > 2
                    IF i = 2 THEN
                        DISPLAY_TERMS terms(), counter
                        DO
                            PRINT "Select a multiplier for your complex term"
                            INPUT "Select from the list by using index: ", i
                        LOOP WHILE i < 1 OR i > counter
                        t.complex.multiplier_index = i
                        DO
                            PRINT "Select an argument for the function for the complex term"
                            INPUT "Select from the list by using index: ", i
                        LOOP WHILE i < 1 OR i > counter
                        t.complex.argument_index = i
                    ELSE
                        BUILD_DEFAULT_COMPLEX terms(), t, counter
                    END IF
                END IF
                INPUT "Enter the exponent for the complex term: ", t.complex.exponent
            END IF
            counter = counter + 1
            REDIM _PRESERVE terms(counter) AS TERM
            terms(counter) = t
            DISPLAY_TERMS terms(), counter
            INPUT "Continue creating more terms (y/n): ", yes_no
            PRINT
        LOOP WHILE PARSE_YES_NO(yes_no)
    END IF
    CLS
    DISPLAY_TERMS terms(), counter
    DO
        DO
            INPUT "Select the terms to construct the polynomial (using index): ", i
        LOOP WHILE i < 1 OR i > counter
        number_of_polynomial_terms = number_of_polynomial_terms + 1
        REDIM _PRESERVE polynomial(number_of_polynomial_terms) AS _UNSIGNED INTEGER
        polynomial(number_of_polynomial_terms) = i
        INPUT "Add more terms to the polynomial (y/n): ", yes_no
    LOOP WHILE PARSE_YES_NO(yes_no)
    BUILD_POLYNOMIAL = number_of_polynomial_terms
END SUB

SUB DISPLAY_FUNCTIONS (t() AS STRING)
    DIM i AS INTEGER
    FOR i = 1 TO 8
        PRINT " ["; i; "]: "; t(i)
    NEXT i
END SUB

SUB DISPLAY_POLYNOMIAL (terms() AS TERM, polynomial() AS _UNSIGNED INTEGER, number_of_polynomial_terms AS INTEGER)
    DIM n AS INTEGER
    'COLOR LIGHT_RED
    PRINT "f(x) = ";
    FOR i = 1 TO number_of_polynomial_terms
        n = polynomial(i)
        IF terms(n).discriminator = 1 THEN
            DISPLAY_SIMPLE_TERM terms(), n
        ELSE
            CALL DISPLAY_COMPLEX_TERM(terms(), n)
        END IF
        IF i < number_of_polynomial_terms THEN PRINT " + ";
    NEXT
    'COLOR WHITE
    PRINT
END SUB

SUB DISPLAY_TERMS (terms() AS TERM, size AS INTEGER)
    IF size > 0 THEN
        COLOR LIGHT_RED
        PRINT "List of terms:":
        FOR i = 1 TO size
            DISPLAY_TERM terms(), i
        NEXT
        PRINT
        COLOR WHITE
    END IF
END SUB

SUB DISPLAY_TERM (terms() AS TERM, i AS INTEGER)
    IF terms(i).discriminator = 1 THEN
        PRINT " ["; i; "] Simple : ->";
        DISPLAY_SIMPLE_TERM terms(), i
        PRINT
    ELSE
        PRINT " ["; i; "] Complex: ->";
        CALL DISPLAY_COMPLEX_TERM(terms(), i)
        PRINT
    END IF
END SUB

SUB DISPLAY_SIMPLE_TERM (t() AS TERM, i AS INTEGER)
    IF t(i).simple.coefficient = 0 THEN
        PRINT " 0";
    ELSEIF t(i).simple.exponent = 0 THEN
        PRINT t(i).simple.coefficient;
    ELSEIF t(i).simple.exponent = 1 THEN
        IF t(i).simple.coefficient = 1 THEN
            PRINT " x ";
        ELSE
            PRINT t(i).simple.coefficient; "x";
        END IF
    ELSE
        PRINT t(i).simple.coefficient; "x ^"; t(i).simple.exponent;
    END IF
END SUB

SUB DISPLAY_COMPLEX_TERM (t() AS TERM, i AS INTEGER)
    DIM multiplier_index AS INTEGER
    DIM argument_index AS INTEGER
    DIM function_index AS INTEGER
    DIM exponent AS _FLOAT

    multiplier_index = t(i).complex.multiplier_index
    argument_index = t(i).complex.argument_index
    function_index = t(i).complex.function_index
    exponent = t(i).complex.exponent
    IF t(multiplier_index).discriminator = 1 THEN
        DISPLAY_SIMPLE_TERM t(), multiplier_index
    ELSE
        DISPLAY_COMPLEX_TERM t(), multiplier_index
    END IF

    IF exponent = 1 THEN
        PRINT FUNCTIONS(function_index); "(";
    ELSE
        PRINT FUNCTIONS(function_index); " ^"; exponent; "(";
    END IF
    IF t(argument_index).discriminator = 1 THEN
        DISPLAY_SIMPLE_TERM t(), argument_index
    ELSE
        DISPLAY_COMPLEX_TERM t(), argument_index
    END IF
    PRINT ")";
END SUB

SUB DRAW_AXES
    DIM x AS INTEGER
    DIM y AS INTEGER
    FOR x = 0 TO LAST_COL STEP SCALE_FACTOR
        IF x = MID_COL THEN
            LINE (x, 0)-(x, LAST_ROW), 7
        ELSE
            LINE (x, 0)-(x, LAST_ROW), 8
        END IF
    NEXT x
    FOR y = 0 TO LAST_ROW STEP SCALE_FACTOR
        IF y = MID_ROW THEN
            LINE (0, y)-(LAST_COL, y), 7
        ELSE
            LINE (0, y)-(LAST_COL, y), 8
        END IF
    NEXT y
END SUB

DECLARE SUB DELAY (duration!)
SUB DELAY (duration AS SINGLE)
    tim = TIMER
    DO
    LOOP UNTIL (TIMER - tim + 86400) - (INT((TIMER - tim + 86400) / 86400) * 86400) > duration
END SUB
'SUB ROUTINES END

'FUNCTION BEGIN
FUNCTION CALCULATE_Y_FOR_POLYNOMIAL (terms() AS TERM, polynomial() AS _UNSIGNED INTEGER, number_of_polynomial_terms AS INTEGER, x AS _FLOAT)
    DIM y AS _FLOAT
    y = 0
    'PRINT "number_of_polynomial_terms"; number_of_polynomial_terms
    FOR i = 1 TO number_of_polynomial_terms
        y = y + CALCULATE_Y_FOR_TERM(terms(), polynomial(i), x)
    NEXT
    CALCULATE_Y_FOR_POLYNOMIAL = y
END FUNCTION

FUNCTION CALCULATE_Y_FOR_TERM (terms() AS TERM, i AS INTEGER, x AS _FLOAT)
    'PRINT "terms(i).discriminator"; terms(i).discriminator
    IF terms(i).discriminator = 1 THEN
        CALCULATE_Y_FOR_TERM = CALCULATE_Y_FOR_SIMPLE_TERM(terms(), i, x)
    ELSE
        CALCULATE_Y_FOR_TERM = CALCULATE_Y_FOR_COMPLEX_TERM(terms(), i, x)
    END IF
END FUNCTION

FUNCTION CALCULATE_Y_FOR_SIMPLE_TERM (t() AS TERM, i AS INTEGER, x AS _FLOAT)
    CALCULATE_Y_FOR_SIMPLE_TERM = t(i).simple.coefficient * x ^ t(i).simple.exponent
END FUNCTION

FUNCTION CALCULATE_Y_FOR_COMPLEX_TERM (t() AS TERM, i AS INTEGER, x AS _FLOAT)
    DIM multiplier_index AS INTEGER
    DIM argument_index AS INTEGER
    DIM function_index AS INTEGER
    DIM exponent AS _FLOAT
    DIM multiplier AS _FLOAT
    DIM argument AS _FLOAT
    DIM function_value AS _FLOAT
    multiplier_index = t(i).complex.multiplier_index
    argument_index = t(i).complex.argument_index
    function_index = t(i).complex.function_index
    exponent = t(i).complex.exponent
    'PRINT "multiplier_index"; multiplier_index; "argument_index"; argument_index; "function_index"; function_index

    IF t(multiplier_index).discriminator = 1 THEN
        multiplier = CALCULATE_Y_FOR_SIMPLE_TERM(t(), multiplier_index, x)
    ELSE
        multiplier = CALCULATE_Y_FOR_COMPLEX_TERM(t(), multiplier_index, x)
    END IF

    IF t(argument_index).discriminator = 1 THEN
        argument = CALCULATE_Y_FOR_SIMPLE_TERM(t(), argument_index, x)
    ELSE
        argument = CALCULATE_Y_FOR_COMPLEX_TERM(t(), argument_index, x)
    END IF

    SELECT CASE function_index
        CASE 1:
            function_value = SIN(argument) ^ exponent
        CASE 2:
            function_value = COS(argument) ^ exponent
        CASE 3:
            function_value = TAN(argument) ^ exponent
        CASE 4:
            function_value = 1 / SIN(argument) ^ exponent
        CASE 5:
            function_value = 1 / COS(argument) ^ exponent
        CASE 6:
            function_value = 1 / TAN(argument) ^ exponent
        CASE 7
            function_value = LOG(argument)
        CASE 8:
            function_value = EXP(argument)
    END SELECT
    'PRINT "function_value"; function_value; "multiplier"; multiplier; "argument"; argument
    CALCULATE_Y_FOR_COMPLEX_TERM = multiplier * function_value
END FUNCTION

FUNCTION PARSE_YES_NO (s AS STRING)
    IF UCASE$(MID$(s, 1, 1)) = "Y" THEN
        PARSE_YES_NO = 1
    ELSE
        PARSE_YES_NO = 0
    END IF
END FUNCTION

FUNCTION FIND_SIMPLE_MULTIPLIER (terms() AS TERM, counter AS INTEGER)
    DIM index AS INTEGER
    index = -1
    FOR i = 0 TO counter
        IF terms(i).discriminator = 1 THEN
            IF terms(i).simple.coefficient = 1 AND terms(i).simple.exponent = 0 THEN
                index = i
                GOTO RETURN_LABEL
            END IF
        END IF
    NEXT
    RETURN_LABEL:
    FIND_SIMPLE_MULTIPLIER = index
END FUNCTION

FUNCTION FIND_SIMPLE_ARGUMENT (terms() AS TERM, counter AS INTEGER)
    DIM index AS INTEGER
    index = -1
    FOR i = 0 TO counter
        IF terms(i).discriminator = 1 THEN
            IF terms(i).simple.coefficient = 1 AND terms(i).simple.exponent = 1 THEN
                index = i
                GOTO RETURN_LABEL
            END IF
        END IF
    NEXT
    RETURN_LABEL:
    FIND_SIMPLE_ARGUMENT = index
END FUNCTION

FUNCTION FMT$ (d AS _FLOAT)
    ' Beginning of fmt
    DIM i AS INTEGER
    DIM n AS INTEGER
    DIM s AS STRING
    s = RTRIM$(LTRIM$(STR$(ABS(d))))
    i = INSTR(1, s, ".")
    IF i = 0 THEN
        n = LEN(s)
    ELSE
        n = LEN(MID$(s, 1, i - 1))
    END IF
    s = ""
    FOR i = 1 TO n
        s = s + "#"
    NEXT
    FMT$ = "" + s + ".##"
    'Ending of fmt
END FUNCTION
'FUNCTION END

