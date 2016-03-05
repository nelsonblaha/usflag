
DECLARE FUNCTION drawunion% ()
DECLARE SUB drawunions ()
DEFSTR A-Z
' Define constants
    CONST pi = 3.141592653589793#, fifthpi = pi / 5#
    CONST true = -1, false = 0
'   Constants in units of pixels
      CONST unionfly = 639#                           'Union's fly
      CONST unionhoist = 639# * 7# / 13# / 1.9# / .4# 'Union's hoist
      CONST rout48 = 639# * .0308# / 1.9# / .4#       'Star's outer radius in
                                                      '    48-star flag
' Declare shared variables
    DIM SHARED k         AS DOUBLE 'Ratio of star's inner to outer radii
               k = ((SQR(5#) - 1) / 2) ^ 2 '= phi ^ -2
    DIM SHARED margin    AS DOUBLE
    DIM SHARED n         AS INTEGER
    DIM SHARED rin       AS DOUBLE
    DIM SHARED rout      AS DOUBLE
    DIM SHARED rows      AS INTEGER
    DIM SHARED version   AS STRING
    DIM SHARED xstart(1) AS DOUBLE
    DIM SHARED xstep     AS DOUBLE
' Declare local variables
    DIM cols  AS INTEGER
    DIM tempi AS INTEGER
  SCREEN 12
  n = 50
  DO
    DO
'     Draw unions with n stars
        version = ""
'       Compute the star radius and the union margins
          rout = rout48 * SQR(48# / CDBL(n))
          rin = rout * k
          margin = rout * 1.5#
'       Draw unions with the same number of stars in each row and with columns
'           aligned from row to row
          FOR rows = 1 TO n
            IF n MOD rows = 0 THEN
              cols = n \ rows
              IF cols = 1 THEN
                xstep = unionfly
                xstart(1) = unionfly / 2#
                xstart(0) = xstart(1)
              ELSE
                xstep = (unionfly - margin * 2#) / CDBL(cols - 1)
                xstart(1) = margin
                xstart(0) = margin
              END IF
              IF drawunion THEN EXIT DO
            END IF
          NEXT rows
'       Draw unions with the same number of stars in each row and with columns
'           staggered from row to row
          FOR rows = 2 TO n
            IF n MOD rows = 0 THEN
              cols = n \ rows
              xstep = (unionfly - margin * 2#) / (CDBL(cols) - .5#)
              xstart(1) = margin
              xstart(0) = margin + xstep / 2#
              IF drawunion THEN EXIT DO
            END IF
          NEXT rows
'       Draw unions with one fewer star in the second and every other row
          FOR rows = 2 TO n
            IF (n + rows \ 2) MOD rows = 0 THEN
              cols = (n + rows \ 2) \ rows
              xstep = (unionfly - margin * 2#) / CDBL(cols - 1)
              xstart(1) = margin
              xstart(0) = margin + xstep / 2#
              IF drawunion THEN EXIT DO
            END IF
          NEXT rows
'       Draw unions with one more star in the second and every other row
          FOR rows = 2 TO n
            IF (n - rows \ 2) MOD rows = 0 THEN
              cols = (n - rows \ 2) \ rows
              xstep = (unionfly - margin * 2#) / CDBL(cols)
              xstart(1) = margin + xstep / 2#
              xstart(0) = margin
              IF drawunion THEN EXIT DO
            END IF
          NEXT rows
        IF version = "" THEN rows = 0: tempi = drawunion
    LOOP
  LOOP

DEFINT A-Z
SUB documentation
'
' http://en.wikisource.org/wiki/Executive_Order_1637
'
'   EXECUTIVE ORDER, 1637, OCTOBER 29, 1912
'
'   The Executive Order of June 24, 1912, is hereby revoked, and for it is
'   substituted the following:
'
'   Whereas, "An Act to Establish the Flag of the United States," approved on
'   the 4th of 'April, 1818, reading as follows:
'
'   "Section 1. Be it enacted, etc., That from and after the fourth day of
'   July next, the flag of the United States be thirteen horizontal strips,
'   alternate red and white; that the union have twenty stars, white in a blue
'   field.
'
'   "Section 2. Be it further enacted, That on the admission of every new
'   State to the Union, one star be added to the union of the flag; and that
'   such addition shall take effect on the fourth of July next succeeding such
'   admission,"
'
'   fails to establish proportions; and
'
'   Whereas investigation shows some sixty-six different sizes of National
'   flags, and of varying proportions, in use in the Executive Departments;
'
'   It is hereby ordered that National Flags and Union Jacks for all
'   Departments of the Government, with the exceptions noted under (a), shall
'   conform to the follow [sic] proportions:
'
'       Fly (length) of Flag........... 1.9
'       Hoist (width) of Flag.......... 1
'       Hoist (width) of Union......... 7/13
'       Fly (length) of Union.......... .76 [that is, .4 x 1.9]
'       Width of each stripe........... 1/13
'
'   (a.) Exception: The colors carried by troops, and camp colors, shall be
'   the sizes prescribed for the military service (Army and Navy).
'
'   Limitation of the number of sizes: With exception of colors under note
'   (a), the sizes of flags 'manufactured or purchased for Government
'   Departments will be limited to those with the following hoists:
'
'       (1)............ 20.00 feet. (7)............. 5.14 feet.
'       (2)..(standard) 19.00 feet. (8)............. 5.00 feet.
'       (3)............ 14.35 feet. (9)............. 3.52 feet.
'       (4)............ 12.19 feet. (10)............ 2.90 feet.
'       (5)............ 10.00 feet. (11)............ 2.37 feet.
'       (6)............  8.94 feet. (12)............ 1.31 feet.
'
'   Union Jacks: The size of the Jack shall be the size of the Union of the
'   National Flag with which it is flown.
'
'   Position and size of stars: The position and size of each star for the
'   union of the flag shall be as indicated on a plan which will be furnished
'   to the Departments by the Navy Department. From this plan can be
'   determined the location and size of stars for flags of any dimensions.
'   Extra blueprints of this plan will be furnished upon application to the
'   Navy Department.
'
'   Order effective: All National Flags and Union Jacks now on hand or for
'   which contracts have been awarded shall be continued in use until
'   unserviceable, but all those manufactured or purchased for government use
'   after the date of this order shall conform strictly to the dimensions and
'   proportions herein prescribed.
'
'   Boat Flags: In order that that identity of the stars in flags when carried
'   by small boats belonging to the Government may be preserved, the custom
'   holding in the Navy for many years, of thirteen (13) stars for boat flags,
'   is hereby approved.
'
'   President's Flag: The color of the field of the President's flag shall be
'   blue.
'
'   WM. H. TAFT.
'
'
' http://en.wikisource.org/wiki/File:1912_US_Flag_specification_1.jpg
'
'   [Flag dimensions are defined by a diagram in the specification:]
'
'   A hoist
'   B fly
'   C horizontal distance of star center from union edge
'   D A-E
'   E union hoist
'   F union fly
'   G horizontal spacing of star centers
'   H vertical distance of star center from union edge
'   I vertical spacing of star centers
'   J diameter of circle circumscribing star
'   K width of stripe
'
'   ENSIGN   A     B      G      D     E      F      C     H     I
'   No  2  19.00 36.100 1.805  8.769 10.230 14.440 .9025 .8501 1.7050
'    "  3  14.34 27.274 1.2382 6.618  7.721 10.906 .6191 .6416 1.2809
'    "  4  12.19 23.161 1.158  5.625  6.5639 9.264 .5790 .5454 1.0889
'    "  6   8.94 16.986  .848  4.125  4.813  6.794 .424  .3999  .7984
'    "  7   5.14  9.766  .488  2.372  2.767  3.906 .2441 .2299  .4590
'
'   ENSIGN    J      K
'   No  2  1.1703 1.4615
'    "  3   .8832 1.103
'    "  4   .7509  .9377  [Table wrapped for lack of space]
'    "  6   .5506  .687
'    "  7   .3165  .395
'
'
' http://en.wikisource.org/wiki/Executive_Order_10798
'
'   Executive Order 10798
'
'   FLAG OF THE UNITED STATES
'
'   WHEREAS the State of Alaska has this day been admitted to the Union; and
'
'   WHEREAS chapter 1 of title 4 of the United States Code provides that a
'   star shall be added to the union of the flag of the United States upon the
'   admission of a new State into the Union and provides that that addition to
'   the flag shall take effect on the fourth day of July then next succeeding
'   the admission of that State; and
'
'   WHEREAS the interests of the Government require that orderly and
'   reasonable provision be made for certain features of the flag:
'
'   NOW, THEREFORE, under and by virtue of the authority vested in me as
'   President of the United States and as Commander-in-Chief of the armed
'   forces of the United States, it is hereby ordered as follows:
'
'   SECTION 1. Proportions.
'
'   National flags and union jacks for all departments and other agencies of
'   the executive branch of the Government (hereinafter referred to as
'   executive agencies) shall conform to the following proportions:
'
'     Hoist (width) of flag..............1
'     Fly (length) of flag...............1.9
'     Hoist (width) of union.............0.5385 (7/13)
'     Fly (length) of union..............0.76
'     Width of each stripe...............0.0769 (1/13)
'
'   Such further proportions as are set forth on the attachment hereto. That
'   attachment is hereby made a part of this order.
'
'   SEC. 2. Sizes.
'
'   (a) Flags manufactured or purchased for executive agencies shall be
'   limited to those having hoists as follows:
'
'     (1).............. 20 feet
'     (2).............. 19 feet
'     (3).............. 14.35 feet
'     (4).............. 12.19 feet
'     (5).............. 10 feet
'     (6).............. 8.94 feet
'     (7).............. 5.14 feet
'     (8).............. 5 feet
'     (9).............. 3.52 feet
'     (10)............. 2.90 feet
'     (11)............. 2.37 feet
'     (12)............. 1.31 feet
'
'   (b) Union jacks manufactured or purchased for executive agencies shall be
'   limited to those the hoists of which correspond to the hoists of the
'   unions of flags of sizes herein authorized. The size of the union jack
'   flown with the national flag shall be the same as the size of the union of
'   that national flag.
'
'   SEC. 3. Position of stars.
'
'   The position of each star of the union of the flag, and of the union jack,
'   shall be as indicated on the attachment hereto.
'
'   SEC. 4. Public inquiries.
'
'   Interested persons may direct inquiries concerning this order to the
'   Quartermaster General of the Army. Inquiries relating to the procurement
'   of national flags by executive agencies other than the Department of
'   Defense may be directed to the General Services Administration.
'
'   SEC. 5. Applicability; prior flag and jack'. [sic]
'
'   (a) All national flags and union jacks manufactured or purchased for the
'   use of executive agencies after the date of this order shall conform
'   strictly to the provisions of sections 1 to 3, inclusive, of this order.
'
'   (b) The colors carried by troops, and camp colors, shall be of the sizes
'   prescribed by the Secretary of Defense for the armed forces of the United
'   States and the sizes of those colors shall not be subject to the
'   provisions of this order.
'
'   (c) Subject to such limited exceptions as the Secretary of Defense, in
'   respect of the Department of Defense, and the Administrator of General
'   Services, in respect of executive agencies other than the Department of
'   Defense, may approve, all national flags and union jacks now in the
'   possession of executive agencies, or hereafter acquired, under contracts
'   awarded prior to the date of this order, by executive agencies, including
'   those so possessed or so acquired by the General Services Administration
'   for distribution to other executive agencies, shall be utilized until
'   unserviceable.
'
'   SEC. 6.
'
'   The flag prescribed by this order shall become the official flag under
'   chapter 1 of title 4 of the United States Code as of July 4, 1959.
'
'   SEC. 7. Revocation.
'
'   Executive Order No. 2390 of May 29, 1916, is hereby revoked.
'
'   SEC. 8.
'
'   This order shall be published in the FEDERAL REGISTER.
'
'   Dwight D. Eisenhower
'   The White House,
'   January 3, 1959.
'
'
' http://en.wikisource.org/wiki/File:1959_US_Flag_specification.jpg
'
'   [Flag dimensions are defined by a diagram in the specification:]
'
'   A hoist
'   B fly
'   C union hoist
'   D union fly
'   E vertical distance of star center from union edge
'   F vertical spacing of star centers
'   G horizontal distance of star center from union edge
'   H horizontal spacing of star centers
'   K diameter of circle circumscribing star
'   L width of stripe ("A" DIVIDED INTO 13 EQUAL PARTS "L")
'
'                       STANDARD PROPORTIONS AND SIZES
'        HOIST             FLY               HOIST              FLY
'   (WIDTH) OF FLAG  (LENGTH) OF FLAG  (WIDTH) OF UNION  (LENGTH) OF UNION
'         1.0              1.9           0.5385 (7/13)         0.76
'          A                B                  C                 D
'
'                           DIAMETER OF STAR  WIDTH OF STRIPE  [Table wrapped
'   0.056 0.071 0.054 0.05      0.0616        0.0769 (1/13)     for lack
'     E     F     G     H          K                L           of space]
'
'
' http://en.wikisource.org/wiki/Executive_Order_10798
'
'   Executive order 10834
'
'   The Flag of the United States
'
'   WHEREAS the State of Hawaii has this day been admitted into the Union; and
'
'   WHEREAS section 2 of title 4 of the United States Code provides as
'   follows: "On the admission of a new State into the Union one star shall be
'   added to the union of the flag; and such addition shall take effect on the
'   fourth day of July then next succeeding such admission."; and
'
'   WHEREAS the Federal Property and Administrative Services Act of 1949 (63
'   Stat. 377), as amended, authorizes the President to prescribe policies and
'   directives governing the procurement and utilization of property by
'   executive agencies; and
'
'   WHEREAS the interests of the Government require that orderly and
'   reasonable provision be made for various matters pertaining to the flag
'   and that appropriate regulations governing the procurement and utilization
'   of national flags and union jacks by executive agencies be prescribed:
'
'   NOW, THEREFORE, by virtue of the authority vested in me as President of
'   the United States and as Commander in Chief of the armed forces of the
'   United States, and the Federal Property and Administrative Services Act of
'   1949, as amended, it is hereby ordered as follows:
'
'   PART I--DESIGN OF THE FLAG
'
'   SECTION 1. The flag of the United States shall have thirteen horizontal
'   stripes, alternate red and white, and a union consisting of white stars on
'   a field of blue.
'
'   SEC. 2. The positions of the stars in the union of the flag and in the
'   union jack shall be as indicated on the attachment to this order, which is
'   hereby made a part of this order.
'
'   SEC. 3. The dimensions of the constituent parts of the flag shall conform
'   to the proportions set forth in the attachment referred to in section 2 of
'   this order.
'
'   PART II--REGULATIONS GOVERNING EXECUTIVE AGENCIES
'
'   SEC. 21. The following sizes of flags are authorized for executive
'   agencies:
'
'     Size             Dimensions of Flag
'                  Hoist (width)  Fly (length)
'                      Feet           Feet
'     (1) ........... 20.00          38.00
'     (2) ........... 10.00          19.00
'     (3) ............ 8.95          17.00
'     (4) ............ 7.00          11.00
'     (5) ............ 5.00           9.50
'     (6) ............ 4.33           5.50
'     (7) ............ 3.50           6.65
'     (8) ............ 3.00           4.00
'     (9) ............ 3.00           5.70
'     (10) ........... 2.37           4.50
'     (11) ........... 1.32           2.50
'
'   SEC. 22. Flags manufactured or purchased for the use of executive
'   agencies:
'
'   (a) Shall conform to the provisions of Part I of this order, except as may
'   be otherwise authorized pursuant to the provisions of section 24, or
'   except as otherwise authorized by the provisions of section 21, of this
'   order.
'
'   (b) Shall conform to the provisions of section 21 of this order, except as
'   may be otherwise authorized pursuant to the provisions of section 24 of
'   this order.
'
'   SEC. 23. The exterior dimensions of each union jack manufactured or
'   purchased for executive agencies shall equal the respective exterior
'   dimensions of the union of a flag of a size authorized by or pursuant to
'   this order. The size of the union jack flown with the national flag shall
'   be the same as the size of the union of that national flag.
'
'   SEC. 24. (a) The Secretary of Defense in respect of procurement for the
'   Department of Defense (including military colors) and the Administrator of
'   General Services in respect of procurement for executive agencies other
'   than the Department of Defense may, for cause which the Secretary or the
'   Administrator, as the case may be, deems sufficient, make necessary minor
'   adjustments in one or more of the dimensions or proportionate dimensions
'   prescribed by this order, or authorize proportions or sizes other than
'   those prescribed by section 3 or section 21 of this order.
'
'   (b) So far as practicable, (1) the actions of the Secretary of Defense
'   under the provisions of section 24(a) of this order, as they relate to the
'   various organizational elements of the Department of Defense, shall be
'   coordinated, and (2) the Secretary and the Administrator shall mutually
'   coordinate their actions under that section.
'
'   SEC. 25. Subject to such limited exceptions as the Secretary of Defense in
'   respect of the Department of Defense, and the Administrator of General
'   Services in respect of executive agencies other than the Department of
'   Defense, may approve, all national flags and union jacks now in the
'   possession of executive agencies, or hereafter acquired by executive
'   agencies under contracts awarded prior to the date of this order,
'   including those so possessed or so acquired by the General Services
'   Administration, for distribution to other agencies, shall be utilized
'   until unserviceable.
'
'   PART III--GENERAL PROVISIONS
'
'   SEC. 31. The flag prescribed by Executive Order No. 10798 of January 3,
'   1959, shall be the official flag of the United States until July 4, 1960,
'   and on that date the flag prescribed by Part I of this order shall become
'   the official flag of the United States; but this section shall neither
'   derogate from section 24 or section 25 of this order nor preclude the
'   procurement, for executive agencies, of flags provided for by or pursuant
'   to this order at any time after the date of this order.
'
'   SEC. 32. As used in this order, the term "executive agencies" means the
'   executive departments and independent establishments in the executive
'   branch of the Government, including wholly-owned Government corporations.
'
'   SEC. 33. Executive Order No. 10798 of January 3, 1959, is hereby revoked.
'
'   Dwight D. Eisenhower The White House,
'   Washington, D.C.
'   August 21, 1959.
'
'
' http://en.wikisource.org/wiki/File:1960_US_Flag_specification.jpg
'
'   [Flag dimensions are defined by a diagram in the specification:]
'
'   A hoist
'   B fly
'   C union hoist
'   D union fly
'   E vertical distance of star center from union edge
'   F vertical spacing of star centers
'   G horizontal distance of star center from union edge
'   H horizontal spacing of star centers
'   K diameter of circle circumscribing star
'   L width of stripe ("A" DIVIDED INTO 13 EQUAL PARTS "L")
'
'                       STANDARD PROPORTIONS AND SIZES
'        HOIST             FLY               HOIST              FLY
'   (WIDTH) OF FLAG  (LENGTH) OF FLAG  (WIDTH) OF UNION  (LENGTH) OF UNION
'         1.0              1.9           0.5385 (7/13)         0.76
'          A                B                  C                 D
'
'                           DIAMETER OF STAR  WIDTH OF STRIPE  [Table wrapped
'   0.054 0.074 0.063 0.063      0.0616        0.0769 (1/13)    for lack of
'     E     F     G     H           K                L          space]
'
' Taft's 1912 Executive Order gives most of the flag proportions explicitly.
' The size of the stars is omitted.  However the associated 48-star-flag
' specification calls the flag's hoist A and the diameter of the circle
' circumscribing each star J.  The specification instantiates J for 5
' different standard flags.
'
' (Note that the values for the redundant measurement D do not agree exactly
' with those for A and E in the table.  Furthermore, the values for D, E, and
' K, which could have been computed by simple multiplication and division from
' A and easily rounded correctly to the number of digits given, contain slight
' and unneccessary errors that suggest the degree of imprecision
' characteristic of slide-rule calculation.)
'
' (Note also the odd swap between C and G in the otherwise alphabetical table
' columns.  This may indicate that those values were accidentally interchanged
' in filling out the table and the columns subsequently relabeled as the
' easiest way to salvage the work.)
'
' For the 1959 and 1960 flags, the diameter (renamed K) is specified as
' .0616 A, a ratio which does agree well with and may have been inferred from
' the specific 1912 values.  The Wikipedia article for the US flag states that
' the star diameter of .0616 is .8 * .077, where .077 is the rounded value of
' 1/13, the width of a stripe.  No supporting citation is given.  I suspect
' that the original intention has been forgotten.
'
' My hypothesis is that each star was intended to occupy 1/8 of the area
' available to it in the 48-star 1912 flag and that the implied ratio of
' .0616 was carried over for the later flags, despite the slight decrease in
' average area available to each star within the union.
'
' The area of the isoceles triangle formed by the circle's center and two
' adjacent convex vertices of the pentagram is
'
'    2
'   S tan(54)/4
'
' where S is the length of the chord between the vertices and all
' trigonometric arguments are given in degrees.  The area of the isoceles
' triangle formed by a concave vertex and two adjacent convex vertices is
'
'    2
'   S tan(36)/4
'
' The length of the chord is
'
'   S = J sin(36)
'
' where J is the circle's diameter.  So the area of the pentagram is
'
'        2   2                              2
'   5/4 J sin (36)[tan(54)-tan(36)] = 5/16 J sqr(50 - 22 sqr(5))
'
'                   2
'   ~= .2806424854 J
'
' One-eighth of the area allocated to each star is
'
'   (.76 A x 7/13 A)/(48 x 8) = 133/124800 A  ~= .001065705128 A
'
' where A is the flag's hoist.  So
'
'         2                                  2
'   5/16 J sqr(50 - 22 sqr(5)) = 133/124800 A
'
'   J/A = sqr(133/39000/sqr(50 - 22 sqr(5))) ~= .0616228555
'
'   J = .0616228555 * 19    ft = 1.17083 ft vs. 1.1703 ft (#2 flag)
'   J = .0616228555 * 14.34 ft =  .88367 ft vs.  .8832 ft (#3 flag)
'   J = .0616228555 * 12.19 ft =  .75118 ft vs.  .7509 ft (#4 flag)
'   J = .0616228555 *  8.94 ft =  .55091 ft vs.  .5506 ft (#6 flag)
'   J = .0616228555 *  5.14 ft =  .31674 ft vs.  .3165 ft (#7 flag)
'
' Working backward, I divide the specified values of J, as well as the minimum
' and maximum values that J could have been rounded from, by the hoist A:
'
'  minimum      specified    maximum
' .06159210526 .06159473684 .06159736842
' .06158647141 .06158995816 .06159344491
' .06159557014 .06159967186 .06160377358
' .06158277405 .06158836689 .06159395973
' .06156614786 .06157587549 .06158560311
'
' overall average = .06158972185#
'
' These values are all very slightly high; but that is plausibly the result of
' imperfect calculation of the square-root and its argument in 1912.  All the
' values of J/A, including the proposed value, round to the .0616 value used
' for K/A in 1959 and 1960.
'
END SUB

DEFSTR A-Z
FUNCTION drawunion%
' Draw the specified flag
'   Declare local variables
      DIM dx     AS DOUBLE
      DIM dy     AS DOUBLE
      DIM i      AS INTEGER
      DIM row    AS INTEGER
      DIM x      AS DOUBLE
      DIM y      AS DOUBLE
      DIM ystart AS DOUBLE
      DIM ystep  AS DOUBLE
    IF rows THEN
'     Avoid overlapping stars
        IF xstep < rout * 2 THEN EXIT FUNCTION
        IF rows = 1 THEN
          ystart = unionhoist / 2
          ystep = unionhoist
        ELSE
          ystart = margin
          ystep = (unionhoist - margin * 2) / (rows - 1)
          IF xstart(0) = xstart(1) THEN
            IF ystep < rout * 2 THEN EXIT FUNCTION
          ELSEIF xstep * xstep + ystep * ystep * 4 < rout * rout * 16 THEN
            EXIT FUNCTION
          ELSEIF rows > 2 AND ystep < rout THEN EXIT FUNCTION
          END IF
        END IF
    END IF
'   Draw the blue of the union
      CLS
      LINE (0, 0)-(unionfly, unionhoist), 1, BF
'   Label the union
      COLOR 15: LOCATE 30, 1
      PRINT STR$(n); "-star union";
    IF rows THEN
'     Draw the stars
        y = ystart
        FOR row = 1 TO rows
'         Draw a row of stars
            FOR x = xstart(row AND 1) TO unionfly - margin * .99 STEP xstep
'             Draw a star
                PSET (x, y - rout)
                FOR i = 1 TO 10 STEP 2
                  dx = rin * SIN(fifthpi * i)
                  dy = rin * COS(fifthpi * i)
                  LINE STEP(0, 0)-(x + dx, y - dy)
                  dx = rout * SIN(fifthpi * (i + 1))
                  dy = rout * COS(fifthpi * (i + 1))
                  LINE STEP(0, 0)-(x + dx, y - dy)
                NEXT i
'             Fill in the star
                PAINT (x, y)
            NEXT x
          y = y + ystep
        NEXT row
      IF version = "" THEN
           version = "A"
      ELSE version = CHR$(ASC(version) + 1)
      END IF
      PRINT ", version "; version;
    ELSE
'     Indicate that no union was found
        PRINT " not found";
    END IF
'   Wait for input
'     Read one character
        PRINT "  Next flag? ";
        DO
          c1 = INKEY$
          IF c1 = CHR$(27) THEN END
        LOOP WHILE c1 = ""
      IF c1 <> " " THEN
'       Increment the number of stars if the right-arrow key is pressed
          IF c1 = CHR$(0) + CHR$(77) THEN
            n = n + 1
'       Decrement the number of stars if the left-arrow key is pressed
          ELSEIF c1 = CHR$(0) + CHR$(75) THEN
            n = n - 1
'       Otherwise, read a new number of stars
          ELSE
            IF c1 >= " " THEN PRINT c1;
            INPUT "", c2
            IF VAL(c1 + c2) THEN n = VAL(c1 + c2)
          END IF
          drawunion = true
      END IF
END FUNCTION

DEFINT A-Z
SUB flagcanon
'flag(1) = "6 * 8 + 0 * 0 = 48"
'flag(2) = "4 * 7 + 3 * 7 = 49"
'flag(3) = "5 * 6 + 4 * 5 = 50"
'flag(4) = "3 * 9 + 3 * 8 = 51"
'flag(5) = "4 * 7 + 3 * 8 = 52"
'flag(6) = "4 * 8 + 3 * 7 = 53"
'flag(7) = "6 * 9 + 0 * 0 = 54"
'flag(8) = "5 * 6 + 5 * 5 = 55"
'flag(9) = "7 * 8 + 0 * 0 = 56"
'flag(10) = "3 *10 + 3 * 9 = 57"
'flag(11) = "5 * 6 + 4 * 7 = 58"
'flag(12) = "5 * 7 + 4 * 6 = 59"
'flag(13) = "4 * 8 + 4 * 7 = 60a"
'flag(14) = "6 * 5 + 5 * 6 = 60b"
'flag(15) = "6 * 6 + 5 * 5 = 61"
'flag(16) = "3 * 12 + 2 * 13 = 62  (problematical)"
'flag(17) = "7 * 9 + 0 * 0 = 63"
'flag(18) = "4 * 8 + 4 * 8 = 64"
'flag(19) = "5 * 7 + 5 * 6 = 65"
'flag(20) = "6 * 6 + 5 * 6 = 66"
'flag(21) = "5 * 7 + 4 * 8 = 67"
'flag(22) = "5 * 8 + 4 * 7 = 68"
'flag(23) = "3 * 12 + 3 * 11 = 69"
'flag(24) = "7 * 10 + 0 * 0 = 70"
'flag(25) = "6 * 6 + 5 * 7 = 71"
'flag(26) = "8 * 9 + 0 * 0 = 72a"
'flag(27) = "4 * 9 + 4 * 9 = 72b"
'flag(28) = "6 * 7 + 5 * 6 = 72c"
'flag(29) = "4 * 10 + 3 * 11 = 73"
'flag(30) = "4 * 11 + 3 * 10 = 74"
'flag(31) = "5 * 8 + 5 * 7 = 75"
'flag(32) = "5 * 8 + 4 * 9 = 76"
'flag(33) = "4 * 11 + 3 * 11 = 77"
'flag(34) = "6 * 7 + 6 * 6 = 78"
'flag(35) = "2 * 26 + 1 * 27 = 79  (problematical)"
'flag(36) = "8 * 10 + 0 * 0 = 80a"
'flag(37) = "4 * 10 + 4 * 10 = 80b"
END SUB

DEFSTR A-Z
SUB history
' 17760614 13 = 3 * 3 + 2 * 2
' 17950501 15 = 3 * 3 + 2 * 2
' 18180704 20 = 4 * 5
' 1819     21 = 5,4,6,6
' 1820     23 = 6,5,6,6
' 1822     24 = 4 * 6
' 1836     25 = 6,5,7,7
' 1837     26 = 7,6,6,7
' 1845     27 = 7,6,7,7
' 1846     28 = 4 * 7
' 1847     29 = 8,7,6,8
' 1848     30 = 5 * 6
' 1851     31 = 7,5,6,6,7
' 1858     32 = 7,6,6,6,7
' 1859     33 = 7,7,5,7,7
' 1861     34 = 7,7,6,7,7
' 1863     35 = 5 * 7
' 1865     36 = 8,6,8,6,8
' 1867     37 = 7,8,7,8,7
' 1877     38 = 7,8,8,8,7
' 1890     43 = 8,7,7,7,7,7
' 1891     44 = 8,7,7,7,7,8
' 1896     45 = 3 * 8 + 3 * 7
' 1908     46 = 8,7,8,8,7,8
' 1912     48 = 6 * 8
' 1959     49 = 4 * 7 + 3 * 7
' 1960     50 = 5 * 6 + 4 * 5
END SUB

USFLAG.BS
Displaying USFLAG.BS.