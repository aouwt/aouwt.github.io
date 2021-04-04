$CONSOLE:ONLY
_DEST _CONSOLE
'Sorry for spaghet, this was hacked together real quick.
ON ERROR GOTO erh
GOTO Start

erh:
PRINT USING "ERROR ### AT ###! (##/25)"; ERR, _ERRORLINE, erno%
erno% = erno% + 1
IF erno% > 25 THEN
    PRINT "Too many errors; halting!"
    SYSTEM
END IF
RESUME NEXT

Start:
PRINT "HTML Linker v1 - Modified for integration to GitHub Actions"
PRINT "Regular version at https://github.com/all-other-usernames-were-taken/all-other-usernames-were-taken/blob/main/html%20linker/"
PRINT "------"
PRINT "SHELL> find * -name '*.html' > ./dirlist.txt"
SHELL "find * -name '*.html' > ./dirlist.txt"
PRINT "Opening output"
dir% = FREEFILE
OPEN "./dirlist.txt" FOR INPUT AS #dir%
DO
    LINE INPUT #dir%, file$
    PRINT "Linking file: " + file$
    f% = FREEFILE
    OPEN file$ + ".linked" FOR OUTPUT AS #f%
    PRINT "Linking"
    PRINT #f%, linker$(LoadFile$(file$));
    DIM q AS STRING * 1: q = CHR$(34)
    s$ = "mv " + q + "./" + file$ + ".linked" + q + " " + q + "../" + file$ + q + " -u"
    PRINT "SHELL DONTWAIT> " + s$
    SHELL _DONTWAIT s$
    PRINT "Closing"
    CLOSE #f%
LOOP UNTIL EOF(dir%)
PRINT "All done, closing and deleting dirlist"
CLOSE #dir%
KILL "./dirlist.txt"
SYSTEM

FUNCTION linker$ (f AS STRING)
    PRINT "Running linker"
    CONST LinkBegin = "<!--LINKER:"
    CONST LinkEnd = "-->"
    TYPE var
        v AS STRING
        n AS STRING
    END TYPE
    DIM vars(100) AS var
    DO
        lstart = INSTR(f, LinkBegin)
        lend = INSTR(f, LinkEnd)
        IF (lstart = 0) OR (lend = 0) THEN EXIT DO

        cmd$ = MID$(f, lstart + LEN(LinkBegin), lend - lstart - LEN(LinkBegin))

        sep% = INSTR(cmd$, ":")
        act$ = LEFT$(cmd$, sep% - 1)
        arg$ = MID$(cmd$, sep% + 1)

        SELECT CASE act$
            CASE "LINK"
                o$ = LoadFile(arg$)

            CASE "SET"
                vars(nextvar%).n = arg$
                vars(nextvar%).v = "TRUE"
                nextvar% = nextvar% + 1

            CASE "IF"
                sep% = INSTR(arg$, ";")
                act$ = MID$(arg$, sep% + 1)
                arg$ = LEFT$(arg$, sep% - 1)
                FOR i% = 0 TO nextvar%
                    IF vars(i%).n = arg$ THEN o$ = act$: EXIT FOR
                NEXT

            CASE "IFN"
                sep% = INSTR(arg$, ";")
                o$ = MID$(arg$, sep% + 1)
                act$ = LEFT$(arg$, sep% - 1)
                FOR i% = 0 TO nextvar%
                    IF vars(i%).n = act$ THEN o$ = "": EXIT FOR
                NEXT

            CASE "STR"
                sep% = INSTR(arg$, "=")
                act$ = MID$(arg$, sep% + 1)
                arg$ = LEFT$(arg$, sep% - 1)
                vars(nextvar%).n = arg$
                vars(nextvar%).v = act$
                nextvar% = nextvar% + 1

            CASE "PUT"
                FOR i% = 0 TO nextvar%
                    IF vars(i%).n = arg$ THEN o$ = vars(i%).v: EXIT FOR
                NEXT

            CASE ELSE
                PRINT "Error: Invalid command " + cmd$
        END SELECT
        f = LEFT$(f, lstart - 1) + o$ + MID$(f, lend + LEN(LinkEnd))
        o$ = ""
    LOOP
    linker$ = f
END FUNCTION

FUNCTION LoadFile$ (file$)
    PRINT "LoadFile: " + file$
    IF _FILEEXISTS(file$) = 0 THEN
        PRINT "Error: cannot find file "; file$
        EXIT FUNCTION
    END IF
    DIM f AS INTEGER
    f = FREEFILE
    OPEN file$ FOR BINARY AS #f
    f$ = SPACE$(LOF(f))
    GET #f, , f$
    CLOSE #f
    LoadFile$ = f$
END FUNCTION

