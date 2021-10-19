$CONSOLE:ONLY
_DEST _CONSOLE
'Sorry for spaghet, this was hacked together real quick.
DIM SHARED lvl AS _UNSIGNED INTEGER
ON ERROR GOTO erh
IF COMMAND$(1) = "bulkmode" GOTO StartBulk

erh:
PRINT "########";
PRINT USING "ERROR ### AT ###_! (##/25)"; ERR, _ERRORLINE, erno%;
PRINT "########"
erno% = erno% + 1
IF erno% > 25 THEN
    PRINT "Too many errors; halting!"
    SYSTEM
END IF
RESUME NEXT

StartBulk:
PRINT "HTML Linker v1 with bulkmode"
PRINT "Regular version at https://github.com/all-other-usernames-were-taken/all-other-usernames-were-taken/blob/main/html%20linker/"
PRINT "------"
PRINT "Initializing"
incl
logp "SHELL> find * -name '*.html' > ./dirlist.txt"
SHELL "find . -name '*.html' > ./dirlist.txt"
logp "Opening output"
dir% = FREEFILE
OPEN "./dirlist.txt" FOR INPUT AS #dir%
decl
incl
DO
    LINE INPUT #dir%, file$
    logp "Linking file: " + file$
    f% = FREEFILE
    OPEN file$ + ".linked" FOR OUTPUT AS #f%
    incl
    logp "Running linker"

    incl
    PRINT #f%, linker$(LoadFile$(file$));
    DIM q AS STRING * 1: q = CHR$(34)
    s$ = "mv " + q + "./" + file$ + ".linked" + q + " " + q + "../" + file$ + q + " -u"
    decl

    logp "SHELL DONTWAIT> " + s$
    SHELL _DONTWAIT s$
    logp "Closing file"
    CLOSE #f%

    decl
LOOP UNTIL EOF(dir%)
logp "All done, closing and deleting dirlist"
CLOSE #dir%
KILL "./dirlist.txt"
SYSTEM

FUNCTION linker$ (f AS STRING)
    CONST LinkBegin = "<!--LINKER:"
    CONST LinkEnd = "-->"
    TYPE var
        v AS STRING
        n AS STRING
    END TYPE
    DIM vars(100) AS var, lstart AS _UNSIGNED LONG, lend AS _UNSIGNED LONG
    DO
        lstart = INSTR(f, LinkBegin)
        lend = INSTR(lstart, f, LinkEnd)
        logp "lstart=" + STR$(lstart) + ";  lend=" + STR$(lend)
        IF (lstart = 0) OR (lend = 0) THEN EXIT DO

        cmd$ = MID$(f, lstart + LEN(LinkBegin), lend - lstart - LEN(LinkBegin))
        logpsep "cmd$='" + cmd$ + "'"
        incl

        sep% = INSTR(cmd$, ":")
        act$ = LEFT$(cmd$, sep% - 1)
        arg$ = MID$(cmd$, sep% + 1)
        logp "seperator=" + STR$(sep%)
        logp "arg='" + arg$ + "'"
        logp "act='" + act$ + "'"
        incl

        SELECT CASE act$
            CASE "LINK"
                o2$ = "<!-- LINK BEGIN " + arg$ + " -->" + LoadFile(arg$) + "<!-- LINK END " + arg$ + " -->"
                o$ = ""

            CASE "SET"
                vars(nextvar%).n = arg$
                vars(nextvar%).v = "TRUE"
                nextvar% = nextvar% + 1
                logp "varcount=" + STR$(nextvar%)

            CASE "IF"
                sep% = INSTR(arg$, ";")
                act$ = MID$(arg$, sep% + 1)
                arg$ = LEFT$(arg$, sep% - 1)
                logp "seperator=" + STR$(sep%)
                logp "var='" + arg$ + "'"
                logp "put='" + act$ + "'"
                incl
                FOR i% = 0 TO nextvar%
                    IF vars(i%).n = arg$ THEN o$ = act$: EXIT FOR
                NEXT
                IF LEN(o$) THEN logp "eval=true (put)" ELSE logp "eval=false (don't put)"
                decl

            CASE "IFN"
                sep% = INSTR(arg$, ";")
                o$ = MID$(arg$, sep% + 1)
                act$ = LEFT$(arg$, sep% - 1)
                logp "seperator=" + STR$(sep%)
                logp "var='" + act$ + "'"
                logp "put='" + o$ + "'"
                incl
                FOR i% = 0 TO nextvar%
                    IF vars(i%).n = act$ THEN o$ = "": EXIT FOR
                NEXT
                IF LEN(o$) THEN logp "eval=false (put)" ELSE logp "eval=true (don't put)"
                decl

            CASE "STR"
                sep% = INSTR(arg$, "=")
                act$ = MID$(arg$, sep% + 1)
                arg$ = LEFT$(arg$, sep% - 1)
                logp "seperator=" + STR$(sep%)
                logp "val='" + act$ + "'"
                logp "var='" + arg$ + "'"
                vars(nextvar%).n = arg$
                vars(nextvar%).v = act$
                nextvar% = nextvar% + 1
                logp "varcount=" + STR$(nextvar%)

            CASE "PUT"
                FOR i% = 0 TO nextvar%
                    IF vars(i%).n = arg$ THEN o$ = vars(i%).v: EXIT FOR
                NEXT
                logp "put='" + o$ + "'"
        END SELECT
        decl
        logp "insert='" + o$ + "'"
        incl
        FOR i% = 1 TO LEN(o$) 'so you can do multiple layers of stuff    (like <!--LINKER:IF:a;<!--LINKER:PUT:a--\>-->
            IF ASC(o$, i%) = 92 THEN i% = i% + 1 '\
            o2$ = o2$ + CHR$(ASC(o$, i%))
        NEXT
        logp "parsed='" + o2$ + "'"
        decl
        'logp "file='" + f + "'"
        f = LEFT$(f, lstart - 1) + o2$ + MID$(f, lend + LEN(LinkEnd))
        o$ = ""
        o2$ = ""
        decl
    LOOP
    linker$ = f
END FUNCTION

FUNCTION LoadFile$ (file$)
    incl
    logp "LoadFile: " + file$
    DIM f AS INTEGER
    f = FREEFILE
    OPEN file$ FOR BINARY AS #f
    f$ = SPACE$(LOF(f))
    GET #f, , f$
    CLOSE #f
    LoadFile$ = f$
    decl
END FUNCTION

SUB logp (s$)
    FOR i% = 1 TO LEN(s$)
        SELECT CASE ASC(s$, i%)
            CASE 0 TO 31
                MID$(s$, i%, 1) = " "
                i% = i% - 1
            CASE 32
                IF ASC(s$, i% - 1) <> 32 THEN b$ = b$ + " "
            CASE ELSE
                b$ = b$ + CHR$(ASC(s$, i%))
        END SELECT
    NEXT
    'IF LEN(b$) > 120 THEN b$ = LEFT$(b$, 60) + "..." + RIGHT$(b$, 60)
    PRINT SPACE$(lvl * 3) + b$
END SUB

SUB decl
    SHARED decl.isdec`
    IF lvl = 0 THEN
        logp "######ERROR! Decl: Decrementing level past 0!######"
        EXIT SUB
    END IF
    lvl = lvl - 1
    IF decl.isdec` = 0 THEN PRINT
    decl.isdec` = (decl.isdec` = 0)
END SUB

SUB incl
    SHARED decl.isdec`
    lvl = lvl + 1
    decl.isdec` = 0
END SUB

SUB logpsep (s$)
    FOR i% = 1 TO LEN(s$)
        SELECT CASE ASC(s$, i%)
            CASE 0 TO 31
                MID$(s$, i%, 1) = " "
                i% = i% - 1
            CASE 32
                IF ASC(s$, i% - 1) <> 32 THEN b$ = b$ + " "
            CASE ELSE
                b$ = b$ + CHR$(ASC(s$, i%))
        END SELECT
    NEXT
    IF LEN(b$) > 120 THEN b$ = LEFT$(b$, 60) + "..." + RIGHT$(b$, 60)
    PRINT STRING$(lvl * 3, "-") + b$

END SUB
