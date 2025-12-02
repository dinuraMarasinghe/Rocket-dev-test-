      PROGRAM DELNT.C
      PROGRAM.NAME = "DELNT.C"

*********************************************************************
* PROGRAM........DELIVERY NOTE CONFIRMATION

* DESCRIPTION.
* This module allows the confirmation of orders for the Account Sale.
* The confirmation is the final stage of an Account Sale before the
* invoice line process commences and denotes that the goods have
* actually been delivered. This process may be requested for a single
* T.P. Number or a range, or a range with up to ten exclusions.
* Specific delivery note SP Numbers must be authorised before
* confirmation can take place if the Delivery Note was authorised.
*
* DUMMY input is used to hold an error message on the screen. When a
* key is pressed, the Exit Menu Link menu will be displayed.
*
* Entry:  Point of Sale Menu 2
* Exit :  Point of Sale Menu 2
*
*  REVISION HISTORY
*   Date    Inits  Job No  Reason
* --------  -----  ------  ------
* 15481r01 09/05/16 brrob PRB0004881 - In some cases when a ticket is
*                         moved to adjustment Auth (AA) state it's not
*                         creating a conf-acct-xref record
* 15572r06 17/01/18 brrob PRB0006157 - DNC stuck in a loop for Wrekin
*                         tickets (961844) when xml comes back with
*                         status of rejected.
* 15616.22 06/05/20 connp changes to allow manual confirmation of E tickets
*                         if the branch is using the collection management app
*                   milst Add arguments BRANCH.CODE, ERR and spares to OM.CHECK.APP.SETTINGS
* 15624rnn 18/11/20 lneal SCP-1735 amend to check for Openfleet key in
*                         OH.DELIV.LOAD
* 15636r17 24/11/20 ederr Only confirm E tickets for app branches if status is IC
* 15655r40 04/08/21 lneal SCP-1913 Do not confirm incomplete split B tickets
* 15655r83 20/01/22 lneal Amend validation around due balance
* 15672r44 08/04/22 umaan SCP-2261 Displayed warning message if the
*                         delivery date is in the future while trying to
*                         delivery confirm an IBT
* 15697r22 11/04/22 lneal SCP-2157 Cancel outstanding delivery jobs on delconf
* 15672r62 17/06/22 vissi SCP-2403 Removed a case to confirm collected split B tickets
* 15723r02 30/01/23 kasoc Check for dump and sundry coded lines for Apex Inv Gen
* 15744r11 12/01/24 rhmic CSD-437 Brand Separation and Tidy Up
* 15699r87 24/07/24 lewdi BRD#204.5 Display warning for M tickets (cannot
*                         delivery un-confirm)
* 15740r61 24/09/24 tokel APT-63, Don't allow C or G Tickets Unconfirm.
* 15712r55 29/10/24 jtudo BRD-240 SimplificatIon for ICT Type 3 and ICT Type 5
*********************************************************************

*******************************************
*    I N I T I A L I S A T I O N
*******************************************
*    DEFINE CONSTANTS
*******************************************
*

      INCLUDE DG-INCLUDES TCSTANDARD

$OPTIONS REALITY
$OPTIONS HEADER.DATE

      DG.PROGRAM.NAME="DELNT.C"
      INCLUDE DG-INCLUDES DEFFUN.INCLUDES

*************
BEFORE.START:
*************

      EQU CLEAR TO FF
      CEOL = CLEOL
      SYS.DATE = DATE()
      SYS.DATE.30 = SYS.DATE - 30
      SYS.DATE.60 = SYS.DATE - 60
      DISP.DATE = OCONV(SYS.DATE,"D2/")
      PROMPT ""
      HOLD.FROM.SP.NUMBER = ""
      HOLD.TO.SP.NUMBER = ""
      CALLED.FROM = 99999

*******************************************
* DIMENSION AND EQUATE VARIABLES
*******************************************

      INCLUDE INCLUDES EQU.BRN.DETAIL.DIM
      INCLUDE INCLUDES EQU.BRANCH.PARAMS.BRANCH.DIM
      INCLUDE INCLUDES EQU.ORDER.HEADER.DIM
      INCLUDE INCLUDES EQU.POS.CUSTOMER.EPOS
      INCLUDE INCLUDES EQU.ORDER.LINE
      INCLUDE INCLUDES EQU.BS.BRANCH.STOCK.DIM
      INCLUDE INCLUDES EQU.PO.HEADER.DIM
      INCLUDE INCLUDES EQU.SMS.TEXT.EPOS.TRANS.DIM
      INCLUDE INCLUDES COMMON.SCREEN
      INCLUDE INCLUDES EQU.OS.ECOMMERCE.DIARY.DIM
      INCLUDE INCLUDES EQU.DELIV.MAN.JOB.RESULT.DIM

      DIM EXC.NO(10)
      DIM HOLD.EXC.NO(10)
      MAT HOLD.EXC.NO=""
      MAT EXC.NO = ""                    ; *Exclusion SP Numbers

      DIM CLEAR.EXC(10)
      CLEAR.EXC(1) = @(29,14):"      "
      CLEAR.EXC(2) = @(29,15):"      "
      CLEAR.EXC(3) = @(29,16):"      "
      CLEAR.EXC(4) = @(29,17):"      "
      CLEAR.EXC(5) = @(29,18):"      "
      CLEAR.EXC(6) = @(46,14):"      "
      CLEAR.EXC(7) = @(46,15):"      "
      CLEAR.EXC(8) = @(46,16):"      "
      CLEAR.EXC(9) = @(46,17):"      "
      CLEAR.EXC(10)= @(46,18):"      "

*****************************************
*   ERROR MESSAGES INITIALISATION
*****************************************

      DIM ERR.MSG(20)
      MAT ERR.MSG = ""

      ERR.MSG(1) = "ENTER 'Y' TO ACCEPT, 'N' TO AMEND OR '/' TO REJECT"
      ERR.MSG(2) = "MANDATORY INPUT"
      ERR.MSG(3) = "EOS CHARACTER SUPPRESSED"
      ERR.MSG(4) = "INVALID ENTRY"
      ERR.MSG(5) = "INVALID AUTHORISED RESPONSE"
      ERR.MSG(6) = "DELIVERY NUMBER MUST BE ZERO"
      ERR.MSG(7) = "DELIVERY CONFIRMATION DATE CANNOT BE EARLIER THAN 30 DAYS AGO"
      ERR.MSG(8) = "DELIVERY CONFIRMATION DATE CANNOT BE GREATER THAN TODAY'S DATE"
      ERR.MSG(9) = "SUPPLIERS DETAIL NOT ENTERED FOR DIRECT ORDER - CANNOT CONFIRM"
      ERR.MSG(10) = "ORDER HEADER HAS NO ORDER LINES -CANNOT CONFIRM"
      ERR.MSG(11) = "BRANCH ORDER NO. NOT PRESENT FOR CUST SITE DELIVERY - CANNOT CONFIRM : "
      ERR.MSG(12) = "DELIVERY CONFIRMATION DATE CANNOT BE EARLIER THAN 60 DAYS AGO"
      ERR.MSG(13) = "THIS ORDER CAN ONLY BE CONFIRMED BY PICKING"
      ERR.MSG(14) = "DELIVERY CONFIRMATION DATE CANNOT BE FROM LAST MONTH"

*******************************************
*  HELP MESSAGES INITIALISATION
*******************************************

      HLINE = @(0,22):CEOL
      DIM HELP.MSG(20)
      MAT HELP.MSG=""

      HELP.MSG(1) = "DATE ON WHICH DELIVERY WAS MADE"
      HELP.MSG(2) = HLINE:"ENTER START DOCUMENT NUMBER TO CONFIRM DELIVERY"
      HELP.MSG(3) = HLINE:"ENTER END DOCUMENT NUMBER TO CONFIRM DELIVERY"
      HELP.MSG(4) = HLINE:"ENTER DOCUMENT NUMBER TO EXCLUDE FROM DELIVERY CONFIRMATION"
      HELP.MSG(5) = HLINE:'"Y" TO CONFIRM DELIVERIES OR "/" TO CANCEL CONFIRMATION'

*******************************************
*  OPENING FILES ROUTINE
*******************************************

      ACCOUNT.NAME = OCONV(@WHO,'MCU')
      ERR = ''

      INCLUDE INCLUDES OPEN.FILE.TERMWORK
      INCLUDE INCLUDES OPEN.FILE.SY.PARAMS
      INCLUDE INCLUDES OPEN.FILE.SCREENS
      INCLUDE INCLUDES OPEN.FILE.BRN.CONTROL
      INCLUDE INCLUDES OPEN.FILE.BRN.DETAIL
      INCLUDE INCLUDES OPEN.FILE.ORDER.HEADER
      INCLUDE INCLUDES OPEN.FILE.ORD.TRACK
      INCLUDE INCLUDES OPEN.FILE.CONF.ACCT.XREF
      INCLUDE INCLUDES OPEN.FILE.ORDER.LINE
      INCLUDE INCLUDES OPEN.FILE.POS.CUSTOMER
      INCLUDE INCLUDES OPEN.FILE.QUOTE.HEADER
      INCLUDE INCLUDES OPEN.FILE.PO.HEADER
      INCLUDE INCLUDES OPEN.FILE.BS.BRANCH.STOCK
      INCLUDE INCLUDES OPEN.FILE.SMS.TEXT.EPOS.TRANS
      INCLUDE INCLUDES OPEN.FILE.DELIV.MAN.JOB.RESULT

      IF ERR # '' THEN
         FATAL.ERROR.MSG = "UNABLE TO OPEN FILE(S) " : ERR
         GOSUB FATAL.ERROR
      END

      INCLUDE INCLUDES CONSTRUCT.TOP.RIGHT
      TERMWORK.KEY = PORT
*
*******************************************
*  MAIN BODY OF PROGRAM
*******************************************
*
      INCLUDE INCLUDES MN.TERMWORK.COMMON
      INCLUDE INCLUDES EQU.MN.TERMWORK.REC

*********************************************************************
START.OF.PROGRAM:
******************

      GOSUB INITIALISE
      GOSUB READ.AND.DISPLAY.SCREEN

**************
MAIN.PROCESS:
**************

      DELIV.DATE = ""
      HOLD.ORD.DELIV.DATE = ""
      FROM.SP.NUMBER = ""
      TO.SP.NUMBER = ""
      MAT EXC.NO = ""
      OK = ""

      MATREAD BRN.DETAIL.REC FROM BRN.DETAIL.FV,BRANCH.CODE ELSE
         MAT BRN.DETAIL.REC = ""
      END
      IF (BRN.AUTO.INVOICE.CONFIRM.AT.PRT = "Y") OR (BRN.AUTO.INVOICE.CONFIRM = "Y") THEN
         CALL ERR("THIS BRANCH IS SET UP FOR AUTO INVOICE CONFIRMATION ON PRINTING - PRESS RETURN","")
         RETURN
      END

      GOSUB 33                           ; * Get Valid Date

      CRT HELP.MSG(2):

      GOSUB 34                           ; * Get From Order Number

      IF DELIV.NO > 0 OR SALE.TYPE = "G" THEN
         TO.SP.NUMBER = FROM.SP.NUMBER
         GOTO 402
      END ELSE
         CRT HELP.MSG(3):
         GOSUB 35                        ; * Get To Order Number
      END

      IF FROM.SP.NUMBER = TO.SP.NUMBER THEN
         GOTO 402
      END

      NOS = 0
      L=13
      C=29
      LOOP NOS = NOS + 1 UNTIL NOS > 10 DO
         L=L+1
         CRT HELP.MSG(4):

         GOSUB 36                        ; * Get Excluded Order Numbers

         IF L=18 THEN
            C=46
            L=13
         END
      REPEAT

*******************************************************
402:
****

      GOSUB GET.OK

      FROM.ORDER.HEADER = FROM.SP.NUMBER[6,1]:FROM.SP.NUMBER[1,1]:BRANCH.CODE:FROM.SP.NUMBER[2,4]
      TO.ORDER.HEADER = FROM.ORDER.HEADER
      TO.ORDER.HEADER[7,4] = TO.SP.NUMBER[2,4]

      STMT = 'SELECT ORDER-HEADER >= "':FROM.ORDER.HEADER:'"'
      STMT := ' AND <= "':TO.ORDER.HEADER:'"'

      EXECUTE STMT RTNLIST ORDER.LIST CAPTURING JUNK

      LOOP
         READNEXT ORDER.HEADER.KEY FROM ORDER.LIST ELSE
            EXIT
         END

         ORD.KEY = ORDER.HEADER.KEY[2,1]:ORDER.HEADER.KEY[7,4]:ORDER.HEADER.KEY[1,1]
         EXCLUDED.ORDER = @FALSE

         FOR NOS = 1 TO 10
            IF ORD.KEY = EXC.NO(NOS) THEN
               EXCLUDED.ORDER = @TRUE
               EXIT
            END
         NEXT

         IF EXCLUDED.ORDER THEN
            CONTINUE
         END

         IF ORACLE.INVOICING THEN
            GOSUB CHECK.FOR.DUMP.AND.SUNDRY.LINES
            IF SKIP.TICKET THEN
               CONTINUE
            END
         END

         IF BRN.CONTROL.REC<2> # "Y" THEN
            GOTO 2
         END

         TRANS.ACTION = 'S'
         DG.TRANS.FLAG = ""

         GOSUB DG.TRANS.BOUND

         IF TRANS.STATUS THEN
            UPDATE.SOURCE.ORDER = @FALSE

*******************************************************
2:          GOSUB IS.ORDER.UPDATE.RQD

            IF BRN.CONTROL.REC<2> # "Y" THEN
               CONTINUE
            END

            TRANS.ACTION = 'E'
            DG.TRANS.FLAG = ""

            GOSUB DG.TRANS.BOUND

            IF TRANS.STATUS THEN
               IF UPDATE.SOURCE.ORDER THEN
                  CALL UPDATE.DIRECT.PO.SUPPLIER.DETAILS(SEND.BRANCH, IBT.ORDER.HEADER.ID, IBT.ORDER.HEADER.REC, "", "", PO.DET.ERR)
               END

               IF OH.WK.ORD.NO[1,3] = 'WEB' OR OH.WK.ORD.NO[1,3] = 'XML' OR OH.WK.ORD.NO[1,6] = 'Ticket' THEN
                  TEXT = 'Confirmed Delivery of'
                  TPNO = ORDER.HEADER.KEY[2,1]
                  TPNO := ORDER.HEADER.KEY[7,4]
                  TPNO := ORDER.HEADER.KEY[1,1]
                  CALL OS.EMAIL.NOTIFICATION(USER,OH.EMAIL.NOTIFICATION,BRANCH.CODE,TPNO,TEXT,SUCCESS,ERROR.MESSAGE,'','','','','')
                  IF NOT(SUCCESS) AND ERROR.MESSAGE NE '' THEN
                     CALL ERR(ERROR.MESSAGE,'')
                  END
               END

            END ELSE
               GOTO TRY.TO.ABORT
            END
         END ELSE
            CRT.VAR = SYS.LINE.ON :"Unable to start a new transaction": SYS.LINE.OFF ; CRT CRT.VAR:
            STOP
         END

      REPEAT

      GOTO BEFORE.START

*******************************************************
* SUBROUTINES AND EXCEPTION ROUTINES
*******************************************************

***********
INITIALISE:
***********

      READU TERMWORK.REC FROM TERMWORK.FV, TERMWORK.KEY ELSE
         CALL ERR("TERMWORK RECORD ":TERMWORK.KEY:" DOES NOT EXIST","")
         GOTO 99999
      END

      USER = TERMWORK.REC<1>
      BRANCH.CODE = TERMWORK.REC<2>
      BRN.CONTROL.KEY = "MIC"

      READ BRN.CONTROL.REC FROM BRN.CONTROL.FV,BRN.CONTROL.KEY ELSE
         CALL ERR("BRANCH CONTROL RECORD ":BRN.CONTROL.KEY:" DOES NOT EXIST","")
         GOTO 99999
      END

      READV BRN.IBT.IND FROM BRN.DETAIL.FV,BRANCH.CODE,29 ELSE
         CALL ERR("BRANCH DETAIL RECORD ":BRANCH.CODE:" DOES NOT EXIST","")
         GOTO 99999
      END

      USER.IS.A.WAREHOUSE = 0

      READV WHS.FLAG FROM BRN.DETAIL.FV,BRANCH.CODE,64 ELSE
         WHS.FLAG = "N"
      END
      IF WHS.FLAG = "Y" THEN
         USER.IS.A.WAREHOUSE = 1
      END

      IN.INVOICE.ACCT = 0

      IF USER.IS.A.WAREHOUSE THEN
         MATREAD BRN.DETAIL.REC FROM BRN.DETAIL.FV,BRANCH.CODE THEN
            IF ACCOUNT.NAME=BRN.WAREHOUSE.INVOICE.ACCT THEN
               IN.INVOICE.ACCT=1
            END
         END
      END

      * Using a 'TRANS' ensures successful read if 'locked' by update
      * (NB 'TRANS' lowers mv to sv, and is quicker than open/matread)
      BPB.LORRY.REGISTRATIONS = TRANS("BRANCH-PARAMS",BRANCH.CODE,39,"X")

      MOBILE.APP = @FALSE

      CALL FINMOD.FUNCTIONALITY("NEW.INVOICE.GENERATION",ORACLE.INVOICING,ERRORS)
      IF ERRORS NE "" THEN
         ORACLE.INVOICING = @FALSE
      END

      RETURN

*******************************************
READ.AND.DISPLAY.SCREEN:
*************************

      SCREENS.KEY = "#DELNOTE.CONF"
      READ SCREEN.1.COMMON FROM SCREENS.FV,SCREENS.KEY ELSE
         FATAL.ERROR.MSG = "CANNOT READ ":SCREENS.KEY:" FROM SCREENS FILE"
         GOSUB FATAL.ERROR
         STOP
      END

      SC.CURR = SCREEN.1.COMMON
      SCREENS.KEY = "$DELNOTE.CONF"
      READ SCREENS.REC FROM SCREENS.FV,SCREENS.KEY ELSE
         CALL ERR("SCREEN RECORD $DELNOTE.CONF DOES NOT EXIST","")
         GOTO 99999
      END
      SCREENS.KEY = "%DELNOTE.CONF"
      READ PART.SCREENS.REC FROM SCREENS.FV, SCREENS.KEY ELSE
         CALL ERR("SCREEN RECORD %DELNOTE.CONF DOES NOT EXIST","")
         GOTO 99999
      END

      CRT.VAR = CLEAR:SCREENS.REC:@(0,0):DISP.DATE:TOP.RIGHT ; CRT CRT.VAR:

      RETURN

******************************************************
33:* Get Valid Date
*******************

      IF HOLD.ORD.DELIV.DATE = "" THEN
         INPUT.DATE = SYS.DATE
      END ELSE
         INPUT.DATE = HOLD.ORD.DELIV.DATE
      END

      CALL INP(48,6,INPUT.DATE,8,'D2/','D2/','',1,'','',CTL,HELP.MSG(1),'')
      IF CTL = "/" THEN
         GOTO 99999
      END

      IF INPUT.DATE < SYS.DATE.60 THEN
         CALL ERR(ERR.MSG(12),"")
         GOTO 33
      END
      IF INPUT.DATE > SYS.DATE THEN
         CALL ERR(ERR.MSG(8),"")
         GOTO 33
      END

      ORD.DELIV.DATE = INPUT.DATE

      RETURN

*****************************************************
34:* Get From Order Number
**************************

      GOSUB 341                          ; * Input Order Number

      CALLED.FROM = 34

      GOSUB 342                          ; * Check Order Number of Correct Type

      IF INPUT.DATE < SYS.DATE.30 AND SALE.TYPE # "G" THEN
         CALL ERR(ERR.MSG(7),"")
         RETURN TO START.OF.PROGRAM
      END

      CALLED.FROM = 34
      GOSUB READ.ORDER.HEADER

      IF SP.NUMBER[1,1] = "M" THEN
         MESSAGE = ' W A R N I N G '
         MESSAGE<2> = "This will delivery confirm these transactions"
         MESSAGE<3> = "and send the details to Oracle."
         MESSAGE<4> = "Are you sure, you will not be able to undo this?  "
         CRT CURSOR.OFF:
         WAIT.FOR.ANY.KEY = @FALSE
         GOSUB DISPLAY.MESSAGE.IN.BOX
         WAIT.FOR.ANY.KEY = @TRUE
         CRT CURSOR.ON:
         XPOS = 63 ; YPOS = 12 ; GOSUB ENTER.Y.OR.N
         IF Y.OR.N = "N" THEN
            RETURN TO START.OF.PROGRAM
         END

         CRT.VAR = CLEAR:SCREENS.REC:@(0,0):DISP.DATE:TOP.RIGHT
         CRT.VAR:= @(48,6):OCONV(ORD.DELIV.DATE,"D2/")
         CRT.VAR:= @(48,8):SP.NUMBER
         CRT CRT.VAR:
      END

      RETURN

*******************************************************
341:* Input Order Number
************************

      CRT.VAR = @(48,8):HOLD.FROM.SP.NUMBER ; CRT CRT.VAR:
240:  CRT.VAR = @(48,8):"" ; CRT CRT.VAR:

      INPUT FROM.SP.NUMBER,6_
      IF FROM.SP.NUMBER = "" AND HOLD.FROM.SP.NUMBER # "" THEN
         FROM.SP.NUMBER = HOLD.FROM.SP.NUMBER
      END

      BEGIN CASE
         CASE FROM.SP.NUMBER = "."
            CALL ERR(ERR.MSG(3),"")
            GOTO 240

         CASE FROM.SP.NUMBER = "/"
            GOTO BEFORE.START

         CASE FROM.SP.NUMBER = ""
            CALL ERR(ERR.MSG(2),"")
            GOTO 240

            *
            **   Allow warehouse IBTs to use alphanumeric order numbers
            *
         CASE NOT(FROM.SP.NUMBER MATCHES "1A4X1N")
            CALL ERR(ERR.MSG(4), "")
            GOTO 240

         CASE 1
            IF FROM.SP.NUMBER[1,1] # "M" OR NOT(USER.IS.A.WAREHOUSE) THEN
               IF NOT(FROM.SP.NUMBER MATCHES "1A5N") THEN
                  CALL ERR(ERR.MSG(4),"")
                  GOTO 240
               END
            END

      END CASE
      SP.NUMBER = FROM.SP.NUMBER

      RETURN

********************************************************
342:* Check Order Number of Correct Type
****************************************

      SALE.TYPE = SP.NUMBER[1,1]
      IF SALE.TYPE # "E" & SALE.TYPE # "F" & SALE.TYPE # "M" AND SALE.TYPE # "B" AND SALE.TYPE # "D" THEN
         CALL ERR ("THIS TYPE OF ORDER DOES NOT NEED AUTHORISATION- ORDER ":SP.NUMBER,"")

         BEGIN CASE
            CASE CALLED.FROM = 34
               RETURN TO 34
            CASE CALLED.FROM = 352
               RETURN TO 35
            CASE CALLED.FROM = 3621
               RETURN TO 36
         END CASE

         GOTO 99999                      ; *Called from illegal module
      END

      RETURN

***************************************************************************
READ.ORDER.HEADER:
******************

      * Read Order Header Record
      DELIV.NO = SP.NUMBER[6,1]
      SALE.TYPE = SP.NUMBER[1,1]
      SALE.NO = SP.NUMBER[2,4]

      ORDER.HEADER.KEY = DELIV.NO:SALE.TYPE:BRANCH.CODE:SALE.NO
      MATREADU ORDER.HEADER.REC FROM ORDER.HEADER.FV,ORDER.HEADER.KEY LOCKED
         CALL ERR("RECORD LOCKED","")
         GOTO 99999
      END ELSE
         CALL ERR("ORDER RECORD ":SP.NUMBER:" DOES NOT EXIST","")
         GOTO 34300                      ; *Abnormal exit
      END

      IF OH.ORD.STAT = 'CA' OR OH.ORD.STAT = 'ZZ' THEN
         IF OH.ORD.STAT = 'CA' THEN
            STATUS = 'CANCELLED'
         END ELSE
            STATUS = 'CLOSED'
         END

         CALL ERR("ORDER ":SP.NUMBER:" HAS BEEN ":STATUS,'C')
         GOSUB 34300
      END

      IF (OH.ORD.STAT # "AA" AND DEL.CONF.DTE NE "") OR (OH.ORD.STAT = "IC" AND NOT(USER.IS.A.WAREHOUSE)) THEN
         MOBILE.APP = @FALSE
         CALL OM.CHECK.APP.SETTINGS(MOBILE.APP,"","","","","")

         IF MOBILE.APP = @TRUE AND SALE.TYPE = "E" THEN
            ** allow confirmation
         END ELSE
            CALL ERR("ORDER ":SP.NUMBER:" HAS ALREADY BEEN CONFIRMED","")
            GOTO 34300                   ; *Abnormal exit
         END
      END

      IF (NOT(OH.DN.DATE) OR OH.ORD.STAT = "DA") AND NOT(USER.IS.A.WAREHOUSE) THEN
         IF OH.ORD.STAT = "DA" AND NOT(USER.IS.A.WAREHOUSE) THEN
            CALL ERR("DELIVERY ADJUSTMENT NOT AUTHORISED FOR ":SP.NUMBER,'')
         END ELSE
            CALL ERR("DELIVERY NOTE NOT PRINTED FOR ":SP.NUMBER,'')
         END

         GOTO 34300                      ; *Abnormal exit
      END

      IF USER.IS.A.WAREHOUSE AND NOT(IN.INVOICE.ACCT) AND (OH.WHOUSE.TYPE = "S" OR OH.WHOUSE.TYPE = "T") THEN
         CALL ERR(ERR.MSG(13),"")
         GOTO 34300
      END

      IF SALE.TYPE = "G" THEN
         IF ORDER.HEADER.REC(13) = "" AND ORDER.HEADER.REC(14) = "" THEN
            CALL ERR(ERR.MSG(9),"")
            GOTO 34300
         END
      END
      IF SALE.TYPE = 'M' AND (DELIV.IND = '3' OR DELIV.IND = '5') AND TRIM(OH.CT.ORD.NO,' ','A') = '' THEN
         KEY = ORDER.HEADER.KEY[2,1] : ORDER.HEADER.KEY[7,4] : ORDER.HEADER.KEY[1,1]
         CALL ERR(ERR.MSG(11) : ' ' : KEY,'')
         GOTO 34300
      END

      GOSUB 1000                         ; * CHECK THAT ORDER HAS AT LEAST 1 LINE

      IF NO.ORD.LINES = 1 THEN
         CALL ERR(ERR.MSG(10),"")
         GOTO 34300
      END

      IF NOT(DCOUNT(OH.DELIV.LOAD<1,1>, "*") = 4) THEN
         IF SALE.TYPE = "F" AND FIELD(OH.DELIV.LOAD,"*",2) > DATE() THEN

            LORRY.REGISTRATION = FIELD(OH.DELIV.LOAD,"*",3)
            LORRY.DATE.DELIVERY = FIELD(OH.DELIV.LOAD,"*",2)
            KEY = ORDER.HEADER.KEY[2,1] : ORDER.HEADER.KEY[7,4] : ORDER.HEADER.KEY[1,1]
            LORRY.FUTURE.ERR.MSG = "TICKET ":KEY:" IS ON LORRY ":LORRY.REGISTRATION:" FOR DELIVERY ON ":OCONV(LORRY.DATE.DELIVERY,"D2/"):" CANNOT CONFIRM"
            CALL ERR(LORRY.FUTURE.ERR.MSG,"")
            GOTO 34300

         END
      END ELSE
         IF SALE.TYPE = "F" AND FIELD(OH.DELIV.LOAD,"*",1) > DATE() THEN

            LORRY.DATE.DELIVERY = FIELD(OH.DELIV.LOAD,"*",1)
            KEY = ORDER.HEADER.KEY[2,1] : ORDER.HEADER.KEY[7,4] : ORDER.HEADER.KEY[1,1]
            LORRY.FUTURE.ERR.MSG = "TICKET ":KEY:" IS SCHEDULED IN OPENFLEET FOR DELIVERY ON " : OCONV(LORRY.DATE.DELIVERY, "D2/") : " CANNOT CONFIRM"
            CALL ERR(LORRY.FUTURE.ERR.MSG,"")
            GOTO 34300

         END
      END

      IF SALE.TYPE = "B" AND OH.PARTIAL # "" THEN
         CONTINUE.WITH.DELCONF = @TRUE

         GOSUB IS.THIS.TICKET.COMPLETE

         IF NOT(CONTINUE.WITH.DELCONF) THEN
            GOTO 34300
         END
      END

      RETURN

***************************************************************************
34300:* Abnormal Exit
*********************

      RELEASE ORDER.HEADER.FV,ORDER.HEADER.KEY

      BEGIN CASE
         CASE CALLED.FROM = 34
            RETURN TO 34
         CASE CALLED.FROM = 355
            RETURN TO 35
      END CASE

      GOTO 99999                         ; * Called from illegal module

***************************************************************************
35:* Get To Order Number
************************

      GOSUB 351                          ; * Input Order Number
      GOSUB 352                          ; * Check Order Number of Correct Type

      CALLED.FROM = 35

      GOSUB 353                          ; * Check Order Same Type as in From Order No.
      GOSUB 354                          ; * Check Order No. >= From Order No.
      GOSUB 355                          ; * Read Order Header Record

      RETURN

**************************************************************************
351:* Input Order Number
************************

      IF HOLD.TO.SP.NUMBER # "" THEN
         DISPLAY.SP.NO = HOLD.TO.SP.NUMBER
      END ELSE
         DISPLAY.SP.NO = FROM.SP.NUMBER
      END
      CRT.VAR = @(48,10):DISPLAY.SP.NO ; CRT CRT.VAR:

250:  CRT.VAR = @(48,10): "" ; CRT CRT.VAR:
      INPUT INPUT.SP.NUMBER,6_

      IF DISPLAY.SP.NO MATCHES "1A4X1N" AND INPUT.SP.NUMBER = "" THEN
         TO.SP.NUMBER = DISPLAY.SP.NO
      END ELSE
         TO.SP.NUMBER = INPUT.SP.NUMBER
         DISPLAY.SP.NO = INPUT.SP.NUMBER
      END

      BEGIN CASE
         CASE TO.SP.NUMBER = "."
            CALL ERR(ERR.MSG(3),"")
            GOTO 250
         CASE TO.SP.NUMBER = "/"
            GOTO BEFORE.START

         CASE NOT(TO.SP.NUMBER MATCHES "1A4X1N")
            CALL ERR(ERR.MSG(4), "")
            GOTO 250

         CASE TO.SP.NUMBER[6,1] # 0
            CALL ERR(ERR.MSG(6),"")
            GOTO 250

         CASE 1
            IF TO.SP.NUMBER[1,1] # "M" OR NOT(USER.IS.A.WAREHOUSE) THEN
               IF NOT(TO.SP.NUMBER MATCHES "1A5N") THEN
                  CALL ERR(ERR.MSG(4),"")
                  GOTO 250
               END
            END

      END CASE

      SP.NUMBER = TO.SP.NUMBER

      RETURN

***************************************************************************
352:* Check Order Number of Correct Type
****************************************

      CALLED.FROM = 352
      GOSUB 342                          ; *Check Order Number of Correct Type

      RETURN

***************************************************************************
353:* Check Order Same Type as in From Order Number
***************************************************

      IF SP.NUMBER[1,1] NE FROM.SP.NUMBER[1,1] THEN
         CALL ERR("SALE TYPE MUST BE OF THE SAME TYPE","")

         BEGIN CASE
            CASE CALLED.FROM = 35
               RETURN TO 35
            CASE CALLED.FROM = 3622
               RETURN TO 36
         END CASE

         GOTO 99999                      ; *Called from illegal module
      END

      RETURN

**************************************************************************
354:* Check Order No. >= From Order No.
***************************************

      IF TO.SP.NUMBER[2,4] < FROM.SP.NUMBER[2,4] THEN
         CALL ERR("SP NUMBER CANNOT BE LESS THEN ":FROM.SP.NUMBER:" ","")
         RETURN TO 35
      END

      RETURN

**************************************************************************
355:* Read Order Header Record
******************************

      CALLED.FROM = 355
      GOSUB READ.ORDER.HEADER

      RETURN

***************************************************************************
36:* Get Excluded Order Numbers
*******************************

      GOSUB 361                          ; * Input Order Number
      IF SP.NUMBER = " " OR SP.NUMBER = "" THEN
         NOS = 10
         RETURN
      END
      GOSUB 362                          ; * Validate Order Number
      EXC.NO(NOS) = SP.NUMBER

      RETURN

***************************************************************************
361:* Input Order Number
************************

      DISPLAY.SP.NO = HOLD.EXC.NO(NOS)
      CRT.VAR = @(C,L):DISPLAY.SP.NO ; CRT CRT.VAR

260:  CRT.VAR = @(C,L):"" ; CRT CRT.VAR:

      INPUT.SP.NO = ""
      INPUT INPUT.SP.NO,6_
      IF INPUT.SP.NO = "" AND DISPLAY.SP.NO # "" THEN
         SP.NUMBER = DISPLAY.SP.NO
      END ELSE
         SP.NUMBER = INPUT.SP.NO
         DISPLAY.SP.NO = INPUT.SP.NO
      END

      BEGIN CASE
         CASE SP.NUMBER = "."
            CALL ERR(ERR.MSG(3),"")
            GOTO 260

         CASE SP.NUMBER = "/"
            GOTO BEFORE.START

         CASE SP.NUMBER = ""

         CASE SP.NUMBER = " "
            FOR N=NOS TO 10
               CRT CLEAR.EXC(N)
            NEXT N
            CRT.VAR = @(45,19):" " ; CRT CRT.VAR:

         CASE NOT(SP.NUMBER MATCHES "1A4X1N")
            CALL ERR(ERR.MSG(4), "")
            GOTO 260

         CASE SP.NUMBER[6,1] # 0
            CALL ERR(ERR.MSG(6),"")
            GOTO 260

         CASE 1
            IF SP.NUMBER[1,1] # "M" OR NOT(USER.IS.A.WAREHOUSE) THEN
               IF NOT(SP.NUMBER MATCHES "1A5N") THEN
                  CALL ERR(ERR.MSG(4),"")
                  GOTO 260
               END
            END

      END CASE

      RETURN

*************************************************************************
362:* Validate Order Number
***************************

      GOSUB 3621                         ; * Check Order Number of Correct Type
      GOSUB 3622                         ; * Check Order Same Type as in "From" SP.NO.
      GOSUB 3623                         ; * Check Order is in Range From - To

      RETURN

*************************************************************************
3621:* Check Order Number of Correct Type
*****************************************

      CALLED.FROM = 3621
      GOSUB 342                          ; * Check Order No. of Correct Type

      RETURN

*************************************************************************
3622:* Check Order No. of Same Type
***********************************

      CALLED.FROM = 3622
      GOSUB 353                          ; * Check Order No. of Same Type

      RETURN

***********************************************************************
3623:* Check Order is in Range From - To SP. Number
***************************************************

      IF SP.NUMBER < FROM.SP.NUMBER OR SP.NUMBER > TO.SP.NUMBER THEN
         CALL ERR("SP NUMBER ":SP.NUMBER:" OUT OF RANGE","")
         RETURN TO 36
      END

      RETURN

****************************************************************************
GET.OK:
*******

      CRT HELP.MSG(5):
      CRT.VAR = @(40,19):"OK? :" ; CRT CRT.VAR:

      INPUT OK,1_
      BEGIN CASE
         CASE OK = "N"
            HOLD.ORD.DELIV.DATE = ORD.DELIV.DATE
            HOLD.FROM.SP.NUMBER = FROM.SP.NUMBER
            HOLD.TO.SP.NUMBER = TO.SP.NUMBER

            FOR T = 1 TO 10
               HOLD.EXC.NO(T)= EXC.NO(T)
            NEXT T
            RETURN TO MAIN.PROCESS

         CASE OK = "/"
            RETURN TO START.OF.PROGRAM

         CASE OK # "Y"
            CALL ERR(ERR.MSG(5),"")
            GOTO GET.OK
      END CASE

      RETURN

**************************************************************************
IS.ORDER.UPDATE.RQD:
*********************

      FOR NOS = 1 TO 10
         IF ORD.KEY = EXC.NO(NOS) THEN
            RETURN                       ; *Increment SP No.
         END
      NEXT NOS

      *  tell people the header is locked
      *  release the records locked in other places, this keeps it nice and neat.
      RELEASE
      READ.DONE = 0
      LOOP UNTIL READ.DONE DO
         MATREADU ORDER.HEADER.REC FROM ORDER.HEADER.FV,ORDER.HEADER.KEY LOCKED
            CALL ERR('Header record ':ORDER.HEADER.KEY:' is locked by terminal ':STATUS():' , trying again.','S2')
         END THEN
            READ.DONE = 1
         END ELSE
            RETURN
         END
      REPEAT

      IF SALE.TYPE = 'M' AND (DELIV.IND = '3' OR DELIV.IND = '5') AND TRIM(OH.CT.ORD.NO,' ','A') = '' THEN
         KEY = ORDER.HEADER.KEY[2,1] : ORDER.HEADER.KEY[7,4] : ORDER.HEADER.KEY[1,1]
         CALL ERR(ERR.MSG(11) : ' ' : KEY,'')
         RETURN
      END

      * If this ticket is loaded on a lorry in the future do not delivery confirm it
      IF NOT(DCOUNT(OH.DELIV.LOAD<1,1>, "*") = 4) THEN
         PLANNED.DELIV.DATE = FIELD(OH.DELIV.LOAD,"*",2)
      END ELSE
         PLANNED.DELIV.DATE = FIELD(OH.DELIV.LOAD,"*",1)
      END

      IF PLANNED.DELIV.DATE > DATE() THEN
         KEY = ORDER.HEADER.KEY[2,1] : ORDER.HEADER.KEY[7,4] : ORDER.HEADER.KEY[1,1]
         CRT HLINE:
         CALL ERR(ERR.MSG(8) : ' ' : KEY,'')
         RETURN
      END

      IF USER.IS.A.WAREHOUSE AND NOT(IN.INVOICE.ACCT) THEN
         IF OH.WHOUSE.TYPE # "S" AND OH.WHOUSE.TYPE # "T" THEN
            GOSUB UPDATE.DVY.NOTE
         END

      END ELSE

         IF DEL.CONF.DTE = "" OR (OH.ORD.STAT = "AA" AND DEL.CONF.DTE # "") THEN
            CONFIRM.MOBILE.APP.COLLECTION = (MOBILE.APP AND SALE.TYPE = "E" AND OH.ORD.STAT = "IC")
            IF OH.ORD.STAT = "DP" OR OH.ORD.STAT = "AA" OR OH.ORD.STAT = "CI" OR CONFIRM.MOBILE.APP.COLLECTION THEN
               GOSUB UPDATE.DVY.NOTE
            END
         END
      END

      RETURN

**************************************************************************
UPDATE.DVY.NOTE:
*****************

      *** ONLY IF THERE IS AT LEAST 1 ORDER LINE
      READ ORD.LINE.REC FROM ORDER.LINE.FV,"001":ORDER.HEADER.KEY THEN

         * If 'Delivered' and Branch has lorry's, then Lorry reg required
         IF INDEX("BDFQT",SALE.TYPE,1) AND OH.PARTIAL # 'COLLECTED' THEN
            IF BPB.LORRY.REGISTRATIONS # "" AND OH.LORRY.REG<1,1> = "" THEN

               ORDER.MESSAGE = "Order:- ":ORDER.HEADER.KEY[2,1]:ORDER.HEADER.KEY[7,4]:ORDER.HEADER.KEY[1,1]:" Requires a Lorry Registration"

               CALL TPCRT(18,12,ORDER.MESSAGE,"","-4","","","","")
               CALL TPCRT(0,22," ","","-4","","","","")

               * pass in flag
               * to ensure load scheduling is not carried out
               MAND = "1":AM:"Y"
               SC.CURR = ""
               SC.PREV = ""

               CALL BS.OE.BRANCH.LORRY("D",13,4,MAND,RETN.REG)

               * Exit if 'abort'
               IF RETN.REG = '/' THEN
                  CALL ERR("DELIVERY HAS NOT BEEN CONFIRMED - CONFIRM WHEN LORRY KNOWN","")
                  CRT.VAR = @(0,12):@(-3) ; CRT CRT.VAR:
                  RETURN
               END

               * Update order header
               OH.LORRY.REG = RETN.REG

            END

         END

         GOSUB CHECK.ORD.QTY

         IF QTY.ERR THEN
            RETURN
         END

         DEL.CONF.DTE = INPUT.DATE

         * record date confirmation was actually made
         OH.DATE.DEL.CONF = DATE()

         GOSUB CHECK.FOR.SUPPLIERS

         MATREAD POS.CUSTOMER.REC FROM POS.CUSTOMER.FV, CUST.CODE ELSE
            MAT POS.CUSTOMER.REC = ""
         END

         *** account direct/delivered ...check customer for delivery time tracking
         DO.CONF = 1
         IF SALE.TYPE = "F" OR SALE.TYPE = "G" THEN

            IF PC.DELIVERY.TRACKING = "Y" THEN
               GOSUB GET.DELIVERY.TIME
            END

            **** if performance monitored then double check date if not as requested
            IF DO.CONF THEN
               IF PC.DELIV.CONF.CHK = "Y" THEN
                  IF DEL.CONF.DTE > DELIV.DATE THEN
                     GOSUB CHECK.DELIVERY.DATE
                  END
               END
            END
         END

         IF DO.CONF THEN
            GOSUB POSS.CALL.MORRISON.TYPE.WEB.SERVICE
            IF CONTINUE.WITH.DELCONF THEN
               GOSUB DO.CONFIRMATION
            END
         END
      END
      RETURN

********************************************************************
DO.CONFIRMATION:
*****************

      IF SALE.TYPE = "F" OR SALE.TYPE = "G" THEN
         *  check to see if xml already sent
         IF PC.EDI.ORD.CONF = "Y" AND OH.XML.SENT = "" THEN

            COMPLETE = 0
            LOOP UNTIL COMPLETE DO
               COMPLETE = 1
               SUCCESS = ""
               RESULT = ""
               CALL OS.CREATE.REIMS.XML.ORDER("ORDER","ORDCONF",ORDER.HEADER.KEY,SUCCESS,RESULT,DEL.CONF.DTE,'','','','')
               IF NOT(SUCCESS) THEN
                  COMPLETE = 0
                  CALL DSPLY.BOX(17,8,48,10,"Y",SY.PARAMS.FV,TERMWORK.FV)

                  CALL TPCRT(42,9,NORMAL,"","","","","","")
                  CALL TPCRT(36,9,TCULDIM:"ERROR":NORMAL,"","","","","","")
                  CALL TPCRT(19,10,ORDER.HEADER.KEY[2,1]:ORDER.HEADER.KEY[7,4]:ORDER.HEADER.KEY[1,1]:" cannot be confirmed for the following","","","","","","")
                  CALL TPCRT(19,11,"reason","","","","","","")
                  CALL TPCRT(19,13,RESULT'L#42',"","","","","","")
                  CALL TPCRT(19,14,RESULT[43,40],"","","","","","")
                  CALL TPCRT(30,16,"(R)etry or (C)ontinue","","","","","","")
                  CALL TPCRT(0,22," ","","-4","","","","")

                  CALL INP(52,16,XMLRETRY,1,"L#1","",CHANGE("R,C,r,c",",",@VM),1,0,0,"","Enter 'R' to retry or 'C' to Continue","")

                  IF XMLRETRY = "C" OR XMLRETRY = 'c' THEN
                     COMPLETE = 1
                  END

                  *  set xml sent flag to order header once success @TRUE
               END ELSE
                  OH.XML.SENT = DATE()
               END

            REPEAT
         END
      END

      IF SALE.TYPE = "B" OR SALE.TYPE = "D" THEN

         ** Make CODs with nothing to pay "CI"
         IF OH.SALE.TYPE = 'D' AND OH.ORD.STAT # 'CI' AND TOT.BAL.DUE = 0 THEN
            OH.ORD.STAT = "CI"
            OH.INV.DATE = DATE()
            CALL BS.ORDER.ADJUSTMENT.PAY(ORDER.HEADER.KEY, MAT ORDER.HEADER.REC, '', '', SUCCESS,'','','','','','')
         END

         OH.CASH.DEL.CONF = DATE()

      END ELSE
         IF OH.WMS.CUSTOMER # '' THEN
            OH.WMS.STATUS = "DC"
            MATBUILD REC.DYN FROM ORDER.HEADER.REC
            CALL WMS.TRANSACTION.AUDIT(ORDER.HEADER.KEY,"ORDER-HEADER",REC.DYN,'',@LOGNAME,'','','',PROGRAM.NAME,'','','','','')
         END
         IF OH.ORD.STAT # "ZZ" THEN
            OH.ORD.STAT = "IC"
         END

      END

      ** Create audit trail on ORDER-HEADER for POS review
      ERROR.CODE = ''
      ERROR = ''

      **  as status does not change on B tickets need to show
      **  something else on audit trail when delconf
      AUDIT.TYPE = ''
      IF (OH.CASH.DEL.CONF OR DEL.CONF.DTE) THEN
         AUDIT.TYPE = "DELC"
         IF OH.SMS.EPOS.TRANS.KEY # "" AND INDEX(OH.SMS.EPOS.TRANS.KEY,'*',1) = 0 THEN

            OH.SMS.EPOS.TRANS.KEY = FIELD(OH.SMS.EPOS.TRANS.KEY,'*',1,1):'*':ORDER.HEADER.KEY[3,4]:'*':DATE():'*':TIME()
            TRANS.CASH = ''
            READV CUST.REC FROM POS.CUSTOMER.FV, CUST.CODE, 1 THEN
               TRANS.CASH = 'A'
            END ELSE
               TRANS.CASH = 'C'
            END

            MAT SMS.TEXT.EPOS.TRANS.REC = ''
            SMSE.TRANS.TYPE = TRANS.CASH
            SMSE.ACCOUNT.DETAILS = CUST.CODE
            SMSE.TRANSACTION.ID = ORDER.HEADER.KEY
            SMSE.OPTOUT = 'N'
            SMSE.TICKET.TYPE = 'DELIVERED'

            MATWRITE SMS.TEXT.EPOS.TRANS.REC TO SMS.TEXT.EPOS.TRANS.FV, OH.SMS.EPOS.TRANS.KEY

         END
      END

      CALL BS.OE.ORDER.HEADER.AUDIT(MAT ORDER.HEADER.REC,AUDIT.TYPE,ERROR,ERROR.CODE,'','','','','')

      IF ERROR.CODE =-1 THEN
         FATAL.ERROR.MSG = "BS.OE.ORDER.HEADER.AUDIT CANNOT OPEN FILES ": ERROR
         GOSUB FATAL.ERROR
      END

      BPB.USING.CRM = TRANS("BRANCH-PARAMS",BRANCH.CODE,106,"X")

      IF (BPB.USING.CRM = "Y") THEN
         QUOTE.ID = "" ; PLAN.ID = ""
         IF MAN.BTCH # "" THEN
            QUOTE.ID = BRANCH.CODE:"|":MAN.BTCH
            READV PLAN.ID FROM QUOTE.HEADER.FV, QUOTE.ID, 139 ELSE
               PLAN.ID = ""
            END
         END

         CALL CRM.FILE.TICKET.DETAILS("S", CUST.CODE, ORDER.HEADER.KEY, QUOTE.ID, PLAN.ID, K.CRM.ACTIVITY.DETAIL, '', '', '', '', '', '', '', '', '', '')
         * Update the activity detail key onto the Order Header
         OH.CRM.ACTIVITY.ID = K.CRM.ACTIVITY.DETAIL
      END

      GOSUB CANCEL.DELIV.MAN.JOBS

      IF DELIV.IND = 3 AND SALE.TYPE = "M" THEN
         IBT.ORDER.HEADER.ID = ORDER.HEADER.KEY
         MATBUILD IBT.ORDER.HEADER.REC FROM ORDER.HEADER.REC

         UPDATE.SOURCE.ORDER = @TRUE
      END

      MATWRITE ORDER.HEADER.REC ON ORDER.HEADER.FV,ORDER.HEADER.KEY

      IF BRN.CONTROL.REC<1> = "Y" THEN
         ORD.TRACK.REC = ""
         ORD.TRACK.REC<1>=AUTH.ID
         ORD.TRACK.REC<2> = "8"
         TRACK.DATE=SYS.DATE:"::::::"
         TRACK.DATE=TRACK.DATE[1,6]
         ORD.TRACK.KEY = ORDER.HEADER.KEY:TRACK.DATE:TIME()
         WRITE ORD.TRACK.REC ON ORD.TRACK.FV,ORD.TRACK.KEY
      END

      IF OH.DIARY.KEY # "" THEN
         DIARY.NO = OH.DIARY.KEY
         DIARY.ERROR = ''
         MAT OS.ECOMMERCE.DIARY.REC = ''
         TEXT = 'Order Delivery Confirmed. Ref:':ORDER.HEADER.KEY[2,1]:ORDER.HEADER.KEY[7,4]:ORDER.HEADER.KEY[1,1]
         CALL OS.DIARY.UPDATE("ADD",'',MAT OS.ECOMMERCE.DIARY.REC,TEXT,'',DIARY.NO,'Y',BRANCH.CODE,@LOGNAME,'','',CUST.CODE,DIARY.ERROR,'','','')
      END

      IF SALE.TYPE # "B" AND SALE.TYPE # "D" THEN
         CONF.ACCT.XREF.REC=""
         WRITE CONF.ACCT.XREF.REC ON CONF.ACCT.XREF.FV,ORDER.HEADER.KEY
      END

      RETURN

**************************************************************************
*    CHECK THAT THE ORDER HAS AT LEAST 1 LINE
**************************************************************************

1000: O.LN.CNT = 0
      READ.ERR = 0
      NO.ORD.LINES = 1
      LOOP UNTIL READ.ERR = 1 DO
         O.LN.CNT = O.LN.CNT + 1
         READV ORD.DEL.FLG FROM ORDER.LINE.FV,O.LN.CNT"R%3":ORDER.HEADER.KEY,24 THEN
            IF ORD.DEL.FLG NE "Y" THEN
               NO.ORD.LINES = 0
            END
         END ELSE
            READ.ERR = 1
         END
      REPEAT

      RETURN

**************
CHECK.ORD.QTY:
**************

      QTY.ERR = 0
      FOR LINE.NO = 1 TO NO.OF.LINES
         READV ORD.QTY FROM ORDER.LINE.FV, LINE.NO"R%3":ORDER.HEADER.KEY,2 THEN READV ORD.DEL.FLG FROM ORDER.LINE.FV, LINE.NO"R%3":ORDER.HEADER.KEY,24 THEN
            IF ORD.QTY = 0 AND ORD.DEL.FLG # "Y" THEN
               ERR.MSG(11) = "LINE ":LINE.NO:" OF ORDER ":ORD.KEY: " HAS A QUANTITY OF 0,  UNABLE TO CONFIRM THIS ORDER."
               CALL ERR(ERR.MSG(11),"")
               LINE.NO = NO.OF.LINES
               QTY.ERR = 1
            END
         END
      NEXT LINE.NO

      RETURN

**************************************************************************
GET.DELIVERY.TIME:
******************

      CALL DSPLY.BOX(5,4,70,11,'Y',SY.PARAMS.FV,TERMWORK.FV)

      CRT.VAR = @(38,4):SPACE(1):ORD.KEY:SPACE(1) ; CRT CRT.VAR
      CRT.VAR = @(6,5):("Customer         : ":CUST.CODE:SPACE(1):PC.CUST.NAME)'L#68' ; CRT CRT.VAR
      CRT.VAR = @(6,6):("Delivery Address : ":DELIV.ADDR.1)'L#68' ; CRT CRT.VAR
      CRT.VAR = @(6,7):("                 : ":DELIV.ADDR.2)'L#68' ; CRT CRT.VAR
      CRT.VAR = @(6,8):("                 : ":DELIV.ADDR.3)'L#68' ; CRT CRT.VAR
      CRT.VAR = @(6,9):("Total Goods Val  : ":TOT.GOOD.AMT'R2')'L#68' ; CRT CRT.VAR
      CRT.VAR = @(6,11):("Enter The EXACT Delivery Time For This Order : ")'L#68' ; CRT CRT.VAR
      CRT.VAR = @(6,12):"(Format Should be hh:mm:ss)" ; CRT CRT.VAR
      CRT.VAR = @(6,13):("If the time is not known enter /. The delivery will not be confirmed") 'L#68' ; CRT CRT.VAR
      CRT.VAR = @(0,22):CLEOL ; CRT CRT.VAR

      HELP = "Format Should be hh:mm:ss or / if time not known(delivery is not confirmed)"
      VALID = 0
      LOOP UNTIL VALID
         CRT CURSOR.ON:
         CALL INP(53,11,OH.DELIVERY.TIME,8,"L#8","MTS","",1,0,0,CTL,HELP,"")
         CRT CURSOR.OFF:

         BEGIN CASE
            CASE CTL = '/'
               VALID = 1
               OH.DELIVERY.TIME = ""
               DO.CONF = 0

            CASE CTL # ""
               CALL ERR('YOU CANNOT USE "':CTL:'" HERE - TRY AGAIN','')
               OH.DELIVERY.TIME = ""

            CASE OH.DELIVERY.TIME # ""
               IF OH.DELIVERY.TIME > 86400 THEN
                  CALL ERR('DELIVERY TIME CANNOT BE AFTER 24:00:00 - PLEASE RE-ENTER','')
                  OH.DELIVERY.TIME = ""
               END ELSE
                  RUSUREHELP = "ARE YOU SURE (Y/N) ?"
                  SURE = 0
                  RUSURE = ""
                  LOOP UNTIL SURE
                     CRT CURSOR.ON:
                     LIN = 23
                     CALL INP(21,LIN,RUSURE,1,"L#1","",CHANGE("Y,N,y,n",",",@VM),1,0,0,"",RUSUREHELP,"")
                     CRT CURSOR.OFF:

                     BEGIN CASE
                        CASE RUSURE = "Y" OR RUSURE = "N" OR RUSURE = "y" OR RUSURE = "n"
                           SURE = 1
                           IF (RUSURE = "Y" OR RUSURE = "y") THEN
                              VALID = 1
                           END

                        CASE 1
                           CALL ERR('INVALID ENTRY - PRESS RETURN','')
                           RUSURE = ""
                     END CASE
                  REPEAT
               END

            CASE 1
               CALL ERR('INVALID ENTRY - PRESS RETURN','')
         END CASE
      REPEAT

      CRT CURSOR.ON:

      RETURN

**************************************************************************
CHECK.DELIVERY.DATE:
********************

      CALL DSPLY.BOX(5,4,70,11,'Y',SY.PARAMS.FV,TERMWORK.FV)

      CRT.VAR = @(36,4):" WARNING " ; CRT CRT.VAR
      CRT.VAR = @(18,5):"DELIVERY PERFORMANCE MONITORED CUSTOMER" ; CRT CRT.VAR
      CRT.VAR = @(20,6):CUST.CODE'L#6':" ":CUST.NAME'L#30' ; CRT CRT.VAR
      CRT.VAR = @(6,8):"The delivery confirmation date (":OCONV(ORD.DELIV.DATE<1,1>,'D2/'):") for ticket ":ORD.KEY'L#6' ; CRT CRT.VAR
      CRT.VAR = @(6,9):"is not the date the customer has requested (":OCONV(DELIV.DATE<1,1>,'D2/'):")" ; CRT CRT.VAR
      CRT.VAR = @(6,10):"Is this correct (Y/N) :" ; CRT CRT.VAR
      CRT.VAR = @(0,22):CLEOL ; CRT CRT.VAR

      RUSUREHELP = "Enter Y,N or / to cancel confirmation of this ticket"
      SURE = 0
      RUSURE = ""
      LOOP UNTIL SURE
         CRT CURSOR.ON:
         CALL INP(30,10,RUSURE,1,"L#1","",CHANGE("Y,N,y,n",",",@VM),1,0,0,CTL,RUSUREHELP,"")
         CRT CURSOR.OFF:

         BEGIN CASE
            CASE CTL = "/"
               DO.CONF = 0
               SURE = 1

            CASE RUSURE = "Y" OR RUSURE = "N" OR RUSURE = "y" OR RUSURE = "n"
               IF (RUSURE = "N" OR RUSURE = "n") THEN
                  VALID = 0
                  LOOP UNTIL VALID

                     CRT.VAR = @(6,12):"New Date :" ; CRT CRT.VAR
                     HELP = "Please enter a new delivery confirmation date for this ticket"
                     CRT CURSOR.ON:
                     CALL INP(17,12,DEL.CONF.DTE,8,"L#8","D2/","",1,0,0,CTL,HELP,"")
                     CRT CURSOR.OFF:

                     BEGIN CASE
                        CASE CTL = "/"
                           CRT.VAR = @(6,12):SPACE(30) ; CRT CRT.VAR
                           VALID = 1

                        CASE CTL # ""
                           CALL ERR('YOU CANNOT USE "':CTL:'" HERE - TRY AGAIN','')

                        CASE DEL.CONF.DTE # ""
                           BEGIN CASE
                              CASE DEL.CONF.DTE > DATE()
                                 CALL ERR('DATE CANNOT BE IN THE FUTURE','')

                              CASE DEL.CONF.DTE < OH.DN.DATE
                                 CALL ERR('CANNOT HAVE BEEN DELIVERED BEFORE DELIVERY NOTE PRINTED','')

                              CASE 1
                                 SURE = 1
                                 VALID = 1
                           END CASE

                        CASE 1
                           CALL ERR('INVALID ENTRY - PRESS RETURN','')
                     END CASE
                  REPEAT
               END ELSE
                  SURE = 1
               END

            CASE 1
               CALL ERR('INVALID ENTRY - PRESS RETURN','')
               RUSURE = ""
         END CASE
      REPEAT

      CRT CURSOR.ON:

      RETURN

      INCLUDE DG-INCLUDES DG.TRANS.BOUND

**************************************************************************
CHECK.FOR.SUPPLIERS:
********************

      FOR LINE = 1 TO NO.OF.LINES
         LINE.KEY = LINE'R%3':ORDER.HEADER.KEY
         MATREAD ORDER.LINE.REC FROM ORDER.LINE.FV,LINE.KEY THEN
            IF OL.LAST.RECEIPT.SUPPLIER = '' THEN

               IF OL.PURCHASE.ORDER # '' THEN
                  PO.HEAD.KEY = FIELD(OL.PURCHASE.ORDER,'*',2,2)
                  MATREAD PO.HEADER.REC FROM PO.HEADER.FV,PO.HEAD.KEY ELSE
                     MAT PO.HEADER.REC = ''
                  END
                  MODE = 'PO'
                  MAT BRANCH.STOCK.REC = ''
               END ELSE
                  MATREAD BRANCH.STOCK.REC FROM BS.BRANCH.STOCK.FV,TRM.BRN.CODE:'*':OL.PROD.CODE ELSE
                     MAT BRANCH.STOCK.REC = ''
                  END
                  MAT PO.HEADER.REC = ''
                  MODE = 'BBS'
               END

               CALL BS.GET.LAST.USED.SUPPLIERS(MODE,TRM.BRN.CODE,MAT BRANCH.STOCK.REC,MAT PO.HEADER.REC,LAST.RECEIPTED.SUPP,LAST.BOUGHT.SUPP,'','','','','')

               OL.LAST.RECEIPT.SUPPLIER = LAST.RECEIPTED.SUPP
               OL.LAST.BOUGHT.SUPPLIER = LAST.BOUGHT.SUPP
               MATWRITE ORDER.LINE.REC TO ORDER.LINE.FV,LINE.KEY

            END
         END
      NEXT LINE
      RETURN
**************************************************************************
************************************
POSS.CALL.MORRISON.TYPE.WEB.SERVICE:
************************************
      CONTINUE.WITH.DELCONF = @TRUE
      IF PC.EPOS.VALIDATION<1,2> # 'M' THEN
         RETURN
      END
*** The program is OS.CALL.VALIDATION.WEB.SERVICE
      PROGRAM.TO.CALL = PC.EPOS.VALIDATION<1,1>

* saved conf date and date confirmed before hand so we can put it back
      SAVED.DEL.CONF = DEL.CONF.DTE
      SAVED.OH.DATE.DEL.CONF = OH.DATE.DEL.CONF

      READ WS.ORDREC FROM ORDER.HEADER.FV,ORDER.HEADER.KEY ELSE
         WS.ORDREC = ""
      END
      MO.SUCCESS = @TRUE
      MO.REASON = ""
      CALL @PROGRAM.TO.CALL('CONFIRM',MO.SUCCESS,MO.REASON,
         WS.ORDREC,ORDER.HEADER.KEY,"","","","","")
      MATPARSE ORDER.HEADER.REC FROM WS.ORDREC
      IF NOT(MO.SUCCESS) THEN
         IF MO.REASON = 'TRANSACTION REJECTED' THEN
            REJ.TICKET = ORDER.HEADER.KEY[2,1]:ORDER.HEADER.KEY[7,4]:ORDER.HEADER.KEY[1,1]
            CALL ERR("TICKET ":REJ.TICKET:" REJECTED - CONTACT CUSTOMER ":CUST.CODE,"")
         END
         CONTINUE.WITH.DELCONF = @FALSE
         MATWRITE ORDER.HEADER.REC TO ORDER.HEADER.FV, ORDER.HEADER.KEY
      END ELSE
** put the confirmed date back as will have been lost in the reread and parse of order-header
         DEL.CONF.DTE = SAVED.DEL.CONF
         OH.DATE.DEL.CONF = SAVED.OH.DATE.DEL.CONF
      END
      RETURN

***************************************************************************
IS.THIS.TICKET.COMPLETE:
************************

      TOT.PARTIAL = DCOUNT(OH.PARTIAL<1>,@VM)
      TOT.LOAD = DCOUNT(OH.DELIV.LOAD<1>, @VM)
      TOT.CASH.SPLIT.DELIV.DATE = DCOUNT(OH.CASH.SPLIT.DELIV.DATE<1>, @VM)

      BEGIN CASE
         CASE TOT.BAL.DUE > 0
            CONTINUE.WITH.DELCONF = @FALSE
         CASE OH.DN.DATE = ""
            CONTINUE.WITH.DELCONF = @FALSE
         CASE OH.PARTIAL<1, TOT.PARTIAL> # "COMPLETED" AND TOT.PARTIAL > 0
            CONTINUE.WITH.DELCONF = @FALSE
      END CASE

      IF CONTINUE.WITH.DELCONF AND TOT.LOAD > 1 AND TOT.CASH.SPLIT.DELIV.DATE > 1 THEN
         LINE.QTYS = 0
         LINE.TOTS = 0
         FOR LN = 1 TO NO.OF.LINES
            MATREAD ORDER.LINE.REC FROM ORDER.LINE.FV, LN "R%3" : ORDER.HEADER.KEY THEN
               IF OL.ORD.DEL.FLG # "Y" THEN
                  LINE.TOTS<LN> = OL.ORD.QTY
                  TOT.QTYS = DCOUNT(OL.QUANTITY.TAKEN<1>, @VM)
                  FOR CNT = 1 TO TOT.QTYS
                     LINE.QTYS<LN> += OL.QUANTITY.TAKEN<1, CNT>
                  NEXT CNT
               END
            END
            IF LINE.TOTS<LN> # LINE.QTYS<LN> THEN
               CONTINUE.WITH.DELCONF = @FALSE
            END
         NEXT LN
      END

      IF CONTINUE.WITH.DELCONF = @FALSE THEN
         CALL ERR("ALL PARTS MUST BE DELIVERED BEFORE YOU CAN CONFIRM", "")
      END

      RETURN

********************************************************************
CANCEL.DELIV.MAN.JOBS:
**********************

      TOT.DELIV.JOBS = DCOUNT(OH.DELIV.LOAD<1>, @VM)

      FOR JOB.POS = 1 TO TOT.DELIV.JOBS
         DMJR.ID = OH.DELIV.LOAD<1, JOB.POS>
         IF DMJR.ID # "" THEN

            MATREAD DELIV.MAN.JOB.RESULT.REC FROM DELIV.MAN.JOB.RESULT.FV, DMJR.ID THEN
               IF DMJR.CANCELLED = "" AND DMJR.FINISHED = "" THEN
                  CALL DELIV.MAN.CANCEL.JOB(DMJR.ID)
                  OH.DELIV.LOAD<1, JOB.POS> = ""
               END
            END

         END
      NEXT JOB.POS

      RETURN

*************************************************
CHECK.FOR.DUMP.AND.SUNDRY.LINES:
********************************

      SKIP.TICKET = @FALSE
      FOR LINE.NO = 1 TO NO.OF.LINES UNTIL SKIP.TICKET
         ORDER.LINE.KEY = LINE.NO "R%3":ORDER.HEADER.KEY
         READV OL.PROD.CODE FROM ORDER.LINE.FV, ORDER.LINE.KEY, 1 THEN

            IF OL.PROD.CODE = "" THEN
               CALL ERR("ORDER ":ORD.KEY:" LINE ":LINE.NO:" CONTAINS A DUMP CODED ITEM!","")
               SKIP.TICKET = @TRUE
            END
            IF OL.PROD.CODE = "SUNDRY" THEN
               CALL ERR("ORDER ":ORD.KEY:" LINE ":LINE.NO:" CONTAINS A SUNDRY ITEM!","")
               SKIP.TICKET = @TRUE
            END

         END
      NEXT LINE.NO

      RETURN

*************************************************************************
ENTER.Y.OR.N:
*************

      Y.OR.N = ''
      INPUT.COMPLETE = @FALSE
      HELP = "Enter Y or N."

      LOOP UNTIL INPUT.COMPLETE DO
         CTL = ''
         CALL INP(XPOS,YPOS,Y.OR.N,1,'L#1','','Y':VM:'N',1,0,0,CTL,HELP,0)

         BEGIN CASE
            CASE CTL = "/" OR CTL = "<" ; INPUT.COMPLETE = @TRUE ; Y.OR.N = "N"
            CASE CTL NE @NULL ; CALL ERR( HELP,"" )
            CASE Y.OR.N = 'Y' ; INPUT.COMPLETE = @TRUE
            CASE Y.OR.N = 'N' ; INPUT.COMPLETE = @TRUE
            CASE 1
               CALL ERR( HELP,"" )
               Y.OR.N = ''
         END CASE
      REPEAT

      CRT.VAR = CLEAR:SCREENS.REC:@(0,0):DISP.DATE:TOP.RIGHT ; CRT CRT.VAR:
      CRT.VAR = @(48,6):OCONV(ORD.DELIV.DATE,"D2/") ; CRT CRT.VAR

      RETURN

*************************************************************************
DISPLAY.MESSAGE.IN.BOX:
**********************

      MESSAGE.MAX.LEN = 0
      MESSAGE.LTOT = DCOUNT( MESSAGE , AM )
      FOR MESSAGE.LNO = 1 TO MESSAGE.LTOT
         IF LEN( MESSAGE<MESSAGE.LNO> ) GT MESSAGE.MAX.LEN THEN
            MESSAGE.MAX.LEN = LEN( MESSAGE<MESSAGE.LNO> )
         END
      NEXT MESSAGE.LNO

      WIDTH = MESSAGE.MAX.LEN + 6
      XPOS = (78 - WIDTH) / 2
      YPOS = 10 - MESSAGE.LTOT
      DEPTH = MESSAGE.LTOT + 4

      FOR LNO = YPOS TO DEPTH
         CRT @(XPOS-1,LNO):NORMAL
      NEXT LNO

      CALL DSPLY.BOX( XPOS,YPOS,WIDTH,DEPTH,"Y",SY.PARAMS.FV,TERMWORK.FV )
      HEADER.POS = (78 - LEN( MESSAGE<1> ) ) / 2
      LN.MSG = TCREV:MESSAGE<1>:NORMAL

      FOR MESSAGE.LNO = 2 TO MESSAGE.LTOT - 1
         XPOS = ( 78 - LEN( MESSAGE<MESSAGE.LNO> ) ) / 2
         LN.MSG := @( XPOS , YPOS + MESSAGE.LNO + 1 ) : MESSAGE<MESSAGE.LNO>
      NEXT MESSAGE.LNO

      BANNER.POS = (78 - LEN( MESSAGE<MESSAGE.LTOT> ) ) / 2
      LN.MSG := @( BANNER.POS , YPOS + 2 + MESSAGE.LNO ) : MESSAGE<MESSAGE.LTOT>

      CRT @(HEADER.POS,YPOS+1):LN.MSG

      IF WAIT.FOR.ANY.KEY THEN DUMMY = KEYIN()

      RETURN

********************************************************************
TRY.TO.ABORT:
*************

      TRANS.ACTION='A'
      DG.TRANS.FLAG=""

      GOSUB DG.TRANS.BOUND

      IF TRANS.STATUS THEN
         CRT.VAR = SYS.LINE.ON :"Your transaction has been aborted": SYS.LINE.OFF ; CRT CRT.VAR:
         GOTO 99999
      END ELSE
         CRT.VAR = SYS.LINE.ON :"Unable to abort the transaction": SYS.LINE.OFF ; CRT CRT.VAR:
         ABORT
      END

*************************************************************************
99999:* End of Program
**********************

      RELEASE
      STOP

************
FATAL.ERROR:
************

      VAR = ''
      VAR<1,1> = "V1"
      VAR<1,2> = PROGRAM.NAME
      VAR<1,3> = DATE()
      VAR<1,4> = TIME()
      VAR<1,5> = LOWER(SYSTEM(9001))
      VAR<2> = FATAL.ERROR.MSG
      VAR<-1> = "!"

      DATA VAR
      CHAIN "SY.FATAL.ERROR"

      RETURN

   END
