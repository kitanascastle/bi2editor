'Battle Isle II Map Editor

#DIM ALL
#DEBUG ERROR ON
#CONSOLE OFF

#INCLUDE "WIN32API.INC"

#INCLUDE "..\DXLib\direct2dlib.inc"
#INCLUDE ONCE "commdlg.inc"

%COMPILE_VERSION = 1021
$CONFIGFILE = "bi2ed.ini"
$DEBUGLOGFILE = "bieditordebug.log"
$spx = CHR$(8,10,10,12,14,16,16,18,20,22,22,24,24,22,22,20,18,16,16,14,12,10,10,8)
$playercolors = CHR$(16,104,56,32,48,64,80)
%MAXGROUNDSPRITES = 1335
%MAXROADSPRITES = 64
%SPRITESPERPLAYER = 1024
%ROADSPIRTESTART = %MAXGROUNDSPRITES+7*%SPRITESPERPLAYER
%EDITORSPRITESTART = %MAXGROUNDSPRITES+7*%SPRITESPERPLAYER+4*%MAXROADSPRITES
%MAXEDITORSPRITES = 19  '6x Sieg + 6x Niederlage + 2x Landesflagge + 2x L�schen + 3x Highlight
%COUNTRYFLAGS = 65536+12
%MAXCOUNTRYFLAGS = 2
%DFDELETEOBJECT = 65536+14
%DFDELETEUNIT = 65536+15
%SHOPHIGHLIGHT = 65536+16
%SELECTIONHIGHLIGHT = 65536+17
%ERRORHIGHLIGHT = 65536+18
%MAXDFLAYER = 30
%MAXUNDOLEVELS = 20
%MAXPLAYERS = 6
%MAXSHOPS = 1024
%MAXACTIONS = 256  'vorgegeben durch BI2020
%MAXVALIDATE = 100
%MAXTRANSPORTERSLOT = 8
%MAXCUSTOMMSG = 16
%CUSTMSGMAXLEN = 2000
$CUSTOMMSGIDS = "USER00;USER01;USER02;USER03;USER04;USER05;USER06;USER07;USER08;USER09;USER10;USER11;USER12;CUTSCENE;BRIEFING;BONUSMAP"

%MAXLANGUAGES = 2  'deutsch + englisch
%WORDSTART_BUTTON = 0
%WORDSTART_ACTIONSCREEN = 10
%WORDSTART_ACTIONLIST = 30
%WORDSTART_WEATHER = 50
%WORDSTART_VICTORYCONDITION = 60
%WORDSTART_SHOPTYPE = 70
%WORDSTART_AICOMMAND = 80
%WORDSTART_TERRAIN = 90
%WORDSTART_OPTIONS = 100
%WORDSTART_OPENDIALOGUE = 110
%WORDSTART_SAVEQUERY = 120
%WORDSTART_PROPERTIES = 130
%WORDSTART_NEWMAP = 150
%WORDSTART_SPRITEINFO = 160
%WORDSTART_VALIDATION = 170
%WORDSTART_MAPINFO = 180
%WORDSTART_BONUSCONDITION = 190
%WORDSTART_ERROR=200
%WORDSTART_WELCOME = 210
%WORDSTART_MESSAGES = 240

%DIALOGUE_PROPERTIES = 1
%DIALOGUE_ACTIONS = 2
%DIALOGUE_SHOP = 4
%DIALOGUE_OPTIONS = 8
%DIALOGUE_NEWMAP = 16



TYPE TShop  '80 Bytes
  unittype AS INTEGER  '1=Shop , 2=AI-Point
  position AS WORD
  shopfunction AS INTEGER  '1=HQ , 2=Produktion , 4=Depot , 8=Akademie , 16=Stadt , 32=Transporter
  u3 AS INTEGER
  nameindex AS INTEGER  'Index in Shopfile (nur f�r Geb�ude)
  position2 AS WORD  '0 f�r AI-Points
  energy AS INTEGER
  material AS INTEGER
  eplus AS BYTE
  mplus AS BYTE
  content(15) AS INTEGER
  u5 AS BYTE
  shoptype AS BYTE  '0=Einheit , 1 = HQ , 2=Flughafen , 4=Hafen , 8=Fabrik , 16=Depot , 32=Stadt , 64=Akademie
  production(3) AS INTEGER
  u6 AS INTEGER
  prio AS INTEGER
  owner AS INTEGER
  aidata1 AS INTEGER
  aidata2 AS INTEGER
  aidata3 AS INTEGER
  aicommand AS INTEGER
  aidata5 AS INTEGER
  aidata6xxx AS INTEGER
  aidata7 AS INTEGER
END TYPE

GLOBAL D2D AS IDIRECT2D
GLOBAL pWindow AS IWindow
GLOBAL EXEPATH$, apperror&, compileDate$
GLOBAL configdata$, selectedLanguage&, reopenLastMap&, bi2020support&, isBI3&, words$()
GLOBAL libFolder$, misFolder$, bi2020folder$
GLOBAL hWIN&, hOldWinProc&, hOldGfxWinProc&
GLOBAL hTEXTFONT&, hMINIFONT&, hBUTTONFONT&, hBIGFONT&
GLOBAL mousexpos&, mouseypos&
GLOBAL windowWidth&, windowHeight&, selectedTab&, showWelcome&, mapChanged&
GLOBAL startterrain$, defaultTerrain&
GLOBAL totalUnitClasses&
GLOBAL sprites&(), nsprites$(), palette$(), pal???(), playerRGB&(), playerUnitOffset&()
GLOBAL paletteCols&, paletteRows&, scrollPaletteToSprite&
GLOBAL missionFileName$, mapnames$(), msgtext$(), unitnames$()
GLOBAL originalMissData$, dfLayerCount&, aiMask&
GLOBAL mapdata%(), mapwidth&, mapheight&, shops() AS TShop, shopnames$(), nshops&, dfdata$, missionnr&
GLOBAL validateMessages$(), validatex&(), validatey&(), nValidate&, showMapError&
GLOBAL gpm?()       'Global Produktions-Palette (64 Words)
GLOBAL allymatrix$  'Allianz-Matrix (6 Bytes)
GLOBAL actions$      'Aktionen (40 Bytes pro Eintrag)
GLOBAL actionCount&  'Anzahl Eintr�ge in actions$
GLOBAL actionsPerPage&  'Anzahl Aktionen, die im Dialog dargestellt werden k�nnen
GLOBAL maparea AS RECT      'Kartenbereich (ohne Rahmen)
GLOBAL palettearea AS RECT  'Palettenbereich (ohne Rahmen)
GLOBAL shoparea AS RECT   'Shopbereich (ohne Rahmen)
GLOBAL allyarea AS RECT   'Allianzenbereich
GLOBAL gpmarea AS RECT   'Allgemeine-Produktionspaletten-Bereich
GLOBAL miscsettingsarea AS RECT  'verschiedene Eigenschaftenbereich
GLOBAL aimaskarea AS RECT   'AI-Maskenbereich
GLOBAL mapdescriptionarea AS RECT  'Karten-Beschreibung
GLOBAL custommessagesarea AS RECT  'eigene Meldungen
GLOBAL newactionarea AS RECT   'neue Aktion-Bereich
GLOBAL actionarea AS RECT   'Aktionslistenbereich
GLOBAL buttonarea AS RECT   'Buttonbereich (ohne Rahmen)
GLOBAL languagearea AS RECT   'Sprachauswahl-Bereich
GLOBAL generaloptionsarea AS RECT   'allgemeine Einstellungen
GLOBAL newmaparea AS RECT  'neue Karte
GLOBAL startterrainarea AS RECT  'Terrain f�r neue Karte
GLOBAL mapSelection AS RECT  'auf Karte markierter Bereich
GLOBAL validationarea AS RECT  'Bereich f�r Validationsmeldungen
GLOBAL customMessages$$()  'benutzerdefinierte Meldungen in Deutsch, Englisch und benutzerdefinierter Sprache
GLOBAL selectedCustomMsg&  'zu bearbeitende benutzerdefinierte Meldung
GLOBAL custMsgUserLang$    '3-Byte-Code der dritten (benutzerdefinierten) Sprache
GLOBAL customterrainsprites&  'zu ladendes Sprite-Set f�r Terrain (PARTxxx.LIB)
GLOBAL customunitsprites&  'zu ladendes Sprite-Set f�r Einheiten (UNITxxx.LIB)
GLOBAL yMapFactor&  '64 f�r BI2 kompatible Karten, 256 f�r Karten gr��er als 64x64
GLOBAL zoom#, scrollX&, scrollY&, dragStartX&, dragStartY&, drawing&, shiftpressed&, ctrlpressed&
GLOBAL highlightedButton&, highlightedActionButton&, highlightedNewActionButton&, highlightShops&
GLOBAL selectedSprite&, selectedLevel&, selectedDF&, selectedShop&, renderedMap$
GLOBAL undoData$(), undoPos&, undoStart&
GLOBAL multiSprites$()
GLOBAL debugdata$()
GLOBAL brushWhite&, brushBlack&, brushGrey96&, brushBorder&, brushBackground&, brushSelectedTab&, brushHighlightedTab&, brushInactiveTab&, brushEditBackground&, brushButton&, brushRed&, brushGreen&, brushGreyscale&()
GLOBAL buttonActions AS IDXCONTROL, buttonProperties AS IDXCONTROL, buttonValidate AS IDXCONTROL, buttonOptions AS IDXCONTROL
GLOBAL buttonNew AS IDXCONTROL, buttonLoad AS IDXCONTROL, buttonSave AS IDXCONTROL, buttonQuit AS IDXCONTROL
GLOBAL buttonDeleteAction() AS IDXCONTROL, buttonNewAction AS IDXCONTROL
GLOBAL listboxWidth AS IDXCONTROL, listboxHeight AS IDXCONTROL
GLOBAL editMapDescription AS IDXCONTROL, editMapShortDescr AS IDXCONTROL, editCustomMessageGER AS IDXCONTROL, editCustomMessageENG AS IDXCONTROL, editCustomMessageUserLang AS IDXCONTROL, editCustomLangCode AS IDXCONTROL
GLOBAL editPlayerName() AS IDXCONTROL
GLOBAL editShopname AS IDXCONTROL, editEnergy AS IDXCONTROL, editMaterial AS IDXCONTROL, editEnergyPlus AS IDXCONTROL, editMaterialPlus AS IDXCONTROL
GLOBAL listboxOwner AS IDXCONTROL, listboxShoptype AS IDXCONTROL, listboxAICommand AS IDXCONTROL
GLOBAL listboxTerrain AS IDXCONTROL, listboxWeather AS IDXCONTROL, listboxWinCond AS IDXCONTROL, listboxNextmap AS IDXCONTROL, listboxBonusmap AS IDXCONTROL, listboxCustomMessages AS IDXCONTROL
GLOBAL listboxPlayer AS IDXCONTROL, listboxTurn AS IDXCONTROL, listboxMovement AS IDXCONTROL, listboxAction AS IDXCONTROL, listboxParam1 AS IDXCONTROL, listboxParam2 AS IDXCONTROL, listboxParam3 AS IDXCONTROL
GLOBAL listboxLevel AS IDXCONTROL, scrollbarActions AS IDXCONTROL
GLOBAL scrollbarTabpages() AS IDXCONTROL
GLOBAL radiogroupLanguage AS IDXCONTROL, checkboxReopen AS IDXCONTROL, checkboxBI2020 AS IDXCONTROL
GLOBAL prodslotspr&, normalslotspr&
GLOBAL createMapFromGifFilename$
GLOBAL mapCreatedFromGif&
GLOBAL enableDebugLog&  'Debug-Logging aktivieren wenn auf 1 gesetzt



'Datei lesen
FUNCTION READFILECONTENT$(f$)
  LOCAL a$, nr&

  IF ISFILE(f$) = 0 THEN
    CALL BIDebugLog("Error: File "+f$+" not found")
    EXIT FUNCTION
  END IF

  nr& = FREEFILE
  OPEN f$ FOR BINARY AS nr&
  GET$ nr&, LOF(nr&), a$
  CLOSE nr&
  CALL BIDebugLog("Reading file "+f$+" ("+FORMAT$(LEN(a$))+" bytes)")

  READFILECONTENT$ = a$
END FUNCTION



'Datei speichern
SUB SAVEFILE(f$, a$)
  LOCAL nr&

  nr& = FREEFILE
  OPEN f$ FOR OUTPUT AS nr&
  PRINT# nr&, a$;
  CLOSE nr&
END SUB



'Schreibt eine Meldung ins Debug-Logfile
SUB BIDebugLog(BYVAL a$)
  LOCAL nr&

  IF enableDebugLog& = 0 THEN EXIT SUB

  nr& = FREEFILE
  OPEN EXEPATH$+$DEBUGLOGFILE FOR APPEND AS nr&
  PRINT# nr&, a$
  CLOSE nr&
END SUB



'ASCII-String in Hex konvertieren
FUNCTION HexString$(a$)
  LOCAL i&, n&, r$

  n& = LEN(a$)
  FOR i& = 1 TO n&
    r$ = r$+HEX$(ASC(a$, i&), 2)
  NEXT i&

  HexString$ = r$
END FUNCTION



'Hex-String in ASCII konvertieren
FUNCTION HexToAsc$(a$)
  LOCAL i&, n&, r$

  n& = LEN(a$)/2
  r$ = STRING$(n&, 0)
  FOR i& = 1 TO n&
    ASC(r$, i&) = VAL("&H"+MID$(a$, i&*2-1, 2))
  NEXT i&

  HexToAsc$ = r$
END FUNCTION



'16-Bit String in 32-Bit String konvertieren
FUNCTION STR16TO32$(a$)
  LOCAL b$, n&, i&

  n& = LEN(a$)/2
  b$ = STRING$(n&*4, 0)
  FOR i& = 1 TO n&
    MID$(b$, i&*4-3, 2) = MID$(a$, i&*2-1, 2)
  NEXT i&

  STR16TO32$ = b$
END FUNCTION



'Index f�r Bezeichner in Word-Array ermitteln
FUNCTION GETWORDINDEX&(BYVAL a$)
  LOCAL p&, nr&, i&

  'Attribut und laufende Nummer trennen
  i& = -1
  p& = INSTR(a$, ANY "123456789")
  IF p& > 0 THEN
    nr& = VAL(MID$(a$, p&))
    a$ = LEFT$(a$, p&-1)
  END IF

  'Attribut auswerten
  SELECT CASE a$
  CASE "BUTTON": IF nr& > 0 AND nr& <= 8 THEN i& = %WORDSTART_BUTTON+nr&-1
  CASE "ACTIONSCREEN": IF nr& > 0 AND nr& <= 20 THEN i& = %WORDSTART_ACTIONSCREEN+nr&-1
  CASE "ACTIONLIST": IF nr& > 0 AND nr& <= 13 THEN i& = %WORDSTART_ACTIONLIST+nr&-1
  CASE "WEATHER": IF nr& > 0 AND nr& <= 5 THEN i& = %WORDSTART_WEATHER+nr&-1
  CASE "VICTORYCONDITION": IF nr& > 0 AND nr& <= 10 THEN i& = %WORDSTART_VICTORYCONDITION+nr&-1
  CASE "BONUSCONDITION": IF nr& > 0 AND nr& <= 4 THEN i& = %WORDSTART_BONUSCONDITION+nr&-1
  CASE "SHOPTYPE": IF nr& > 0 AND nr& <= 7 THEN i& = %WORDSTART_SHOPTYPE+nr&-1
  CASE "AICOMMAND": IF nr& > 0 AND nr& <= 9 THEN i& = %WORDSTART_AICOMMAND+nr&-1
  CASE "TERRAIN": IF nr& > 0 AND nr& <= 4 THEN i& = %WORDSTART_TERRAIN+nr&-1
  CASE "OPTIONS": IF nr& > 0 AND nr& <= 4 THEN i& = %WORDSTART_OPTIONS+nr&-1
  CASE "OPENDIALOGUE": IF nr& > 0 AND nr& <= 2 THEN i& = %WORDSTART_OPENDIALOGUE+nr&-1
  CASE "SAVEQUERY": IF nr& > 0 AND nr& <= 2 THEN i& = %WORDSTART_SAVEQUERY+nr&-1
  CASE "PROPERTIES": IF nr& > 0 AND nr& <= 13 THEN i& = %WORDSTART_PROPERTIES+nr&-1
  CASE "NEWMAP": IF nr& > 0 AND nr& <= 7 THEN i& = %WORDSTART_NEWMAP+nr&-1
  CASE "SPRITEINFO": IF nr& > 0 AND nr& <= 5 THEN i& = %WORDSTART_SPRITEINFO+nr&-1
  CASE "VALIDATION": IF nr& > 0 AND nr& <= 5 THEN i& = %WORDSTART_VALIDATION+nr&-1
  CASE "MAPINFO": IF nr& > 0 AND nr& <= 2 THEN i& = %WORDSTART_MAPINFO+nr&-1
  CASE "ERROR": IF nr& > 0 AND nr& <= 6 THEN i& = %WORDSTART_ERROR+nr&-1
  CASE "WELCOME": IF nr& > 0 AND nr& <= 26 THEN i& = %WORDSTART_WELCOME+nr&-1
  CASE "MESSAGE": IF nr& > 0 AND nr& <= 99 THEN i& = %WORDSTART_MESSAGES+nr&-1
  END SELECT

  GETWORDINDEX& = i&
END FUNCTION



'Konfigurationsdatei einlesen
FUNCTION READCONFIG&
  LOCAL p&, q&, m&, section&, lang&
  LOCAL a$, b$, atr$
  DIM words$(999, %MAXLANGUAGES-1)

  'Datei einlesen
  configdata$ = READFILECONTENT$(EXEPATH$+$CONFIGFILE)
  a$ = configdata$
  REPLACE CHR$(10) WITH CHR$(13) IN a$
  REPLACE CHR$(9) WITH CHR$(32) IN a$
  m& = LEN(a$)
  IF m& = 0 THEN EXIT FUNCTION

  'Datei auswerten
  p& = 1
  WHILE p& <= m&
    q& = INSTR(p&, a$, CHR$(13))
    b$ = TRIM$(MID$(a$, p&, q&-p&))
    p& = q&+1
    IF b$ <> "" THEN
      IF LEFT$(b$, 1) = "[" AND RIGHT$(b$, 1) = "]" THEN
        'neue Sektion
        SELECT CASE UCASE$(MID$(b$, 2, LEN(b$)-2))
        CASE "SETTINGS": section& = 1
        CASE "GERMAN": section& = 2 : lang& = 0
        CASE "ENGLISH": section& = 2 : lang& = 1
        END SELECT
      ELSE
        'Attribut
        q& = INSTR(b$, "=")
        IF q& > 0 THEN
          atr$ = UCASE$(RTRIM$(LEFT$(b$, q&-1)))
          b$ = LTRIM$(MID$(b$, q&+1))
          SELECT CASE section&
          CASE 1:  'Settings
            SELECT CASE atr$
            CASE "LANGUAGE":
              SELECT CASE UCASE$(b$)
              CASE "GER": selectedLanguage& = 0
              CASE "ENG": selectedLanguage& = 1
              END SELECT
            CASE "REOPEN":
              missionFileName$ = b$
              reopenLastMap& = b$ <> ""
            CASE "BI2020":
              IF b$ <> "" AND b$ <> "0" THEN bi2020support& = -1
            CASE "LIBFOLDER":
              libFolder$ = b$
              IF libFolder$ <> "" AND RIGHT$(libFolder$, 1) <> "\" THEN libFolder$ = libFolder$+"\"
            CASE "MISFOLDER":
              misFolder$ = b$
              IF misFolder$ <> "" AND RIGHT$(misFolder$, 1) <> "\" THEN misFolder$ = misFolder$+"\"
            CASE "BI2020FOLDER":
              bi2020folder$ = b$
              IF bi2020folder$ <> "" AND RIGHT$(bi2020folder$, 1) <> "\" THEN bi2020folder$ = bi2020folder$+"\"
            CASE "CUSTOMTERRAINSPRITES":
              customterrainsprites& = VAL(b$)
            CASE "CUSTOMUNITSPRITES":
              customunitsprites& = VAL(b$)
            CASE "DEFAULTCUSTMSGLANG":
              custMsgUserLang$ = LEFT$(b$, 3)
            END SELECT

          CASE 2:  'Sprachen
            q& = GETWORDINDEX&(atr$)
            IF q& >= 0 THEN words$(q&, lang&) = b$
          END SELECT
        END IF
      END IF
    END IF
  WEND

  IF libFolder$ = "" THEN libFolder$ = GetFolderLocation$(EXEPATH$, "LIB")
  IF misFolder$ = "" THEN misFolder$ = GetFolderLocation$(EXEPATH$, "MIS")
  IF bi2020folder$ = "" AND bi2020support& <> 0 THEN bi2020folder$ = FindFileInFolder$(EXEPATH$, "bi2020.exe")

  READCONFIG& = 1
END FUNCTION



'Konfigurationsdatei speichern
SUB SAVECONFIG
  LOCAL a$

  IF apperror& <> 0 THEN EXIT SUB

  CALL REPLACESETTING("SETTINGS", "LANGUAGE", MID$("GERENG", selectedLanguage&*3+1, 3))
  CALL REPLACESETTING("SETTINGS", "REOPEN", IIF$(reopenLastMap&, missionFileName$, ""))
  CALL REPLACESETTING("SETTINGS", "BI2020", FORMAT$(ABS(bi2020support&)))
  CALL REPLACESETTING("SETTINGS", "LIBFOLDER", libFolder$)
  CALL REPLACESETTING("SETTINGS", "MISFOLDER", misFolder$)
  CALL REPLACESETTING("SETTINGS", "BI2020FOLDER", bi2020folder$)
  CALL REPLACESETTING("SETTINGS", "DEFAULTCUSTMSGLANG", custMsgUserLang$)

  CALL SAVEFILE(EXEPATH$+$CONFIGFILE, configdata$)
END SUB



'Konfigurations-String aktualisieren (oder hinzuf�gen)
SUB REPLACESETTING(section$, atr$, newvalue$)
  LOCAL p&, q&, u$, b$

  'Sektionsanfang finden
  u$ = UCASE$(configdata$)
  p& = INSTR(u$, "["+section$+"]")
  IF p& = 0 THEN
    'Sektion nicht gefunden -> am Ende der Datei hinzuf�gen
    configdata$ = RTRIM$(configdata$, ANY CHR$(13,10))+CHR$(13,10,13,10)
    configdata$ = configdata$+"["+section$+"]"+CHR$(13,10)+atr$+"="+newvalue$+CHR$(13,10)
    EXIT SUB
  END IF
  p& = INSTR(p&, u$, CHR$(13,10))
  IF p& = 0 THEN
    configdata$ = configdata$+CHR$(13,10)
    u$ = u$+CHR$(13,10)
    p& = LEN(configdata$)+1
  ELSE
    p& = p&+2
  END IF

  'Attribut finden
  WHILE p& <= LEN(configdata$)
    q& = INSTR(p&, u$, CHR$(13,10))
    IF q& = 0 THEN q& = LEN(configdata$)+1
    b$ = MID$(u$, p&, q&-p&)
    IF LEFT$(REMOVE$(b$, ANY CHR$(9,32)), 1) = "[" THEN EXIT LOOP
    IF LEFT$(REMOVE$(b$, ANY CHR$(9,32)), LEN(atr$)+1) = atr$+"=" THEN
      'Attribut gefunden
      p& = INSTR(p&, configdata$, "=")
      configdata$ = LEFT$(configdata$, p&)+newvalue$+MID$(configdata$, q&)
      EXIT SUB
    END IF
    p& = q&+2
  WEND

  'Attribut nicht gefunden -> am Ende der Sektion hinzuf�gen
  IF p& <= LEN(configdata$) THEN
    WHILE INSTR(CHR$(13,10,9,32)+"[", MID$(configdata$, p&, 1)) > 0
      p& = p&-1
    WEND
  END IF
  configdata$ = LEFT$(configdata$, p&)+CHR$(13,10)+atr$+"="+newvalue$+MID$(configdata$, p&+1)
END SUB



'Liefert ein Wort in der gew�hlten Sprache zur�ck
FUNCTION GETWORD$(nr&)
  GETWORD$ = words$(nr&, selectedLanguage&)
END FUNCTION



'�bersetzt ein deutsches Wort in die gew�hlte Sprache
FUNCTION TRANSLATEWORD$(d$)
  LOCAL i&, u$

  'Wort im W�rterbuch suchen
  u$ = UCASE$(d$)
  FOR i& = 0 TO 999
    IF UCASE$(words$(i&, 0)) = u$ THEN
      TRANSLATEWORD$ = words$(i&, selectedLanguage&)
      EXIT FUNCTION
    END IF
  NEXT i&

  'Wort nicht gefunden
  TRANSLATEWORD$ = d$
END FUNCTION



'BI3 Video-Nachricht auf Video-Datei abbilden (�bernommen aus MCP000.DAT)
FUNCTION MAPBI3VIDEOTOFILENUMBER&(videonr&)
  LOCAL f&

  SELECT CASE videonr&
  CASE 100: f& = 400
  CASE 101: f& = 1
  CASE 102: f& = 2
  CASE 103: f& = 3
  CASE 104: f& = 4
  CASE 105: f& = 421
  CASE 106: f& = 426
  CASE 107: f& = 0
  CASE 108: f& = 501

  CASE 200: f& = 401
  CASE 201: f& = 5
  CASE 202: f& = 6
  CASE 203: f& = 422
  CASE 204: f& = 427

  CASE 300: f& = 402
  CASE 301: f& = 7
  CASE 302: f& = 8
  CASE 303: f& = 9
  CASE 304: f& = 423
  CASE 305: f& = 425
  CASE 306: f& = 502
  CASE 307: f& = 507

  CASE 400: f& = 403
  CASE 401: f& = 10
  CASE 402: f& = 11
  CASE 403: f& = 504
  CASE 404: f& = 420
  CASE 405: f& = 424

  CASE 500: f& = 404
  CASE 501: f& = 12
  CASE 502: f& = 13
  CASE 503: f& = 14
  CASE 504: f& = 15
  CASE 505: f& = 16
  CASE 506: f& = 421
  CASE 507: f& = 425
  CASE 508: f& = 501

  CASE 600: f& = 405
  CASE 601: f& = 17
  CASE 602: f& = 18
  CASE 606: f& = 422
  CASE 607: f& = 427
  CASE 608: f& = 502
  CASE 609: f& = 507
  CASE 610: f& = 502
  CASE 611: f& = 507
  CASE 612: f& = 502
  CASE 613: f& = 507

  CASE 700: f& = 406
  CASE 701: f& = 19
  CASE 702: f& = 20
  CASE 703: f& = 21
  CASE 704: f& = 22
  CASE 705: f& = 23
  CASE 706: f& = 24
  CASE 707: f& = 25
  CASE 708: f& = 421
  CASE 709: f& = 425
  CASE 710: f& = 507
  CASE 711: f& = 503

  CASE 800: f& = 407
  CASE 801: f& = 26
  CASE 802: f& = 27
  CASE 803: f& = 28
  CASE 804: f& = 29
  CASE 805: f& = 30
  CASE 806: f& = 31
  CASE 807: f& = 32
  CASE 808: f& = 507
  CASE 809: f& = 420
  CASE 810: f& = 424

  CASE 900: f& = 408
  CASE 901: f& = 35
  CASE 902: f& = 36
  CASE 903: f& = 37
  CASE 904: f& = 38
  CASE 905: f& = 39
  CASE 906: f& = 40
  CASE 907: f& = 34
  CASE 908: f& = 423
  CASE 909: f& = 426
  CASE 910: f& = 503
  CASE 911: f& = 504
  CASE 912: f& = 507
  CASE 913: f& = 502
  CASE 914: f& = 504
  CASE 915: f& = 507

  CASE 1000: f& = 409
  CASE 1001: f& = 41
  CASE 1002: f& = 42
  CASE 1003: f& = 43
  CASE 1004: f& = 44
  CASE 1005: f& = 45
  CASE 1006: f& = 46
  CASE 1007: f& = 47
  CASE 1008: f& = 422
  CASE 1009: f& = 425
  CASE 1010: f& = 501
  CASE 1011: f& = 502
  CASE 1012: f& = 507
  CASE 1013: f& = 502
  CASE 1014: f& = 507
  CASE 1015: f& = 502
  CASE 1016: f& = 507

  CASE 1100: f& = 410
  CASE 1101: f& = 48
  CASE 1102: f& = 49
  CASE 1103: f& = 50
  CASE 1104: f& = 51
  CASE 1105: f& = 420
  CASE 1106: f& = 426
  CASE 1107: f& = 504

  CASE 1200: f& = 411
  CASE 1201: f& = 52
  CASE 1202: f& = 53
  CASE 1203: f& = 421
  CASE 1204: f& = 424
  CASE 1205: f& = 501
  CASE 1206: f& = 502
  CASE 1207: f& = 507
  CASE 1208: f& = 502
  CASE 1209: f& = 507

  CASE 1300: f& = 412
  CASE 1301: f& = 56
  CASE 1302: f& = 57
  CASE 1303: f& = 58
  CASE 1304: f& = 59
  CASE 1305: f& = 60
  CASE 1306: f& = 61
  CASE 1307: f& = 422
  CASE 1308: f& = 426
  CASE 1309: f& = 501
  CASE 1310: f& = 505
  CASE 1311: f& = 506
  CASE 1312: f& = 500
  CASE 1313: f& = 505
  CASE 1314: f& = 500

  CASE 1400: f& = 413
  CASE 1401: f& = 62
  CASE 1402: f& = 63
  CASE 1403: f& = 64
  CASE 1404: f& = 65
  CASE 1405: f& = 421
  CASE 1406: f& = 427
  CASE 1407: f& = 501
  CASE 1408: f& = 502
  CASE 1409: f& = 507

  CASE 1500: f& = 414
  CASE 1501: f& = 66
  CASE 1502: f& = 67
  CASE 1503: f& = 68
  CASE 1504: f& = 420
  CASE 1505: f& = 426
  CASE 1506: f& = 501
  CASE 1507: f& = 502
  CASE 1508: f& = 507
  CASE 1509: f& = 502
  CASE 1510: f& = 507
  CASE 1511: f& = 502

  CASE 1600: f& = 415
  CASE 1601: f& = 69
  CASE 1602: f& = 70
  CASE 1603: f& = 71
  CASE 1604: f& = 72
  CASE 1605: f& = 73
  CASE 1606: f& = 74
  CASE 1607: f& = 75
  CASE 1608: f& = 76
  CASE 1609: f& = 77
  CASE 1610: f& = 421
  CASE 1611: f& = 425

  CASE 1700: f& = 416
  CASE 1701: f& = 79
  CASE 1702: f& = 80
  CASE 1703: f& = 81
  CASE 1704: f& = 82
  CASE 1705: f& = 83
  CASE 1706: f& = 84
  CASE 1707: f& = 78
  CASE 1708: f& = 422
  CASE 1709: f& = 426
  CASE 1710: f& = 501
  CASE 1711: f& = 505
  CASE 1712: f& = 500
  CASE 1713: f& = 505
  CASE 1714: f& = 500

  CASE 1800: f& = 417
  CASE 1801: f& = 85
  CASE 1802: f& = 86
  CASE 1803: f& = 87
  CASE 1804: f& = 88
  CASE 1805: f& = 89
  CASE 1806: f& = 420
  CASE 1807: f& = 427
  CASE 1808: f& = 501
  CASE 1809: f& = 505
  CASE 1810: f& = 506
  CASE 1811: f& = 500
  CASE 1812: f& = 505
  CASE 1813: f& = 506
  CASE 1814: f& = 500

  CASE 1900: f& = 418
  CASE 1901: f& = 90
  CASE 1902: f& = 91
  CASE 1903: f& = 92
  CASE 1904: f& = 93
  CASE 1905: f& = 94
  CASE 1906: f& = 421
  CASE 1907: f& = 425
  CASE 1908: f& = 501

  CASE 2000: f& = 419
  CASE 2001: f& = 95
  CASE 2002: f& = 96
  CASE 2003: f& = 97
  CASE 2004: f& = 98
  CASE 2005: f& = 998
  CASE 2006: f& = 422
  CASE 2007: f& = 426
  CASE 2008: f& = 501

  CASE ELSE: f& = videonr&
  END SELECT

  MAPBI3VIDEOTOFILENUMBER& = f&
END FUNCTION



'Farbwert in Terrain umwandeln
SUB SetTerrainFromRGB(c&, x&, y&)
  LOCAL hue!, saturation!, brightness!
  LOCAL a$

  D2D.BGRtoHSB(c&, hue!, saturation!, brightness!)  'Farbton: 0=Rot , 60=Gelb , 120=Gr�n , 180=T�rkis , 240=Blau , 300=Lila

  IF saturation! < 0.1 THEN
    mapdata%(x&, y&, 0, 0) = 271
    mapdata%(x&, y&, 1, 0) = 288  'H�gel
  ELSE
    SELECT CASE hue!
    CASE 216 TO 300:  'blau
      mapdata%(x&, y&, 0, 0) = 97-INT(brightness!*2.9)  'Wasser
    CASE 144 TO 216:  't�rkis
      mapdata%(x&, y&, 0, 0) = 387-INT(brightness!*2.9)  'Wasser (Dschungel)
    CASE 90 TO 120:  'gr�n (Richtung gelb)
      IF brightness! <= 0.4 THEN
        mapdata%(x&, y&, 0, 0) = 271
        mapdata%(x&, y&, 1, 0) = 243  'Wald auf Gras
      ELSE
        a$ = MKI$(272)+MKI$(274)+MKI$(271)+MKI$(273)
        mapdata%(x&, y&, 0, 0) = CVI(a$, INT((brightness!-0.4)*6.66)*2+1)  'Gras
      END IF
    CASE 120 TO 144:  'gr�n (Richtung t�rkis)
      mapdata%(x&, y&, 0, 0) = 355  'Dschungelboden
      IF brightness! <= 0.4 THEN mapdata%(x&, y&, 1, 0) = 390  'Dschungelbewuchs
    CASE 30 TO 90:  'gelb
      mapdata%(x&, y&, 0, 0) = IIF&(brightness! < 0.5, 312, 176)  'Sand
    CASE ELSE
      mapdata%(x&, y&, 0, 0) = 16
    END SELECT
  END IF
END SUB



'Karte aus GIF Bild erzeugen
'bi2editor -mapfromgif G:\Homepage-Bau\Arena\BI\bi2westeros.gif
SUB CreateMapFromGif(BYVAL f$)
  LOCAL pixeldata$, bitmapwidth&, bitmapheight&, i&, n&, m&, mspr&, dx&, dy&, x&, y&, p&, c&, terrain&

  'GIF Datei laden
  f$ = TRIM$(f$, CHR$(34))
  IF ISFILE(f$) = 0 THEN
    MessageBox hWIN&, f$+"", GETWORD$(%WORDSTART_ERROR+3), %MB_OK OR %MB_ICONERROR
    EXIT SUB
  END IF
  D2D.GraphicLoadGIF(f$, bitmapwidth&, bitmapheight&, pixeldata$)
  IF bitmapwidth& < 1 OR bitmapheight& < 1 THEN
    MessageBox hWIN&, f$+"", GETWORD$(%WORDSTART_ERROR+4), %MB_OK OR %MB_ICONERROR
    EXIT SUB
  END IF
  IF bitmapwidth& > 256 OR bitmapheight& > 256 THEN
    MessageBox hWIN&, f$+"", GETWORD$(%WORDSTART_ERROR+5), %MB_OK OR %MB_ICONERROR
    EXIT SUB
  END IF

  'neue Karte mit derselben Gr��e wie das Bild erzeugen
  CALL CREATEMAP(bitmapwidth&, bitmapheight&, 95)

  'Terrain aus Pixeldaten erzeugen
  p& = 1
  FOR y& = 0 TO bitmapheight&-1
    FOR x& = 0 TO bitmapwidth&-1
      c& = CVL(pixeldata$, p&)
      p& = p&+4
      CALL SetTerrainFromRGB(c&, x&, y&)
    NEXT x&
  NEXT y&

  'H�gelgruppen durch Gebirge ersetzen
  CALL INITMULTISPRITES
  FOR mspr& = 12 TO 10 STEP -1  '16/9/4 Felder gro�er Berg
    n& = LEN(multiSprites$(mspr&))/4
    FOR y& = 0 TO bitmapheight&-1
      FOR x& = 0 TO bitmapwidth&-1
        IF mapdata%(x&, y&, 1, 0) = 288 THEN
          'pr�fen, ob ausreichend H�gel vorhanden sind, um Gebirgsfeld vollst�ndig darzustellen
          m& = 0
          FOR i& = 0 TO n&-1
            dx& = ASC(multiSprites$(mspr&), i&*4+1)
            IF dx& > 127 THEN dx& = dx&-256
            dy& = ASC(multiSprites$(mspr&), i&*4+2)
            IF dy& > 127 THEN dy& = dy&-256
            IF (x& AND 1) = 1 AND (dx& AND 1) = 1 THEN dy& = dy&+1
            IF x&+dx& >= 0 AND x&+dx& < mapwidth& AND y&+dy& >= 0 AND y&+dy& < mapheight& AND mapdata%(x&+dx&, y&+dy&, 1, 0) = 288 THEN m& = m&+1
          NEXT i&
          IF m& = n& THEN
            'Gebirge einf�gen
            FOR i& = 0 TO n&-1
              dx& = ASC(multiSprites$(mspr&), i&*4+1)
              IF dx& > 127 THEN dx& = dx&-256
              dy& = ASC(multiSprites$(mspr&), i&*4+2)
              IF dy& > 127 THEN dy& = dy&-256
              IF (x& AND 1) = 1 AND (dx& AND 1) = 1 THEN dy& = dy&+1
              mapdata%(x&+dx&, y&+dy&, 1, 0) = CVI(multiSprites$(mspr&), i&*4+3)
            NEXT i&
          END IF
        END IF
      NEXT x&
    NEXT y&
  NEXT mspr&

  'Karte weichzeichen
  FOR y& = 0 TO bitmapheight&-1
    FOR x& = 0 TO bitmapwidth&-1
      CALL SMOOTHMAP(x&, y&, 0)
      CALL SMOOTHMAP(x&, y&, 1)
    NEXT x&
  NEXT y&

  mapCreatedFromGif& = 1
END SUB



'Pr�ft ob sich ein Punkt in einem Rechteck befindet
FUNCTION IsInRect&(x&, y&, r AS RECT)
  IsInRect& = IIF&(x& >= r.left AND x& <= r.right AND y& >= r.top AND y& <= r.bottom, -1, 0)
END FUNCTION



'Erzeugt eine Liste von Listbox-Items in der gew�hlten Sprache
FUNCTION CREATELISTBOXITEMS$(start&, n&)
  LOCAL a$, i&

  FOR i& = 0 TO n&-1
    IF a$ <> "" THEN a$ = a$+";"
    a$ = a$+GETWORD$(start&+i&)
  NEXT i&

  CREATELISTBOXITEMS$ = a$
END FUNCTION



'Erzeugt eine Liste mit den Eintr�gen "Spieler 1" bis "Spieler 6" in der gew�hlten Sprache
FUNCTION CREATEPLAYERLISTBOXITEMS$
  LOCAL a$, pl$, i&

  pl$ = GETWORD$(%WORDSTART_ACTIONSCREEN+1)
  FOR i& = 1 TO 6
    IF a$ <> "" THEN a$ = a$+";"
    a$ = a$+pl$+" "+FORMAT$(i&)
  NEXT i&

  CREATEPLAYERLISTBOXITEMS$ = a$
END FUNCTION



'Liefert das DX-Handle zu einem Sprite
'65536-65547 = Editor-Sprites (Fahnen)
'65548 = Shop-Highlight
'65549 = Selektions-Highlight
FUNCTION GETSPRITEHANDLE&(sprnr&)
  LOCAL nr&, pl&

  nr& = sprnr&
  IF sprnr& >= 65536 THEN
    IF sprnr& >= 70000 THEN
      'Stra�en/Wege/Gr�ben/Schienen
      nr& = sprnr&-70000+%ROADSPIRTESTART
    ELSE
      'Fahnen und Highlights
      nr& = sprnr&-65536+%EDITORSPRITESTART
    END IF
  ELSE
    IF sprnr& >= 1337 THEN
      'Einheiten und Geb�ude in Spielerfarben
      IF sprnr& = 1830 OR sprnr& = 1832 THEN
        'Sprites von Spieler 0 �berlagern sich mit Spieler 1 wenn Sprite-Nummer > 255 ist
        nr& = sprnr&-playerUnitOffset&(0)+%SPRITESPERPLAYER*0
      ELSE
        FOR pl& = 6 TO 0 STEP -1
          IF sprnr& >= 1337+playerUnitOffset&(pl&) THEN
            nr& = sprnr&-playerUnitOffset&(pl&)+%SPRITESPERPLAYER*pl&
            EXIT FOR
          END IF
        NEXT pl&
      END IF
    END IF
  END IF

  GETSPRITEHANDLE& = sprites&(nr&)
END FUNCTION



'Einfarbiges Bitmap in Hexform erzeugen (24x24)
FUNCTION DRAWHIGHLIGHT$(cl???)
  LOCAL a$, wd&, row&

  'Slot
  FOR row& = 1 TO 24
    wd& = ASC($spx, row&)
    a$ = a$+REPEAT$(12-wd&/2, MKL$(0)) + REPEAT$(wd&, MKL$(cl???)) + REPEAT$(12-wd&/2, MKL$(0))
  NEXT row&

  DRAWHIGHLIGHT$ = a$
END FUNCTION



'Bitmap mit Einheitenslot erzeugen (26x26)
FUNCTION DRAWSLOT$(borderColor???, slotColor???)
  LOCAL a$, wd&, row&

  'oberer Rahmen
  wd& = ASC($spx)
  a$ = REPEAT$(13-wd&/2, MKL$(0)) + REPEAT$(wd&, MKL$(borderColor???)) + REPEAT$(13-wd&/2, MKL$(0))

  'Slot
  FOR row& = 1 TO 24
    wd& = ASC($spx, row&)
    a$ = a$+REPEAT$(12-wd&/2, MKL$(0)) + MKL$(borderColor???) + REPEAT$(wd&, MKL$(slotColor???)) + MKL$(borderColor???) + REPEAT$(12-wd&/2, MKL$(0))
  NEXT row&

  'unterer Rahmen
  a$ = a$+LEFT$(a$, 26*4)

  DRAWSLOT$ = a$
END FUNCTION



'Pr�ft, ob ein Sprite zu einem Multi-Sprite geh�rt
FUNCTION ISMULTISPRITE&(sprnr&)
  LOCAL i&, n&

  n& = UBOUND(multiSprites$())
  FOR i& = 0 TO n&
    IF INSTR(multiSprites$(i&), CHR$(0,0)+MKI$(sprnr&)) > 0 THEN
      ISMULTISPRITE& = i&
      EXIT FUNCTION
    END IF
  NEXT i&

  ISMULTISPRITE& = -1
END FUNCTION



'Pr�ft, ob ein Punkt innerhalb eines Rechtecks liegt
FUNCTION HITTEST&(px&, py&, rx0&, ry0&, rx1&, ry1&)
  HITTEST& = (px& >= rx0& AND px& <= rx1& AND py& >= ry0& AND py& <= ry1&)
END FUNCTION



'Kartenfeld zu Pixelposition errechnen
SUB GETMAPPOS(BYVAL x&, BYVAL y&, mapx&, mapy&)
  LOCAL v&, w&

  mapx& = INT(x&/16)
  mapy& = INT((y&-IIF&((mapx& AND 1) = 1, 12, 0))/24)

  v& = x& AND 15
  IF v& < 8 THEN
    IF (mapx& AND 1) = 0 THEN
      w& = 12-ASC($spx, (y& MOD 24)+1)/2
      IF v& < w& THEN
        mapx& = mapx&-1
        mapy& = INT((y&-12)/24)
      END IF
    ELSE
      w& = ASC($spx, (y& MOD 24)+1)/2-4
      IF v& < w& THEN
        mapx& = mapx&-1
        mapy& = INT(y&/24)
      END IF
    END IF
  END IF
END SUB



'Aktions-Parameter-Listbox 1 bef�llen
SUB POPULATEACTIONPARAMLISTBOX(BYVAL actionNr&)
  LOCAL i&, a$

  SELECT CASE actionNr&
  CASE 0, 12:  'Wetter
    listboxParam1.SetItems(CREATELISTBOXITEMS$(%WORDSTART_WEATHER, 5))

  CASE 1:  'Nachricht
    FOR i& = 0 TO 98
      IF a$ <> "" THEN a$ = a$+";"
      a$ = a$+LEFT$(GETWORD$(%WORDSTART_MESSAGES+i&), 17)
    NEXT i&
    IF isBI3& = 0 AND totalUnitClasses& > 54 THEN
      FOR i& = 99 TO 154
        a$ = a$+";EDT"+FORMAT$(i&, "000")
      NEXT i&
    END IF
    listboxParam1.SetItems(a$)

  CASE 2, 3:  'Sieg / Niederlage
    listboxParam1.SetItems(CREATELISTBOXITEMS$(%WORDSTART_VICTORYCONDITION, IIF&(bi2020support& = 0, 9, 10)))

  CASE 4, 9, 10, 11:  'DF-Layer
    listboxParam1.SetItems("0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29")

  CASE 5:  'Allianz
    listboxParam1.SetItems(CREATEPLAYERLISTBOXITEMS$)

  CASE 6:  'BI3 Video-Nachricht
    FOR i& = 0 TO 32 : a$ = a$+"VIDEO"+FORMAT$(i&, "000")+";" : NEXT i&
    FOR i& = 34 TO 53 : a$ = a$+"VIDEO"+FORMAT$(i&, "000")+";" : NEXT i&
    FOR i& = 56 TO 98 : a$ = a$+"VIDEO"+FORMAT$(i&, "000")+";" : NEXT i&
    FOR i& = 500 TO 509 : a$ = a$+"VIDEO"+FORMAT$(i&, "000")+";" : NEXT i&
    FOR i& = 400 TO 419 : a$ = a$+"INTRO"+FORMAT$(i&-399, "000")+";" : NEXT i&
    listboxParam1.SetItems(LEFT$(a$, LEN(a$)-1))

  CASE 7:  'benutzerdefinierte Nachricht
    listboxParam1.SetItems($CUSTOMMSGIDS)

  CASE 8:  'Siegbedingung f�r Bonusmission
    listboxParam1.SetItems(CREATELISTBOXITEMS$(%WORDSTART_BONUSCONDITION, 4))

  END SELECT
END SUB



'Aktions-Parameter-Listbox 2 bef�llen
SUB POPULATEACTIONPARAMLISTBOX2(BYVAL actionNr&, BYVAL p1&)
  LOCAL i&, j&, a$, b$, wordturn$

  wordturn$ = GETWORD$(%WORDSTART_ACTIONSCREEN+2)+" "
  IF actionNr& <> 12 THEN listboxParam2.SetItems("n/a")

  SELECT CASE actionNr&
  CASE 2, 3:  'Sieg / Niederlage
    SELECT CASE p1&
    CASE 0:  'Spieler besiegt
      'Spieler steht in oberster Listbox

    CASE 1:  'Shop erobert
      FOR i& = 0 TO nshops&-1
        IF shops(i&).unittype = 1 AND shops(i&).nameindex > 0 THEN
          a$ = a$+shopnames$(i&)+";"
        END IF
      NEXT i&
      IF a$ <> "" THEN a$ = LEFT$(a$, LEN(a$)-1)
      listboxParam2.SetItems(a$)

    CASE 2:  'Runde erreicht
      a$ = wordturn$+"1"
      FOR i& = 2 TO 100
        a$ = a$+";"+wordturn$+FORMAT$(i&)
      NEXT i&
      listboxParam2.SetItems(a$)

    CASE 3 TO 8:  'Einheit tot
      FOR i& = 0 TO 52
        IF a$ <> "" THEN a$ = a$+";"
        a$ = a$+unitnames$(i&)
      NEXT i&
      listboxParam2.SetItems(a$)

    END SELECT

  CASE 4:  'DF-Layer
    'kein Parameter

  CASE 5:  'Allianz
    FOR i& = 1 TO 62
      a$ = ""
      FOR j& = 0 TO 5
        IF (i& AND 2^j&) > 0 THEN
          IF a$ <> "" THEN a$ = a$+"+"
          a$ = a$+FORMAT$(j&+1)
        END IF
      NEXT i&
      a$ = IIF$(LEN(a$) < 6, GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" ", LEFT$(GETWORD$(%WORDSTART_ACTIONSCREEN+1), 2)+". ")+a$
      b$ = b$+a$+";"
    NEXT i&
    IF b$ <> "" THEN b$ = LEFT$(b$, LEN(b$)-1)
    listboxParam2.SetItems(b$)

  CASE 8:  'Siegbedingung f�r Bonusmission
    SELECT CASE p1&
    CASE 0:  'Shop erobert
      FOR i& = 0 TO nshops&-1
        IF shops(i&).unittype = 1 AND shops(i&).nameindex > 0 THEN
          a$ = a$+shopnames$(i&)+";"
        END IF
      NEXT i&
      IF a$ <> "" THEN a$ = LEFT$(a$, LEN(a$)-1)
      listboxParam2.SetItems(a$)

    CASE 1:  'Runde noch nicht erreicht
      a$ = wordturn$+"1"
      FOR i& = 2 TO 100
        a$ = a$+";"+wordturn$+FORMAT$(i&)
      NEXT i&
      listboxParam2.SetItems(a$)

    CASE 3:  'Einheit �berlebt
      FOR i& = 0 TO 52
        IF a$ <> "" THEN a$ = a$+";"
        a$ = a$+unitnames$(i&)
      NEXT i&
      listboxParam2.SetItems(a$)

    END SELECT

  CASE 12:  'lokales Wetter
    FOR i& = 0 TO mapheight&-1
      a$ = a$+FORMAT$(i&)+";"
    NEXT i&
    a$ = LEFT$(a$, LEN(a$)-1)
    listboxParam2.SetItems(a$)
    listboxParam3.SetItems(a$)

  END SELECT
END SUB



'Liefert die n�chste Einheit, die sich noch nicht in der allgemeinen Produktionspalette befindet
FUNCTION GETNEXTGPM&(slotnr&, pl&)
  LOCAL i&, v&, duplicate&

  'Einheit im Slot ermitteln
  v& = gpm?(slotnr&, pl&)

  'n�chste Einheit ermitteln, die noch nicht in der allgemeinen Produktionspaletten ist
  DO
    v& = v&+1
    IF v& = 8 THEN v& = 9  'Fahrende Festung �berspringen
    IF v& = 21 THEN v& = 22  'Rune �berspringen
    IF v& > 22 AND v& < 25 THEN v& = 25  'Skull und Ionstar �berspringen
    IF v& > 26 AND v& < 52 THEN v& = 52  'Z�ge, Flugzeuge, Schiffe �berspringen
    IF v& >= totalUnitClasses& OR v& > 63 THEN v& = 0
    duplicate& = 0
    FOR i& = 0 TO 12
      IF gpm?(i&, pl&) = v& THEN duplicate& = 1
    NEXT i&
  LOOP UNTIL duplicate& = 0

  GETNEXTGPM& = v&
END FUNCTION



'Konvertiert eine Sprite-Nummer in eine Einheiten-Nummer (unabh�ngig vom Besitzer)
FUNCTION SPRITETOUNIT&(BYVAL sprnr&)
  LOCAL n&

  'pr�fen, ob Sprite eine Einheit ist
  IF sprnr& < 1337 THEN
    SPRITETOUNIT& = -1
    EXIT FUNCTION
  END IF

  'Einheiten-Nummer ermitteln
  FOR n& = 6 TO 0 STEP -1
    IF sprnr& >= 1337+playerUnitOffset&(n&) THEN
      SPRITETOUNIT& = sprnr&-playerUnitOffset&(n&)-1337
      EXIT FUNCTION
    END IF
  NEXT n&

  SPRITETOUNIT& = -1
END FUNCTION



'Ermittelt den Spieler zu einer Einheit anhand der Sprite-Nummer
FUNCTION GETPLAYERFORUNIT&(BYVAL sprnr&)
  LOCAL pl&

  'pr�fen, ob Sprite eine Einheit ist
  IF sprnr& < 1337 THEN
    GETPLAYERFORUNIT& = -1
    EXIT FUNCTION
  END IF

  'Einheiten-Nummer ermitteln
  FOR pl& = 6 TO 0 STEP -1
    IF sprnr& >= 1337+playerUnitOffset&(pl&) THEN
      GETPLAYERFORUNIT& = pl&
      EXIT FUNCTION
    END IF
  NEXT pl&

  GETPLAYERFORUNIT& = -1
END FUNCTION



'Shop-Typ anhand von Sprite ermitteln
FUNCTION GETSHOPTYPEFORSPRITE&(sprnr&)
  LOCAL tp&

  SELECT CASE sprnr&
  CASE 54, 55: tp& = 1
  CASE 76: tp& = 8
  CASE 71: tp& = 4
  CASE 80, 81: tp& = 2
  CASE 44, 45: tp& = 16
  CASE 66: tp& = 64
  CASE 63, 69, 86, 87, 88, 393, 395: tp& = 32
  CASE ELSE: tp& = -1
  END SELECT

  GETSHOPTYPEFORSPRITE& = tp&
END FUNCTION



'Ermittelt die Shop-Funktion aus dem Shop-Typ
FUNCTION GETSHOPFUNCTION&(BYVAL tp&)
  LOCAL sf&

  SELECT CASE tp&
  CASE 0: sf& = 32
  CASE 1: sf& = 1
  CASE 2, 4, 8: sf& = 2
  CASE 16: sf& = 4
  CASE 32: sf& = 16
  CASE 64: sf& = 8
  END SELECT

  GETSHOPFUNCTION& = sf&
END FUNCTION



'Ermittelt welche Spieler in der Mission vorhanden sind
FUNCTION GETACTIVEPLAYERS&
  LOCAL plmask&, shopnr&, x&, y&, v&, pl&

  'Shops pr�fen
  FOR shopnr& = 0 TO nshops&-1
    plmask& = plmask& OR shops(shopnr&).owner
  NEXT shopnr&

  'Einheiten pr�fen
  FOR y& = 0 TO mapheight&-1
    FOR x& = 0 TO mapwidth&-1
      v& = mapdata%(x&, y&, 2, 0)
      pl& = GETPLAYERFORUNIT&(v&)
      IF pl& >= 0 THEN plmask& = plmask& OR 2^pl&
    NEXT x&
  NEXT y&

  GETACTIVEPLAYERS& = plmask& AND 63
END FUNCTION



'Speichert die Karte im Undo-Array
SUB SAVEUNDO
  LOCAL a$

  'pr�fen, ob tats�chliche eine �nderung stattgefunden hat
  a$ = PEEK$(VARPTR(mapdata%(0, 0, 0, 0)), mapwidth&*mapheight&*6*%MAXDFLAYER)
  IF undoPos& <> undoStart& AND a$ = undoData$((undoPos&-1) MOD %MAXUNDOLEVELS) THEN EXIT SUB

  undoData$(undoPos&) = a$
  IF undoPos& = %MAXUNDOLEVELS-1 THEN undoPos& = 0 ELSE undoPos& = undoPos&+1
  IF undoPos& = undoStart& THEN
    IF undoStart& = %MAXUNDOLEVELS-1 THEN undoStart& = 0 ELSE undoStart& = undoStart&+1
  END IF
END SUB



'Stellt den letzten Kartenzustand wieder her
SUB UNDOEDIT
  LOCAL shopnr&, shopx&, shopy&, sprnr&

  IF undoPos& = undoStart& THEN EXIT SUB

  IF undoPos& = 0 THEN undoPos& = %MAXUNDOLEVELS-1 ELSE undoPos& = undoPos&-1
  POKE$ VARPTR(mapdata%(0, 0, 0, 0)), undoData$(undoPos&)

  'ung�ltige Shops und AI-Points l�schen
  FOR shopnr& = nshops&-1 TO 0 STEP -1
    shopx& = shops(shopnr&).position AND (yMapFactor&-1)
    shopy& = INT(shops(shopnr&).position/yMapFactor&)
    IF shops(shopnr&).unittype = 2 THEN
      'AI-Point ist nur g�ltig, falls sich auf dem Feld eine Einheit befindet
      IF mapdata%(shopx&, shopy&, 2, 0) = -1 THEN CALL DELETESHOP(shopnr&)
    ELSE
      IF shops(shopnr&).shopfunction = 32 THEN
        'Transporter ist nur g�ltig, falls sich auf dem Feld eine Einheit befindet
        IF mapdata%(shopx&, shopy&, 2, 0) = -1 THEN CALL DELETESHOP(shopnr&)
      ELSE
        'Shop ist nur g�ltig, falls sich auf dem Feld ein Shop-Sprite befindet (oder Shop ein Geheim-Depot ist)
        sprnr& = mapdata%(shopx&, shopy&, 1, 0)
        IF shops(shopnr&).shopfunction <> 4 AND GETSHOPTYPEFORSPRITE&(sprnr&) < 0 THEN CALL DELETESHOP(shopnr&)
      END IF
    END IF
  NEXT shopnr&
END SUB



'Daten aus angew�hlten Shop in Eingabefelder �bertragen
SUB SHOWSHOP
  LOCAL shopx&, shopy&, sprnr&, isShop&, isAIPoint&

  'Einheiten-Palette aktivieren
  IF selectedTab& = -1 THEN CALL HIDEMAPPROPERTIES
  CALL SelectTab(4)

  'Name
  IF shops(selectedShop&).unittype = 2 THEN
    isAIPoint& = 1
    shopnames$(selectedShop&) = "AI Point "+FORMAT$(selectedShop&+1)
  ELSE
    IF shops(selectedShop&).shoptype > 0 THEN isShop& = 1
    shopx& = shops(selectedShop&).position AND (yMapFactor&-1)
    shopy& = INT(shops(selectedShop&).position/yMapFactor&)
    IF shopx& >= 0 AND shopx& < mapwidth& AND shopy& >= 0 AND shopy& < mapheight& THEN
      sprnr& = SPRITETOUNIT&(mapdata%(shopx&, shopy&, 2, 0))
      IF sprnr& >= 0 AND sprnr& < 64 THEN shopnames$(selectedShop&) = unitnames$(sprnr&)+" "+FORMAT$(selectedShop&+1)
    END IF
  END IF
  editShopname.Value = shopnames$(selectedShop&)
  editShopname.Visible = 1

  'AI-Point
  listboxAICommand.SelectedItem = shops(selectedShop&).aicommand
  listboxAICommand.Visible = isAIPoint&

  'Energie und Material
  editEnergy.Value = FORMAT$(shops(selectedShop&).energy)
  editEnergy.Visible = isShop&
  editMaterial.Value = FORMAT$(shops(selectedShop&).material)
  editMaterial.Visible = isShop&
  editEnergyPlus.Value = FORMAT$(shops(selectedShop&).eplus)
  editEnergyPlus.Visible = isShop&
  editMaterialPlus.Value = FORMAT$(shops(selectedShop&).mplus)
  editMaterialPlus.Visible = isShop&

  'Besitzer und Typ
  listboxOwner.SelectedItem = LOG2(shops(selectedShop&).owner)
  listboxOwner.Visible = isShop&
  listboxShoptype.SelectedItem = LOG2(shops(selectedShop&).shoptype)
  listboxShoptype.Visible = isShop&
END SUB



'Daten aus Eingabefeldern in angew�hlten Shop �bertragen
SUB UPDATESHOP
  IF shops(selectedShop&).unittype = 2 THEN
    'AI-Point
    shops(selectedShop&).aicommand = listboxAICommand.SelectedItem
  ELSE
    IF shops(selectedShop&).shoptype = 0 THEN EXIT SUB  'Transporter
    'Shop
    shops(selectedShop&).owner = 2^listboxOwner.SelectedItem
    shops(selectedShop&).shoptype = 2^listboxShoptype.SelectedItem
    shops(selectedShop&).shopfunction = GETSHOPFUNCTION&(shops(selectedShop&).shoptype)

    shopnames$(selectedShop&) = RTRIM$(editShopname.Value)
    shops(selectedShop&).energy = VAL(editEnergy.Value)
    shops(selectedShop&).material = VAL(editMaterial.Value)
    shops(selectedShop&).eplus = VAL(editEnergyPlus.Value)
    shops(selectedShop&).mplus = VAL(editMaterialPlus.Value)
  END IF

  mapChanged& = -1
END SUB



'Shop-Dialog ausblenden
SUB HIDESHOPDIALOGUE
  IF selectedShop& >= 0 THEN CALL UPDATESHOP
  selectedShop& = -1
END SUB



'Karteneigenschaften ausblenden
SUB HIDEMAPPROPERTIES
  LOCAL i&

  listboxTerrain.Visible = 0
  listboxWeather.Visible = 0
  listboxWinCond.Visible = 0
  listboxNextmap.Visible = 0
  listboxBonusmap.Visible = 0
  editMapDescription.Visible = 0
  editMapShortDescr.Visible = 0
  listboxCustomMessages.Visible = 0
  editCustomMessageGER.Visible = 0
  editCustomMessageENG.Visible = 0
  editCustomMessageUserLang.Visible = 0
  editCustomLangCode.Visible = 0
  FOR i& = 0 TO %MAXPLAYERS-1
    editPlayerName(i&).Visible = 0
  NEXT i&
END SUB



'Aktionen ausblenden
SUB HIDEACTIONDIALOGUE
  LOCAL i&

  FOR i& = 0 TO %MAXACTIONS-1
    buttonDeleteAction(i&).Visible = 0
  NEXT i&
  buttonNewAction.Visible = 0
  listboxPlayer.Visible = 0
  listboxTurn.Visible = 0
  listboxMovement.Visible = 0
  listboxAction.Visible = 0
  listboxParam1.Visible = 0
  listboxParam2.Visible = 0
  listboxParam3.Visible = 0
  listboxLevel.Visible = 0
  scrollbarActions.Visible = 0
END SUB



'Einstellungen ausblenden
SUB HIDEOPTIONS
  radiogroupLanguage.Visible = 0
  checkboxReopen.Visible = 0
  checkboxBI2020.Visible = 0
END SUB



'Neue-Karte Dialog ausblenden
SUB HIDENEWMAPDIALOGUE
  listboxWidth.Visible = 0
  listboxHeight.Visible = 0
END SUB



'Alle Dialoge bis auf den aktiven ausblenden
SUB HIDEDIALOGUES(showdialogue&)
  IF (showdialogue& AND %DIALOGUE_PROPERTIES) = 0 THEN CALL HIDEMAPPROPERTIES
  IF (showdialogue& AND %DIALOGUE_ACTIONS) = 0 THEN CALL HIDEACTIONDIALOGUE
  IF (showdialogue& AND %DIALOGUE_SHOP) = 0 THEN CALL HIDESHOPDIALOGUE
  IF (showdialogue& AND %DIALOGUE_OPTIONS) = 0 THEN CALL HIDEOPTIONS
  IF (showdialogue& AND %DIALOGUE_NEWMAP) = 0 THEN CALL HIDENEWMAPDIALOGUE
END SUB



'Objekt-Palette definieren
SUB INITOBJECTPALETTE
  LOCAL i&, j&
  REDIM palette$(4), playerUnitOffset&(6)

  'Einheiten-Offset festlegen
  playerUnitOffset&(0) = 0
  playerUnitOffset&(1) = 256
  playerUnitOffset&(2) = 768
  playerUnitOffset&(3) = 1792
  playerUnitOffset&(4) = 3840
  playerUnitOffset&(5) = 7936
  playerUnitOffset&(6) = 16128

  'Terrain
  palette$(0) = MKI$(273)+MKI$(271)+MKI$(274)+MKI$(272)+MKI$(275)+MKI$(128)+MKI$(129)+MKI$(130)+MKI$(131)+MKI$(132)+MKI$(133)+MKI$(134)+MKI$(135) _
            + MKI$(136)+MKI$(137)+MKI$(138)+MKI$(139)+MKI$(140)+MKI$(276)+MKI$(311)+MKI$(150)+MKI$(151)+MKI$(152) _
            + MKI$(32)+MKI$(35)+MKI$(36)+MKI$(33)+MKI$(34)+MKI$(17)+MKI$(18)+MKI$(95)+MKI$(96)+MKI$(97)+MKI$(110)+MKI$(111)+MKI$(112)+MKI$(113)+MKI$(41) _
            + MKI$(0)+MKI$(1)+MKI$(3)+MKI$(6)+MKI$(5)+MKI$(7)+MKI$(10)+MKI$(12)+MKI$(11)+MKI$(13)+MKI$(8)+MKI$(2)+MKI$(9)+MKI$(4)+MKI$(16) _
            + MKI$(153)+MKI$(154)+MKI$(155)+MKI$(156)+MKI$(157)+MKI$(158)+MKI$(159)+MKI$(160)+MKI$(161)+MKI$(162)+MKI$(163)+MKI$(164)+MKI$(165)+MKI$(166) _
            + MKI$(167)+MKI$(168)+MKI$(169)+MKI$(170)+MKI$(171)+MKI$(172)+MKI$(173)+MKI$(174)+MKI$(175)+MKI$(176)+MKI$(177)+MKI$(178)+MKI$(179) _
            + MKI$(187)+MKI$(188)+MKI$(189)+MKI$(190)+MKI$(191)+MKI$(192)+MKI$(193)+MKI$(194)+MKI$(195)+MKI$(196)+MKI$(197)+MKI$(198)+MKI$(199) _
            + MKI$(200)+MKI$(201)+MKI$(202)+MKI$(203)+MKI$(204)+MKI$(205)+MKI$(206)+MKI$(207)+MKI$(208)+MKI$(209)+MKI$(210)+MKI$(211)+MKI$(212)+MKI$(213) _
            + MKI$(214)+MKI$(215)+MKI$(216)+MKI$(217)+MKI$(218)+MKI$(219)+MKI$(220)+MKI$(221)+MKI$(224)+MKI$(225)+MKI$(226)+MKI$(227)+MKI$(228)+MKI$(229) _
            + MKI$(230)+MKI$(231)+MKI$(232)+MKI$(185)+MKI$(233)+MKI$(237)+MKI$(242)+MKI$(312) _
            + MKI$(349)+MKI$(350)+MKI$(351)+MKI$(352)+MKI$(361)+MKI$(362)+MKI$(363)+MKI$(364)+MKI$(365)+MKI$(366)+MKI$(367)+MKI$(368)+MKI$(369)+MKI$(370) _
            + MKI$(371)+MKI$(372)+MKI$(353)+MKI$(354)+MKI$(355)+MKI$(356)+MKI$(408)+MKI$(409)+MKI$(410)+MKI$(411)+MKI$(412)+MKI$(413)+MKI$(414)+MKI$(415) _
            + MKI$(416)+MKI$(417)+MKI$(418)+MKI$(419)+MKI$(420)+MKI$(421)+MKI$(422)+MKI$(423)+MKI$(424)+MKI$(425)+MKI$(426)+MKI$(427)+MKI$(428) _
            + MKI$(385)+MKI$(386)+MKI$(387)+MKI$(463)+MKI$(464)+MKI$(465)+MKI$(466)+MKI$(429)+MKI$(430)+MKI$(431)+MKI$(432) _
            + MKI$(527)+MKI$(528)+MKI$(529)+MKI$(530)+MKI$(531)+MKI$(526)+MKI$(515)+MKI$(516)+MKI$(517)+MKI$(518)+MKI$(519)+MKI$(520)+MKI$(521)+MKI$(522) _
            + MKI$(523)+MKI$(524)+MKI$(525) _
            + MKI$(467)+MKI$(468)+MKI$(469)+MKI$(470)+MKI$(471)+MKI$(472)+MKI$(473)+MKI$(474)+MKI$(475)+MKI$(533)+MKI$(534)+MKI$(513)+MKI$(476)+MKI$(477) _
            + MKI$(478)+MKI$(479)+MKI$(480)+MKI$(481)+MKI$(482)+MKI$(483)+MKI$(484)+MKI$(485)+MKI$(486)+MKI$(510)+MKI$(511)+MKI$(514)+MKI$(573)+MKI$(575) _
            + MKI$(576)+MKI$(577)+MKI$(581)+MKI$(535)+MKI$(536)+MKI$(537)+MKI$(538)+MKI$(539)+MKI$(540)+MKI$(541)+MKI$(542)+MKI$(543) _
            + MKI$(544)+MKI$(545)+MKI$(546)+MKI$(547)+MKI$(548)+MKI$(549)+MKI$(550)+MKI$(551)+MKI$(552)+MKI$(553)+MKI$(554)+MKI$(555)+MKI$(556)+MKI$(557) _
            + MKI$(558)+MKI$(559)+MKI$(560)+MKI$(561)+MKI$(499)+MKI$(487)+MKI$(488)+MKI$(500)+MKI$(501)+MKI$(489)+MKI$(490)+MKI$(504)+MKI$(502)+MKI$(503) _
            + MKI$(512)+MKI$(509)+MKI$(1282)+MKI$(506)+MKI$(505)+MKI$(571)+MKI$(570)+MKI$(507)+MKI$(497)+MKI$(493)+MKI$(494)+MKI$(491)+MKI$(492)+MKI$(495) _
            + MKI$(496)+MKI$(498)+MKI$(508)+MKI$(562)+MKI$(564)+MKI$(565)+MKI$(566)+MKI$(567)+MKI$(568)

  'Objekte
  palette$(1) = MKI$(24)+MKI$(25)+MKI$(26)+MKI$(27)+MKI$(19)+MKI$(20)+MKI$(21)+MKI$(22)+MKI$(23)+MKI$(28)+MKI$(29)+MKI$(30)+MKI$(37)+MKI$(64)+MKI$(65) _
            + MKI$(70)+MKI$(243)+MKI$(270)+MKI$(268)+MKI$(338)+MKI$(315)+MKI$(325)+MKI$(326) _
            + MKI$(327)+MKI$(328)+MKI$(329)+MKI$(330)+MKI$(331)+MKI$(332)+MKI$(333)+MKI$(334)+MKI$(335)+MKI$(336)+MKI$(339)+MKI$(340)+MKI$(341)+MKI$(342) _
            + MKI$(343)+MKI$(287)+MKI$(288)+MKI$(289)+MKI$(291)+MKI$(279)+MKI$(292)+MKI$(300)+MKI$(307)+MKI$(445)+MKI$(440)+MKI$(31)+MKI$(1330)+MKI$(234) _
            + MKI$(1331)+MKI$(1283)+MKI$(345)+MKI$(344)+MKI$(239)+MKI$(236)+MKI$(235)+MKI$(238)+MKI$(459)+MKI$(460)+MKI$(461)+MKI$(389)+MKI$(390)+MKI$(391) _
            + MKI$(392)+MKI$(407)+MKI$(394)+MKI$(578)+ MKI$(40)+MKI$(388)+MKI$(360)+MKI$(358)+MKI$(359)

  'Geb�ude
  palette$(2) = MKI$(54)+MKI$(55)+MKI$(76)+MKI$(71)+MKI$(80)+MKI$(81)+MKI$(44)+MKI$(45)+MKI$(66)+MKI$(63)+MKI$(69)+MKI$(86)+MKI$(87)+MKI$(88)+MKI$(393)+MKI$(395)+MKI$(563)

  'Stra�en
  palette$(3) = MKI$(90)+MKI$(91)+MKI$(92)+MKI$(93)+MKI$(1264)+MKI$(1265)+MKI$(1266)+MKI$(1267)+MKI$(1268)+MKI$(1269)+MKI$(1270)+MKI$(1271)+MKI$(1272) _
            + MKI$(1273)+MKI$(1274)

  'Einheiten
  palette$(4) = MKI$(17518)
  FOR j& = 0 TO LEN($playercolors)-1
    FOR i& = 0 TO totalUnitClasses&-1
      IF i& <> 53 THEN palette$(4) = palette$(4)+MKI$(1337+i&+playerUnitOffset&(j&))
    NEXT i&
  NEXT j&
END SUB



'Multi-Sprites definieren
SUB INITMULTISPRITES
  REDIM multiSprites$(15)

  'Angaben beziehen sich auf gerade X-Position f�r Zentral-Sprite
  multiSprites$(0) = CHR$(0,0)+MKI$(76)+CHR$(0,255)+MKI$(77)+CHR$(255,0)+MKI$(78)+CHR$(1,0)+MKI$(79)  'Fabrik
  multiSprites$(1) = CHR$(0,0)+MKI$(54)+CHR$(0,1)+MKI$(58)+CHR$(0,2)+MKI$(57)+CHR$(255,0)+MKI$(59)+CHR$(255,1)+MKI$(60)+CHR$(1,0)+MKI$(61)+CHR$(1,1)+MKI$(62)  'HQ (Eingang oben)
  multiSprites$(2) = CHR$(0,0)+MKI$(55)+CHR$(0,255)+MKI$(58)+CHR$(0,254)+MKI$(56)+CHR$(255,255)+MKI$(60)+CHR$(255,254)+MKI$(59)+CHR$(1,255)+MKI$(62)+CHR$(1,254)+MKI$(61)  'HQ (Eingang unten)
  multiSprites$(3) = CHR$(0,0)+MKI$(71)+CHR$(255,255)+MKI$(72)+CHR$(255,0)+MKI$(73)+CHR$(1,255)+MKI$(74)+CHR$(1,0)+MKI$(75)  'Hafen
  multiSprites$(4) = CHR$(0,0)+MKI$(80)+CHR$(255,0)+MKI$(82)+CHR$(1,0)+MKI$(83)+CHR$(0,1)+MKI$(85)  'Flughafen (Eingang oben)
  multiSprites$(5) = CHR$(0,0)+MKI$(81)+CHR$(255,255)+MKI$(82)+CHR$(1,255)+MKI$(83)+CHR$(0,255)+MKI$(84)  'Flughafen (Eingang unten)
  multiSprites$(6) = CHR$(0,0)+MKI$(66)+CHR$(255,0)+MKI$(68)+CHR$(255,255)+MKI$(67)  'Traininglager
  multiSprites$(7) = CHR$(0,0)+MKI$(44)+CHR$(1,255)+MKI$(46)+CHR$(1,0)+MKI$(47)+CHR$(2,0)+MKI$(49)  'Depot (Eingang links)
  multiSprites$(8) = CHR$(0,0)+MKI$(45)+CHR$(255,255)+MKI$(46)+CHR$(255,0)+MKI$(47)+CHR$(254,0)+MKI$(48)  'Depot (Eingang rechts)
  multiSprites$(9) = CHR$(0,0)+MKI$(563)+CHR$(255,0)+MKI$(579)+CHR$(1,0)+MKI$(580)+CHR$(0,1)+MKI$(569)  'Titan-Net Festung
  '
  multiSprites$(10) = CHR$(0,0)+MKI$(292)+CHR$(255,0)+MKI$(294)+CHR$(1,0)+MKI$(295)+CHR$(0,1)+MKI$(293)  'Berg (4 Felder)
  multiSprites$(11) = CHR$(0,0)+MKI$(300)+CHR$(0,255)+MKI$(292)+CHR$(254,0)+MKI$(294)+CHR$(2,0)+MKI$(295)+CHR$(0,1)+MKI$(293)+CHR$(255,255)+MKI$(296)+CHR$(1,255)+MKI$(298)+CHR$(255,0)+MKI$(297)+CHR$(1,0)+MKI$(299)  'Berg (9 Felder)
  multiSprites$(12) = CHR$(0,0)+MKI$(307)+CHR$(0,1)+MKI$(308)+CHR$(255,0)+MKI$(309)+CHR$(1,0)+MKI$(310)+CHR$(254,0)+MKI$(296)+CHR$(255,255)+MKI$(296)+CHR$(2,0)+MKI$(298)+CHR$(1,255)+MKI$(298) _
                    + CHR$(254,1)+MKI$(297)+CHR$(255,1)+MKI$(297)+CHR$(2,1)+MKI$(299)+CHR$(1,1)+MKI$(299)+CHR$(0,255)+MKI$(292)+CHR$(253,0)+MKI$(294)+CHR$(3,0)+MKI$(295)+CHR$(0,2)+MKI$(293)  'Berg (16 Felder)
  multiSprites$(13) = CHR$(0,0)+MKI$(279)+CHR$(255,0)+MKI$(281)+CHR$(1,0)+MKI$(282)+CHR$(0,1)+MKI$(280)  'H�gel (4 Felder)
  multiSprites$(14) = CHR$(0,0)+MKI$(445)+CHR$(255,0)+MKI$(447)+CHR$(1,0)+MKI$(448)+CHR$(0,1)+MKI$(446)  'H�gel Dschungelterrain (4 Felder)
  multiSprites$(15) = CHR$(0,0)+MKI$(440)+CHR$(255,0)+MKI$(442)+CHR$(1,0)+MKI$(443)+CHR$(0,1)+MKI$(441)  'H�gel Dschungelterrain (4 Felder)
END SUB



'Bildschirmmaske definieren
SUB INITAREAS
  LOCAL i&

  'Kartenbereich
  maparea.left = 11
  maparea.top = 11
  maparea.right = windowWidth&-422
  maparea.bottom = windowHeight&-11

  'Palettenbereich
  palettearea.left = windowWidth&-410
  palettearea.top = 42
  palettearea.right = windowWidth&-11
  palettearea.bottom = windowHeight&-73

  'Buttons
  buttonarea.left = windowWidth&-410
  buttonarea.top = windowHeight&-72
  buttonarea.right = windowWidth&-11
  buttonarea.bottom = windowHeight&-11

  'Shop (�berlagert Palettenbereich)
  shoparea.left = windowWidth&-410
  shoparea.top = windowHeight&-230
  shoparea.right = windowWidth&-11
  shoparea.bottom = windowHeight&-73

  'Allgemeine Produktionspalette
  gpmarea.left = palettearea.left+10
  gpmarea.top = palettearea.top+26
  gpmarea.right = gpmarea.left+389
  gpmarea.bottom = gpmarea.top+62

  'Terrain
  miscsettingsarea.left = palettearea.left
  miscsettingsarea.top = palettearea.top+117
  miscsettingsarea.right = palettearea.right
  miscsettingsarea.bottom = miscsettingsarea.top+40

  'Allianzen
  allyarea.left = palettearea.left+70
  allyarea.top = palettearea.top+119+50
  allyarea.right = allyarea.left+105
  allyarea.bottom = allyarea.top+115

  'AI-Maske
  aimaskarea.left = palettearea.left+185
  aimaskarea.top = palettearea.top+119+50
  aimaskarea.right = aimaskarea.left+15
  aimaskarea.bottom = palettearea.bottom

  'Karten-Beschreibung
  mapdescriptionarea.left = palettearea.left
  mapdescriptionarea.top = palettearea.top+312
  mapdescriptionarea.right = palettearea.right
  mapdescriptionarea.bottom = palettearea.top+412

  'Eigene Meldungen
  custommessagesarea.left = palettearea.left
  custommessagesarea.top = palettearea.top+467
  custommessagesarea.right = palettearea.right
  custommessagesarea.bottom = palettearea.bottom

  'Neue Aktion
  newactionarea.left = palettearea.left
  newactionarea.top = palettearea.top+23
  newactionarea.right = palettearea.right
  newactionarea.bottom = newactionarea.top+93

  'Aktionen
  actionarea.left = palettearea.left
  actionarea.top = palettearea.top+93
  actionarea.right = palettearea.right
  actionarea.bottom = palettearea.bottom

  'neue Karte
  newmaparea.left = palettearea.left
  newmaparea.top = palettearea.top+23
  newmaparea.right = palettearea.right
  newmaparea.bottom = newmaparea.top+40

  'Terrain f�r neue Karte
  startterrainarea.left = palettearea.left+9
  startterrainarea.top = newmaparea.bottom+10
  startterrainarea.right = palettearea.right-9
  startterrainarea.bottom = palettearea.bottom+24

  'Sprachauswahl
  languagearea.left = palettearea.left+10
  languagearea.top = palettearea.top+26
  languagearea.right = languagearea.left+389
  languagearea.bottom = languagearea.top+60

  'allgemeine Einstellungen
  generaloptionsarea.left = palettearea.left+10
  generaloptionsarea.top = languagearea.bottom+23
  generaloptionsarea.right = languagearea.left+389
  generaloptionsarea.bottom = generaloptionsarea.top+60

  'Validationsmeldungen
  validationarea.left = palettearea.left
  validationarea.top = palettearea.top+5
  validationarea.right = palettearea.right
  validationarea.bottom = palettearea.bottom
END SUB



'Missionsnamen initialisieren
SUB INITMAPNAMES
  LOCAL a$, n&, i&
  REDIM mapnames$(40)

  IF bi2020support& <> 0 THEN
    'Datei einlesen
    a$ = READFILECONTENT$(EXEPATH$+"MIS\MAPCODES.TXT")
    IF a$ <> "" THEN
      n& = TextToArray&(a$, mapnames$())
      CALL BIDebugLog("Found "+FORMAT$(n&)+" map codes in MAPCODES.TXT")
      EXIT SUB
    END IF
  END IF

  CALL BIDebugLog("MAPCODES.TXT not found")
  IF isBI3& = 0 THEN
    mapnames$(0) = "AMPORGE"
    mapnames$(1) = "JOGRWAI"
    mapnames$(2) = "GEGIDOS"
    mapnames$(3) = "WABODAE"
    mapnames$(4) = "BUFASWE"
    mapnames$(5) = "GEHAUWA"
    mapnames$(7) = "OLARIBU"
    mapnames$(8) = "FITORGE"
    mapnames$(11) = "WABIKDO"
    mapnames$(12) = "GEEUSAT"
    mapnames$(15) = "KAIMAWA"
    mapnames$(16) = "GEDEROM"
    mapnames$(21) = "ULUARGE"
    mapnames$(23) = "ABUNDWA"
    mapnames$(25) = "WAFEFAL"
    mapnames$(26) = "BUSALUG"
    mapnames$(29) = "GEKEFZU"
    mapnames$(31) = "DAFATWA"
    mapnames$(32) = "SIETIBU"
    mapnames$(39) = "LANADGE"

    mapnames$(35) = "YETUDWA"
    mapnames$(36) = "WAGOPAY"
    mapnames$(37) = "ZAFLUGE"
    mapnames$(40) = "SKATZWA"
  ELSE
    REDIM mapnames$(20)
    FOR i& = 1 TO 20
      mapnames$(i&) = "BI3-"+FORMAT$(i&, "000")
    NEXT i&
  END IF
END SUB



'Einheitennamen initialisieren
SUB INITUNITNAMES
  LOCAL i&
  DIM unitnames$(63)

  FOR i& = 1 TO DATACOUNT
    unitnames$(i&-1) = READ$(i&)
  NEXT i&

  DATA "PLANUM-5", "REGIO", "SINUS", "RANGER", "BUGGY"
  DATA "ORION-OR3", "DEMON-I31", "IMPERATOR-SP", "TECHNOTRAX", "SNAKE"
  DATA "STING", "SAMURAI-2", "PULSAR-A3", "NASHORN", "SPRING-1"
  DATA "ELIXIR-2", "TROLL-I42", "ARCHIMEDES", "ALGOL", "ALCOR"
  DATA "RUNE", "MEDUSA", "SKULL", "IONSTAR", "SUPER-VIRUS"
  DATA "ATLAS", "DOLMEN-Z1", "ANACONDA-Z2", "EXCALIBUR-Z3", "MENHIR-Z4"
  DATA "MONOLITH-Z6", "UHUR-51", "GHOST-FB3", "THUNDER-FX", "STORMBRINGER"
  DATA "DRAGON-H1", "GENOM-J1", "EXTERMINATOR", "SPERBER-TB4", "SPECTRUM"
  DATA "CRUX", "GUPPI-H2", "REX", "M�VE-SX1", "PATRIX"
  DATA "TITAN-N2", "SHELL-S3", "ORCA-U7", "POLAR-C6", "ZENIT-MBS19"
  DATA "HYDRA", "VADER-D1", "COMET-FP42"
END SUB



'Nachrichten initialisieren
SUB INITMESSAGES
  LOCAL i&, n&

  n& = DATACOUNT
  DIM msgtext$(n&-1)
  FOR i& = 1 TO n&
    msgtext$(i&-1) = READ$(i&)
  NEXT i&

  '0
  DATA "Zielgeb�ude besetzt"
  DATA "Leichter Regen"
  DATA "Starker Regen"
  DATA "Leichter Schnee"
  DATA "Neuschnee"
  '5
  DATA "Ausgepr�gtes Hoch"
  DATA "Sieg"
  DATA "Niederlage"
  DATA "Intro LANADGE"
  DATA "Intro SIETIBU"
  '10
  DATA "Fahrende Festung gesichtet"
  DATA "Partisanen ignorieren"
  DATA "Drohne gesichtet"
  DATA "St�rfahrzeug gesichtet"
  DATA "Intro Fahrende Festung"
  '15
  DATA "Intro Drohne"
  DATA "Intro St�rfahrzeug"
  DATA "Intro General Odan"
  DATA "Intro Beg Beb"
  DATA "Intro AMPORGE"
  '20
  DATA "Intro JOGRWAI"
  DATA "Intro GEGIDOS"
  DATA "Intro WABODAE"
  DATA "Intro BUFASWE"
  DATA "Intro GEHAUWA"
  '25
  DATA "Aldinium sammeln"
  DATA "Intro Aisascia"
  DATA "Achtung Minen"
  DATA "Aufkl�rungssatelit ausgefallen"
  DATA "Intro Buggy"
  '30
  DATA "Intro Kaskaia be Con"
  DATA "Partisane Mol Durag"
  DATA "Br�ckensprengung"
  DATA "Waffenstillstand"
  DATA "Fehlinformation"
  '35
  DATA "Alaia verschleppt"
  DATA "Warnung Kel Gon Garin"
  DATA "Landungsboote erobern"
  DATA "Alaia befreit"
  DATA "Br�cke zerst�rt"
  '40
  DATA "Rebellen festgenommen"
  DATA "Admiral ver�rgert"
  DATA "Admiral ist gegangen"
  DATA "Geheimdepot im Hafen"
  DATA "Schelte vom Rat"
  '45
  DATA "Dem Rat nicht vertrauen"
  DATA "Aisascias Drohung"
  DATA "Intro OLARIBU"
  DATA "Mol Durags Drohung"
  DATA "Raketenpanzerfabrik"
  '50
  DATA "Nachschubswege sabotiert"
  DATA "Mol Durags R�ckzug"
  DATA "Gr��e von den Rebellen"
  DATA "Intro FITORGE"
  DATA "Rebellen nehmen Fabrik ein"
  '55
  DATA "Drohung der Kais"
  DATA "Intro GEKEFZU"
  DATA "Aldinium entdeckt"
  DATA "Banditen vertreiben"
  DATA "Letzte Feindverb�nde"
  '60
  DATA "Aisascia entstellt"
  DATA "Demons eingekesselt"
  DATA "62 - undefiniert"
  DATA "Intro KAIMAWA"
  DATA "Hafen der Kais einnehmen"
  '65
  DATA "Intro WABIKDO"
  DATA "Intro ULUARGE"
  DATA "Zugefrorenen Flu� �berqueren"
  DATA "Titannet baut Br�cke"
  DATA "Berg auf Insel erkunden"
  '70
  DATA "Feind schwach im Norden"
  DATA "Intro WAFEFAL"
  DATA "Intro BUSALUG"
  DATA "Feindliche Flugobjekte"
  DATA "Nachschubvorrichtungen"
  '75
  DATA "Hinweis Treibstoff"
  DATA "Hinweis Klemmtechnik"
  DATA "Hinweis Waffen"
  DATA "Hinweis Munition"
  DATA "Hinweis Wetter"
  '80
  DATA "Hinweis unbewaffnet"
  DATA "Hinweis Gel�nde"
  DATA "Intro ABUNDWA"
  DATA "Drohnung Delf 1A"
  DATA "Nachschub Kriegsflotte"
  '85
  DATA "Nachschub Aufkl�rer"
  DATA "Intro DAFATWA"
  DATA "Aldinium entdeckt"
  DATA "Es war eine Falle!"
  DATA "Mol Durags Angriff"
  '90
  DATA "Intro GEEUSAT"
  DATA "Falle durch ROOM?"
  DATA "ROOM vernichtet"
  DATA "Aisascia �ber ROOM"
  DATA "Intro GEDEROM"
  '95
  DATA "Kuhn Kaaps Drohung"
  DATA "96 - undefinert"
  DATA "97 - undefinert"
  DATA "Kuhn Kaaps Dank"
END SUB



'Pinsel und Fonts erzeugen
SUB INITBRUSHES
  LOCAL i&

  'Farbcodes der Spieler
  DIM playerRGB&(5), brushGreyscale&(9)
  playerRGB&(0) = D2D.CreateSolidBrush(0, 0, 255)
  playerRGB&(1) = D2D.CreateSolidBrush(255, 0, 0)
  playerRGB&(2) = D2D.CreateSolidBrush(255, 127, 0)
  playerRGB&(3) = D2D.CreateSolidBrush(110, 100, 0)
  playerRGB&(4) = D2D.CreateSolidBrush(160, 160, 120)
  playerRGB&(5) = D2D.CreateSolidBrush(170, 170, 0)

  'Pinsel erzeugen
  brushWhite& = D2D.CreateSolidBrush(255, 255, 255)
  brushBlack& = D2D.CreateSolidBrush(0, 0, 0)
  brushGrey96& = D2D.CreateSolidBrush(96, 96, 96)
  brushRed& = D2D.CreateSolidBrush(255, 0, 0)
  brushGreen& = D2D.CreateSolidBrush(0, 128, 0)
  brushBorder& = D2D.CreateSolidBrush(16, 16, 16)
  IF isBI3& = 0 THEN
    brushBackground& = D2D.CreateSolidBrush(160, 200, 224)
    brushSelectedTab& = D2D.CreateSolidBrush(168, 208, 232)
    brushHighlightedTab& = D2D.CreateSolidBrush(142, 178, 200)
    brushInactiveTab& = D2D.CreateSolidBrush(106, 133, 150)
    brushEditBackground& = D2D.CreateSolidBrush(80, 100, 112)
    brushButton& = D2D.CreateSolidBrush(114, 143, 160)
  ELSE
    brushBackground& = D2D.CreateSolidBrush(160, 32, 32)
    brushSelectedTab& = D2D.CreateSolidBrush(176, 44, 44)
    brushHighlightedTab& = D2D.CreateSolidBrush(112, 16, 16)
    brushInactiveTab& = D2D.CreateSolidBrush(88, 0, 0)
    brushEditBackground& = D2D.CreateSolidBrush(144, 60, 40)
    brushButton& = D2D.CreateSolidBrush(176, 32, 32)
  END IF
  FOR i& = 0 TO 9
    brushGreyscale&(i&) = D2D.CreateSolidBrush(i&*16+64, i&*16+64, i&*16+64)
  NEXT i&

  'Text-Font erzeugen
  hMINIFONT& = D2D.CreateFont("Arial", 0, 8)
  hTEXTFONT& = D2D.CreateFont("Arial", 0, 10)
  hBUTTONFONT& = D2D.CreateFont("Arial", 1, 10)
  hBIGFONT& = D2D.CreateFont("Arial", 1, 14)
END SUB



'Controls erzeugen
SUB INITCONTROLS
  LOCAL i&, a$

  'Buttons
  buttonActions = CLASS "DXCONTROL"
  buttonActions.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+0), buttonarea.left+2, buttonarea.top+2, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  buttonProperties = CLASS "DXCONTROL"
  buttonProperties.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+1), buttonarea.left+102, buttonarea.top+2, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  buttonValidate = CLASS "DXCONTROL"
  buttonValidate.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+2), buttonarea.left+202, buttonarea.top+2, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  buttonOptions = CLASS "DXCONTROL"
  buttonOptions.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+3), buttonarea.left+302, buttonarea.top+2, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  '
  buttonNew = CLASS "DXCONTROL"
  buttonNew.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+4), buttonarea.left+2, buttonarea.top+32, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  buttonLoad = CLASS "DXCONTROL"
  buttonLoad.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+5), buttonarea.left+102, buttonarea.top+32, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  buttonSave = CLASS "DXCONTROL"
  buttonSave.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+6), buttonarea.left+202, buttonarea.top+32, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)
  buttonQuit = CLASS "DXCONTROL"
  buttonQuit.InitButton(D2D, GETWORD$(%WORDSTART_BUTTON+7), buttonarea.left+302, buttonarea.top+32, 96, 27, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(BUTTONPRESSED), 1)

  'Tabpage Scrollbars
  REDIM scrollbarTabpages(4)
  FOR i& = 0 TO 4
    scrollbarTabpages(i&) = CLASS "DXCONTROL"
    scrollbarTabpages(i&).InitScrollbar(D2D, 1, 1, palettearea.right-16, palettearea.top, 16, palettearea.bottom-palettearea.top, brushBorder&, brushHighlightedTab&, brushButton&, 0)
    scrollbarTabpages(i&).Enabled = 1
  NEXT i&

  'Shop Dialog
  editShopname = CLASS "DXCONTROL"
  editShopname.InitEdit(D2D, "", "", shoparea.left+3, shoparea.top+3, shoparea.right-shoparea.left-6, 24, 16, hBIGFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editEnergy = CLASS "DXCONTROL"
  editEnergy.InitEdit(D2D, "E", "", shoparea.left+24, shoparea.top+40, 30, 20, 2, hBUTTONFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editMaterial = CLASS "DXCONTROL"
  editMaterial.InitEdit(D2D, "M", "", shoparea.left+24, shoparea.top+70, 30, 20, 2, hBUTTONFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editEnergyPlus = CLASS "DXCONTROL"
  editEnergyPlus.InitEdit(D2D, "E+", "", shoparea.left+24, shoparea.top+100, 30, 20, 2, hBUTTONFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editMaterialPlus = CLASS "DXCONTROL"
  editMaterialPlus.InitEdit(D2D, "M+", "", shoparea.left+24, shoparea.top+130, 30, 20, 2, hBUTTONFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  '
  listboxOwner = CLASS "DXCONTROL"
  listboxOwner.InitListbox(D2D, shoparea.right-130, shoparea.top+40, 122, 20, CREATEPLAYERLISTBOXITEMS$+";Neutral", hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED),0)
  listboxShoptype = CLASS "DXCONTROL"
  listboxShoptype.InitListbox(D2D, shoparea.right-130, shoparea.top+70, 122, 20, CREATELISTBOXITEMS$(%WORDSTART_SHOPTYPE, 7), hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)

  'AI Point
  listboxAICommand = CLASS "DXCONTROL"
  listboxAICommand.InitListbox(D2D, shoparea.left+80, shoparea.top+40, 122, 20, CREATELISTBOXITEMS$(%WORDSTART_AICOMMAND, 9), hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)

  'Karteneigenschaften
  listboxTerrain = CLASS "DXCONTROL"
  listboxTerrain.InitListbox(D2D, miscsettingsarea.left+5, miscsettingsarea.top, 122, 20, CREATELISTBOXITEMS$(%WORDSTART_TERRAIN, 4), hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxWeather = CLASS "DXCONTROL"
  listboxWeather.InitListbox(D2D, miscsettingsarea.left+138, miscsettingsarea.top, 122, 20, CREATELISTBOXITEMS$(%WORDSTART_WEATHER, 5), hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxWinCond = CLASS "DXCONTROL"
  listboxWinCond.InitListbox(D2D, miscsettingsarea.left+271, miscsettingsarea.top, 122, 20, "0;1;2;3;4;5;6;7;8;9", hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  a$ = ""
  FOR i& = 0 TO UBOUND(mapnames$())
    IF a$ <> "" THEN a$ = a$+";"
    a$ = a$+FORMAT$(i&)+" - "+mapnames$(i&)
  NEXT i&
  FOR i& = 0 TO %MAXPLAYERS-1
    editPlayerName(i&) = CLASS "DXCONTROL"
    editPlayerName(i&).InitEdit(D2D, "", GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(i&+1), palettearea.left+2, allyarea.top-1+i&*20, 60, 16, 16, hBUTTONFONT&, playerRGB&(i&), brushBorder&, brushWhite&, 0)
  NEXT i&
  listboxNextmap = CLASS "DXCONTROL"
  listboxNextmap.InitListbox(D2D, aimaskarea.left+40, aimaskarea.top, 122, 20, a$, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxBonusmap = CLASS "DXCONTROL"
  listboxBonusmap.InitListbox(D2D, aimaskarea.left+40, aimaskarea.top+50, 122, 20, a$, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  editMapShortDescr = CLASS "DXCONTROL"
  editMapShortDescr.InitEdit(D2D, "", "", mapdescriptionarea.left+3, mapdescriptionarea.top+3, mapdescriptionarea.right-mapdescriptionarea.left-6, 24, 24, hBIGFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editMapDescription = CLASS "DXCONTROL"
  editMapDescription.InitEditMultiline(D2D, "", "", mapdescriptionarea.left+3, mapdescriptionarea.top+30, mapdescriptionarea.right-mapdescriptionarea.left-6, 96, 100, hBIGFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  '
  listboxCustomMessages = CLASS "DXCONTROL"
  listboxCustomMessages.InitListbox(D2D, custommessagesarea.right-140, custommessagesarea.top-23, 122, 20, $CUSTOMMSGIDS, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)
  editCustomMessageGER = CLASS "DXCONTROL"
  editCustomMessageGER.InitEditMultiline(D2D, "", "", custommessagesarea.left+3, custommessagesarea.top+23, custommessagesarea.right-custommessagesarea.left-6, 96, %CUSTMSGMAXLEN, hTEXTFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editCustomMessageENG = CLASS "DXCONTROL"
  editCustomMessageENG.InitEditMultiline(D2D, "", "", custommessagesarea.left+3, custommessagesarea.top+143, custommessagesarea.right-custommessagesarea.left-6, 96, %CUSTMSGMAXLEN, hTEXTFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editCustomMessageUserLang = CLASS "DXCONTROL"
  editCustomMessageUserLang.InitEditMultiline(D2D, "", "", custommessagesarea.left+3, custommessagesarea.top+263, custommessagesarea.right-custommessagesarea.left-6, 96, %CUSTMSGMAXLEN, hTEXTFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)
  editCustomMessageUserLang.DisabledColor = brushGrey96&
  editCustomLangCode = CLASS "DXCONTROL"
  editCustomLangCode.InitEdit(D2D, "", custMsgUserLang$, custommessagesarea.right-43, custommessagesarea.top+243, 40, 16, 3, hMINIFONT&, brushWhite&, brushBorder&, brushEditBackground&, 0)

  'Aktionen
  DIM buttonDeleteAction(%MAXACTIONS-1)
  FOR i& = 0 TO %MAXACTIONS-1
    buttonDeleteAction(i&) = CLASS "DXCONTROL"
    buttonDeleteAction(i&).InitButton(D2D, "X", actionarea.left+2, actionarea.top+30+i&*20, 16, 16, hBUTTONFONT&, brushRed&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(DELETEACTION), 0)
  NEXT i&
  '
  buttonNewAction = CLASS "DXCONTROL"
  buttonNewAction.InitButton(D2D, "+", newactionarea.right-20, newactionarea.top-20, 16, 16, hBUTTONFONT&, brushGreen&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(INSERTACTION), 0)
  listboxPlayer = CLASS "DXCONTROL"
  listboxPlayer.InitListbox(D2D, newactionarea.left+50, newactionarea.top+2, 122, 20, CREATEPLAYERLISTBOXITEMS$, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxTurn = CLASS "DXCONTROL"
  a$ = "0"
  FOR i& = 1 TO 99
    a$ = a$+";"+FORMAT$(i&)
  NEXT i&
  listboxTurn.InitListbox(D2D, newactionarea.left+50, newactionarea.top+24, 122, 20, a$, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxMovement = CLASS "DXCONTROL"
  a$ = "0"
  FOR i& = 1 TO 20
    a$ = a$+";"+FORMAT$(i&)
  NEXT i&
  listboxMovement.InitListbox(D2D, newactionarea.left+50, newactionarea.top+46, 122, 20, a$, hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  i& = IIF&(bi2020support& = 0, 7, 13)
  listboxAction = CLASS "DXCONTROL"
  listboxAction.InitListbox(D2D, newactionarea.left+250, newactionarea.top+2, 122, 20, CREATELISTBOXITEMS$(%WORDSTART_ACTIONLIST, i&), hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)
  listboxParam1 = CLASS "DXCONTROL"
  listboxParam1.InitListbox(D2D, newactionarea.left+250, newactionarea.top+24, 122, 20, "n/a", hMINIFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)
  listboxParam2 = CLASS "DXCONTROL"
  listboxParam2.InitListbox(D2D, newactionarea.left+250, newactionarea.top+46, 122, 20, "n/a", hMINIFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxParam3 = CLASS "DXCONTROL"
  listboxParam3.InitListbox(D2D, newactionarea.left+312, newactionarea.top+46, 60, 20, "n/a", hMINIFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  listboxLevel = CLASS "DXCONTROL"
  listboxLevel.InitListbox(D2D, newactionarea.left+250, newactionarea.top-22, 122, 20, "0;1;2;3;4;5;6;7;8;9;10", hMINIFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, 0, 0)
  '
  actionsPerPage& = (actionarea.bottom-actionarea.top-48)/20
  scrollbarActions = CLASS "DXCONTROL"
  scrollbarActions.InitScrollbar(D2D, actionsPerPage&, actionsPerPage&, actionarea.right-16, actionarea.top+24, 16, actionarea.bottom-actionarea.top-48, brushBorder&, brushHighlightedTab&, brushButton&, 0)

  'neue Karte
  listboxWidth = CLASS "DXCONTROL"
  listboxWidth.InitListbox(D2D, newmaparea.left+65, newmaparea.top+2, 122, 20, "16;20;24;28;32;36;40;44;48;52;56;60;64", hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)
  listboxHeight = CLASS "DXCONTROL"
  listboxHeight.InitListbox(D2D, newmaparea.left+260, newmaparea.top+2, 122, 20, "16;20;24;28;32;36;40;44;48;52;56;60;64", hBUTTONFONT&, brushBlack&, brushBorder&, brushButton&, brushHighlightedTab&, CODEPTR(LISTBOXITEMCHANGED), 0)

  'Einstellungen
  radiogroupLanguage = CLASS "DXCONTROL"
  a$ = "          Deutsch;          English"
  radiogroupLanguage.InitRadiogroup(D2D, languagearea.left, languagearea.top, languagearea.right-languagearea.left, languagearea.bottom-languagearea.top, a$, hBUTTONFONT&, brushBlack&, brushBorder&, CODEPTR(SETTINGSCHANGED), 0)
  radiogroupLanguage.SelectedItem = selectedLanguage&
  '
  checkboxReopen = CLASS "DXCONTROL"
  checkboxReopen.InitCheckbox(D2D, GETWORD$(%WORDSTART_OPTIONS+2), generaloptionsarea.left, generaloptionsarea.top+2, generaloptionsarea.right-generaloptionsarea.left, 24, hBUTTONFONT&, brushBlack&, brushBorder&, CODEPTR(SETTINGSCHANGED), 0)
  checkboxReopen.SelectedItem = reopenLastMap&
  '
  checkboxBI2020 = CLASS "DXCONTROL"
  checkboxBI2020.InitCheckbox(D2D, GETWORD$(%WORDSTART_OPTIONS+3), generaloptionsarea.left, generaloptionsarea.top+32, generaloptionsarea.right-generaloptionsarea.left, 24, hBUTTONFONT&, brushBlack&, brushBorder&, CODEPTR(SETTINGSCHANGED), 0)
  checkboxBI2020.SelectedItem = bi2020support&
END SUB



'Liefert die Message-ID der ersten Nachricht, die ein Missionbriefing darstellt
FUNCTION GETMISSIONBRIEFINGID&
  LOCAL i&, n&, category&, tp&, v&, briefingIds$

  briefingIds$ = CHR$(8, 9, 19, 20, 21, 22, 23, 24, 47, 53, 56, 63, 65, 66, 71, 72, 82, 86, 90, 94)

  n& = LEN(actions$)/40
  FOR i& = 0 TO n&-1
    category& = ASC(actions$, i&*40+1)
    tp& = ASC(actions$, i&*40+8)
    v& = CVI(actions$, i&*40+13)
    IF category& = 0 AND tp& = 3 AND INSTR(briefingIds$, CHR$(v&)) > 0 THEN
      GETMISSIONBRIEFINGID& = v&
      EXIT FUNCTION
    END IF
  NEXT i&

  'keine g�ltige Nachricht gefunden
  GETMISSIONBRIEFINGID& = 0
END FUNCTION



'Sucht eine Aktion mit passender Kategorie, Typ und Wert
FUNCTION FINDACTION&(c&, t&, s&)
  LOCAL i&, n&, category&, tp&, shopnr&

  n& = LEN(actions$)/40
  FOR i& = 0 TO n&-1
    category& = ASC(actions$, i&*40+1)
    tp& = ASC(actions$, i&*40+8)
    shopnr& = ASC(actions$, i&*40+15)
    IF category& = c& AND tp& = t& AND shopnr& = s& THEN
      FINDACTION& = i&
      EXIT FUNCTION
    END IF
  NEXT i&

  'keine passende Aktion gefunden
  FINDACTION& = -1
END FUNCTION



'Liefert den Shop an einer Kartenposition
FUNCTION FINDSHOP&(x&, y&)
  LOCAL shopnr&, shopx&, shopy&

  FOR shopnr& = 0 TO nshops&-1
    shopx& = shops(shopnr&).position AND (yMapFactor&-1)
    shopy& = INT(shops(shopnr&).position/yMapFactor&)
    IF shopx& = x& AND shopy& = y& THEN
      FINDSHOP& = shopnr&
      EXIT FUNCTION
    END IF
  NEXT shopnr&

  FINDSHOP& = -1
END FUNCTION



'Liefert den Shop mit einem bestimmten Namen
FUNCTION FINDSHOPBYNAME&(nm$)
  LOCAL shopnr&

  ARRAY SCAN shopnames$(0) FOR nshops&, COLLATE UCASE, =nm$, TO shopnr&

  FINDSHOPBYNAME& = IIF&(nshops& = 0 OR shopnr& = 0, -1, shopnr&-1)
END FUNCTION



'Spiegelt einen String spaltenweise (4 Bytes pro Spalte)
FUNCTION MIRRORSTRINGX$(a$, rows&)
  LOCAL b$, rowlen&, cols&, i&, k&, p&, q&, v$

  b$ = a$
  rowlen& = LEN(a$)/rows&
  cols& = rowlen&/4
  p& = 1
  q& = rowlen&-3
  FOR i& = 0 TO rows&-1
    FOR k& = 0 TO cols&-1
      MID$(b$, q&, 4) = MID$(a$, p&, 4)
      p& = p&+4
      q& = q&-4
    NEXT k&
    q& = q&+rowlen&*2
  NEXT i&

  MIRRORSTRINGX$ = b$
END FUNCTION



'Spiegelt einen String zeilenweise
FUNCTION MIRRORSTRINGY$(a$, rows&)
  LOCAL b$, rowlen&, i&, p&

  rowlen& = LEN(a$)/rows&
  p& = LEN(a$)-rowlen&+1
  FOR i& = 0 TO rows&-1
    b$ = b$+MID$(a$, p&, rowlen&)
    p& = p&-rowlen&
  NEXT i&

  MIRRORSTRINGY$ = b$
END FUNCTION



'Stra�en/Wege/Gr�ben/Schienen durch Spiegelung erzeugen (pixeldata$ enth�lt die 24 Basis-Sprites, die restlichen 40 werden hier erzeugt)
SUB MIRRORROADS(pixeldata$(), startdest&)
  LOCAL sprwd&, sprhg&, spr23&

  sprwd& = 24
  sprhg& = 24

  'Basis-Sprites auf den richtigen Index verschieben (nur Sprites 0-3 sind in der Datei an der richtigen Position)
  spr23& = sprites&(startdest&+23)
  sprites&(startdest&+63) = sprites&(startdest&+22)
  sprites&(startdest&+55) = sprites&(startdest&+21)
  sprites&(startdest&+54) = sprites&(startdest&+20)
  sprites&(startdest&+31) = sprites&(startdest&+19)
  sprites&(startdest&+30) = sprites&(startdest&+18)
  sprites&(startdest&+29) = sprites&(startdest&+17)
  sprites&(startdest&+28) = sprites&(startdest&+16)
  sprites&(startdest&+27) = sprites&(startdest&+15)
  sprites&(startdest&+23) = sprites&(startdest&+14)
  sprites&(startdest&+22) = sprites&(startdest&+13)
  sprites&(startdest&+21) = sprites&(startdest&+12)
  sprites&(startdest&+20) = sprites&(startdest&+11)
  sprites&(startdest&+19) = sprites&(startdest&+10)
  sprites&(startdest&+18) = sprites&(startdest&+9)
  sprites&(startdest&+15) = sprites&(startdest&+8)
  sprites&(startdest&+11) = sprites&(startdest&+7)
  sprites&(startdest&+9) = sprites&(startdest&+6)
  sprites&(startdest&+7) = sprites&(startdest&+5)
  sprites&(startdest&+6) = spr23&
  sprites&(startdest&+5) = sprites&(startdest&+4)

  'Sprites spiegeln
  sprites&(startdest&+4) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(2), sprhg&))
  sprites&(startdest&+8) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(1), sprhg&))
  sprites&(startdest&+10) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(4), sprhg&))
  sprites&(startdest&+12) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(3), sprhg&))
  sprites&(startdest&+13) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(7), sprhg&))
  sprites&(startdest&+14) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(5), sprhg&))
  sprites&(startdest&+16) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(2), sprhg&), sprhg&))
  sprites&(startdest&+17) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(4), sprhg&))
  sprites&(startdest&+24) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(3), sprhg&), sprhg&))
  sprites&(startdest&+25) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(7), sprhg&), sprhg&))
  sprites&(startdest&+26) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(10), sprhg&), sprhg&))
  sprites&(startdest&+32) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(2), sprhg&))
  sprites&(startdest&+33) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(3), sprhg&))
  sprites&(startdest&+34) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(11), sprhg&))
  sprites&(startdest&+35) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(16), sprhg&))
  sprites&(startdest&+36) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(9), sprhg&))
  sprites&(startdest&+37) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(10), sprhg&))
  sprites&(startdest&+38) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(13), sprhg&))
  sprites&(startdest&+39) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(18), sprhg&))
  sprites&(startdest&+40) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(4), sprhg&), sprhg&))
  sprites&(startdest&+41) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(7), sprhg&))
  sprites&(startdest&+42) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(12), sprhg&))
  sprites&(startdest&+43) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(17), sprhg&))
  sprites&(startdest&+44) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(10), sprhg&))
  sprites&(startdest&+45) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(15), sprhg&))
  sprites&(startdest&+46) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(14), sprhg&))
  sprites&(startdest&+47) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(19), sprhg&))
  sprites&(startdest&+48) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(23), sprhg&))
  sprites&(startdest&+49) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(5), sprhg&))
  sprites&(startdest&+50) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(13), sprhg&), sprhg&))
  sprites&(startdest&+51) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(18), sprhg&), sprhg&))
  sprites&(startdest&+52) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(13), sprhg&))
  sprites&(startdest&+53) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(14), sprhg&))
  sprites&(startdest&+56) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(5), sprhg&), sprhg&))
  sprites&(startdest&+57) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(8), sprhg&))
  sprites&(startdest&+58) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(14), sprhg&), sprhg&))
  sprites&(startdest&+59) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(MIRRORSTRINGY$(pixeldata$(19), sprhg&), sprhg&))
  sprites&(startdest&+60) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(18), sprhg&))
  sprites&(startdest&+61) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGX$(pixeldata$(19), sprhg&))
  sprites&(startdest&+62) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, MIRRORSTRINGY$(pixeldata$(21), sprhg&))
END SUB



'Sprites aus XCT Format nach BI2 Format konvertieren
SUB LOADSPRITEFROMXCT(a$, sprnr&, transparentcolor&)
  LOCAL e$, i&, c&, sprwd&, sprhg&

  sprwd& = 24
  sprhg& = 24

  'Sprite von 8-Bit nach 32-Bit konvertieren
  e$ = REPEAT$(sprwd&*sprhg&, MKL$(0))
  FOR i& = 1 TO sprwd&*sprhg&
    c& = ASC(a$, i&)
    IF c& <> transparentcolor& THEN MID$(e$, i&*4-3, 4) = MKL$(pal???(c&))
  NEXT i&

  sprites&(sprnr&) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, e$)
END SUB



'Sprites aus BI2 LIB Datei laden
'tp = 0 : Terrain
'tp = 1 : Einheiten (je 2 Sprites sind in der Datei, die �brigen 4 werden durch Spiegelung erzeugt)
'tp = 2 : Stra�en
FUNCTION LOADSPRITESFROMLIB&(tp&, f$, startsprnr&, endsprnr&, sprstep&, startdest&, transparentcolor&, recolor$)
  LOCAL a$, d$, e$
  LOCAL i&, k&, x&, y&, p&, q&, c&, wd&, bytesperrow&, pindex&, npl&, cnew&, pl&, plstart&, plend&, recol&
  LOCAL blockx&, blocky&, xoff&, yoff&, sprwd&, sprhg&
  LOCAL pixeldata$()

  'Datei einlesen
  a$ = READFILECONTENT$(f$)
  IF a$ = "" THEN EXIT FUNCTION
  IF startsprnr& = -1 THEN
    pindex& = CVL(a$)
    startsprnr& = 0
    endsprnr& = (LEN(a$)-pindex&)/IIF&(tp& = 1, 24, 12)-1
    recolor$ = ""
    FOR i& = startsprnr& TO endsprnr&
      recolor$ = recolor$+MKL$(i&)
    NEXT i&
    IF tp& = 1 THEN totalUnitClasses& = endsprnr&+1
  ELSE
    pindex& = CVL(a$)+startsprnr&*12
  END IF
  sprwd& = 24
  sprhg& = 24
  npl& = LEN($playercolors)

  'Einzelbilder extrahieren
  FOR i& = 0 TO endsprnr&-startsprnr&
    d$ = STRING$(sprwd&*sprhg&, 0)
    p& = CVL(a$, pindex&+9)
    pindex& = pindex&+12*sprstep&

    'Sprite von LIB nach PB konvertieren
    IF isBI3& = 0 THEN
      FOR y& = 0 TO sprhg&-1
        blocky& = INT(y&/6)
        yoff& = y& MOD 6
        FOR x& = 0 TO sprwd&-1
          blockx& = INT(x&/6)  '0..3
          xoff& = x& MOD 6     '0..5
          q& = (xoff&*4+blocky&)+(yoff&*4+blockx&)*sprwd&+1
          p& = p&+1
          ASC(d$, q&) = ASC(a$, p&)
        NEXT x&
      NEXT y&
    ELSE
      d$ = MID$(a$, p&+1, sprwd&*sprhg&)
    END IF

    'Sprite von 8-Bit nach 32-Bit konvertieren
    IF tp& = 2 THEN DIM pixeldata$(endsprnr&-startsprnr&)
    recol& = INSTR(recolor$, MKL$(i&))  'pr�fen ob Sprite in der Liste der Sprites steht, die in mehreren Farben erzeugt werden m�ssen (Einheiten, Geb�ude)
    plstart& = 1
    plend& = IIF&(recol& = 0, 1, npl&)  'falls ja, dann 7 Durchl�ufe, sonst nur 1 Durchlauf
    FOR pl& = plstart& TO plend&
      e$ = REPEAT$(sprwd&*sprhg&, MKL$(0))
      cnew& = ASC($playercolors, pl&)
      FOR k& = 1 TO sprwd&*sprhg&
        c& = ASC(d$, k&)
        IF c& <> transparentcolor& THEN
          IF recol& > 0 AND c& < 8 THEN c& = c&+cnew&
          MID$(e$, k&*4-3, 4) = MKL$(pal???(c&))
        END IF
      NEXT k&
      IF tp& = 2 THEN pixeldata$(i&) = e$
      IF tp& = 1 OR pl& = 1 THEN sprites&(startdest&+i&+(pl&-1)*%SPRITESPERPLAYER) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, e$)
      IF tp& = 0 AND recol& > 0 THEN sprites&(1437+i&+(pl&-1)*%SPRITESPERPLAYER) = D2D.CreateMemoryBitmap(sprwd&, sprhg&, e$)
    NEXT pl&
  NEXT i&

  'restliche Stra�en/Wege/Gr�ben/Schienen durch Spiegelung erzeugen
  IF tp& = 2 THEN CALL MIRRORROADS(pixeldata$(), startdest&)

  LOADSPRITESFROMLIB& = 1
END FUNCTION



'BI2 Sprites laden
FUNCTION LOADSPRITES$
  LOCAL i&, a$, f$, p$
  DIM pal???(255), sprites&(%MAXGROUNDSPRITES+7*%SPRITESPERPLAYER+4*%MAXROADSPRITES+%MAXEDITORSPRITES)

  'Datei einlesen
  f$ = GetFileLocation$("bi2ed.lib")
  IF f$ = "" THEN
    LOADSPRITES$ = "bi2ed.lib"
    EXIT FUNCTION
  END IF
  a$ = READFILECONTENT$(f$)

  'Palette extrahieren und Farben von R6:G6:B6 nach B8:G8:R8 konvertieren
  FOR i& = 0 TO 255
    pal???(i&) = ASC(a$, i&*3+1)*65536+ASC(a$, i&*3+2)*256+ASC(a$, i&*3+3)+255*16777216
  NEXT i&

  'Editor-Symbole laden
  FOR i& = 0 TO 15
    CALL LOADSPRITEFROMXCT(MID$(a$, i&*576+769, 576), i&+%EDITORSPRITESTART, 0)
  NEXT i&

  'Landschaft
  f$ = GetFileLocation$("LIB\PART"+FORMAT$(customterrainsprites&, "000")+".LIB")
  IF f$ = "" OR LOADSPRITESFROMLIB&(0, f$, 0, %MAXGROUNDSPRITES-1, 1, 0, 0, STR16TO32$(palette$(2))) = 0 THEN
    LOADSPRITES$ = "LIB\PART"+FORMAT$(customterrainsprites&, "000")+".LIB"
    EXIT FUNCTION
  END IF

  'Einheiten
  f$ = GetFileLocation$("LIB\UNIT"+FORMAT$(customunitsprites&, "000")+".LIB")
  IF f$ = "" OR LOADSPRITESFROMLIB&(1, f$, -1, -1, 2, 1337, 143, "") = 0 THEN
    LOADSPRITES$ = "LIB\UNIT"+FORMAT$(customunitsprites&, "000")+".LIB"
    EXIT FUNCTION
  END IF

  'Stra�en/Wege/Gr�ben/Schienen
  FOR i& = 0 TO 3
    f$ = GetFileLocation$("LIB\LAYR00"+FORMAT$(i&)+".LIB")
    IF f$ = "" OR LOADSPRITESFROMLIB&(2, f$, 0, 23, 1, %ROADSPIRTESTART+i&*%MAXROADSPRITES, 0, "") = 0 THEN
      LOADSPRITES$ = "LIB\LAYR00"+FORMAT$(i&)+".LIB"
      EXIT FUNCTION
    END IF
  NEXT i&

  'Slots
  prodslotspr& = D2D.CreateMemoryBitmap(26, 26, DRAWSLOT$(255*16777216+240*65536+240*256+0, 255*16777216+56*65536+50*256+40))
  normalslotspr& = D2D.CreateMemoryBitmap(26, 26, DRAWSLOT$(255*16777216+240*65536+240*256+240, 255*16777216+56*65536+50*256+40))

  'Highlights
  sprites&(%EDITORSPRITESTART+16) = D2D.CreateMemoryBitmap(24, 24, DRAWHIGHLIGHT$(128*16777216+255*65536+0*256+255))
  sprites&(%EDITORSPRITESTART+17) = D2D.CreateMemoryBitmap(24, 24, DRAWHIGHLIGHT$(128*16777216+128*65536+128*256+128))
  sprites&(%EDITORSPRITESTART+18) = D2D.CreateMemoryBitmap(24, 24, DRAWHIGHLIGHT$(128*16777216+0*65536+128*256+255))
END FUNCTION



'Ermittelt den Ort einer Datei
FUNCTION GetFileLocation$(f$)
  LOCAL fullpath$

  IF LEFT$(f$, 4) <> "LIB\" THEN
    fullpath$ = EXEPATH$+f$
  ELSE
    IF libFolder$ <> "" THEN
      fullpath$ = libFolder$+MID$(f$, 5)
    END IF
  END IF

  IF ISFILE(fullpath$) THEN GetFileLocation$ = fullpath$
END FUNCTION



'Ermittelt den Ort eines Verzeichnisses
FUNCTION GetFolderLocation$(startfolder$, foldername$)
  LOCAL i&, n&, f$, d$()
  DIM d$(999)

  'pr�fen, ob gesuchtes Verzeichnis �bergeordnetes Verzeichnis des Startverzeichnisses ist
  i& = INSTR(UCASE$(startfolder$), "\"+UCASE$(foldername$)+"\")
  IF i& > 0 THEN
    GetFolderLocation$ = LEFT$(startfolder$, i&)+foldername$+"\"
    EXIT FUNCTION
  END IF

  'pr�fen, ob gesuchtes Verzeichnis im Startverzeichnis liegt
  IF ISFOLDER(startfolder$+foldername$) THEN
    GetFolderLocation$ = startfolder$+foldername$+"\"
    EXIT FUNCTION
  END IF

  'alle Unterverzeichnisse des Startverzeichnisses ermitteln
  f$ = DIR$(startfolder$+"*", ONLY %SUBDIR)
  WHILE f$ <> "" AND n& < 1000
    d$(n&) = f$
    n& = n&+1
    f$ = DIR$
  WEND

  'alle Unterverzeichnisse durchsuchen
  FOR i& = 0 TO n&-1
    f$ = GetFolderLocation$(startfolder$+d$(i&)+"\", foldername$)
    IF f$ <> "" THEN
      GetFolderLocation$ = f$
      EXIT FUNCTION
    END IF
  NEXT i&

  GetFolderLocation$ = ""
END FUNCTION



'Ermittelt den Ort einer Datei
FUNCTION FindFileInFolder$(startfolder$, fname$)
  LOCAL i&, n&, f$, d$()
  DIM d$(999)

  'pr�fen, ob gesuchte Datei im Startverzeichnis liegt
  IF ISFILE(startfolder$+fname$) THEN
    FindFileInFolder$ = startfolder$
    EXIT FUNCTION
  END IF

  'alle Unterverzeichnisse des Startverzeichnisses ermitteln
  f$ = DIR$(startfolder$+"*", ONLY %SUBDIR)
  WHILE f$ <> "" AND n& < 1000
    d$(n&) = f$
    n& = n&+1
    f$ = DIR$
  WEND

  'alle Unterverzeichnisse durchsuchen
  FOR i& = 0 TO n&-1
    f$ = FindFileInFolder$(startfolder$+d$(i&)+"\", fname$)
    IF f$ <> "" THEN
      FindFileInFolder$ = f$
      EXIT FUNCTION
    END IF
  NEXT i&

  FindFileInFolder$ = ""
END FUNCTION



'Pr�ft, ob der Editor f�r Battle Isle 2 oder 3 gestartet wurde
SUB CheckBIVersion
  LOCAL a$

  a$ = UCASE$(libFolder$)
  IF a$ = "" THEN a$ = UCASE$(EXEPATH$)
  isBI3& = 0
  IF INSTR(a$, "BI3") > 0 OR INSTR(a$, "Battle Isle 3") > 0 OR INSTR(a$, "SDI") > 0 THEN isBI3& = 1
END SUB



'Konvertiert den Inhalt einer BI2 Textdatei in ein Array
FUNCTION TextToArray&(BYVAL a$, textlines$())
  LOCAL l&, p&, q&, nr&, mx&
  LOCAL b$

  'Zeilenumbr�che formatieren
  REPLACE CHR$(10) WITH CHR$(13) IN a$
  l& = LEN(a$)

  'Umlaute ersetzen
  REPLACE CHR$(142) WITH "�" IN a$
  REPLACE CHR$(153) WITH "�" IN a$
  REPLACE CHR$(154) WITH "�" IN a$

  'Zeilen verarbeiten
  REDIM textlines$(99)
  p& = 1
  WHILE p& <= l&
    q& = INSTR(p&, a$, CHR$(13))
    IF q& = 0 THEN q& = l&+1
    b$ = RTRIM$(MID$(a$, p&, q&-p&))
    IF LEFT$(b$, 1) = "#" AND RIGHT$(b$, 1) = "}" THEN
      'g�ltige Zeile gefunden
      nr& = VAL(MID$(b$, 2, 4))
      IF nr& > UBOUND(textlines$()) THEN REDIM PRESERVE textlines$(nr&+100)
      p& = INSTR(b$, "{")
      IF p& > 0 THEN textlines$(nr&) = MID$(b$, p&+1, LEN(b$)-p&-1)
      mx& = MAX&(mx&, nr&)
    END IF
    p& = q&+1
  WEND

  'Anzahl Element zur�ckliefern
  REDIM PRESERVE textlines$(mx&)

  TextToArray& = mx&+1
END FUNCTION



'Komprimierte TXT Datei entpacken
FUNCTION DECODETEXT$(a$)
  LOCAL id$, rep$, decoded$
  LOCAL orglen&, p&, inputlen&, c&, n&, q&, plaintextmask&, b&

  'Header auslesen
  id$ = LEFT$(a$, 4)
  IF id$ <> "TPWM" THEN
    DecodeText$ = a$
    EXIT FUNCTION
  END IF
  orglen& = CVL(a$, 5)
  inputlen& = LEN(a$)
  p& = 9

  'Zeichen dekodieren/entpacken
  WHILE p& <= inputlen&
    plaintextmask& = ASC(a$, p&)
    b& = 256
    p& = p&+1
    WHILE p& <= inputlen& AND b& > 1
      b& = b&/2
      IF (plaintextmask& AND b&) = 0 THEN
        'Klartext
        c& = ASC(a$, p&)
        decoded$ = decoded$+CHR$(c&)
        p& = p&+1
      ELSE
        'wiederholte Daten
        c& = ASC(a$, p&)
        n& = c&  'die untersten 4 Bit geben die Anzahl der zu kopierender Zeichen (+3) an
        p& = p&+1
        q& = LEN(decoded$)+1-ASC(a$, p&)
        IF n& < 16 THEN
          n& = n&+3
        ELSE
          q& = q&-256*INT(n&/16)
          n& = (n& AND 15)+3
        END IF
        IF q& > 0 THEN
          rep$ = MID$(decoded$, q&, n&)
          IF LEN(rep$) < n& THEN rep$ = LEFT$(REPEAT$(n&, rep$), n&)
          decoded$ = decoded$+rep$
          p& = p&+1
        END IF
      END IF
    WEND
  WEND

  DECODETEXT$ = LEFT$(decoded$, orglen&)
END FUNCTION



'Ermittelt den Namen der Shopdatei zu einer Missionsdatei
FUNCTION GETSHOPFILENAME$(f$)
  LOCAL shopfile$, g$, p&

  shopfile$ = f$
  p& = INSTR(-1, shopfile$, ".")
  IF p& > 0 THEN shopfile$ = LEFT$(shopfile$, p&-1)
  shopfile$ = shopfile$+".TXT"
  p& = INSTR(UCASE$(shopfile$), "\MIS\")
  IF p& > 0 THEN
    g$ = LEFT$(shopfile$, p&)+MID$("GERENG", selectedLanguage&*3+1, 3)+MID$(shopfile$, p&+4)
    IF ISFILE(g$) THEN shopfile$ = g$
  END IF

  GETSHOPFILENAME$ = shopfile$
END FUNCTION



'Shopnamen laden
FUNCTION LOADSHOPNAMES&(f$)
  LOCAL a$, b$, p&, q&, l&, nr&, s$

  'Datei einlesen
  a$ = DECODETEXT$(READFILECONTENT$(f$))
  IF a$ = "" THEN EXIT FUNCTION

  'Zeilenumbr�che formatieren
  REPLACE CHR$(10) WITH CHR$(13) IN a$
  l& = LEN(a$)

  'Umlaute ersetzen
  REPLACE CHR$(142) WITH "�" IN a$
  REPLACE CHR$(153) WITH "�" IN a$
  REPLACE CHR$(154) WITH "�" IN a$

  'Zeilen verarbeiten
  p& = 1
  WHILE p& <= l&
    q& = INSTR(p&, a$, CHR$(13))
    IF q& = 0 THEN q& = l&+1
    b$ = RTRIM$(MID$(a$, p&, q&-p&))
    IF LEFT$(b$, 1) = "#" AND RIGHT$(b$, 1) = "}" THEN
      'g�ltige Zeile gefunden
      nr& = VAL(MID$(b$, 2, 4))
      p& = INSTR(b$, "{")
      IF p& > 0 THEN
        s$ = MID$(b$, p&+1, LEN(b$)-p&-1)
        SELECT CASE nr&
        CASE 0: editMapDescription.Value = s$
        CASE 1: editMapShortDescr.Value = s$
        CASE ELSE: IF nr& >= 2 AND nr& < nshops&+2 THEN shopnames$(nr&-2) = s$
        END SELECT
      END IF
    END IF
    p& = q&+1
  WEND

  LOADSHOPNAMES& = 1
END FUNCTION



'Z�hlt Material und Energie der Spieler
SUB COUNTMATERIALENERGY
  LOCAL i&, pl&, plmask&, a$, en&()
  DIM en&(3, 5)

  'Material udn Energie ermitteln
  FOR i& = 0 TO nshops&-1
    pl& = LOG2(shops(i&).owner)
    IF pl& < 6 THEN
      en&(0, pl&) = en&(0, pl&)+shops(i&).energy
      en&(1, pl&) = en&(1, pl&)+shops(i&).material
      en&(2, pl&) = en&(2, pl&)+shops(i&).eplus
      en&(3, pl&) = en&(3, pl&)+shops(i&).mplus
    END IF
  NEXT i&

  'Daten von allen aktiven Spielern anzeigen
  plmask& = GETACTIVEPLAYERS&
  FOR pl& = 0 TO 5
    IF (plmask& AND 2^pl&) <> 0 THEN
      a$ = GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(pl&+1)+" = E: "+FORMAT$(en&(0, pl&))+" , M: "+FORMAT$(en&(1, pl&))+" , E+: "+FORMAT$(en&(2, pl&))+" , M+: "+FORMAT$(en&(3, pl&))
      CALL ADDVALIDATIONMSG(a$, -1, -1)
    END IF
  NEXT pl&
END SUB



'L�dt eine Mission
FUNCTION LOADMISSION&(f$)
  LOCAL a$, m$, g$, shopfile$
  LOCAL pMiss&, pMap&, pShop&, pActn&, pDF&, pCustMsg&
  LOCAL x&, y&, i&, j&, k&, p&, v&, dfnr&, dflen&, dx&, dy&, missNr&
'  REDIM debugdata$(99)

  'Datei einlesen
  p& = INSTR(-1, f$, "\")
  IF MID$(f$, p&+1, 4) = "MISS" THEN missNr& = VAL(MID$(f$, p&+5, 4))
  a$ = READFILECONTENT$(f$)
  nValidate& = 0

  'Header auswerten
  IF LEFT$(a$, 4) <> "MSSN" THEN CALL BIDebugLog("Error: Bad file header") : EXIT FUNCTION
  pMiss& = CVL(a$, 5)
  pMap& = CVL(a$, 9)
  pShop& = CVL(a$, 13)
  pActn& = CVL(a$, 17)
  dfLayerCount& = CVI(a$, 21)
  pDF& = CVL(a$, 23)-4
  pCustMsg& = CVL(a$, 147)
  IF MID$(a$, pActn&+1, 4) <> "ACTN" THEN CALL BIDebugLog("Error: Map has no ACTN segment") : EXIT FUNCTION
  IF MID$(a$, pShop&+1, 4) <> "SHOP" THEN CALL BIDebugLog("Error: Map has no SHOP segment") : EXIT FUNCTION
  IF MID$(a$, pMiss&+1, 4) <> "MISS" THEN CALL BIDebugLog("Error: Map has no MISS segment") : EXIT FUNCTION
  IF MID$(a$, pMap&+1, 3) <> "MAP" THEN CALL BIDebugLog("Error: Map has no MAP segment") : EXIT FUNCTION

  'Karte extrahieren
  x& = CVI(a$, pActn&+5)
  y& = CVI(a$, pActn&+7)
  CALL BIDebugLog("Map size: "+FORMAT$(x&)+" x "+FORMAT$(y&))
  CALL CREATEMAP(x&, y&, 95)
  p& = pActn&+9
  FOR k& = 0 TO 2
    FOR j& = 0 TO y&-1
      FOR i& = 0 TO x&-1
        v& = CVI(a$, p&)
        IF k& = 1 AND v& < 1 THEN v& = -1
        IF k& = 2 AND v& < -1 THEN v& = -1
        IF k& = 2 AND v& >= 0 THEN v& = v&+1337-256
        mapdata%(i&, j&, k&, 0) = v&
        p& = p&+2
      NEXT i&
    NEXT j&
  NEXT k&

  'Shop extrahieren
  editMapDescription.Value = ""
  editMapShortDescr.Value = ""
  nshops& = CVI(a$, pShop&+5)
  CALL BIDebugLog("Number of shops: "+FORMAT$(nshops&))
  FOR i& = 0 TO nshops&-1
    shopnames$(i&) = "SHOP "+FORMAT$(i&+1)
    POKE$ VARPTR(shops(i&)), MID$(a$, pShop&+7+i&*SIZEOF(TShop), SIZEOF(TShop))
  NEXT i&
  shopfile$ = GETSHOPFILENAME$(f$)
  i& = LOADSHOPNAMES&(shopfile$)
'CALL COMPAREDEBUGDATA("SHOP")

  'Missionsparameter auswerten
  '001
  '003
  '005
  '007 = n�chste Missionsnummer
  '008 = 39 = Campaign-Ende ???
  '009 = Startwetter
  '010 = vorhandene Spieler Maske
  '011 = AI Maske
  '012 = Anzahl Siegbedingungen
  '016 = Landscape
  '281
  '282
  '283
  '289
  '294 = Message-ID des Auftrags
  '295 = diese Missionsnummer
  '297
  '298
  '299 = identisch mit 298
  originalMissData$ = MID$(a$, pMiss&+5, pActn&-pMiss&-4)
  aiMask& = ASC(originalMissData$, 11)
  listboxTerrain.SelectedItem = ASC(originalMissData$, 16)
  listboxWeather.SelectedItem = ASC(originalMissData$, 9)
  listboxWinCond.SelectedItem = ASC(originalMissData$, 12)
  listboxNextmap.SelectedItem = ASC(originalMissData$, 7)
  listboxBonusmap.SelectedItem = ASC(originalMissData$, 297)
  allymatrix$ = MID$(originalMissData$, 17, 6)

'debugdata$(missNr&) = MKI$(dfLayerCount&)+originalMissData$
'MID$(debugdata$(missNr&), 153, 128) = STRING$(128, 0)
'MID$(debugdata$(missNr&), 17, 6) = STRING$(6, 0)

  'allgemeine Produktionspalette
  g$ = MID$(originalMissData$, 153, 128)
  REDIM gpm?(12, 1)
  j& = 0
  k& = 0
  FOR i& = 1 TO 64
    v& = ASC(g$, i&*2-1)
    IF (v& AND 1) = 1 AND j& < 13 THEN
      gpm?(j&, 0) = i&
      j& = j&+1
    END IF
    IF (v& AND 2) = 2 AND k& < 13 THEN
      gpm?(k&, 1) = i&
      k& = k&+1
    END IF
  NEXT i&

  'Spielernamen (nur Battle Isle 2020)
  IF bi2020support& <> 0 THEN
    IF ASC(originalMissData$, 23) > 32 THEN
      FOR i& = 0 TO %MAXPLAYERS-1
        editPlayerName(i&).Value = RTRIM$(MID$(originalMissData$, 23+i&*16, 16))
      NEXT i&
    ELSE
      FOR i& = 0 TO %MAXPLAYERS-1
        editPlayerName(i&).Value = GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(i&+1)
      NEXT i&
    END IF
  END IF

  'Aktionen extrahieren
  actions$ = MID$(a$, pMap&+5, pDF&-pMap&-4)
  CALL BIDebugLog("Number of actions: "+FORMAT$(LEN(actions$)/40))
  CALL SORTACTIONS
  actionCount& = LEN(actions$)/40

  'Material und Energie z�hlen
  CALL COUNTMATERIALENERGY

  'DF Layer extrahieren
  CALL BIDebugLog("Number of DF layers: "+FORMAT$(dfLayerCount&))
  IF dfLayerCount& > %MAXDFLAYER THEN dfLayerCount& = %MAXDFLAYER
  FOR i& = 0 TO dfLayerCount&-1
    pDF& = CVL(a$, 23+i&*4)-4
    dflen& = CVL(a$, 151+i&*4)
    IF pDF& > 0 AND dflen& > 0 THEN
      dfdata$ = MID$(a$, pDF&+1, dflen&)
      'Layer-Nummer ermitteln
      IF LEFT$(dfdata$, 2) <> "DF" OR VERIFY(MID$(dfdata$, 3, 2), "0123456789") > 0 THEN EXIT FOR
      dfnr& = VAL(MID$(dfdata$, 3, 2))
      IF dfnr& < 0 OR dfnr& >= %MAXDFLAYER THEN EXIT FOR
      IF MID$(dfdata$, 5) = NODFLAYER$ THEN ITERATE FOR
      'Karten�nderungen extrahieren
      p& = 5
      k& = 0
      WHILE p& <= dflen&
        v& = CVWRD(dfdata$, p&)  'Position
        dx& = v& AND (yMapFactor&-1)
        dy& = INT(v&/yMapFactor&)
        IF dx& >= 0 AND dx& < x& AND dy& >= 0 AND dy& < y& THEN
          mapdata%(dx&, dy&, 0, dfnr&+1) = CVI(dfdata$, p&+2)
          mapdata%(dx&, dy&, 1, dfnr&+1) = CVI(dfdata$, p&+4)
          v& = CVI(dfdata$, p&+6)
          mapdata%(dx&, dy&, 2, dfnr&+1) = IIF&(v& = -1, -1, 1337-256+v&)
          k& = k&+1
        END IF
        p& = p&+8
      WEND
      CALL ADDVALIDATIONMSG("DF"+FORMAT$(dfnr&, "00")+" : "+FORMAT$(k&)+" "+GETWORD$(%WORDSTART_MAPINFO+1), dx&, dy&)
    END IF
  NEXT i&

  'benutzerdefinierte Meldungen
  IF pCustMsg& > 0 AND MID$(a$, pCustMsg&-3, 4) = "CMSG" THEN
    'altes (ANSI) Format
    dflen& = CVL(a$, 275)
    IF dflen& > 0 THEN
      dfdata$ = MID$(a$, pCustMsg&+1, dflen&-4)
      p& = 1
      FOR i& = 0 TO %MAXCUSTOMMSG-1
        v& = CVI(dfdata$, p&)
        customMessages$$(0, i&) = MID$(dfdata$, p&+2, v&)
        p& = p&+2+v&
        v& = CVI(dfdata$, p&)
        customMessages$$(1, i&) = MID$(dfdata$, p&+2, v&)
        p& = p&+2+v&
        customMessages$$(2, i&) = ""
      NEXT i&
    END IF
  ELSE
    IF pCustMsg& > 0 AND MID$(a$, pCustMsg&-3, 4) = "UMSG" THEN
      'neues (UNICODE) Format
      dflen& = CVL(a$, 275)
      IF dflen& > 3 THEN
        custMsgUserLang$ = MID$(a$, pCustMsg&+1, 3)
        editCustomLangCode.Value = custMsgUserLang$
        dfdata$ = MID$(a$, pCustMsg&+4, dflen&-7)
        p& = 1
        FOR i& = 0 TO %MAXCUSTOMMSG-1
          v& = CVI(dfdata$, p&)
          customMessages$$(0, i&) = UTF8TOCHR$(MID$(dfdata$, p&+2, v&))
          p& = p&+2+v&
          v& = CVI(dfdata$, p&)
          customMessages$$(1, i&) = UTF8TOCHR$(MID$(dfdata$, p&+2, v&))
          p& = p&+2+v&
          v& = CVI(dfdata$, p&)
          customMessages$$(2, i&) = UTF8TOCHR$(MID$(dfdata$, p&+2, v&))
          p& = p&+2+v&
        NEXT i&
      END IF

    ELSE
      'keine Eintr�ge vorhanden
      REDIM customMessages$$(2, %MAXCUSTOMMSG-1)
    END IF
  END IF
  selectedCustomMsg& = 0
  editCustomMessageGER.Value = customMessages$$(0, selectedCustomMsg&)
  editCustomMessageENG.Value = customMessages$$(1, selectedCustomMsg&)
  editCustomMessageUserLang.Value = customMessages$$(2, selectedCustomMsg&)

  'Missionsname anzeigen
  a$ = MID$(f$, INSTR(-1, f$, "\")+1)
  missionnr& = -1
  IF LEFT$(a$, 4) = "MISS" THEN missionnr& = VAL(MID$(a$, 5))
  IF isBI3& <> 0 AND missionnr& >= 100 THEN missionnr& = missionnr&-99
  IF missionnr& > UBOUND(mapnames$()) THEN missionnr& = -1
  CALL SETCAPTION(a$)
  missionFileName$ = f$

  undoStart& = 0
  undoPos& = 0
  showWelcome& = 0
  selectedShop& = -1
  mapChanged& = 0
  buttonNewAction.Enabled = IIF&(LEN(actions$)/40 >= %MAXACTIONS, 0, 1)
  scrollbarActions.MaxScroll = LEN(actions$)/40 - actionsPerPage&

  IF nValidate& > 0 THEN
    CALL HIDEDIALOGUES(0)
    CALL SelectTab(-6)
  END IF

  LOADMISSION& = 1
END FUNCTION



'Speichert die Mission
SUB SAVEMISSION(f$)
  LOCAL mssn$, miss$, actn$, shop$, map$, df$, custmsg$, custmsgUTF8$
  LOCAL i&, j&, v&, v2&, v3&, x&, y&, wd&, hg&, msgid&, shopfile$, g$, dflen$, dfstart$, dfdata$()
  DIM dfdata$(%MAXDFLAYER-1)

  'MISS (Missionsparameter) erstellen (328 Bytes)
  IF originalMissData$ = "" THEN originalMissData$ = STRING$(328, 0)
  ASC(originalMissData$, 7) = listboxNextmap.SelectedItem
  ASC(originalMissData$, 9) = listboxWeather.SelectedItem
  ASC(originalMissData$, 10) = GETACTIVEPLAYERS&
  ASC(originalMissData$, 11) = aiMask&
  ASC(originalMissData$, 12) = listboxWinCond.SelectedItem
  ASC(originalMissData$, 16) = listboxTerrain.SelectedItem
  IF bi2020support& <> 0 THEN
    ASC(originalMissData$, 297) = listboxBonusmap.SelectedItem
    FOR i& = 0 TO %MAXPLAYERS-1
      IF editPlayerName(i&).Value = GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(i&+1) THEN
        MID$(originalMissData$, 23+i&*16, 16) = SPACE$(16)
      ELSE
        MID$(originalMissData$, 23+i&*16, 16) = editPlayerName(i&).Value+SPACE$(16-LEN(editPlayerName(i&).Value))
      END IF
    NEXT i&
  END IF
  ASC(originalMissData$, 294) = GETMISSIONBRIEFINGID&
  MID$(originalMissData$, 17, 6) = allymatrix$
  g$ = STRING$(128, 0)
  FOR i& = 0 TO 12
    v& = gpm?(i&, 0)
    IF v& > 0 THEN ASC(g$, v&*2-1) = ASC(g$, v&*2-1) OR 1
    v& = gpm?(i&, 1)
    IF v& > 0 THEN ASC(g$, v&*2-1) = ASC(g$, v&*2-1) OR 2
  NEXT i&
  MID$(originalMissData$, 153, 128) = g$
  miss$ = "MISS"+originalMissData$

  'ACTN (Karte) erstellen
  actn$ = "ACTN"+MKI$(mapwidth&)+MKI$(mapheight&)+PEEK$(VARPTR(mapdata%(0, 0, 0, 0)), mapwidth&*mapheight&*6)
  FOR j& = 0 TO mapheight&-1
    FOR i& = 0 TO mapwidth&-1
      v& = mapdata%(i&, j&, 2, 0)
      IF v& >= 0 THEN MID$(actn$, i&*2+j&*mapwidth&*2+mapwidth&*mapheight&*4+9, 2) = MKI$(v&-1337+256)
    NEXT i&
  NEXT j&

  'ungenutzte AI-Points l�schen
  FOR i& = nshops&-1 TO 0 STEP -1
    IF shops(i&).unittype = 2 AND shops(i&).aicommand = 0 THEN CALL DELETESHOP(i&)
  NEXT i&

  'Shops nach vorne sortieren
  CALL REORGANIZESHOPS

  'SHOP (Shops) erstellen
  shop$ = "SHOP"+MKI$(nshops&)
  FOR i& = 0 TO nshops&-1
    IF shops(i&).unittype = 1 AND shops(i&).nameindex > 0 THEN
      shops(i&).nameindex = i&+2
    END IF
    shop$ = shop$+PEEK$(VARPTR(shops(i&)), SIZEOF(TShop))
  NEXT i&

  'MAP (Aktionen) erstellen
  map$ = "MAP"+CHR$(0)+actions$

  'benutzerdefinierte Meldungen
  customMessages$$(0, selectedCustomMsg&) = editCustomMessageGER.Value
  customMessages$$(1, selectedCustomMsg&) = editCustomMessageENG.Value
  customMessages$$(2, selectedCustomMsg&) = editCustomMessageUserLang.Value
  custMsgUserLang$ = TRIM$(editCustomLangCode.Value)
  FOR i& = 0 TO %MAXCUSTOMMSG-1
    IF custMsgUserLang$ = "" THEN
      custmsg$ = custmsg$+MKI$(LEN(customMessages$$(0, i&)))+customMessages$$(0, i&)+MKI$(LEN(customMessages$$(1, i&)))+customMessages$$(1, i&)
    ELSE
      custmsgUTF8$ = CHRTOUTF8$(customMessages$$(0, i&))
      custmsg$ = custmsg$+MKI$(LEN(custmsgUTF8$))+custmsgUTF8$
      custmsgUTF8$ = CHRTOUTF8$(customMessages$$(1, i&))
      custmsg$ = custmsg$+MKI$(LEN(custmsgUTF8$))+custmsgUTF8$
      custmsgUTF8$ = CHRTOUTF8$(customMessages$$(2, i&))
      custmsg$ = custmsg$+MKI$(LEN(custmsgUTF8$))+custmsgUTF8$
    END IF
  NEXT i&

  'DFxx (Karten�nderungen) erstellen
  dfLayerCount& = 0
  wd& = MIN&(yMapFactor&, mapwidth&)
  hg& = MIN&(yMapFactor&, mapheight&)
  FOR i& = 1 TO %MAXDFLAYER
   'Karten�nderungen suchen
    df$ = ""
    FOR y& = 0 TO hg&-1
      FOR x& = 0 TO wd&-1
        IF mapdata%(x&, y&, 0, i&) <> 0 OR mapdata%(x&, y&, 1, i&) <> 0 OR mapdata%(x&, y&, 2, i&) <> 0 THEN
          v& = mapdata%(x&, y&, 0, i&)
          v2& = mapdata%(x&, y&, 1, i&)
          v3& = mapdata%(x&, y&, 2, i&)
          df$ = df$+MKWRD$(x&+y*yMapFactor&)+MKI$(v&)+MKI$(IIF&(v2& <= 0, -1, v2&))+MKI$(IIF&(v3& <= 0, -1, v3&-1337+256))
        END IF
      NEXT x&
    NEXT y&
    IF df$ <> "" THEN
      dfdata$(dfLayerCount&) = df$
      dfLayerCount& = dfLayerCount&+1
    END IF
  NEXT i&
  IF dfLayerCount& = 0 THEN
    dfLayerCount& = 1
    dfdata$(0) = NODFLAYER$
  END IF

  'MSSN (Header) erstellen (278 Bytes)
  mssn$ = "MSSN"+MKL$(278)+MKL$(278+LEN(miss$)+LEN(actn$)+LEN(shop$))+MKL$(278+LEN(miss$)+LEN(actn$))+MKL$(278+LEN(miss$)) _
      + MKI$(dfLayerCount&)
  v& = 278+LEN(miss$)+LEN(actn$)+LEN(shop$)+LEN(map$)+4
  df$ = ""
  FOR i& = 0 TO dfLayerCount&-1
    dfstart$ = dfstart$+MKL$(v&)
    dflen$ = dflen$+MKL$(4+LEN(dfdata$(i&)))
    v& = v&+LEN(dfdata$(i&))+4
    df$ = df$+"DF"+FORMAT$(i&, "00")+dfdata$(i&)
  NEXT i&
  IF custMsgUserLang$ = "" THEN
    IF custmsg$ <> STRING$(%MAXCUSTOMMSG*4, 0) THEN
      df$ = df$+"CMSG"+custmsg$
      dfstart$ = dfstart$+STRING$(31*4-LEN(dfstart$), 0)+MKL$(v&)
      dflen$ = dflen$+STRING$(31*4-LEN(dflen$), 0)+MKL$(4+LEN(custmsg$))
    END IF
  ELSE
    IF custmsg$ <> STRING$(%MAXCUSTOMMSG*3, 0) THEN
      df$ = df$+"UMSG"+custMsgUserLang$+custmsg$
      dfstart$ = dfstart$+STRING$(31*4-LEN(dfstart$), 0)+MKL$(v&)
      dflen$ = dflen$+STRING$(31*4-LEN(dflen$), 0)+MKL$(4+LEN(custmsg$))
    END IF
  END IF
  mssn$ = mssn$+dfstart$+STRING$(128-LEN(dfstart$), 0)
  mssn$ = mssn$+dflen$+STRING$(128-LEN(dflen$), 0)

  'Datei speichern
  CALL SAVEFILE(f$, mssn$+miss$+actn$+shop$+map$+df$)
'  CALL SAVEFILE(f$, mssn$+miss$+actn$+shop$+map$+dfdata$)
  missionFileName$ = f$

  'Shopnamen speichern
  shop$ = CHR$(13,10)+"#0000:0000{"+UCASEBI2$(editMapDescription.Value)+"}"+CHR$(13,10)+"#0001:0024{"+UCASEBI2$(editMapShortDescr.Value)+"}"+CHR$(13,10)
  FOR i& = 0 TO nshops&-1
    IF shops(i&).unittype = 1 AND shops(i&).nameindex > 0 THEN shop$ = shop$+"#"+FORMAT$(i&+2, "0000")+":0024{"+UCASEBI2$(shopnames$(i&))+"}"+CHR$(13,10)
  NEXT i&
  REPLACE "�" WITH CHR$(142) IN shop$
  REPLACE "�" WITH CHR$(153) IN shop$
  REPLACE "�" WITH CHR$(154) IN shop$
  shopfile$ = GETSHOPFILENAME$(f$)
  CALL SAVEFILE(shopfile$, shop$)

  'Missionsname anzeigen
  f$ = MID$(f$, INSTR(-1, f$, "\")+1)
  CALL SETCAPTION(f$)
  mapChanged& = 0
END SUB



'Mission-�ffnen Dialog anzeigen
SUB OPENMISSION
  LOCAL ofn AS OPENFILENAME
  LOCAL szFile AS ASCIIZ*1024
  LOCAL szFilter AS STRING*1024
  LOCAL f$

  szFile = ""
  szFilter = GETWORD$(%WORDSTART_OPENDIALOGUE+0)+CHR$(0)+"*.DAT"+CHR$(0)+GETWORD$(%WORDSTART_OPENDIALOGUE+1)+CHR$(0)+"*.*"+CHR$(0,0)

  ofn.lStructSize = SIZEOF(ofn)
  ofn.hwndOwner = hWIN&
  ofn.lpstrFile = VARPTR(szFile)
  ofn.nMaxFile = 1023
  ofn.lpstrFilter = VARPTR(szFilter)
  ofn.nFilterIndex = 1
  ofn.lpstrFileTitle = %NULL
  ofn.nMaxFileTitle = 0
  ofn.lpstrInitialDir = %NULL
  ofn.Flags = %OFN_PATHMUSTEXIST OR %OFN_FILEMUSTEXIST

  IF GetOpenFileName(ofn) <> 0 THEN
    f$ = szFile
    IF NOT LOADMISSION&(f$) THEN
      '...
    END IF
  END IF
END SUB



'Mission-Speichern Dialog anzeigen
FUNCTION SAVEDIALOG$
  LOCAL ofn AS OPENFILENAME
  LOCAL szFile AS ASCIIZ*1024
  LOCAL szFilter AS STRING*1024
  LOCAL f$

  szFile = missionFileName$
  szFilter = GETWORD$(%WORDSTART_OPENDIALOGUE+0)+CHR$(0)+"*.DAT"+CHR$(0)+GETWORD$(%WORDSTART_OPENDIALOGUE+1)+CHR$(0)+"*.*"+CHR$(0,0)

  ofn.lStructSize = SIZEOF(ofn)
  ofn.hwndOwner = hWIN&
  ofn.lpstrFile = VARPTR(szFile)
  ofn.nMaxFile = 1023
  ofn.lpstrFilter = VARPTR(szFilter)
  ofn.nFilterIndex = 1
  ofn.lpstrFileTitle = %NULL
  ofn.nMaxFileTitle = 0
  ofn.lpstrInitialDir = %NULL
  ofn.Flags = %OFN_PATHMUSTEXIST

  IF GetSaveFileName(ofn) = 0 THEN EXIT FUNCTION
  SAVEDIALOG$ = szFile
END FUNCTION



'Daten f�r nicht vorhandenen DF-Layer erzeugen
FUNCTION NODFLAYER$
  LOCAL a$, d$, i&, n&

  a$ = "444630302300950068008900240068007D007F0025007F0070005F002E0089007D007D002F007D007E007E0030007F005F005F003800950095008900390089007D007E003A007E007E007F004400830064007F0045007F007F005F00FEFEFEFEFEFEFEFEFEFEFEFFF9FEFEF9FEF9F9FEFEFE"
  n& = LEN(a$)/2
  d$ = SPACE$(n&)
  FOR i& = 1 TO n&
    ASC(d$, i&) = VAL("&H"+MID$(a$, i&*2-1, 2))
  NEXT i&

  NODFLAYER$ = d$
END FUNCTION



'Tabpage wechseln
SUB SelectTab(tabnr&)
  LOCAL i&

  selectedTab& = tabnr&
  FOR i& = 0 TO 4
    IF i& <> selectedTab& THEN scrollbarTabpages(i&).Visible = 0
  NEXT i&
END SUB



'Text in Gro�buchstaben konvertieren falls Battle Isle 2020 Features ausgeschaltet sind
FUNCTION UCASEBI2$(a$)
  IF bi2020support& = 0 THEN
    UCASEBI2$ = UCASE$(a$)
  ELSE
    UCASEBI2$ = a$
  END IF
END FUNCTION



'Neue Karte erzeugen
SUB CREATEMAP(x&, y&, terrain&)
  LOCAL i&, j&

  originalMissData$ = ""
  allymatrix$ = STRING$(6, 0)
  actions$ = ""
  nshops& = 0
  mapwidth& = x&
  mapheight& = y&
  yMapFactor& = IIF&(x& > 64 OR y& > 64, 256, 64)
  REDIM mapdata%(x&-1, y&-1, 2, %MAXDFLAYER)

  REDIM customMessages$$(2, %MAXCUSTOMMSG-1)
  editCustomMessageGER.Value = ""
  editCustomMessageENG.Value = ""
  editCustomMessageUserLang.Value = ""

  FOR j& = 0 TO y&-1
    FOR i& = 0 TO x&-1
      mapdata%(i&, j&, 0, 0) = terrain&
      mapdata%(i&, j&, 1, 0) = -1
      mapdata%(i&, j&, 2, 0) = -1
    NEXT i&
  NEXT j&
END SUB



'Sortiert die Aktionen nach Runde/Bewegung
SUB SORTACTIONS
  LOCAL i&, n&, category&, turnnr&, movenr&, level&, nr&, b$
  LOCAL t&(), a&()

  'Zeitpunkt jeder Aktion ermitteln
  n& = LEN(actions$)/40
  IF n& < 2 THEN EXIT SUB
  DIM t&(n&-1), a&(n&-1)
  FOR i& = 0 TO n&-1
    category& = ASC(actions$, i&*40+1)
    level& = GETACTIONLEVEL&(category&)
    turnnr& = ASC(actions$, i&*40+3)
    movenr& = ASC(actions$, i&*40+5)
    t&(i&) = turnnr&*1000+movenr&
    IF category& = 80 THEN t&(i&) = t&(i&)+1000000  'Niederlagen hinter Siegbedingungn
    IF category& = 0 THEN t&(i&) = t&(i&)+2000000   'alle normalen Aktionen noch hinter die Niederlagen
    t&(i&) = t&(i&)+level&*5000000
    a&(i&) = i&
  NEXT i&

  'Aktionen sortieren
  ARRAY SORT t&(0) FOR n&, TAGARRAY a&()

  'Aktionen sortiert zur�ckschreiben
  FOR i& = 0 TO n&-1
    nr& = a&(i&)
    b$ = b$+MID$(actions$, nr&*40+1, 40)
  NEXT i&
  actions$ = b$
END SUB



'Ausgew�hlten DF-Layer vollst�ndig kopieren
SUB COPYDFLAYER
  LOCAL i&, k&, mx&, my&, a$

  IF selectedDF& = 0 THEN EXIT SUB

  'unterste Zeile mit DF-Informationen suchen
  mx& = MIN&(yMapFactor&, mapwidth&)
  my& = MIN&(yMapFactor&, mapheight&)
  a$ = STRING$(mx&*2, 0)
  WHILE my& > 0
    IF PEEK$(VARPTR(mapdata%(0, my&-1, 0, selectedDF&)), mx&*2) <> a$ THEN EXIT LOOP
    IF PEEK$(VARPTR(mapdata%(0, my&-1, 1, selectedDF&)), mx&*2) <> a$ THEN EXIT LOOP
    IF PEEK$(VARPTR(mapdata%(0, my&-1, 2, selectedDF&)), mx&*2) <> a$ THEN EXIT LOOP
    my& = my&-1
  WEND

  'kompletten DF-Layer auslesen
  a$ = CHR$(mx&)+CHR$(my&)
  FOR k& = 0 TO 2
    FOR i& = 0 TO my&-1
      a$ = a$+PEEK$(VARPTR(mapdata%(0, i&, k&, selectedDF&)), mx&*2)
    NEXT i&
  NEXT k&

  'Daten f�r Zwischenablage erzeugen
  a$ = "BI2ED:DF"+FORMAT$(selectedDF&-1, "00")+HexString$(a$)

  'Daten in Zwischenablage kopieren
  CLIPBOARD SET TEXT a$
END SUB



'Markierten Bereich der Karte kopieren
SUB COPYMAP
  LOCAL a$, b$, x0&, x1&, y0&, y1&
  LOCAL shopnr&, shopx&, shopy&, i&, k&, n&, p&, p2&

  IF mapSelection.left < 0 OR mapSelection.right < 0 THEN EXIT SUB

  'Selektionsbereich bestimmen
  x0& = MIN&(mapSelection.left, mapSelection.right)
  x1& = MAX&(mapSelection.left, mapSelection.right)
  y0& = MIN&(mapSelection.top, mapSelection.bottom)
  y1& = MAX&(mapSelection.top, mapSelection.bottom)

  'Shops im Selektionsbereich suchen
  FOR shopnr& = 0 TO nshops&-1
    shopx& = shops(shopnr&).position AND (yMapFactor&-1)
    shopy& = INT(shops(shopnr&).position/yMapFactor&)
    IF shopx& >= x0& AND shopx& <= x1& AND shopy& >= y0& AND shopy& <= y1& THEN
      p& = shops(shopnr&).position
      p2& = shops(shopnr&).position2
      shops(shopnr&).position = (shopy&-y0&)*256+shopx&-x0&  'beim Kopieren immer Faktor 256 verwenden, um Shops aus kleinen Maps auch in gro�e kopieren zu k�nnen
      shops(shopnr&).position2 = shops(shopnr&).position
      b$ = b$+LEFT$(shopnames$(shopnr&), 32)+SPACE$(32-LEN(shopnames$(shopnr&)))+PEEK$(VARPTR(shops(shopnr&)), SIZEOF(TShop))
      shops(shopnr&).position = p&
      shops(shopnr&).position2 = p2&
      n& = n&+1
    END IF
  NEXT shopnr&
  a$ = CHR$(x0&)+CHR$(y0&)+CHR$(x1&-x0&+1)+CHR$(y1&-y0&+1)+CHR$(n&)

  'Karte im Selektionsbereich kopieren
  FOR k& = 0 TO 2
    FOR i& = y0& TO y1&
      a$ = a$+PEEK$(VARPTR(mapdata%(x0&, i&, k&, 0)), (x1&-x0&+1)*2)
    NEXT i&
  NEXT k&
  a$ = a$+b$

  'Daten f�r Zwischenablage erzeugen
  a$ = "BI2ED:DATA"+HexString$(a$)

  'Daten in Zwischenablage kopieren
  CLIPBOARD SET TEXT a$
END SUB



'Kartenbereich aus der Zwischenablage einf�gen
SUB PASTEMAP
  LOCAL a$, i&, k&, mapx&, mapy&, wd&, hg&, n&, p&, clipwd&, cliphg&, x0&
  LOCAL shopnr&, shopx&, shopy&

  'pr�fen, ob Mauscursor im Kartenbereich ist
  mapx& = -1
  IF mousexpos& >= maparea.left AND mousexpos& <= maparea.right AND mouseypos& >= maparea.top AND mouseypos& <= maparea.bottom THEN
    CALL GETMAPPOS((mousexpos&-maparea.left+scrollX&)/zoom#, (mouseypos&-maparea.top+scrollY&)/zoom#, mapx&, mapy&)
  END IF
  IF mapx& < 0 OR mapx& >= mapwidth& OR mapy& < 0 OR mapy& >= mapheight& THEN EXIT SUB

  'Zwischenablage auslesen
  CLIPBOARD GET TEXT TO a$
  IF LEFT$(a$, 8) = "BI2ED:DF" THEN CALL PASTEDFLAYER(MID$(a$, 9))
  IF LEFT$(a$, 10) <> "BI2ED:DATA" THEN EXIT SUB
  a$ = HexToAsc$(MID$(a$, 11))
  x0& = ASC(a$, 1) AND 1
  wd& = ASC(a$, 3)
  hg& = ASC(a$, 4)
  n& = ASC(a$, 5)
  IF x0& <> (mapx& AND 1) THEN mapx& = IIF&(mapx& = 0, 1, mapx&-1)
  clipwd& = MIN&(wd&, mapwidth&-mapx&)
  cliphg& = MIN&(hg&, mapheight&-mapy&)
  CALL SAVEUNDO

  'existierende Shops im Zielbereich l�schen
  FOR shopnr& = nshops&-1 TO 0 STEP -1
    shopx& = shops(shopnr&).position AND (yMapFactor&-1)
    shopy& = INT(shops(shopnr&).position/yMapFactor&)
    IF shopx& >= mapx& AND shopx& < mapx&+clipwd& AND shopy& >= mapy& AND shopy& < mapy&+cliphg& THEN CALL DELETESHOP(shopnr&)
  NEXT shopnr&

  'Kartenausschnitt einf�gen
  FOR k& = 0 TO 2
    p& = wd&*hg&*k&*2+6
    FOR i& = mapy& TO mapy&+cliphg&-1
      POKE$ VARPTR(mapdata%(mapx&, i&, k&, 0)), MID$(a$, p&, clipwd&*2)
      p& = p&+wd&*2
    NEXT i&
  NEXT k&

  'Shops einf�gen
  p& = wd&*hg&*6+6
  shopnr& = nshops&
  FOR i& = 1 TO n&
    IF shopnr& = %MAXSHOPS THEN EXIT FOR
    CALL INSERTSHOP(0, 0, 1)
    shopnames$(shopnr&) = RTRIM$(MID$(a$, p&, 32))
    POKE$ VARPTR(shops(shopnr&)), MID$(a$, p&+32, SIZEOF(TShop))
    shopx& = shops(shopnr&).position AND 255
    shopy& = INT(shops(shopnr&).position/256)  'beim Kopieren immer Faktor 256 verwenden, um Shops aus kleinen Maps auch in gro�e kopieren zu k�nnen
    shops(shopnr&).position = (shopx&+mapx&)+(shopy&+mapy&)*yMapFactor&
    shops(shopnr&).position2 = shops(shopnr&).position
    p& = p&+32+SIZEOF(TShop)
    shopnr& = shopnr&+1
  NEXT i&

  mapChanged& = -1
END SUB



'DF-Layer aus der Zwischenablage einf�gen
SUB PASTEDFLAYER(a$)
  LOCAL i&, k&, p&, nr&, wd&, hg&

  'DF-Layer ermitteln
  nr& = VAL(LEFT$(a$, 2))
  IF nr& < 0 OR nr& > 9 THEN EXIT SUB
  IF nr& = 0 AND LEFT$(a$, 2) <> "00" THEN EXIT SUB
  selectedDF& = nr&+1

  'Gr��e des DF-Layers ermitteln
  a$ = HexToAsc$(MID$(a$, 3))
  wd& = ASC(a$, 1)
  hg& = ASC(a$, 2)
  IF wd& > mapwidth& OR wd& > yMapFactor& THEN EXIT SUB
  IF hg& > mapheight& OR hg& > yMapFactor& THEN EXIT SUB
  CALL SAVEUNDO

  'DF-Layer einf�gen
  p& = 3
  FOR k& = 0 TO 2
    FOR i& = 0 TO hg&-1
      POKE$ VARPTR(mapdata%(0, i&, k&, selectedDF&)), MID$(a$, p&, wd&*2)
      p& = p&+wd&*2
    NEXT i&
  NEXT k&

  mapChanged& = -1
END SUB



'Shops auf der Karte hervorheben
SUB TOGGLEHIGHLIGHTSHOPS
  highlightShops& = 1-highlightShops&
END SUB



'DF-Layer wechseln
SUB SWITCHDFLAYER(nr&)
  LOCAL f$

  selectedDF& = nr&

  f$ = MID$(missionFileName$, INSTR(-1, missionFileName$, "\")+1)
  CALL SETCAPTION(f$)
END SUB



'Programmtitel anzeigen
SUB SETCAPTION(f$)
  LOCAL c AS ASCIIZ*256

  c = "Battle Isle II Map Editor - "+MID$(f$, INSTR(-1, f$, "\")+1)
  c = c+IIF$(missionnr& >= 0 AND mapnames$(missionnr&) <> "", " - "+mapnames$(missionnr&), "")+" ("+FORMAT$(mapwidth&)+"x"+FORMAT$(mapheight&)+")"
  IF selectedDF& > 0 THEN c = c+" - DF-Layer "+FORMAT$(selectedDF&-1)

  SetWindowTextA hWIN&, c
END SUB



'Shop an Cursor-Position einf�gen
SUB CREATESHOPATCURSORPOS
  LOCAL mapx&, mapy&, sprnr&, pl&

  'Kartenposition bestimmen
  IF mousexpos& >= maparea.left AND mousexpos& <= maparea.right AND mouseypos& >= maparea.top AND mouseypos& <= maparea.bottom THEN
    CALL GETMAPPOS((mousexpos&-maparea.left+scrollX&)/zoom#, (mouseypos&-maparea.top+scrollY&)/zoom#, mapx&, mapy&)
  END IF
  IF mapx& < 0 OR mapx& >= mapwidth& OR mapy& < 0 OR mapy& >= mapheight& THEN EXIT SUB

  'pr�fen, ob sich hier bereits ein Shop befindet
  IF FINDSHOP&(mapx&, mapy&) >= 0 THEN EXIT SUB

  'Art des einzuf�genden Shops bestimmen
  sprnr& = mapdata%(mapx&, mapy&, 2, 0)
  IF sprnr& < 0 THEN
    CALL INSERTSHOP(mapx&, mapy&, 16)  'geheimes Depot
  ELSE
    pl& = GETPLAYERFORUNIT&(sprnr&)
    IF pl& >= 0 AND pl& < 6 AND (aiMask& AND 2^pl&) > 0 THEN
      CALL INSERTAIPOINT(mapx&, mapy&)  'AI Point
    ELSE
      CALL INSERTSHOP(mapx&, mapy&, 0)  'Transporter
    END IF
  END IF

  'Shop-Dialog aktualisieren
  IF selectedShop& >= 0 THEN CALL UPDATESHOP
  selectedShop& = nshops&-1
  CALL HIDEDIALOGUES(%DIALOGUE_SHOP)
  CALL SHOWSHOP
END SUB



'Shop einf�gen
SUB INSERTSHOP(x&, y&, tp&)
  LOCAL i&, owner&, sprnr&

  IF nshops& = %MAXSHOPS OR x& >= yMapFactor& OR y& >= yMapFactor& THEN EXIT SUB

  sprnr& = SPRITETOUNIT&(mapdata%(x&, y&, 2, 0))
  owner& = 64  'neutral
  IF tp& = 0 THEN owner& = 2^GETPLAYERFORUNIT&(mapdata%(x&, y&, 2, 0))
  POKE$ VARPTR(shops(nshops&)), STRING$(SIZEOF(TShop), 0)
  shops(nshops&).owner = owner&
  shops(nshops&).unittype = 1
  shops(nshops&).shoptype = tp&
  shops(nshops&).shopfunction = GETSHOPFUNCTION&(tp&)
  shops(nshops&).position = x&+y&*yMapFactor&
  shops(nshops&).position2 = x&+y&*yMapFactor&
  shops(nshops&).nameindex = IIF&(sprnr& >= 0 AND sprnr& < 64, 0, 9999)
  FOR i& = 0 TO 15
    shops(nshops&).content(i&) = -1
  NEXT i&
  shopnames$(nshops&) = "SHOP "+FORMAT$(nshops&+1)
  nshops& = nshops&+1
END SUB



'Shop l�schen
SUB DELETESHOP(shopNr&)
  LOCAL n&, actionnr&,category&, tp&, snr&

  'Shop l�schen
  ARRAY DELETE shops(shopNr&)
  ARRAY DELETE shopnames$(shopNr&)
  nshops& = nshops&-1

  'Siegbedingungen anpassen, die sich auf Shops beziehen
  n& = LEN(actions$)/40
  FOR actionnr& = n&-1 TO 0 STEP -1
    category& = ASC(actions$, actionnr&*40+1)
    tp& = ASC(actions$, actionnr&*40+8)
    snr& = ASC(actions$, actionnr&*40+15)
    IF (category& = 48 OR category& = 80) AND tp& = 9 AND snr& >= shopNr& THEN
      IF snr& = shopNr& THEN
        'dieser Shop wurde gel�scht
        actions$ = LEFT$(actions$, actionnr&*40)+MID$(actions$, actionnr&*40+41)
      ELSE
        'dieser Shop hat nun eine um 1 kleinere Nummer
        ASC(actions$, actionnr&*40+15) = snr&-1
      END IF
    END IF
  NEXT actionnr&

  'Shop abw�hlen
  IF shopNr& = selectedShop& THEN selectedShop& = -1
END SUB



'Shops umsortieren, so da� Shop am Anfang der Liste stehen und Einheiten am Ende
SUB REORGANIZESHOPS
  LOCAL shops$, u$, i&, k&, n&, tp&, shopnr&
  LOCAL sname$()
  DIM sname$(nshops&-1)

  'Shops und Einheiten trennen
  FOR i& = 0 TO nshops&-1
    IF shops(i&).unittype = 1 AND shops(i&).nameindex > 0 THEN
      shops$ = shops$+PEEK$(VARPTR(shops(i&)), SIZEOF(TShop))
      sname$(n&) = shopnames$(i&)
      'alle "Spieler erobert Shop" Actions suchen und durch neue Shopnummer ersetzen
      FOR k& = 0 TO LEN(actions$)/40-1
        tp& = ASC(actions$, k&*40+8)
        shopnr& = ASC(actions$, k&*40+15)
        IF tp& = 9 AND shopnr& = i& THEN ASC(actions$, k&*40+15) = n&
      NEXT k&
      n& = n&+1
    ELSE
      u$ = u$+PEEK$(VARPTR(shops(i&)), SIZEOF(TShop))
    END IF
  NEXT i&

  'Daten zur�ckschreiben
  POKE$ VARPTR(shops(0)), shops$+u$
  FOR i& = 0 TO n&
    shopnames$(i&) = sname$(i&)
  NEXT i&
END SUB



'AI-Point einf�gen
SUB INSERTAIPOINT(x&, y&)

  IF nshops& = %MAXSHOPS OR x& >= yMapFactor& OR y& >= yMapFactor& THEN EXIT SUB

  POKE$ VARPTR(shops(nshops&)), STRING$(SIZEOF(TShop), 0)
  shops(nshops&).unittype = 2
  shops(nshops&).position = x&+y&*yMapFactor&
  nshops& = nshops&+1
END SUB



'Aktion einf�gen
SUB INSERTACTION(btn&)
  LOCAL a$, category&, turnnr&, movenr&, pl&, tp&, v&, w&, tmp&, newally&, level&, scrolledToBottom&

  'Eingabeformular auslesen
  turnnr& = listboxTurn.SelectedItem
  movenr& = listboxMovement.SelectedItem
  category& = listboxAction.SelectedItem
  v& = listboxParam1.SelectedItem
  w& = listboxParam2.SelectedItem
  pl& = listboxPlayer.SelectedItem
  IF (category& < 2 OR category& > 3) AND category& <> 8 THEN pl& = 2^pl&
  IF bi2020support& <> 0 THEN level& = listboxLevel.SelectedItem

  SELECT CASE category&
  CASE 0:  'Wetter
    category& = 0
    tp& = 2

  CASE 1:  'Nachricht
    category& = 0
    tp& = 3

  CASE 2, 3:  'Sieg / Niederlage
    category& = IIF&(category& = 2, 48, 80)
    tp& = v&+8
    SELECT CASE v&
    CASE 0:  'Spieler besiegt
      v& = pl&
      w& = 0
    CASE 1:  'Shop erobert
      v& = pl&
      w& = FINDSHOPBYNAME&(RTRIM$(listboxParam2.ItemValue(w&)))
    CASE 2:  'Runde erreicht
      v& = w&+1
      w& = 0
    CASE 3 TO 8:  'Einheit tot
      tmp& = v&
      v& = w&
      w& = tmp&-3
      tp& = 12
    CASE 9:  'Team kontrolliert alle HQs
      tp& = 26
    END SELECT
    pl& = 0

  CASE 4:  'DF-Layer
    category& = 0
    tp& = 4

  CASE 5:  'Allianz
    category& = 0
    tp& = 7
    newally& = w&+1
    w& = 0

  CASE 6:  'BI3 Video-Nachricht
    a$ = listboxParam1.ItemValue(v&)
    IF LEFT$(a$, 5) = "VIDEO" THEN
      v& = VAL(MID$(a$, 6))+10000
    ELSE
      v& = VAL(MID$(a$, 6))+10399
    END IF
    category& = 0
    tp& = 3

  CASE 7:  'benutzerdefinierte Nachricht
    category& = 0
    tp& = 20

  CASE 8:  'Siegbedingung f�r Bonusmission
    category& = 48
    SELECT CASE v&
    CASE 0:  'Shop erobert
      v& = pl&
      w& = FINDSHOPBYNAME&(RTRIM$(listboxParam2.ItemValue(w&)))
      tp& = 21
    CASE 1:  'Runde noch nicht erreicht
      v& = w&+1
      w& = 0
      tp& = 22
    CASE 2:  'keine Verluste
      v& = pl&
      tp& = 23
    CASE 3:  'Einheit �berlebt
      v& = w&
      tp& = 24
    END SELECT
    pl& = 0

  CASE 9:  'DF-Layer (leichte Schwierigkeit)
    category& = 0
    tp& = 17

  CASE 10:  'DF-Layer (schwere Schwierigkeit)
    category& = 0
    tp& = 18

  CASE 11:  'DF-Layer (schwere Schwierigkeit)
    category& = 0
    tp& = 19

  CASE 12:  'lokales Wetter
    category& = 0
    tp& = 27
    tmp& = listboxParam3.SelectedItem
    w& = MIN&(w&, tmp&)+MAX(w&, tmp&)*256

  END SELECT

  'Aktion erzeugen
  a$ = STRING$(40, 0)
  ASC(a$, 1) = category&+level&   'category
  MID$(a$, 3, 2) = MKI$(turnnr&)  'turn
  MID$(a$, 5, 2) = MKI$(movenr&)  'movement
  ASC(a$, 7) = pl&                'player
  ASC(a$, 8) = tp&                'actiontype
  MID$(a$, 13, 2) = MKI$(v&)      'actionparam
  MID$(a$, 15, 2) = MKWRD$(w&)    'shop
  ASC(a$, 17) = newally&          'newally
  actions$ = actions$+a$

  CALL SORTACTIONS
  mapChanged& = -1
  actionCount& = LEN(actions$)/40
  scrolledToBottom& = (scrollbarActions.ScrollPosition = scrollbarActions.MaxScroll)
  scrollbarActions.MaxScroll = actionCount& - actionsPerPage&
  IF scrollbarActions.MaxScroll > 0 AND scrolledToBottom& <> 0 THEN scrollbarActions.ScrollPosition = scrollbarActions.ScrollPosition+1
  IF actionCount& >= %MAXACTIONS THEN buttonNewAction.Enabled = 0
END SUB



'Aktion l�schen
SUB DELETEACTION(btn&)
  LOCAL actionNr&

  actionNr& = btn& - buttonDeleteAction(0).ID + scrollbarActions.ScrollPosition
  actions$ = LEFT$(actions$, actionNr&*40)+MID$(actions$, actionNr&*40+41)
  actionCount& = LEN(actions$)/40
  scrollbarActions.MaxScroll = actionCount& - actionsPerPage&
  IF scrollbarActions.ScrollPosition > scrollbarActions.MaxScroll THEN scrollbarActions.ScrollPosition = scrollbarActions.MaxScroll
  buttonNewAction.Enabled = 1
  mapChanged& = -1
END SUB



'Listbox Eintrag ausgew�hlt
SUB LISTBOXITEMCHANGED(lbx&)

  SELECT CASE lbx&
  CASE listboxOwner.ID, listboxShoptype.ID:  'Shop-Besitzer / Shop-Typ
    CALL UPDATESHOP

  CASE listboxAICommand.ID:  'AI-Point Befehl
    CALL UPDATESHOP

  CASE listboxAction.ID:  'Aktion
    CALL POPULATEACTIONPARAMLISTBOX(listboxAction.SelectedItem)
    CALL POPULATEACTIONPARAMLISTBOX2(listboxAction.SelectedItem, listboxParam1.SelectedItem)

  CASE listboxParam1.ID:  'erster Aktions-Parameter
    CALL POPULATEACTIONPARAMLISTBOX2(listboxAction.SelectedItem, listboxParam1.SelectedItem)

  CASE listboxWidth.ID, listboxHeight.ID:  'neue Karte
    CALL CREATEMAP(listboxWidth.SelectedItem*4+16, listboxHeight.SelectedItem*4+16, defaultTerrain&)

  CASE listboxCustomMessages.ID  'benutzerdefinierte Meldungen
    customMessages$$(0, selectedCustomMsg&) = editCustomMessageGER.Value
    customMessages$$(1, selectedCustomMsg&) = editCustomMessageENG.Value
    customMessages$$(2, selectedCustomMsg&) = editCustomMessageUserLang.Value
    selectedCustomMsg& = listboxCustomMessages.SelectedItem
    editCustomMessageGER.Value = customMessages$$(0, selectedCustomMsg&)
    editCustomMessageENG.Value = customMessages$$(1, selectedCustomMsg&)
    editCustomMessageUserLang.Value = customMessages$$(2, selectedCustomMsg&)
  END SELECT
END SUB



'Multi-Sprite einf�gen
SUB INSERTMULTISPRITE(x&, y&, mspr&)
  LOCAL i&, n&, dx&, dy&, sprnr&, lv&

  n& = LEN(multiSprites$(mspr&))/4
  FOR i& = 0 TO n&-1
    dx& = ASC(multiSprites$(mspr&), i&*4+1)
    IF dx& > 127 THEN dx& = dx&-256
    dy& = ASC(multiSprites$(mspr&), i&*4+2)
    IF dy& > 127 THEN dy& = dy&-256
    IF (x& AND 1) = 1 AND (dx& AND 1) = 1 THEN dy& = dy&+1

    sprnr& = CVI(multiSprites$(mspr&), i&*4+3)
    lv& = selectedLevel&
    IF sprnr& = 569 OR sprnr& = 579 OR sprnr& = 580 THEN lv& = 0  'die unteren drei Teile von Titan-Nets Festung sind Terrain statt Geb�ude
    IF x&+dx& >= 0 AND x&+dx& < mapwidth& AND y&+dy& >= 0 AND y&+dy& < mapheight& THEN CALL UPDATEMAP(x&+dx&, y&+dy&, lv&, sprnr&)
  NEXT i&
END SUB



'markierten Bereich der Karte mit gew�hltem Terrain f�llen
'Einheit oder Objekt unter Cursor l�schen
SUB DELETESPRITE
  LOCAL i&, j&, shopnr&

  IF mousexpos& >= maparea.left AND mousexpos& <= maparea.right AND mouseypos& >= maparea.top AND mouseypos& <= maparea.bottom THEN
    CALL GETMAPPOS((mousexpos&-maparea.left+scrollX&)/zoom#, (mouseypos&-maparea.top+scrollY&)/zoom#, i&, j&)
    IF i& >= 0 AND i& < mapwidth& AND j& >= 0 AND j& < mapheight& THEN
      IF selectedDF& = 0 THEN
        'normale Karte
        IF mapdata%(i&, j&, 2, 0) > 0 THEN
          CALL SAVEUNDO
          CALL UPDATEMAP(i&, j&, 2, -1)
        ELSE
          IF mapdata%(i&, j&, 1, 0) > 0 THEN
            CALL SAVEUNDO
            CALL UPDATEMAP(i&, j&, 1, -1)
            CALL SMOOTHMAP(i&, j&, 1)
          ELSE
            'falls sich auf diesem Feld ein Shop befindet, dann diesen l�schen
            shopnr& = FINDSHOP&(i&, j&)
            IF shopnr& >= 0 THEN CALL DELETESHOP(shopnr&)
          END IF
        END IF
      ELSE
        'DF-Layer
        IF mapdata%(i&, j&, 2, selectedDF&) > 0 THEN
          CALL SAVEUNDO
          CALL UPDATEMAP(i&, j&, 2, -1)  'DF-Layer Einheit l�schen
        ELSE
          IF mapdata%(i&, j&, 1, selectedDF&) > 0 THEN
            CALL SAVEUNDO
            CALL UPDATEMAP(i&, j&, 1, -1)  'DF-Layer Objekt l�schen
          ELSE
            IF mapdata%(i&, j&, 0, selectedDF&) <> -1 THEN
              CALL SAVEUNDO
              CALL UPDATEMAP(i&, j&, 0, -1)  'DF-Layer Terrain l�schen
            ELSE
              CALL SAVEUNDO
              'Eintrag komplett l�schen
              mapdata%(i&, j&, 0, selectedDF&) = 0
              mapdata%(i&, j&, 1, selectedDF&) = 0
              mapdata%(i&, j&, 2, selectedDF&) = 0
            END IF
          END IF
        END IF
      END IF
    END IF
  END IF
END SUB



'l�scht alle Objekte im markierten Bereich der Karte
SUB DELETESECTION
  LOCAL x0&, x1&, y0&, y1&, i&, k&, n1&, n2&, n3&, lv&

  IF mapSelection.left < 0 OR mapSelection.right < 0 THEN EXIT SUB

  'Selektionsbereich bestimmen
  x0& = MIN&(mapSelection.left, mapSelection.right)
  x1& = MAX&(mapSelection.left, mapSelection.right)
  y0& = MIN&(mapSelection.top, mapSelection.bottom)
  y1& = MAX&(mapSelection.top, mapSelection.bottom)

  'pr�fen, ob Objekte im Selektionsbereich vorhanden sind
  IF selectedDF& = 0 THEN
    FOR k& = y0& TO y1&
      FOR i& = x0& TO x1&
        IF mapdata%(i&, k&, 1, 0) > 0 THEN n1& = n1&+1
        IF mapdata%(i&, k&, 2, 0) > 0 THEN n2& = n2&+1
      NEXT i&
    NEXT k&
    lv& = IIF&(n2& = 0, 1, 2)
  ELSE
    FOR k& = y0& TO y1&
      FOR i& = x0& TO x1&
        IF mapdata%(i&, k&, 1, selectedDF&) <> -1 THEN n1& = n1&+1
        IF mapdata%(i&, k&, 2, selectedDF&) <> -1 THEN n2& = n2&+1
        IF mapdata%(i&, k&, 0, selectedDF&) <> -1 THEN n3& = n3&+1
      NEXT i&
    NEXT k&
    IF n2& > 0 THEN lv& = 2 ELSE lv& = IIF&(n1& = 0, 0, 1)
  END IF
  IF n1& = 0 AND n2& = 0 AND n3& = 0 THEN EXIT SUB

  CALL SAVEUNDO

  'Bereich l�schen
  FOR k& = y0& TO y1&
    FOR i& = x0& TO x1&
      IF selectedDF& = 0 THEN
        CALL UPDATEMAP(i&, k&, lv&, IIF&(lv& = 0, 0, -1))
      ELSE
        mapdata%(i&, k&, 0, selectedDF&) = 0
        mapdata%(i&, k&, 1, selectedDF&) = 0
        mapdata%(i&, k&, 2, selectedDF&) = 0
      END IF
    NEXT i&
  NEXT k&
  IF n2& = 0 THEN CALL SMOOTHAREA(x0&, y0&, x1&, y1&, 1)
END SUB



SUB FILLSELECTION
  LOCAL x0&, x1&, y0&, y1&, i&, k&

  IF mapSelection.left < 0 OR mapSelection.right < 0 THEN EXIT SUB
  IF selectedLevel& <> 0 OR selectedSprite& < 0 THEN EXIT SUB
  IF selectedDF& <> 0 THEN EXIT SUB

  'Selektionsbereich bestimmen
  x0& = MIN&(mapSelection.left, mapSelection.right)
  x1& = MAX&(mapSelection.left, mapSelection.right)
  y0& = MIN&(mapSelection.top, mapSelection.bottom)
  y1& = MAX&(mapSelection.top, mapSelection.bottom)
  CALL SAVEUNDO

  'Bereich f�llen
  FOR k& = y0& TO y1&
    FOR i& = x0& TO x1&
      mapdata%(i&, k&, selectedLevel&, selectedDF&) = selectedSprite&
    NEXT i&
  NEXT k&
  CALL SMOOTHAREA(x0&, y0&, x1&, y1&, selectedLevel&)
  mapChanged& = -1
END SUB



'ausgew�hlten DF-Layer l�schen
SUB CLEARDFLAYER
  LOCAL i&, k&

  IF selectedDF& = 0 THEN EXIT SUB

  CALL SAVEUNDO

  'DF-Layer l�schen
  FOR k& = 0 TO mapheight&-1
    FOR i& = 0 TO mapwidth&-1
      mapdata%(i&, k&, 0, selectedDF&) = 0
      mapdata%(i&, k&, 1, selectedDF&) = 0
      mapdata%(i&, k&, 2, selectedDF&) = 0
    NEXT i&
  NEXT k&

  mapChanged& = -1
END SUB



'Kartendata an Cursorposition lesen
FUNCTION GETMAPINFO$(mx&, my&)
  LOCAL i&, j&, a$

  IF mx& < maparea.left OR mx& > maparea.right OR my& < maparea.top OR my& > maparea.bottom THEN EXIT FUNCTION
  CALL GETMAPPOS((mx&-maparea.left+scrollX&)/zoom#, (my&-maparea.top+scrollY&)/zoom#, i&, j&)
  IF i& < 0 OR i& >= mapwidth& OR j& < 0 OR j& >= mapheight& THEN EXIT FUNCTION

  'Sprites an dieser Stelle auslesen
  a$ = FORMAT$(mapdata%(i&, j&, 0, selectedDF&))+" / "+FORMAT$(mapdata%(i&, j&, 1, selectedDF&))+" / "+FORMAT$(mapdata%(i&, j&, 2, selectedDF&))
  GETMAPINFO$ = a$
END FUNCTION



'Sprite unter Cursorposition von Karte lesen
SUB PEEKSPRITE(layer&)
  LOCAL i&, j&

  'pr�fen, ob Mauscursor sich �ber der Karte befindet
  IF mousexpos& < maparea.left OR mousexpos& > maparea.right OR mouseypos& < maparea.top OR mouseypos& > maparea.bottom THEN EXIT SUB
  CALL GETMAPPOS((mousexpos&-maparea.left+scrollX&)/zoom#, (mouseypos&-maparea.top+scrollY&)/zoom#, i&, j&)
  IF i& < 0 OR i& >= mapwidth& OR j& < 0 OR j& >= mapheight& THEN EXIT SUB

  'Sprite an dieser Stelle auslesen
  IF layer& = 0 THEN
    'normale Karte
    IF mapdata%(i&, j&, 2, 0) > 0 THEN
      selectedSprite& = mapdata%(i&, j&, 2, 0)
    ELSE
      IF mapdata%(i&, j&, 1, 0) > 0 THEN
        selectedSprite& = mapdata%(i&, j&, 1, 0)
      ELSE
        selectedSprite& = mapdata%(i&, j&, 0, 0)
      END IF
    END IF
  ELSE
    'DF-Layer
    IF mapdata%(i&, j&, 2, layer&) > 0 THEN
      selectedSprite& = mapdata%(i&, j&, 2, layer&)
    ELSE
      IF mapdata%(i&, j&, 1, layer&) > 0 THEN
        selectedSprite& = mapdata%(i&, j&, 1, layer&)
      ELSE
        IF mapdata%(i&, j&, 0, layer&) > 0 THEN
          selectedSprite& = mapdata%(i&, j&, 0, layer&)
        ELSE
          CALL PEEKSPRITE(0)
          EXIT SUB
        END IF
      END IF
    END IF
  END IF

  'Palette mit diesem Sprite aktivieren
  selectedLevel& = 0
  FOR i& = 0 TO 4
    FOR j& = 0 TO LEN(palette$(i&))/2-1
      IF CVI(palette$(i&), j&*2+1) = selectedSprite& THEN
        IF selectedTab& < 0 THEN CALL HIDEDIALOGUES(0)
        CALL SelectTab(i&)
        scrollPaletteToSprite& = j&+1
        IF selectedTab& <> 4 THEN CALL HIDESHOPDIALOGUE
        selectedLevel& = IIF&(selectedTab& = 0, 0, IIF&(selectedTab& = 4, 2, 1))
        EXIT SUB
      END IF
    NEXT j&
  NEXT i&
END SUB



'Karte bearbeiten
SUB UPDATEMAP(x&, y&, lv&, sprnr&)
  LOCAL shopnr&, tp&

  IF lv& > 0 AND selectedDF& = 0 THEN
    'Shop l�schen, falls sich an dieser Stelle ein Shop befand
    shopnr& = FINDSHOP&(x&, y&)
    IF shopnr& >= 0 THEN
      IF shops(shopnr&).unittype = 1 AND shops(shopnr&).nameindex > 0 THEN
        IF lv& = 1 THEN CALL DELETESHOP(shopnr&)
      ELSE
        IF lv& = 2 THEN CALL DELETESHOP(shopnr&)
      END IF
    END IF
  END IF

  IF lv& = 1 AND selectedDF& = 0 THEN
    'falls neues Sprite ein Shop ist, dann Shop an dieser Stelle einf�gen
    tp& = GETSHOPTYPEFORSPRITE&(sprnr&)
    IF tp& > 0 THEN CALL INSERTSHOP(x&, y&, tp&)
  END IF

  IF selectedDF& = 0 OR x& < yMapFactor& OR y& < yMapFactor& THEN
    mapdata%(x&, y&, lv&, selectedDF&) = sprnr&  'DF-Layer sind nur in den oberen linken 64x64 Feldern g�ltig
    IF selectedDF& > 0 THEN
      IF lv& <> 0 AND mapdata%(x&, y&, 0, selectedDF&) = 0 THEN mapdata%(x&, y&, 0, selectedDF&) = -1
      IF lv& <> 1 AND mapdata%(x&, y&, 1, selectedDF&) = 0 THEN mapdata%(x&, y&, 1, selectedDF&) = -1
      IF lv& <> 2 AND mapdata%(x&, y&, 2, selectedDF&) = 0 THEN mapdata%(x&, y&, 2, selectedDF&) = -1
    END IF
  END IF
  mapChanged& = -1
END SUB



'Bef�llt die "neighbor" Arrays mit den Koordinaten der benachbarten Felder (im Uhrzeigersinn beginnened im Norden)
SUB GETADJACENTFIELDS(BYVAL x&, BYVAL y&, xoff&(), yoff&())
  LOCAL i&
  REDIM xoff&(6), yoff&(6)

  xoff&(0) = x&
  yoff&(0) = y&-1
  xoff&(3) = x&
  yoff&(3) = y&+1
  IF (x& AND 1) = 0 THEN
    xoff&(1) = x&+1
    yoff&(1) = y&-1
    xoff&(2) = x&+1
    yoff&(2) = y&
    xoff&(4) = x&-1
    yoff&(4) = y&
    xoff&(5) = x&-1
    yoff&(5) = y&-1
  ELSE
    xoff&(1) = x&+1
    yoff&(1) = y&
    xoff&(2) = x&+1
    yoff&(2) = y&+1
    xoff&(4) = x&-1
    yoff&(4) = y&+1
    xoff&(5) = x&-1
    yoff&(5) = y&
  END IF

  'Koordinaten au�erhalb der Karte auf -1 setzen
  FOR i& = 0 TO 5
    IF xoff&(i&) < 0 OR xoff&(i&) >= mapwidth& OR yoff&(i&) < 0 OR yoff&(i&) >= mapheight& THEN xoff&(i&) = -1
  NEXT i&
END SUB



'Ein Feld der Karte weichzeichnen
SUB SMOOTHMAP(BYVAL x&, BYVAL y&, level&)
  LOCAL i&, j&, m&, spr&, spr2&
  LOCAL xoff&(), yoff&(), xFieldsToSmooth&(), yFieldsToSmooth&()

  'alle Felder ermitteln, die weichgezeichnet werden m�ssen
  CALL GETADJACENTFIELDS(x&, y&, xFieldsToSmooth&(), yFieldsToSmooth&())
  xFieldsToSmooth&(6) = x&
  yFieldsToSmooth&(6) = y&

  'Felder weichzeichnen
  FOR i& = 0 TO 6
    x& = xFieldsToSmooth&(i&)
    y& = yFieldsToSmooth&(i&)
    IF x& >= 0 THEN
      m& = 0
      spr& = mapdata%(x&, y&, level&, 0)
      SELECT CASE spr&
      CASE 95 TO 109:  'Wasser
        CALL GETADJACENTFIELDS(x&, y&, xoff&(), yoff&())
        FOR j& = 0 TO 5
          IF xoff&(j&) < 0 THEN
            spr2& = spr&
          ELSE
            spr2& = mapdata%(xoff&(j&), yoff&(j&), level&, 0)
          END IF
          SELECT CASE spr2&
          CASE 15 TO 94, 124 TO 9999: m& = m& OR 2^j&
          END SELECT
        NEXT j&

        SELECT CASE m&
        CASE 0,9,18,19,21,22,23,26,27,29,30,31,36 TO 39,42 TO 47,50 TO 55,58 TO 63: spr& = IIF&(spr& = 96 OR spr& = 97, spr&, 95)
        CASE 1: spr& = 103
        CASE 2,3: spr& = 101
        CASE 4,12: spr& = 98
        CASE 5,6,7,10,11,13,14,15: spr& = 106
        CASE 8: spr& = 102
        CASE 16,24: spr& = 99
        CASE 17,25,40,41,48,49,56,57: spr& = 107
        CASE 20,28: spr& = 104
        CASE 32,33:spr& = 100
        CASE 34,35: spr& = 105
        END SELECT
        mapdata%(x&, y&, level&, 0) = spr&

      CASE 373 TO 382, 385 TO 387:  'Wasser (Dschungel)
        CALL GETADJACENTFIELDS(x&, y&, xoff&(), yoff&())
        FOR j& = 0 TO 5
          IF xoff&(j&) < 0 THEN
            spr2& = spr&
          ELSE
            spr2& = mapdata%(xoff&(j&), yoff&(j&), level&, 0)
          END IF
          SELECT CASE spr2&
          CASE 0 TO 372, 383, 384, 388 TO 462, 467 TO 9999: m& = m& OR 2^j&
          END SELECT
        NEXT j&

        SELECT CASE m&
        CASE 0,9,18,19,21,22,23,26,27,29,30,31,36 TO 39,42 TO 47,50 TO 55,58 TO 63: spr& = IIF&(spr& = 386 OR spr& = 387, spr&, 385)
        CASE 1: spr& = 378
        CASE 2,3: spr& = 376
        CASE 4,12: spr& = 373
        CASE 5,6,7,10,11,13,14,15: spr& = 381
        CASE 8: spr& = 377
        CASE 16,24: spr& = 374
        CASE 17,25,40,41,48,49,56,57: spr& = 382
        CASE 20,28: spr& = 379
        CASE 32,33:spr& = 375
        CASE 34,35: spr& = 380
        END SELECT
        mapdata%(x&, y&, level&, 0) = spr&

      CASE 243 TO 267, 269:  'Wald
        CALL GETADJACENTFIELDS(x&, y&, xoff&(), yoff&())
        FOR j& = 0 TO 5
          IF xoff&(j&) < 0 THEN
            spr2& = spr&
          ELSE
            spr2& = mapdata%(xoff&(j&), yoff&(j&), level&, 0)
          END IF
          IF spr2& <> 269 AND (spr2& < 243 OR spr2& > 267) THEN m& = m& OR 2^j&
        NEXT j&

        SELECT CASE m&
        CASE 0: spr& = 243
        CASE 1: spr& = 262
        CASE 2: spr& = 261
        CASE 3: spr& = 245
        CASE 4: spr& = 259
        CASE 5: spr& = 255  'ist am dichtesten dran
        CASE 6: spr& = 252
        CASE 7: spr& = 255
        CASE 8: spr& = 263
        CASE 9: spr& = 263  'ist am dichtesten dran
        CASE 10: spr& = 258  'ist am dichtesten dran
        CASE 11: spr& = 250  'ist am dichtesten dran
        CASE 12: spr& = 247
        CASE 13: spr& = 250  'ist am dichtesten dran
        CASE 14: spr& = 257
        CASE 15: spr& = 250
        CASE 16: spr& = 258
        CASE 17: spr& = 254  'ist am dichtesten dran
        CASE 18: spr& = 246  'ist am dichtesten dran
        CASE 19: spr& = 266  'ist am dichtesten dran
        CASE 20: spr& = 249  'ist am dichtesten dran
        CASE 21: spr& = 266  'ist am dichtesten dran
        CASE 22: spr& = 266  'ist am dichtesten dran
        CASE 23: spr& = 266  'ist am dichtesten dran
        CASE 24: spr& = 246
        CASE 25: spr& = 251  'ist am dichtesten dran
        CASE 26: spr& = 266  'ist am dichtesten dran
        CASE 27: spr& = 266  'ist am dichtesten dran
        CASE 28: spr& = 249
        CASE 29: spr& = 249  'ist am dichtesten dran
        CASE 30: spr& = 266  'ist am dichtesten dran
        CASE 31: spr& = 266  'ist am dichtesten dran
        CASE 32: spr& = 260
        CASE 33: spr& = 244
        CASE 34: spr& = 248  'ist am dichtesten dran
        CASE 35: spr& = 248
        CASE 36: spr& = 269  'ist am dichtesten dran
        CASE 37: spr& = 269  'ist am dichtesten dran
        CASE 38: spr& = 252  'ist am dichtesten dran
        CASE 39: spr& = 266  'ist am dichtesten dran
        CASE 40: spr& = 256  'ist am dichtesten dran
        CASE 41: spr& = 251  'ist am dichtesten dran
        CASE 42: spr& = 266  'ist am dichtesten dran
        CASE 43: spr& = 248  'ist am dichtesten dran
        CASE 44: spr& = 269  'ist am dichtesten dran
        CASE 45: spr& = 269
        CASE 46: spr& = 257  'ist am dichtesten dran
        CASE 47: spr& = 266  'ist am dichtesten dran
        CASE 48: spr& = 253
        CASE 49: spr& = 254
        CASE 50: spr& = 264  'ist am dichtesten dran
        CASE 51: spr& = 264  'ist am dichtesten dran
        CASE 52: spr& = 265  'ist am dichtesten dran
        CASE 53: spr& = 266  'ist am dichtesten dran
        CASE 54: spr& = 265  'ist am dichtesten dran
        CASE 55: spr& = 264
        CASE 56: spr& = 256
        CASE 57: spr& = 251
        CASE 58: spr& = 266  'ist am dichtesten dran
        CASE 59: spr& = 266  'ist am dichtesten dran
        CASE 60: spr& = 265  'ist am dichtesten dran
        CASE 61: spr& = 266  'ist am dichtesten dran
        CASE 62: spr& = 265
        CASE 63: spr& = 266
        END SELECT
        mapdata%(x&, y&, level&, 0) = spr&

      CASE 389 TO 391, 397 TO 406:  'Dschungel
        CALL GETADJACENTFIELDS(x&, y&, xoff&(), yoff&())
        FOR j& = 0 TO 5
          IF xoff&(j&) < 0 THEN
            spr2& = spr&
          ELSE
            spr2& = mapdata%(xoff&(j&), yoff&(j&), level&, 0)
          END IF
          IF spr2& < 389 OR spr2& > 406 THEN m& = m& OR 2^j&
          IF spr2& > 391 AND spr2& < 397 AND spr2& <> 394 THEN m& = m& OR 2^j&
        NEXT j&

        SELECT CASE m&
        CASE 33, 49, 51: spr& = 397
        CASE 3, 7, 39: spr& = 398
        CASE 12, 14, 30: spr& = 399
        CASE 24, 56, 60: spr& = 400
        CASE 35: spr& = 401
        CASE 28: spr& = 402
        CASE 1: spr& = 403
        CASE 8: spr& = 404
        CASE 48: spr& = 405
        CASE 6: spr& = 406
        CASE ELSE:
          IF mapdata%(x&, y&, level&, 0) >= 389 AND mapdata%(x&, y&, level&, 0) <= 391 THEN
            spr& = mapdata%(x&, y&, level&, 0)
          ELSE
            spr& = 389
          END IF
        END SELECT
        mapdata%(x&, y&, level&, 0) = spr&

      CASE 315 TO 324:  'Weizen
        CALL GETADJACENTFIELDS(x&, y&, xoff&(), yoff&())
        FOR j& = 0 TO 5
          IF xoff&(j&) < 0 THEN
            spr2& = spr&
          ELSE
            spr2& = mapdata%(xoff&(j&), yoff&(j&), level&, 0)
          END IF
          IF spr2& < 315 OR spr2& > 324 THEN m& = m& OR 2^j&
        NEXT j&

        SELECT CASE m&
        CASE 0: spr& = 315
        CASE 1,35: spr& = 322
        CASE 3,39: spr& = 323
        CASE 6: spr& = 320
        CASE 8,28: spr& = 316
        CASE 12,14,30: spr& = 317
        CASE 33,51: spr& = 321
        CASE 48: spr& = 319
        CASE 60: spr& = 318
        CASE 54,55,62,63: spr& = 324
        CASE ELSE: spr& = 324
        END SELECT
        mapdata%(x&, y&, level&, 0) = spr&

      END SELECT
    END IF
  NEXT i&
END SUB



'Zeichnet einen Bereich der Karte weich
SUB SMOOTHAREA(BYVAL x0&, BYVAL y0&, BYVAL x1&, BYVAL y1&, level&)
  LOCAL i&

  'obere und untere Kante weichzeichnen
  FOR i& = x0& TO x1&
    CALL SMOOTHMAP(i&, y0&, level&)
    CALL SMOOTHMAP(i&, y1&, level&)
  NEXT i&

  'linke und rechte Kante weichzeichnen
  FOR i& = y0&+1 TO y1&-1
    CALL SMOOTHMAP(x0&, i&, level&)
    CALL SMOOTHMAP(x1&, i&, level&)
  NEXT i&
END SUB



'Ausgew�hltes Sprite hervorheben
SUB HIGHLIGHTSELECTEDSPRITE(x&, y&, sprsize&)
  D2D.GraphicLine(x&, y&+sprsize&/2, x&+sprsize&/3, y&+sprsize&, brushWhite&, 2)
  D2D.GraphicLine(x&+sprsize&/3, y&+sprsize&, x&+sprsize&*2/3, y&+sprsize&, brushWhite&, 2)
  D2D.GraphicLine(x&+sprsize&*2/3, y&+sprsize&, x&+sprsize&, y&+sprsize&/2, brushWhite&, 2)
  D2D.GraphicLine(x&+sprsize&, y&+sprsize&/2, x&+sprsize&*2/3, y&, brushWhite&, 2)
  D2D.GraphicLine(x&+sprsize&*2/3, y&, x&+sprsize&/3, y&, brushWhite&, 2)
  D2D.GraphicLine(x&+sprsize&/3, y&, x&, y&+sprsize&/2, brushWhite&, 2)
END SUB



'Mission testen
SUB LAUNCHMAP
  LOCAL r&, f$, gamepath$
  LOCAL gameapp&
  LOCAL fname AS ASCIIZ*512, fargs AS ASCIIZ*512, directory AS ASCIIZ*512

  IF mapwidth& = 0 THEN EXIT SUB

  'ermitteln, mit welchem Programm die Mission getestet werden soll
  IF isBI3& <> 0 THEN
    gameapp& = 3
  ELSE
    IF bi2020support& <> 0 AND bi2020folder$ <> "" OR ISFILE(bi2020folder$+"bi2020.exe") <> 0 THEN gameapp& = 2020 ELSE gameapp& = 2
  END IF
  IF gameapp& < 2020 AND (misFolder$ = "" OR ISFOLDER(misFolder$) = 0) THEN
    r& = MessageBox(hWIN&, GETWORD$(%WORDSTART_ERROR+1), GETWORD$(%WORDSTART_ERROR+0), %MB_OK OR %MB_ICONERROR)
    EXIT SUB
  END IF

  SELECT CASE gameapp&
  CASE 2:
    'Mission als AMPORGE speichern
    f$ = missionFileName$
    CALL SAVEMISSION(misFolder$+"MISS000.DAT")
    missionFileName$ = f$
    f$ = MID$(f$, INSTR(-1, f$, "\")+1)
    CALL SETCAPTION(f$)

    'Battle Isle II starten
    r& = SHELL(EXEPATH$+"DOSBox\DOSBox.exe "+EXEPATH$+"BI2\BI2.BAT -conf "+EXEPATH$+"DOSBox\bi2ed.conf -noconsole -exit")

  CASE 3:
    'Mission als MISS100 speichern
    f$ = missionFileName$
    CALL SAVEMISSION(misFolder$+"MISS100.DAT")
    missionFileName$ = f$
    f$ = MID$(f$, INSTR(-1, f$, "\")+1)
    CALL SETCAPTION(f$)

    'Battle Isle III starten
    gamepath$ = GetFolderLocation$(EXEPATH$, "SDI")
    IF gamepath$ <> "" THEN
      r& = SHELL(gamepath$+"SDI_1R.EXE")
    END IF

  CASE 2020:
    'Mission speichern falls noch nicht geschehen
    IF missionFileName$ = "" OR mapChanged& <> 0 THEN
      f$ = SAVEDIALOG$
      IF f$ = "" THEN EXIT SUB
      CALL SAVEMISSION(f$)
    END IF

    'Battle Isle 2020 starten
    fname = bi2020folder$+"bi2020.exe"
    f$ = missionFileName$
    REPLACE " " WITH "+" IN f$
    fargs = "/TESTMAP "+f$
    directory = bi2020folder$
    r& = ShellExecute(BYVAL %NULL, "open", fname, fargs, directory, %SW_SHOW)
  END SELECT
END SUB



'Stra�e/Weg/Graben/Schiene weich zeichnen
FUNCTION SMOOTHROAD&(sprnr&, x&, y&)
  LOCAL i&, m&, d&, spr&, buildings$
  LOCAL xFieldsToSmooth&(), yFieldsToSmooth&()

  'alle angrenzenden Felder ermitteln
  CALL GETADJACENTFIELDS(x&, y&, xFieldsToSmooth&(), yFieldsToSmooth&())

  'angrenzende identische Sprites oder Geb�ude suchen
  buildings$ = STR16TO32$(palette$(2))
  SELECT CASE sprnr&
  CASE 90: buildings$ = buildings$+MKL$(1264)+MKL$(1265)+MKL$(1266)+MKL$(1270)+MKL$(1271)+MKL$(1272)+MKL$(1274)
  CASE 91: buildings$ = buildings$+MKL$(1265)+MKL$(1267)+MKL$(1269)+MKL$(1270)+MKL$(1272)+MKL$(1273)+MKL$(1274)
  CASE 92: buildings$ = buildings$+MKL$(1264)+MKL$(1267)+MKL$(1268)+MKL$(1270)+MKL$(1271)+MKL$(1273)+MKL$(1274)
  CASE 93: buildings$ = buildings$+MKL$(1266)+MKL$(1268)+MKL$(1269)+MKL$(1271)+MKL$(1272)+MKL$(1273)+MKL$(1274)
  END SELECT
  d& = 1
  FOR i& = 0 TO 5
    IF xFieldsToSmooth&(i&) >= 0 THEN
      spr& = mapdata%(xFieldsToSmooth&(i&), yFieldsToSmooth&(i&), 1, 0)
      IF spr& = sprnr& OR INSTR(buildings$, MKL$(spr&)) > 0 THEN m& = m& OR d&
    END IF
    d& = d&+d&
  NEXT i&

  'Editor-Sprite in Game-Sprite konvertieren
  SELECT CASE sprnr&
  CASE 90: SMOOTHROAD& = m&+70000
  CASE 91: SMOOTHROAD& = 3*%MAXROADSPRITES+m&+70000
  CASE 92: SMOOTHROAD& = %MAXROADSPRITES+m&+70000
  CASE 93: SMOOTHROAD& = 2*%MAXROADSPRITES+m&+70000
  END SELECT
END FUNCTION



'Sprite darstellen
SUB DRAWSPRITE(sprnr&, mapx&, mapy&)
  LOCAL x0&, y0&, x1&, y1&, srcx0&, srcy0&, srcx1&, srcy1&

  'Zielbereich berechnen
  x0& = maparea.left+(mapx&*16)*zoom#-scrollX&
  y0& = maparea.top+(mapy&*24+(mapx& AND 1)*12)*zoom#-scrollY&
  x1& = maparea.left+(mapx&*16+24)*zoom#-scrollX&
  y1& = maparea.top+(mapy&*24+(mapx& AND 1)*12+24)*zoom#-scrollY&
  IF sprnr& >= 65536 AND sprnr& < 65536+12 THEN
    x0& = x0&-8*zoom#
    x1& = x1&-8*zoom#
  END IF
  IF x1& < maparea.left OR x0& > maparea.right OR y1& < maparea.top OR y0& > maparea.bottom THEN EXIT SUB

  'Quellbereich berechnen
  srcx0& = 0
  srcy0& = 0
  srcx1& = 24
  srcy1& = 24
  IF x0& < maparea.left THEN
    srcx0& = INT((maparea.left-x0&)/zoom#)
    x0& = maparea.left
  END IF
  IF x1& > maparea.right THEN
    srcx1& = 24-INT((x1&-maparea.right)/zoom#)
    x1& = maparea.right
  END IF
  IF y0& < maparea.top THEN
    srcy0& = INT((maparea.top-y0&)/zoom#)
    y0& = maparea.top
  END IF
  IF y1& > maparea.bottom THEN
    srcy1& = 24-INT((y1&-maparea.bottom)/zoom#)
    y1& = maparea.bottom
  END IF

  D2D.GraphicStretch(GETSPRITEHANDLE&(sprnr&), srcx0&, srcy0&, srcx1&, srcy1&, x0&, y0&, x1&, y1&)
END SUB



'Karte darstellen
SUB RENDERMAP
  LOCAL wd&, hg&, i&, j&, k&, sprnr&, shopnr&, pl&, ac&, sh$

  IF mapwidth& = 0 THEN EXIT SUB
  sh$ = STR16TO32$(palette$(2))

  'Bitmap erstellen
  wd& = mapwidth&*16+8
  hg& = mapheight&*24+12

  'Kartenelemente rendern
  FOR k& = 0 TO 2
    FOR j& = 0 TO mapheight&-1
      FOR i& = 0 TO mapwidth&-1
        shopnr& = -1
        sprnr& = mapdata%(i&, j&, k&, 0)
        IF selectedDF& > 0 AND mapdata%(i&, j&, k&, selectedDF&) <> 0 THEN
          IF mapdata%(i&, j&, k&, selectedDF&) <> -1 OR k& > 0 THEN sprnr& = mapdata%(i&, j&, k&, selectedDF&)
        END IF
        IF k& = 1 AND INSTR(sh$, MKL$(sprnr&)) > 0 THEN
          'Shop in Besitzerfarbe darstellen
          shopnr& = FINDSHOP&(i&, j&)
          IF shopnr& >= 0 THEN sprnr& = 1437+sprnr&+playerUnitOffset&(LOG2(shops(shopnr&).owner))
        END IF
        IF k& = 1 AND sprnr& >= 90 AND sprnr& <= 93 THEN sprnr& = SMOOTHROAD&(sprnr&, i&, j&)
        IF k& = 1 AND sprnr& >= 1264 AND sprnr& <= 1274 THEN
          SELECT CASE sprnr&
          CASE 1266, 1268, 1269, 1271, 1272, 1273, 1274: CALL DRAWSPRITE(SMOOTHROAD&(93, i&, j&), i&, j&)  'Graben ganz nach unten
          END SELECT
          SELECT CASE sprnr&
          CASE 1264, 1267, 1268, 1270, 1271, 1273, 1274: CALL DRAWSPRITE(SMOOTHROAD&(92, i&, j&), i&, j&)  'Weg �ber Graben zeichnen
          END SELECT
          SELECT CASE sprnr&
          CASE 1264, 1265, 1266, 1270, 1271, 1272, 1274: CALL DRAWSPRITE(SMOOTHROAD&(90, i&, j&), i&, j&)  'Stra�e �ber Weg zeichnen
          END SELECT
          SELECT CASE sprnr&
          CASE 1265, 1267, 1269, 1270, 1272, 1273, 1274: CALL DRAWSPRITE(SMOOTHROAD&(91, i&, j&), i&, j&)  'Stra�e �ber Weg zeichnen
          END SELECT
          sprnr& = 0
        END IF
        IF sprnr& > 0 OR k& = 0 THEN CALL DRAWSPRITE(sprnr&, i&, j&)

        'DF-Layer l�scht Objekt/Einheit im normalen Layer
        IF selectedDF& > 0 AND k& > 0 AND mapdata%(i&, j&, k&, selectedDF&) = -1 THEN CALL DRAWSPRITE(%DFDELETEOBJECT+k&-1, i&, j&)
      NEXT i&
    NEXT j&
  NEXT k&

  'Shops hervorheben und/oder als Siegbedingung anzeigen
  FOR shopnr& = 0 TO nshops&-1
    i& = shops(shopnr&).position AND (yMapFactor&-1)
    j& = INT(shops(shopnr&).position/yMapFactor&)

    IF highlightShops& = 1 THEN CALL DRAWSPRITE(%SHOPHIGHLIGHT, i&, j&)

    ac& = FINDACTION&(48, 9, shopnr&)
    IF ac& < 0 AND bi2020support& <> 0 THEN ac& = FINDACTION&(48+listboxLevel.SelectedItem, 9, shopnr&)
    IF ac& >= 0 THEN
      pl& = ASC(actions$, ac&*40+13)
      'Fahne zeichnen
      CALL DRAWSPRITE(65536+pl&, i&, j&)
    END IF
    ac& = FINDACTION&(80, 9, shopnr&)
    IF ac& < 0 AND bi2020support& <> 0 THEN ac& = FINDACTION&(80+listboxLevel.SelectedItem, 9, shopnr&)
    IF ac& >= 0 THEN
      pl& = ASC(actions$, ac&*40+13)
      'auf dem Kopf stehende Fahne zeichnen
      CALL DRAWSPRITE(65536+6+pl&, i&, j&)
    END IF
  NEXT shopnr&

  'Selektion anzeigen
  IF mapSelection.left >= 0 THEN
    FOR j& = MIN&(mapSelection.top, mapSelection.bottom) TO MAX&(mapSelection.top, mapSelection.bottom)
      FOR i& = MIN&(mapSelection.left, mapSelection.right) TO MAX&(mapSelection.left, mapSelection.right)
        CALL DRAWSPRITE(%SELECTIONHIGHLIGHT, i&, j&)
      NEXT i&
    NEXT j&
  END IF

  'Kartenfehler anzeigen
  IF showMapError& >= 0 THEN CALL DRAWSPRITE(%ERRORHIGHLIGHT, validatex&(showMapError&), validatey&(showMapError&))
END SUB



'Shop darstellen
SUB DRAWSHOP(shopnr&)
  LOCAL hBmp&, i&, j&, n&, shopx&, shopy&, wd&, hg&, cl&, sprnr&, d$

  'Rahmen
  D2D.GraphicBox(shoparea.left-1, shoparea.top-1, shoparea.right+1, shoparea.bottom+1, brushBorder&, brushBackground&)
  D2D.GraphicBox(shoparea.left, shoparea.top, shoparea.right, shoparea.top+30, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(shoparea.left-1, shoparea.top+30, shoparea.right+1, shoparea.top+30, brushBorder&)

  'Name
  IF shops(shopnr&).unittype = 2 THEN
    shopnames$(shopnr&) = "AI Point "+FORMAT$(shopnr&+1)
    '03+04 = Position der Einheit
    '73 = 1..2
    '75 = 6..10
  ELSE
    shopx& = shops(shopnr&).position AND (yMapFactor&-1)
    shopy& = INT(shops(shopnr&).position/yMapFactor&)
    IF shopx& >= 0 AND shopx& < mapwidth& AND shopy& >= 0 AND shopy& < mapheight& THEN
      sprnr& = SPRITETOUNIT&(mapdata%(shopx&, shopy&, 2, 0))
      IF sprnr& >= 0 AND sprnr& < 64 THEN shopnames$(shopnr&) = unitnames$(sprnr&)+" "+FORMAT$(shopnr&+1)
    END IF
  END IF

  'AI-Point darstellen
  IF shops(shopnr&).unittype = 2 THEN
    CALL DRAWAIPOINT(shopnr&)
    EXIT SUB
  END IF

  'Energie und Material
  IF shops(shopnr&).nameindex > 0 THEN
    'Produktions-Slots
    FOR i& = 0 TO 3
      IF shops(shopnr&).shopfunction = 2 THEN D2D.GraphicCopy(prodslotspr&, shoparea.left+86, shoparea.top+36+i&*30)
      sprnr& = shops(shopnr&).production(i&)
      IF sprnr& > 0 THEN D2D.GraphicCopy(GETSPRITEHANDLE&(sprnr&+1337), shoparea.left+87, shoparea.top+37+i&*30)
    NEXT i&
  END IF

  'Inhalts-Slots
  FOR j& = 0 TO 3
    FOR i& = 0 TO 3
      IF shops(shopnr&).shoptype = 0 AND n& = %MAXTRANSPORTERSLOT THEN EXIT FOR
      D2D.GraphicCopy(normalslotspr&, shoparea.left+126+i&*30, shoparea.top+36+j&*30)
      sprnr& = shops(shopnr&).content(i&+j&*4)
      IF sprnr& >= 0 THEN D2D.GraphicCopy(GETSPRITEHANDLE&(sprnr&+1337), shoparea.left+127+i&*30, shoparea.top+37+j&*30)
      n& = n&+1
    NEXT i&
  NEXT j&
END SUB



'AI-Point darstellen
SUB DRAWAIPOINT(shopnr&)
  D2D.GraphicPrint("Befehl", shoparea.left+20, shoparea.top+40, brushBlack&, hBUTTONFONT&)
END SUB



'Objekt-Palette darstellen
SUB RENDERPALETTE(palnr&)
  LOCAL pal$, a$
  LOCAL n&, requiredRows&, x&, y&, p&, sprnr&, hBmp&

  IF palnr& < 0 THEN
    SELECT CASE palnr&
    CASE -1: CALL DRAWMAPPROPERTIES
    CASE -2: CALL DRAWACTIONDIALOG
    CASE -3: CALL DRAWOPTIONS
    CASE -4: CALL DRAWNEWMAPDIALOGUE
    CASE -5: CALL DRAWVALIDATION
    CASE -6: CALL DRAWMAPINFO
    END SELECT
  ELSE
    'Palette festlegen
    pal$ = palette$(palnr&)

    'Palette darstellen
    n& = LEN(pal$)/2
    paletteCols& = 16
    requiredRows& = INT((n&+paletteCols&-1)/paletteCols&)
    paletteRows& = (palettearea.bottom-palettearea.top-30)/25
    IF selectedShop& >= 0 THEN paletteRows& = paletteRows&-(shoparea.bottom-shoparea.top)/25
    IF paletteRows& < requiredRows& THEN
      paletteCols& = 15
      requiredRows& = INT((n&+paletteCols&-1)/paletteCols&)
      scrollbarTabpages(palnr&).Height = IIF&(selectedShop& >= 0, palettearea.bottom-palettearea.top-shoparea.bottom+shoparea.top, palettearea.bottom-palettearea.top)
      scrollbarTabpages(palnr&).Visible = 1
      scrollbarTabpages(palnr&).MaxScroll = requiredRows&-paletteRows&+1
      scrollbarTabpages(palnr&).VisibleRows = paletteRows&
      IF scrollPaletteToSprite& > 0 THEN
        p& = (scrollPaletteToSprite&-1)/paletteCols&
        IF p& < scrollbarTabpages(palnr&).ScrollPosition OR p& >= scrollbarTabpages(palnr&).ScrollPosition+paletteRows& THEN scrollbarTabpages(palnr&).ScrollPosition = p&
        scrollPaletteToSprite& = 0
      END IF
    ELSE
      scrollbarTabpages(palnr&).Visible = 0
    END IF

    IF scrollbarTabpages(palnr&).Visible THEN p& = scrollbarTabpages(palnr&).ScrollPosition*paletteCols&
    FOR y& = 0 TO paletteRows&-1
      FOR x& = 0 TO paletteCols&-1
        sprnr& = CVI(pal$, p&*2+1)
        D2D.GraphicCopy(GETSPRITEHANDLE&(sprnr&), palettearea.left+x&*25, palettearea.top+1+y&*25)
        IF sprnr& = selectedSprite& THEN CALL HIGHLIGHTSELECTEDSPRITE(palettearea.left+x&*25, palettearea.top+1+y&*25, 24)
        p& = p&+1
        IF p& = n& THEN EXIT, EXIT
      NEXT i&
    NEXT y&
  END IF

  'Shop anzeigen, falls ein Shop angew�hlt war
  IF selectedShop& >= 0 THEN
    CALL DRAWSHOP(selectedShop&)
  ELSE
    editShopname.Visible = 0
    editEnergy.Visible = 0
    editMaterial.Visible = 0
    editEnergyPlus.Visible = 0
    editMaterialPlus.Visible = 0
    listboxOwner.Visible = 0
    listboxShoptype.Visible = 0
    listboxAICommand.Visible = 0

    'Mausposition anzeigen
    CALL GETMAPPOS((mousexpos&-maparea.left+scrollX&)/zoom#, (mouseypos&-maparea.top+scrollY&)/zoom#, x&, y&)
    IF x& >= 0 AND x& < mapwidth& AND y& >= 0 AND y& < mapheight& AND IsInRect&(mousexpos&, mouseypos&, maparea) <> 0 THEN
      a$ = GETWORD$(%WORDSTART_SPRITEINFO+4)+" "+FORMAT$(x&)+","+FORMAT$(y&)
      D2D.GraphicTextSize(a$, hBUTTONFONT&, x&, y&)
      D2D.GraphicPrint(a$, (palettearea.left+palettearea.right-x&)/2, palettearea.bottom-20, brushBlack&, hBUTTONFONT&)
    END IF

    'Sprite-Informationen anzeigen
    a$ = GETWORD$(%WORDSTART_SPRITEINFO+selectedLevel&)+" "+GETWORD$(%WORDSTART_SPRITEINFO+3)+" "+FORMAT$(selectedSprite&)
    D2D.GraphicPrint(a$, palettearea.left+4, palettearea.bottom-20, brushBlack&, hBUTTONFONT&)
    a$ = GETMAPINFO$(mousexpos&, mouseypos&)
    D2D.GraphicTextSize(a$, hBUTTONFONT&, x&, y&)
    D2D.GraphicPrint(a$, palettearea.right-4-x&-IIF&(palnr& < 0 OR scrollbarTabpages(palnr&).Visible = 0, 0, 16), palettearea.bottom-20, brushBlack&, hBUTTONFONT&)
  END IF
END SUB



'Tab-Header rendern
SUB RENDERTAB(RenderTarget AS ID2D1HwndRenderTarget)
  LOCAL i&, sprnr&, cl&, btn&

  'pr�fen, ob Mauscursor �ber einem Tab ist
  btn& = -1
  FOR i& = 0 TO 4
    IF HITTEST&(mousexpos&, mouseypos&, palettearea.left+i&*80, palettearea.top-31, palettearea.left+i&*80+78, palettearea.top-2) THEN
      btn& = i&
      EXIT FOR
    END IF
  NEXT i&

  'Rahmen
  D2D.GraphicBox(palettearea.left-1, palettearea.top-32, palettearea.right+1, palettearea.bottom+1, brushBorder&, brushBackground&)
  D2D.GraphicLine(palettearea.left-1, palettearea.top-1, palettearea.right+1, palettearea.top-1, brushBorder&)
  FOR i& = 1 TO 4
    D2D.GraphicLine(palettearea.left-1+i&*80, palettearea.top-32, palettearea.left-1+i&*80, palettearea.top-1, brushBorder&)
  NEXT i&

  'Tabs
  FOR i& = 0 TO 4
    cl& = IIF&(selectedTab& = i&, brushSelectedTab&, IIF&(btn& = i&, brushHighlightedTab&, brushInactiveTab&))
    D2D.GraphicBox(palettearea.left+i&*80, palettearea.top-31, palettearea.left+79+i&*80+IIF&(i& = 4, 1, 0), palettearea.top-1, cl&, cl&)
    sprnr& = CVI(MKI$(149)+MKI$(25)+MKI$(63)+MKI$(90)+MKI$(1345), i&*2+1)
    IF apperror& = 0 THEN D2D.GraphicCopy(GETSPRITEHANDLE&(sprnr&), palettearea.left+i&*80+28, palettearea.top-29)
  NEXT i&
END SUB



'Validationsmeldungen zeigen
SUB DRAWVALIDATION
  LOCAL i&

  D2D.GraphicPrint(GETWORD$(%WORDSTART_VALIDATION+0), validationarea.left+10, validationarea.top, brushBlack&, hBIGFONT&)

  FOR i& = 0 TO nValidate&-1
    IF mousexpos& >= validationarea.left AND mousexpos& < validationarea.right AND mouseypos& >= validationarea.top+i&*20+30 AND mouseypos& < validationarea.top+i&*20+49 THEN
      D2D.GraphicBox(validationarea.left+1, validationarea.top+i&*20+30, validationarea.right-1, validationarea.top+i&*20+49, brushSelectedTab&, brushSelectedTab&)
    END IF
    D2D.GraphicPrint(validateMessages$(i&), validationarea.left+10, validationarea.top+i&*20+30, brushRed&, hBUTTONFONT&)
  NEXT i&
  IF nValidate& = 0 THEN D2D.GraphicPrint(GETWORD$(%WORDSTART_VALIDATION+1), validationarea.left+10, validationarea.top++30, brushBlack&, hBUTTONFONT&)
END SUB



'Karteninformation zeigen
SUB DRAWMAPINFO
  LOCAL i&

  D2D.GraphicPrint(GETWORD$(%WORDSTART_MAPINFO+0), validationarea.left+10, validationarea.top, brushBlack&, hBIGFONT&)

  FOR i& = 0 TO nValidate&-1
    IF mousexpos& >= validationarea.left AND mousexpos& < validationarea.right AND mouseypos& >= validationarea.top+i&*20+30 AND mouseypos& < validationarea.top+i&*20+49 THEN
      D2D.GraphicBox(validationarea.left+1, validationarea.top+i&*20+30, validationarea.right-1, validationarea.top+i&*20+49, brushSelectedTab&, brushSelectedTab&)
    END IF
    D2D.GraphicPrint(validateMessages$(i&), validationarea.left+10, validationarea.top+i&*20+30, brushGrey96&, hBUTTONFONT&)
  NEXT i&
END SUB



'Neue-Karte Dialog darstellen
SUB DRAWNEWMAPDIALOGUE
  LOCAL a$, textWidth&, textHeight&, i&, sprnr&

  'Titel
  a$ = GETWORD$(%WORDSTART_NEWMAP+0)
  D2D.GraphicBox(newmaparea.left, newmaparea.top-23, newmaparea.right, newmaparea.top-1, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(newmaparea.left-1, newmaparea.top-1, newmaparea.right+1, newmaparea.top-1, brushBorder&)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, newmaparea.left+(newmaparea.right-newmaparea.left-textWidth&)/2, newmaparea.top-21, brushBlack&, hBUTTONFONT&)

  'Breite und H�he
  a$ = GETWORD$(%WORDSTART_NEWMAP+1)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, newmaparea.left+55-textWidth&, newmaparea.top+5, brushBlack&, hBUTTONFONT&)
  a$ = GETWORD$(%WORDSTART_NEWMAP+2)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, newmaparea.left+250-textWidth&, newmaparea.top+5, brushBlack&, hBUTTONFONT&)

  'Start-Terrain
  FOR i& = 0 TO LEN(startterrain$)/2-1
    sprnr& = CVI(startterrain$, i&*2+1)
    D2D.GraphicStretch(GETSPRITEHANDLE&(sprnr&), 0, 0, 24, 24, startterrainarea.left+i&*48, startterrainarea.top, startterrainarea.left+i&*48+46, startterrainarea.top+46)
    IF sprnr& = defaultTerrain& THEN CALL HIGHLIGHTSELECTEDSPRITE(startterrainarea.left+i&*48, startterrainarea.top, 46)
  NEXT i&

  listboxWidth.Visible = 1
  listboxHeight.Visible = 1
END SUB



'Karteneigenschaften darstellen
SUB DRAWMAPPROPERTIES
  LOCAL a$, textWidth&, textHeight&
  LOCAL i&, j&, v&, cl&, n&

  'Allgemeine Produktion-Palette
  a$ = GETWORD$(%WORDSTART_PROPERTIES+0)
  D2D.GraphicBox(palettearea.left, palettearea.top, palettearea.right, palettearea.top+22, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(palettearea.left-1, palettearea.top+22, palettearea.right+1, palettearea.top+22, brushBorder&)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, palettearea.left+(palettearea.right-palettearea.left-textWidth&)/2, palettearea.top+2, brushBlack&, hBUTTONFONT&)
  '
  FOR j& = 0 TO 1
    FOR i& = 0 TO 12
      v& = gpm?(i&, j&)
      D2D.GraphicCopy(normalslotspr&, gpmarea.left+i&*30, gpmarea.top+j&*32)
      IF v& > 0 THEN D2D.GraphicCopy(GETSPRITEHANDLE&(v&-1+1337+playerUnitOffset&(j&)), gpmarea.left+i&*30+1, gpmarea.top+j&*32+1)
    NEXT i&
  NEXT j&

  'Terrain/Startwetter/Siegbedingungen
  D2D.GraphicLine(miscsettingsarea.left-1, miscsettingsarea.top-27, miscsettingsarea.right+1, miscsettingsarea.top-27, brushBorder&)
  D2D.GraphicBox(miscsettingsarea.left, miscsettingsarea.top-26, miscsettingsarea.right, miscsettingsarea.top-4, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(miscsettingsarea.left-1, miscsettingsarea.top-4, miscsettingsarea.right+1, miscsettingsarea.top-4, brushBorder&)
  a$ = GETWORD$(%WORDSTART_PROPERTIES+1)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, miscsettingsarea.left+66-textWidth&/2, miscsettingsarea.top-23, brushBlack&, hBUTTONFONT&)
  a$ = GETWORD$(%WORDSTART_PROPERTIES+2)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, miscsettingsarea.left+199-textWidth&/2, miscsettingsarea.top-23, brushBlack&, hBUTTONFONT&)
  a$ = GETWORD$(%WORDSTART_PROPERTIES+3)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, miscsettingsarea.left+332-textWidth&/2, miscsettingsarea.top-23, brushBlack&, hBUTTONFONT&)
  listboxTerrain.Visible = 1
  listboxWeather.Visible = 1
  listboxWinCond.Visible = 1
  listboxNextmap.Visible = 1
  IF bi2020support& <> 0 THEN listboxBonusmap.Visible = 1
  editMapDescription.Visible = 1
  editMapShortDescr.Visible = 1

  'Allianzen
  D2D.GraphicLine(palettearea.left-1, allyarea.top-29, palettearea.right+1, allyarea.top-29, brushBorder&)
  D2D.GraphicBox(palettearea.left, allyarea.top-28, palettearea.right, allyarea.top-6, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(palettearea.left-1, allyarea.top-6, palettearea.right+1, allyarea.top-6, brushBorder&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_PROPERTIES+4), palettearea.left+2, allyarea.top-25, brushBlack&, hBUTTONFONT&)
  '
  FOR i& = 0 TO %MAXPLAYERS-1
    IF bi2020support& <> 0 THEN
      editPlayerName(i&).Visible = 1
    ELSE
      D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(i&+1), palettearea.left+2, allyarea.top-1+i&*20, playerRGB&(i&), hBUTTONFONT&)
    END IF
    v& = ASC(allymatrix$, i&+1)
    FOR j& = 0 TO 5
      IF (v& AND (2^j&)) <> 0 THEN
        cl& = playerRGB&(j&)
      ELSE
        cl& = brushHighlightedTab&
      END IF
      D2D.GraphicBox(allyarea.left+j&*18, allyarea.top+i&*20, allyarea.left+15+j&*18, allyarea.top+15+i&*20, cl&, cl&)
    NEXT j&
  NEXT i&

  'AI Maske
  D2D.GraphicPrint("AI", aimaskarea.left, aimaskarea.top-25, brushBlack&, hBUTTONFONT&)
  '
  FOR i& = 0 TO %MAXPLAYERS-1
    D2D.GraphicBox(aimaskarea.left, aimaskarea.top+i&*20, aimaskarea.left+15, aimaskarea.top+15+i&*20, brushHighlightedTab&, brushHighlightedTab&)
    IF (aiMask& AND (2^i&)) <> 0 THEN D2D.GraphicBox(aimaskarea.left+2, aimaskarea.top+2+i&*20, aimaskarea.left+13, aimaskarea.top+13+i&*20, brushGrey96&, brushGrey96&)
  NEXT i&

  'n�chste Mission
  a$ = GETWORD$(%WORDSTART_PROPERTIES+6)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, aimaskarea.left+101-textwidth&/2, aimaskarea.top-25, brushBlack&, hBUTTONFONT&)

  'Bonusmission
  IF bi2020support& <> 0 THEN
    a$ = GETWORD$(%WORDSTART_PROPERTIES+7)
    D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
    D2D.GraphicPrint(a$, aimaskarea.left+101-textwidth&/2, aimaskarea.top+30, brushBlack&, hBUTTONFONT&)
  END IF

  'Kartenbeschreibung
  D2D.GraphicLine(mapdescriptionarea.left-1, mapdescriptionarea.top-24, mapdescriptionarea.right+1, mapdescriptionarea.top-24, brushBorder&)
  D2D.GraphicBox(mapdescriptionarea.left, mapdescriptionarea.top-23, mapdescriptionarea.right, mapdescriptionarea.top-1, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(mapdescriptionarea.left-1, mapdescriptionarea.top-1, mapdescriptionarea.right+1, mapdescriptionarea.top-1, brushBorder&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_PROPERTIES+5), mapdescriptionarea.left+2, mapdescriptionarea.top-20, brushBlack&, hBUTTONFONT&)

  'eigene Meldungen
  IF bi2020support& <> 0 THEN
    D2D.GraphicLine(custommessagesarea.left-1, custommessagesarea.top-25, custommessagesarea.right+1, custommessagesarea.top-25, brushBorder&)
    D2D.GraphicBox(custommessagesarea.left, custommessagesarea.top-24, custommessagesarea.right, custommessagesarea.top-1, brushHighlightedTab&, brushHighlightedTab&)
    D2D.GraphicLine(custommessagesarea.left-1, custommessagesarea.top-1, custommessagesarea.right+1, custommessagesarea.top-1, brushBorder&)
    D2D.GraphicPrint(GETWORD$(%WORDSTART_PROPERTIES+8), custommessagesarea.left+2, custommessagesarea.top-20, brushBlack&, hBUTTONFONT&)
    D2D.GraphicPrint(GETWORD$(%WORDSTART_PROPERTIES+9), custommessagesarea.left+2, custommessagesarea.top+4, brushBlack&, hBUTTONFONT&)
    D2D.GraphicPrint(GETWORD$(%WORDSTART_PROPERTIES+10), custommessagesarea.left+2, custommessagesarea.top+124, brushBlack&, hBUTTONFONT&)
    D2D.GraphicPrint(GETWORD$(%WORDSTART_PROPERTIES+11), custommessagesarea.left+2, custommessagesarea.top+244, brushBlack&, hBUTTONFONT&)
    a$ = GETWORD$(%WORDSTART_PROPERTIES+12)
    D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
    D2D.GraphicPrint(a$, custommessagesarea.right-50-textwidth&, custommessagesarea.top+244, brushBlack&, hBUTTONFONT&)
    listboxCustomMessages.Visible = 1
    editCustomMessageGER.Visible = 1
    editCustomMessageENG.Visible = 1
    editCustomMessageUserLang.Visible = 1
    editCustomLangCode.Visible = 1
    editCustomMessageUserLang.Enabled = LEN(TRIM$(editCustomLangCode.Value)) = 3
  END IF
END SUB



'Akionen darstellen
SUB DRAWACTIONDIALOG
  LOCAL a$
  LOCAL i&, firstVisibleAction&

  'neue Aktion
  D2D.GraphicLine(newactionarea.left-1, newactionarea.top-1, newactionarea.right+1, newactionarea.top-1, brushBorder&)
  D2D.GraphicBox(newactionarea.left, newactionarea.top-23, newactionarea.right, newactionarea.top-1, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+0), newactionarea.left+2, newactionarea.top-19, brushBlack&, hBUTTONFONT&)
  IF bi2020support& <> 0 THEN D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+19), newactionarea.left+200, newactionarea.top-19, brushBlack&, hBUTTONFONT&)
  '
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+1), newactionarea.left, newactionarea.top+4, brushBlack&, hBUTTONFONT&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+2), newactionarea.left, newactionarea.top+26, brushBlack&, hBUTTONFONT&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+3), newactionarea.left, newactionarea.top+48, brushBlack&, hBUTTONFONT&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+4), newactionarea.left+200, newactionarea.top+4, brushBlack&, hBUTTONFONT&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+5), newactionarea.left+200, newactionarea.top+26, brushBlack&, hBUTTONFONT&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+6), newactionarea.left+200, newactionarea.top+48, brushBlack&, hBUTTONFONT&)

  'Aktionen
  D2D.GraphicLine(actionarea.left-1, actionarea.top, actionarea.right+1, actionarea.top, brushBorder&)
  D2D.GraphicBox(actionarea.left, actionarea.top+1, actionarea.right, actionarea.top+23, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(actionarea.left-1, actionarea.top+23, actionarea.right+1, actionarea.top+23, brushBorder&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+7)+" ("+FORMAT$(actionCount&)+")", actionarea.left+2, actionarea.top+4, brushBlack&, hBUTTONFONT&)
  '
  firstVisibleAction& = scrollbarActions.ScrollPosition
  FOR i& = 0 TO actionsPerPage&-1
    buttonDeleteAction(i&).Visible = IIF&(firstVisibleAction&+i& < actionCount&, 1, 0)
    IF firstVisibleAction&+i& < actionCount& THEN CALL DRAWACTION(firstVisibleAction&+i&, actionarea.top+30+i&*20)
  NEXT i&
  FOR i& = actionsPerPage& TO %MAXACTIONS-1
    buttonDeleteAction(i&).Visible = 0
  NEXT i&
  buttonNewAction.Visible = 1
  listboxPlayer.Visible = 1
  listboxTurn.Visible = 1
  listboxMovement.Visible = 1
  listboxAction.Visible = 1
  listboxParam1.Visible = 1
  listboxParam2.Visible = 1
  IF listboxAction.SelectedItem = 12 THEN
    listboxParam3.Visible = 1
    listboxParam2.Width = 60
  ELSE
    listboxParam3.Visible = 0
    listboxParam2.Width = 122
  END IF
  listboxLevel.Visible = ABS(bi2020support&)
  scrollbarActions.Visible = 1
  scrollbarActions.Enabled = 1
END SUB



'Aktionsstufe aus Kategorie ermitteln
FUNCTION GETACTIONLEVEL&(category&)
  LOCAL level&

  IF category& < 48 THEN level& = category&
  IF category& > 48 AND category& < 80 THEN level& = category&-48
  IF category& > 80 THEN level& = category&-80

  GETACTIONLEVEL& = level&
END FUNCTION



'Aktion darstellen
SUB DRAWACTION(actionnr&, y&)
  LOCAL category&, turnnr&, movenr&, pl&, tp&, v&, shopnr&, newally&, level&, selectedLevel&
  LOCAL cl&, x&, i&, t$, textWidth&, textHeight&
  LOCAL actiontype$(), nactiontypes&

  'Aktionstypen auslesen
  nactiontypes& = DATACOUNT
  DIM actiontype$(nactiontypes&-1)
  FOR i& = 1 TO nactiontypes&
    actiontype$(i&-1) = TRANSLATEWORD$(READ$(i&))
  NEXT i&

  'Argumente auslesen
  category& = ASC(actions$, actionnr&*40+1)
  turnnr& = CVI(actions$, actionnr&*40+3)
  movenr& = CVI(actions$, actionnr&*40+5)
  pl& = ASC(actions$, actionnr&*40+7)
  tp& = ASC(actions$, actionnr&*40+8)
  v& = CVI(actions$, actionnr&*40+13)
  shopnr& = CVWRD(actions$, actionnr&*40+15)
  newally& = ASC(actions$, actionnr&*40+17)
  level& = GETACTIONLEVEL&(category&)
  selectedLevel& = listboxLevel.SelectedItem
  IF level& > 0 THEN
    category& = category&-level&
    D2D.GraphicBox(actionarea.left+1, y&, actionarea.right-1, y&+19, brushGreyscale&(9-level&), brushGreyscale&(9-level&))
    IF level& = selectedLevel& THEN D2D.GraphicBox(actionarea.left+1, y&, actionarea.left+24, y&+19, brushRed&, brushRed&)
  END IF

  'Runde und Bewegung
  IF category& = 0 AND pl& > 0 AND pl& < 64 THEN cl& = playerRGB&(LOG2(pl&)) ELSE cl& = brushBlack&
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+8)+" "+FORMAT$(turnnr&), actionarea.left+22, y&, cl&, hBUTTONFONT&)
  D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONSCREEN+9)+" "+FORMAT$(movenr&), actionarea.left+70, y&, cl&, hBUTTONFONT&)

  'Effekt
  SELECT CASE category&
  CASE 0:  'normal
    D2D.GraphicPrint(IIF$(tp& <= nactiontypes&, actiontype$(tp&-1), "???"), actionarea.left+130, y&, cl&, hBUTTONFONT&)
  CASE 48:  'Siegbedingung
    D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONLIST+IIF&(tp& >= 21 AND tp& < 26, 8, 2)), actionarea.left+130, y&, cl&, hBUTTONFONT&)
  CASE 80:  'Niederlagenbedingung
    D2D.GraphicPrint(GETWORD$(%WORDSTART_ACTIONLIST+3), actionarea.left+130, y&, cl&, hBUTTONFONT&)
  END SELECT

  x& = actionarea.left+205
  SELECT CASE tp&
  CASE 2:  'Wetter
    IF v& >= 0 AND v& <= 4 THEN t$ = GETWORD$(%WORDSTART_WEATHER+v&) ELSE t$ = FORMAT$(v&)
  CASE 3:  'Nachricht
    IF v& <= 98 THEN
      t$ = GETWORD$(%WORDSTART_MESSAGES+v&)
    ELSE
      IF isBI3& = 0 THEN
        IF totalUnitClasses& > 54 AND v& <= 154 THEN
          t$ = "EDT"+FORMAT$(v&, "000")
        ELSE
          t$ = FORMAT$(v&)
        END IF
      ELSE
        IF v& > 9999 THEN v& = v&-10000 : ELSE v& = MAPBI3VIDEOTOFILENUMBER&(v&)
        SELECT CASE v&
        CASE 400 TO 419: t$ = "INTRO "+FORMAT$(v&-399)
        CASE 500 TO 509: t$ = GETWORD$(%WORDSTART_ACTIONLIST+0)+" "+FORMAT$(v&-499)
        CASE ELSE: t$ = "VIDEO "+FORMAT$(v&, "000")
        END SELECT
      END IF
    END IF
  CASE 4:  'DF-Layer
    t$ =  FORMAT$(v&)
  CASE 7:  'Allianz
    IF v& >= 0 AND v& < 6 THEN cl& = playerRGB&(v&) ELSE cl& = brushBlack&
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(v&+1)+" "+GETWORD$(%WORDSTART_ACTIONSCREEN+10)+" "
    D2D.GraphicPrint(t$, x&, y&, cl&, hBUTTONFONT&)
    D2D.GraphicTextSize(t$, hBUTTONFONT&, textwidth&, textheight&)
    x& = x&+textwidth&
    t$ = ""
    FOR i& = 0 TO 5
      IF (newally& AND 2^i&) > 0 THEN
        D2D.GraphicPrint(FORMAT$(i&+1), x&, y&, playerRGB&(i&), hBUTTONFONT&)
        x& = x&+15
      END IF
    NEXT i&
  CASE 8:  'Spieler besiegt
    IF v& >= 0 AND v& <= 5 THEN cl& = playerRGB&(v&) ELSE cl& = brushBlack&
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(v&+1)+ " "+GETWORD$(%WORDSTART_ACTIONSCREEN+11)
  CASE 9:  'Spieler erobert Shop
    IF v& >= 0 AND v& <= 5 THEN cl& = playerRGB&(v&) ELSE cl& = brushBlack&
    t$ = LEFT$(GETWORD$(%WORDSTART_ACTIONSCREEN+1), 2)+". "+FORMAT$(v&+1)+" "+GETWORD$(%WORDSTART_ACTIONSCREEN+12)+" "+shopnames$(shopnr&)
  CASE 10:  'Runde erreicht
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+2)+" "+FORMAT$(v&)
  CASE 12:  'Einheit vernichtet
    IF shopnr& >= 0 AND shopnr& < 6 THEN cl& = playerRGB&(shopnr&) ELSE cl& = brushBlack&
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+13)+" "+IIF$(v& >= 0 AND v& < 64, unitnames$(v&), FORMAT$(v&))
  CASE 18:  'DF-Layer (nur normaler Schwierigkeitsgrad)
    t$ =  FORMAT$(v&)
  CASE 19:  'DF-Layer (nur schwerer Schwierigkeitsgrad)
    t$ =  FORMAT$(v&)
  CASE 20:  'benutzerdefinierte Nachricht
    t$ = PARSE$($CUSTOMMSGIDS, ";", v&+1)
  CASE 21:  'Siegbedingung f�r Bonusmission: Shop erobert
    IF v& >= 0 AND v& <= 5 THEN cl& = playerRGB&(v&) ELSE cl& = brushBlack&
    t$ = LEFT$(GETWORD$(%WORDSTART_ACTIONSCREEN+1), 2)+". "+FORMAT$(v&+1)+" "+GETWORD$(%WORDSTART_ACTIONSCREEN+12)+" "+shopnames$(shopnr&)
  CASE 22:  'Runde noch nicht erreicht
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+14)+" "+FORMAT$(v&)
  CASE 23:  'keine Verluste
    IF v& >= 0 AND v& <= 5 THEN cl& = playerRGB&(v&) ELSE cl& = brushBlack&
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+1)+" "+FORMAT$(v&+1)+ " "+GETWORD$(%WORDSTART_ACTIONSCREEN+15)
  CASE 24:  'Einheit �berlebt
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+16)+" "+IIF$(v& >= 0 AND v& < 64, unitnames$(v&), FORMAT$(v&))
  CASE 26:  'Team kontrolliert alle HQs
    t$ = GETWORD$(%WORDSTART_ACTIONSCREEN+18)+" "+GETWORD$(%WORDSTART_VICTORYCONDITION+9)
  CASE 27:  'lokales Wetter
    IF v& >= 0 AND v& <= 4 THEN t$ = GETWORD$(%WORDSTART_WEATHER+v&)+" "+GETWORD$(%WORDSTART_ACTIONSCREEN+17)+" "+FORMAT$(shopnr& AND 255)+"-"+FORMAT$(INT(shopnr&/256)) ELSE t$ = FORMAT$(v&)
  CASE ELSE:
    t$ =  FORMAT$(v&)
  END SELECT

  D2D.GraphicTextSize(t$, hBUTTONFONT&, textwidth&, textheight&)
  WHILE x&+textwidth& > actionarea.right AND LEN(t$) > 8
    t$ = LEFT$(t$, LEN(t$)-1)
    D2D.GraphicTextSize(t$, hBUTTONFONT&, textwidth&, textheight&)
  WEND
  D2D.GraphicPrint(t$, x&, y&, cl&, hBUTTONFONT&)

  DATA "?1", "Wetter", "Nachricht", "DF Layer", "?5", "?6", "Allianz", "Sieg", "Sieg", "?10"
  DATA "?11", "?12", "?13", "?14", "?15", "?16", "DF-Layer:1", "DF-Layer:2", "DF-Layer:3", "Nachricht"
  DATA "Bonusmission", "Bonusmission", "Bonusmission", "Bonusmission", "Bonusmission", "Sieg", "Wetter"
END SUB



'Einstellungs-Dialog anzeigen
SUB DRAWOPTIONS
  LOCAL i&, textwidth&, textheight&, a$

  radiogroupLanguage.Visible = 1
  checkboxReopen.Visible = 1
  checkboxBI2020.Visible = 1

  'Sprache
  a$ = GETWORD$(%WORDSTART_OPTIONS+0)
  D2D.GraphicBox(palettearea.left, palettearea.top, palettearea.right, palettearea.top+22, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(palettearea.left-1, palettearea.top+22, palettearea.right+1, palettearea.top+22, brushBorder&)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, palettearea.left+(palettearea.right-palettearea.left-textWidth&)/2, palettearea.top+2, brushBlack&, hBUTTONFONT&)
  FOR i& = 0 TO %MAXCOUNTRYFLAGS-1
    D2D.GraphicCopy(GETSPRITEHANDLE&(%COUNTRYFLAGS+i&), languagearea.left+24, languagearea.top+3+i&*30)
  NEXT i&

  'allgemeine Einstellungen
  a$ = GETWORD$(%WORDSTART_OPTIONS+1)
  D2D.GraphicLine(palettearea.left-1, generaloptionsarea.top-24, palettearea.right+1, generaloptionsarea.top-24, brushBorder&)
  D2D.GraphicBox(palettearea.left, generaloptionsarea.top-23, palettearea.right, generaloptionsarea.top-1, brushHighlightedTab&, brushHighlightedTab&)
  D2D.GraphicLine(palettearea.left-1, generaloptionsarea.top-1, palettearea.right+1, generaloptionsarea.top-1, brushBorder&)
  D2D.GraphicTextSize(a$, hBUTTONFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(a$, palettearea.left+(palettearea.right-palettearea.left-textWidth&)/2, generaloptionsarea.top-21, brushBlack&, hBUTTONFONT&)

END SUB



'Startbildschirm anzeigen
SUB SHOWWELCOMESCREEN
  LOCAL i&, x&, y&, textwidth&, textheight&, t$

  'Titel
  x& = maparea.left+10
  y& = maparea.top+10
  D2D.GraphicPrint(GETWORD$(%WORDSTART_WELCOME+0), x&, y&, brushBlack&, hBIGFONT&)
  y& = y&+40

  'Bedienung
  D2D.GraphicPrint(GETWORD$(%WORDSTART_WELCOME+1), x&, y&, brushBlack&, hBIGFONT&)
  y& = y&+25
  FOR i& = 2 TO 25
    D2D.GraphicPrint(GETWORD$(%WORDSTART_WELCOME+i&), x&, y&, brushBlack&, hTEXTFONT&)
    y& = y&+18
  NEXT i&

  'Version
  t$ = "Version "+FORMAT$(%COMPILE_VERSION/1000, "0.000")+" / "+compileDate$
  D2D.GraphicTextSize(t$, hTEXTFONT&, textwidth&, textheight&)
  D2D.GraphicPrint(t$, maparea.right-textwidth&-10, maparea.top+10, brushBlack&, hTEXTFONT&)
END SUB



'Bildschirm rendern
SUB RENDERSCENE(RenderTarget AS ID2D1HwndRenderTarget)
  LOCAL i&

  'Kartenbereich
  D2D.GraphicBox(maparea.left-1, maparea.top-1, maparea.right+1, maparea.bottom+1, brushBorder&, brushBackground&)
  CALL RENDERMAP
  D2D.GraphicBox(maparea.left-1, maparea.top-1, maparea.right+1, maparea.bottom+1, brushBorder&, -1)
  IF showWelcome& THEN CALL SHOWWELCOMESCREEN

  'Tabpages (Paletten)
  CALL RENDERTAB(RenderTarget)

  'Palette
  IF apperror& = 0 THEN CALL RENDERPALETTE(selectedTab&)

  'Buttons
  D2D.GraphicBox(buttonarea.left-1, buttonarea.top-1, buttonarea.right+1, buttonarea.bottom+1, brushBorder&, brushBackground&)
END SUB


  'Sprites zeigen
SUB SHOWSPRITES(RenderTarget AS ID2D1HwndRenderTarget)
  LOCAL i&, j&

  FOR j& = 0 TO 19
    FOR i& = 0 TO 49
      RenderTarget.DrawBitmap(D2D.Sprites(sprites&(i&+j&*50)), D2D.Helper.RectF(maparea.left+i&*24, maparea.top+j&*24, maparea.left+i&*24+24, maparea.top+j&*24+24), 1.0, %D2D1_BITMAP_INTERPOLATION_MODE_LINEAR)
    NEXT i&
  NEXT j&
END SUB



'Validationsmeldung erzeugen
SUB ADDVALIDATIONMSG(a$, x&, y&)
  IF nValidate& >= %MAXVALIDATE THEN EXIT SUB
  validateMessages$(nValidate&) = a$
  validatex&(nValidate&) = x&
  validatey&(nValidate&) = y&
  nValidate& = nValidate&+1
END SUB



'Karte validieren
SUB VALIDATEMAP
  LOCAL i&, k&, n&, x&, y&, z&
  REDIM validateMessages$(%MAXVALIDATE-1), validatex&(%MAXVALIDATE-1), validatey&(%MAXVALIDATE-1)

  nValidate& = 0

  'pr�fen, ob nicht-fabrikations Shops Produktionsmen� haben
  FOR i& = 0 TO nshops&-1
    IF shops(i&).shopfunction <> 2 THEN
      IF shops(i&).production(0) > 0 OR shops(i&).production(1) > 0 OR shops(i&).production(2) > 0 OR shops(i&).production(3) > 0 THEN
        CALL ADDVALIDATIONMSG(shopnames$(i&)+" "+GETWORD$(%WORDSTART_VALIDATION+2), shops(i&).position AND (yMapFactor&-1), INT(shops(i&).position/yMapFactor&))
      END IF
    END IF
  NEXT i&

  'pr�fen, ob sich 2 Shops an derselben Stelle befinden
  FOR i& = 0 TO nshops&-2
    FOR k& = i&+1 TO nshops&-1
      IF shops(i&).position = shops(k&).position THEN
        CALL ADDVALIDATIONMSG(shopnames$(i&)+" "+GETWORD$(%WORDSTART_VALIDATION+3)+" "+shopnames$(k&), shops(i&).position AND (yMapFactor&-1), INT(shops(i&).position/yMapFactor&))
      END IF
    NEXT k&
  NEXT i&

  'problematische Sprites pr�fen
  n& = DATACOUNT
  FOR i& = 1 TO n&
    k& = VAL(READ$(i&))
    FOR z& = 0 TO 1
      FOR y& = 0 TO mapheight&-1
        FOR x& = 0 TO mapwidth&-1
          IF mapdata%(x&, y&, z&, 0) = k& THEN
            CALL ADDVALIDATIONMSG(GETWORD$(%WORDSTART_VALIDATION+4), x&, y&)
          END IF
        NEXT x&
      NEXT y&
    NEXT z&
  NEXT i&

  DATA 196, 205, 206, 207, 208, 92
END SUB



'Karteninformation anzeigen
SUB SHOWMAPINFO
  LOCAL i&, x&, y&, n&, foundx&, foundy&

  'Energie und Material aller Spieler ermitteln
  nValidate& = 0
  CALL COUNTMATERIALENERGY

  'Eintr�ge auf DF-Layern z�hlen
  FOR i& = 1 TO %MAXDFLAYER
    n& = 0
    FOR y& = 0 TO mapheight&-1
      FOR x& = 0 TO mapwidth&-1
        IF mapdata%(x&, y&, 0, i&) <> 0 OR mapdata%(x&, y&, 1, i&) <> 0 OR mapdata%(x&, y&, 2, i&) <> 0 THEN
          n& = n&+1
          foundx& = x&
          foundy& = y&
        END IF
      NEXT x&
    NEXT y&
    IF n& > 0 THEN CALL ADDVALIDATIONMSG("DF"+FORMAT$(i&-1, "00")+" : "+FORMAT$(n&)+" "+GETWORD$(%WORDSTART_MAPINFO+1), foundx&, foundy&)
  NEXT i&

  CALL HIDEDIALOGUES(0)
  CALL SelectTab(-6)
END SUB



'Vergleicht die Debugdaten
SUB COMPAREDEBUGDATA(label$)
  LOCAL mnr&, i&, x&, y&, mx&, v&, w&, differs&, nfiles&, nrows&, blockoffset&

  CON.CLS

  'Dateien (Eintr�ge in debugdata) z�hlen
  FOR mnr& = 0 TO UBOUND(debugdata$())
    IF LEN(debugdata$(mnr&)) > 0 THEN
      nfiles& = nfiles&+1
      mx& = MAX&(mx&, LEN(debugdata$(mnr&)))
    END IF
  NEXT mnr&

  'Anzahl Bl�cke pro Bildschirmseite berechnen (bei 28 Bytes pro Zeile und 1 Zeile pro Mission)
  nrows& = INT(25/(nfiles+2))
  mx& = MIN&(mx&, nrows&*28)
  nrows& = INT((mx&+27)/28)

  'Missionsnummer
  COLOR 15, 0
  FOR i& = 0 TO nrows&-1
    y& = 2
    FOR mnr& = 0 TO UBOUND(debugdata$())
      IF LEN(debugdata$(mnr&)) > 0 THEN
        LOCATE y&+i&*(nfiles&+2), 1
        PRINT label$+FORMAT$(mnr&, "000");
        y& = y&+1
      END IF
    NEXT mnr&
  NEXT i&

  'Daten anzeigen, die sich von Satz zu Satz unterscheiden (28 Bytes pro Zeile, maximal 1 Bildschirmseite)
  blockoffset& = 0
  x& = 9
  FOR i& = 1 TO mx&
    'Unterschiede suchen
    differs& = 0
    v& = -1
    FOR mnr& = 0 TO UBOUND(debugdata$())
      IF LEN(debugdata$(mnr&)) >= i& THEN
        w& = ASC(debugdata$(mnr&), i&)
        IF v& = -1 THEN
          v& = w&
        ELSE
          IF v& <> w& THEN
            differs& = 1
            EXIT FOR
          END IF
        END IF
      END IF
    NEXT mnr&

    'Wert anzeigen
    IF differs& = 1 THEN
      COLOR 14, 0
      LOCATE 1+blockoffset&, x&
      PRINT FORMAT$(i&, "000");

      COLOR 7, 0
      y& = 2+blockoffset&
      FOR mnr& = 0 TO UBOUND(debugdata$())
        IF LEN(debugdata$(mnr&)) > 0 THEN
          LOCATE y&, x&
          PRINT FORMAT$(ASC(debugdata$(mnr&), i&), "000");
          y& = y&+1
        END IF
      NEXT mnr&
      x& = x&+4
    END IF

    IF x& > 117 THEN
      x& = 9
      blockoffset& = blockoffset&+nfiles&+2
    END IF
  NEXT i&
END SUB



'Grafik-Fenster erzeugen
SUB CREATEWIN
  LOCAL hDC&
  LOCAL RC AS RECT

  'Fenster mittig auf Desktop erzeugen
  GetClientRect(GetDesktopWindow(), RC)
  windowWidth& = RC.Right-RC.Left-40
  windowHeight& = RC.Bottom-RC.Top-80
  pWindow = CLASS "CWindow"
  IF ISNOTHING(pWindow) THEN EXIT SUB
  hWIN& = pWindow.CreateWindow(%NULL, "Battle Isle II Map Editor", 0, 0, 0, 0, 0, 0, CODEPTR(WindowProc))
  pWindow.ClassStyle = %CS_DBLCLKS
  pWindow.SetClientSize windowWidth&, windowHeight&
  pWindow.CenterWindow
END SUB



'Cursortasten gedr�ckt
SUB KEYBOARDMOVE(x&, y&)
  'Karte scrollen
  scrollX& = MAX&(0, MIN&(mapwidth&*16*zoom#-maparea.right+maparea.left+8*zoom#, scrollX&+x&*zoom#*16))
  scrollY& = MAX&(0, MIN&(mapheight*24*zoom#-maparea.bottom+maparea.top+12*zoom#, scrollY&+y&*zoom#*16))
END SUB



'Button gedr�ckt
SUB BUTTONPRESSED(btn&)
  LOCAL f$

  SELECT CASE btn&
  CASE buttonNew.ID:  'neue Karte
    IF SAVEQUERY& = 2 THEN EXIT SUB
    CALL HIDEDIALOGUES(0)
    CALL INITNEWMAPDIALOGUE
    showWelcome& = 0
    CALL SelectTab(-4)

  CASE buttonLoad.ID:  'Laden
    CALL OPENMISSION

  CASE buttonSave.ID:  'Speichern
    IF mapwidth& = 0 THEN EXIT SUB
    f$ = SAVEDIALOG$
    IF f$ <> "" THEN CALL SAVEMISSION(f$)

  CASE buttonQuit.ID:  'Beenden
    IF SAVEQUERY& = 2 THEN EXIT SUB
    PostQuitMessage 0

  CASE buttonProperties.ID:  'Eigenschaften
    IF mapwidth& = 0 THEN EXIT SUB
    CALL HIDEDIALOGUES(0)
    CALL SelectTab(-1)

  CASE buttonActions.ID:  'Aktionen
    IF mapwidth& = 0 THEN EXIT SUB
    CALL HIDEDIALOGUES(0)
    CALL SelectTab(-2)

  CASE buttonValidate.ID:  'Validieren
    IF mapwidth& = 0 THEN EXIT SUB
    CALL VALIDATEMAP
    CALL HIDEDIALOGUES(0)
    CALL SelectTab(-5)

  CASE buttonOptions.ID:  'Einstellungen
    CALL HIDEDIALOGUES(0)
    CALL SelectTab(-3)

  END SELECT
END SUB



'Einstellungen ge�ndert
SUB SETTINGSCHANGED(btn&)
  SELECT CASE btn&
  CASE radiogroupLanguage.ID:  'Sprache
    selectedLanguage& = radiogroupLanguage.SelectedItem
  CASE checkboxReopen.ID:  'letzte Karte wieder �ffnen
    reopenLastMap& = checkboxReopen.SelectedItem
  CASE checkboxBI2020.ID:  'Battle Isle 2020 unterst�tzen
    bi2020support& = checkboxBI2020.SelectedItem
    CALL ENABLEBI2020
  END SELECT
END SUB



'Battle Isle 2020 Unterst�tzung ein/ausschalten
SUB ENABLEBI2020
  LOCAL a$, i&

  'Missionscodes
  CALL INITMAPNAMES
  IF ISOBJECT(listboxNextmap) THEN
    FOR i& = 0 TO UBOUND(mapnames$())
      IF a$ <> "" THEN a$ = a$+";"
      a$ = a$+FORMAT$(i&)+" - "+mapnames$(i&)
    NEXT i&
    listboxNextmap.SetItems(a$)
    listboxBonusmap.SetItems(a$)
  END IF

  'Aktionstypen
  IF ISOBJECT(listboxAction) THEN
    listboxAction.SetItems(CREATELISTBOXITEMS$(%WORDSTART_ACTIONLIST, IIF&(bi2020support& = 0, 6, 11)))
  END IF
END SUB



'Dialog f�r neue Karte initialisieren
SUB INITNEWMAPDIALOGUE
  LOCAL a$, i&

  a$ = "16;20;24;28;32;36;40;44;48;52;56;60;64"
  IF bi2020support& <> 0 THEN
    FOR i& = 68 TO 256 STEP 4
      a$ = a$+";"+FORMAT$(i&)
    NEXT i&
  END IF
  listboxWidth.SetItems(a$)
  listboxHeight.SetItems(a$)
END SUB



'Pr�fen, ob Karte noch gespeichert werden mu�
FUNCTION SAVEQUERY&
  LOCAL r&, f$

  IF mapChanged& = 0 THEN EXIT FUNCTION
  r& = MessageBox(hWIN&, GETWORD$(%WORDSTART_SAVEQUERY+1), GETWORD$(%WORDSTART_SAVEQUERY+0), %MB_YESNOCANCEL OR %MB_ICONQUESTION)

  IF r& = %IDCANCEL THEN
    'Abbruch
    SAVEQUERY& = 2
    EXIT FUNCTION
  END IF

  IF r& = %IDYES THEN
    'Speichern
    f$ = SAVEDIALOG$
    IF f$ = "" THEN
      SAVEQUERY& = 2
      EXIT FUNCTION
    END IF
    CALL SAVEMISSION(f$)
    SAVEQUERY& = 1
    EXIT FUNCTION
  END IF

  SAVEQUERY& = 0
END FUNCTION



'Maus-Bewegung verarbeiten
SUB MOUSEMOVE(x&, y&)
  LOCAL i&, j&, btn&

  mousexpos& = x&
  mouseypos& = y&

  'Karte scrollen
  IF dragStartX& >= 0 THEN
    scrollX& = MAX&(0, MIN&(mapwidth&*16*zoom#-maparea.right+maparea.left+8*zoom#, scrollX&+dragStartX&-x&))
    scrollY& = MAX&(0, MIN&(mapheight*24*zoom#-maparea.bottom+maparea.top+12*zoom#, scrollY&+dragStartY&-y&))
    dragStartX& = x&
    dragStartY& = y&
  END IF

  'Zeichnen
  IF drawing& > 0 AND x& >= maparea.left AND x& <= maparea.right AND y& >= maparea.top AND y& <= maparea.bottom AND selectedSprite& >= 0 THEN
    CALL GETMAPPOS((x&-maparea.left+scrollX&)/zoom#, (y&-maparea.top+scrollY&)/zoom#, i&, j&)
    IF i& >= 0 AND i& < mapwidth& AND j& >= 0 AND j& < mapheight& AND mapdata%(i&, j&, selectedLevel&, selectedDF&) <> selectedSprite& THEN
      CALL UPDATEMAP(i&, j&, selectedLevel&, selectedSprite&)
      CALL SMOOTHMAP(i&, j&, selectedLevel&)
    END IF
  END IF

  'Kartenbereich markieren
  IF mapSelection.left >= 0 AND shiftpressed = 1 THEN
    CALL GETMAPPOS((x&-maparea.left+scrollX&)/zoom#, (y&-maparea.top+scrollY&)/zoom#, i&, j&)
    IF i& >= 0 AND i& < mapwidth& AND j& >= 0 AND j& < mapheight& THEN
      mapSelection.right = i&
      mapSelection.bottom = j&
    END IF
  END IF
END SUB



'Maus-Klicks verarbeiten
'md: 1 = links gedr�ckt , 2 = links losgelassen , 3 = rechts gedr�ckt , 4 = rechts losgelassen
SUB MOUSECLICK(x&, y&, md&)
  LOCAL sprnr&, i&, j&, e&, n&, shopnr&, editnr&, listnr&, pl&
  LOCAL oldedit, newedit AS IDXCONTROL

  oldedit = D2D.GetFocusedControl()
  D2D.OnClick(x&, y&, md&)

  SELECT CASE md&
  CASE 1:  'links gedr�ckt
    IF x& >= maparea.left AND x& <= maparea.right AND y& >= maparea.top AND y& <= maparea.bottom AND selectedSprite& >= 0 THEN
      CALL SAVEUNDO
      IF ISMULTISPRITE&(selectedSprite&) = -1 THEN drawing& = 1
    END IF

  CASE 2:  'links losgelassen
    'Tab-Wechsel
    IF HITTEST&(x&, y&, palettearea.left, 11, palettearea.right, palettearea.top-2) THEN
      IF selectedTab& < 0 THEN CALL HIDEDIALOGUES(0)
      CALL SelectTab(INT((x&-palettearea.left)/80))
      IF selectedTab& <> 4 THEN CALL HIDESHOPDIALOGUE
    END IF

    'Eingabefeld fokusieren
    newedit = D2D.GetFocusedControl()
    IF ISOBJECT(oldedit) <> ISOBJECT(newedit) THEN
      CALL UPDATESHOP
    END IF

    'Sprite-Auswahl
    IF x& >= palettearea.left AND x& < palettearea.left+25*paletteCols& AND y& > palettearea.top AND y& < palettearea.top+25*paletteRows& AND selectedTab& >= 0 THEN
      sprnr& = INT((x&-palettearea.left)/25)+INT((y&-palettearea.top)/25)*paletteCols&
      IF scrollbarTabpages(selectedTab&).Visible <> 0 THEN sprnr& = sprnr&+scrollbarTabpages(selectedTab&).ScrollPosition*paletteCols&
      IF sprnr& >= 0 AND sprnr& < LEN(palette$(selectedTab&))/2 THEN
        selectedSprite& = CVI(palette$(selectedTab&), sprnr&*2+1)
        selectedLevel& = IIF&(selectedTab& = 0, 0, IIF&(selectedTab& = 4, 2, 1))
      END IF
    END IF

    'Karte bearbeiten
    IF x& >= maparea.left AND x& <= maparea.right AND y& >= maparea.top AND y& <= maparea.bottom AND selectedSprite& >= 0 THEN
      CALL GETMAPPOS((x&-maparea.left+scrollX&)/zoom#, (y&-maparea.top+scrollY&)/zoom#, i&, j&)
      IF i& >= 0 AND i& < mapwidth& AND j& >= 0 AND j& < mapheight& THEN
        sprnr& = ISMULTISPRITE&(selectedSprite&)
        IF sprnr& >= 0 THEN
          CALL INSERTMULTISPRITE(i&, j&, sprnr&)
        ELSE
          IF drawing& = 0 THEN CALL SAVEUNDO
          CALL UPDATEMAP(i&, j&, selectedLevel&, selectedSprite&)
          IF ctrlpressed& = 0 THEN CALL SMOOTHMAP(i&, j&, selectedLevel&)
        END IF
      END IF
    END IF
    drawing& = 0

    'Produktions-Slot
    IF selectedShop& >= 0 AND shops(selectedShop&).shopfunction = 2 AND selectedSprite& > 1337 THEN
      FOR i& = 0 TO 3
        IF HITTEST&(x&, y&, shoparea.left+86, shoparea.top+36+i&*30, shoparea.left+109, shoparea.top+59+i&*30) THEN
          e& = (selectedSprite&-1337) AND 255
          shops(selectedShop&).production(i&) = e&
         END IF
      NEXT i&
    END IF

    'Shop-Inhalt
    IF selectedShop& >= 0 AND shops(selectedShop&).unittype = 1 AND selectedSprite& >= 1337 THEN
      n& = 0
      FOR j& = 0 TO 3
        FOR i& = 0 TO 3
          IF shops(selectedShop&).shoptype = 0 AND n& = %MAXTRANSPORTERSLOT THEN EXIT FOR
          IF HITTEST&(x&, y&, shoparea.left+126+i&*30, shoparea.top+36+j&*30, shoparea.left+149+i&*30, shoparea.top+59+j&*30) THEN
            e& = (selectedSprite&-1337) AND 255
            shops(selectedShop&).content(i&+j&*4) = e&
           END IF
           n& = n&+1
        NEXT i&
      NEXT j&
    END IF

    'allgemeine Produktionspalette
    IF selectedTab& = -1 AND HITTEST&(x&, y&, gpmarea.left, gpmarea.top, gpmarea.right, gpmarea.bottom) THEN
      i& = INT((x&-gpmarea.left)/30)
      j& = INT((y&-gpmarea.top)/32)
      e& = GETNEXTGPM&(i&, j&)
      gpm?(i&, j&) = e&
    END IF

    'Allianzen
    IF selectedTab& = -1 THEN
      FOR j& = 0 TO 5
        FOR i& = 0 TO 5
          IF i& <> j& AND HITTEST&(x&, y&, allyarea.left+i&*18, allyarea.top+j&*20, allyarea.left+16+i&*18, allyarea.top+16+j&*20) THEN
            e& = ASC(allymatrix$, j&+1)
            e& = e& XOR 2^i&
            ASC(allymatrix$, j&+1) = e&
            e& = ASC(allymatrix$, i&+1)
            e& = e& XOR 2^j&
            ASC(allymatrix$, i&+1) = e&
           END IF
        NEXT i&
      NEXT j&
    END IF

    'AI-Maske
    IF selectedTab& = -1 THEN
      FOR i& = 0 TO 5
        IF HITTEST&(x&, y&, aimaskarea.left, aimaskarea.top+i&*20, aimaskarea.left+16, aimaskarea.top+16+i&*20) THEN
          aiMask& = aiMask& XOR 2^i&
         END IF
      NEXT i&
    END IF

    'Start-Terrain-Auswahl
    IF x& >= startterrainarea.left AND x& < startterrainarea.right AND y& > startterrainarea.top AND y& < startterrainarea.bottom AND selectedTab& = -4 THEN
      sprnr& = INT((x&-startterrainarea.left)/48)
      IF sprnr& >= 0 AND sprnr& < LEN(startterrain$)/2 THEN
        defaultTerrain& = CVI(startterrain$, sprnr&*2+1)
        CALL CREATEMAP(listboxWidth.SelectedItem*4+16, listboxHeight.SelectedItem*4+16, defaultTerrain&)
      END IF
    END IF

    'Fehlermeldungen / Karteninfo
    IF (selectedTab& = -5 OR selectedTab& = -6) AND x& > validationarea.left AND x& < validationarea.right AND y& >= validationarea.top+30 THEN
      showMapError& = INT((y&-validationarea.top-30)/20)
      IF showMapError& >= nValidate& THEN
        showMapError& = -1
      ELSE
        IF showMapError& >= 0 AND LEFT$(validateMessages$(showMapError&), 2) = "DF" THEN CALL SWITCHDFLAYER(VAL(MID$(validateMessages$(showMapError&), 3 ,2))+1)
      END IF
    END IF

  CASE 3:  'rechts gedr�ckt
    IF x& >= maparea.left AND x& <= maparea.right AND y& >= maparea.top AND y& <= maparea.bottom THEN
      dragStartX& = x&
      dragStartY& = y&
    END IF

  CASE 4:  'rechts losgelassen
    dragStartX& = -1

    'Shop ausw�hlen
    IF x& >= maparea.left AND x& <= maparea.right AND y& >= maparea.top AND y& <= maparea.bottom THEN
      CALL GETMAPPOS((x&-maparea.left+scrollX&)/zoom#, (y&-maparea.top+scrollY&)/zoom#, i&, j&)
      IF i& >= 0 AND i& < mapwidth& AND j& >= 0 AND j& < mapheight& THEN
        shopnr& = FINDSHOP&(i&, j&)
        IF shopnr& >= 0 THEN
          'Shop anzeigen
          IF selectedShop& >= 0 THEN CALL UPDATESHOP
          selectedShop& = shopnr&
          CALL HIDEDIALOGUES(%DIALOGUE_SHOP)
          CALL SHOWSHOP
        ELSE
          'pr�fen, ob sich eine AI Einheit an dieser Stelle befindet
          e& = mapdata%(i&, j&, 2, 0)
          pl& = GETPLAYERFORUNIT&(e&)
          IF e& >= 0 AND pl& >= 0 AND pl& < 6 AND (aiMask& AND 2^pl&) > 0 THEN
            CALL INSERTAIPOINT(i&, j&)
            IF selectedShop& >= 0 THEN CALL UPDATESHOP
            selectedShop& = nshops&-1
            CALL HIDEDIALOGUES(%DIALOGUE_SHOP)
            CALL SHOWSHOP
          ELSE
            'nichts ausgew�hlt
            CALL HIDESHOPDIALOGUE
          END IF
        END IF
      END IF
    END IF

    'Produktions-Slot
    IF selectedShop& >= 0 THEN
      FOR i& = 0 TO 3
        IF HITTEST&(x&, y&, shoparea.left+86, shoparea.top+36+i&*30, shoparea.left+109, shoparea.top+59+i&*30) THEN
          shops(selectedShop&).production(i&) = 0
         END IF
      NEXT i&
    END IF

    'Shop-Inhalt
    IF selectedShop& >= 0 THEN
      FOR j& = 0 TO 3
        FOR i& = 0 TO 3
          IF HITTEST&(x&, y&, shoparea.left+126+i&*30, shoparea.top+36+j&*30, shoparea.left+149+i&*30, shoparea.top+59+j&*30) THEN
            shops(selectedShop&).content(i&+j&*4) = -1
           END IF
        NEXT i&
      NEXT j&
    END IF

    'allgemeine Produktionspalette
    IF selectedTab& = -1 AND HITTEST&(x&, y&, gpmarea.left, gpmarea.top, gpmarea.right, gpmarea.bottom) THEN
      i& = INT((x&-gpmarea.left)/30)
      j& = INT((y&-gpmarea.top)/32)
      gpm?(i&, j&) = 0
    END IF

  END SELECT
END SUB



'Mausrad verarbeiten
SUB MOUSEWHEEL(v#)
  LOCAL wd&, hg&, newZoom#, oldZoom#

  newZoom# = zoom#+v#*0.25
  oldZoom# = zoom#
  wd& = (maparea.right-maparea.left)/2
  hg& = (maparea.bottom-maparea.top)/2
  zoom# = MAX(IIF(yMapFactor& = 64, 1.0, 0.25), MIN(6.0, newZoom#))
  scrollX& = MAX&(0, MIN&(mapwidth&*16*zoom#-maparea.right+maparea.left+8*zoom#, (scrollX&+wd&)/oldZoom#*zoom#-wd&))
  scrollY& = MAX&(0, MIN&(mapheight&*24*zoom#-maparea.bottom+maparea.top+12*zoom#, (scrollY&+hg&)/oldZoom#*zoom#-hg&))
END SUB



'Tastendruck verarbeiten
SUB KEYPRESS(k$)
  LOCAL oldtext$
  LOCAL c AS IDXCONTROL

  c = D2D.GetFocusedControl()
  IF ISOBJECT(c) THEN
    oldtext$ = c.Value
    D2D.OnKeyPress(k$)
    IF c.ControlType = %CTYPE_EDIT THEN
      IF oldtext$ <> c.Value THEN CALL UPDATESHOP
      EXIT SUB  'Edit-Control erh�lt Key-Event exklusiv
    END IF
  END IF

  SELECT CASE k$
  CASE CHR$(27):  'Markierung aufheben
    mapSelection.left = -1
    mapSelection.right = -1
    showMapError& = -1
  CASE ",": CALL SWITCHDFLAYER(0)
  CASE "0" TO "9": CALL SWITCHDFLAYER(ASC(k$)-47)
  CASE "+": CALL MOUSEWHEEL(1)
  CASE "-": CALL MOUSEWHEEL(-1)
  CASE "F", "f": CALL FILLSELECTION
  CASE "I", "i": CALL PEEKSPRITE(selectedDF&)
  CASE "S", "s": CALL TOGGLEHIGHLIGHTSHOPS
  CASE CHR$(3): CALL COPYMAP
  CASE CHR$(9): CALL SHOWMAPINFO
  CASE CHR$(11): CALL COPYDFLAYER
  CASE CHR$(15): CALL BUTTONPRESSED(buttonLoad.ID)  'Laden
  CASE CHR$(18): CALL CLEARDFLAYER
  CASE CHR$(19): CALL BUTTONPRESSED(buttonSave.ID)  'Speichern
  CASE CHR$(22): CALL PASTEMAP
  CASE CHR$(26): CALL UNDOEDIT
  CASE CHR$(0, 72): CALL KEYBOARDMOVE(0, -1)
  CASE CHR$(0, 75): CALL KEYBOARDMOVE(-1, 0)
  CASE CHR$(0, 77): CALL KEYBOARDMOVE(1, 0)
  CASE CHR$(0, 80): CALL KEYBOARDMOVE(0, 1)
  CASE CHR$(0, 82):
    CALL CREATESHOPATCURSORPOS
  CASE CHR$(0, 83):
    IF mapSelection.left < 0 OR mapSelection.right < 0 THEN
      CALL DELETESPRITE
    ELSE
      CALL DELETESECTION
    END IF
  CASE CHR$(0, 63)  'F5
    CALL LAUNCHMAP
  END SELECT
END SUB



'Fenstergr��e �ndern
SUB RESIZEWIN(wd&, hg&)
  LOCAL i&

  IF NOT ISOBJECT(buttonNew) THEN EXIT SUB

  CALL INITAREAS

  'Buttons verschieben
  buttonNew.XPos = buttonarea.left+2
  buttonNew.YPos = buttonarea.top+32
  buttonLoad.XPos = buttonarea.left+102
  buttonLoad.YPos = buttonarea.top+32
  buttonSave.XPos = buttonarea.left+202
  buttonSave.YPos = buttonarea.top+32
  buttonQuit.XPos = buttonarea.left+302
  buttonQuit.YPos = buttonarea.top+32
  buttonActions.XPos = buttonarea.left+2
  buttonActions.YPos = buttonarea.top+2
  buttonProperties.XPos = buttonarea.left+102
  buttonProperties.YPos = buttonarea.top+2
  buttonValidate.XPOS = buttonarea.left+202
  buttonValidate.YPOS = buttonarea.top+2
  buttonOptions.XPos = buttonarea.left+302
  buttonOptions.YPos = buttonarea.top+2

  'Scrollbars verschieben
  FOR i& = 0 TO 4
    scrollbarTabpages(i&).XPos = palettearea.right-16
    scrollbarTabpages(i&).Height = palettearea.bottom-palettearea.top
  NEXT i&

  'Shop Dialog verschieben
  editShopname.XPos = shoparea.left+3
  editShopname.YPos = shoparea.top+3
  editEnergy.XPos = shoparea.left+24
  editEnergy.YPos = shoparea.top+40
  editMaterial.XPos = shoparea.left+24
  editMaterial.YPos = shoparea.top+70
  editEnergyPlus.XPos = shoparea.left+24
  editEnergyPlus.YPos = shoparea.top+100
  editMaterialPlus.XPos = shoparea.left+24
  editMaterialPlus.YPos = shoparea.top+130
  listboxOwner.XPos = shoparea.right-130
  listboxOwner.YPos = shoparea.top+40
  listboxShoptype.XPos = shoparea.right-130
  listboxShoptype.YPos = shoparea.top+70

  'AI Point Dialog verschieben
  listboxAICommand.XPos = shoparea.left+80
  listboxAICommand.YPos = shoparea.top+40

  'Karteneigenschaften verschieben
  listboxTerrain.XPos = miscsettingsarea.left+5
  listboxWeather.XPos = miscsettingsarea.left+138
  listboxWinCond.XPos = miscsettingsarea.left+271

  'Aktions-Dialog verschieben
  FOR i& = 0 TO %MAXACTIONS-1
    buttonDeleteAction(i&).XPos = actionarea.left+2
  NEXT i&
  buttonNewAction.XPos = newactionarea.right-20
  listboxPlayer.XPos = newactionarea.left+50
  listboxTurn.XPos = newactionarea.left+50
  listboxMovement.XPos = newactionarea.left+50
  listboxAction.XPos = newactionarea.left+250
  listboxParam1.XPos = newactionarea.left+250
  listboxParam2.XPos = newactionarea.left+250
  listboxParam3.XPos = newactionarea.left+312
  listboxNextmap.XPos = aimaskarea.left+40
  listboxBonusmap.XPos = aimaskarea.left+40
  editMapDescription.XPos = mapdescriptionarea.left+3
  editMapShortDescr.XPos = mapdescriptionarea.left+3
  listboxCustomMessages.XPos = custommessagesarea.right-140
  editCustomMessageGER.XPos = custommessagesarea.left+3
  editCustomMessageENG.XPos = custommessagesarea.left+3
  editCustomMessageUserLang.XPos = custommessagesarea.left+3
  editCustomLangCode.XPos = custommessagesarea.right-43
  FOR i& = 0 TO %MAXPLAYERS-1
    editPlayerName(i&).XPos = palettearea.left+2
  NEXT i&
  listboxLevel.XPos = newactionarea.left+250
  actionsPerPage& = (actionarea.bottom-actionarea.top-48)/20
  scrollbarActions.XPos = actionarea.right-16
  scrollbarActions.Height = actionarea.bottom-actionarea.top-48
  scrollbarActions.VisibleRows = actionsPerPage&
  scrollbarActions.MaxScroll = actionCount& - actionsPerPage&

  'Einstellungen verschieben
  radiogroupLanguage.XPos = languagearea.left
  checkboxReopen.XPos = generaloptionsarea.left
  checkboxBI2020.XPos = generaloptionsarea.left

  'Neue Karte verschieben
  listboxWidth.XPos = newmaparea.left+65
  listboxHeight.XPos = newmaparea.left+260
END SUB



'Taste gedr�ckt oder losgelassen
SUB KEYEVENT(k&, md&)
  LOCAL mapx&, mapy&
  LOCAL c AS IDXCONTROL

  c = D2D.GetFocusedControl()
  IF ISOBJECT(c) THEN EXIT SUB

  SELECT CASE k&
  CASE 2 TO 11:  '1 to 0
    IF shiftpressed& <> 0 THEN
      IF k& = 11 THEN
        CALL SWITCHDFLAYER(11)
      ELSE
        CALL SWITCHDFLAYER(k&+10)
      END IF
    ELSE
      IF ctrlpressed& <> 0 THEN
        IF k& = 11 THEN
          CALL SWITCHDFLAYER(21)
        ELSE
          CALL SWITCHDFLAYER(k&+20)
        END IF
      END IF
    END IF
  CASE 29:  'CTRL
    ctrlpressed& = 1-md&
  CASE 42:  'SHIFT
    shiftpressed& = 1-md&
    CALL GETMAPPOS((mousexpos&-maparea.left+scrollX&)/zoom#, (mouseypos&-maparea.top+scrollY&)/zoom#, mapx&, mapy&)
    IF md& = 0 AND mapSelection.left = -1 THEN
      'Markierung anfangen
      IF mapx& >= 0 AND mapx& < mapwidth& AND mapy& >= 0 AND mapy& < mapheight& THEN
        mapSelection.left = mapx&
        mapSelection.top = mapy&
        mapSelection.right = mapx&
        mapSelection.bottom = mapy&
      END IF
    END IF
  END SELECT
END SUB



'Window Ereignis Verarbeitung
FUNCTION WindowProc (BYVAL hwnd AS DWORD, BYVAL wMsg AS DWORD, BYVAL wParam AS DWORD, BYVAL lParam AS LONG) AS LONG
  LOCAL X&, Y&, EXTKEY&, K&

  SELECT CASE wMsg
  CASE %WM_LBUTTONDOWN
    CALL MOUSECLICK(lParam AND 65535, INT(lParam/65536), 1)
    EXIT FUNCTION

  CASE %WM_LBUTTONUP
    CALL MOUSECLICK(lParam AND 65535, INT(lParam/65536), 2)
    EXIT FUNCTION

  CASE %WM_RBUTTONDOWN
    CALL MOUSECLICK(lParam AND 65535, INT(lParam/65536), 3)
    EXIT FUNCTION

  CASE %WM_RBUTTONUP
    CALL MOUSECLICK(lParam AND 65535, INT(lParam/65536), 4)
    EXIT FUNCTION

  CASE %WM_MOUSEMOVE
    X& = lParam AND 65535
    Y& = INT(lParam/65536)
    CALL MOUSEMOVE(X&, Y&)
    EXIT FUNCTION

  CASE %WM_MOUSEWHEEL
    CALL MOUSEWHEEL(GET_WHEEL_DELTA_WPARAM(wParam)/%WHEEL_DELTA)
    EXIT FUNCTION

  CASE %WM_KEYDOWN
    EXTKEY& = INT(lParam/16777216) AND 1
    K& = INT(lParam/65536) AND 255
    IF EXTKEY& = 1 AND K& >= 71 THEN
      CALL KEYPRESS(CHR$(0, K&))
    END IF
    IF EXTKEY& = 0 AND K& >= 59 AND K& <= 67 THEN
      CALL KEYPRESS(CHR$(0, K&))
    END IF
'PRINT "KeyDown:"; K&; wParam; EXTKEY&
    CALL KEYEVENT(k&, 0)
    EXIT FUNCTION

  CASE %WM_KEYUP
'PRINT INT(lParam/65536) AND 255, wParam; INT(lParam/16777216) AND 1
    K& = INT(lParam/65536) AND 255
    CALL KEYEVENT(k&, 1)
    EXIT FUNCTION

  CASE %WM_CHAR
    CALL KEYPRESS(CHR$(wParam))
'PRINT CHR$(wParam), wParam; lParam

  CASE %WM_TIMER
    D2D.OnRender(mousexpos&, mouseypos&)
    EXIT FUNCTION

  CASE %WM_SIZE
    windowWidth& = LO(INTEGER, lParam)
    windowHeight& = HI(INTEGER, lParam)
    IF ISOBJECT(D2D) THEN D2D.OnResize(windowWidth&, windowHeight&)
    CALL RESIZEWIN(windowWidth&, windowHeight&)

  CASE %WM_GETMINMAXINFO
    POKE LONG, lParam+24, 800  'ptMinTrackSize.x
    POKE LONG, lParam+28, 600  'ptMinTrackSize.y

  CASE %WM_CLOSE
    IF SAVEQUERY& = 2 THEN EXIT FUNCTION

  CASE %WM_DESTROY
    D2D = NOTHING
    'close the application by sending a WM_QUIT message
    PostQuitMessage 0
    EXIT FUNCTION

  CASE %WM_SYSCOMMAND
    X& = wParam AND &HFFF0
    IF X& = %SC_CLOSE THEN
      SendMessage hwnd, %WM_CLOSE, 0, 0
      EXIT FUNCTION
    END IF
  END SELECT

  'pass unprocessed messages to Windows
  FUNCTION = DefWindowProc(hWnd, wMsg, wParam, lParam)
END FUNCTION



'Converts the last WinApi error to a text message
FUNCTION FormatErrorMessage$
  LOCAL E&
  LOCAL ERRMSG AS ASCIIZ*1024

  E& = GetLastError
  FormatMessage %FORMAT_MESSAGE_FROM_SYSTEM, BYVAL 0, E&, 0,ERRMSG, 1023, 0

  FormatErrorMessage$ = "Error: "+FORMAT$(E&)+" - "+ERRMSG
END FUNCTION



SUB LogStartUpInfo
  LOCAL a$

  a$ = DATE$
  CALL BIDebugLog("Battle Isle Editor started on "+RIGHT$(a$, 4)+"-"+LEFT$(a$, 2)+"-"+MID$(a$, 4, 2)+" "+TIME$)
  CALL BIDebugLog("Command line arguments: "+TRIM$(COMMAND$))
  CALL BIDebugLog("Language: "+IIF$(selectedLanguage& = 0, "GER", "ENG"))
  CALL BIDebugLog("Editor mode: "+IIF$(isBI3& = 1, "BI3", "BI2"))
  CALL BIDebugLog("Default custom language: "+custMsgUserLang$)
  CALL BIDebugLog("Open recent map: "+missionFileName$)
  CALL BIDebugLog("Battle Isle 2020 support: "+IIF$(bi2020support& = 0, "No", "Yes"))
  CALL BIDebugLog("Battle Isle Editor folder: "+EXEPATH$)
  CALL BIDebugLog("LIB folder: "+libFolder$)
  CALL BIDebugLog("MIS folder: "+misFolder$)
  CALL BIDebugLog("BI2020 folder: "+bi2020folder$)
  CALL BIDebugLog("Custom terrain sprites: "+FORMAT$(customterrainsprites&))
  CALL BIDebugLog("Custom unit sprites: "+FORMAT$(CUSTOMUNITSPRITES))
END SUB



'Kommandozeilenargumente verarbeiten
SUB ProcessCommandLineArgs
  LOCAL c$, a$, i&, n&

  c$ = TRIM$(COMMAND$)
  n& = PARSECOUNT(c$, " ")
  i& = 1
  WHILE i& <= n&
    a$ = UCASE$(PARSE$(c$, " ", i&))
    SELECT CASE a$
    CASE "-MAPFROMGIF", "/MAPFROMGIF":
      createMapFromGifFilename$ = PARSE$(c$, " ", i&+1)
      i& = i&+1
    CASE "/DEBUGLOG"
      enableDebugLog& = 1
    END SELECT
    i& = i&+1
  WEND
END SUB



'Hauptprogramm
FUNCTION PBMAIN&
  LOCAL e$, cfg&, k&
  LOCAL msgtext AS ASCIIZ*512
  LOCAL Built AS IPOWERTIME
  DIM undoData$(%MAXUNDOLEVELS-1), shops(%MAXSHOPS-1), shopnames$(%MAXSHOPS-1), gpm?(12, 1), editPlayerName(%MAXPLAYERS-1)
  DIM validateMessages$(%MAXVALIDATE-1), validatex&(%MAXVALIDATE-1), validatey&(%MAXVALIDATE-1), customMessages$$(2, %MAXCUSTOMMSG-1)
  DIM debugdata$(99)

  EXEPATH$ = EXE.PATH$
  IF AfxGetWindowsVersion => 6 THEN SetProcessDPIAware
  CALL ProcessCommandLineArgs
  cfg& = READCONFIG&
  CALL CheckBIVersion
  yMapFactor& = 64
  CALL LogStartUpInfo

  LET Built = CLASS "PowerTime"
  Built.FileTime = %PB_COMPILETIME
  compileDate$ = Built.DateString

  D2D = CLASS "CDIRECT2D"
  IF ISNOTHING(D2D) THEN EXIT FUNCTION

  CALL CREATEWIN
  CALL INITAREAS
  CALL INITMAPNAMES
  CALL INITUNITNAMES

  IF ISFALSE D2D.InitD2D(hWIN&, 2000, CODEPTR(RENDERSCENE)) THEN EXIT FUNCTION
  CALL INITBRUSHES
  CALL INITCONTROLS
  CALL INITOBJECTPALETTE  'Shops definieren, damit diese in LOADSPRITES$ korrekt eingef�rbt werden

'CALL COMPAREDEBUGDATA("MISS")

  IF createMapFromGifFilename$ <> "" THEN CALL CreateMapFromGif(createMapFromGifFilename$)
  IF mapCreatedFromGif& = 0 THEN
    IF missionFileName$ <> "" AND ISFILE(missionFileName$) THEN
      CALL LOADMISSION&(missionFileName$)
    ELSE
      showWelcome& = 1
    END IF
  END IF

  highlightedButton& = -1
  highlightedActionButton& = -1
  highlightedNewActionButton& = -1
  selectedShop& = -1
  mapSelection.left = -1
  mapSelection.right = -1
  selectedSprite& = -1
  showMapError& = -1
  dragStartX& = -1
  zoom# = 1.0
  startterrain$ = MKI$(95)+MKI$(96)+MKI$(97)+MKI$(273)+MKI$(271)+MKI$(274)+MKI$(312)+MKI$(355)
  defaultTerrain& = 97

  ShowWindow hWIN&, %SW_SHOW
  UpdateWindow hWIN&
  SetTimer(hWIN&, 1, 50, %NULL)

  e$ = LOADSPRITES$
  IF e$ <> "" OR cfg& = 0 THEN
    apperror& = 1
    IF cfg& = 0 THEN e$ = $CONFIGFILE
    msgtext = GETWORD$(%WORDSTART_ERROR+2)+CHR$(13,10,13,10)+e$
    MessageBox hWIN&, msgtext, GETWORD$(%WORDSTART_ERROR+3), %MB_OK OR %MB_ICONERROR
    PostQuitMessage 0
  END IF

  CALL INITOBJECTPALETTE  'Paletten erneut erzeugen, da jetzt bekannt ist, welche Einheitenklassen existieren
  CALL INITMULTISPRITES

  'Window Nachrichten verarbeiten
  LOCAL uMsg AS tagMsg
  WHILE GetMessage(uMsg, %NULL, 0, 0)
    TranslateMessage uMsg
    DispatchMessage uMsg
  WEND

  KillTimer(hWIN&, 1)

  CALL SAVECONFIG
  CALL BIDebugLog("Battle Isle Editor exited normally.")
  CALL BIDebugLog("")
END FUNCTION
