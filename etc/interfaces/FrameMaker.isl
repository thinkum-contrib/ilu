INTERFACE FrameMaker BRAND "V1";

(* This is the interface published with FrameMaker 4.  It has
 * not been extensively tested, and we have no idea of whether
 * it works with FrameMaker 5 or later versions.
 *)

TYPE Error = RECORD code : CARDINAL, message : ilu.CString END;
TYPE CommandArray = ARRAY OF 150 INTEGER;
TYPE Commands = RECORD length : CARDINAL, commands : CommandArray END;
TYPE DocAndError = RECORD doc : CARDINAL, error : Error END;
TYPE DocIndex = CARDINAL;

(*********************************************
 *
 * Keyboard command codes and their default keyboard assignments follow.
 *
 * Modifiers for keyboard assignments are:
 * 	^ = Control
 *	~ = Meta
 *	! = Escape
 *	s = Shift
 *)

CONSTANT NULLINPUT : INTEGER =	-1;
CONSTANT KBD-INPUT : CARDINAL =	 1;

(* start of "gobbled" functions. All function codes between
 * KBD-GBL-START and KBD-GBL-END are gobbled. To gobble means
 * that, if one such function is found in the input stream,
 * then all immediately following such functions are thrown away.
 *)
CONSTANT KBD-GBL-START : CARDINAL = 0x100;

		(* Cursor (insert point) moving commands *)
		(* There are more cursor moving commands defined later *)
CONSTANT CSR-HOME : CARDINAL =  		0x100;	(* R11 - Top of column  *)
CONSTANT CSR-UP : CARDINAL =    		0x101;	(* R8,  ^p *)
CONSTANT CSR-DOWN : CARDINAL =  		0x102;	(* R14, ^n *)
CONSTANT CSR-RIGHT : CARDINAL = 		0x103;	(* R12, ^f *)
CONSTANT CSR-LEFT : CARDINAL =  		0x104;	(* R10, ^b *)
CONSTANT CSR-BOL : CARDINAL =   		0x105;	(* ^a - Beginning of line *)
CONSTANT CSR-EOL : CARDINAL =   		0x106;	(* ^e - End of line       *)
CONSTANT CSR-BOW : CARDINAL =			0x107;	(* ~b - Previous word start *)
CONSTANT CSR-EOW : CARDINAL =			0x108;	(* ~f - Next word end *)
CONSTANT CSR-BOS : CARDINAL =     	0x109;  	(* ~a - Previous sentence start *)
CONSTANT CSR-EOS : CARDINAL =     	0x10A;	(* ~e - Next sentence end *)
CONSTANT CSR-BOP : CARDINAL =			0x10B;	(* ~[ - Previous paragraph start *)
CONSTANT CSR-EOP : CARDINAL =			0x10C;	(* ~] - Next paragraph end *)
CONSTANT CSR-TOTR : CARDINAL =		0x10D;	(* UNUSED  RECYCLE THIS VALUE *)
CONSTANT CSR-BOTR : CARDINAL =		0x10E;	(* sR11 - Bottom of column *)
CONSTANT CSR-TOP : CARDINAL =			0x10F;	(* ~{ - Beginning of flow *)
CONSTANT CSR-BOT : CARDINAL =			0x110;	(* ~} - End of flow *)

		(* Deletion commands *)
		(* There are more deletion commands defined later *)
CONSTANT DEL-CHARBWD : CARDINAL =		0x112;	(* Delete, BackSpace, ^h - Delete *)
								(* backward one character *)
CONSTANT DEL-CHARFWD : CARDINAL =		0x113;	(* ^d - Delete forward one character *)
CONSTANT DEL-BOW : CARDINAL =			0x114;	(* ~Delete, ~BackSpace, ~^h - Delete *)
								(* backward to previous word end *)
CONSTANT DEL-EOW : CARDINAL =			0x115;	(* ~d - Delete forward to next word start *)
CONSTANT DEL-EOL : CARDINAL =     	0x116;	(* ^k - Delete forward to end of line *)
CONSTANT DEL-EOS : CARDINAL =			0x117;	(* ~k - Delete forward to next sentence end *)
CONSTANT DEL-SEL : CARDINAL =			0x118;	(* see DEL-CHARBWD, Clear shortcut *)
CONSTANT DEL-BOL : CARDINAL =			0x119;	(*  ^u - Delete backward to start of line *)

		(* For dialogs *)
CONSTANT CSR-BOXTOP : CARDINAL =		0x11A;	(* ~R8 - Top of scroll/roll box *)
CONSTANT CSR-BOXBOT : CARDINAL =		0x11B;	(* ~R14 - Bottom of scroll/roll box *)		

		(* Kerning commands *)
CONSTANT KBD-KERNUP : CARDINAL = 		0x11A;	(* ~R8, ^R8   - Move 1 point up *)
CONSTANT KBD-KERNDOWN : CARDINAL = 	0x11B;	(* ~R14, ^R14 - Move 1 point down *)
CONSTANT KBD-KERNLEFT : CARDINAL = 	0x11C;	(* ~R10, ^R10 - Move 1 point left *)
CONSTANT KBD-KERNRIGHT : CARDINAL =	0x11D;	(* ~R12, ^R12 - Move 1 point right*)
CONSTANT KBD-KERNHOME : CARDINAL = 	0x11E;	(* ~R11, ^R11 - Move back to baseline *)

CONSTANT KBD-KERNUP6 : CARDINAL =     0x121;   (* sR8, SHIFT~R8   - Move 6 point up     *)
CONSTANT KBD-KERNDOWN6 : CARDINAL =   0x122;   (* sR14, SHIFT~R14 - Move 6 point down   *)
CONSTANT KBD-KERNLEFT6 : CARDINAL =   0x123;   (* sR10, SHIFT~R10 - Move 6 point left   *)
CONSTANT KBD-KERNRIGHT6 : CARDINAL =  0x124;   (* sR12, SHIFT~R12 - Move 6 point right  *)

		(* Cursor (insert point) moving commands *)
		(* There are more cursor moving commands defined earlier *)
CONSTANT CSR-NEXT-BOW : CARDINAL =	0x140;	(* !bw - Next word start *)
CONSTANT CSR-NEXT-BOS : CARDINAL =   	0x141;  	(* !bs - Next sentence start *)
CONSTANT CSR-NEXT-BOP : CARDINAL =	0x142;	(* !bp - Next paragraph start *)

		(* Deletion commands *)
		(* There are more deletion commands defined earlier *)
CONSTANT DEL-WORD-START : CARDINAL =	0x160;	(* !kb - Delete backward to previous word start *)
CONSTANT DEL-WORD-END : CARDINAL =	0x161;	(* !kf - Delete forward to next word end *)
CONSTANT DEL-NEXT-SS : CARDINAL =		0x162;	(* !ks - Delete forward to next sentence start *)
CONSTANT DEL-BOS : CARDINAL =			0x163;	(* !ka - Delete backward to previous sentence end *)

CONSTANT KBD-GBL-END : CARDINAL = 0x1FF;

		(* Misc. editing commands *)
CONSTANT KBD-BACKTAB : CARDINAL =		0x220;	(* sTab - Back tab (as in dialogs) *)
CONSTANT KBD-SHFTSPACE : CARDINAL =	0x221;	(* sSpace - Shift Space *)
CONSTANT KBD-FIRSTTAB : CARDINAL =	0x222;	(* ~Tab - First tab (as in dialogs) *)

CONSTANT KBD-XCHARS : CARDINAL =	0x224;	(* ^t, ~t - transpose characters (eXchange) *)

CONSTANT KBD-SOFTHYPHEN : CARDINAL =	0x225;	(* !-d, ^- - discretionary hyphen *)
CONSTANT KBD-DONTHYPHEN : CARDINAL =	0x226;   (* !ns, s~- - suppress hyphenation *)
CONSTANT KBD-HARDHYPHEN : CARDINAL =	0x227;	(* !-h, ~- - nonbreaking hyphen *)

CONSTANT KBD-HARDSPACE : CARDINAL =	0x228;	(* ![Space]h, ~[Space] - hard space (not word delimeter) *)
CONSTANT KBD-HARDRETURN : CARDINAL =	0x229;	(* ~Return - Hard return *)

CONSTANT KBD-NUMSPACE : CARDINAL =	0x22A;	(* ![Space]1 - number space *)
CONSTANT KBD-THINSPACE : CARDINAL =	0x22B;	(* ![Space]t - thin space = 1/12 em *)
CONSTANT KBD-EMSPACE : CARDINAL =	0x22C;	(* ![Space]m - em space = 1 em *)
CONSTANT KBD-ENSPACE : CARDINAL =	0x22D;	(* ![Space]n - en space = 1/2 em *)

CONSTANT KBD-OPENLINE : CARDINAL =	0x22E;	(* ^o - Open line (same as Return L10) *)

		(* Search and Replace commands *)
CONSTANT KBD-FPREV : CARDINAL =		0x230;	(* ~^r, !sp - Search backward *)
CONSTANT KBD-FNEXT : CARDINAL =		0x231;	(* ~^s, !sn, L9 - Search forward *)
CONSTANT KBD-RONCE : CARDINAL =		0x232;	(* ^% , !ro - Change*)
CONSTANT KBD-RGLOBAL : CARDINAL =		0x233;	(*      !rg - Change all*)
CONSTANT KBD-RANDF : CARDINAL =  		0x234;	(*      !ra - Replace and find again *)
CONSTANT KBD-SETSEARCH : CARDINAL =	0x235;   (* 		!ss - Display Set Search dialog *)

		(* Highlighting commands *)
		(* There are more highlighting commands defined later *)
CONSTANT HIGH-CHAR : CARDINAL =       0x240;	(* !hc - Highlight next character *)
CONSTANT HIGH-WORD : CARDINAL =       0x241;	(* !hw - Highlight next word *)
CONSTANT HIGH-LINE : CARDINAL =       0x242;	(* !hl - Highlight next line *)
CONSTANT HIGH-SENT : CARDINAL =       0x243;	(* !hs - Highlight next sentence *)
CONSTANT HIGH-PGF : CARDINAL =        0x244;	(* !hp - Highlight next paragraph *)
CONSTANT HIGH-SHL : CARDINAL =        0x245;	(* !hb - Shift highlighting left 1 char *)
CONSTANT HIGH-SHR : CARDINAL =        0x246;	(* !hf - Shift highlighting right 1 char *)
CONSTANT HIGH-CLEAR : CARDINAL =	0x247;	(* !h0 - Clear Selection *)

		(* Misc. control commands, some are also on menus *)
CONSTANT KBD-ABORT : CARDINAL =  		0x250;	(* ^c  - Abort process like import or RGOLBAL *)
CONSTANT KBD-CAPTURE : CARDINAL =		0x251;	(*     - Capture portion of screen, not used  *)
CONSTANT KBD-ECAPTURE : CARDINAL =	0x256;	(* !dc - Capture portion of screen, compressed*)
CONSTANT KBD-RECORD : CARDINAL = 		0x252;	(* ^]  - Record keystrokes                    *)
CONSTANT KBD-GETTRIGGER : CARDINAL =	0x253;	(* ^]  - Get trigger for recorded keystrokes  *)

		(* Character attribute commands *)
CONSTANT TXT-BOLD : CARDINAL =        0x260;	(* !cb, F2 - Set chars to bold *)
CONSTANT TXT-ITALIC : CARDINAL =      0x261;	(* !ci, F3 - Set chars to italic *)
CONSTANT TXT-UNDERLINE : CARDINAL =   0x262;	(* !cu, F4 - Set chars to underline *)
CONSTANT TXT-PLAIN : CARDINAL =       0x263;	(* !cp, F1 - Set chars to plain *)
CONSTANT TXT-SUPER : CARDINAL =       0x264;	(* !c+ - Set chars to superscript *)
CONSTANT TXT-SUB : CARDINAL =         0x265;	(* !c- - Set chars to subscript *)
CONSTANT TXT-NORMAL : CARDINAL =      0x266;	(* !c= - Set chars to normal *)
CONSTANT TXT-INCSIZE : CARDINAL =     0x267;	(* !c], !+s - Increment text size *)
CONSTANT TXT-DECSIZE : CARDINAL =     0x268;	(* !c[, !-s - Decrement text size *)
CONSTANT TXT-SQUEEZE : CARDINAL =		0x269;	(* !cR10  - Squeeze spacing 20% of an em *)
CONSTANT TXT-SPREAD : CARDINAL = 		0x26A;	(* !cR12  - Spread spacing 20% of an em *)
CONSTANT TXT-BAM : CARDINAL =         0x26B;	(* !cc - Set to font dialog values, no dialog *)
CONSTANT TXT-STRIKEOUT : CARDINAL =	0x26C;	(* !cs, F5 - Set chars to strikethrough *)
CONSTANT TXT-DEFAULT : CARDINAL =		0x26D;	(* !cd - Set chars to default pgf font *)
CONSTANT TXT-OVERLINE : CARDINAL =	0x26E;	(* !co - Set chars to overline *)
CONSTANT TXT-CHANGEBAR : CARDINAL =	0x26F;	(* !ch - Set chars to change bar *)
CONSTANT TXT-KERN : CARDINAL =		0x270;	(* !ck - Set chars to kerned *)
CONSTANT TXT-OUTLINE : CARDINAL =	0x271;	(* !ct - Set chars to outline *)
CONSTANT TXT-SHADOW : CARDINAL =	0x272;	(* !ca - Set chars to shadow *)
CONSTANT TXT-MINICAPS : CARDINAL =	0x273;	(* !cm - Set chars to small caps *)

		(* Paragraph attributes and justification commands *)
CONSTANT PGF-INCLINE : CARDINAL =     0x280;	(* !j+, !+l - Increment line spacing *)
CONSTANT PGF-DECLINE : CARDINAL =     0x281;	(* !j-, !-l - Decrement line spacing *)
CONSTANT PGF-CENTER : CARDINAL =      0x282;	(* !jc - Center paragraph *)
CONSTANT PGF-LEFT : CARDINAL =        0x283;	(* !jl - Left justify paragraph *)
CONSTANT PGF-RIGHT : CARDINAL =       0x284;	(* !jr - Right justify paragraph *)
CONSTANT PGF-FULL : CARDINAL =        0x285;	(* !jf - Full justify paragraph *)
CONSTANT PGF-BAM : CARDINAL =         0x286;	(* !jj - Set to pgf dialog values, no dialog *)
CONSTANT PGF-LINEFIX : CARDINAL =		0x287;	(* !jx - Fixed line spacing *)
CONSTANT PGF-LINEFLOAT : CARDINAL =	0x288;	(* !jo - Floating line spacing *)
CONSTANT PGF-UNIFY : CARDINAL =		0x289;	(* !jU - Make all pgfs with current *)
								(* pgf's tag match current pgf's fmt *)

		(* Alignment commands *)
CONSTANT KBD-ALIGN-TOP : CARDINAL =		0x290;	(* !jt - Top align *)
CONSTANT KBD-ALIGN-MIDDLE : CARDINAL =	0x291;	(* !jm - Top/bottom (middle) align *)
CONSTANT KBD-ALIGN-BOTTOM : CARDINAL =	0x292;	(* !jb - Bottom align *)

		(*  Main window commands, some are also on menus   *)
CONSTANT KBD-NEW : CARDINAL =         0x300;	(*       !dn *)
CONSTANT KBD-OPEN : CARDINAL =        0x301;	(* ^x^f, !do *)
CONSTANT KBD-HELP : CARDINAL =        0x302;	(* ~?,   !dh *)
CONSTANT KBD-INFO : CARDINAL =        0x303;	(*       !dI *)

		(* Book kit file menu commands *)
CONSTANT KBD-BOOKADDFILE : CARDINAL = 		0x30A;	(* !df *)
CONSTANT KBD-BOOKEDITDEFINE : CARDINAL =		0x30B;	(* !dd *)
CONSTANT KBD-BOOKEDITFILELIST : CARDINAL =	0x30C;	(* !de *)

		(* File menu commands *)
(*		KBD-NEW			defined in main window commands !dn *)
(*		KBD-OPEN		defined in main window commands !do *)
CONSTANT KBD-SAVE : CARDINAL =        0x310;	(* ^x^s, !ds *)
CONSTANT KBD-SAVEAS : CARDINAL =      0x311;	(* ^x^w, !da *)
CONSTANT KBD-REVERT : CARDINAL =		0x312;	(*       !dr *)
CONSTANT KBD-PRINT : CARDINAL =       0x313;	(*       !dp *)
CONSTANT KBD-IMPORT : CARDINAL =      0x314;	(*       !di *)
CONSTANT KBD-GENERATE : CARDINAL =    0x315;	(*       !dg *)
CONSTANT KBD-USEFMTFROM : CARDINAL =  0x316;	(* 		 !du *)
CONSTANT KBD-KBMACRO : CARDINAL =		0x317;	(* 		 !dk *)
(*		KBD-CAPTURE		defined in misc. control commands !dc *)
CONSTANT KBD-SESSION : CARDINAL =		0x318;	(* !dP *)
(*		KBD-QUITWIN		defined in window menu commands !dq *)

CONSTANT KBD-PAGESETUP : CARDINAL = 	0x319;   (* page setup for Mac *)

		(* File menu commands access by shift *)
CONSTANT KBD-OPENALL : CARDINAL =		0x31A;	(* !dO - Open all *)
CONSTANT KBD-QUITALL : CARDINAL =		0x31B;	(* !dQ - Quit all *)
CONSTANT KBD-SAVEALL : CARDINAL =		0x31C;	(* !dS - Save all *)
CONSTANT KBD-REPEATNEW : CARDINAL =	0x31D;	(* !dN - Repeat last new command *)

		(* Edit menu commands *)
CONSTANT KBD-UNDO : CARDINAL =        0x320;	(*     !eu, L4 *)
CONSTANT KBD-CUT : CARDINAL =         0x321;	(* ^w, !ex     *)
CONSTANT KBD-COPY : CARDINAL =        0x322;	(* ~w, !ec     *)
CONSTANT KBD-PASTE : CARDINAL =       0x323;	(* ^y, !ep	   *)
CONSTANT KBD-CLEAR : CARDINAL =       0x324;   (*     !eb     *)
CONSTANT KBD-COPYFONT : CARDINAL =	0x325;	(* 	   !ef - Copy font attr *)
CONSTANT KBD-COPYPGF : CARDINAL =		0x326;	(* 	   !er - Copy pgf  attr *)
CONSTANT KBD-SELECTALL : CARDINAL =   0x327;	(*     !ea     *)
CONSTANT KBD-STUFF : CARDINAL =       0x328;	(* ~y, !i	   *)
CONSTANT KBD-SEARCH : CARDINAL =      0x329;	(* ^s, !es     *)
CONSTANT KBD-SPELLING : CARDINAL =	0x32A;	(*	   !el     *)
CONSTANT KBD-CAPITAL : CARDINAL =		0x32B;	(*     !eC     *)

CONSTANT KBD-PUT : CARDINAL =			0x32C;	(* L6 -	put on suntools shelf *)
CONSTANT KBD-GET : CARDINAL =			0x32D;	(* L8 - get from shelf	      *)
CONSTANT KBD-PUTCUT : CARDINAL =		0x32E;	(* L10 - put on suntools shelf and cut *)

CONSTANT KBD-ALLCAP : CARDINAL =		0x33A;	(* ~u - convert selected text to cap *)
CONSTANT KBD-ALLLOWER : CARDINAL =	0x33B;	(* ~l - convert selected text to lower case *)
CONSTANT KBD-INITCAP : CARDINAL =		0x33C;	(* ~c - convert selected text to initial cap *)

		(* Format menu commands *)
CONSTANT KBD-FONTDESIGN : CARDINAL = 		0x330;	(* !fc     *)
CONSTANT KBD-PGFDESIGN : CARDINAL =  		0x331;	(* !fp, L3 *)
CONSTANT KBD-COLPRO : CARDINAL =      	0x332;	(* !fl     *)
CONSTANT KBD-DOCUMENTPRO : CARDINAL =	   	0x333;	(* !fd     *)
CONSTANT KBD-CBARPRO : CARDINAL =	    	0x334;	(* !fb     *)
CONSTANT KBD-FOOTNOTEPRO : CARDINAL =    	0x335;	(* !fn     *)
CONSTANT KBD-EQUATION : CARDINAL =        0x336;   (* !fe     *)

CONSTANT KBD-FONTCATALOG : CARDINAL = 	0x337;
CONSTANT KBD-PGFCATALOG : CARDINAL =  	0x338;

		(* Page menu commands *)
CONSTANT KBD-FIRSTPAGE : CARDINAL =   0x340;	(* !pf, ~<, ~F6 *)
CONSTANT KBD-LASTPAGE : CARDINAL =    0x341;	(* !pl, ~>, ~F7 *)
CONSTANT KBD-BODYPAGE : CARDINAL =	0x342;	(* !pb *)
CONSTANT KBD-MASTERPAGE : CARDINAL =	0x343;	(* !pm *)
CONSTANT KBD-REFPAGE : CARDINAL =		0x344;	(* !pr *)
CONSTANT KBD-GOTOPAGE : CARDINAL =    0x345;	(* !pg, ^g*)
CONSTANT KBD-ADDPAGE : CARDINAL =     0x346;	(* !pa *)
CONSTANT KBD-DELETEPAGE : CARDINAL =  0x347;	(* !pd *)
CONSTANT KBD-COLLAYOUT : CARDINAL =   0x348;	(* !pt *) 
CONSTANT KBD-CONNECT : CARDINAL =     0x349;	(* !pc *)
CONSTANT KBD-PAGEBACK : CARDINAL = 	0x34A;	(* !pk *)
CONSTANT KBD-FREEZE : CARDINAL =		0x34B;   (* !pz *)

CONSTANT KBD-PREVPAGE : CARDINAL =    0x34C;	(* !pp, ~v, F6 *)
CONSTANT KBD-NEXTPAGE : CARDINAL =    0x34D;	(* !pn, ^v, F7 *)

CONSTANT KBD-ROTPAGE-PLUS : CARDINAL =	0x34E;	(* !po *)
CONSTANT KBD-ROTPAGE-MINUS : CARDINAL =	0x34F;	(* !pO *)

CONSTANT KBD-SPLIT : CARDINAL =		0x35A;	(* !CS - Split column below IP *)
CONSTANT KBD-CONNECTCOL : CARDINAL =	0x35B;	(* !CC - Connect columns *)
CONSTANT KBD-CUTHEAD : CARDINAL =		0x35C;	(* !CH - Cut head *)
CONSTANT KBD-CURTAIL : CARDINAL =		0x35D;	(* !CT - Cut tail *)

		(* Special menu commands *)
CONSTANT KBD-ANCHOR : CARDINAL =      0x350;	(* !sa *)
CONSTANT KBD-FOOTNOTE : CARDINAL =	0x351;	(* !sf *)
CONSTANT KBD-REFERENCE : CARDINAL =	0x352;	(* !sc *)
CONSTANT KBD-VARIABLE : CARDINAL =	0x353;	(* !sv *)
CONSTANT KBD-INSET : CARDINAL =		0x354;	(* !si *)
CONSTANT KBD-MARKERS : CARDINAL =     0x355;	(* !sm *)

CONSTANT KBD-NEWMARKER : CARDINAL =	0x356;	(* !mk - Insert new marker *)

		(* View menu commands *)
CONSTANT KBD-OPTIONS : CARDINAL =		0x360;	(* !vo *)
CONSTANT KBD-BORDERS : CARDINAL =     0x361;	(* !vb *)
CONSTANT KBD-SYMBOLS : CARDINAL =     0x362;	(* !vt *)
CONSTANT KBD-RULERS : CARDINAL =     	0x363;	(* !vr *)
CONSTANT KBD-GRID : CARDINAL =     	0x364;	(* !vg *)
CONSTANT KBD-SEPARATIONS : CARDINAL = 0x365;	(* !vs *)

CONSTANT KBD-TOGGLEDRAW : CARDINAL =	0x366;	(* !vv - Toggle draw/don't draw preference *)

CONSTANT KBD-VIEWSEP1 : CARDINAL =    0x36D;	(* !v1 *) 
CONSTANT KBD-VIEWSEP2 : CARDINAL =    0x36E;	(* !v2 *)
CONSTANT KBD-VIEWSEP3 : CARDINAL =    0x36F;	(* !v3 *)

		(* Graphics menu commands *)
CONSTANT KBD-FLIPUD : CARDINAL =			0x370;	(* !gv *)
CONSTANT KBD-FLIPLR : CARDINAL =			0x371;	(* !gh *)
CONSTANT KBD-ROT-PLUS : CARDINAL =		0x372;	(* !gt *)
CONSTANT KBD-SCALE : CARDINAL =       	0x373;	(* !gz *)
CONSTANT KBD-SMOOTH : CARDINAL =      	0x374;	(* !gs *)
CONSTANT KBD-UNSMOOTH : CARDINAL =    	0x375;	(* !gm *)
CONSTANT KBD-RESHAPE : CARDINAL =     	0x376;	(* !gr *)
CONSTANT KBD-SETSIDES : CARDINAL =    	0x378;	(* !gn *)
CONSTANT KBD-CONSTRAIN : CARDINAL =   	0x379;
CONSTANT KBD-SNAP : CARDINAL =        	0x37A;	(* !gp *)
CONSTANT KBD-GRAVITY : CARDINAL =     	0x37B;	(* !gy *)
CONSTANT KBD-KEEPTOOL : CARDINAL =    	0x37C;
CONSTANT KBD-FRONT : CARDINAL =       	0x380;	(* !gf *)
CONSTANT KBD-BACK : CARDINAL =        	0x381;	(* !gb *)
CONSTANT KBD-GROUP : CARDINAL =       	0x382;	(* !gg *)
CONSTANT KBD-UNGROUP : CARDINAL =     	0x383;	(* !gu *)
CONSTANT KBD-ALIGN : CARDINAL =       	0x384;	(* !ga *)
CONSTANT KBD-DISTRIBUTE : CARDINAL =  	0x385;	(* !gd *)
CONSTANT KBD-ROT-MINUS : CARDINAL =		0x386;	(* !gT *)
CONSTANT KBD-OBJPROPS : CARDINAL =		0x387;	(* !go *)
CONSTANT KBD-PICKOBJPROPS : CARDINAL =	0x388;	(* !gO *)

CONSTANT KBD-ROTATE : CARDINAL =				0x389;   (* !xx1 *)
CONSTANT KBD-ROTATE-INTERACTIVE : CARDINAL =  0x38A;   (* !xx2 *)

		(* Window menu commands  *)
CONSTANT KBD-CLOSEWIN : CARDINAL =    0x390;   (* !wc       *)
CONSTANT KBD-OPENWIN : CARDINAL =     0x391;	(* !wo       *)
CONSTANT KBD-CLOPWIN : CARDINAL =     0x392;   (* L7 - Close/open toggle *)  
CONSTANT KBD-MOVEWIN : CARDINAL =     0x393;   (* !wm       *)
	(* 0x394; UNUSED *)
CONSTANT KBD-EXPOSEWIN : CARDINAL =   0x395;   (* !we       *)
CONSTANT KBD-HIDEWIN : CARDINAL =     0x396;   (* !wh, sL5  *)
CONSTANT KBD-HISHWIN : CARDINAL =     0x397;	(* L5 - Hide/show toggel *)
CONSTANT KBD-REFRESHWIN : CARDINAL =  0x398;   (* !wr, ^l   *)
CONSTANT KBD-QUITWIN : CARDINAL =     0x399;   (* !wq, !dq, ^x^c *)

		(* Only used internally to resize document toggling *)
		(* between lock and unlock. *)
CONSTANT KBD-RESIZELOCK : CARDINAL =		0x39A;
CONSTANT KBD-RESIZEUNLOCK : CARDINAL =	0x39B;

		(* Only used internally *)  
CONSTANT KBD-QUIETCLOSEWIN : CARDINAL =   0x39C;

    (* Bring up the Popup Menu (ala right button), in X-Motif port *)
CONSTANT KBD-POPUP-MENU : CARDINAL =      0x39D;

		(* Tools window tools *)
CONSTANT KBD-LINETOOL : CARDINAL =    0x3A0;   (* !1l  *)
CONSTANT KBD-RECTTOOL : CARDINAL =    0x3A1;   (* !1r  *)
CONSTANT KBD-POLYGTOOL : CARDINAL =   0x3A2;   (* !1pg *)
CONSTANT KBD-POLYLTOOL : CARDINAL =   0x3A3;   (* !1pl *)
CONSTANT KBD-ARCTOOL : CARDINAL =     0x3A4;   (* !1a  *)
CONSTANT KBD-ROUNDRECT : CARDINAL =   0x3A5;   (* !1R  *)
CONSTANT KBD-OVALTOOL : CARDINAL =    0x3A6;   (* !1e   ("ellipse") *)
CONSTANT KBD-TEXTLTOOL : CARDINAL =   0x3A7;   (* !1tl *)
CONSTANT KBD-TEXTRTOOL : CARDINAL =   0x3A8;   (* !1tc *)
CONSTANT KBD-FREETOOL : CARDINAL =    0x3A9;   (* !1f  *)
CONSTANT KBD-FRAMETOOL : CARDINAL =   0x3AA;   (* !1m  *)
CONSTANT KBD-LASTTOOL : CARDINAL =    0x3AB;   (* !11  select last-used tool  *)
 
        (* Line width commands *)
CONSTANT KBD-WIDTH0 : CARDINAL =      0x3AC;   (* !0w - Set to thinnest width  *)
CONSTANT KBD-WIDTH1 : CARDINAL =      0x3AD;   (* !9w - Set to thickest width   *)
CONSTANT KBD-INCWIDTH : CARDINAL =    0x3AE;   (* !+w - Increment line width   *)
CONSTANT KBD-DECWIDTH : CARDINAL =    0x3AF;   (* !-w - Decrement line width   *)
 
        (* Pen pattern commands *)
CONSTANT KBD-PEN0 : CARDINAL =        0x3B0;   (* !0p - Set to "first" pen pat *)
CONSTANT KBD-PEN1 : CARDINAL =        0x3B1;   (* !9p - Set to last pen  pattern*)
CONSTANT KBD-INCPEN : CARDINAL =      0x3B2;   (* !+p - Increment pen pattern  *)
CONSTANT KBD-DECPEN : CARDINAL =      0x3B3;   (* !-p - Decrement pen pattern  *)
 
        (* Fill pattern commands *)
CONSTANT KBD-FILL0 : CARDINAL =       0x3B4;   (* !0f - Set to "first" fill pat*)
CONSTANT KBD-FILL1 : CARDINAL =       0x3B5;   (* !9f - Set to last fill pattern*)
CONSTANT KBD-INCFILL : CARDINAL =     0x3B6;   (* !+f - Increment fill pattern *)
CONSTANT KBD-DECFILL : CARDINAL =     0x3B7;   (* !-f - Decrement fill pattern *)
 
        (* This cmds are never issued by keyboard only by graphics palette *)
CONSTANT KBD-SETFILL : CARDINAL =      0x3B8;
CONSTANT KBD-SETPEN : CARDINAL =       0x3B9;
CONSTANT KBD-SETWIDTH : CARDINAL =     0x3BA;
CONSTANT KBD-SETCAP : CARDINAL =       0x3BB;
CONSTANT KBD-SETSEP : CARDINAL =       0x3BC;

		(* Spelling checker commands *)
CONSTANT KBD-CHECKSEL : CARDINAL =	0x3C0;	(* !ls  check selection		*)
CONSTANT KBD-CHECKDOC : CARDINAL =	0x3C1;	(* !le  check entire doc	*)
CONSTANT KBD-CORRECT : CARDINAL =		0x3C2;	(* !lcw correct word		*)
CONSTANT KBD-ADDUSRDICT : CARDINAL =	0x3C3;	(* !lap add to personal dict	*)
CONSTANT KBD-ADDDOCDICT : CARDINAL =	0x3C4;	(* !lad add to document dict	*)
CONSTANT KBD-ADDAUTOCORR : CARDINAL = 0x3C5;	(* !lac add to auto corrections *)
CONSTANT KBD-DELUSRDICT : CARDINAL =	0x3C6;	(* !lxp del from personal dict	*)
CONSTANT KBD-DELDOCDICT : CARDINAL =	0x3C7;	(* !lxd del from document dict	*)
CONSTANT KBD-CLEARAUTO : CARDINAL =	0x3C8;   (* !lca clear auto corrections  *)
CONSTANT KBD-CHANGEDICT : CARDINAL =	0x3C9;	(* !lcd change dictionaries	*)
CONSTANT KBD-SPELLRESET : CARDINAL =  0x3CA;	(* !lr  reset checked pgfs	*)
CONSTANT KBD-CHECKPAGE : CARDINAL =	0x3CB;	(* !lp  check page		*)
CONSTANT KBD-SPOPTIONS : CARDINAL =	0x3CC;   (* !lo  spell check options *)
CONSTANT KBD-HYPHENATE : CARDINAL =	0x3CD;   (* !l-  hyphenate word *)
CONSTANT KBD-CHECKBATCH : CARDINAL =  0x3CE;   (* !lb  batch spell check *)
CONSTANT KBD-REFORMATDOC : CARDINAL = 0x3CF;   (* !lR  reformat entire document *)

        (* Smart quotes *)
CONSTANT KBD-SINGLE-QUOTE : CARDINAL =    0x400;   (* '    *)
CONSTANT KBD-DOUBLE-QUOTE : CARDINAL =    0x401;   (* "    *)

		(* Highlighting commands *)
		(* There are more highlighting commands defined earlier *)
CONSTANT HIGH-CHAR-PREV : CARDINAL =  	0x410;	(* !HC - Extend one character to the left *)
CONSTANT HIGH-WORD-PREV : CARDINAL =  	0x411;	(* !hHW - Select current word, then extend to previous one *)
CONSTANT HIGH-LINE-PREV : CARDINAL =  	0x412;	(* !hHL - Select current line, then extend to previous one *)
CONSTANT HIGH-SENT-PREV : CARDINAL =  	0x413;	(* !hHS - Select current sentence, then extend to previous one *)
CONSTANT HIGH-PGF-PREV : CARDINAL =   	0x414;	(* !hHP - Select current paragraph, then extend to previous one *)

CONSTANT HIGH-LINE-UP : CARDINAL =   		0x415;	(* !hu - Extend one line up *)
CONSTANT HIGH-LINE-DOWN : CARDINAL =   	0x416;	(* !hd - Extend one line down*)
CONSTANT HIGH-COL-TOP : CARDINAL =   		0x417;	(* !ht - Extend to top of column *)
CONSTANT HIGH-COL-BOT : CARDINAL =   		0x418;	(* !hm - Extend to bottom of column *)
CONSTANT HIGH-FLOW-BEG : CARDINAL =   	0x419;	(* !hg - Extend to beginning of flow *)
CONSTANT HIGH-FLOW-END : CARDINAL =		0x41A;	(* !he - Extend to end of flow *)

CONSTANT HIGH-CHAR-NEXT : CARDINAL =  	0x510;	(* !hHc - Extend one character to the right *)
CONSTANT HIGH-WORD-NEXT : CARDINAL =  	0x511;	(* !hHw - Select current word, then extend to next one *)
CONSTANT HIGH-LINE-NEXT : CARDINAL =  	0x512;	(* !hHl - Select current line, then extend to next one *)
CONSTANT HIGH-SENT-NEXT : CARDINAL =  	0x513;	(* !hHs - Select current sentence, then extend to next one *)
CONSTANT HIGH-PGF-NEXT : CARDINAL =   	0x514;	(* !hHp - Select current paragraph, then extend to next one *)

CONSTANT KBD-VIEWER : CARDINAL =		0xF00;	(* !ZZv	toggle viewer brand 	*)
CONSTANT KBD-DSEXIT : CARDINAL =		0xF01;	(* !ZZz	exercise dsexit		*)
CONSTANT KBD-MEMFAIL : CARDINAL =		0xF02;	(* !ZZy	exercise mem-fail	*)
CONSTANT KBD-SAVEMETA : CARDINAL =	0xF03;	(* !ZZs	toggle mode so Save Text saves meta Chars *) 
CONSTANT KBD-MEM-STATS : CARDINAL =	0xF04;	(* !ZZm print busy/free memory totals *)
CONSTANT KBD-CACHE-STATS : CARDINAL =	0xF05;	(* !ZZc print cache statistics *)

CONSTANT KBD-NEWVAR : CARDINAL =		0xF06;	(* new variable @ ip *)
CONSTANT KBD-UPDATEREF : CARDINAL =	0xF07;	(* update ref @ ip *)
CONSTANT KBD-DEREFREF : CARDINAL =	0xF08;	(* dereference ref @ ip *)
CONSTANT KBD-HEATREF : CARDINAL =		0xF09;	(* heat reference @ ip *)

		(* Document right border commands *)
CONSTANT KBD-ALLSELECT : CARDINAL = 	0xF20;   (* !1s - smart selection *)
CONSTANT KBD-OBJSELECT : CARDINAL = 	0xF21;   (* !1o - object selection *)
CONSTANT KBD-TOOLWIN : CARDINAL =     0xF22;   (* !1w - tools window *)
CONSTANT KBD-PGFWIN : CARDINAL =      0xF23;   (* !jw - paragraph catalog window *)
CONSTANT KBD-FONTWIN : CARDINAL =     0xF24;   (* !cw - font catalog window *)
CONSTANT KBD-RESIZEBOX : CARDINAL =   0xF25;	(* window resize box *)
CONSTANT KBD-MATHWIN : CARDINAL =     0xF26;   (* !mw math window *)
CONSTANT KBD-RESIZEBOXM : CARDINAL =  0xF27;	(* window resize box using ctrl-middle mouse*)

		(* Document bottom border commands *)
CONSTANT KBD-ZOOMIN : CARDINAL =			0xF30;	(* !zi - zoom in *)
CONSTANT KBD-ZOOMOUT : CARDINAL = 		0xF31;	(* !zo - zoom out *)
CONSTANT KBD-ZOOM-FIT-PAGE : CARDINAL =  	0xF32;   (* !zp - zoom fit page in window *)
CONSTANT KBD-ZOOM-FIT-WINDOW : CARDINAL = 0xF33;   (* !zw - zoom fit window to page *)
CONSTANT KBD-ZOOM : CARDINAL =			0xF34;   (* zoom *)
CONSTANT KBD-ZOOM100 : CARDINAL =			0xF35;	(* !zz - zoom to 100% *)

CONSTANT KBD-RENAMEPAGE : CARDINAL =		0xF3A;	(* !pN - rename master/reference page *)

		(* Font and paragraph catalog selection quick key. *)
CONSTANT KBD-FONTQUICK : CARDINAL =	0xF40;	(* !qc, F8, ^8 - Char fmt quick key *)
CONSTANT KBD-PGFQUICK : CARDINAL =	0xF41;	(* !qp, F9, ^9 - Pgf fmt quick key *)
CONSTANT KBD-VARQUICK : CARDINAL =	0xF42;	(* !qv,     ^0 - Variable quick key *)

		(* Dialog commands: set all to As Is and reset. *)
CONSTANT KBD-NOCHANGEDB : CARDINAL =	0xF4A;	(* ~F8 - Set all items fo As Is in dialog *)
CONSTANT KBD-RESETDB : CARDINAL =		0xF4B;	(* ~F9 - Reset dialog *)

		(* New Equation commands. *)
CONSTANT KBD-SMEQN : CARDINAL =       	0xF50;	(* !ms - Small equation *)
CONSTANT KBD-MEDEQN : CARDINAL =      	0xF51;	(* !mm - Medium equation *)
CONSTANT KBD-LGEQN : CARDINAL =       	0xF52;	(* !ml - Large equation *)
CONSTANT KBD-PUTINLINE : CARDINAL =       0xF53;	(* !mp - Shrinkwrap *)
CONSTANT KBD-ANTIPUTINLINE : CARDINAL =   0xF54;	(* !me - Expand (unwrap) *)

CONSTANT KBD-EVACUATE : CARDINAL =	0xF60;	(* (unbound) Force assertion botch *)

CONSTANT FM-TERMINATE : CARDINAL =	0xFFF;	(* Quit FrameMaker *)

CONSTANT KBD-MATH-BASE : CARDINAL =	0x1000;

CONSTANT TEXTSEL-QUICK-COPY : CARDINAL =	0x001;
CONSTANT TEXTSEL-EXTEND : CARDINAL =		0x002;
CONSTANT TEXTSEL-EXTEND-WORD : CARDINAL =	0x004;
CONSTANT TEXTSEL-EXTEND-LINE : CARDINAL =	0x008;
CONSTANT TEXTSEL-EXTEND-PGF : CARDINAL =	0x010;
CONSTANT TEXTSEL-WORD : CARDINAL =		0x020;
CONSTANT TEXTSEL-LINE : CARDINAL =		0x040;
CONSTANT TEXTSEL-PGF : CARDINAL =			0x080;
CONSTANT TEXTSEL-SELECT-ONLY : CARDINAL = 0x100;
(* These codes are the bits to set in the saveOptions argument to SaveAsMIF *)
CONSTANT MIF-SAVE-TEXT : CARDINAL =	0x001;
CONSTANT MIF-SAVE-TAGS : CARDINAL =	0x002;
CONSTANT MIF-SAVE-FMTS : CARDINAL =	0x004;
CONSTANT MIF-SAVE-FONTS : CARDINAL =	0x008;
CONSTANT MIF-SAVE-MKRS : CARDINAL =	0x010;
CONSTANT MIF-SAVE-AFMS : CARDINAL =	0x020;
CONSTANT MIF-SAVE-LAYT : CARDINAL =	0x040;
CONSTANT MIF-SAVE-MPAGE : CARDINAL =	0x080;
CONSTANT MIF-SAVE-FCAT : CARDINAL =	0x100;
CONSTANT MIF-SAVE-PCAT : CARDINAL =	0x200;
CONSTANT MIF-SAVE-TMPLT : CARDINAL =	0x400;
CONSTANT MIF-SAVE-DICT : CARDINAL =	0x800;
CONSTANT MIF-SAVE-VARS : CARDINAL =	0x1000;

TYPE Server = CLASS
   (* Sun RPC, Program number 300009, version 1 *)
   METHODS
	DoCommands (doc : INTEGER, commands : Commands) : Error = 1,
	NewDocument (template : ilu.CString) : DocAndError = 100,
	OpenDocument (name : ilu.CString) : DocAndError = 101,
	Import (isCurrentDoc : BOOLEAN, doc : DocIndex, file : ilu.CString, isCopy :BOOLEAN) : Error = 102,
	SaveAsMaker (doc : DocIndex, file : ilu.CString, mode : INTEGER, isMakeBackup : BOOLEAN, saveOptions : INTEGER) : Error = 103,
	SaveAsText (doc : DocIndex, file : ilu.CString, mode : INTEGER, isMakeBackup : BOOLEAN, saveOptions : INTEGER) : Error = 104,
	SaveAsMIF (doc : DocIndex, file : ilu.CString, mode : INTEGER, isMakeBackup : BOOLEAN, saveOptions : INTEGER) : Error = 105,
	ListExternals (doc : DocIndex, file : ilu.CString) : Error = 106,
	Quit() : Error = 107,

	GoToLink (doc : DocIndex, destinationDoc : ilu.CString, tag : ilu.CString, isNewWindow : BOOLEAN) : Error = 202,
	Message (doc : ilu.CString, message : ilu.CString) : Error = 203,

	Launch () : Error = 300,
	EditInset (insetFile : ilu.CString, isInternal : BOOLEAN) : Error = 301,
	UpdateInset (insetFile : ilu.CString) : Error = 302
   END;
	
