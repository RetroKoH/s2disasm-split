; Sonic the Hedgehog 2 disassembled binary

; Nemesis,   2004: Created original disassembly for SNASM68K
; Aurochs,   2005: Translated to AS and annotated
; Xenowhirl, 2007: More annotation, overall cleanup, Z80 disassembly
; ---------------------------------------------------------------------------
; NOTES:
;
; Set your editor's tab width to 8 characters wide for viewing this file.
;
; It is highly suggested that you read the AS User's Manual before diving too
; far into this disassembly. At least read the section on nameless temporary
; symbols. Your brain may melt if you don't know how those work.
;
; See s2.notes.txt for more comments about this disassembly and other useful info.

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; ASSEMBLY OPTIONS:
;
gameRevision = 1
;	| If 0, a REV00 ROM is built
;	| If 1, a REV01 ROM is built, which contains some fixes
;	| If 2, a (theoretical) REV02 ROM is built, which contains even more fixes
padToPowerOfTwo = 1
;	| If 1, pads the end of the ROM to the next power of two bytes (for real hardware)
;
fixBugs = 0
;	| If 1, enables all bug-fixes
;	| See also the 'FixDriverBugs' flag in 's2.sounddriver.asm'
;	| See also the 'FixMusicAndSFXDataBugs' flag in 'build.lua'
allOptimizations = 0
;	| If 1, enables all optimizations
;
skipChecksumCheck = 0
;	| If 1, disables the slow bootup checksum calculation
;
zeroOffsetOptimization = 0|allOptimizations
;	| If 1, makes a handful of zero-offset instructions smaller
;
removeJmpTos = 0|(gameRevision=2)|allOptimizations
;	| If 1, many unnecessary JmpTos are removed, improving performance
;
addsubOptimize = 0|(gameRevision=2)|allOptimizations
;	| If 1, some add/sub instructions are optimized to addq/subq
;
relativeLea = 0|(gameRevision<>2)|allOptimizations
;	| If 1, makes some instructions use pc-relative addressing, instead of absolute long
;
useFullWaterTables = 0
;	| If 1, zone offset tables for water levels cover all level slots instead of only slots 8-$F
;	| Set to 1 if you've shifted level IDs around or you want water in levels with a level slot below 8

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; AS-specific macros and assembler settings
	CPU 68000
	include "s2.macrosetup.asm"

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Simplifying macros and functions
	include "s2.macros.asm"

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Equates section - Names for variables.
	include "s2.constants.asm"

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Expressing SMPS bytecode in a portable and human-readable form
FixMusicAndSFXDataBugs = fixBugs
SonicDriverVer = 2 ; Tell SMPS2ASM that we are targetting Sonic 2's sound driver
	include "sound/_smps2asm_inc.asm"

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Expressing sprite mappings and DPLCs in a portable and human-readable form
SonicMappingsVer := 2
SonicDplcVer := 2
	include "mappings/MapMacros.asm"

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; start of ROM

StartOfRom:
    if * <> 0
	fatal "StartOfRom was $\{*} but it should be 0"
    endif
Vectors:
	dc.l System_Stack	; Initial stack pointer value
	dc.l EntryPoint		; Start of program
	dc.l ErrorTrap		; Bus error
	dc.l ErrorTrap		; Address error (4)
	dc.l ErrorTrap		; Illegal instruction
	dc.l ErrorTrap		; Division by zero
	dc.l ErrorTrap		; CHK exception
	dc.l ErrorTrap		; TRAPV exception (8)
	dc.l ErrorTrap		; Privilege violation
	dc.l ErrorTrap		; TRACE exception
	dc.l ErrorTrap		; Line-A emulator
	dc.l ErrorTrap		; Line-F emulator (12)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (16)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (20)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (24)
	dc.l ErrorTrap		; Spurious exception
	dc.l ErrorTrap		; IRQ level 1
	dc.l ErrorTrap		; IRQ level 2
	dc.l ErrorTrap		; IRQ level 3 (28)
	dc.l H_Int			; IRQ level 4 (horizontal retrace interrupt)
	dc.l ErrorTrap		; IRQ level 5
	dc.l V_Int			; IRQ level 6 (vertical retrace interrupt)
	dc.l ErrorTrap		; IRQ level 7 (32)
	dc.l ErrorTrap		; TRAP #00 exception
	dc.l ErrorTrap		; TRAP #01 exception
	dc.l ErrorTrap		; TRAP #02 exception
	dc.l ErrorTrap		; TRAP #03 exception (36)
	dc.l ErrorTrap		; TRAP #04 exception
	dc.l ErrorTrap		; TRAP #05 exception
	dc.l ErrorTrap		; TRAP #06 exception
	dc.l ErrorTrap		; TRAP #07 exception (40)
	dc.l ErrorTrap		; TRAP #08 exception
	dc.l ErrorTrap		; TRAP #09 exception
	dc.l ErrorTrap		; TRAP #10 exception
	dc.l ErrorTrap		; TRAP #11 exception (44)
	dc.l ErrorTrap		; TRAP #12 exception
	dc.l ErrorTrap		; TRAP #13 exception
	dc.l ErrorTrap		; TRAP #14 exception
	dc.l ErrorTrap		; TRAP #15 exception (48)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (52)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (56)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (60)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved)
	dc.l ErrorTrap		; Unused (reserved) (64)
; byte_100:
Header:
	dc.b "SEGA GENESIS    " ; Console name
	dc.b "(C)SEGA 1992.SEP" ; Copyright holder and release date (generally year)
	dc.b "SONIC THE             HEDGEHOG 2                " ; Domestic name
	dc.b "SONIC THE             HEDGEHOG 2                " ; International name
    if gameRevision=0
	dc.b "GM 00001051-00"   ; Version (REV00)
    elseif gameRevision=1
	dc.b "GM 00001051-01"   ; Version (REV01)
    elseif gameRevision=2
	dc.b "GM 00001051-02"   ; Version (REV02)
    endif
; word_18E
Checksum:
	dc.w $D951		; Checksum (patched later if incorrect)
	dc.b "J               " ; I/O Support
	dc.l StartOfRom		; Start address of ROM
; dword_1A4
ROMEndLoc:
	dc.l EndOfRom-1		; End address of ROM
	dc.l RAM_Start&$FFFFFF		; Start address of RAM
	dc.l (RAM_End-1)&$FFFFFF		; End address of RAM
	dc.b "    "		; Backup RAM ID
	dc.l $20202020		; Backup RAM start address
	dc.l $20202020		; Backup RAM end address
	dc.b "            "	; Modem support
	dc.b "                                        "	; Notes (unused, anything can be put in this space, but it has to be 52 bytes.)
	dc.b "JUE             " ; Country code (region)
EndOfHeader:

; ===========================================================================
; Crash/Freeze the 68000. Note that the Z80 continues to run, so the music keeps playing.
; loc_200:
ErrorTrap:
	nop	; delay
	nop	; delay
	bra.s	ErrorTrap	; Loop indefinitely.

; ===========================================================================
; loc_206:
EntryPoint:
	tst.l	(HW_Port_1_Control-1).l	; test ports A and B control
	bne.s	PortA_Ok	; If so, branch.
	tst.w	(HW_Expansion_Control-1).l	; test port C control
; loc_214:
PortA_Ok:
	bne.s	PortC_OK ; Skip the VDP and Z80 setup code if port A, B or C is ok...?
	lea	SetupValues(pc),a5	; Load setup values array address.
	movem.w	(a5)+,d5-d7
	movem.l	(a5)+,a0-a4
	move.b	HW_Version-Z80_Bus_Request(a1),d0	; Get hardware version
	andi.b	#$F,d0	; Compare
	beq.s	SkipSecurity	; If the console has no TMSS, skip the security stuff.
	move.l	#'SEGA',Security_Addr-Z80_Bus_Request(a1) ; Satisfy the TMSS
; loc_234:
SkipSecurity:
	move.w	(a4),d0	; check if VDP works
	moveq	#0,d0	; clear d0
	movea.l	d0,a6	; clear a6
	move.l	a6,usp	; set usp to $0

	moveq	#VDPInitValues_End-VDPInitValues-1,d1 ; run the following loop $18 times
; loc_23E:
VDPInitLoop:
	move.b	(a5)+,d5	; add $8000 to value
	move.w	d5,(a4)	; move value to VDP register
	add.w	d7,d5	; next register
	dbf	d1,VDPInitLoop

	move.l	(a5)+,(a4)	; set VRAM write mode
	move.w	d0,(a3)	; clear the screen
	move.w	d7,(a1)	; stop the Z80
	move.w	d7,(a2)	; reset the Z80
; loc_250:
WaitForZ80:
	btst	d0,(a1)	; has the Z80 stopped?
	bne.s	WaitForZ80	; if not, branch

	moveq	#Z80StartupCodeEnd-Z80StartupCodeBegin-1,d2
; loc_256:
Z80InitLoop:
	move.b	(a5)+,(a0)+
	dbf	d2,Z80InitLoop

	move.w	d0,(a2)
	move.w	d0,(a1)	; start the Z80
	move.w	d7,(a2)	; reset the Z80

; loc_262:
ClrRAMLoop:
	move.l	d0,-(a6)	; clear 4 bytes of RAM
	dbf	d6,ClrRAMLoop	; repeat until the entire RAM is clear
	move.l	(a5)+,(a4)	; set VDP display mode and increment mode
	move.l	(a5)+,(a4)	; set VDP to CRAM write

	moveq	#bytesToLcnt($80),d3	; set repeat times
; loc_26E:
ClrCRAMLoop:
	move.l	d0,(a3)	; clear 2 palettes
	dbf	d3,ClrCRAMLoop	; repeat until the entire CRAM is clear
	move.l	(a5)+,(a4)	; set VDP to VSRAM write

	moveq	#bytesToLcnt($50),d4	; set repeat times
; loc_278: ClrVDPStuff:
ClrVSRAMLoop:
	move.l	d0,(a3)	; clear 4 bytes of VSRAM.
	dbf	d4,ClrVSRAMLoop	; repeat until the entire VSRAM is clear
	moveq	#PSGInitValues_End-PSGInitValues-1,d5	; set repeat times.
; loc_280:
PSGInitLoop:
	move.b	(a5)+,PSG_input-VDP_data_port(a3) ; reset the PSG
	dbf	d5,PSGInitLoop	; repeat for other channels
	move.w	d0,(a2)
	movem.l	(a6),d0-a6	; clear all registers
	move	#$2700,sr	; set the sr
 ; loc_292:
PortC_OK: ;;
	bra.s	GameProgram	; Branch to game program.
; ===========================================================================
; byte_294:
SetupValues:
	dc.w	$8000,bytesToLcnt($10000),$100

	dc.l	Z80_RAM
	dc.l	Z80_Bus_Request
	dc.l	Z80_Reset
	dc.l	VDP_data_port, VDP_control_port

VDPInitValues:	; values for VDP registers
	dc.b 4			; Command $8004 - HInt off, Enable HV counter read
	dc.b $14		; Command $8114 - Display off, VInt off, DMA on, PAL off
	dc.b $30		; Command $8230 - Scroll A Address $C000
	dc.b $3C		; Command $833C - Window Address $F000
	dc.b 7			; Command $8407 - Scroll B Address $E000
	dc.b $6C		; Command $856C - Sprite Table Address $D800
	dc.b 0			; Command $8600 - Null
	dc.b 0			; Command $8700 - Background color Pal 0 Color 0
	dc.b 0			; Command $8800 - Null
	dc.b 0			; Command $8900 - Null
	dc.b $FF		; Command $8AFF - Hint timing $FF scanlines
	dc.b 0			; Command $8B00 - Ext Int off, VScroll full, HScroll full
	dc.b $81		; Command $8C81 - 40 cell mode, shadow/highlight off, no interlace
	dc.b $37		; Command $8D37 - HScroll Table Address $DC00
	dc.b 0			; Command $8E00 - Null
	dc.b 1			; Command $8F01 - VDP auto increment 1 byte
	dc.b 1			; Command $9001 - 64x32 cell scroll size
	dc.b 0			; Command $9100 - Window H left side, Base Point 0
	dc.b 0			; Command $9200 - Window V upside, Base Point 0
	dc.b $FF		; Command $93FF - DMA Length Counter $FFFF
	dc.b $FF		; Command $94FF - See above
	dc.b 0			; Command $9500 - DMA Source Address $0
	dc.b 0			; Command $9600 - See above
	dc.b $80		; Command $9780	- See above + VRAM fill mode
VDPInitValues_End:

	dc.l	vdpComm($0000,VRAM,DMA) ; value for VRAM write mode

	; Z80 instructions (not the sound driver; that gets loaded later)
Z80StartupCodeBegin: ; loc_2CA:
    save
    CPU Z80 ; start assembling Z80 code
    phase 0 ; pretend we're at address 0
	xor	a	; clear a to 0
	ld	bc,((Z80_RAM_End-Z80_RAM)-zStartupCodeEndLoc)-1 ; prepare to loop this many times
	ld	de,zStartupCodeEndLoc+1	; initial destination address
	ld	hl,zStartupCodeEndLoc	; initial source address
	ld	sp,hl	; set the address the stack starts at
	ld	(hl),a	; set first byte of the stack to 0
	ldir		; loop to fill the stack (entire remaining available Z80 RAM) with 0
	pop	ix	; clear ix
	pop	iy	; clear iy
	ld	i,a	; clear i
	ld	r,a	; clear r
	pop	de	; clear de
	pop	hl	; clear hl
	pop	af	; clear af
	ex	af,af'	; swap af with af'
	exx		; swap bc/de/hl with their shadow registers too
	pop	bc	; clear bc
	pop	de	; clear de
	pop	hl	; clear hl
	pop	af	; clear af
	ld	sp,hl	; clear sp
	di		; clear iff1 (for interrupt handler)
	im	1	; interrupt handling mode = 1
	ld	(hl),0E9h ; replace the first instruction with a jump to itself
	jp	(hl)	  ; jump to the first instruction (to stay there forever)
zStartupCodeEndLoc:
    dephase ; stop pretending
	restore
    padding off ; unfortunately our flags got reset so we have to set them again...
Z80StartupCodeEnd:

	dc.w	$8104	; value for VDP display mode
	dc.w	$8F02	; value for VDP increment
	dc.l	vdpComm($0000,CRAM,WRITE)	; value for CRAM write mode
	dc.l	vdpComm($0000,VSRAM,WRITE)	; value for VSRAM write mode

PSGInitValues:
	dc.b	$9F,$BF,$DF,$FF	; values for PSG channel volumes
PSGInitValues_End:
; ===========================================================================

	even
; loc_300:
GameProgram:
	tst.w	(VDP_control_port).l
; loc_306:
CheckSumCheck:
    if gameRevision>0
	move.w	(VDP_control_port).l,d1
	btst	#1,d1
	bne.s	CheckSumCheck	; wait until DMA is completed
    endif
	btst	#6,(HW_Expansion_Control).l
	beq.s	ChecksumTest
	cmpi.l	#'init',(Checksum_fourcc).w ; has checksum routine already run?
	beq.w	GameInit

; loc_328:
ChecksumTest:
    if skipChecksumCheck=0	; checksum code
	movea.l	#EndOfHeader,a0	; start checking bytes after the header ($200)
	movea.l	#ROMEndLoc,a1	; stop at end of ROM
	move.l	(a1),d0
	moveq	#0,d1
; loc_338:
ChecksumLoop:
	add.w	(a0)+,d1
	cmp.l	a0,d0
	bhs.s	ChecksumLoop
	movea.l	#Checksum,a1	; read the checksum
	cmp.w	(a1),d1	; compare correct checksum to the one in ROM
	bne.w	ChecksumError	; if they don't match, branch
    endif
;checksum_good:
	; Clear some RAM only on a coldboot.
	lea	(CrossResetRAM).w,a6
	moveq	#0,d7

	move.w	#bytesToLcnt(CrossResetRAM_End-CrossResetRAM),d6
-	move.l	d7,(a6)+
	dbf	d6,-

	move.b	(HW_Version).l,d0
	andi.b	#$C0,d0
	move.b	d0,(Graphics_Flags).w
	move.l	#'init',(Checksum_fourcc).w ; set flag so checksum won't be run again
; loc_370:
GameInit:
	; Clear some RAM on every boot and reset.
	lea	(RAM_Start&$FFFFFF).l,a6
	moveq	#0,d7
	move.w	#bytesToLcnt(CrossResetRAM-RAM_Start),d6
; loc_37C:
GameClrRAM:
	move.l	d7,(a6)+
	dbf	d6,GameClrRAM	; clear RAM ($0000-$FDFF)

	bsr.w	VDPSetupGame
	bsr.w	JmpTo_SoundDriverLoad
	bsr.w	JoypadInit
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; set Game Mode to Sega Screen
; loc_394:
MainGameLoop:
	move.b	(Game_Mode).w,d0 ; load Game Mode
	andi.w	#$3C,d0	; limit Game Mode value to $3C max (change to a maximum of 7C to add more game modes)
	jsr	GameModesArray(pc,d0.w)	; jump to apt location in ROM
	bra.s	MainGameLoop	; loop indefinitely
; ===========================================================================
; loc_3A2:
GameModesArray: ;;
GameMode_SegaScreen:	bra.w	SegaScreen		; SEGA screen mode
GameMode_TitleScreen:	bra.w	TitleScreen		; Title screen mode
GameMode_Demo:		bra.w	Level			; Demo mode
GameMode_Level:		bra.w	Level			; Zone play mode
GameMode_SpecialStage:	bra.w	SpecialStage		; Special stage play mode
GameMode_ContinueScreen:bra.w	ContinueScreen		; Continue mode
GameMode_2PResults:	bra.w	TwoPlayerResults	; 2P results mode
GameMode_2PLevelSelect:	bra.w	LevelSelectMenu2P	; 2P level select mode
GameMode_EndingSequence:bra.w	JmpTo_EndingSequence	; End sequence mode
GameMode_OptionsMenu:	bra.w	OptionsMenu		; Options mode
GameMode_LevelSelect:	bra.w	LevelSelectMenu		; Level select mode
; ===========================================================================
    if skipChecksumCheck=0	; checksum error code
; loc_3CE:
ChecksumError:
	move.l	d1,-(sp)
	bsr.w	VDPSetupGame
	move.l	(sp)+,d1
	move.l	#vdpComm($0000,CRAM,WRITE),(VDP_control_port).l ; set VDP to CRAM write
	moveq	#$3F,d7
; loc_3E2:
Checksum_Red:
	move.w	#$E,(VDP_data_port).l ; fill palette with red
	dbf	d7,Checksum_Red	; repeat $3F more times
; loc_3EE:
ChecksumFailed_Loop:
	bra.s	ChecksumFailed_Loop
    endif
; ===========================================================================
; loc_3F0:
LevelSelectMenu2P: ;;
	jmp	(MenuScreen).l
; ===========================================================================
; loc_3F6:
JmpTo_EndingSequence ; JmpTo
	jmp	(EndingSequence).l
; ===========================================================================
; loc_3FC:
OptionsMenu: ;;
	jmp	(MenuScreen).l
; ===========================================================================
; loc_402:
LevelSelectMenu: ;;
	jmp	(MenuScreen).l
; ===========================================================================

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; vertical and horizontal interrupt handlers
; VERTICAL INTERRUPT HANDLER:
V_Int:
	movem.l	d0-a6,-(sp)
	tst.b	(Vint_routine).w
	beq.w	Vint_Lag

-	move.w	(VDP_control_port).l,d0
	andi.w	#8,d0
	beq.s	-

	move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
	move.l	(Vscroll_Factor).w,(VDP_data_port).l ; send screen y-axis pos. to VSRAM
	btst	#6,(Graphics_Flags).w ; is Megadrive PAL?
	beq.s	+		; if not, branch

	move.w	#$700,d0
-	dbf	d0,- ; wait here in a loop doing nothing for a while...
+
	move.b	(Vint_routine).w,d0
	move.b	#VintID_Lag,(Vint_routine).w
	move.w	#1,(Hint_flag).w
	andi.w	#$3E,d0
	move.w	Vint_SwitchTbl(pc,d0.w),d0
	jsr	Vint_SwitchTbl(pc,d0.w)

VintRet:
	addq.l	#1,(Vint_runcount).w
	movem.l	(sp)+,d0-a6
	rte
; ===========================================================================
Vint_SwitchTbl: offsetTable
Vint_Lag_ptr		offsetTableEntry.w Vint_Lag			;   0
Vint_SEGA_ptr:		offsetTableEntry.w Vint_SEGA		;   2
Vint_Title_ptr:		offsetTableEntry.w Vint_Title		;   4
Vint_Unused6_ptr:	offsetTableEntry.w Vint_Unused6		;   6
Vint_Level_ptr:		offsetTableEntry.w Vint_Level		;   8
Vint_S2SS_ptr:		offsetTableEntry.w Vint_S2SS		;  $A
Vint_TitleCard_ptr:	offsetTableEntry.w Vint_TitleCard	;  $C
Vint_UnusedE_ptr:	offsetTableEntry.w Vint_UnusedE		;  $E
Vint_Pause_ptr:		offsetTableEntry.w Vint_Pause		; $10
Vint_Fade_ptr:		offsetTableEntry.w Vint_Fade		; $12
Vint_PCM_ptr:		offsetTableEntry.w Vint_PCM			; $14
Vint_Menu_ptr:		offsetTableEntry.w Vint_Menu		; $16
Vint_Ending_ptr:	offsetTableEntry.w Vint_Ending		; $18
Vint_CtrlDMA_ptr:	offsetTableEntry.w Vint_CtrlDMA		; $1A
; ===========================================================================
;VintSub0
Vint_Lag:
	cmpi.b	#GameModeID_TitleCard|GameModeID_Demo,(Game_Mode).w	; pre-level Demo Mode?
	beq.s	loc_4C4
	cmpi.b	#GameModeID_TitleCard|GameModeID_Level,(Game_Mode).w	; pre-level Zone play mode?
	beq.s	loc_4C4
	cmpi.b	#GameModeID_Demo,(Game_Mode).w	; Demo Mode?
	beq.s	loc_4C4
	cmpi.b	#GameModeID_Level,(Game_Mode).w	; Zone play mode?
	beq.s	loc_4C4

	stopZ80			; stop the Z80
	bsr.w	sndDriverInput	; give input to the sound driver
	startZ80		; start the Z80

	bra.s	VintRet
; ---------------------------------------------------------------------------

loc_4C4:
	tst.b	(Water_flag).w
	beq.w	Vint0_noWater
	move.w	(VDP_control_port).l,d0
	btst	#6,(Graphics_Flags).w
	beq.s	+

	move.w	#$700,d0
-	dbf	d0,- ; do nothing for a while...
+
	move.w	#1,(Hint_flag).w

	stopZ80

	tst.b	(Water_fullscreen_flag).w
	bne.s	loc_526

	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM

	bra.s	loc_54A
; ---------------------------------------------------------------------------

loc_526:
	dma68kToVDP Underwater_palette,$0000,palette_line_size*4,CRAM

loc_54A:
	move.w	(Hint_counter_reserve).w,(a5)
	move.w	#$8200|(VRAM_Plane_A_Name_Table/$400),(VDP_control_port).l	; Set scroll A PNT base to $C000
	bsr.w	sndDriverInput

	startZ80

	bra.w	VintRet
; ---------------------------------------------------------------------------

Vint0_noWater:
	move.w	(VDP_control_port).l,d0
    if ~~fixBugs
	; As with the sprite table upload, this only needs to be done in two-player mode.

	; Update V-Scroll.
	move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
	move.l	(Vscroll_Factor).w,(VDP_data_port).l
    endif
	btst	#6,(Graphics_Flags).w
	beq.s	+

	move.w	#$700,d0
-	dbf	d0,- ; do nothing for a while...
+
	move.w	#1,(Hint_flag).w
	move.w	(Hint_counter_reserve).w,(VDP_control_port).l
	move.w	#$8200|(VRAM_Plane_A_Name_Table/$400),(VDP_control_port).l	; Set scroll A PNT base to $C000
    if ~~fixBugs
	; Does not need to be done on lag frames.
	move.l	(Vscroll_Factor_P2).w,(Vscroll_Factor_P2_HInt).w
    endif

	stopZ80
    if fixBugs
	; In two-player mode, we have to update the sprite table
	; even during a lag frame so that the top half of the screen
	; shows the correct sprites.
	tst.w	(Two_player_mode).w
	beq.s	++

	; Update V-Scroll.
	move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
	move.l	(Vscroll_Factor).w,(VDP_data_port).l

	; Like in Sonic 3, the sprite tables are page-flipped in two-player mode.
	; This fixes a race-condition where incomplete sprite tables can be uploaded
	; to the VDP on lag frames, causing corrupted sprites to appear.

	; Upload the front buffer.
	tst.b	(Current_sprite_table_page).w
	beq.s	+
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	bra.s	++
+
	dma68kToVDP Sprite_Table_Alternate,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
+
    else
	; In the original game, the sprite table is needlessly updated on lag frames.
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
    endif
	bsr.w	sndDriverInput
	startZ80

	bra.w	VintRet
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; This subroutine copies the H scroll table buffer (in main RAM) to the H scroll
; table (in VRAM).
;VintSub2
Vint_SEGA:
	bsr.w	Do_ControllerPal

	dma68kToVDP Horiz_Scroll_Buf,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM
	jsrto	SegaScr_VInt, JmpTo_SegaScr_VInt
	tst.w	(Demo_Time_left).w	; is there time left on the demo?
	beq.w	+	; if not, return
	subq.w	#1,(Demo_Time_left).w	; subtract 1 from time left in demo
+
	rts
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;VintSub14
Vint_PCM:
	move.b	(Vint_runcount+3).w,d0
	andi.w	#$F,d0
	bne.s	+

	stopZ80
	bsr.w	ReadJoypads
	startZ80
+
	tst.w	(Demo_Time_left).w	; is there time left on the demo?
	beq.w	+	; if not, return
	subq.w	#1,(Demo_Time_left).w	; subtract 1 from time left in demo
+
	rts
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;VintSub4
Vint_Title:
	bsr.w	Do_ControllerPal
	bsr.w	ProcessDPLC
	tst.w	(Demo_Time_left).w	; is there time left on the demo?
	beq.w	+	; if not, return
	subq.w	#1,(Demo_Time_left).w	; subtract 1 from time left in demo
+
	rts
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;VintSub6
Vint_Unused6:
	bsr.w	Do_ControllerPal
	rts
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;VintSub10
Vint_Pause:
	cmpi.b	#GameModeID_SpecialStage,(Game_Mode).w	; Special Stage?
	beq.w	Vint_Pause_specialStage
;VintSub8
Vint_Level:
	stopZ80

	bsr.w	ReadJoypads
	tst.b	(Teleport_timer).w
	beq.s	loc_6F8
	lea	(VDP_control_port).l,a5
	tst.w	(Game_paused).w	; is the game paused?
	bne.w	loc_748	; if yes, branch
	subq.b	#1,(Teleport_timer).w
	bne.s	+
	move.b	#0,(Teleport_flag).w
+
	cmpi.b	#16,(Teleport_timer).w
	blo.s	loc_6F8
	lea	(VDP_data_port).l,a6
	move.l	#vdpComm($0000,CRAM,WRITE),(VDP_control_port).l
	move.w	#$EEE,d0 ; White.

	move.w	#32-1,d1
-	move.w	d0,(a6)
	dbf	d1,-

	; Skip a colour.
	move.l	#vdpComm($0042,CRAM,WRITE),(VDP_control_port).l

    if fixBugs
	move.w	#31-1,d1
    else
	; This does one more colour than necessary: it isn't accounting for
	; the colour that was skipped earlier!
	move.w	#32-1,d1
    endif
-	move.w	d0,(a6)
	dbf	d1,-

	bra.s	loc_748
; ---------------------------------------------------------------------------

loc_6F8:
	tst.b	(Water_fullscreen_flag).w
	bne.s	loc_724
	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM
	bra.s	loc_748
; ---------------------------------------------------------------------------

loc_724:

	dma68kToVDP Underwater_palette,$0000,palette_line_size*4,CRAM

loc_748:
	move.w	(Hint_counter_reserve).w,(a5)
	move.w	#$8200|(VRAM_Plane_A_Name_Table/$400),(VDP_control_port).l	; Set scroll A PNT base to $C000

	dma68kToVDP Horiz_Scroll_Buf,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

    if fixBugs
	tst.w	(Two_player_mode).w
	beq.s	++
	; Like in Sonic 3, the sprite tables are page-flipped in two-player mode.
	; This fixes a race-condition where incomplete sprite tables can be uploaded
	; to the VDP on lag frames, causing corrupted sprites to appear.

	; Perform page-flipping.
	tst.b	(Sprite_table_page_flip_pending).w
	beq.s	+
	sf.b	(Sprite_table_page_flip_pending).w
	not.b	(Current_sprite_table_page).w
+
	; Upload the front buffer.
	tst.b	(Current_sprite_table_page).w
	bne.s	+
	dma68kToVDP Sprite_Table_Alternate,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	bra.s	++
+
    endif
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
+

	bsr.w	ProcessDMAQueue
	bsr.w	sndDriverInput

	startZ80

	movem.l	(Camera_RAM).w,d0-d7
	movem.l	d0-d7,(Camera_RAM_copy).w
	movem.l	(Camera_X_pos_P2).w,d0-d7
	movem.l	d0-d7,(Camera_P2_copy).w
	movem.l	(Scroll_flags).w,d0-d3
	movem.l	d0-d3,(Scroll_flags_copy).w
	move.l	(Vscroll_Factor_P2).w,(Vscroll_Factor_P2_HInt).w
	cmpi.b	#$5C,(Hint_counter_reserve+1).w
	bhs.s	Do_Updates
	move.b	#1,(Do_Updates_in_H_int).w
	rts

; ---------------------------------------------------------------------------
; Subroutine to run a demo for an amount of time
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_7E6: Demo_Time:
Do_Updates:
	jsrto	LoadTilesAsYouMove, JmpTo_LoadTilesAsYouMove
	jsr	(HudUpdate).l
	bsr.w	ProcessDPLC2
	tst.w	(Demo_Time_left).w	; is there time left on the demo?
	beq.w	+		; if not, branch
	subq.w	#1,(Demo_Time_left).w	; subtract 1 from time left in demo
+
	rts
; End of function Do_Updates

; ---------------------------------------------------------------------------
;Vint10_specialStage
Vint_Pause_specialStage:
	stopZ80

	bsr.w	ReadJoypads
	jsr	(sndDriverInput).l
	tst.b	(SS_Last_Alternate_HorizScroll_Buf).w
	beq.s	loc_84A

	dma68kToVDP SS_Horiz_Scroll_Buf_2,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM
	bra.s	loc_86E
; ---------------------------------------------------------------------------
loc_84A:
	dma68kToVDP SS_Horiz_Scroll_Buf_1,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

loc_86E:
	startZ80
	rts
; ========================================================================>>>
;VintSubA
Vint_S2SS:
	stopZ80

	bsr.w	ReadJoypads
	bsr.w	SSSet_VScroll

	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM

	tst.b	(SS_Alternate_HorizScroll_Buf).w
	beq.s	loc_906

	dma68kToVDP SS_Horiz_Scroll_Buf_2,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM
	bra.s	loc_92A
; ---------------------------------------------------------------------------

loc_906:
	dma68kToVDP SS_Horiz_Scroll_Buf_1,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

loc_92A:
	tst.b	(SSTrack_Orientation).w			; Is the current track frame flipped?
	beq.s	++								; Branch if not
	moveq	#0,d0
	move.b	(SSTrack_drawing_index).w,d0	; Get drawing position
	cmpi.b	#4,d0							; Have we finished drawing and streaming track frame?
	bge.s	++								; Branch if yes (nothing to draw)
	add.b	d0,d0							; Convert to index
	tst.b	(SS_Alternate_PNT).w			; [(SSTrack_drawing_index) * 2] = subroutine
	beq.s	+								; Branch if not using the alternate Plane A name table
	addi_.w	#8,d0							; ([(SSTrack_drawing_index) * 2] + 8) = subroutine
+
	move.w	SS_PNTA_Transfer_Table(pc,d0.w),d0
	jsr	SS_PNTA_Transfer_Table(pc,d0.w)
+
	bsr.w	SSRun_Animation_Timers
	addi_.b	#1,(SSTrack_drawing_index).w	; Run track timer
	move.b	(SSTrack_drawing_index).w,d0	; Get new timer value
	cmp.b	d1,d0							; Is it less than the player animation timer?
	blt.s	+++								; Branch if so
	move.b	#0,(SSTrack_drawing_index).w	; Start drawing new frame
	lea	(VDP_control_port).l,a6
	tst.b	(SS_Alternate_PNT).w			; Are we using the alternate address for plane A?
	beq.s	+								; Branch if not
	move.w	#$8200|(VRAM_SS_Plane_A_Name_Table1/$400),(a6)	; Set PNT A base to $C000
	bra.s	++
; ===========================================================================
;off_97A
SS_PNTA_Transfer_Table:	offsetTable
		offsetTableEntry.w loc_A50	; 0
		offsetTableEntry.w loc_A76	; 1
		offsetTableEntry.w loc_A9C	; 2
		offsetTableEntry.w loc_AC2	; 3
		offsetTableEntry.w loc_9B8	; 4
		offsetTableEntry.w loc_9DE	; 5
		offsetTableEntry.w loc_A04	; 6
		offsetTableEntry.w loc_A2A	; 7
; ===========================================================================
+
	move.w	#$8200|(VRAM_SS_Plane_A_Name_Table2/$400),(a6)	; Set PNT A base to $8000
+
	eori.b	#1,(SS_Alternate_PNT).w			; Toggle flag
+
	bsr.w	ProcessDMAQueue
	jsr	(sndDriverInput).l

	startZ80

	bsr.w	ProcessDPLC2
	tst.w	(Demo_Time_left).w
	beq.w	+	; rts
	subq.w	#1,(Demo_Time_left).w
+
	rts
; ---------------------------------------------------------------------------
; (!)
; Each of these functions copies one fourth of pattern name table A into VRAM
; from a buffer in main RAM. $700 bytes are copied each frame, with the target
; are in VRAM depending on the current drawing position.
loc_9B8:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table1 + 0 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_9DE:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table1 + 1 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_A04:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table1 + 2 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_A2A:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table1 + 3 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_A50:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table2 + 0 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_A76:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table2 + 1 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_A9C:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table2 + 2 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ---------------------------------------------------------------------------
loc_AC2:
	dma68kToVDP PNT_Buffer,VRAM_SS_Plane_A_Name_Table2 + 3 * (PNT_Buffer_End-PNT_Buffer),PNT_Buffer_End-PNT_Buffer,VRAM
	rts
; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_AE8
SSSet_VScroll:
	move.w	(VDP_control_port).l,d0
	move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
	move.l	(Vscroll_Factor).w,(VDP_data_port).l
	rts
; End of function SSSet_VScroll


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_B02
SSRun_Animation_Timers:
	move.w	(SS_Cur_Speed_Factor).w,d0		; Get current speed factor
	cmp.w	(SS_New_Speed_Factor).w,d0		; Has the speed factor changed?
	beq.s	+								; Branch if yes
	move.l	(SS_New_Speed_Factor).w,(SS_Cur_Speed_Factor).w	; Save new speed factor
	move.b	#0,(SSTrack_duration_timer).w	; Reset timer
+
	subi_.b	#1,(SSTrack_duration_timer).w	; Run track timer
	bgt.s	+								; Branch if not expired yet
	lea	(SSAnim_Base_Duration).l,a0
	move.w	(SS_Cur_Speed_Factor).w,d0		; The current speed factor is an index
	lsr.w	#1,d0
	move.b	(a0,d0.w),d1
	move.b	d1,(SS_player_anim_frame_timer).w	; New player animation length (later halved)
	move.b	d1,(SSTrack_duration_timer).w		; New track timer
	subq.b	#1,(SS_player_anim_frame_timer).w	; Subtract one
	rts
; ---------------------------------------------------------------------------
+
	move.b	(SS_player_anim_frame_timer).w,d1	; Get current player animatino length
	addq.b	#1,d1		; Increase it
	rts
; End of function SSRun_Animation_Timers

; ===========================================================================
;byte_B46
SSAnim_Base_Duration:
	dc.b 60
	dc.b 30	; 1
	dc.b 15	; 2
	dc.b 10	; 3
	dc.b  8	; 4
	dc.b  6	; 5
	dc.b  5	; 6
	dc.b  0	; 7
	even
; ===========================================================================
;VintSub1A
Vint_CtrlDMA:
	stopZ80
	jsr	(ProcessDMAQueue).l
	startZ80
	rts
; ===========================================================================
;VintSubC
Vint_TitleCard:
	stopZ80

	bsr.w	ReadJoypads
	tst.b	(Water_fullscreen_flag).w
	bne.s	loc_BB2

	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM
	bra.s	loc_BD6
; ---------------------------------------------------------------------------

loc_BB2:
	dma68kToVDP Underwater_palette,$0000,palette_line_size*4,CRAM

loc_BD6:
	move.w	(Hint_counter_reserve).w,(a5)

	dma68kToVDP Horiz_Scroll_Buf,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

    if fixBugs
	tst.w	(Two_player_mode).w
	beq.s	++
	; Like in Sonic 3, the sprite tables are page-flipped in two-player mode.
	; This fixes a race-condition where incomplete sprite tables can be uploaded
	; to the VDP on lag frames, causing corrupted sprites to appear.

	; Perform page-flipping.
	tst.b	(Sprite_table_page_flip_pending).w
	beq.s	+
	sf.b	(Sprite_table_page_flip_pending).w
	not.b	(Current_sprite_table_page).w
+
	; Upload the front buffer.
	tst.b	(Current_sprite_table_page).w
	bne.s	+
	dma68kToVDP Sprite_Table_Alternate,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	bra.s	++
+
    endif
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
+
	bsr.w	ProcessDMAQueue
	jsr	(DrawLevelTitleCard).l
	jsr	(sndDriverInput).l

	startZ80

	movem.l	(Camera_RAM).w,d0-d7
	movem.l	d0-d7,(Camera_RAM_copy).w
	movem.l	(Scroll_flags).w,d0-d1
	movem.l	d0-d1,(Scroll_flags_copy).w
	move.l	(Vscroll_Factor_P2).w,(Vscroll_Factor_P2_HInt).w
	bsr.w	ProcessDPLC
	rts
; ===========================================================================
;VintSubE
Vint_UnusedE:
	bsr.w	Do_ControllerPal
	addq.b	#1,(VIntSubE_RunCount).w
	move.b	#VintID_UnusedE,(Vint_routine).w
	rts
; ===========================================================================
;VintSub12
Vint_Fade:
	bsr.w	Do_ControllerPal
	move.w	(Hint_counter_reserve).w,(a5)
	bra.w	ProcessDPLC
; ===========================================================================
;VintSub18
Vint_Ending:
	stopZ80

	bsr.w	ReadJoypads

	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	dma68kToVDP Horiz_Scroll_Buf,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

	bsr.w	ProcessDMAQueue
	bsr.w	sndDriverInput
	movem.l	(Camera_RAM).w,d0-d7
	movem.l	d0-d7,(Camera_RAM_copy).w
	movem.l	(Scroll_flags).w,d0-d3
	movem.l	d0-d3,(Scroll_flags_copy).w
	jsrto	LoadTilesAsYouMove, JmpTo_LoadTilesAsYouMove

	startZ80

	move.w	(Ending_VInt_Subrout).w,d0
	beq.s	+	; rts
	clr.w	(Ending_VInt_Subrout).w
	move.w	off_D3C-2(pc,d0.w),d0
	jsr	off_D3C(pc,d0.w)
+
	rts
; ===========================================================================
off_D3C:	offsetTable
		offsetTableEntry.w (+)	; 1
		offsetTableEntry.w (++)	; 2
; ===========================================================================
+
	dmaFillVRAM 0,VRAM_EndSeq_Plane_A_Name_Table,VRAM_EndSeq_Plane_Table_Size	; VRAM Fill $C000 with $2000 zeros
	rts
; ---------------------------------------------------------------------------
+
	dmaFillVRAM 0,VRAM_EndSeq_Plane_B_Name_Table2,VRAM_EndSeq_Plane_Table_Size
	dmaFillVRAM 0,VRAM_EndSeq_Plane_A_Name_Table,VRAM_EndSeq_Plane_Table_Size

	lea	(VDP_control_port).l,a6
	move.w	#$8B00,(a6)		; EXT-INT off, V scroll by screen, H scroll by screen
	move.w	#$8400|(VRAM_EndSeq_Plane_B_Name_Table2/$2000),(a6)	; PNT B base: $4000
	move.w	#$9011,(a6)		; Scroll table size: 64x64
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_EndSeq_Plane_A_Name_Table + planeLoc(64,22,33),VRAM,WRITE),d0	;$50AC0003
	moveq	#23-1,d1
	moveq	#15-1,d2
	jsrto	PlaneMapToVRAM_H40, PlaneMapToVRAM_H40
	rts
; ===========================================================================
;VintSub16
Vint_Menu:
	stopZ80

	bsr.w	ReadJoypads

	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	dma68kToVDP Horiz_Scroll_Buf,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

	bsr.w	ProcessDMAQueue
	bsr.w	sndDriverInput

	startZ80

	bsr.w	ProcessDPLC
	tst.w	(Demo_Time_left).w
	beq.w	+	; rts
	subq.w	#1,(Demo_Time_left).w
+
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_E98
Do_ControllerPal:
	stopZ80

	bsr.w	ReadJoypads
	tst.b	(Water_fullscreen_flag).w
	bne.s	loc_EDA

	dma68kToVDP Normal_palette,$0000,palette_line_size*4,CRAM
	bra.s	loc_EFE
; ---------------------------------------------------------------------------

loc_EDA:
	dma68kToVDP Underwater_palette,$0000,palette_line_size*4,CRAM

loc_EFE:
	dma68kToVDP Sprite_Table,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	dma68kToVDP Horiz_Scroll_Buf,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size,VRAM

	bsr.w	sndDriverInput

	startZ80

	rts
; End of function sub_E98
; ||||||||||||||| E N D   O F   V - I N T |||||||||||||||||||||||||||||||||||

; ===========================================================================
; Start of H-INT code
H_Int:
	tst.w	(Hint_flag).w
	beq.w	H_Int_Done
	tst.w	(Two_player_mode).w
	beq.w	PalToCRAM
	move.w	#0,(Hint_flag).w
	move.l	a5,-(sp)
	move.l	d0,-(sp)

-	move.w	(VDP_control_port).l,d0	; loop start: Wait until we're in the H-blank region
	andi.w	#4,d0
	beq.s	-	; loop end

	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l		; Display disable

	move.w	#$8200|(VRAM_Plane_A_Name_Table_2P/$400),(VDP_control_port).l	; PNT A base: $A000

	; Update V-Scroll.
	move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
	move.l	(Vscroll_Factor_P2_HInt).w,(VDP_data_port).l

	stopZ80
    if fixBugs
	; Like in Sonic 3, the sprite tables are page-flipped in two-player mode.
	; This fixes a race-condition where incomplete sprite tables can be uploaded
	; to the VDP on lag frames.

	; Upload the front buffer.
	tst.b	(Current_sprite_table_page).w
	beq.s	+
	dma68kToVDP Sprite_Table_P2,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
	bra.s	++
+
	dma68kToVDP Sprite_Table_P2_Alternate,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
+
    else
	dma68kToVDP Sprite_Table_P2,VRAM_Sprite_Attribute_Table,VRAM_Sprite_Attribute_Table_Size,VRAM
    endif
	startZ80

-	move.w	(VDP_control_port).l,d0
	andi.w	#4,d0
	beq.s	-

	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l		; Display enable
	move.l	(sp)+,d0
	movea.l	(sp)+,a5

H_Int_Done:
	rte


; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; game code

; ---------------------------------------------------------------------------
; loc_1000:
PalToCRAM:
	move	#$2700,sr
	move.w	#0,(Hint_flag).w
	movem.l	a0-a1,-(sp)
	lea	(VDP_data_port).l,a1
	lea	(Underwater_palette).w,a0 	; load palette from RAM
	move.l	#vdpComm($0000,CRAM,WRITE),4(a1)	; set VDP to write to CRAM address $00
    rept 32
	move.l	(a0)+,(a1)	; move palette to CRAM (all 64 colors at once)
    endm
	move.w	#$8ADF,4(a1)	; Write %1101 %1111 to register 10 (interrupt every 224th line)
	movem.l	(sp)+,a0-a1
	tst.b	(Do_Updates_in_H_int).w
	bne.s	loc_1072
	rte
; ===========================================================================

loc_1072:
	clr.b	(Do_Updates_in_H_int).w
	movem.l	d0-a6,-(sp)
	bsr.w	Do_Updates
	movem.l	(sp)+,d0-a6
	rte

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Input our music/sound selection to the sound driver.

sndDriverInput:
	lea	(Sound_Queue&$00FFFFFF).l,a0
	lea	(Z80_RAM+zAbsVar).l,a1 ; $A01B80

	cmpi.b	#$80,zVar.QueueToPlay(a1)	; If this (zReadyFlag) isn't $80, the driver is processing a previous sound request.
	bne.s	.doSFX	; So we'll wait until at least the next frame before putting anything in there.

	; If there's something in the first music queue slot, then play it.
	_move.b	SoundQueue.Music0(a0),d0
	beq.s	.checkMusic2
	_clr.b	SoundQueue.Music0(a0)
	bra.s	.playMusic
; ---------------------------------------------------------------------------
; loc_10A4:
.checkMusic2:
	; If there's something in the second music queue slot, then play it.
	move.b	SoundQueue.Music1(a0),d0
	beq.s	.doSFX
	clr.b	SoundQueue.Music1(a0)
; loc_10AE:
.playMusic:
	; If this is 'MusID_Pause' or 'MusID_Unpause', then this isn't a real
	; sound ID, and it shouldn't be passed to the driver. Instead, it
	; should be used here to manually set the driver's pause flag.
	move.b	d0,d1
	subi.b	#MusID_Pause,d1
	bcs.s	.isNotPauseCommand
	addi.b	#$7F,d1
	move.b	d1,zVar.StopMusic(a1)
	bra.s	.doSFX
; ---------------------------------------------------------------------------
; loc_10C0:
.isNotPauseCommand:
	; Send the music's sound ID to the driver.
	move.b	d0,zVar.QueueToPlay(a1)
; loc_10C4:
.doSFX:
	; Process the SFX queue.
    if fixBugs
	moveq	#3-1,d1
    else
	; This is too high: there is only room for three bytes in the
	; driver's queue. This causes the first byte of 'VoiceTblPtr' to be
	; overwritten.
	moveq	#4-1,d1
    endif

.loop:
	; If there's no sound queued, skip this slot.
	move.b	SoundQueue.SFX0(a0,d1.w),d0
	beq.s	.skip
	; If this slot in the driver's queue is occupied, skip this slot.
	tst.b	zVar.Queue0(a1,d1.w)
	bne.s	.skip
	; Remove the sound from this queue, and put it in the driver's queue.
	clr.b	SoundQueue.SFX0(a0,d1.w)
	move.b	d0,zVar.Queue0(a1,d1.w)

.skip:
	dbf	d1,.loop

	rts
; End of function sndDriverInput

    if ~~removeJmpTos
; sub_10E0:
JmpTo_LoadTilesAsYouMove ; JmpTo
	jmp	(LoadTilesAsYouMove).l
JmpTo_SegaScr_VInt ; JmpTo
	jmp	(SegaScr_VInt).l

	align 4
    endif




; ---------------------------------------------------------------------------
; Subroutine to initialize joypads
; ---------------------------------------------------------------------------
; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_10EC:
JoypadInit:
	stopZ80
	moveq	#$40,d0
	move.b	d0,(HW_Port_1_Control).l	; init port 1 (joypad 1)
	move.b	d0,(HW_Port_2_Control).l	; init port 2 (joypad 2)
	move.b	d0,(HW_Expansion_Control).l	; init port 3 (expansion/extra)
	startZ80
	rts
; End of function JoypadInit

; ---------------------------------------------------------------------------
; Subroutine to read joypad input, and send it to the RAM
; ---------------------------------------------------------------------------
; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_111C:
ReadJoypads:
	lea	(Ctrl_1).w,a0	; address where joypad states are written
	lea	(HW_Port_1_Data).l,a1	; first joypad port
	bsr.s	Joypad_Read		; do the first joypad
	addq.w	#2,a1			; do the second joypad

; sub_112A:
Joypad_Read:
	move.b	#0,(a1)
	nop
	nop
	move.b	(a1),d0
	lsl.b	#2,d0
	andi.b	#$C0,d0
	move.b	#$40,(a1)
	nop
	nop
	move.b	(a1),d1
	andi.b	#$3F,d1
	or.b	d1,d0
	not.b	d0
	move.b	(a0),d1
	eor.b	d0,d1
	move.b	d0,(a0)+
	and.b	d0,d1
	move.b	d1,(a0)+
	rts
; End of function Joypad_Read


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_1158:
VDPSetupGame:
	lea	(VDP_control_port).l,a0
	lea	(VDP_data_port).l,a1
	lea	(VDPSetupArray).l,a2
	moveq	#bytesToWcnt(VDPSetupArray_End-VDPSetupArray),d7
; loc_116C:
VDP_Loop:
	move.w	(a2)+,(a0)
	dbf	d7,VDP_Loop	; set the VDP registers

	move.w	(VDPSetupArray+2).l,d0
	move.w	d0,(VDP_Reg1_val).w
	move.w	#$8A00+223,(Hint_counter_reserve).w	; H-INT every 224th scanline
	moveq	#0,d0

	move.l	#vdpComm($0000,VSRAM,WRITE),(VDP_control_port).l
	move.w	d0,(a1)
	move.w	d0,(a1)

	move.l	#vdpComm($0000,CRAM,WRITE),(VDP_control_port).l

	move.w	#bytesToWcnt(palette_line_size*4),d7
; loc_11A0:
VDP_ClrCRAM:
	move.w	d0,(a1)
	dbf	d7,VDP_ClrCRAM	; clear	the CRAM

	clr.l	(Vscroll_Factor).w
	clr.l	(unk_F61A).w
	move.l	d1,-(sp)

	dmaFillVRAM 0,$0000,$10000	; fill entire VRAM with 0

	move.l	(sp)+,d1
	rts
; End of function VDPSetupGame

; ===========================================================================
; word_11E2:
VDPSetupArray:
	dc.w $8004		; H-INT disabled
	dc.w $8134		; Genesis mode, DMA enabled, VBLANK-INT enabled
	dc.w $8200|(VRAM_Plane_A_Name_Table/$400)	; PNT A base: $C000
	dc.w $8328		; PNT W base: $A000
	dc.w $8400|(VRAM_Plane_B_Name_Table/$2000)	; PNT B base: $E000
	dc.w $8500|(VRAM_Sprite_Attribute_Table/$200)	; Sprite attribute table base: $F800
	dc.w $8600
	dc.w $8700		; Background palette/color: 0/0
	dc.w $8800
	dc.w $8900
	dc.w $8A00		; H-INT every scanline
	dc.w $8B00		; EXT-INT off, V scroll by screen, H scroll by screen
	dc.w $8C81		; H res 40 cells, no interlace, S/H disabled
	dc.w $8D00|(VRAM_Horiz_Scroll_Table/$400)	; H scroll table base: $FC00
	dc.w $8E00
	dc.w $8F02		; VRAM pointer increment: $0002
	dc.w $9001		; Scroll table size: 64x32
	dc.w $9100		; Disable window
	dc.w $9200		; Disable window
VDPSetupArray_End:

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_1208:
ClearScreen:
	stopZ80

	dmaFillVRAM 0,$0000,tiles_to_bytes(2)				; Fill first $40 bytes of VRAM with 0
	dmaFillVRAM 0,VRAM_Plane_A_Name_Table,VRAM_Plane_Table_Size	; Clear Plane A pattern name table
	dmaFillVRAM 0,VRAM_Plane_B_Name_Table,VRAM_Plane_Table_Size	; Clear Plane B pattern name table

	tst.w	(Two_player_mode).w
	beq.s	+

	dmaFillVRAM 0,VRAM_Plane_A_Name_Table_2P,VRAM_Plane_Table_Size
+
	clr.l	(Vscroll_Factor).w
	clr.l	(unk_F61A).w

    if fixBugs
	clearRAM Sprite_Table,Sprite_Table_End
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len
    else
	; These '+4's shouldn't be here; clearRAM accidentally clears an additional 4 bytes
	clearRAM Sprite_Table,Sprite_Table_End+4
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len+4
    endif

	startZ80
	rts
; End of function ClearScreen


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; JumpTo load the sound driver
; sub_130A:
JmpTo_SoundDriverLoad ; JmpTo
	nop
	jmp	(SoundDriverLoad).l
; End of function JmpTo_SoundDriverLoad

; ===========================================================================
; unused mostly-leftover subroutine to load the sound driver
; SoundDriverLoadS1:
	move.w	#$100,(Z80_Bus_Request).l ; stop the Z80
	move.w	#$100,(Z80_Reset).l ; reset the Z80
	lea	(Z80_RAM).l,a1
	move.b	#$F3,(a1)+	; di
	move.b	#$F3,(a1)+	; di
	move.b	#$C3,(a1)+	; jp
	move.b	#0,(a1)+	; jp address low byte
	move.b	#0,(a1)+	; jp address high byte
	move.w	#0,(Z80_Reset).l
	nop
	nop
	nop
	nop
	move.w	#$100,(Z80_Reset).l ; reset the Z80
	move.w	#0,(Z80_Bus_Request).l ; start the Z80
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Despite the name, this can actually be used for playing sounds.
; The original source code called this 'bgmset'.
; sub_135E:
PlayMusic:
	tst.b	(Sound_Queue.Music0).w
	bne.s	+
	move.b	d0,(Sound_Queue.Music0).w
	rts
+
	move.b	d0,(Sound_Queue.Music1).w
	rts
; End of function PlayMusic


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Despite the name, this can actually be used for playing music.
; The original source code called this 'sfxset'.
; sub_1370
PlaySound:
	; Curiously, none of these functions write to 'Sound_Queue.Queue2'...
	move.b	d0,(Sound_Queue.SFX0).w
	rts
; End of function PlaySound


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Despite the name, this can actually be used for playing music.
; Unfortunately, the original name for this is not known.
; sub_1376: PlaySoundStereo:
PlaySound2:
	move.b	d0,(Sound_Queue.SFX1).w
	rts
; End of function PlaySound2


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Play a sound if the source is on-screen.
; sub_137C:
PlaySoundLocal:
	tst.b	render_flags(a0)
	bpl.s	.return
	move.b	d0,(Sound_Queue.SFX0).w

.return:
	rts
; End of function PlaySoundLocal

; ---------------------------------------------------------------------------
; Subroutine to pause the game
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_1388:
PauseGame:
	nop
	tst.b	(Life_count).w	; do you have any lives left?
	beq.w	Unpause		; if not, branch
    if fixBugs
	; The game still lets you pause if player 2 got a Game Over, or if
	; either player got a Time Over. The following code fixes this.
	tst.b	(Life_count_2P).w
	beq.w	Unpause
	tst.b	(Time_Over_flag).w
	bne.w	Unpause
	tst.b   (Time_Over_flag_2P).w
	bne.w   Unpause
    endif
	tst.w	(Game_paused).w	; is game already paused?
	bne.s	+		; if yes, branch
	move.b	(Ctrl_1_Press).w,d0 ; is Start button pressed?
	or.b	(Ctrl_2_Press).w,d0 ; (either player)
	andi.b	#button_start_mask,d0
	beq.s	Pause_DoNothing	; if not, branch
+
	move.w	#1,(Game_paused).w	; freeze time
	move.b	#MusID_Pause,(Sound_Queue.Music0).w	; pause music
; loc_13B2:
Pause_Loop:
	move.b	#VintID_Pause,(Vint_routine).w
	bsr.w	WaitForVint
	tst.b	(Slow_motion_flag).w	; is slow-motion cheat on?
	beq.s	Pause_ChkStart		; if not, branch
	btst	#button_A,(Ctrl_1_Press).w	; is button A pressed?
	beq.s	Pause_ChkBC		; if not, branch
	move.b	#GameModeID_TitleScreen,(Game_Mode).w ; set game mode to 4 (title screen)
	nop
	bra.s	Pause_Resume
; ===========================================================================
; loc_13D4:
Pause_ChkBC:
	btst	#button_B,(Ctrl_1_Held).w ; is button B pressed?
	bne.s	Pause_SlowMo		; if yes, branch
	btst	#button_C,(Ctrl_1_Press).w ; is button C pressed?
	bne.s	Pause_SlowMo		; if yes, branch
; loc_13E4:
Pause_ChkStart:
	move.b	(Ctrl_1_Press).w,d0	; is Start button pressed?
	or.b	(Ctrl_2_Press).w,d0	; (either player)
	andi.b	#button_start_mask,d0
	beq.s	Pause_Loop	; if not, branch
; loc_13F2:
Pause_Resume:
	move.b	#MusID_Unpause,(Sound_Queue.Music0).w	; unpause the music
; loc_13F8:
Unpause:
	move.w	#0,(Game_paused).w	; unpause the game
; return_13FE:
Pause_DoNothing:
	rts
; ===========================================================================
; loc_1400:
Pause_SlowMo:
	move.w	#1,(Game_paused).w
	move.b	#MusID_Unpause,(Sound_Queue.Music0).w
	rts
; End of function PauseGame

; ---------------------------------------------------------------------------
; Subroutine to transfer a plane map to VRAM
; ---------------------------------------------------------------------------

; control register:
;    CD1 CD0 A13 A12 A11 A10 A09 A08     (D31-D24)
;    A07 A06 A05 A04 A03 A02 A01 A00     (D23-D16)
;     ?   ?   ?   ?   ?   ?   ?   ?      (D15-D8)
;    CD5 CD4 CD3 CD2  ?   ?  A15 A14     (D7-D0)
;
;	A00-A15 - address
;	CD0-CD3 - code
;	CD4 - 1 if VRAM copy DMA mode. 0 otherwise.
;	CD5 - DMA operation
;
;	Bits CD3-CD0:
;	0000 - VRAM read
;	0001 - VRAM write
;	0011 - CRAM write
;	0100 - VSRAM read
;	0101 - VSRAM write
;	1000 - CRAM read
;
; d0 = control register
; d1 = width
; d2 = heigth
; a1 = source address

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_140E: ShowVDPGraphics: PlaneMapToVRAM:
PlaneMapToVRAM_H40:
	lea	(VDP_data_port).l,a6
	move.l	#vdpCommDelta(planeLoc(64,0,1)),d4	; $800000
-	move.l	d0,VDP_control_port-VDP_data_port(a6)	; move d0 to VDP_control_port
	move.w	d1,d3
-	move.w	(a1)+,(a6)	; from source address to destination in VDP
	dbf	d3,-		; next tile
	add.l	d4,d0		; increase destination address by $80 (1 line)
	dbf	d2,--		; next line
	rts
; End of function PlaneMapToVRAM_H40

; ---------------------------------------------------------------------------
; Alternate subroutine to transfer a plane map to VRAM
; (used for Special Stage background)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_142E: ShowVDPGraphics2: PlaneMapToVRAM2:
PlaneMapToVRAM_H80_SpecialStage:
	lea	(VDP_data_port).l,a6
	move.l	#vdpCommDelta(planeLoc(128,0,1)),d4	; $1000000
-	move.l	d0,VDP_control_port-VDP_data_port(a6)
	move.w	d1,d3
-	move.w	(a1)+,(a6)
	dbf	d3,-
	add.l	d4,d0
	dbf	d2,--
	rts
; End of function PlaneMapToVRAM_H80_SpecialStage


; ---------------------------------------------------------------------------
; Subroutine for queueing VDP commands (seems to only queue transfers to VRAM),
; to be issued the next time ProcessDMAQueue is called.
; Can be called a maximum of 18 times before the buffer needs to be cleared
; by issuing the commands (this subroutine DOES check for overflow)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_144E: DMA_68KtoVRAM: QueueCopyToVRAM: QueueVDPCommand: Add_To_DMA_Queue:
QueueDMATransfer:
	movea.l	(VDP_Command_Buffer_Slot).w,a1
	cmpa.w	#VDP_Command_Buffer_Slot,a1
	beq.s	QueueDMATransfer_Done ; return if there's no more room in the buffer

	; piece together some VDP commands and store them for later...
	move.w	#$9300,d0 ; command to specify DMA transfer length & $00FF
	move.b	d3,d0
	move.w	d0,(a1)+ ; store command

	move.w	#$9400,d0 ; command to specify DMA transfer length & $FF00
	lsr.w	#8,d3
	move.b	d3,d0
	move.w	d0,(a1)+ ; store command

	move.w	#$9500,d0 ; command to specify source address & $0001FE
	lsr.l	#1,d1
	move.b	d1,d0
	move.w	d0,(a1)+ ; store command

	move.w	#$9600,d0 ; command to specify source address & $01FE00
	lsr.l	#8,d1
	move.b	d1,d0
	move.w	d0,(a1)+ ; store command

	move.w	#$9700,d0 ; command to specify source address & $FE0000
	lsr.l	#8,d1
	;andi.b	#$7F,d1		; this instruction safely allows source to be in RAM; S3K added this
	move.b	d1,d0
	move.w	d0,(a1)+ ; store command

	andi.l	#$FFFF,d2 ; command to specify destination address and begin DMA
	lsl.l	#2,d2
	lsr.w	#2,d2
	swap	d2
	ori.l	#vdpComm($0000,VRAM,DMA),d2 ; set bits to specify VRAM transfer
	move.l	d2,(a1)+ ; store command

	move.l	a1,(VDP_Command_Buffer_Slot).w ; set the next free slot address
	cmpa.w	#VDP_Command_Buffer_Slot,a1
	beq.s	QueueDMATransfer_Done ; return if there's no more room in the buffer
	move.w	#0,(a1) ; put a stop token at the end of the used part of the buffer
; return_14AA:
QueueDMATransfer_Done:
	rts
; End of function QueueDMATransfer


; ---------------------------------------------------------------------------
; Subroutine for issuing all VDP commands that were queued
; (by earlier calls to QueueDMATransfer)
; Resets the queue when it's done
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_14AC: CopyToVRAM: IssueVDPCommands: Process_DMA: Process_DMA_Queue:
ProcessDMAQueue:
	lea	(VDP_control_port).l,a5
	lea	(VDP_Command_Buffer).w,a1
; loc_14B6:
ProcessDMAQueue_Loop:
	move.w	(a1)+,d0
	beq.s	ProcessDMAQueue_Done ; branch if we reached a stop token
	; issue a set of VDP commands...
	move.w	d0,(a5)		; transfer length
	move.w	(a1)+,(a5)	; transfer length
	move.w	(a1)+,(a5)	; source address
	move.w	(a1)+,(a5)	; source address
	move.w	(a1)+,(a5)	; source address
	move.w	(a1)+,(a5)	; destination
	move.w	(a1)+,(a5)	; destination
	cmpa.w	#VDP_Command_Buffer_Slot,a1
	bne.s	ProcessDMAQueue_Loop ; loop if we haven't reached the end of the buffer
; loc_14CE:
ProcessDMAQueue_Done:
	move.w	#0,(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
	rts
; End of function ProcessDMAQueue



; ---------------------------------------------------------------------------
; START OF NEMESIS DECOMPRESSOR

; For format explanation see http://info.sonicretro.org/Nemesis_compression
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; Nemesis decompression to VRAM
; sub_14DE: NemDecA:
NemDec:
	movem.l	d0-a1/a3-a5,-(sp)
	lea	(NemDec_WriteAndStay).l,a3 ; write all data to the same location
	lea	(VDP_data_port).l,a4	   ; specifically, to the VDP data port
	bra.s	NemDecMain

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; Nemesis decompression to RAM
; input: a4 = starting address of destination
; sub_14F0: NemDecB:
NemDecToRAM:
	movem.l	d0-a1/a3-a5,-(sp)
	lea	(NemDec_WriteAndAdvance).l,a3 ; advance to the next location after each write


; sub_14FA:
NemDecMain:
	lea	(Decomp_Buffer).w,a1
	move.w	(a0)+,d2
	lsl.w	#1,d2
	bcc.s	+
	adda.w	#NemDec_WriteAndStay_XOR-NemDec_WriteAndStay,a3
+	lsl.w	#2,d2
	movea.w	d2,a5
	moveq	#8,d3
	moveq	#0,d2
	moveq	#0,d4
	bsr.w	NemDecPrepare
	move.b	(a0)+,d5
	asl.w	#8,d5
	move.b	(a0)+,d5
	move.w	#$10,d6
	bsr.s	NemDecRun
	movem.l	(sp)+,d0-a1/a3-a5
	rts
; End of function NemDec


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; part of the Nemesis decompressor
; sub_1528:
NemDecRun:
	move.w	d6,d7
	subq.w	#8,d7
	move.w	d5,d1
	lsr.w	d7,d1
	cmpi.b	#$FC,d1
	bhs.s	loc_1574
	andi.w	#$FF,d1
	add.w	d1,d1
	move.b	(a1,d1.w),d0
	ext.w	d0
	sub.w	d0,d6
	cmpi.w	#9,d6
	bhs.s	+
	addq.w	#8,d6
	asl.w	#8,d5
	move.b	(a0)+,d5
+	move.b	1(a1,d1.w),d1
	move.w	d1,d0
	andi.w	#$F,d1
	andi.w	#$F0,d0

loc_155E:
	lsr.w	#4,d0

loc_1560:
	lsl.l	#4,d4
	or.b	d1,d4
	subq.w	#1,d3
	bne.s	NemDec_WriteIter_Part2
	jmp	(a3) ; dynamic jump! to NemDec_WriteAndStay, NemDec_WriteAndAdvance, NemDec_WriteAndStay_XOR, or NemDec_WriteAndAdvance_XOR
; ===========================================================================
; loc_156A:
NemDec_WriteIter:
	moveq	#0,d4
	moveq	#8,d3
; loc_156E:
NemDec_WriteIter_Part2:
	dbf	d0,loc_1560
	bra.s	NemDecRun
; ===========================================================================

loc_1574:
	subq.w	#6,d6
	cmpi.w	#9,d6
	bhs.s	+
	addq.w	#8,d6
	asl.w	#8,d5
	move.b	(a0)+,d5
+
	subq.w	#7,d6
	move.w	d5,d1
	lsr.w	d6,d1
	move.w	d1,d0
	andi.w	#$F,d1
	andi.w	#$70,d0
	cmpi.w	#9,d6
	bhs.s	loc_155E
	addq.w	#8,d6
	asl.w	#8,d5
	move.b	(a0)+,d5
	bra.s	loc_155E
; End of function NemDecRun

; ===========================================================================
; loc_15A0:
NemDec_WriteAndStay:
	move.l	d4,(a4)
	subq.w	#1,a5
	move.w	a5,d4
	bne.s	NemDec_WriteIter
	rts
; ---------------------------------------------------------------------------
; loc_15AA:
NemDec_WriteAndStay_XOR:
	eor.l	d4,d2
	move.l	d2,(a4)
	subq.w	#1,a5
	move.w	a5,d4
	bne.s	NemDec_WriteIter
	rts
; ===========================================================================
; loc_15B6:
NemDec_WriteAndAdvance:
	move.l	d4,(a4)+
	subq.w	#1,a5
	move.w	a5,d4
	bne.s	NemDec_WriteIter
	rts

    if *-NemDec_WriteAndAdvance > NemDec_WriteAndStay_XOR-NemDec_WriteAndStay
	fatal "the code in NemDec_WriteAndAdvance must not be larger than the code in NemDec_WriteAndStay"
    endif
    org NemDec_WriteAndAdvance+NemDec_WriteAndStay_XOR-NemDec_WriteAndStay

; ---------------------------------------------------------------------------
; loc_15C0:
NemDec_WriteAndAdvance_XOR:
	eor.l	d4,d2
	move.l	d2,(a4)+
	subq.w	#1,a5
	move.w	a5,d4
	bne.s	NemDec_WriteIter
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Part of the Nemesis decompressor

; sub_15CC:
NemDecPrepare:
	move.b	(a0)+,d0

-	cmpi.b	#$FF,d0
	bne.s	+
	rts
; ---------------------------------------------------------------------------
+	move.w	d0,d7

loc_15D8:
	move.b	(a0)+,d0
	cmpi.b	#$80,d0
	bhs.s	-

	move.b	d0,d1
	andi.w	#$F,d7
	andi.w	#$70,d1
	or.w	d1,d7
	andi.w	#$F,d0
	move.b	d0,d1
	lsl.w	#8,d1
	or.w	d1,d7
	moveq	#8,d1
	sub.w	d0,d1
	bne.s	loc_1606
	move.b	(a0)+,d0
	add.w	d0,d0
	move.w	d7,(a1,d0.w)
	bra.s	loc_15D8
; ---------------------------------------------------------------------------
loc_1606:
	move.b	(a0)+,d0
	lsl.w	d1,d0
	add.w	d0,d0
	moveq	#1,d5
	lsl.w	d1,d5
	subq.w	#1,d5

-	move.w	d7,(a1,d0.w)
	addq.w	#2,d0
	dbf	d5,-

	bra.s	loc_15D8
; End of function NemDecPrepare

; ---------------------------------------------------------------------------
; END OF NEMESIS DECOMPRESSOR
; ---------------------------------------------------------------------------



; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; ---------------------------------------------------------------------------
; Subroutine to load pattern load cues (aka to queue pattern load requests)
; ---------------------------------------------------------------------------

; ARGUMENTS
; d0 = index of PLC list (see ArtLoadCues)

; NOTICE: This subroutine does not check for buffer overruns. The programmer
;	  (or hacker) is responsible for making sure that no more than
;	  16 load requests are copied into the buffer.
;    _________DO NOT PUT MORE THAN 16 LOAD REQUESTS IN A LIST!__________
;         (or if you change the size of Plc_Buffer, the limit becomes (Plc_Buffer_Only_End-Plc_Buffer)/6)

; sub_161E: PLCLoad:
LoadPLC:
	movem.l	a1-a2,-(sp)
	lea	(ArtLoadCues).l,a1
	add.w	d0,d0
	move.w	(a1,d0.w),d0
	lea	(a1,d0.w),a1
	lea	(Plc_Buffer).w,a2

-	tst.l	(a2)
	beq.s	+ ; if it's zero, exit this loop
	addq.w	#6,a2
	bra.s	-
+
	move.w	(a1)+,d0
	bmi.s	+ ; if it's negative, skip the next loop

-	move.l	(a1)+,(a2)+
	move.w	(a1)+,(a2)+
	dbf	d0,-
+
	movem.l	(sp)+,a1-a2 ; a1=object
	rts
; End of function LoadPLC


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Queue pattern load requests, but clear the PLQ first

; ARGUMENTS
; d0 = index of PLC list (see ArtLoadCues)

; NOTICE: This subroutine does not check for buffer overruns. The programmer
;	  (or hacker) is responsible for making sure that no more than
;	  16 load requests are copied into the buffer.
;	  _________DO NOT PUT MORE THAN 16 LOAD REQUESTS IN A LIST!__________
;         (or if you change the size of Plc_Buffer, the limit becomes (Plc_Buffer_Only_End-Plc_Buffer)/6)
; sub_1650:
LoadPLC2:
	movem.l	a1-a2,-(sp)
	lea	(ArtLoadCues).l,a1
	add.w	d0,d0
	move.w	(a1,d0.w),d0
	lea	(a1,d0.w),a1
	bsr.s	ClearPLC
	lea	(Plc_Buffer).w,a2
	move.w	(a1)+,d0
	bmi.s	+ ; if it's negative, skip the next loop

-	move.l	(a1)+,(a2)+
	move.w	(a1)+,(a2)+
	dbf	d0,-
+
	movem.l	(sp)+,a1-a2
	rts
; End of function LoadPLC2


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; Clear the pattern load queue ($FFF680 - $FFF700)

ClearPLC:
	lea	(Plc_Buffer).w,a2

	moveq	#bytesToLcnt(Plc_Buffer_End-Plc_Buffer),d0
-	clr.l	(a2)+
	dbf	d0,-

	rts
; End of function ClearPLC

; ---------------------------------------------------------------------------
; Subroutine to use graphics listed in a pattern load cue
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_168A:
RunPLC_RAM:
	tst.l	(Plc_Buffer).w
	beq.s	.return
	tst.w	(Plc_Buffer_Reg18).w
	bne.s	.return
	movea.l	(Plc_Buffer).w,a0
	lea_	NemDec_WriteAndStay,a3
	nop
	lea	(Decomp_Buffer).w,a1
	move.w	(a0)+,d2
	bpl.s	+
	adda.w	#NemDec_WriteAndStay_XOR-NemDec_WriteAndStay,a3
+
	andi.w	#$7FFF,d2
    if ~~fixBugs
	; This is done too early: this variable is used to determine when
	; there are PLCs to process, which means that as soon as this
	; variable is set, PLC processing will occur during V-Int. If an
	; interrupt occurs between here and the end of this function, then
	; the PLC processor will begin despite it not being fully
	; initialised yet, causing a crash. S3K fixes this bug by moving this
	; instruction to the end of the function.
	move.w	d2,(Plc_Buffer_Reg18).w
    endif

	bsr.w	NemDecPrepare
	move.b	(a0)+,d5
	asl.w	#8,d5
	move.b	(a0)+,d5
	moveq	#$10,d6
	moveq	#0,d0
	move.l	a0,(Plc_Buffer).w
	move.l	a3,(Plc_Buffer_Reg0).w
	move.l	d0,(Plc_Buffer_Reg4).w
	move.l	d0,(Plc_Buffer_Reg8).w
	move.l	d0,(Plc_Buffer_RegC).w
	move.l	d5,(Plc_Buffer_Reg10).w
	move.l	d6,(Plc_Buffer_Reg14).w
    if fixBugs
	; See above.
	move.w	d2,(Plc_Buffer_Reg18).w
    endif

.return:
	rts
; End of function RunPLC_RAM


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Process one PLC from the queue

; sub_16E0:
ProcessDPLC:
	tst.w	(Plc_Buffer_Reg18).w
	beq.w	+	; rts
	move.w	#6,(Plc_Buffer_Reg1A).w
	moveq	#0,d0
	move.w	(Plc_Buffer+4).w,d0
	addi.w	#$C0,(Plc_Buffer+4).w
	bra.s	ProcessDPLC_Main

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Process one PLC from the queue

; loc_16FC:
ProcessDPLC2:
	tst.w	(Plc_Buffer_Reg18).w
	beq.s	+	; rts
	move.w	#3,(Plc_Buffer_Reg1A).w
	moveq	#0,d0
	move.w	(Plc_Buffer+4).w,d0
	addi.w	#$60,(Plc_Buffer+4).w

; loc_1714:
ProcessDPLC_Main:
	lea	(VDP_control_port).l,a4
	lsl.l	#2,d0		; set up target VRAM address
	lsr.w	#2,d0
	ori.w	#vdpComm($0000,VRAM,WRITE)>>16,d0
	swap	d0
	move.l	d0,(a4)
	subq.w	#4,a4
	movea.l	(Plc_Buffer).w,a0
	movea.l	(Plc_Buffer_Reg0).w,a3
	move.l	(Plc_Buffer_Reg4).w,d0
	move.l	(Plc_Buffer_Reg8).w,d1
	move.l	(Plc_Buffer_RegC).w,d2
	move.l	(Plc_Buffer_Reg10).w,d5
	move.l	(Plc_Buffer_Reg14).w,d6
	lea	(Decomp_Buffer).w,a1

-	movea.w	#8,a5
	bsr.w	NemDec_WriteIter
	subq.w	#1,(Plc_Buffer_Reg18).w
	beq.s	ProcessDPLC_Pop
	subq.w	#1,(Plc_Buffer_Reg1A).w
	bne.s	-

	move.l	a0,(Plc_Buffer).w
	move.l	a3,(Plc_Buffer_Reg0).w
	move.l	d0,(Plc_Buffer_Reg4).w
	move.l	d1,(Plc_Buffer_Reg8).w
	move.l	d2,(Plc_Buffer_RegC).w
	move.l	d5,(Plc_Buffer_Reg10).w
	move.l	d6,(Plc_Buffer_Reg14).w
+
	rts

; ===========================================================================
; pop one request off the buffer so that the next one can be filled

; loc_177A:
ProcessDPLC_Pop:
	lea	(Plc_Buffer).w,a0
	moveq	#bytesToLcnt(Plc_Buffer_Only_End-Plc_Buffer-6),d0
-	move.l	6(a0),(a0)+
	dbf	d0,-

    if fixBugs
	; The above code does not properly 'pop' the 16th PLC entry.
	; Because of this, occupying the 16th slot will cause it to
	; be repeatedly decompressed infinitely.
	; Granted, this could be conisdered more of an optimisation
	; than a bug: treating the 16th entry as a dummy that
	; should never be occupied makes this code unnecessary.
	; Still, the overhead of this code is minimal.
    if (Plc_Buffer_Only_End-Plc_Buffer-6)&2
	move.w	6(a0),(a0)
    endif

	clr.l	(Plc_Buffer_Only_End-6).w
    endif

	rts

; End of function ProcessDPLC


; ---------------------------------------------------------------------------
; Subroutine to execute a pattern load cue directly from the ROM
; rather than loading them into the queue first
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

RunPLC_ROM:
	lea	(ArtLoadCues).l,a1
	add.w	d0,d0
	move.w	(a1,d0.w),d0
	lea	(a1,d0.w),a1

	move.w	(a1)+,d1
-	movea.l	(a1)+,a0
	moveq	#0,d0
	move.w	(a1)+,d0
	lsl.l	#2,d0
	lsr.w	#2,d0
	ori.w	#vdpComm($0000,VRAM,WRITE)>>16,d0
	swap	d0
	move.l	d0,(VDP_control_port).l
	bsr.w	NemDec
	dbf	d1,-

	rts
; End of function RunPLC_ROM

; ---------------------------------------------------------------------------
; Enigma Decompression Algorithm

; ARGUMENTS:
; d0 = starting art tile (added to each 8x8 before writing to destination)
; a0 = source address
; a1 = destination address

; For format explanation see http://info.sonicretro.org/Enigma_compression
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; EniDec_17BC:
EniDec:
	movem.l	d0-d7/a1-a5,-(sp)
	movea.w	d0,a3		; store starting art tile
	move.b	(a0)+,d0
	ext.w	d0
	movea.w	d0,a5		; store first byte, extended to word
	move.b	(a0)+,d4	; store second byte
	lsl.b	#3,d4		; multiply by 8
	movea.w	(a0)+,a2	; store third and fourth byte
	adda.w	a3,a2		; add starting art tile
	movea.w	(a0)+,a4	; store fifth and sixth byte
	adda.w	a3,a4		; add starting art tile
	move.b	(a0)+,d5	; store seventh byte
	asl.w	#8,d5		; shift up by a byte
	move.b	(a0)+,d5	; store eighth byte in lower register byte
	moveq	#16,d6		; 16 bits = 2 bytes

EniDec_Loop:
	moveq	#7,d0		; process 7 bits at a time
	move.w	d6,d7
	sub.w	d0,d7
	move.w	d5,d1
	lsr.w	d7,d1
	andi.w	#$7F,d1		; keep only lower 7 bits
	move.w	d1,d2
	cmpi.w	#$40,d1		; is bit 6 set?
	bhs.s	.sevenbitentry	; if it is, branch
	moveq	#6,d0		; if not, process 6 bits instead of 7
	lsr.w	#1,d2		; bitfield now becomes TTSSSS instead of TTTSSSS

.sevenbitentry:
	bsr.w	EniDec_ChkGetNextByte
	andi.w	#$F,d2	; keep only lower nybble
	lsr.w	#4,d1	; store upper nybble (max value = 7)
	add.w	d1,d1
	jmp	EniDec_JmpTable(pc,d1.w)
; End of function EniDec

; ===========================================================================

EniDec_Sub0:
	move.w	a2,(a1)+	; write to destination
	addq.w	#1,a2		; increment
	dbf	d2,EniDec_Sub0	; repeat
	bra.s	EniDec_Loop
; ===========================================================================

EniDec_Sub4:
	move.w	a4,(a1)+	; write to destination
	dbf	d2,EniDec_Sub4	; repeat
	bra.s	EniDec_Loop
; ===========================================================================

EniDec_Sub8:
	bsr.w	EniDec_GetInlineCopyVal

.loop:
	move.w	d1,(a1)+
	dbf	d2,.loop

	bra.s	EniDec_Loop
; ===========================================================================

EniDec_SubA:
	bsr.w	EniDec_GetInlineCopyVal

.loop:
	move.w	d1,(a1)+
	addq.w	#1,d1
	dbf	d2,.loop

	bra.s	EniDec_Loop
; ===========================================================================

EniDec_SubC:
	bsr.w	EniDec_GetInlineCopyVal

.loop:
	move.w	d1,(a1)+
	subq.w	#1,d1
	dbf	d2,.loop

	bra.s	EniDec_Loop
; ===========================================================================

EniDec_SubE:
	cmpi.w	#$F,d2
	beq.s	EniDec_End

.loop:
	bsr.w	EniDec_GetInlineCopyVal
	move.w	d1,(a1)+
	dbf	d2,.loop

	bra.s	EniDec_Loop
; ===========================================================================
; Enigma_JmpTable:
EniDec_JmpTable:
	bra.s	EniDec_Sub0
	bra.s	EniDec_Sub0
	bra.s	EniDec_Sub4
	bra.s	EniDec_Sub4
	bra.s	EniDec_Sub8
	bra.s	EniDec_SubA
	bra.s	EniDec_SubC
	bra.s	EniDec_SubE
; ===========================================================================

EniDec_End:
	subq.w	#1,a0
	cmpi.w	#16,d6		; were we going to start on a completely new byte?
	bne.s	.notnewbyte	; if not, branch
	subq.w	#1,a0

.notnewbyte:
	move.w	a0,d0
	lsr.w	#1,d0		; are we on an odd byte?
	bcc.s	.evenbyte	; if not, branch
	addq.w	#1,a0		; ensure we're on an even byte

.evenbyte:
	movem.l	(sp)+,d0-d7/a1-a5
	rts

;  S U B R O U T I N E


EniDec_GetInlineCopyVal:
	move.w	a3,d3		; store starting art tile
	move.b	d4,d1		; store PCCVH bitfield
	add.b	d1,d1
	bcc.s	.skippriority	; if d4 was < $80
	subq.w	#1,d6		; get next bit number
	btst	d6,d5		; is the bit set?
	beq.s	.skippriority	; if not, branch
	ori.w	#high_priority,d3	; set high priority bit

.skippriority:
	add.b	d1,d1
	bcc.s	.skiphighpal	; if d4 was < $40
	subq.w	#1,d6		; get next bit number
	btst	d6,d5
	beq.s	.skiphighpal
	addi.w	#palette_line_2,d3	; set second palette line bit

.skiphighpal:
	add.b	d1,d1
	bcc.s	.skiplowpal	; if d4 was < $20
	subq.w	#1,d6		; get next bit number
	btst	d6,d5
	beq.s	.skiplowpal
	addi.w	#palette_line_1,d3	; set first palette line bit

.skiplowpal:
	add.b	d1,d1
	bcc.s	.skipyflip	; if d4 was < $10
	subq.w	#1,d6		; get next bit number
	btst	d6,d5
	beq.s	.skipyflip
	ori.w	#flip_y,d3	; set Y-flip bit

.skipyflip:
	add.b	d1,d1
	bcc.s	.skipxflip	; if d4 was < 8
	subq.w	#1,d6
	btst	d6,d5
	beq.s	.skipxflip
	ori.w	#flip_x,d3	; set X-flip bit

.skipxflip:
	move.w	d5,d1
	move.w	d6,d7		; get remaining bits
	sub.w	a5,d7		; subtract minimum bit number
	bcc.s	.enoughbits	; if we're beyond that, branch
	move.w	d7,d6
	addi.w	#16,d6		; 16 bits = 2 bytes
	neg.w	d7		; calculate bit deficit
	lsl.w	d7,d1		; make space for this many bits
	move.b	(a0),d5		; get next byte
	rol.b	d7,d5		; make the upper X bits the lower X bits
	add.w	d7,d7
	and.w	EniDec_AndVals-2(pc,d7.w),d5	; only keep X lower bits
	add.w	d5,d1		; compensate for the bit deficit

.maskvalue:
	move.w	a5,d0
	add.w	d0,d0
	and.w	EniDec_AndVals-2(pc,d0.w),d1	; only keep as many bits as required
	add.w	d3,d1		; add starting art tile
	move.b	(a0)+,d5	; get current byte, move onto next byte
	lsl.w	#8,d5		; shift up by a byte
	move.b	(a0)+,d5	; store next byte in lower register byte
	rts
; ===========================================================================
.enoughbits:
	beq.s	.justenough	; if the exact number of bits are leftover, branch
	lsr.w	d7,d1		; remove unneeded bits
	move.w	a5,d0
	add.w	d0,d0
	and.w	EniDec_AndVals-2(pc,d0.w),d1	; only keep as many bits as required
	add.w	d3,d1		; add starting art tile
	move.w	a5,d0		; store number of bits used up by inline copy
	bra.s	EniDec_ChkGetNextByte	; move onto next byte
; ===========================================================================
.justenough:
	moveq	#16,d6	; 16 bits = 2 bytes
	bra.s	.maskvalue
; End of function EniDec_GetInlineCopyVal

; ===========================================================================
; word_190A:
EniDec_AndVals:
	dc.w	 1,    3,    7,   $F
	dc.w   $1F,  $3F,  $7F,  $FF
	dc.w  $1FF, $3FF, $7FF, $FFF
	dc.w $1FFF,$3FFF,$7FFF,$FFFF
; ===========================================================================

EniDec_ChkGetNextByte:
	sub.w	d0,d6
	cmpi.w	#9,d6
	bhs.s	.return
	addq.w	#8,d6	; 8 bits = 1 byte
	asl.w	#8,d5	; shift up by a byte
	move.b	(a0)+,d5	; store next byte in lower register byte

.return:
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; ---------------------------------------------------------------------------
; KOSINSKI DECOMPRESSION PROCEDURE
; (sometimes called KOZINSKI decompression)

; This is the only procedure in the game that stores variables on the stack.

; ARGUMENTS:
; a0 = source address
; a1 = destination address

; For format explanation, see http://info.sonicretro.org/Kosinski_compression
; ---------------------------------------------------------------------------
; KozDec_193A:
KosDec:
	subq.l	#2,sp
	move.b	(a0)+,1(sp)
	move.b	(a0)+,(sp)
	move.w	(sp),d5
	moveq	#$F,d4

Kos_Loop:
	lsr.w	#1,d5
	move	sr,d6
	dbf	d4,.chkbit
	move.b	(a0)+,1(sp)
	move.b	(a0)+,(sp)
	move.w	(sp),d5
	moveq	#$F,d4

.chkbit:
	move	d6,ccr
	bcc.s	Kos_RLE
	move.b	(a0)+,(a1)+
	bra.s	Kos_Loop
; ---------------------------------------------------------------------------
Kos_RLE:
	moveq	#0,d3
	lsr.w	#1,d5
	move	sr,d6
	dbf	d4,.chkbit
	move.b	(a0)+,1(sp)
	move.b	(a0)+,(sp)
	move.w	(sp),d5
	moveq	#$F,d4

.chkbit:
	move	d6,ccr
	bcs.s	Kos_SeparateRLE
	lsr.w	#1,d5
	dbf	d4,.loop1
	move.b	(a0)+,1(sp)
	move.b	(a0)+,(sp)
	move.w	(sp),d5
	moveq	#$F,d4

.loop1:
	roxl.w	#1,d3
	lsr.w	#1,d5
	dbf	d4,.loop2
	move.b	(a0)+,1(sp)
	move.b	(a0)+,(sp)
	move.w	(sp),d5
	moveq	#$F,d4

.loop2:
	roxl.w	#1,d3
	addq.w	#1,d3
	moveq	#-1,d2
	move.b	(a0)+,d2
	bra.s	Kos_RLELoop
; ---------------------------------------------------------------------------
Kos_SeparateRLE:
	move.b	(a0)+,d0
	move.b	(a0)+,d1
	moveq	#-1,d2
	move.b	d1,d2
	lsl.w	#5,d2
	move.b	d0,d2
	andi.w	#7,d1
	beq.s	Kos_SeparateRLE2
	move.b	d1,d3
	addq.w	#1,d3

Kos_RLELoop:
	move.b	(a1,d2.w),d0
	move.b	d0,(a1)+
	dbf	d3,Kos_RLELoop
	bra.s	Kos_Loop
; ---------------------------------------------------------------------------
Kos_SeparateRLE2:
	move.b	(a0)+,d1
	beq.s	Kos_Done
	cmpi.b	#1,d1
	beq.w	Kos_Loop
	move.b	d1,d3
	bra.s	Kos_RLELoop
; ---------------------------------------------------------------------------
Kos_Done:
	addq.l	#2,sp
	rts
; End of function KosDec

; ===========================================================================

    if gameRevision<2
	nop
    endif




; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_19DC:
PalCycle_Load:
	bsr.w	PalCycle_SuperSonic
	moveq	#0,d2
	moveq	#0,d0
	move.b	(Current_Zone).w,d0	; use level number as index into palette cycles
	add.w	d0,d0			; (multiply by element size = 2 bytes)
	move.w	PalCycle(pc,d0.w),d0	; load animated palettes offset index into d0
	jmp	PalCycle(pc,d0.w)	; jump to PalCycle + offset index
; ---------------------------------------------------------------------------
	rts
; End of function PalCycle_Load

; ===========================================================================
; off_19F4:
PalCycle: zoneOrderedOffsetTable 2,1
	zoneOffsetTableEntry.w PalCycle_EHZ	; EHZ
	zoneOffsetTableEntry.w PalCycle_Null	; Zone 1
	zoneOffsetTableEntry.w PalCycle_WZ	; WZ
	zoneOffsetTableEntry.w PalCycle_Null	; Zone 3
	zoneOffsetTableEntry.w PalCycle_MTZ	; MTZ1,2
	zoneOffsetTableEntry.w PalCycle_MTZ	; MTZ3
	zoneOffsetTableEntry.w PalCycle_WFZ	; WFZ
	zoneOffsetTableEntry.w PalCycle_HTZ	; HTZ
	zoneOffsetTableEntry.w PalCycle_HPZ	; HPZ
	zoneOffsetTableEntry.w PalCycle_Null	; Zone 9
	zoneOffsetTableEntry.w PalCycle_OOZ	; OOZ
	zoneOffsetTableEntry.w PalCycle_MCZ	; MCZ
	zoneOffsetTableEntry.w PalCycle_CNZ	; CNZ
	zoneOffsetTableEntry.w PalCycle_CPZ	; CPZ
	zoneOffsetTableEntry.w PalCycle_CPZ	; DEZ
	zoneOffsetTableEntry.w PalCycle_ARZ	; ARZ
	zoneOffsetTableEntry.w PalCycle_WFZ	; SCZ
    zoneTableEnd

; ===========================================================================
; return_1A16:
PalCycle_Null:
	rts
; ===========================================================================

PalCycle_EHZ:
	lea	(CyclingPal_EHZ_ARZ_Water).l,a0
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#7,(PalCycle_Timer).w
	move.w	(PalCycle_Frame).w,d0
	addq.w	#1,(PalCycle_Frame).w
	andi.w	#3,d0
	lsl.w	#3,d0
	move.l	(a0,d0.w),(Normal_palette_line2+6).w
	move.l	4(a0,d0.w),(Normal_palette_line2+$1C).w

.return:
	rts
; ===========================================================================

; PalCycle_Level2:
PalCycle_WZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#2,(PalCycle_Timer).w
	lea	(CyclingPal_WoodConveyor).l,a0
	move.w	(PalCycle_Frame).w,d0
	subq.w	#2,(PalCycle_Frame).w
	bcc.s	+
	move.w	#6,(PalCycle_Frame).w
+	lea	(Normal_palette_line4+6).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)

.return:
	rts
; ===========================================================================

PalCycle_MTZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	++
	move.w	#$11,(PalCycle_Timer).w
	lea	(CyclingPal_MTZ1).l,a0
	move.w	(PalCycle_Frame).w,d0
	addq.w	#2,(PalCycle_Frame).w
	cmpi.w	#$C,(PalCycle_Frame).w
	blo.s	+
	move.w	#0,(PalCycle_Frame).w
+	lea	(Normal_palette_line3+$A).w,a1
	move.w	(a0,d0.w),(a1)
+
	subq.w	#1,(PalCycle_Timer2).w
	bpl.s	++
	move.w	#2,(PalCycle_Timer2).w
	lea	(CyclingPal_MTZ2).l,a0
	move.w	(PalCycle_Frame2).w,d0
	addq.w	#2,(PalCycle_Frame2).w
	cmpi.w	#6,(PalCycle_Frame2).w
	blo.s	+
	move.w	#0,(PalCycle_Frame2).w
+	lea	(Normal_palette_line3+2).w,a1
	move.l	(a0,d0.w),(a1)+
	move.w	4(a0,d0.w),(a1)
+
	subq.w	#1,(PalCycle_Timer3).w
	bpl.s	.return
	move.w	#9,(PalCycle_Timer3).w
	lea	(CyclingPal_MTZ3).l,a0
	move.w	(PalCycle_Frame3).w,d0
	addq.w	#2,(PalCycle_Frame3).w
	cmpi.w	#$14,(PalCycle_Frame3).w
	blo.s	+
	move.w	#0,(PalCycle_Frame3).w
+	lea	(Normal_palette_line3+$1E).w,a1
	move.w	(a0,d0.w),(a1)

.return:
	rts
; ===========================================================================

PalCycle_HTZ:
	lea	(CyclingPal_Lava).l,a0
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#0,(PalCycle_Timer).w
	move.w	(PalCycle_Frame).w,d0
	addq.w	#1,(PalCycle_Frame).w
	andi.w	#$F,d0
	move.b	PalCycle_HTZ_LavaDelayData(pc,d0.w),(PalCycle_Timer+1).w
	lsl.w	#3,d0
	move.l	(a0,d0.w),(Normal_palette_line2+6).w
	move.l	4(a0,d0.w),(Normal_palette_line2+$1C).w

.return:
	rts
; ===========================================================================
; byte_1B40:
PalCycle_HTZ_LavaDelayData: ; number of frames between changes of the lava palette
	dc.b	$B, $B, $B, $A
	dc.b	 8, $A, $B, $B
	dc.b	$B, $B, $D, $F
	dc.b	$D, $B, $B, $B
	even
; ===========================================================================

PalCycle_HPZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#4,(PalCycle_Timer).w
	lea	(CyclingPal_HPZWater).l,a0
	move.w	(PalCycle_Frame).w,d0
	subq.w	#2,(PalCycle_Frame).w
	bcc.s	+
	move.w	#6,(PalCycle_Frame).w
+	lea	(Normal_palette_line4+$12).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
	lea	(CyclingPal_HPZUnderwater).l,a0
	lea	(Underwater_palette_line4+$12).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)

.return:
	rts
; ===========================================================================

PalCycle_OOZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#7,(PalCycle_Timer).w
	lea	(CyclingPal_Oil).l,a0
	move.w	(PalCycle_Frame).w,d0
	addq.w	#2,(PalCycle_Frame).w
	andi.w	#6,(PalCycle_Frame).w
	lea	(Normal_palette_line3+$14).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)

.return:
	rts
; ===========================================================================

PalCycle_MCZ:
	tst.b	(Current_Boss_ID).w
	bne.s	.return
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#1,(PalCycle_Timer).w
	lea	(CyclingPal_Lantern).l,a0
	move.w	(PalCycle_Frame).w,d0
	addq.w	#2,(PalCycle_Frame).w
	andi.w	#6,(PalCycle_Frame).w
	move.w	(a0,d0.w),(Normal_palette_line2+$16).w

.return:
	rts
; ===========================================================================

PalCycle_CNZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.w	CNZ_SkipToBossPalCycle
	move.w	#7,(PalCycle_Timer).w
	lea	(CyclingPal_CNZ1).l,a0
	move.w	(PalCycle_Frame).w,d0
	addq.w	#2,(PalCycle_Frame).w
	cmpi.w	#6,(PalCycle_Frame).w
	blo.s	+
	move.w	#0,(PalCycle_Frame).w
+
	lea	(a0,d0.w),a0
	lea	(Normal_palette).w,a1
	_move.w	0(a0),$4A(a1)
	move.w	6(a0),$4C(a1)
	move.w	$C(a0),$4E(a1)
	move.w	$12(a0),$56(a1)
	move.w	$18(a0),$58(a1)
	move.w	$1E(a0),$5A(a1)
	lea	(CyclingPal_CNZ3).l,a0
	lea	(a0,d0.w),a0
	_move.w	0(a0),$64(a1)
	move.w	6(a0),$66(a1)
	move.w	$C(a0),$68(a1)
	lea	(CyclingPal_CNZ4).l,a0
	move.w	(PalCycle_Frame_CNZ).w,d0
	addq.w	#2,(PalCycle_Frame_CNZ).w
	cmpi.w	#$24,(PalCycle_Frame_CNZ).w
	blo.s	+
	move.w	#0,(PalCycle_Frame_CNZ).w
+
	lea	(Normal_palette_line4+$12).w,a1
	move.w	4(a0,d0.w),(a1)+
	move.w	2(a0,d0.w),(a1)+
	move.w	(a0,d0.w),(a1)+

CNZ_SkipToBossPalCycle:
	tst.b	(Current_Boss_ID).w
	beq.w	+++	; rts
	subq.w	#1,(PalCycle_Timer2).w
	bpl.w	+++	; rts
	move.w	#3,(PalCycle_Timer2).w
	move.w	(PalCycle_Frame2).w,d0
	addq.w	#2,(PalCycle_Frame2).w
	cmpi.w	#6,(PalCycle_Frame2).w
	blo.s	+
	move.w	#0,(PalCycle_Frame2).w
+	lea	(CyclingPal_CNZ1_B).l,a0
	lea	(a0,d0.w),a0
	lea	(Normal_palette).w,a1
	_move.w	0(a0),$24(a1)
	move.w	6(a0),$26(a1)
	move.w	$C(a0),$28(a1)
	lea	(CyclingPal_CNZ2_B).l,a0
	move.w	(PalCycle_Frame3).w,d0
	addq.w	#2,(PalCycle_Frame3).w
	cmpi.w	#$14,(PalCycle_Frame3).w
	blo.s	+
	move.w	#0,(PalCycle_Frame3).w
+	move.w	(a0,d0.w),$3C(a1)
	lea	(CyclingPal_CNZ3_B).l,a0
	move.w	(PalCycle_Frame2_CNZ).w,d0
	addq.w	#2,(PalCycle_Frame2_CNZ).w
	andi.w	#$E,(PalCycle_Frame2_CNZ).w
	move.w	(a0,d0.w),$3E(a1)
+	rts
; ===========================================================================

PalCycle_CPZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#7,(PalCycle_Timer).w
	lea	(CyclingPal_CPZ1).l,a0
	move.w	(PalCycle_Frame).w,d0
	addq.w	#6,(PalCycle_Frame).w
	cmpi.w	#$36,(PalCycle_Frame).w
	blo.s	+
	move.w	#0,(PalCycle_Frame).w
+	lea	(Normal_palette_line4+$18).w,a1
	move.l	(a0,d0.w),(a1)+
	move.w	4(a0,d0.w),(a1)
	lea	(CyclingPal_CPZ2).l,a0
	move.w	(PalCycle_Frame2).w,d0
	addq.w	#2,(PalCycle_Frame2).w
	cmpi.w	#$2A,(PalCycle_Frame2).w
	blo.s	+
	move.w	#0,(PalCycle_Frame2).w
+	move.w	(a0,d0.w),(Normal_palette_line4+$1E).w
	lea	(CyclingPal_CPZ3).l,a0
	move.w	(PalCycle_Frame3).w,d0
	addq.w	#2,(PalCycle_Frame3).w
	andi.w	#$1E,(PalCycle_Frame3).w
	move.w	(a0,d0.w),(Normal_palette_line3+$1E).w

.return:
	rts
; ===========================================================================

PalCycle_ARZ:
	lea	(CyclingPal_EHZ_ARZ_Water).l,a0
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	.return
	move.w	#5,(PalCycle_Timer).w
	move.w	(PalCycle_Frame).w,d0
	addq.w	#1,(PalCycle_Frame).w
	andi.w	#3,d0
	lsl.w	#3,d0
	lea	(Normal_palette_line3+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)

.return:
	rts
; ===========================================================================

PalCycle_WFZ:
	subq.w	#1,(PalCycle_Timer).w
	bpl.s	+++
	move.w	#1,(PalCycle_Timer).w
	lea	(CyclingPal_WFZFire).l,a0
	tst.b	(WFZ_SCZ_Fire_Toggle).w
	beq.s	+
	move.w	#5,(PalCycle_Timer).w
	lea	(CyclingPal_WFZBelt).l,a0
+	move.w	(PalCycle_Frame).w,d0
	addq.w	#8,(PalCycle_Frame).w
	cmpi.w	#$20,(PalCycle_Frame).w
	blo.s	+
	move.w	#0,(PalCycle_Frame).w
+	lea	(Normal_palette_line3+$E).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
+	subq.w	#1,(PalCycle_Timer2).w
	bpl.s	++	; subq.w
	move.w	#3,(PalCycle_Timer2).w
	lea	(CyclingPal_WFZ1).l,a0
	move.w	(PalCycle_Frame2).w,d0
	addq.w	#2,(PalCycle_Frame2).w
	cmpi.w	#$44,(PalCycle_Frame2).w
	blo.s	+	; move.w
	move.w	#0,(PalCycle_Frame2).w
+	move.w	(a0,d0.w),(Normal_palette_line3+$1C).w
+
	subq.w	#1,(PalCycle_Timer3).w
	bpl.s	.return
	move.w	#5,(PalCycle_Timer3).w
	lea	(CyclingPal_WFZ2).l,a0
	move.w	(PalCycle_Frame3).w,d0
	addq.w	#2,(PalCycle_Frame3).w
	cmpi.w	#$18,(PalCycle_Frame3).w
	blo.s	+
	move.w	#0,(PalCycle_Frame3).w
+	move.w	(a0,d0.w),(Normal_palette_line3+$1E).w

.return:
	rts
; ===========================================================================

; ----------------------------------------------------------------------------
; word_1E5A:
	BINCLUDE "art/palettes/Title Water.bin"; S1 Title Screen Water palette (unused)
; word_1E7A:
CyclingPal_EHZ_ARZ_Water:
	BINCLUDE "art/palettes/EHZ ARZ Water.bin"; Emerald Hill/Aquatic Ruin Rotating Water palette
; word_1E9A:
CyclingPal_Lava:
	BINCLUDE "art/palettes/Hill Top Lava.bin"; Hill Top Lava palette
; word_1F1A:
CyclingPal_WoodConveyor:
	BINCLUDE "art/palettes/Wood Conveyor.bin"; Wood Conveyor Belts palette
; byte_1F2A:
CyclingPal_MTZ1:
	BINCLUDE "art/palettes/MTZ Cycle 1.bin"; Metropolis Cycle #1 palette
; word_1F36:
CyclingPal_MTZ2:
	BINCLUDE "art/palettes/MTZ Cycle 2.bin"; Metropolis Cycle #2 palette
; word_1F42:
CyclingPal_MTZ3:
	BINCLUDE "art/palettes/MTZ Cycle 3.bin"; Metropolis Cycle #3 palette
; word_1F56:
CyclingPal_HPZWater:
	BINCLUDE "art/palettes/HPZ Water Cycle.bin"; Hidden Palace Water Cycle
; word_1F66:
CyclingPal_HPZUnderwater:
	BINCLUDE "art/palettes/HPZ Underwater Cycle.bin"; Hidden Palace Underwater Cycle
; word_1F76:
CyclingPal_Oil:
	BINCLUDE "art/palettes/OOZ Oil.bin"; Oil Ocean Oil palette
; word_1F86:
CyclingPal_Lantern:
	BINCLUDE "art/palettes/MCZ Lantern.bin"; Mystic Cave Lanterns
; word_1F8E:
CyclingPal_CNZ1:
	BINCLUDE "art/palettes/CNZ Cycle 1.bin"; Casino Night Cycles 1 & 2
; word_1FB2:
CyclingPal_CNZ3:
	BINCLUDE "art/palettes/CNZ Cycle 3.bin"; Casino Night Cycle 3
; word_1FC4:
CyclingPal_CNZ4:
	BINCLUDE "art/palettes/CNZ Cycle 4.bin"; Casino Night Cycle 4
; word_1FEC:
CyclingPal_CNZ1_B:
	BINCLUDE "art/palettes/CNZ Boss Cycle 1.bin"; Casino Night Boss Cycle 1
; word_1FFE:
CyclingPal_CNZ2_B:
	BINCLUDE "art/palettes/CNZ Boss Cycle 2.bin"; Casino Night Boss Cycle 2
; word_2012:
CyclingPal_CNZ3_B:
	BINCLUDE "art/palettes/CNZ Boss Cycle 3.bin"; Casino Night Boss Cycle 3
; word_2022:
CyclingPal_CPZ1:
	BINCLUDE "art/palettes/CPZ Cycle 1.bin"; Chemical Plant Cycle 1
; word_2058:
CyclingPal_CPZ2:
	BINCLUDE "art/palettes/CPZ Cycle 2.bin"; Chemical Plant Cycle 2
; word_2082:
CyclingPal_CPZ3:
	BINCLUDE "art/palettes/CPZ Cycle 3.bin"; Chemical Plant Cycle 3
; word_20A2:
CyclingPal_WFZFire:
	BINCLUDE "art/palettes/WFZ Fire Cycle.bin"; Wing Fortress Fire Cycle palette
; word_20C2:
CyclingPal_WFZBelt:
	BINCLUDE "art/palettes/WFZ Conveyor Cycle.bin"; Wing Fortress Conveyor Belt Cycle palette
; word_20E2: CyclingPal_CPZ4:
CyclingPal_WFZ1:
	BINCLUDE "art/palettes/WFZ Cycle 1.bin"; Wing Fortress Flashing Light Cycle 1
; word_2126:
CyclingPal_WFZ2:
	BINCLUDE "art/palettes/WFZ Cycle 2.bin"; Wing Fortress Flashing Light Cycle 2
; ----------------------------------------------------------------------------


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_213E:
PalCycle_SuperSonic:
	move.b	(Super_Sonic_palette).w,d0
	beq.s	.return	; return, if Sonic isn't super
	bmi.w	.normal	; branch, if fade-in is done
	subq.b	#1,d0
	bne.s	.revert	; branch for values greater than 1

	; fade from Sonic's to Super Sonic's palette
	; run frame timer
	subq.b	#1,(Palette_timer).w
	bpl.s	.return
	move.b	#3,(Palette_timer).w

	; increment palette frame and update Sonic's palette
	lea	(CyclingPal_SSTransformation).l,a0
	move.w	(Palette_frame).w,d0
	addq.w	#8,(Palette_frame).w	; 1 palette entry = 1 word, Sonic uses 4 shades of blue
	cmpi.w	#$30,(Palette_frame).w	; has palette cycle reached the 6th frame?
	blo.s	+			; if not, branch
	move.b	#-1,(Super_Sonic_palette).w	; mark fade-in as done
	move.b	#0,(MainCharacter+obj_control).w	; restore Sonic's movement
+
	lea	(Normal_palette+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
    if fixBugs
	; underwater palettes
	lea	(CyclingPal_CPZUWTransformation).l,a0
	cmpi.b	#chemical_plant_zone,(Current_Zone).w
	beq.s	+
	cmpi.b	#aquatic_ruin_zone,(Current_Zone).w
	bne.s	.return
	lea	(CyclingPal_ARZUWTransformation).l,a0
+	lea	(Underwater_palette+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
    else
	; Note: The fade in for Sonic's underwater palette is missing.
	; Because of this, Super Sonic's transformation will be uncorrect
	; when underwater.
    endif
.return:
	rts
; ===========================================================================
; loc_2188: PalCycle_SuperSonic_revert:
.revert:	; runs the fade in transition backwards
	; run frame timer
	subq.b	#1,(Palette_timer).w
	bpl.s	.return
	move.b	#3,(Palette_timer).w

	; decrement palette frame and update Sonic's palette
	lea	(CyclingPal_SSTransformation).l,a0
	move.w	(Palette_frame).w,d0
	subq.w	#8,(Palette_frame).w	; previous frame
	bcc.s	+			; branch, if it isn't the first frame
    if fixBugs
	move.w	#0,(Palette_frame).w
    else
	; This does not clear the full variable, causing this palette cycle
	; to behave incorrectly the next time it is activated.
	move.b	#0,(Palette_frame).w
    endif
	move.b	#0,(Super_Sonic_palette).w	; stop palette cycle
+
	lea	(Normal_palette+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
	; underwater palettes
	lea	(CyclingPal_CPZUWTransformation).l,a0
	cmpi.b	#chemical_plant_zone,(Current_Zone).w
	beq.s	+
	cmpi.b	#aquatic_ruin_zone,(Current_Zone).w
	bne.s	.return
	lea	(CyclingPal_ARZUWTransformation).l,a0
+	lea	(Underwater_palette+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
	rts
; ===========================================================================
; loc_21E6: PalCycle_SuperSonic_normal:
.normal:
	; run frame timer
	subq.b	#1,(Palette_timer).w
	bpl.s	.return
	move.b	#7,(Palette_timer).w

	; increment palette frame and update Sonic's palette
	lea	(CyclingPal_SSTransformation).l,a0
	move.w	(Palette_frame).w,d0
	addq.w	#8,(Palette_frame).w	; next frame
	cmpi.w	#$78,(Palette_frame).w	; is it the last frame?
    if fixBugs
	bls.s	+			; if not, branch
    else
	; This condition causes the last frame to be skipped.
	blo.s	+			; if not, branch
    endif
	move.w	#$30,(Palette_frame).w	; reset frame counter (Super Sonic's normal palette cycle starts at $30. Everything before that is for the palette fade)
+
	lea	(Normal_palette+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
	; underwater palettes
	lea	(CyclingPal_CPZUWTransformation).l,a0
	cmpi.b	#chemical_plant_zone,(Current_Zone).w
	beq.s	+
	cmpi.b	#aquatic_ruin_zone,(Current_Zone).w
	bne.w	.return
	lea	(CyclingPal_ARZUWTransformation).l,a0
+	lea	(Underwater_palette+4).w,a1
	move.l	(a0,d0.w),(a1)+
	move.l	4(a0,d0.w),(a1)
	rts
; End of function PalCycle_SuperSonic

; ===========================================================================
;----------------------------------------------------------------------------
;Palette for transformation to Super Sonic
;----------------------------------------------------------------------------
; Pal_2246:
CyclingPal_SSTransformation:
	BINCLUDE	"art/palettes/Super Sonic transformation.bin"
;----------------------------------------------------------------------------
;Palette for transformation to Super Sonic while underwater in CPZ
;----------------------------------------------------------------------------
; Pal_22C6:
CyclingPal_CPZUWTransformation:
	BINCLUDE	"art/palettes/CPZWater SS transformation.bin"
;----------------------------------------------------------------------------
;Palette for transformation to Super Sonic while underwater in ARZ
;----------------------------------------------------------------------------
; Pal_2346:
CyclingPal_ARZUWTransformation:
	BINCLUDE	"art/palettes/ARZWater SS transformation.bin"

; ---------------------------------------------------------------------------
; Subroutine to fade in from black
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_23C6: Pal_FadeTo:
Pal_FadeFromBlack:
	move.w	#$3F,(Palette_fade_range).w
	moveq	#0,d0
	lea	(Normal_palette).w,a0
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0
	moveq	#0,d1
	move.b	(Palette_fade_length).w,d0
; loc_23DE: Pal_ToBlack:
.palettewrite:
	move.w	d1,(a0)+
	dbf	d0,.palettewrite	; fill palette with $000 (black)

	move.w	#$15,d4

.nextframe:
	move.b	#VintID_Fade,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.s	.UpdateAllColours
	bsr.w	RunPLC_RAM
	dbf	d4,.nextframe

	rts
; End of function Pal_FadeFromBlack

; ---------------------------------------------------------------------------
; Subroutine to update all colours once
; ---------------------------------------------------------------------------
; sub_23FE: Pal_FadeIn:
.UpdateAllColours:
	; Update above-water palette
	moveq	#0,d0
	lea	(Normal_palette).w,a0
	lea	(Target_palette).w,a1
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0
	adda.w	d0,a1

	move.b	(Palette_fade_length).w,d0

.nextcolour:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour

	tst.b	(Water_flag).w
	beq.s	.skipunderwater
	; Update underwater palette
	moveq	#0,d0
	lea	(Underwater_palette).w,a0
	lea	(Underwater_target_palette).w,a1
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0
	adda.w	d0,a1

	move.b	(Palette_fade_length).w,d0

.nextcolour2:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour2

.skipunderwater:
	rts

; ---------------------------------------------------------------------------
; Subroutine to update a single colour once
; ---------------------------------------------------------------------------
; sub_243E: Pal_AddColor:
.UpdateColour:
	move.w	(a1)+,d2
	move.w	(a0),d3
	cmp.w	d2,d3
	beq.s	.updatenone

;.updateblue:
	move.w	d3,d1
	addi.w	#$200,d1	; increase blue value
	cmp.w	d2,d1		; has blue reached threshold level?
	bhi.s	.updategreen	; if yes, branch
	move.w	d1,(a0)+	; update palette
	rts

; loc_2454: Pal_AddGreen:
.updategreen:
	move.w	d3,d1
	addi.w	#$20,d1		; increase green value
	cmp.w	d2,d1
	bhi.s	.updatered
	move.w	d1,(a0)+	; update palette
	rts

; loc_2462: Pal_AddRed:
.updatered:
	addq.w	#2,(a0)+	; increase red value
	rts

; loc_2466: Pal_AddNone:
.updatenone:
	addq.w	#2,a0
	rts


; ---------------------------------------------------------------------------
; Subroutine to fade out to black
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_246A: Pal_FadeFrom:
Pal_FadeToBlack:
	move.w	#$3F,(Palette_fade_range).w

	move.w	#$15,d4

.nextframe:
	move.b	#VintID_Fade,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.s	.UpdateAllColours
	bsr.w	RunPLC_RAM
	dbf	d4,.nextframe

	rts
; End of function Pal_FadeToBlack

; ---------------------------------------------------------------------------
; Subroutine to update all colours once
; ---------------------------------------------------------------------------
; sub_248A: Pal_FadeOut:
.UpdateAllColours:
	; Update above-water palette
	moveq	#0,d0
	lea	(Normal_palette).w,a0
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0

	move.b	(Palette_fade_length).w,d0
.nextcolour:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour

	; Notice how this one lacks a check for
	; if Water_flag is set, unlike Pal_FadeFromBlack?

	; Update underwater palette
	moveq	#0,d0
	lea	(Underwater_palette).w,a0
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0

	move.b	(Palette_fade_length).w,d0
.nextcolour2:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour2

	rts

; ---------------------------------------------------------------------------
; Subroutine to update a single colour once
; ---------------------------------------------------------------------------
; sub_24B8: Pal_DecColor:
.UpdateColour:
	move.w	(a0),d2
	beq.s	.updatenone
;.updatered:
	move.w	d2,d1
	andi.w	#$E,d1
	beq.s	.updategreen
	subq.w	#2,(a0)+	; decrease red value
	rts

; loc_24C8: Pal_DecGreen:
.updategreen:
	move.w	d2,d1
	andi.w	#$E0,d1
	beq.s	.updateblue
	subi.w	#$20,(a0)+	; decrease green value
	rts

; loc_24D6: Pal_DecBlue:
.updateblue:
	move.w	d2,d1
	andi.w	#$E00,d1
	beq.s	.updatenone
	subi.w	#$200,(a0)+	; decrease blue value
	rts

; loc_24E4: Pal_DecNone:
.updatenone:
	addq.w	#2,a0
	rts


; ---------------------------------------------------------------------------
; Subroutine to fade in from white
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_24E8: Pal_MakeWhite:
Pal_FadeFromWhite:
	move.w	#$3F,(Palette_fade_range).w
	moveq	#0,d0
	lea	(Normal_palette).w,a0
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0
	move.w	#$EEE,d1

	move.b	(Palette_fade_length).w,d0

.palettewrite:
	move.w	d1,(a0)+
	dbf	d0,.palettewrite

	move.w	#$15,d4

.nextframe:
	move.b	#VintID_Fade,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.s	.UpdateAllColours
	bsr.w	RunPLC_RAM
	dbf	d4,.nextframe

	rts
; End of function Pal_FadeFromWhite

; ---------------------------------------------------------------------------
; Subroutine to update all colours once
; ---------------------------------------------------------------------------
; sub_2522: Pal_WhiteToBlack:
.UpdateAllColours:
	; Update above-water palette
	moveq	#0,d0
	lea	(Normal_palette).w,a0
	lea	(Target_palette).w,a1
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0
	adda.w	d0,a1

	move.b	(Palette_fade_length).w,d0

.nextcolour:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour

	tst.b	(Water_flag).w
	beq.s	.skipunderwater
	; Update underwater palette
	moveq	#0,d0
	lea	(Underwater_palette).w,a0
	lea	(Underwater_target_palette).w,a1
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0
	adda.w	d0,a1

	move.b	(Palette_fade_length).w,d0

.nextcolour2:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour2

.skipunderwater:
	rts

; ---------------------------------------------------------------------------
; Subroutine to update a single colour once
; ---------------------------------------------------------------------------
; sub_2562: Pal_DecColor2:
.UpdateColour:
	move.w	(a1)+,d2
	move.w	(a0),d3
	cmp.w	d2,d3
	beq.s	.updatenone
;.updateblue:
	move.w	d3,d1
	subi.w	#$200,d1	; decrease blue value
	bcs.s	.updategreen
	cmp.w	d2,d1
	blo.s	.updategreen
	move.w	d1,(a0)+
	rts

; loc_257A: Pal_DecGreen2:
.updategreen:
	move.w	d3,d1
	subi.w	#$20,d1	; decrease green value
	bcs.s	.updatered
	cmp.w	d2,d1
	blo.s	.updatered
	move.w	d1,(a0)+
	rts

; loc_258A: Pal_DecRed2:
.updatered:
	subq.w	#2,(a0)+	; decrease red value
	rts

; loc_258E: Pal_DecNone2:
.updatenone:
	addq.w	#2,a0
	rts


; ---------------------------------------------------------------------------
; Subroutine to fade out to white (used when you enter a special stage)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_2592: Pal_MakeFlash:
Pal_FadeToWhite:
	move.w	#$3F,(Palette_fade_range).w

	move.w	#$15,d4

.nextframe:
	move.b	#VintID_Fade,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.s	.UpdateAllColours
	bsr.w	RunPLC_RAM
	dbf	d4,.nextframe

	rts
; End of function Pal_FadeToWhite

; ---------------------------------------------------------------------------
; Subroutine to update all colours once
; ---------------------------------------------------------------------------
; sub_25B2: Pal_ToWhite:
.UpdateAllColours:
	; Update above-water palette
	moveq	#0,d0
	lea	(Normal_palette).w,a0
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0

	move.b	(Palette_fade_length).w,d0

.nextcolour:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour

	; Notice how this one lacks a check for
	; if Water_flag is set, unlike Pal_FadeFromWhite?

	; Update underwater palette
	moveq	#0,d0
	lea	(Underwater_palette).w,a0
	move.b	(Palette_fade_start).w,d0
	adda.w	d0,a0

	move.b	(Palette_fade_length).w,d0

.nextcolour2:
	bsr.s	.UpdateColour
	dbf	d0,.nextcolour2

	rts

; ---------------------------------------------------------------------------
; Subroutine to update a single colour once
; ---------------------------------------------------------------------------
; sub_25E0: Pal_AddColor2:
.UpdateColour:
	move.w	(a0),d2
	cmpi.w	#$EEE,d2
	beq.s	.updatenone
;.updatered:
	move.w	d2,d1
	andi.w	#$E,d1
	cmpi.w	#$E,d1
	beq.s	.updategreen
	addq.w	#2,(a0)+	; increase red value
	rts

; loc_25F8: Pal_AddGreen2:
.updategreen:
	move.w	d2,d1
	andi.w	#$E0,d1
	cmpi.w	#$E0,d1
	beq.s	.updateblue
	addi.w	#$20,(a0)+	; increase green value
	rts

; loc_260A: Pal_AddBlue2:
.updateblue:
	move.w	d2,d1
	andi.w	#$E00,d1
	cmpi.w	#$E00,d1
	beq.s	.updatenone
	addi.w	#$200,(a0)+	; increase blue value
	rts

; loc_261C: Pal_AddNone2:
.updatenone:
	addq.w	#2,a0
	rts
; End of function Pal_AddColor2


; Unused - dead code/data for old SEGA screen:

; ===========================================================================
; PalCycle_Sega:
	tst.b	(PalCycle_Timer+1).w
	bne.s	loc_2680
	lea	(Normal_palette_line2).w,a1
	lea	(Pal_Sega1).l,a0
	moveq	#5,d1
	move.w	(PalCycle_Frame).w,d0

loc_2636:
	bpl.s	loc_2640
	addq.w	#2,a0
	subq.w	#1,d1
	addq.w	#2,d0
	bra.s	loc_2636
; ===========================================================================

loc_2640:
	move.w	d0,d2
	andi.w	#$1E,d2
	bne.s	loc_264A
	addq.w	#2,d0

loc_264A:
	cmpi.w	#$60,d0
	bhs.s	loc_2654
	move.w	(a0)+,(a1,d0.w)

loc_2654:
	addq.w	#2,d0
	dbf	d1,loc_2640
	move.w	(PalCycle_Frame).w,d0
	addq.w	#2,d0
	move.w	d0,d2
	andi.w	#$1E,d2
	bne.s	loc_266A
	addq.w	#2,d0

loc_266A:
	cmpi.w	#$64,d0
	blt.s	loc_2678
	move.w	#$401,(PalCycle_Timer).w
	moveq	#-$C,d0

loc_2678:
	move.w	d0,(PalCycle_Frame).w
	moveq	#1,d0
	rts
; ===========================================================================

loc_2680:
	subq.b	#1,(PalCycle_Timer).w
	bpl.s	loc_26D2
	move.b	#4,(PalCycle_Timer).w
	move.w	(PalCycle_Frame).w,d0
	addi.w	#$C,d0
	cmpi.w	#$30,d0
	blo.s	loc_269E
	moveq	#0,d0
	rts
; ===========================================================================

loc_269E:
	move.w	d0,(PalCycle_Frame).w
	lea	(Pal_Sega2).l,a0
	lea	(a0,d0.w),a0
	lea	(Normal_palette+4).w,a1
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+
	move.w	(a0)+,(a1)
	lea	(Normal_palette_line2).w,a1
	moveq	#0,d0
	moveq	#$2C,d1

loc_26BE:
	move.w	d0,d2
	andi.w	#$1E,d2
	bne.s	loc_26C8
	addq.w	#2,d0

loc_26C8:
	move.w	(a0),(a1,d0.w)
	addq.w	#2,d0
	dbf	d1,loc_26BE

loc_26D2:
	moveq	#1,d0
	rts

; ===========================================================================
;----------------------------------------------------------------------------
; Unused palette for the Sega logo
;----------------------------------------------------------------------------
; Pal_26D6:
Pal_Sega1:	BINCLUDE	"art/palettes/Unused Sega logo.bin"
;----------------------------------------------------------------------------
; Unused palette for the Sega logo (fading?)
;----------------------------------------------------------------------------
; Pal_26E2:
Pal_Sega2:	BINCLUDE	"art/palettes/Unused Sega logo 2.bin"

; end of dead code/data

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_2712: PalLoad1:
PalLoad_ForFade:
	lea	(PalPointers).l,a1
	lsl.w	#3,d0
	adda.w	d0,a1
	movea.l	(a1)+,a2
	movea.w	(a1)+,a3
	adda.w	#Target_palette-Normal_palette,a3

	move.w	(a1)+,d7
-	move.l	(a2)+,(a3)+
	dbf	d7,-

	rts
; End of function PalLoad_ForFade


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_272E: PalLoad2:
PalLoad_Now:
	lea	(PalPointers).l,a1
	lsl.w	#3,d0
	adda.w	d0,a1
	movea.l	(a1)+,a2
	movea.w	(a1)+,a3

	move.w	(a1)+,d7
-	move.l	(a2)+,(a3)+
	dbf	d7,-

	rts
; End of function PalLoad_Now


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_2746: PalLoad3_Water:
PalLoad_Water_Now:
	lea	(PalPointers).l,a1
	lsl.w	#3,d0
	adda.w	d0,a1
	movea.l	(a1)+,a2
	movea.w	(a1)+,a3
	suba.l	#Normal_palette-Underwater_palette,a3

	move.w	(a1)+,d7
-	move.l	(a2)+,(a3)+
	dbf	d7,-

	rts
; End of function PalLoad_Water_Now


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_2764: PalLoad4_Water:
PalLoad_Water_ForFade:
	lea	(PalPointers).l,a1
	lsl.w	#3,d0
	adda.w	d0,a1
	movea.l	(a1)+,a2
	movea.w	(a1)+,a3
	suba.l	#Normal_palette-Underwater_target_palette,a3

	move.w	(a1)+,d7
-	move.l	(a2)+,(a3)+
	dbf	d7,-

	rts
; End of function PalLoad_Water_ForFade

; ===========================================================================
;----------------------------------------------------------------------------
; Palette pointers
; (PALETTE DESCRIPTOR ARRAY)
; This struct array defines the palette to use for each level.
;----------------------------------------------------------------------------

palptr	macro	ptr,lineno
	dc.l ptr	; Pointer to palette
	dc.w (Normal_palette+lineno*palette_line_size)&$FFFF	; Location in ram to load palette into
	dc.w bytesToLcnt(ptr_End-ptr)	; Size of palette in (bytes / 4)
	endm

PalPointers:
PalPtr_SEGA:	palptr Pal_SEGA,  0
PalPtr_Title:	palptr Pal_Title, 1
PalPtr_MenuB:	palptr Pal_MenuB, 0
PalPtr_BGND:	palptr Pal_BGND,  0
PalPtr_EHZ:	palptr Pal_EHZ,   1
PalPtr_EHZ2:	palptr Pal_EHZ,   1
PalPtr_WZ:	palptr Pal_WZ,    1
PalPtr_EHZ3:	palptr Pal_EHZ,   1
PalPtr_MTZ:	palptr Pal_MTZ,   1
PalPtr_MTZ2:	palptr Pal_MTZ,   1
PalPtr_WFZ:	palptr Pal_WFZ,   1
PalPtr_HTZ:	palptr Pal_HTZ,   1
PalPtr_HPZ:	palptr Pal_HPZ,   1
PalPtr_EHZ4:	palptr Pal_EHZ,   1
PalPtr_OOZ:	palptr Pal_OOZ,   1
PalPtr_MCZ:	palptr Pal_MCZ,   1
PalPtr_CNZ:	palptr Pal_CNZ,   1
PalPtr_CPZ:	palptr Pal_CPZ,   1
PalPtr_DEZ:	palptr Pal_DEZ,   1
PalPtr_ARZ:	palptr Pal_ARZ,   1
PalPtr_SCZ:	palptr Pal_SCZ,   1
PalPtr_HPZ_U:	palptr Pal_HPZ_U, 0
PalPtr_CPZ_U:	palptr Pal_CPZ_U, 0
PalPtr_ARZ_U:	palptr Pal_ARZ_U, 0
PalPtr_SS:	palptr Pal_SS,    0
PalPtr_MCZ_B:	palptr Pal_MCZ_B, 1
PalPtr_CNZ_B:	palptr Pal_CNZ_B, 1
PalPtr_SS1:	palptr Pal_SS1,   3
PalPtr_SS2:	palptr Pal_SS2,   3
PalPtr_SS3:	palptr Pal_SS3,   3
PalPtr_SS4:	palptr Pal_SS4,   3
PalPtr_SS5:	palptr Pal_SS5,   3
PalPtr_SS6:	palptr Pal_SS6,   3
PalPtr_SS7:	palptr Pal_SS7,   3
PalPtr_SS1_2p:	palptr Pal_SS1_2p,3
PalPtr_SS2_2p:	palptr Pal_SS2_2p,3
PalPtr_SS3_2p:	palptr Pal_SS3_2p,3
PalPtr_OOZ_B:	palptr Pal_OOZ_B, 1
PalPtr_Menu:	palptr Pal_Menu,  0
PalPtr_Result:	palptr Pal_Result,0

; ----------------------------------------------------------------------------
; This macro defines Pal_ABC and Pal_ABC_End, so palptr can compute the size of
; the palette automatically
; path2 is used for the Sonic and Tails palette, which has 2 palette lines
palette macro {INTLABEL},path,path2
__LABEL__ label *
	BINCLUDE "art/palettes/path"
    if "path2"<>""
	BINCLUDE "art/palettes/path2"
    endif
__LABEL___End label *
	endm

Pal_SEGA:  palette Sega screen.bin ; SEGA screen palette (Sonic and initial background)
Pal_Title: palette Title screen.bin ; Title screen Palette
Pal_MenuB: palette S2B Level Select.bin ; Leftover S2B level select palette
Pal_BGND:  palette SonicAndTails.bin,SonicAndTails2.bin ; "Sonic and Miles" background palette (also usually the primary palette line)
Pal_EHZ:   palette EHZ.bin ; Emerald Hill Zone palette
Pal_WZ:    palette Wood Zone.bin ; Wood Zone palette
Pal_MTZ:   palette MTZ.bin ; Metropolis Zone palette
Pal_WFZ:   palette WFZ.bin ; Wing Fortress Zone palette
Pal_HTZ:   palette HTZ.bin ; Hill Top Zone palette
Pal_HPZ:   palette HPZ.bin ; Hidden Palace Zone palette
Pal_HPZ_U: palette HPZ underwater.bin ; Hidden Palace Zone underwater palette
Pal_OOZ:   palette OOZ.bin ; Oil Ocean Zone palette
Pal_MCZ:   palette MCZ.bin ; Mystic Cave Zone palette
Pal_CNZ:   palette CNZ.bin ; Casino Night Zone palette
Pal_CPZ:   palette CPZ.bin ; Chemical Plant Zone palette
Pal_CPZ_U: palette CPZ underwater.bin ; Chemical Plant Zone underwater palette
Pal_DEZ:   palette DEZ.bin ; Death Egg Zone palette
Pal_ARZ:   palette ARZ.bin ; Aquatic Ruin Zone palette
Pal_ARZ_U: palette ARZ underwater.bin ; Aquatic Ruin Zone underwater palette
Pal_SCZ:   palette SCZ.bin ; Sky Chase Zone palette
Pal_MCZ_B: palette MCZ Boss.bin ; Mystic Cave Zone boss palette
Pal_CNZ_B: palette CNZ Boss.bin ; Casino Night Zone boss palette
Pal_OOZ_B: palette OOZ Boss.bin ; Oil Ocean Zone boss palette
Pal_Menu:  palette Menu.bin ; Menu palette
Pal_SS:    palette Special Stage Main.bin ; Special Stage palette
Pal_SS1:   palette Special Stage 1.bin ; Special Stage 1 palette
Pal_SS2:   palette Special Stage 2.bin ; Special Stage 2 palette
Pal_SS3:   palette Special Stage 3.bin ; Special Stage 3 palette
Pal_SS4:   palette Special Stage 4.bin ; Special Stage 4 palette
Pal_SS5:   palette Special Stage 5.bin ; Special Stage 5 palette
Pal_SS6:   palette Special Stage 6.bin ; Special Stage 6 palette
Pal_SS7:   palette Special Stage 7.bin ; Special Stage 7 palette
Pal_SS1_2p:palette Special Stage 1 2p.bin ; Special Stage 1 2p palette
Pal_SS2_2p:palette Special Stage 2 2p.bin ; Special Stage 2 2p palette
Pal_SS3_2p:palette Special Stage 3 2p.bin ; Special Stage 3 2p palette
Pal_Result:palette Special Stage Results Screen.bin ; Special Stage Results Screen palette
; ===========================================================================

    if gameRevision<2
	nop
    endif




; ---------------------------------------------------------------------------
; Subroutine to perform vertical synchronization
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_3384: DelayProgram:
WaitForVint:
	move	#$2300,sr

-	tst.b	(Vint_routine).w
	bne.s	-
	rts
; End of function WaitForVint


; ---------------------------------------------------------------------------
; Subroutine to generate a pseudo-random number in d0
; d0 = (RNG & $FFFF0000) | ((RNG*41 & $FFFF) + ((RNG*41 & $FFFF0000) >> 16))
; RNG = ((RNG*41 + ((RNG*41 & $FFFF) << 16)) & $FFFF0000) | (RNG*41 & $FFFF)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_3390:
RandomNumber:
	move.l	(RNG_seed).w,d1
	bne.s	+
	move.l	#$2A6D365A,d1 ; if the RNG is 0, reset it to this crazy number

	; set the high word of d0 to be the high word of the RNG
	; and multiply the RNG by 41
+	move.l	d1,d0
	asl.l	#2,d1
	add.l	d0,d1
	asl.l	#3,d1
	add.l	d0,d1

	; add the low word of the RNG to the high word of the RNG
	; and set the low word of d0 to be the result
	move.w	d1,d0
	swap	d1
	add.w	d1,d0
	move.w	d0,d1
	swap	d1

	move.l	d1,(RNG_seed).w
	rts
; End of function RandomNumber


; ---------------------------------------------------------------------------
; Subroutine to calculate sine and cosine of an angle
; d0 = input byte = angle (360 degrees == 256)
; d0 = output word = 255 * sine(angle)
; d1 = output word = 255 * cosine(angle)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_33B6:
CalcSine:
	andi.w	#$FF,d0
	add.w	d0,d0
	addi.w	#$80,d0
	move.w	Sine_Data(pc,d0.w),d1 ; cos
	subi.w	#$80,d0
	move.w	Sine_Data(pc,d0.w),d0 ; sin
	rts
; End of function CalcSine

; ===========================================================================
; word_33CE:
Sine_Data:	BINCLUDE	"misc/sinewave.bin"


; ---------------------------------------------------------------------------
; Subroutine to calculate arctangent of y/x
; d1 = input x
; d2 = input y
; d0 = output angle (360 degrees == 256)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_364E:
CalcAngle:
	movem.l	d3-d4,-(sp)
	moveq	#0,d3
	moveq	#0,d4
	move.w	d1,d3
	move.w	d2,d4
	or.w	d3,d4
	beq.s	CalcAngle_Zero ; special case return if x and y are both 0
	move.w	d2,d4

	absw.w	d3	; calculate absolute value of x
	absw.w	d4	; calculate absolute value of y
	cmp.w	d3,d4
	bhs.w	+
	lsl.l	#8,d4
	divu.w	d3,d4
	moveq	#0,d0
	move.b	Angle_Data(pc,d4.w),d0
	bra.s	++
+
	lsl.l	#8,d3
	divu.w	d4,d3
	moveq	#$40,d0
	sub.b	Angle_Data(pc,d3.w),d0
+
	tst.w	d1
	bpl.w	+
	neg.w	d0
	addi.w	#$80,d0
+
	tst.w	d2
	bpl.w	+
	neg.w	d0
	addi.w	#$100,d0
+
	movem.l	(sp)+,d3-d4
	rts
; ===========================================================================
; loc_36AA:
CalcAngle_Zero:
	move.w	#$40,d0
	movem.l	(sp)+,d3-d4
	rts
; End of function CalcAngle

; ===========================================================================
; byte_36B4:
Angle_Data:	BINCLUDE	"misc/angles.bin"

; ===========================================================================

    if gameRevision<2
	nop
    endif




; loc_37B8:
SegaScreen:
	move.b	#MusID_Stop,d0
	bsr.w	PlayMusic ; stop music
	bsr.w	ClearPLC
	bsr.w	Pal_FadeToBlack

	clearRAM Misc_Variables,Misc_Variables_End

	clearRAM Object_RAM,Object_RAM_End ; fill object RAM with 0

	lea	(VDP_control_port).l,a6
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8200|(VRAM_SegaScr_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_SegaScr_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $A000
	move.w	#$8700,(a6)		; Background palette/color: 0/0
	move.w	#$8B03,(a6)		; EXT-INT disabled, V scroll by screen, H scroll by line
	move.w	#$8C81,(a6)		; H res 40 cells, no interlace, S/H disabled
	move.w	#$9003,(a6)		; Scroll table size: 128x32 ($2000 bytes)
	clr.b	(Water_fullscreen_flag).w
	clr.w	(Two_player_mode).w
	move	#$2700,sr
	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	ClearScreen

	dmaFillVRAM 0,VRAM_SegaScr_Plane_A_Name_Table,VRAM_SegaScr_Plane_Table_Size ; clear Plane A pattern name table

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Sega_Logo),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_SEGA).l,a0
	bsr.w	NemDec

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Trails),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_IntroTrails).l,a0
	bsr.w	NemDec

	; This gets overwritten by the upscaled Sonic sprite. This may have
	; been used to test the Sega screen before the sprite upscaling logic
	; was added.
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtUnc_Giant_Sonic),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_SilverSonic).l,a0
	bsr.w	NemDec

	lea	(Chunk_Table).l,a1
	lea	(MapEng_SEGA).l,a0
	move.w	#make_art_tile(ArtTile_VRAM_Start,0,0),d0
	bsr.w	EniDec

	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_SegaScr_Plane_B_Name_Table,VRAM,WRITE),d0
	moveq	#40-1,d1	; 40 cells wide
	moveq	#28-1,d2	; 28 cells tall
	bsr.w	PlaneMapToVRAM_H80_Sega

	tst.b	(Graphics_Flags).w ; are we on a Japanese Mega Drive?
	bmi.s	SegaScreen_Contin ; if not, branch

	; load an extra sprite to hide the TM (trademark) symbol on the SEGA screen
	lea	(SegaHideTM).w,a1
	move.b	#ObjID_SegaHideTM,id(a1)	; load objB1 at $FFFFB080
	move.b	#$4E,subtype(a1) ; <== ObjB1_SubObjData
; loc_38CE:
SegaScreen_Contin:
	moveq	#PalID_SEGA,d0
	bsr.w	PalLoad_Now
	move.w	#-$A,(PalCycle_Frame).w
	move.w	#0,(PalCycle_Timer).w
	move.w	#0,(SegaScr_VInt_Subrout).w
	move.w	#0,(SegaScr_PalDone_Flag).w
	lea	(SegaScreenObject).w,a1
	move.b	#ObjID_SonicOnSegaScr,id(a1) ; load objB0 (sega screen?) at $FFFFB040
	move.b	#$4C,subtype(a1) ; <== ObjB0_SubObjData
	move.w	#4*60,(Demo_Time_left).w	; 4 seconds
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
; loc_390E:
Sega_WaitPalette:
	move.b	#VintID_SEGA,(Vint_routine).w
	bsr.w	WaitForVint
	jsrto	RunObjects, JmpTo_RunObjects
	jsr	(BuildSprites).l
	tst.b	(SegaScr_PalDone_Flag).w
	beq.s	Sega_WaitPalette
    if ~~fixBugs
	; This is a leftover from Sonic 1: ObjB0 plays the Sega sound now.
	; Normally, you'll only hear one Sega sound, but the game actually
	; tries to play it twice. The only reason it doesn't is because the
	; sound queue only has room for one sound per frame. Some custom
	; sound drivers don't have this limitation, however, and the sound
	; will indeed play twice in those.
	move.b	#SndID_SegaSound,d0
	bsr.w	PlaySound	; play "SEGA" sound
    endif
	move.b	#VintID_SEGA,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	#3*60,(Demo_Time_left).w	; 3 seconds
; loc_3940:
Sega_WaitEnd:
	move.b	#VintID_PCM,(Vint_routine).w
	bsr.w	WaitForVint
	tst.w	(Demo_Time_left).w
	beq.s	Sega_GotoTitle
	move.b	(Ctrl_1_Press).w,d0	; is Start button pressed?
	or.b	(Ctrl_2_Press).w,d0	; (either player)
	andi.b	#button_start_mask,d0
	beq.s	Sega_WaitEnd		; if not, branch
; loc_395E:
Sega_GotoTitle:
	clr.w	(SegaScr_PalDone_Flag).w
	clr.w	(SegaScr_VInt_Subrout).w
	move.b	#GameModeID_TitleScreen,(Game_Mode).w	; => TitleScreen
	rts

; ---------------------------------------------------------------------------
; Subroutine that does the exact same thing as PlaneMapToVRAM_H80_SpecialStage
; (this one is used at the Sega screen)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_396E: ShowVDPGraphics3: PlaneMapToVRAM3:
PlaneMapToVRAM_H80_Sega:
	lea	(VDP_data_port).l,a6
	move.l	#vdpCommDelta(planeLoc(128,0,1)),d4	; $1000000
-	move.l	d0,VDP_control_port-VDP_data_port(a6)
	move.w	d1,d3
-	move.w	(a1)+,(a6)
	dbf	d3,-
	add.l	d4,d0
	dbf	d2,--
	rts
; End of function PlaneMapToVRAM_H80_Sega

; ===========================================================================

    if gameRevision<2
	nop
    endif

    if ~~removeJmpTos
; sub_3990:
JmpTo_RunObjects ; JmpTo
	jmp	(RunObjects).l

	align 4
    endif




; ===========================================================================
; loc_3998:
TitleScreen:
	; Stop music.
	move.b	#MusID_Stop,d0
	bsr.w	PlayMusic

	; Clear the PLC queue, preventing any PLCs from before loading after this point.
	bsr.w	ClearPLC

	; Fade out.
	bsr.w	Pal_FadeToBlack

	; Disable interrupts, so that we can have exclusive access to the VDP.
	move	#$2700,sr

	; Configure the VDP for this screen mode.
	lea	(VDP_control_port).l,a6
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8200|(VRAM_TtlScr_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_TtlScr_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $E000
	move.w	#$9001,(a6)		; Scroll table size: 64x32
	move.w	#$9200,(a6)		; Disable window
	move.w	#$8B03,(a6)		; EXT-INT disabled, V scroll by screen, H scroll by line
	move.w	#$8720,(a6)		; Background palette/color: 2/0

	clr.b	(Water_fullscreen_flag).w

	move.w	#$8C81,(a6)		; H res 40 cells, no interlace, S/H disabled

	; Reset plane maps, sprite table, and scroll tables.
	bsr.w	ClearScreen

	; Reset a bunch of engine state.
	clearRAM Object_Display_Lists,Object_Display_Lists_End ; fill $AC00-$AFFF with $0
	clearRAM Object_RAM,Object_RAM_End ; fill object RAM ($B000-$D5FF) with $0
	clearRAM Misc_Variables,Misc_Variables_End ; clear CPU player RAM and following variables
	clearRAM Camera_RAM,Camera_RAM_End ; clear camera RAM and following variables

	; Load the credit font for the following text.
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_CreditText),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_CreditText).l,a0
	bsr.w	NemDec

	; Load the 'Sonic and Miles 'Tails' Prower in' text.
	lea	(off_B2B0).l,a1
	jsr	(loc_B272).l

	; Fade-in, showing the text that was just loaded.
	clearRAM Target_palette,Target_palette_End	; fill palette with 0 (black)
	moveq	#PalID_BGND,d0
	bsr.w	PalLoad_ForFade
	bsr.w	Pal_FadeFromBlack

	; 'Pal_FadeFromBlack' enabled the interrupts, so disable them again
	; so that we have exclusive access to the VDP for the following calls
	; to the Nemesis decompressor.
	move	#$2700,sr

	; Load assets while the above text is being displayed.
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Title),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Title).l,a0
	bsr.w	NemDec

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_TitleSprites),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_TitleSprites).l,a0
	bsr.w	NemDec

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_MenuJunk),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_MenuJunk).l,a0
	bsr.w	NemDec

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Player1VS2),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Player1VS2).l,a0
	bsr.w	NemDec

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_FontStuff_TtlScr),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_FontStuff).l,a0
	bsr.w	NemDec

	; Clear some variables.
	move.b	#0,(Last_star_pole_hit).w
	move.b	#0,(Last_star_pole_hit_2P).w
	move.w	#0,(Debug_placement_mode).w
	move.w	#0,(Demo_mode_flag).w
	move.w	#0,(unk_FFDA).w
	move.w	#0,(PalCycle_Timer).w
	move.w	#0,(Two_player_mode).w
	move.b	#0,(Level_started_flag).w

	; And finally fade out.
	bsr.w	Pal_FadeToBlack

	; 'Pal_FadeToBlack' enabled the interrupts, so disable them again
	; so that we have exclusive access to the VDP for the following calls
	; to the plane map loader.
	move	#$2700,sr

	; Decompress the first part of the title screen background plane map...
	lea	(Chunk_Table).l,a1
	lea	(MapEng_TitleScreen).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_Title,2,0),d0
	bsr.w	EniDec

	; ...and send it to VRAM.
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_TtlScr_Plane_B_Name_Table,VRAM,WRITE),d0
	moveq	#40-1,d1 ; Width
	moveq	#28-1,d2 ; Height
	jsrto	PlaneMapToVRAM_H40, PlaneMapToVRAM_H40

	; Decompress the second part of the title screen background plane map...
	lea	(Chunk_Table).l,a1
	lea	(MapEng_TitleBack).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_Title,2,0),d0
	bsr.w	EniDec

	; ...and send it to VRAM.
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_TtlScr_Plane_B_Name_Table+planeLoc(64,40,0),VRAM,WRITE),d0
	moveq	#24-1,d1 ; Width
	moveq	#28-1,d2 ; Height
	jsrto	PlaneMapToVRAM_H40, PlaneMapToVRAM_H40

	; Decompress the title screen emblem plane map...
	lea	(Chunk_Table).l,a1
	lea	(MapEng_TitleLogo).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_Title,3,1),d0
	bsr.w	EniDec

	; ...add the copyright text to it...
	lea	(Chunk_Table+planeLoc(40,28,26)).l,a1
	lea	(CopyrightText).l,a2
	moveq	#bytesToWcnt(CopyrightText_End-CopyrightText),d6
-	move.w	(a2)+,(a1)+
	dbf	d6,-

	; ...and send it to VRAM.
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_TtlScr_Plane_A_Name_Table,VRAM,WRITE),d0
	moveq	#40-1,d1 ; Width
	moveq	#28-1,d2 ; Height
	jsrto	PlaneMapToVRAM_H40, PlaneMapToVRAM_H40

	; Clear the palette.
	clearRAM Normal_palette,Target_palette_End

	; Load the title screen palette, so we can fade into it later.
	moveq	#PalID_Title,d0
	bsr.w	PalLoad_ForFade

	; Reset some variables.
	move.b	#0,(Debug_mode_flag).w
	move.w	#0,(Two_player_mode).w

	; Set the time that the title screen lasts (little over ten seconds).
	move.w	#60*10+40,(Demo_Time_left).w

	; Clear the player's inputs, to prevent a leftover input from
	; skipping the intro.
	clr.w	(Ctrl_1).w

	; Load the object responsible for the intro animation.
	move.b	#ObjID_TitleIntro,(IntroSonic+id).w
	move.b	#2,(IntroSonic+subtype).w

	; Run it for a frame, so that it initialises.
	jsr	(RunObjects).l
	jsr	(BuildSprites).l

	; Load some standard sprites.
	moveq	#PLCID_Std1,d0
	bsr.w	LoadPLC2

	; Reset the cheat input state.
	move.w	#0,(Correct_cheat_entries).w
	move.w	#0,(Correct_cheat_entries_2).w

	; I do not know why these are here.
	nop
	nop
	nop
	nop
	nop
	nop

	; Reset Sonic's position record buffer.
	move.w	#4,(Sonic_Pos_Record_Index).w
	move.w	#0,(Sonic_Pos_Record_Buf).w

	; Reset the two player mode results data.
	lea	(Results_Data_2P).w,a1
	moveq	#bytesToWcnt(Results_Data_2P_End-Results_Data_2P),d0
-	move.w	#-1,(a1)+
	dbf	d0,-

	; Initialise the camera's X position.
	move.w	#-$280,(Camera_X_pos).w

	; Enable the VDP's display.
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l

	; Fade into the palette that was loaded earlier.
	bsr.w	Pal_FadeFromBlack

; loc_3C14:
TitleScreen_Loop:
	move.b	#VintID_Title,(Vint_routine).w
	bsr.w	WaitForVint

	jsr	(RunObjects).l
	jsrto	SwScrl_Title, JmpTo_SwScrl_Title
	jsr	(BuildSprites).l

	; Find the masking sprite, and move it to the proper location. The
	; sprite is normally at X 128+128, but in order to perform masking,
	; it must be at X 0.
	; The masking sprite is used to stop Sonic and Tails from overlapping
	; the emblem.
	; You might be wondering why it alternates between 0 and 4 for the X
	; position. That's because masking sprites only work if another
	; sprite rendered before them (or if the previous scanline reached
	; its pixel limit). Because of this, a sprite is placed at X 4 before
	; a second one is placed at X 0.
	lea	(Sprite_Table+4).w,a1
	moveq	#0,d0

	moveq	#(Sprite_Table_End-Sprite_Table)/8-1,d6
-	tst.w	(a1)	; The masking sprite has its art-tile set to $0000.
	bne.s	+
	bchg	#2,d0	; Alternate between X positions of 0 and 4.
	move.w	d0,2(a1)
+	addq.w	#8,a1
	dbf	d6,-

	bsr.w	RunPLC_RAM
	bsr.w	TailsNameCheat

	; If the timer has run out, go play a demo.
	tst.w	(Demo_Time_left).w
	beq.w	TitleScreen_Demo

	; If the intro is still playing, then don't let the start button
	; begin the game.
	tst.b	(IntroSonic+obj0e_intro_complete).w
	beq.w	TitleScreen_Loop

	; If the start button has not been pressed, then loop back and keep
	; running the title screen.
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	andi.b	#button_start_mask,d0
	beq.w	TitleScreen_Loop ; loop until Start is pressed

	; At this point, the start button has been pressed and it's time to
	; enter one player mode, two player mode, or the options menu.

	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)

	move.b	#3,(Life_count).w
	move.b	#3,(Life_count_2P).w

	moveq	#0,d0
	move.w	d0,(Ring_count).w
	move.l	d0,(Timer).w
	move.l	d0,(Score).w
	move.w	d0,(Ring_count_2P).w
	move.l	d0,(Timer_2P).w
	move.l	d0,(Score_2P).w
	move.b	d0,(Continue_count).w

	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w

	move.b	#MusID_FadeOut,d0 ; prepare to stop music (fade out)
	bsr.w	PlaySound

	moveq	#0,d0
	move.b	(Title_screen_option).w,d0
	bne.s	TitleScreen_CheckIfChose2P	; branch if not a 1-player game

	moveq	#0,d0
	move.w	d0,(Two_player_mode_copy).w
	move.w	d0,(Two_player_mode).w
    if emerald_hill_zone_act_1=0
	move.w	d0,(Current_ZoneAndAct).w ; emerald_hill_zone_act_1
    else
	move.w	#emerald_hill_zone_act_1,(Current_ZoneAndAct).w
    endif
	tst.b	(Level_select_flag).w	; has level select cheat been entered?
	beq.s	+			; if not, branch
	btst	#button_A,(Ctrl_1_Held).w ; is A held down?
	beq.s	+	 		; if not, branch
	move.b	#GameModeID_LevelSelect,(Game_Mode).w ; => LevelSelectMenu
	rts
; ---------------------------------------------------------------------------
+
	move.w	d0,(Current_Special_StageAndAct).w
	move.w	d0,(Got_Emerald).w
	move.l	d0,(Got_Emeralds_array).w
	move.l	d0,(Got_Emeralds_array+4).w
	rts
; ===========================================================================
; loc_3CF6:
TitleScreen_CheckIfChose2P:
	subq.b	#1,d0
	bne.s	TitleScreen_ChoseOptions

	moveq	#1,d1
	move.w	d1,(Two_player_mode_copy).w
	move.w	d1,(Two_player_mode).w

	moveq	#0,d0
	move.w	d0,(Got_Emerald).w
	move.l	d0,(Got_Emeralds_array).w
	move.l	d0,(Got_Emeralds_array+4).w

	move.b	#GameModeID_2PLevelSelect,(Game_Mode).w ; => LevelSelectMenu2P
	move.b	#0,(Current_Zone_2P).w
	rts
; ---------------------------------------------------------------------------
; loc_3D20:
TitleScreen_ChoseOptions:
	move.b	#GameModeID_OptionsMenu,(Game_Mode).w ; => OptionsMenu
	move.b	#0,(Options_menu_box).w
	rts
; ===========================================================================
; loc_3D2E:
TitleScreen_Demo:
	move.b	#MusID_FadeOut,d0
	bsr.w	PlaySound

	move.w	(Demo_number).w,d0
	andi.w	#7,d0
	add.w	d0,d0
	move.w	DemoLevels(pc,d0.w),d0
	move.w	d0,(Current_ZoneAndAct).w

	addq.w	#1,(Demo_number).w
	cmpi.w	#(DemoLevels_End-DemoLevels)/2,(Demo_number).w
	blo.s	+
	move.w	#0,(Demo_number).w
+
	move.w	#1,(Demo_mode_flag).w
	move.b	#GameModeID_Demo,(Game_Mode).w ; => Level (Demo mode)
	cmpi.w	#emerald_hill_zone_act_1,(Current_ZoneAndAct).w
	bne.s	+
	move.w	#1,(Two_player_mode).w
+
	move.b	#3,(Life_count).w
	move.b	#3,(Life_count_2P).w

	moveq	#0,d0
	move.w	d0,(Ring_count).w
	move.l	d0,(Timer).w
	move.l	d0,(Score).w
	move.w	d0,(Ring_count_2P).w
	move.l	d0,(Timer_2P).w
	move.l	d0,(Score_2P).w

	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w

	rts
; ===========================================================================
; word_3DAC:
DemoLevels:
	dc.w	emerald_hill_zone_act_1		; EHZ (2P)
	dc.w	chemical_plant_zone_act_1	; CPZ
	dc.w	aquatic_ruin_zone_act_1		; ARZ
	dc.w	casino_night_zone_act_1		; CNZ
DemoLevels_End:

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_3DB4:
TailsNameCheat:
	lea	(TailsNameCheat_Buttons).l,a0
	move.w	(Correct_cheat_entries).w,d0
	adda.w	d0,a0
	move.b	(Ctrl_1_Press).w,d0
	andi.b	#button_up_mask|button_down_mask|button_left_mask|button_right_mask,d0
	beq.s	++	; rts
	cmp.b	(a0),d0
	bne.s	+
	addq.w	#1,(Correct_cheat_entries).w
	tst.b	1(a0)		; read the next entry
	bne.s	++		; if it's not zero, return

	; Switch the detected console's region between Japanese and
	; international. This affects the presence of trademark symbols, and
	; causes Tails' name to swap between 'Tails' and 'Miles'.
	bchg	#7,(Graphics_Flags).w

	move.b	#SndID_Ring,d0 ; play the ring sound for a successfully entered cheat
	bsr.w	PlaySound
+
	move.w	#0,(Correct_cheat_entries).w
+
	rts
; End of function TailsNameCheat

; ===========================================================================
; byte_3DEE:
TailsNameCheat_Buttons:
	dc.b	button_up_mask
	dc.b	button_down_mask
	dc.b	button_down_mask
	dc.b	button_down_mask
	dc.b	button_up_mask
	dc.b	0	; end
	even
; ---------------------------------------------------------------------------------
; Nemesis compressed art
; 10 blocks
; Player 1 2 VS Text
; ---------------------------------------------------------------------------------
; ArtNem_3DF4:
ArtNem_Player1VS2:	BINCLUDE	"art/nemesis/1Player2VS.nem"
	even

	charset '0','9',0 ; Add character set for numbers
	charset '*',$A ; Add character for star
	charset '@',$B ; Add character for copyright symbol
	charset ':',$C ; Add character for colon
	charset '.',$D ; Add character for period
	charset 'A','Z',$E ; Add character set for letters

; word_3E82:
CopyrightText:
  irpc chr,"@ 1992 SEGA"
    if "chr"<>" "
	dc.w  make_art_tile(ArtTile_ArtNem_FontStuff_TtlScr + 'chr'|0,0,0)
    else
	dc.w  make_art_tile(ArtTile_VRAM_Start,0,0)
    endif
  endm
CopyrightText_End:

    charset ; Revert character set

    if ~~removeJmpTos
; sub_3E98:
JmpTo_SwScrl_Title ; JmpTo
	jmp	(SwScrl_Title).l

	align 4
    endif




;----------------------------------------------------------------------------
; 1P Music Playlist
;----------------------------------------------------------------------------
; byte_3EA0:
MusicList: zoneOrderedTable 1,1
	zoneTableEntry.b MusID_EHZ	; EHZ
	zoneTableEntry.b MusID_EHZ	; Zone 1
	zoneTableEntry.b MusID_MTZ	; WZ
	zoneTableEntry.b MusID_OOZ	; Zone 3
	zoneTableEntry.b MusID_MTZ	; MTZ1,2
	zoneTableEntry.b MusID_MTZ	; MTZ3
	zoneTableEntry.b MusID_WFZ	; WFZ
	zoneTableEntry.b MusID_HTZ	; HTZ
	zoneTableEntry.b MusID_HPZ	; HPZ
	zoneTableEntry.b MusID_SCZ	; Zone 9
	zoneTableEntry.b MusID_OOZ	; OOZ
	zoneTableEntry.b MusID_MCZ	; MCZ
	zoneTableEntry.b MusID_CNZ	; CNZ
	zoneTableEntry.b MusID_CPZ	; CPZ
	zoneTableEntry.b MusID_DEZ	; DEZ
	zoneTableEntry.b MusID_ARZ	; ARZ
	zoneTableEntry.b MusID_SCZ	; SCZ
    zoneTableEnd
	even
;----------------------------------------------------------------------------
; 2P Music Playlist
;----------------------------------------------------------------------------
; byte_3EB2:
MusicList2: zoneOrderedTable 1,1
	zoneTableEntry.b MusID_EHZ_2P	; EHZ
	zoneTableEntry.b MusID_EHZ	; Zone 1
	zoneTableEntry.b MusID_MTZ	; WZ
	zoneTableEntry.b MusID_OOZ	; Zone 3
	zoneTableEntry.b MusID_MTZ	; MTZ1,2
	zoneTableEntry.b MusID_MTZ	; MTZ3
	zoneTableEntry.b MusID_WFZ	; WFZ
	zoneTableEntry.b MusID_HTZ	; HTZ
	zoneTableEntry.b MusID_HPZ	; HPZ
	zoneTableEntry.b MusID_SCZ	; Zone 9
	zoneTableEntry.b MusID_OOZ	; OOZ
	zoneTableEntry.b MusID_MCZ_2P	; MCZ
	zoneTableEntry.b MusID_CNZ_2P	; CNZ
	zoneTableEntry.b MusID_CPZ	; CPZ
	zoneTableEntry.b MusID_DEZ	; DEZ
	zoneTableEntry.b MusID_ARZ	; ARZ
	zoneTableEntry.b MusID_SCZ	; SCZ
    zoneTableEnd
	even
; ===========================================================================

; ---------------------------------------------------------------------------
; Level
; DEMO AND ZONE LOOP (MLS values $08, $0C; bit 7 set indicates that load routine is running)
; ---------------------------------------------------------------------------
; loc_3EC4:
Level:
	bset	#GameModeFlag_TitleCard,(Game_Mode).w ; add $80 to screen mode (for pre level sequence)
	tst.w	(Demo_mode_flag).w	; test the old flag for the credits demos (now unused)
	bmi.s	+
	move.b	#MusID_FadeOut,d0
	bsr.w	PlaySound	; fade out music
+
	bsr.w	ClearPLC
	bsr.w	Pal_FadeToBlack
	tst.w	(Demo_mode_flag).w
	bmi.s	Level_ClrRam
	move	#$2700,sr
	bsr.w	ClearScreen
	jsr	(LoadTitleCard).l ; load title card patterns
	move	#$2300,sr
	moveq	#0,d0
	move.w	d0,(Level_frame_counter).w
	move.b	(Current_Zone).w,d0

	; multiply d0 by 12, the size of a level art load block
	add.w	d0,d0
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0

	lea	(LevelArtPointers).l,a2
	lea	(a2,d0.w),a2
	moveq	#0,d0
	move.b	(a2),d0	; PLC1 ID
	beq.s	+
	bsr.w	LoadPLC
+
	moveq	#PLCID_Std2,d0
	bsr.w	LoadPLC
	bsr.w	Level_SetPlayerMode
	moveq	#PLCID_MilesLife2P,d0
	tst.w	(Two_player_mode).w
	bne.s	+
	cmpi.w	#2,(Player_mode).w
	bne.s	Level_ClrRam
	addq.w	#PLCID_MilesLife-PLCID_MilesLife2P,d0
+
	tst.b	(Graphics_Flags).w
	bpl.s	+
	addq.w	#PLCID_TailsLife2P-PLCID_MilesLife2P,d0
+
	bsr.w	LoadPLC
; loc_3F48:
Level_ClrRam:
	clearRAM Object_Display_Lists,Object_Display_Lists_End
	clearRAM Object_RAM,LevelOnly_Object_RAM_End ; clear object RAM and level-only object RAM
	clearRAM MiscLevelVariables,MiscLevelVariables_End
	clearRAM Misc_Variables,Misc_Variables_End
	clearRAM Oscillating_Data,Oscillating_variables_End
    if fixBugs
	clearRAM CNZ_saucer_data,CNZ_saucer_data_End
    else
	; The '+C0' shouldn't be here; CNZ_saucer_data is only $40 bytes large
	clearRAM CNZ_saucer_data,CNZ_saucer_data_End+$C0
    endif

	cmpi.w	#chemical_plant_zone_act_2,(Current_ZoneAndAct).w ; CPZ 2
	beq.s	Level_InitWater
	cmpi.b	#aquatic_ruin_zone,(Current_Zone).w ; ARZ
	beq.s	Level_InitWater
	cmpi.b	#hidden_palace_zone,(Current_Zone).w ; HPZ
	bne.s	+

Level_InitWater:
	move.b	#1,(Water_flag).w
	move.w	#0,(Two_player_mode).w
+
	lea	(VDP_control_port).l,a6
	move.w	#$8B03,(a6)		; EXT-INT disabled, V scroll by screen, H scroll by line
	move.w	#$8200|(VRAM_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $E000
	move.w	#$8500|(VRAM_Sprite_Attribute_Table/$200),(a6)	; Sprite attribute table base: $F800
	move.w	#$9001,(a6)		; Scroll table size: 64x32
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8720,(a6)		; Background palette/color: 2/0
	move.w	#$8C81,(a6)		; H res 40 cells, no interlace
	tst.b	(Debug_options_flag).w
	beq.s	++
	btst	#button_C,(Ctrl_1_Held).w
	beq.s	+
	move.w	#$8C89,(a6)	; H res 40 cells, no interlace, S/H enabled
+
	btst	#button_A,(Ctrl_1_Held).w
	beq.s	+
	move.b	#1,(Debug_mode_flag).w
+
	move.w	#$8ADF,(Hint_counter_reserve).w	; H-INT every 223rd scanline
	tst.w	(Two_player_mode).w
	beq.s	+
	move.w	#$8A6B,(Hint_counter_reserve).w	; H-INT every 108th scanline
	move.w	#$8014,(a6)			; H-INT enabled
	move.w	#$8C87,(a6)			; H res 40 cells, double res interlace
+
	move.w	(Hint_counter_reserve).w,(a6)
	clr.w	(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
	tst.b	(Water_flag).w	; does level have water?
	beq.s	Level_LoadPal	; if not, branch
	move.w	#$8014,(a6)	; H-INT enabled
	moveq	#0,d0
	move.w	(Current_ZoneAndAct).w,d0
    if ~~useFullWaterTables
	subi.w	#hidden_palace_zone_act_1,d0
    endif
	ror.b	#1,d0
	lsr.w	#6,d0
	andi.w	#$FFFE,d0
	lea	(WaterHeight).l,a1	; load water height array
	move.w	(a1,d0.w),d0
	move.w	d0,(Water_Level_1).w ; set water heights
	move.w	d0,(Water_Level_2).w
	move.w	d0,(Water_Level_3).w
	clr.b	(Water_routine).w	; clear water routine counter
	clr.b	(Water_fullscreen_flag).w	; clear water movement
	move.b	#1,(Water_on).w	; enable water
; loc_407C:
Level_LoadPal:
	moveq	#PalID_BGND,d0
	bsr.w	PalLoad_Now	; load Sonic's palette line
	tst.b	(Water_flag).w	; does level have water?
	beq.s	Level_GetBgm	; if not, branch
	moveq	#PalID_HPZ_U,d0	; palette number $15
	cmpi.b	#hidden_palace_zone,(Current_Zone).w
	beq.s	Level_WaterPal ; branch if level is HPZ
	moveq	#PalID_CPZ_U,d0	; palette number $16
	cmpi.b	#chemical_plant_zone,(Current_Zone).w
	beq.s	Level_WaterPal ; branch if level is CPZ
	moveq	#PalID_ARZ_U,d0	; palette number $17
; loc_409E:
Level_WaterPal:
	bsr.w	PalLoad_Water_Now	; load underwater palette (with d0)
	tst.b	(Last_star_pole_hit).w ; is it the start of the level?
	beq.s	Level_GetBgm	; if yes, branch
	move.b	(Saved_Water_move).w,(Water_fullscreen_flag).w
; loc_40AE:
Level_GetBgm:
	tst.w	(Demo_mode_flag).w
	bmi.s	+
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	lea_	MusicList,a1
	tst.w	(Two_player_mode).w
	beq.s	Level_PlayBgm
	lea_	MusicList2,a1
; loc_40C8:
Level_PlayBgm:
	move.b	(a1,d0.w),d0		; load from music playlist
	move.w	d0,(Level_Music).w	; store level music
	bsr.w	PlayMusic		; play level music
	move.b	#ObjID_TitleCard,(TitleCard+id).w ; load Obj34 (level title card) at $FFFFB080
; loc_40DA:
Level_TtlCard:
	move.b	#VintID_TitleCard,(Vint_routine).w
	bsr.w	WaitForVint
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	move.w	(TitleCard_ZoneName+x_pos).w,d0
	cmp.w	(TitleCard_ZoneName+titlecard_x_target).w,d0 ; has title card sequence finished?
	bne.s	Level_TtlCard		; if not, branch
	tst.l	(Plc_Buffer).w		; are there any items in the pattern load cue?
	bne.s	Level_TtlCard		; if yes, branch
	move.b	#VintID_TitleCard,(Vint_routine).w
	bsr.w	WaitForVint
	jsr	(Hud_Base).l
+
	moveq	#PalID_BGND,d0
	bsr.w	PalLoad_ForFade	; load Sonic's palette line
	bsr.w	LevelSizeLoad
	jsrto	DeformBgLayer, JmpTo_DeformBgLayer
	clr.w	(Vscroll_Factor_FG).w
	move.w	#-224,(Vscroll_Factor_P2_FG).w

	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len

	bsr.w	LoadZoneTiles
	jsrto	loadZoneBlockMaps, JmpTo_loadZoneBlockMaps
	jsr	(LoadAnimatedBlocks).l
	jsrto	DrawInitialBG, JmpTo_DrawInitialBG
	jsr	(ConvertCollisionArray).l
	bsr.w	LoadCollisionIndexes
	bsr.w	WaterEffects
	bsr.w	InitPlayers
	move.w	#0,(Ctrl_1_Logical).w
	move.w	#0,(Ctrl_2_Logical).w
	move.w	#0,(Ctrl_1).w
	move.w	#0,(Ctrl_2).w
	move.b	#1,(Control_Locked).w
	move.b	#1,(Control_Locked_P2).w
	move.b	#0,(Level_started_flag).w
; Level_ChkWater:
	tst.b	(Water_flag).w	; does level have water?
	beq.s	+	; if not, branch
	move.b	#ObjID_WaterSurface,(WaterSurface1+id).w ; load Obj04 (water surface) at $FFFFB380
	move.w	#$60,(WaterSurface1+x_pos).w ; set horizontal offset
	move.b	#ObjID_WaterSurface,(WaterSurface2+id).w ; load Obj04 (water surface) at $FFFFB3C0
	move.w	#$120,(WaterSurface2+x_pos).w ; set different horizontal offset
+
	cmpi.b	#chemical_plant_zone,(Current_Zone).w	; check if zone == CPZ
	bne.s	+			; branch if not
	move.b	#ObjID_CPZPylon,(CPZPylon+id).w ; load Obj7C (CPZ pylon) at $FFFFB340
+
	cmpi.b	#oil_ocean_zone,(Current_Zone).w	; check if zone == OOZ
	bne.s	Level_ClrHUD		; branch if not
	move.b	#ObjID_Oil,(Oil+id).w ; load Obj07 (OOZ oil) at $FFFFB380
; Level_LoadObj: misnomer now
Level_ClrHUD:
	moveq	#0,d0
	tst.b	(Last_star_pole_hit).w	; are you starting from a lamppost?
	bne.s	Level_FromCheckpoint	; if yes, branch
	move.w	d0,(Ring_count).w	; clear rings
	move.l	d0,(Timer).w		; clear time
	move.b	d0,(Extra_life_flags).w	; clear extra lives counter
	move.w	d0,(Ring_count_2P).w	; ditto for player 2
	move.l	d0,(Timer_2P).w
	move.b	d0,(Extra_life_flags_2P).w
; loc_41E4:
Level_FromCheckpoint:
	move.b	d0,(Time_Over_flag).w
	move.b	d0,(Time_Over_flag_2P).w
	move.b	d0,(SlotMachine_Routine).w
	move.w	d0,(SlotMachineInUse).w
	move.w	d0,(Debug_placement_mode).w
	move.w	d0,(Level_Inactive_flag).w
	move.b	d0,(Teleport_timer).w
	move.b	d0,(Teleport_flag).w
	move.w	d0,(Rings_Collected).w
	move.w	d0,(Rings_Collected_2P).w
	move.w	d0,(Monitors_Broken).w
	move.w	d0,(Monitors_Broken_2P).w
	move.w	d0,(Loser_Time_Left).w
	bsr.w	OscillateNumInit
	move.b	#1,(Update_HUD_score).w
	move.b	#1,(Update_HUD_rings).w
	move.b	#1,(Update_HUD_timer).w
	move.b	#1,(Update_HUD_timer_2P).w
	jsr	(ObjectsManager).l
	jsr	(RingsManager).l
	jsr	(SpecialCNZBumpers).l
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	jsrto	AniArt_Load, JmpTo_AniArt_Load
	bsr.w	SetLevelEndType
	move.w	#0,(Demo_button_index).w
	move.w	#0,(Demo_button_index_2P).w
	lea	(DemoScriptPointers).l,a1
	moveq	#0,d0
	move.b	(Current_Zone).w,d0	; load zone value
	lsl.w	#2,d0
	movea.l	(a1,d0.w),a1
	tst.w	(Demo_mode_flag).w
	bpl.s	+
	lea	(EndingDemoScriptPointers).l,a1
	move.w	(Ending_demo_number).w,d0
	subq.w	#1,d0
	lsl.w	#2,d0
	movea.l	(a1,d0.w),a1
+
	move.b	1(a1),(Demo_press_counter).w
    if emerald_hill_zone<>0
	cmpi.b	#emerald_hill_zone,(Current_Zone).w
    else
	tst.b	(Current_Zone).w	; emerald_hill_zone
    endif
	bne.s	+
	lea	(Demo_EHZ_Tails).l,a1
	move.b	1(a1),(Demo_press_counter_2P).w
+
	move.w	#$668,(Demo_Time_left).w
	tst.w	(Demo_mode_flag).w
	bpl.s	+
	move.w	#$21C,(Demo_Time_left).w
	cmpi.w	#4,(Ending_demo_number).w
	bne.s	+
	move.w	#$1FE,(Demo_Time_left).w
+
	tst.b	(Water_flag).w
	beq.s	++
	moveq	#PalID_HPZ_U,d0
	cmpi.b	#hidden_palace_zone,(Current_Zone).w
	beq.s	+
	moveq	#PalID_CPZ_U,d0
	cmpi.b	#chemical_plant_zone,(Current_Zone).w
	beq.s	+
	moveq	#PalID_ARZ_U,d0
+
	bsr.w	PalLoad_Water_ForFade
+
	move.w	#-1,(TitleCard_ZoneName+titlecard_leaveflag).w
	move.b	#$E,(TitleCard_Left+routine).w	; make the left part move offscreen
	move.w	#$A,(TitleCard_Left+titlecard_location).w

-	move.b	#VintID_TitleCard,(Vint_routine).w
	bsr.w	WaitForVint
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	tst.b	(TitleCard_Background+id).w
	bne.s	-	; loop while the title card background is still loaded

	lea	(TitleCard).w,a1
	move.b	#$16,TitleCard_ZoneName-TitleCard+routine(a1)
	move.w	#$2D,TitleCard_ZoneName-TitleCard+anim_frame_duration(a1)
	move.b	#$16,TitleCard_Zone-TitleCard+routine(a1)
	move.w	#$2D,TitleCard_Zone-TitleCard+anim_frame_duration(a1)
	tst.b	TitleCard_ActNumber-TitleCard+id(a1)
	beq.s	+	; branch if the act number has been unloaded
	move.b	#$16,TitleCard_ActNumber-TitleCard+routine(a1)
	move.w	#$2D,TitleCard_ActNumber-TitleCard+anim_frame_duration(a1)
+	move.b	#0,(Control_Locked).w
	move.b	#0,(Control_Locked_P2).w
	move.b	#1,(Level_started_flag).w

; Level_StartGame: loc_435A:
	bclr	#GameModeFlag_TitleCard,(Game_Mode).w ; clear $80 from the game mode

; ---------------------------------------------------------------------------
; Main level loop (when all title card and loading sequences are finished)
; ---------------------------------------------------------------------------
; loc_4360:
Level_MainLoop:
	bsr.w	PauseGame
	move.b	#VintID_Level,(Vint_routine).w
	bsr.w	WaitForVint
	addq.w	#1,(Level_frame_counter).w ; add 1 to level timer
	bsr.w	MoveSonicInDemo
	bsr.w	WaterEffects
	jsr	(RunObjects).l
	tst.w	(Level_Inactive_flag).w
	bne.w	Level
	jsrto	DeformBgLayer, JmpTo_DeformBgLayer
	bsr.w	UpdateWaterSurface
	jsr	(RingsManager).l
	cmpi.b	#casino_night_zone,(Current_Zone).w	; is it CNZ?
	bne.s	+			; if not, branch past jsr
	jsr	(SpecialCNZBumpers).l
+
	jsrto	AniArt_Load, JmpTo_AniArt_Load
	bsr.w	PalCycle_Load
	bsr.w	RunPLC_RAM
	bsr.w	OscillateNumDo
	bsr.w	ChangeRingFrame
	bsr.w	CheckLoadSignpostArt
	jsr	(BuildSprites).l
	jsr	(ObjectsManager).l
	cmpi.b	#GameModeID_Demo,(Game_Mode).w	; check if in demo mode
	beq.s	+
	cmpi.b	#GameModeID_Level,(Game_Mode).w	; check if in normal play mode
	beq.w	Level_MainLoop
	rts
; ---------------------------------------------------------------------------
+
	tst.w	(Level_Inactive_flag).w
	bne.s	+
	tst.w	(Demo_Time_left).w
	beq.s	+
	cmpi.b	#GameModeID_Demo,(Game_Mode).w
	beq.w	Level_MainLoop
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen
	rts
; ---------------------------------------------------------------------------
+
	cmpi.b	#GameModeID_Demo,(Game_Mode).w
	bne.s	+
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen
+
	move.w	#1*60,(Demo_Time_left).w	; 1 second
	move.w	#$3F,(Palette_fade_range).w
	clr.w	(PalChangeSpeed).w
-
	move.b	#VintID_Level,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.w	MoveSonicInDemo
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	jsr	(ObjectsManager).l
	subq.w	#1,(PalChangeSpeed).w
	bpl.s	+
	move.w	#2,(PalChangeSpeed).w
	bsr.w	Pal_FadeToBlack.UpdateAllColours
+
	tst.w	(Demo_Time_left).w
	bne.s	-
	rts

; ---------------------------------------------------------------------------
; Subroutine to set the player mode, which is forced to Sonic and Tails in
; the demo mode and in 2P mode
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_4450:
Level_SetPlayerMode:
	cmpi.b	#GameModeID_TitleCard|GameModeID_Demo,(Game_Mode).w ; pre-level demo mode?
	beq.s	+			; if yes, branch
	tst.w	(Two_player_mode).w	; 2P mode?
	bne.s	+			; if yes, branch
	move.w	(Player_option).w,(Player_mode).w ; use the option chosen in the Options screen
	rts
+
	move.w	#0,(Player_mode).w	; force Sonic and Tails
	rts
; End of function Level_SetPlayerMode


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_446E:
InitPlayers:
	move.w	(Player_mode).w,d0
	bne.s	InitPlayers_Alone ; branch if this isn't a Sonic and Tails game

	move.b	#ObjID_Sonic,(MainCharacter+id).w ; load Obj01 Sonic object at $FFFFB000
	move.b	#ObjID_SpindashDust,(Sonic_Dust+id).w ; load Obj08 Sonic's spindash dust/splash object at $FFFFD100

	cmpi.b	#wing_fortress_zone,(Current_Zone).w
	beq.s	+ ; skip loading Tails if this is WFZ
	cmpi.b	#death_egg_zone,(Current_Zone).w
	beq.s	+ ; skip loading Tails if this is DEZ
	cmpi.b	#sky_chase_zone,(Current_Zone).w
	beq.s	+ ; skip loading Tails if this is SCZ

	move.b	#ObjID_Tails,(Sidekick+id).w ; load Obj02 Tails object at $FFFFB040
	move.w	(MainCharacter+x_pos).w,(Sidekick+x_pos).w
	move.w	(MainCharacter+y_pos).w,(Sidekick+y_pos).w
	subi.w	#$20,(Sidekick+x_pos).w
	addi_.w	#4,(Sidekick+y_pos).w
	move.b	#ObjID_SpindashDust,(Tails_Dust+id).w ; load Obj08 Tails' spindash dust/splash object at $FFFFD140
+
	rts
; ===========================================================================
; loc_44BE:
InitPlayers_Alone: ; either Sonic or Tails but not both
	subq.w	#1,d0
	bne.s	InitPlayers_TailsAlone ; branch if this is a Tails alone game

	move.b	#ObjID_Sonic,(MainCharacter+id).w ; load Obj01 Sonic object at $FFFFB000
	move.b	#ObjID_SpindashDust,(Sonic_Dust+id).w ; load Obj08 Sonic's spindash dust/splash object at $FFFFD100
	rts
; ===========================================================================
; loc_44D0:
InitPlayers_TailsAlone:
	move.b	#ObjID_Tails,(MainCharacter+id).w ; load Obj02 Tails object at $FFFFB000
	move.b	#ObjID_SpindashDust,(Tails_Dust+id).w ; load Obj08 Tails' spindash dust/splash object at $FFFFD100
	addi_.w	#4,(MainCharacter+y_pos).w
	rts
; End of function InitPlayers





; ---------------------------------------------------------------------------
; Subroutine to move the water or oil surface sprites to where the screen is at
; (the closest match I could find to this subroutine in Sonic 1 is Obj1B_Action)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_44E4:
UpdateWaterSurface:
	tst.b	(Water_flag).w
	beq.s	++	; rts
	move.w	(Camera_X_pos).w,d1
    if fixBugs
	; This function can cause the water surface's to be cut off at the
	; left when the game is paused. This is because this function pushes
	; the water surface sprite to the right every frame. To fix this,
	; just avoid pushing the sprite to the right when the game is about
	; to be paused.
	move.b	(Ctrl_1_Press).w,d0 ; is Start button pressed?
	or.b	(Ctrl_2_Press).w,d0 ; (either player)
	andi.b	#button_start_mask,d0
	bne.s	+
    endif
	btst	#0,(Level_frame_counter+1).w
	beq.s	+
	addi.w	#$20,d1
+		; match obj x-position to screen position
	move.w	d1,d0
	addi.w	#$60,d0
	move.w	d0,(WaterSurface1+x_pos).w
	addi.w	#$120,d1
	move.w	d1,(WaterSurface2+x_pos).w
+
	rts
; End of function UpdateWaterSurface


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; ---------------------------------------------------------------------------
; Subroutine to do special water effects
; ---------------------------------------------------------------------------
; sub_450E: ; LZWaterEffects:
WaterEffects:
	tst.b	(Water_flag).w	; does level have water?
	beq.s	NonWaterEffects	; if not, branch
	tst.b	(Deform_lock).w
	bne.s	MoveWater
	cmpi.b	#6,(MainCharacter+routine).w	; is player dead?
	bhs.s	MoveWater			; if yes, branch
	bsr.w	DynamicWater
; loc_4526: ; LZMoveWater:
MoveWater:
	clr.b	(Water_fullscreen_flag).w
	moveq	#0,d0
	cmpi.b	#aquatic_ruin_zone,(Current_Zone).w	; is level ARZ?
	beq.s	+		; if yes, branch
	move.b	(Oscillating_Data).w,d0
	lsr.w	#1,d0
+
	add.w	(Water_Level_2).w,d0
	move.w	d0,(Water_Level_1).w
		; calculate distance between water surface and top of screen
	move.w	(Water_Level_1).w,d0
	sub.w	(Camera_Y_pos).w,d0
	bhs.s	+
	tst.w	d0
	bpl.s	+
	move.b	#224-1,(Hint_counter_reserve+1).w	; H-INT every 224th scanline
	move.b	#1,(Water_fullscreen_flag).w
+
	cmpi.w	#224-1,d0
	blo.s	+
	move.w	#224-1,d0
+
	move.b	d0,(Hint_counter_reserve+1).w	; H-INT every d0 scanlines
; loc_456A:
NonWaterEffects:
	cmpi.b	#oil_ocean_zone,(Current_Zone).w	; is the level OOZ?
	bne.s	+			; if not, branch
	bsr.w	OilSlides		; call oil slide routine
+
	cmpi.b	#wing_fortress_zone,(Current_Zone).w	; is the level WFZ?
	bne.s	+			; if not, branch
	bsr.w	WindTunnel		; call wind and block break routine
+
	rts
; End of function WaterEffects

; ===========================================================================
    if useFullWaterTables
WaterHeight: zoneOrderedTable 2,2
	zoneTableEntry.w  $600, $600	; EHZ
	zoneTableEntry.w  $600, $600	; Zone 1
	zoneTableEntry.w  $600, $600	; WZ
	zoneTableEntry.w  $600, $600	; Zone 3
	zoneTableEntry.w  $600, $600	; MTZ1,2
	zoneTableEntry.w  $600, $600	; MTZ3
	zoneTableEntry.w  $600, $600	; WFZ
	zoneTableEntry.w  $600, $600	; HTZ
	zoneTableEntry.w  $600, $600	; HPZ
	zoneTableEntry.w  $600, $600	; Zone 9
	zoneTableEntry.w  $600, $600	; OOZ
	zoneTableEntry.w  $600, $600	; MCZ
	zoneTableEntry.w  $600, $600	; CNZ
	zoneTableEntry.w  $600, $710	; CPZ
	zoneTableEntry.w  $600, $600	; DEZ
	zoneTableEntry.w  $410, $510	; ARZ
	zoneTableEntry.w  $600, $600	; SCZ
    zoneTableEnd
    else
; word_4584:
WaterHeight:
	dc.w  $600, $600	; HPZ
	dc.w  $600, $600
	dc.w  $600, $600	; OOZ
	dc.w  $600, $600	; MCZ
	dc.w  $600, $600	; CNZ
	dc.w  $600, $710	; CPZ
	dc.w  $600, $600	; DEZ
	dc.w  $410, $510	; ARZ
    endif

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_45A4: ; LZDynamicWater:
DynamicWater:
	moveq	#0,d0
	move.w	(Current_ZoneAndAct).w,d0
    if ~~useFullWaterTables
	subi.w	#hidden_palace_zone_act_1,d0
    endif
	ror.b	#1,d0
	lsr.w	#6,d0
	andi.w	#$FFFE,d0
	move.w	Dynamic_water_routine_table(pc,d0.w),d0
	jsr	Dynamic_water_routine_table(pc,d0.w)
	moveq	#0,d1
	move.b	(Water_on).w,d1
	move.w	(Water_Level_3).w,d0
	sub.w	(Water_Level_2).w,d0
	beq.s	++	; rts
	bcc.s	+
	neg.w	d1
+
	add.w	d1,(Water_Level_2).w
+
	rts
; End of function DynamicWater

; ===========================================================================
    if useFullWaterTables
Dynamic_water_routine_table: zoneOrderedOffsetTable 2,2
	; EHZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; Zone 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; WZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; Zone 3
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; MTZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; MTZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 3
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 4
	; WFZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; HTZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; HPZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; Zone 9
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; OOZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; MCZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; CNZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; CPZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterCPZ2 ; Act 2
	; DEZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; ARZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
	; SCZ
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 1
	zoneOffsetTableEntry.w DynamicWaterNull ; Act 2
    zoneTableEnd
    else
; off_45D8:
Dynamic_water_routine_table: offsetTable
	; HPZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
	; Zone 9
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
	; OOZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
	; MCZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
	; CNZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
	; CPZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterCPZ2 ; Act 2
	; DEZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
	; ARZ
	offsetTableEntry.w DynamicWaterNull ; Act 1
	offsetTableEntry.w DynamicWaterNull ; Act 2
    endif
; ===========================================================================
; return_45F8:
DynamicWaterNull:
	rts
; ===========================================================================
; loc_45FA:
DynamicWaterCPZ2:
	cmpi.w	#$1DE0,(Camera_X_pos).w
	blo.s	+	; rts
	move.w	#$510,(Water_Level_3).w
+	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Equates:
windtunnel_min_x_pos	= 0
windtunnel_max_x_pos	= 4
windtunnel_min_y_pos	= 2
windtunnel_max_y_pos	= 6

; sub_460A:
WindTunnel:
	tst.w	(Debug_placement_mode).w
	bne.w	WindTunnel_End	; don't interact with wind tunnels while in debug mode
	lea	(WindTunnelsCoordinates).l,a2
	moveq	#(WindTunnelsCoordinates_End-WindTunnelsCoordinates)/8-1,d1
	lea	(MainCharacter).w,a1 ; a1=character
-	; check for current wind tunnel if the main character is inside it
	move.w	x_pos(a1),d0
	cmp.w	windtunnel_min_x_pos(a2),d0
	blo.w	WindTunnel_Leave	; branch, if main character is too far left
	cmp.w	windtunnel_max_x_pos(a2),d0
	bhs.w	WindTunnel_Leave	; branch, if main character is too far right
	move.w	y_pos(a1),d2
	cmp.w	windtunnel_min_y_pos(a2),d2
	blo.w	WindTunnel_Leave	; branch, if main character is too far up
	cmp.w	windtunnel_max_y_pos(a2),d2
	bhs.s	WindTunnel_Leave	; branch, if main character is too far down
	tst.b	(WindTunnel_holding_flag).w
	bne.w	WindTunnel_End
	cmpi.b	#4,routine(a1)		; is the main character hurt, dying, etc. ?
	bhs.s	WindTunnel_LeaveHurt	; if yes, branch
	move.b	#1,(WindTunnel_flag).w	; affects character animation and bubble movement
	subi_.w	#4,x_pos(a1)	; move main character to the left
	move.w	#-$400,x_vel(a1)
	move.w	#0,y_vel(a1)
	move.b	#AniIDSonAni_Float2,anim(a1)
	bset	#1,status(a1)	; set "in-air" bit
	btst	#button_up,(Ctrl_1_Held).w	; is Up being pressed?
	beq.s	+				; if not, branch
	subq.w	#1,y_pos(a1)	; move up
+
	btst	#button_down,(Ctrl_1_Held).w	; is Down being pressed?
	beq.s	+				; if not, branch
	addq.w	#1,y_pos(a1)	; move down
+
	rts
; ===========================================================================
; loc_4690:
WindTunnel_Leave:
	addq.w	#8,a2
	dbf	d1,-	; check next tunnel
	; when all wind tunnels have been checked
	tst.b	(WindTunnel_flag).w
	beq.s	WindTunnel_End
	move.b	#AniIDSonAni_Walk,anim(a1)
; loc_46A2:
WindTunnel_LeaveHurt:	; the main character is hurt or dying, leave the tunnel and don't check the other
	clr.b	(WindTunnel_flag).w
; return_46A6:
WindTunnel_End:
	rts
; End of function WindTunnel

; ===========================================================================
; word_46A8:
WindTunnelsCoordinates:
	dc.w $1510,$400,$1AF0,$580
	dc.w $20F0,$618,$2500,$680
WindTunnelsCoordinates_End:

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_46B8:
OilSlides:
	lea	(MainCharacter).w,a1 ; a1=character
	move.b	(Ctrl_1_Held_Logical).w,d2
	bsr.s	+
	lea	(Sidekick).w,a1 ; a1=character
	move.b	(Ctrl_2_Held_Logical).w,d2
+
	btst	#1,status(a1)
	bne.s	+
	move.w	y_pos(a1),d0
	add.w	d0,d0
	andi.w	#$F00,d0
	move.w	x_pos(a1),d1
	lsr.w	#7,d1
	andi.w	#$7F,d1
	add.w	d1,d0
	lea	(Level_Layout).w,a2
	move.b	(a2,d0.w),d0
	lea	OilSlides_Chunks_End(pc),a2

	moveq	#OilSlides_Chunks_End-OilSlides_Chunks-1,d1
-	cmp.b	-(a2),d0
	dbeq	d1,-

	beq.s	loc_4712
+
    if status_sec_isSliding = 7
	tst.b	status_secondary(a1)
	bpl.s	+	; rts
    else
	btst	#status_sec_isSliding,status_secondary(a1)
	beq.s	+	; rts
    endif
	move.w	#5,move_lock(a1)
	andi.b	#(~status_sec_isSliding_mask)&$FF,status_secondary(a1)
+	rts
; ===========================================================================

loc_4712:
	lea	(OilSlides_Speeds).l,a2
	move.b	(a2,d1.w),d0
	beq.s	loc_476E
	move.b	inertia(a1),d1
	tst.b	d0
	bpl.s	+
	cmp.b	d0,d1
	ble.s	++
	subi.w	#$40,inertia(a1)
	bra.s	++
; ===========================================================================
+
	cmp.b	d0,d1
	bge.s	+
	addi.w	#$40,inertia(a1)
+
	bclr	#0,status(a1)
	tst.b	d1
	bpl.s	+
	bset	#0,status(a1)
+
	move.b	#AniIDSonAni_Slide,anim(a1)
	ori.b	#status_sec_isSliding_mask,status_secondary(a1)
	move.b	(Vint_runcount+3).w,d0
	andi.b	#$1F,d0
	bne.s	+	; rts
	move.w	#SndID_OilSlide,d0
	jsr	(PlaySound).l
+
	rts
; ===========================================================================

loc_476E:
	move.w	#4,d1
	move.w	inertia(a1),d0
	btst	#button_left,d2
	beq.s	+
	move.b	#AniIDSonAni_Walk,anim(a1)
	bset	#0,status(a1)
	sub.w	d1,d0
	tst.w	d0
	bpl.s	+
	sub.w	d1,d0
+
	btst	#button_right,d2
	beq.s	+
	move.b	#AniIDSonAni_Walk,anim(a1)
	bclr	#0,status(a1)
	add.w	d1,d0
	tst.w	d0
	bmi.s	+
	add.w	d1,d0
+
	move.w	#4,d1
	tst.w	d0
	beq.s	+++
	bmi.s	++
	sub.w	d1,d0
	bhi.s	+
	move.w	#0,d0
	move.b	#AniIDSonAni_Wait,anim(a1)
+	bra.s	++
; ===========================================================================
+
	add.w	d1,d0
	bhi.s	+
	move.w	#0,d0
	move.b	#AniIDSonAni_Wait,anim(a1)
+
	move.w	d0,inertia(a1)
	ori.b	#status_sec_isSliding_mask,status_secondary(a1)
	rts
; End of function OilSlides

; ===========================================================================
OilSlides_Speeds:
	dc.b  -8, -8, -8,  8,  8,  0,  0,  0, -8, -8,  0,  8,  8,  8,  0,  8
	dc.b   8,  8,  0, -8,  0,  0, -8,  8, -8, -8, -8,  8,  8,  8, -8, -8 ; 16

; These are the IDs of the chunks where Sonic and Tails will slide
OilSlides_Chunks:
	dc.b $2F,$30,$31,$33,$35,$38,$3A,$3C,$63,$64,$83,$90,$91,$93,$A1,$A3
	dc.b $BD,$C7,$C8,$CE,$D7,$D8,$E6,$EB,$EC,$ED,$F1,$F2,$F3,$F4,$FA,$FD ; 16
OilSlides_Chunks_End:
	even




; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_481E:
MoveSonicInDemo:
	tst.w	(Demo_mode_flag).w	; is demo mode on?
	bne.w	MoveDemo_On	; if yes, branch
	rts
; ---------------------------------------------------------------------------
; demo recording routine
; (unused/dead code, but obviously used during development)
; ---------------------------------------------------------------------------
; MoveDemo_Record: loc_4828:
	; calculate output location of recorded player 1 demo?
	lea	(DemoScriptPointers).l,a1
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	lsl.w	#2,d0
	movea.l	(a1,d0.w),a1
	move.w	(Demo_button_index).w,d0
	adda.w	d0,a1

	move.b	(Ctrl_1_Held).w,d0	; load input of player 1
	cmp.b	(a1),d0			; is same button held?
	bne.s	+			; if not, branch
	addq.b	#1,1(a1)		; increment press length counter
	cmpi.b	#$FF,1(a1)		; is button held too long?
	beq.s	+			; if yes, branch
	bra.s	MoveDemo_Record_P2	; go to player 2
; ===========================================================================
+
	move.b	d0,2(a1)		; store last button press
	move.b	#0,3(a1)		; reset hold length counter
	addq.w	#2,(Demo_button_index).w ; advance to next button press
	andi.w	#$3FF,(Demo_button_index).w ; wrap at max button press changes 1024
; loc_486A:
MoveDemo_Record_P2:
	cmpi.b	#emerald_hill_zone,(Current_Zone).w
	bne.s	++	; rts
	lea	($FEC000).l,a1		; output location of recorded player 2 demo? (unknown)
	move.w	(Demo_button_index_2P).w,d0
	adda.w	d0,a1
	move.b	(Ctrl_2_Held).w,d0	; load input of player 2
	cmp.b	(a1),d0			; is same button held?
	bne.s	+			; if not, branch
	addq.b	#1,1(a1)		; increment press length counter
	cmpi.b	#$FF,1(a1)		; is button held too long?
	beq.s	+			; if yes, branch
	bra.s	++			; if not, return
; ===========================================================================
+
	move.b	d0,2(a1)		; store last button press
	move.b	#0,3(a1)		; reset hold length counter
	addq.w	#2,(Demo_button_index_2P).w ; advance to next button press
	andi.w	#$3FF,(Demo_button_index_2P).w ; wrap at max button press changes 1024
+	rts
	; end of inactive recording code
; ===========================================================================
	; continue with MoveSonicInDemo:

; loc_48AA:
MoveDemo_On:
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	andi.b	#button_start_mask,d0
	beq.s	+
	tst.w	(Demo_mode_flag).w
	bmi.s	+
	move.b	#GameModeID_TitleScreen,(Game_Mode).w ; => TitleScreen
+
	lea	(DemoScriptPointers).l,a1 ; load pointer to input data
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	cmpi.b	#GameModeID_SpecialStage,(Game_Mode).w ; special stage mode?
	bne.s	MoveDemo_On_P1		; if yes, branch
	moveq	#6,d0
; loc_48DA:
MoveDemo_On_P1:
	lsl.w	#2,d0
	movea.l	(a1,d0.w),a1

	move.w	(Demo_button_index).w,d0
	adda.w	d0,a1	; a1 now points to the current button press data
	move.b	(a1),d0	; load button press
	lea	(Ctrl_1_Held).w,a0
	move.b	d0,d1
    if fixBugs
	; In REV00 of Sonic 1, this instruction was 'move.b (a0),d2'. The
	; purpose of this is to XOR the current frame's input with the
	; previous frame's input to determine which inputs had been pressed
	; on the current frame. The usage of '(a0)' for this is a problem
	; because it doesn't hold the *demo* inputs from the previous frame,
	; but rather the *player's* inputs from the *current* frame.
	; This meant that it was possible for the player to influence the
	; demos by pressing buttons on the joypad. In REV01 of Sonic 1, this
	; instruction was replaced with a 'moveq #0,d2', effectively
	; dummying-out the process of differentiating newly-pressed inputs
	; from old held inputs, causing every input to be treated as
	; newly-pressed on every frame. While this isn't a problem in this
	; game, it does become a problem if Sonic or Tails is given a
	; double-jump ability, as the ability will constantly be activated
	; when they shouldn't be. While not exactly the intended use for this
	; variable, 'Ctrl_1_Held_Logical' does happen to hold the inputs from
	; the previous frame, so we can use this here instead to fix this bug
	; properly.
	move.b	Ctrl_1_Held_Logical-Ctrl_1_Held(a0),d2
    else
	moveq	#0,d2
    endif
	eor.b	d2,d0	; determine which buttons differ between this frame and the last
	move.b	d1,(a0)+ ; save button press data from demo to Ctrl_1_Held
	and.b	d1,d0	; only keep the buttons that were pressed on this frame
	move.b	d0,(a0)+ ; save the same thing to Ctrl_1_Press
	subq.b	#1,(Demo_press_counter).w  ; decrement counter until next press
	bcc.s	MoveDemo_On_P2	   ; if it isn't 0 yet, branch
	move.b	3(a1),(Demo_press_counter).w ; reset counter to length of next press
	addq.w	#2,(Demo_button_index).w ; advance to next button press
; loc_4908:
MoveDemo_On_P2:
	cmpi.b	#emerald_hill_zone,(Current_Zone).w
	bne.s	MoveDemo_On_SkipP2 ; if it's not the EHZ demo, branch to skip player 2
	lea	(Demo_EHZ_Tails).l,a1

	; same as the corresponding remainder of MoveDemo_On_P1, but for player 2
	move.w	(Demo_button_index_2P).w,d0
	adda.w	d0,a1
	move.b	(a1),d0
	lea	(Ctrl_2_Held).w,a0
	move.b	d0,d1
    if fixBugs
	; In REV00 of Sonic 1, this instruction was 'move.b (a0),d2'. The
	; purpose of this is to XOR the current frame's input with the
	; previous frame's input to determine which inputs had been pressed
	; on the current frame. The usage of '(a0)' for this is a problem
	; because it doesn't hold the *demo* inputs from the previous frame,
	; but rather the *player's* inputs from the *current* frame.
	; This meant that it was possible for the player to influence the
	; demos by pressing buttons on the joypad. In REV01 of Sonic 1, this
	; instruction was replaced with a 'moveq #0,d2', effectively
	; dummying-out the process of differentiating newly-pressed inputs
	; from old held inputs, causing every input to be treated as
	; newly-pressed on every frame. While this isn't a problem in this
	; game, it does become a problem if Sonic or Tails is given a
	; double-jump ability, as the ability will constantly be activated
	; when they shouldn't be. While not exactly the intended use for this
	; variable, 'Ctrl_1_Held_Logical' does happen to hold the inputs from
	; the previous frame, so we can use this here instead to fix this bug
	; properly.
	move.b	Ctrl_1_Held_Logical-Ctrl_1_Held(a0),d2
    else
	moveq	#0,d2
    endif
	eor.b	d2,d0
	move.b	d1,(a0)+
	and.b	d1,d0
	move.b	d0,(a0)+
	subq.b	#1,(Demo_press_counter_2P).w
	bcc.s	+	; rts
	move.b	3(a1),(Demo_press_counter_2P).w
	addq.w	#2,(Demo_button_index_2P).w
+
	rts
; ===========================================================================
; loc_4940:
MoveDemo_On_SkipP2:
	move.w	#0,(Ctrl_2).w
	rts
; End of function MoveSonicInDemo

; ===========================================================================
; ---------------------------------------------------------------------------
; DEMO SCRIPT POINTERS

; Contains an array of pointers to the script controlling the players actions
; to use for each level.
; ---------------------------------------------------------------------------
; off_4948:
DemoScriptPointers: zoneOrderedTable 4,1
	zoneTableEntry.l Demo_EHZ	; EHZ
	zoneTableEntry.l Demo_EHZ	; Zone 1
	zoneTableEntry.l Demo_EHZ	; WZ
	zoneTableEntry.l Demo_EHZ	; Zone 3
	zoneTableEntry.l Demo_EHZ	; MTZ1,2
	zoneTableEntry.l Demo_EHZ	; MTZ3
	zoneTableEntry.l Demo_EHZ	; WFZ
	zoneTableEntry.l Demo_EHZ	; HTZ
	zoneTableEntry.l Demo_EHZ	; HPZ
	zoneTableEntry.l Demo_EHZ	; Zone 9
	zoneTableEntry.l Demo_EHZ	; OOZ
	zoneTableEntry.l Demo_EHZ	; MCZ
	zoneTableEntry.l Demo_CNZ	; CNZ
	zoneTableEntry.l Demo_CPZ	; CPZ
	zoneTableEntry.l Demo_EHZ	; DEZ
	zoneTableEntry.l Demo_ARZ	; ARZ
	zoneTableEntry.l Demo_EHZ	; SCZ
    zoneTableEnd
; ---------------------------------------------------------------------------
; dword_498C:
EndingDemoScriptPointers:
	; these values are invalid addresses, but they were used for the ending
	; demos, which aren't present in Sonic 2
	dc.l   $8B0837
	dc.l   $42085C	; 1
	dc.l   $6A085F	; 2
	dc.l   $2F082C	; 3
	dc.l   $210803	; 4
	dc.l $28300808	; 5
	dc.l   $2E0815	; 6
	dc.l	$F0846	; 7
	dc.l   $1A08FF	; 8
	dc.l  $8CA0000	; 9
	dc.l	     0	; 10
	dc.l	     0	; 11




; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_49BC:
LoadCollisionIndexes:
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	lsl.w	#2,d0
	move.l	#Primary_Collision,(Collision_addr).w
	move.w	d0,-(sp)
	movea.l	Off_ColP(pc,d0.w),a0
	lea	(Primary_Collision).w,a1
	bsr.w	KosDec
	move.w	(sp)+,d0
	movea.l	Off_ColS(pc,d0.w),a0
	lea	(Secondary_Collision).w,a1
	bra.w	KosDec
; End of function LoadCollisionIndexes

; ===========================================================================
; ---------------------------------------------------------------------------
; Pointers to primary collision indexes

; Contains an array of pointers to the primary collision index data for each
; level. 1 pointer for each level, pointing the primary collision index.
; ---------------------------------------------------------------------------
Off_ColP: zoneOrderedTable 4,1
	zoneTableEntry.l ColP_EHZHTZ	; EHZ
	zoneTableEntry.l ColP_Invalid	; Zone 1
	zoneTableEntry.l ColP_WZ	; WZ
	zoneTableEntry.l ColP_Invalid	; Zone 3
	zoneTableEntry.l ColP_MTZ	; MTZ1,2
	zoneTableEntry.l ColP_MTZ	; MTZ3
	zoneTableEntry.l ColP_WFZSCZ	; WFZ
	zoneTableEntry.l ColP_EHZHTZ	; HTZ
	zoneTableEntry.l ColP_HPZ	; HPZ
	zoneTableEntry.l ColP_Invalid	; Zone 9
	zoneTableEntry.l ColP_OOZ	; OOZ
	zoneTableEntry.l ColP_MCZ	; MCZ
	zoneTableEntry.l ColP_CNZ	; CNZ
	zoneTableEntry.l ColP_CPZDEZ	; CPZ
	zoneTableEntry.l ColP_CPZDEZ	; DEZ
	zoneTableEntry.l ColP_ARZ	; ARZ
	zoneTableEntry.l ColP_WFZSCZ	; SCZ
    zoneTableEnd

; ---------------------------------------------------------------------------
; Pointers to secondary collision indexes

; Contains an array of pointers to the secondary collision index data for
; each level. 1 pointer for each level, pointing the secondary collision
; index.
; ---------------------------------------------------------------------------
Off_ColS: zoneOrderedTable 4,1
	zoneTableEntry.l ColS_EHZHTZ	; EHZ
	zoneTableEntry.l ColP_Invalid	; Zone 1
	zoneTableEntry.l ColP_WZ	; WZ
	zoneTableEntry.l ColP_Invalid	; Zone 3
	zoneTableEntry.l ColP_MTZ	; MTZ1,2
	zoneTableEntry.l ColP_MTZ	; MTZ3
	zoneTableEntry.l ColS_WFZSCZ	; WFZ
	zoneTableEntry.l ColS_EHZHTZ	; HTZ
	zoneTableEntry.l ColS_HPZ	; HPZ
	zoneTableEntry.l ColP_Invalid	; Zone 9
	zoneTableEntry.l ColP_OOZ	; OOZ
	zoneTableEntry.l ColP_MCZ	; MCZ
	zoneTableEntry.l ColS_CNZ	; CNZ
	zoneTableEntry.l ColS_CPZDEZ	; CPZ
	zoneTableEntry.l ColS_CPZDEZ	; DEZ
	zoneTableEntry.l ColS_ARZ	; ARZ
	zoneTableEntry.l ColS_WFZSCZ	; SCZ
    zoneTableEnd


; ---------------------------------------------------------------------------
; Oscillating number subroutine
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_4A70:
OscillateNumInit:
	lea	(Oscillating_Numbers).w,a1
	lea	(Osc_Data).l,a2
	moveq	#bytesToWcnt(Osc_Data_End-Osc_Data),d1
; loc_4A7C:
Osc_Loop:
	move.w	(a2)+,(a1)+
	dbf	d1,Osc_Loop
	rts
; End of function OscillateNumInit

; ===========================================================================
; word_4A84:
Osc_Data:
	dc.w %0000000001111101		; oscillation direction bitfield
	dc.w   $80,   0	; baseline values
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w   $80,   0
	dc.w $3848, $EE
	dc.w $2080, $B4
	dc.w $3080,$10E
	dc.w $5080,$1C2
	dc.w $7080,$276
	dc.w   $80,   0
	dc.w $4000, $FE
Osc_Data_End:

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_4AC6:
OscillateNumDo:
	tst.w	(Two_player_mode).w
	bne.s	+
	cmpi.b	#6,(MainCharacter+routine).w
	bhs.s	OscillateNumDo_Return
+
	lea	(Oscillating_Numbers).w,a1
	lea	(Osc_Data2).l,a2
	move.w	(a1)+,d3
	moveq	#bytesToLcnt(Osc_Data2_End-Osc_Data2),d1

-	move.w	(a2)+,d2
	move.w	(a2)+,d4
	btst	d1,d3
	bne.s	+
	move.w	2(a1),d0
	add.w	d2,d0
	move.w	d0,2(a1)
	_add.w	d0,0(a1)
	_cmp.b	0(a1),d4
	bhi.s	++
	bset	d1,d3
	bra.s	++
; ===========================================================================
+
	move.w	2(a1),d0
	sub.w	d2,d0
	move.w	d0,2(a1)
	_add.w	d0,0(a1)
	_cmp.b	0(a1),d4
	bls.s	+
	bclr	d1,d3
+
	addq.w	#4,a1
	dbf	d1,-

	move.w	d3,(Oscillation_Control).w
; return_4B22:
OscillateNumDo_Return:
	rts
; End of function OscillateNumDo

; ===========================================================================
; word_4B24:
Osc_Data2:
	dc.w	 2, $10
	dc.w	 2, $18
	dc.w	 2, $20
	dc.w	 2, $30
	dc.w	 4, $20
	dc.w	 8,   8
	dc.w	 8, $40
	dc.w	 4, $40
	dc.w	 2, $38
	dc.w	 2, $38
	dc.w	 2, $20
	dc.w	 3, $30
	dc.w	 5, $50
	dc.w	 7, $70
	dc.w	 2, $40
	dc.w	 2, $40
Osc_Data2_End:



; ---------------------------------------------------------------------------
; Subroutine to change global object animation variables (like rings)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_4B64:
ChangeRingFrame:
	subq.b	#1,(Logspike_anim_counter).w
	bpl.s	+
	move.b	#$B,(Logspike_anim_counter).w
	subq.b	#1,(Logspike_anim_frame).w ; animate unused log spikes
	andi.b	#7,(Logspike_anim_frame).w
+
	subq.b	#1,(Rings_anim_counter).w
	bpl.s	+
	move.b	#7,(Rings_anim_counter).w
	addq.b	#1,(Rings_anim_frame).w ; animate rings in the level (obj25)
	andi.b	#3,(Rings_anim_frame).w
+
	subq.b	#1,(Unknown_anim_counter).w
	bpl.s	+
	move.b	#7,(Unknown_anim_counter).w
	addq.b	#1,(Unknown_anim_frame).w ; animate nothing (deleted special stage object is my best guess)
	cmpi.b	#6,(Unknown_anim_frame).w
	blo.s	+
	move.b	#0,(Unknown_anim_frame).w
+
	tst.b	(Ring_spill_anim_counter).w
	beq.s	+	; rts
	moveq	#0,d0
	move.b	(Ring_spill_anim_counter).w,d0
	add.w	(Ring_spill_anim_accum).w,d0
	move.w	d0,(Ring_spill_anim_accum).w
	rol.w	#7,d0
	andi.w	#3,d0
	move.b	d0,(Ring_spill_anim_frame).w ; animate scattered rings (obj37)
	subq.b	#1,(Ring_spill_anim_counter).w
+
	rts
; End of function ChangeRingFrame




; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

nosignpost macro actid
	cmpi.w	#actid,(Current_ZoneAndAct).w
	beq.ATTRIBUTE	+	; rts
    endm

; sub_4BD2:
SetLevelEndType:
	move.w	#0,(Level_Has_Signpost).w	; set level type to non-signpost
	tst.w	(Two_player_mode).w	; is it two-player competitive mode?
	bne.s	LevelEnd_SetSignpost	; if yes, branch
	nosignpost.w emerald_hill_zone_act_2
	nosignpost.w metropolis_zone_act_3
	nosignpost.w wing_fortress_zone_act_1
	nosignpost.w hill_top_zone_act_2
	nosignpost.w oil_ocean_zone_act_2
	nosignpost.s mystic_cave_zone_act_2
	nosignpost.s casino_night_zone_act_2
	nosignpost.s chemical_plant_zone_act_2
	nosignpost.s death_egg_zone_act_1
	nosignpost.s aquatic_ruin_zone_act_2
	nosignpost.s sky_chase_zone_act_1

; loc_4C40:
LevelEnd_SetSignpost:
	move.w	#1,(Level_Has_Signpost).w	; set level type to signpost
+	rts
; End of function SetLevelEndType


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_4C48:
CheckLoadSignpostArt:
	tst.w	(Level_Has_Signpost).w
	beq.s	+	; rts
	tst.w	(Debug_placement_mode).w
	bne.s	+	; rts
	move.w	(Camera_X_pos).w,d0
	move.w	(Camera_Max_X_pos).w,d1
	subi.w	#$100,d1
	cmp.w	d1,d0
	blt.s	SignpostUpdateTailsBounds
	tst.b	(Update_HUD_timer).w
	beq.s	SignpostUpdateTailsBounds
	cmp.w	(Camera_Min_X_pos).w,d1
	beq.s	SignpostUpdateTailsBounds
	move.w	d1,(Camera_Min_X_pos).w ; prevent camera from scrolling back to the left
	tst.w	(Two_player_mode).w
	bne.s	+	; rts
	moveq	#PLCID_Signpost,d0 ; <== PLC_1F
	bra.w	LoadPLC2		; load signpost art
; ---------------------------------------------------------------------------
; loc_4C80:
SignpostUpdateTailsBounds:
	tst.w	(Two_player_mode).w
	beq.s	+	; rts
	move.w	(Camera_X_pos_P2).w,d0
	move.w	(Tails_Max_X_pos).w,d1
	subi.w	#$100,d1
	cmp.w	d1,d0
	blt.s	+	; rts
	tst.b	(Update_HUD_timer_2P).w
	beq.s	+	; rts
	cmp.w	(Tails_Min_X_pos).w,d1
	beq.s	+	; rts
	move.w	d1,(Tails_Min_X_pos).w ; prevent Tails from going past new left boundary
+	rts
; End of function CheckLoadSignpostArt




; ===========================================================================
; macro to simplify editing the demo scripts
demoinput macro buttons,duration
btns_mask := 0
  irpc btn,"buttons"
    switch "btn"
    case "U"
btns_mask := btns_mask|button_up_mask
    case "D"
btns_mask := btns_mask|button_down_mask
    case "L"
btns_mask := btns_mask|button_left_mask
    case "R"
btns_mask := btns_mask|button_right_mask
    case "A"
btns_mask := btns_mask|button_A_mask
    case "B"
btns_mask := btns_mask|button_B_mask
    case "C"
btns_mask := btns_mask|button_C_mask
    case "S"
btns_mask := btns_mask|button_start_mask
    elsecase
    endcase
  endm
	dc.b	btns_mask,duration-1
 endm
; ---------------------------------------------------------------------------
; EHZ Demo Script (Sonic)
; ---------------------------------------------------------------------------
; byte_4CA8: Demo_Def:
Demo_EHZ:
	demoinput ,	$4C
	demoinput R,	$43
	demoinput RC,	9
	demoinput R,	$3F
	demoinput RC,	6
	demoinput R,	$B0
	demoinput RC,	$A
	demoinput R,	$46
	demoinput ,	$1E
	demoinput L,	$F
	demoinput ,	5
	demoinput L,	5
	demoinput ,	9
	demoinput L,	$3F
	demoinput ,	5
	demoinput R,	$67
	demoinput ,	$62
	demoinput R,	$12
	demoinput ,	$22
	demoinput D,	8
	demoinput DC,	7
	demoinput D,	$E
	demoinput ,	$3C
	demoinput R,	$A
	demoinput ,	$1E
	demoinput D,	7
	demoinput DC,	7
	demoinput D,	2
	demoinput ,	$F
	demoinput R,	$100
	demoinput R,	$2F
	demoinput ,	$23
	demoinput C,	8
	demoinput RC,	$10
	demoinput R,	3
	demoinput ,	$30
	demoinput RC,	$24
	demoinput R,	$BE
	demoinput ,	$C
	demoinput L,	$14
	demoinput ,	$17
	demoinput D,	3
	demoinput DC,	7
	demoinput D,	3
	demoinput ,	$64
	demoinput S,	1
	demoinput A,	1
	demoinput ,	1
; ---------------------------------------------------------------------------
; EHZ Demo Script (Tails)
; ---------------------------------------------------------------------------
; byte_4D08:
Demo_EHZ_Tails:
	demoinput ,	$3C
	demoinput R,	$10
	demoinput UR,	$44
	demoinput URC,	$7
	demoinput UR,	$7
	demoinput R,	$CA
	demoinput ,	$12
	demoinput R,	$2
	demoinput RC,	$9
	demoinput R,	$53
	demoinput ,	$12
	demoinput R,	$B
	demoinput RC,	$F
	demoinput R,	$24
	demoinput ,	$B
	demoinput C,	$5
	demoinput ,	$E
	demoinput R,	$56
	demoinput ,	$1F
	demoinput R,	$5B
	demoinput ,	$11
	demoinput R,	$100
	demoinput R,	$C1
	demoinput ,	$21
	demoinput L,	$E
	demoinput ,	$E
	demoinput C,	$5
	demoinput RC,	$10
	demoinput C,	$6
	demoinput ,	$D
	demoinput L,	$6
	demoinput ,	$5F
	demoinput R,	$74
	demoinput ,	$19
	demoinput L,	$45
	demoinput ,	$9
	demoinput D,	$31
	demoinput ,	$9
	demoinput R,	$E
	demoinput ,	$24
	demoinput R,	$28
	demoinput ,	$5
	demoinput R,	$1
	demoinput ,	$1
	demoinput ,	$1
	demoinput ,	$1
	demoinput ,	$1
	demoinput ,	$1
; ---------------------------------------------------------------------------
; CNZ Demo Script
; ---------------------------------------------------------------------------
Demo_CNZ:
	demoinput ,	$49
	demoinput R,	$11
	demoinput UR,	1
	demoinput R,	2
	demoinput UR,	7
	demoinput R,	$61
	demoinput RC,	6
	demoinput C,	2
	demoinput ,	9
	demoinput L,	3
	demoinput DL,	4
	demoinput L,	2
	demoinput ,	$1A
	demoinput R,	$12
	demoinput RC,	$1A
	demoinput C,	5
	demoinput RC,	$24
	demoinput R,	$1B
	demoinput ,	8
	demoinput L,	$11
	demoinput ,	$F
	demoinput R,	$78
	demoinput RC,	$17
	demoinput C,	1
	demoinput ,	$10
	demoinput L,	$12
	demoinput ,	8
	demoinput R,	$53
	demoinput ,	$70
	demoinput R,	$75
	demoinput ,	$38
	demoinput R,	$17
	demoinput ,	5
	demoinput L,	$27
	demoinput ,	$D
	demoinput L,	$13
	demoinput ,	$6A
	demoinput C,	$11
	demoinput RC,	3
	demoinput DRC,	6
	demoinput DR,	$15
	demoinput R,	6
	demoinput ,	6
	demoinput L,	$D
	demoinput ,	$49
	demoinput L,	$A
	demoinput ,	$1F
	demoinput R,	7
	demoinput ,	$30
	demoinput L,	2
	demoinput ,	$100
	demoinput ,	$50
	demoinput R,	1
	demoinput RC,	$C
	demoinput R,	$2B
	demoinput ,	$5F
; ---------------------------------------------------------------------------
; CPZ Demo Script
; ---------------------------------------------------------------------------
Demo_CPZ:
	demoinput ,	$47
	demoinput R,	$1C
	demoinput RC,	8
	demoinput R,	$A
	demoinput ,	$1C
	demoinput R,	$E
	demoinput RC,	$29
	demoinput R,	$100
	demoinput R,	$E8
	demoinput DR,	5
	demoinput D,	2
	demoinput L,	$34
	demoinput DL,	$68
	demoinput L,	1
	demoinput ,	$16
	demoinput C,	1
	demoinput LC,	8
	demoinput L,	$F
	demoinput ,	$18
	demoinput R,	2
	demoinput DR,	2
	demoinput R,	$D
	demoinput ,	$20
	demoinput RC,	7
	demoinput R,	$B
	demoinput ,	$1C
	demoinput L,	$E
	demoinput ,	$1D
	demoinput L,	7
	demoinput ,	$100
	demoinput ,	$E0
	demoinput R,	$F
	demoinput ,	$1D
	demoinput L,	3
	demoinput ,	$26
	demoinput R,	7
	demoinput ,	7
	demoinput C,	5
	demoinput ,	$29
	demoinput L,	$12
	demoinput ,	$18
	demoinput R,	$1A
	demoinput ,	$11
	demoinput L,	$2E
	demoinput ,	$14
	demoinput S,	1
	demoinput A,	1
	demoinput ,	1
; ---------------------------------------------------------------------------
; ARZ Demo Script
; ---------------------------------------------------------------------------
Demo_ARZ:
	demoinput ,	$43
	demoinput R,	$4B
	demoinput RC,	9
	demoinput R,	$50
	demoinput RC,	$C
	demoinput R,	6
	demoinput ,	$1B
	demoinput R,	$61
	demoinput RC,	$15
	demoinput R,	$55
	demoinput ,	$41
	demoinput R,	5
	demoinput UR,	1
	demoinput R,	$5C
	demoinput ,	$47
	demoinput R,	$3C
	demoinput RC,	9
	demoinput R,	$28
	demoinput ,	$B
	demoinput R,	$93
	demoinput RC,	$33
	demoinput R,	$23
	demoinput ,	$23
	demoinput R,	$4D
	demoinput ,	$1F
	demoinput L,	2
	demoinput UL,	3
	demoinput L,	1
	demoinput ,	$B
	demoinput L,	$D
	demoinput ,	$11
	demoinput R,	6
	demoinput ,	$62
	demoinput R,	4
	demoinput RC,	6
	demoinput R,	$17
	demoinput ,	$1C
	demoinput R,	$57
	demoinput RC,	$B
	demoinput R,	$17
	demoinput ,	$16
	demoinput R,	$D
	demoinput ,	$2C
	demoinput C,	2
	demoinput RC,	$1B
	demoinput R,	$83
	demoinput ,	$C
	demoinput S,	1

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||



;sub_4E98:
LoadZoneTiles:
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	lea	(LevelArtPointers).l,a2
	lea	(a2,d0.w),a2
	move.l	(a2)+,d0
	andi.l	#$FFFFFF,d0	; 8x8 tile pointer
	movea.l	d0,a0
	lea	(Chunk_Table).l,a1
	bsr.w	KosDec
	move.w	a1,d3
	cmpi.b	#hill_top_zone,(Current_Zone).w
	bne.s	+
	lea	(ArtKos_HTZ).l,a0
	lea	(Chunk_Table+tiles_to_bytes(ArtTile_ArtKos_NumTiles_HTZ_Main)).l,a1
	bsr.w	KosDec	; patch for HTZ
	move.w	#tiles_to_bytes(ArtTile_ArtKos_NumTiles_HTZ),d3
+
	cmpi.b	#wing_fortress_zone,(Current_Zone).w
	bne.s	+
	lea	(ArtKos_WFZ).l,a0
	lea	(Chunk_Table+tiles_to_bytes(ArtTile_ArtKos_NumTiles_WFZ_Main)).l,a1
	bsr.w	KosDec	; patch for WFZ
	move.w	#tiles_to_bytes(ArtTile_ArtKos_NumTiles_WFZ),d3
+
	cmpi.b	#death_egg_zone,(Current_Zone).w
	bne.s	+
	move.w	#tiles_to_bytes(ArtTile_ArtKos_NumTiles_DEZ),d3
+
	move.w	d3,d7
	andi.w	#$FFF,d3
	lsr.w	#1,d3
	rol.w	#4,d7
	andi.w	#$F,d7

-	move.w	d7,d2
	lsl.w	#7,d2
	lsl.w	#5,d2
	move.l	#$FFFFFF,d1
	move.w	d2,d1
	jsr	(QueueDMATransfer).l
	move.w	d7,-(sp)
	move.b	#VintID_TitleCard,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.w	RunPLC_RAM
	move.w	(sp)+,d7
	move.w	#$800,d3
	dbf	d7,-

	rts
; End of function LoadZoneTiles

; ===========================================================================

    if gameRevision<2
	nop
    endif

    if ~~removeJmpTos
JmpTo_loadZoneBlockMaps ; JmpTo
	jmp	(loadZoneBlockMaps).l
JmpTo_DeformBgLayer ; JmpTo
	jmp	(DeformBgLayer).l
JmpTo_AniArt_Load ; JmpTo
	jmp	(AniArt_Load).l
JmpTo_DrawInitialBG ; JmpTo
	jmp	(DrawInitialBG).l

	align 4
    endif




; ===========================================================================
; loc_4F64:
SpecialStage:
	cmpi.b	#7,(Current_Special_Stage).w
	blo.s	+
	move.b	#0,(Current_Special_Stage).w
+
	move.w	#SndID_SpecStageEntry,d0 ; play that funky special stage entry sound
	bsr.w	PlaySound
	move.b	#MusID_FadeOut,d0 ; fade out the music
	bsr.w	PlayMusic
	bsr.w	Pal_FadeToWhite
	tst.w	(Two_player_mode).w
	beq.s	+
	move.w	#0,(Two_player_mode).w
	st.b	(SS_2p_Flag).w ; set to -1
	bra.s	++
; ===========================================================================
+
	sf.b	(SS_2p_Flag).w ; set to 0
; (!)
+
	move	#$2700,sr		; Mask all interrupts
	lea	(VDP_control_port).l,a6
	move.w	#$8B03,(a6)		; EXT-INT disabled, V scroll by screen, H scroll by line
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8ADF,(Hint_counter_reserve).w	; H-INT every 224th scanline
	move.w	#$8200|(VRAM_SS_Plane_A_Name_Table1/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_SS_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $A000
	move.w	#$8C08,(a6)		; H res 32 cells, no interlace, S/H enabled
	move.w	#$9003,(a6)		; Scroll table size: 128x32
	move.w	#$8700,(a6)		; Background palette/color: 0/0
	move.w	#$8D00|(VRAM_Horiz_Scroll_Table/$400),(a6)		; H scroll table base: $FC00
	move.w	#$8500|(VRAM_Sprite_Attribute_Table/$200),(a6)	; Sprite attribute table base: $F800
	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l

; /------------------------------------------------------------------------\
; | We're gonna zero-fill a bunch of VRAM regions. This was done by macro, |
; | so there's gonna be a lot of wasted cycles.                            |
; \------------------------------------------------------------------------/

	dmaFillVRAM 0,VRAM_SS_Plane_A_Name_Table2,VRAM_SS_Plane_Table_Size ; clear Plane A pattern name table 1
	dmaFillVRAM 0,VRAM_SS_Plane_A_Name_Table1,VRAM_SS_Plane_Table_Size ; clear Plane A pattern name table 2
	dmaFillVRAM 0,VRAM_SS_Plane_B_Name_Table,VRAM_SS_Plane_Table_Size ; clear Plane B pattern name table
	dmaFillVRAM 0,VRAM_Horiz_Scroll_Table,VRAM_Horiz_Scroll_Table_Size  ; clear Horizontal scroll table

	clr.l	(Vscroll_Factor).w
	clr.l	(unk_F61A).w
	clr.b	(SpecialStage_Started).w

; /------------------------------------------------------------------------\
; | Now we clear out some regions in main RAM where we want to store some  |
; | of our data structures.                                                |
; \------------------------------------------------------------------------/
    if fixBugs
	clearRAM Sprite_Table,Sprite_Table_End
	clearRAM SS_Horiz_Scroll_Buf_1,SS_Horiz_Scroll_Buf_1+HorizontalScrollBuffer.len
	clearRAM SS_Shared_RAM,SS_Shared_RAM_End
    else
	; These '+4's shouldn't be here; 'clearRAM' accidentally clears an additional 4 bytes.
	clearRAM Sprite_Table,Sprite_Table_End+4
	clearRAM SS_Horiz_Scroll_Buf_1,SS_Horiz_Scroll_Buf_1+HorizontalScrollBuffer.len+4
	clearRAM SS_Shared_RAM,SS_Shared_RAM_End+4
    endif
	clearRAM Object_Display_Lists,Object_Display_Lists_End
	clearRAM Object_RAM,Object_RAM_End

    if fixBugs
	; The DMA queue needs to be reset here, to prevent the remaining queued DMA transfers from
	; overwriting the special stage's graphics.
	; In a bizarre twice of luck, the above bug actually nullifies this bug: the excessive
	; SS_Shared_RAM clear sets VDP_Command_Buffer to 0, just like the below code.
	clr.w	(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
    endif

	move	#$2300,sr
	lea	(VDP_control_port).l,a6
	move.w	#$8F02,(a6)		; VRAM pointer increment: $0002
	bsr.w	ssInitTableBuffers
	bsr.w	ssLdComprsdData
	move.w	#0,(SpecialStage_CurrentSegment).w
	moveq	#PLCID_SpecialStage,d0
	bsr.w	RunPLC_ROM
	clr.b	(Level_started_flag).w
	move.l	#0,(Camera_X_pos).w	; probably means something else in this context
	move.l	#0,(Camera_Y_pos).w
	move.l	#0,(Camera_X_pos_copy).w
	move.l	#0,(Camera_Y_pos_copy).w
	cmpi.w	#1,(Player_mode).w	; is this a Tails alone game?
	bgt.s	+			; if yes, branch
	move.b	#ObjID_SonicSS,(MainCharacter+id).w ; load Obj09 (special stage Sonic)
	tst.w	(Player_mode).w		; is this a Sonic and Tails game?
	bne.s	++			; if not, branch
+	move.b	#ObjID_TailsSS,(Sidekick+id).w ; load Obj10 (special stage Tails)
+	move.b	#ObjID_SSHUD,(SpecialStageHUD+id).w ; load Obj5E (special stage HUD)
	move.b	#ObjID_StartBanner,(SpecialStageStartBanner+id).w ; load Obj5F (special stage banner)
	move.b	#ObjID_SSNumberOfRings,(SpecialStageNumberOfRings+id).w ; load Obj87 (special stage ring count)
	move.w	#$80,(SS_Offset_X).w
	move.w	#$36,(SS_Offset_Y).w
	bsr.w	SSPlaneB_Background
	bsr.w	SSDecompressPlayerArt
	bsr.w	SSInitPalAndData
	move.l	#$C0000,(SS_New_Speed_Factor).w
	clr.w	(Ctrl_1_Logical).w
	clr.w	(Ctrl_2_Logical).w

-	move.b	#VintID_S2SS,(Vint_routine).w
	bsr.w	WaitForVint
	move.b	(SSTrack_drawing_index).w,d0
	bne.s	-

	bsr.w	SSTrack_Draw

-	move.b	#VintID_S2SS,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.w	SSTrack_Draw
	bsr.w	SSLoadCurrentPerspective
	bsr.w	SSObjectsManager
	move.b	(SSTrack_duration_timer).w,d0
	subq.w	#1,d0
	bne.s	-

	jsr	(Obj5A_CreateRingsToGoText).l
	bsr.w	SS_ScrollBG
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	move.b	#VintID_CtrlDMA,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	#MusID_SpecStage,d0
	bsr.w	PlayMusic
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	Pal_FadeFromWhite

-	bsr.w	PauseGame
	move.w	(Ctrl_1).w,(Ctrl_1_Logical).w
	move.w	(Ctrl_2).w,(Ctrl_2_Logical).w
	cmpi.b	#GameModeID_SpecialStage,(Game_Mode).w ; special stage mode?
	bne.w	SpecialStage_Unpause		; if not, branch
	move.b	#VintID_S2SS,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.w	SSTrack_Draw
	bsr.w	SSSetGeometryOffsets
	bsr.w	SSLoadCurrentPerspective
	bsr.w	SSObjectsManager
	bsr.w	SS_ScrollBG
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	tst.b	(SpecialStage_Started).w
	beq.s	-

	moveq	#PLCID_SpecStageBombs,d0
	bsr.w	LoadPLC

-	bsr.w	PauseGame
	cmpi.b	#GameModeID_SpecialStage,(Game_Mode).w ; special stage mode?
	bne.w	SpecialStage_Unpause		; if not, branch
	move.b	#VintID_S2SS,(Vint_routine).w
	bsr.w	WaitForVint
	bsr.w	SSTrack_Draw
	bsr.w	SSSetGeometryOffsets
	bsr.w	SSLoadCurrentPerspective
	bsr.w	SSObjectsManager
	bsr.w	SS_ScrollBG
	bsr.w	PalCycle_SS
	tst.b	(SS_Pause_Only_flag).w
	beq.s	+
	move.w	(Ctrl_1).w,d0
	andi.w	#(button_start_mask<<8)|button_start_mask,d0
	move.w	d0,(Ctrl_1_Logical).w
	move.w	(Ctrl_2).w,d0
	andi.w	#(button_start_mask<<8)|button_start_mask,d0
	move.w	d0,(Ctrl_2_Logical).w
	bra.s	++
; ===========================================================================
+
	move.w	(Ctrl_1).w,(Ctrl_1_Logical).w
	move.w	(Ctrl_2).w,(Ctrl_2_Logical).w
+
	jsr	(RunObjects).l
	tst.b	(SS_Check_Rings_flag).w
	bne.s	+
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	bra.s	-
; ===========================================================================
+
	andi.b	#7,(Emerald_count).w
	tst.b	(SS_2p_Flag).w
	beq.s	+
	lea	(SS2p_RingBuffer).w,a0
	move.w	(a0)+,d0
	add.w	(a0)+,d0
	add.w	(a0)+,d0
	add.w	(a0)+,d0
	add.w	(a0)+,d0
	add.w	(a0)+,d0
	bra.s	++
; ===========================================================================
+
	move.w	(Ring_count).w,d0
	add.w	(Ring_count_2P).w,d0
+
	cmp.w	(SS_Perfect_rings_left).w,d0
	bne.s	+
	st.b	(Perfect_rings_flag).w
+
	bsr.w	Pal_FadeToWhite
	tst.w	(Two_player_mode_copy).w
	bne.w	loc_540C
	move	#$2700,sr
	lea	(VDP_control_port).l,a6
	move.w	#$8200|(VRAM_Menu_Plane_A_Name_Table/$400),(a6)		; PNT A base: $C000
	move.w	#$8400|(VRAM_Menu_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $E000
	move.w	#$9001,(a6)		; Scroll table size: 64x32
	move.w	#$8C81,(a6)		; H res 40 cells, no interlace, S/H disabled
	bsr.w	ClearScreen
	jsrto	Hud_Base, JmpTo_Hud_Base
	clr.w	(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
	move	#$2300,sr
	moveq	#PalID_Result,d0
	bsr.w	PalLoad_Now
	moveq	#PLCID_Std1,d0
	bsr.w	LoadPLC2
	move.l	#vdpComm(tiles_to_bytes(ArtTile_VRAM_Start+2),VRAM,WRITE),d0
	lea	SpecialStage_ResultsLetters(pc),a0
	jsrto	LoadTitleCardSS, JmpTo_LoadTitleCardSS
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_SpecialStageResults),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_SpecialStageResults).l,a0
	bsr.w	NemDec
	move.w	(Player_mode).w,d0
	beq.s	++
	subq.w	#1,d0
	beq.s	+
	clr.w	(Ring_count).w
	bra.s	++
; ===========================================================================
+
	clr.w	(Ring_count_2P).w
+
	move.w	(Ring_count).w,(Bonus_Countdown_1).w
	move.w	(Ring_count_2P).w,(Bonus_Countdown_2).w
	clr.w	(Total_Bonus_Countdown).w
	tst.b	(Got_Emerald).w
	beq.s	+
	move.w	#1000,(Total_Bonus_Countdown).w
+
	move.b	#1,(Update_HUD_score).w
	move.b	#1,(Update_Bonus_score).w
	move.w	#MusID_EndLevel,d0
	jsr	(PlaySound).l

	clearRAM Object_Display_Lists,Object_Display_Lists_End
	clearRAM Object_RAM,Object_RAM_End

	move.b	#ObjID_SSResults,(SpecialStageResults+id).w ; load Obj6F (special stage results) at $FFFFB800
-
	move.b	#VintID_Level,(Vint_routine).w
	bsr.w	WaitForVint
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	tst.w	(Level_Inactive_flag).w
	beq.s	-
	tst.l	(Plc_Buffer).w
	bne.s	-
	move.w	#SndID_SpecStageEntry,d0
	bsr.w	PlaySound
	bsr.w	Pal_FadeToWhite
	tst.w	(Two_player_mode_copy).w
	bne.s	loc_540C
	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)
	rts
; ===========================================================================

loc_540C:
	move.w	#VsRSID_SS,(Results_Screen_2P).w
	move.b	#GameModeID_2PResults,(Game_Mode).w ; => TwoPlayerResults
	rts
; ===========================================================================

; loc_541A:
SpecialStage_Unpause:
	move.b	#MusID_Unpause,(Sound_Queue.Music0).w
	move.b	#VintID_Level,(Vint_routine).w
	bra.w	WaitForVint




; ===========================================================================
; ---------------------------------------------------------------------------
; Animated color of the twinkling stars in the special stage background
; ---------------------------------------------------------------------------
; loc_542A: Pal_UNK8:
Pal_SpecialStageStars:	dc.w  $EEE, $CCC, $AAA,	$888, $888, $AAA, $CCC,	$EEE

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_543A
PalCycle_SS:
	move.b	(Vint_runcount+3).w,d0
	andi.b	#3,d0
	bne.s	+
	move.b	(SS_Star_color_1).w,d0
	addi_.b	#1,(SS_Star_color_1).w
	andi.w	#7,d0
	add.w	d0,d0
	move.w	Pal_SpecialStageStars(pc,d0.w),(Normal_palette+$1C).w
	move.b	(SS_Star_color_2).w,d0
	addi_.b	#1,(SS_Star_color_2).w
	andi.w	#7,d0
	add.w	d0,d0
	move.w	Pal_SpecialStageStars(pc,d0.w),(Normal_palette+$1E).w
+
	cmpi.b	#6,(Current_Special_Stage).w
	bne.s	+
	cmpi.b	#3,(Current_Special_Act).w
	beq.w	SSCheckpoint_rainbow
/
	tst.b	(SS_Checkpoint_Rainbow_flag).w
	beq.s	+	; rts
	move.b	(Vint_runcount+3).w,d0
	andi.b	#7,d0
	bne.s	+	; rts
	move.b	(SS_Rainbow_palette).w,d0
	addi_.b	#1,(SS_Rainbow_palette).w
	andi.b	#3,d0
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	move.w	word_54C4(pc,d0.w),(Normal_palette_line4+$16).w
	move.w	word_54C6(pc,d0.w),(Normal_palette_line4+$18).w
	move.w	word_54C8(pc,d0.w),(Normal_palette_line4+$1A).w
+
	rts
; ===========================================================================
; special stage rainbow blinking sprite palettes... (Chaos Emerald colors?)
;word_54BC:
		dc.w   $0EE, $0C0, $0EE, $0C0
word_54C4:	dc.w   $0EE
word_54C6:	dc.w   $0CC
word_54C8:	dc.w   $088, $0E0, $0C0, $080, $EE0, $CC0, $880, $E0E, $C0C, $808
; ===========================================================================

;loc_54DC
SSCheckpoint_rainbow:
	tst.b	(SS_Pause_Only_flag).w
	beq.s	-
	moveq	#0,d0
	move.b	(Vint_runcount+3).w,d0
	andi.b	#1,d0
	bne.w	-
	move.w	(Ring_count).w,d2
	add.w	(Ring_count_2P).w,d2
	cmp.w	(SS_Ring_Requirement).w,d2
	blt.w	-
	lea	(Normal_palette+2).w,a0
	movea.l	a0,a1
	move.w	(a0)+,d0

	moveq	#$B,d1
-	move.w	(a0)+,(a1)+
	dbf	d1,-

	move.w	d0,(a1)
	rts
; End of function PalCycle_SS


;|||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_5514
SSLoadCurrentPerspective:
	cmpi.b	#4,(SSTrack_drawing_index).w
	bne.s	+	; rts
	movea.l	#SSRAM_MiscKoz_SpecialPerspective,a0
	moveq	#0,d0
	move.b	(SSTrack_mapping_frame).w,d0
	add.w	d0,d0
	adda.w	(a0,d0.w),a0
	move.l	a0,(SS_CurrentPerspective).w
+	rts
; End of function SSLoadCurrentPerspective


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_5534
SSObjectsManager:
	cmpi.b	#4,(SSTrack_drawing_index).w
	bne.w	return_55DC
	moveq	#0,d0
	move.b	(SpecialStage_CurrentSegment).w,d0
	cmp.b	(SpecialStage_LastSegment2).w,d0
	beq.w	return_55DC
	move.b	d0,(SpecialStage_LastSegment2).w
	movea.l	(SS_CurrentLevelLayout).w,a1
	move.b	(a1,d0.w),d3
	andi.w	#$7F,d3
	lea	(Ani_SSTrack_Len).l,a0
	move.b	(a0,d3.w),d3
	add.w	d3,d3
	add.w	d3,d3
	movea.l	(SS_CurrentLevelObjectLocations).w,a0
-
	bsr.w	SSAllocateObject
	bne.s	return_55DC
	moveq	#0,d0
	move.b	(a0)+,d0
	bmi.s	++
	move.b	d0,d1
	andi.b	#$40,d1
	bne.s	+
	addq.w	#1,(SS_Perfect_rings_left).w
	move.b	#ObjID_SSRing,id(a1)
	add.w	d0,d0
	add.w	d0,d0
	add.w	d3,d0
	move.w	d0,objoff_30(a1)
	move.b	(a0)+,angle(a1)
	bra.s	-
; ===========================================================================
+
	andi.w	#$3F,d0
	move.b	#ObjID_SSBomb,id(a1)
	add.w	d0,d0
	add.w	d0,d0
	add.w	d3,d0
	move.w	d0,objoff_30(a1)
	move.b	(a0)+,angle(a1)
	bra.s	-
; ===========================================================================
+
	move.l	a0,(SS_CurrentLevelObjectLocations).w
	addq.b	#1,d0
	beq.s	return_55DC
	addq.b	#1,d0
	beq.s	++
	addq.b	#1,d0
	beq.s	+
	st.b	(SS_NoCheckpoint_flag).w
	sf.b	(SS_NoCheckpointMsg_flag).w
	bra.s	++
; ===========================================================================
+
	tst.b	(SS_2p_Flag).w
	bne.s	+
	move.b	#ObjID_SSEmerald,id(a1)
	rts
; ===========================================================================
+
	move.b	#ObjID_SSMessage,id(a1)

return_55DC:
	rts
; End of function SSObjectsManager

; ===========================================================================
SSTrackPNTCommands:
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table2 + 0 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table2 + 1 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table2 + 2 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table2 + 3 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table1 + 0 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table1 + 1 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table1 + 2 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
	dc.l vdpComm(VRAM_SS_Plane_A_Name_Table1 + 3 * (PNT_Buffer_End-PNT_Buffer),VRAM,WRITE)
Ani_SSTrack_Len:
	dc.b SSTrackAni_TurnThenRise_End - SSTrackAni_TurnThenRise		; 0
	dc.b SSTrackAni_TurnThenDrop_End - SSTrackAni_TurnThenDrop		; 1
	dc.b SSTrackAni_TurnThenStraight_End - SSTrackAni_TurnThenStraight	; 2
	dc.b SSTrackAni_Straight_End - SSTrackAni_Straight			; 3
	dc.b SSTrackAni_StraightThenTurn_End - SSTrackAni_StraightThenTurn	; 4
	dc.b   0	; 5
	even

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_5604
SSTrack_Draw:
	moveq	#0,d0
	move.b	(SSTrack_drawing_index).w,d0					; Get drawing position
	cmpi.b	#4,d0								; Is it time to draw a new frame?
	bge.w	SSTrackSetOrientation						; Branch if not
	add.w	d0,d0								; Multiply by 4
	add.w	d0,d0
	bne.w	SSTrack_BeginDraw						; Branch if we don't need to start a new segment
	move.l	(SSTrack_last_mappings).w,(SSTrack_last_mappings_copy).w	; Save last mappings
	move.b	(SSTrack_mapping_frame).w,(SSTrack_last_mapping_frame).w	; Save last frame
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	move.b	(SpecialStage_CurrentSegment).w,d1				; Get current segment ID
	move.b	(SSTrack_anim_frame).w,d2					; Get current frame
	movea.l	(SS_CurrentLevelLayout).w,a1					; Pointer to level layout
	move.b	(a1,d1.w),d3							; Get segment geometry type
	andi.b	#$7F,d3								; Strip flip flag
	move.b	d3,(SSTrack_anim).w						; Set this as new animation
	move.w	d3,d1								; Copy to d1
	add.w	d3,d3								; Turn it into an index
	lea	(Ani_SpecialStageTrack).l,a1					; Animation table
	adda.w	(a1,d3.w),a1							; Add offset so a1 points to animation data
	adda.w	d2,a1								; Offset into current animation frame
	moveq	#0,d4
	move.b	(a1),d4								; d4 = animation frame to draw
	move.b	d4,(SSTrack_mapping_frame).w					; Save to RAM
	lsl.w	#2,d4
	lea	(Map_SpecialStageTrack).l,a1					; Mappings table
	movea.l	(a1,d4.w),a0							; a0 = pointer to mappings for current track frame
	movea.l	a0,a1								; Copy to a1
	moveq	#0,d2
	move.b	(a0)+,d2							; Skip the first 2 bytes
	move.b	(a0)+,d2							; Why not 'addq.l	#2,a0'?
	move.b	(a0)+,d2							; Get byte
	lsl.w	#8,d2								; Shift it up to be the high byte of a word
	move.b	(a0)+,d2							; Read another byte; why not 'move.w	(a0)+,d2'?
	addq.w	#4,d2								; Add 4
	adda.w	d2,a1								; Use as offset from start of file
	movea.l	a1,a2								; Save to a2
	moveq	#0,d2
	move.b	(a1)+,d2							; Skip the first 2 bytes
	move.b	(a1)+,d2							; Why not 'addq.l	#2,a1'?
	move.b	(a1)+,d2							; Get byte
	lsl.w	#8,d2								; Shift it up to be the high byte of a word
	move.b	(a1)+,d2							; Read another byte; why not 'move.w	(a1)+,d2'?
	addq.w	#4,d2								; Add 4
	adda.w	d2,a2								; Use as offset from previous offset
	move.b	(a2)+,d2							; Ignore the first 3 bytes
	move.b	(a2)+,d2							; Why not 'addq.l	#3,a2'?
	move.b	(a2)+,d2
	move.b	(a2)+,d2							; Get byte (unused)
	move.l	a0,(SSTrack_mappings_bitflags).w				; Save pointer to bit flags mappings
	move.l	a0,(SSTrack_last_mappings).w					; ...twice
	move.l	a1,(SSTrack_mappings_uncompressed).w				; Save pointer to uncompressed mappings
	move.l	a2,(SSTrack_mappings_RLE).w					; Save pointer to RLE mappings
	lea_	Ani_SSTrack_Len,a4						; Pointer to animation lengths
	move.b	(a4,d1.w),d2							; Get length of current animation
	move.b	(SSTrack_anim_frame).w,(SSTrack_last_anim_frame).w		; Save old frame
	addi_.b	#1,(SSTrack_anim_frame).w					; Increment current frame
	cmp.b	(SSTrack_anim_frame).w,d2					; Compare with animation length
	bne.s	SSTrack_BeginDraw						; If not equal, branch
	move.b	#0,(SSTrack_anim_frame).w					; Reset to start
	move.b	(SpecialStage_CurrentSegment).w,(SpecialStage_LastSegment).w	; Save old segment
	addi_.b	#1,(SpecialStage_CurrentSegment).w				; Increment current segment

;loc_56D2
SSTrack_BeginDraw:
	tst.b	(SS_Alternate_PNT).w						; Are we using the alternate PNT?
	beq.s	+								; Branch if not
	addi.w	#$10,d0								; Change where we will be drawing
+
	lea_	SSTrackPNTCommands,a3						; Table of VRAM commands
	movea.l	(a3,d0.w),a3							; Get command to set destination in VRAM for current frame
	move.l	a3,(VDP_control_port).l						; Send it to VDP
	lea	(VDP_data_port).l,a6
	bsr.w	SSTrackSetOrientation						; Set oriantation flags
	movea.l	(SSTrack_mappings_bitflags).w,a0				; Get pointer to bit flags mappings
	movea.l	(SSTrack_mappings_uncompressed).w,a1				; Get pointer to uncompressed mappings
	movea.l	(SSTrack_mappings_RLE).w,a2					; Get pointer to RLE mappings
	lea	(SSDrawRegBuffer).w,a3						; Pointer to register buffer from last draw
	movem.w	(a3)+,d2-d7							; Restore registers from previous call (or set them to zero)
	lea	(SSPNT_UncLUT).l,a3						; Pattern name list for drawing routines
	lea	(SSPNT_RLELUT).l,a4						; RLE-encoded pattern name list for drawing routines
	movea.w	#-8,a5								; Initialize loop counter: draws 7 lines
	moveq	#0,d0
	tst.b	(SSTrack_Orientation).w						; Is the current segment flipped?
	bne.w	SSTrackDrawLineFlipLoop						; Branch if yes

;loc_5722
SSTrackDrawLineLoop:
	adda_.w	#1,a5								; Increment loop counter
	cmpa.w	#0,a5								; Have all 7 lines been drawn?
	beq.w	SSTrackDraw_return						; If yes, return

;loc_572E
SSTrackDrawLoop_Inner:
	moveq	#0,d1
	subq.w	#1,d7								; Subtract 1 from bit counter
	bpl.s	+								; Branch if we still have bits we can use
	move.b	(a0)+,d6							; Get a new byte from bit flags
	moveq	#7,d7								; We now have 8 fresh new bits
+
	add.b	d6,d6								; Do we have to use RLE compression?
	bcc.s	SSTrackDrawRLE							; Branch if yes
	subq.b	#1,d5								; Subtract 1 from bit counter
	bpl.s	+								; Branch if we still have bits we can use
	move.b	(a1)+,d4							; Get a new byte from uncompressed mappings pointer
	moveq	#7,d5								; We now have 8 fresh new bits
+
	add.b	d4,d4								; Do we need a 10-bit index?
	bcc.s	+								; Branch if not
	moveq	#$A,d0								; d0 = 10 bits
	sub.b	d5,d0								; d0 = 10 - d5
	subq.b	#3,d0								; d0 =  7 - d5; why not shorten it to 'moveq	#7,d0 \n	sub.b	d5,d0'?
	add.w	d0,d0								; Convert into table index
	move.w	SSTrackDrawUnc_Read10LUT(pc,d0.w),d0
	jmp	SSTrackDrawUnc_Read10LUT(pc,d0.w)
; ===========================================================================
;off_5758
SSTrackDrawUnc_Read10LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got7	; 0
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got6	; 1
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got5	; 2
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got4	; 3
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got3	; 4
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got2	; 5
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got1	; 6
		offsetTableEntry.w SSTrackDrawUnc_Read10_Got0	; 7
; ===========================================================================
+
	moveq	#6,d0								; d0 = 6
	sub.b	d5,d0								; d0 = 6 - d5
	addq.b	#1,d0								; d0 = 7 - d5; why not shorten it to 'moveq	#7,d0 \n	sub.b	d5,d0'?
	add.w	d0,d0								; Convert into table index
	move.w	SSTrackDrawUnc_Read6LUT(pc,d0.w),d0
	jmp	SSTrackDrawUnc_Read6LUT(pc,d0.w)
; ===========================================================================
;off_5778
SSTrackDrawUnc_Read6LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got7	; 0
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got6	; 1
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got5	; 2
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got4	; 3
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got3	; 4
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got2	; 5
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got1	; 6
		offsetTableEntry.w SSTrackDrawUnc_Read6_Got0	; 7
; ===========================================================================

SSTrackDrawRLE:
	subq.b	#1,d3								; Subtract 1 from bit counter
	bpl.s	++								; Branch if we still have bits we can use
	move.b	(a2)+,d2							; Get a new byte from RLE mappings pointer
	cmpi.b	#-1,d2								; Is d2 equal to -1?
	bne.s	+								; Branch if not
	moveq	#0,d3								; Set bit counter to zero
	bra.w	SSTrackDrawLineLoop
; ===========================================================================
+
	moveq	#7,d3								; We now have 8 fresh new bits
+
	add.b	d2,d2								; Do we need a 7-bit index?
	bcc.s	+								; Branch if not
	moveq	#7,d0								; d0 = 7
	sub.b	d3,d0								; d0 = 10 - d3
	add.b	d0,d0								; Convert into table index
	move.w	SSTrackDrawRLE_Read7LUT(pc,d0.w),d0
	jmp	SSTrackDrawRLE_Read7LUT(pc,d0.w)
; ===========================================================================
;off_57AE
SSTrackDrawRLE_Read7LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got7	; 0
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got6	; 1
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got5	; 2
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got4	; 3
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got3	; 4
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got2	; 5
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got1	; 6
		offsetTableEntry.w SSTrackDrawRLE_Read7_Got0	; 7
; ===========================================================================
+
	moveq	#6,d0								; d0 = 6
	sub.b	d3,d0								; d0 = 6 - d3
	addq.b	#1,d0								; d0 = 7 - d3; why not shorten it to 'moveq	#7,d0 \n	sub.b	d3,d0'?
	add.b	d0,d0								; Convert into table index
	move.w	SSTrackDrawRLE_Read6LUT(pc,d0.w),d0
	jmp	SSTrackDrawRLE_Read6LUT(pc,d0.w)
; ===========================================================================
;off_57CE
SSTrackDrawRLE_Read6LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got7	; 0
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got6	; 1
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got5	; 2
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got4	; 3
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got3	; 4
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got2	; 5
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got1	; 6
		offsetTableEntry.w SSTrackDrawRLE_Read6_Got0	; 7
; ===========================================================================
;loc_57DE
SSTrackDrawUnc_Read10_Got0:
	; Reads 10 bits from uncompressed mappings, 0 bits in bit buffer
	moveq	#0,d0
	move.b	(a1)+,d0
	lsl.w	#2,d0
	move.b	(a1)+,d4
	rol.b	#2,d4
	move.b	d4,d1
	andi.b	#3,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#6,d5
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5806
SSTrackDrawUnc_Read10_Got1:
	; Reads 10 bits from uncompressed mappings, 1 bit in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$200,d0
	move.b	(a1)+,d1
	lsl.w	#1,d1
	or.w	d1,d0
	move.b	(a1)+,d4
	rol.b	#1,d4
	move.b	d4,d1
	andi.b	#1,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#7,d5
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5836
SSTrackDrawUnc_Read10_Got2:
	; Reads 10 bits from uncompressed mappings, 2 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$300,d0
	move.b	(a1)+,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#0,d5								; Bit buffer now empty
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5856
SSTrackDrawUnc_Read10_Got3:
	; Reads 10 bits from uncompressed mappings, 3 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$380,d0
	move.b	(a1)+,d4
	ror.b	#1,d4
	move.b	d4,d1
	andi.b	#$7F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#1,d5								; Bit buffer now has 1 bit
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5880
SSTrackDrawUnc_Read10_Got4:
	; Reads 10 bits from uncompressed mappings, 4 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3C0,d0
	move.b	(a1)+,d4
	ror.b	#2,d4
	move.b	d4,d1
	andi.b	#$3F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#2,d5								; Bit buffer now has 2 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_58AA
SSTrackDrawUnc_Read10_Got5:
	; Reads 10 bits from uncompressed mappings, 5 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3E0,d0
	move.b	(a1)+,d4
	ror.b	#3,d4
	move.b	d4,d1
	andi.b	#$1F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#3,d5								; Bit buffer now has 3 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_58D4
SSTrackDrawUnc_Read10_Got6:
	; Reads 10 bits from uncompressed mappings, 6 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3F0,d0
	move.b	(a1)+,d4
	ror.b	#4,d4
	move.b	d4,d1
	andi.b	#$F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#4,d5								; Bit buffer now has 4 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_58FE
SSTrackDrawUnc_Read10_Got7:
	; Reads 10 bits from uncompressed mappings, 7 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3F8,d0
	move.b	(a1)+,d4
	rol.b	#3,d4
	move.b	d4,d1
	andi.b	#7,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#5,d5								; Bit buffer now has 5 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5928
SSTrackDrawUnc_Read6_Got0:
	; Reads 6 bits from uncompressed mappings, 0 bits in bit buffer
	move.b	(a1)+,d4
	ror.b	#2,d4
	move.b	d4,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#2,d5								; Bit buffer now has 2 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5944
SSTrackDrawUnc_Read6_Got1:
	; Reads 6 bits from uncompressed mappings, 1 bit in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$20,d0
	move.b	(a1)+,d4
	ror.b	#3,d4
	move.b	d4,d1
	andi.b	#$1F,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#3,d5								; Bit buffer now has 3 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_596A
SSTrackDrawUnc_Read6_Got2:
	; Reads 6 bits from uncompressed mappings, 2 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$30,d0
	move.b	(a1)+,d4
	ror.b	#4,d4
	move.b	d4,d1
	andi.b	#$F,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#4,d5								; Bit buffer now has 4 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5990
SSTrackDrawUnc_Read6_Got3:
	; Reads 6 bits from uncompressed mappings, 3 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$38,d0
	move.b	(a1)+,d4
	rol.b	#3,d4
	move.b	d4,d1
	andi.b	#7,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#5,d5								; Bit buffer now has 5 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_59B6
SSTrackDrawUnc_Read6_Got4:
	; Reads 6 bits from uncompressed mappings, 4 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$3C,d0
	move.b	(a1)+,d4
	rol.b	#2,d4
	move.b	d4,d1
	andi.b	#3,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#6,d5								; Bit buffer now has 6 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_59DC
SSTrackDrawUnc_Read6_Got5:
	; Reads 6 bits from uncompressed mappings, 5 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$3E,d0
	move.b	(a1)+,d4
	rol.b	#1,d4
	move.b	d4,d1
	andi.b	#1,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#7,d5								; Bit buffer now has 7 bits
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5A02
SSTrackDrawUnc_Read6_Got6:
	; Reads 6 bits from uncompressed mappings, 6 bits in bit buffer
	lsr.b	#2,d4
	andi.w	#$3F,d4
	add.w	d4,d4
	move.w	(a3,d4.w),d4
	ori.w	#palette_line_3,d4
	move.w	d4,(a6)
	moveq	#0,d5								; Bit buffer now empty
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5A1A
SSTrackDrawUnc_Read6_Got7:
	; Reads 6 bits from uncompressed mappings, 7 bits in bit buffer
	ror.b	#2,d4
	move.b	d4,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	ori.w	#palette_line_3,d0
	move.w	d0,(a6)
	moveq	#1,d5								; Bit buffer now has 1 bit
	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5A34
SSTrackDrawRLE_Read7_Got0:
	; Reads 7 bits from RLE-compressed mappings, 0 bits in bit buffer
	move.b	(a2)+,d2
	ror.b	#1,d2
	move.b	d2,d0
	andi.w	#$7F,d0
	moveq	#1,d3								; Bit buffer now has 1 bit
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5A66
SSTrackDrawRLE_Read7_Got1:
	; Reads 7 bits from RLE-compressed mappings, 1 bit in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$40,d1
	move.b	(a2)+,d2
	ror.b	#2,d2
	move.b	d2,d0
	andi.w	#$3F,d0
	or.b	d1,d0
	moveq	#2,d3								; Bit buffer now has 2 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5AA2
SSTrackDrawRLE_Read7_Got2:
	; Reads 7 bits from RLE-compressed mappings, 2 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$60,d1
	move.b	(a2)+,d2
	ror.b	#3,d2
	move.b	d2,d0
	andi.w	#$1F,d0
	or.b	d1,d0
	moveq	#3,d3								; Bit buffer now has 3 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5ADE
SSTrackDrawRLE_Read7_Got3:
	; Reads 7 bits from RLE-compressed mappings, 3 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$70,d1
	move.b	(a2)+,d2
	ror.b	#4,d2
	move.b	d2,d0
	andi.w	#$F,d0
	or.b	d1,d0
	moveq	#4,d3								; Bit buffer now has 4 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5B1A
SSTrackDrawRLE_Read7_Got4:
	; Reads 7 bits from RLE-compressed mappings, 4 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$78,d1
	move.b	(a2)+,d2
	rol.b	#3,d2
	move.b	d2,d0
	andi.w	#7,d0
	or.b	d1,d0
	moveq	#5,d3								; Bit buffer now has 5 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5B56
SSTrackDrawRLE_Read7_Got5:
	; Reads 7 bits from RLE-compressed mappings, 5 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$7C,d1
	move.b	(a2)+,d2
	rol.b	#2,d2
	move.b	d2,d0
	andi.w	#3,d0
	or.b	d1,d0
	moveq	#6,d3								; Bit buffer now has 6 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5B92
SSTrackDrawRLE_Read7_Got6:
	; Reads 7 bits from RLE-compressed mappings, 6 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$7E,d1
	move.b	(a2)+,d2
	rol.b	#1,d2
	move.b	d2,d0
	andi.w	#1,d0
	or.b	d1,d0
	moveq	#7,d3								; Bit buffer now has 7 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5BCE
SSTrackDrawRLE_Read7_Got7:
	; Reads 7 bits from RLE-compressed mappings, 7 bits in bit buffer
	lsr.b	#1,d2
	andi.w	#$7F,d2
	moveq	#0,d3								; Bit buffer now empty
	cmpi.b	#$7F,d2
	beq.w	SSTrackDrawLineLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d2
	add.w	d2,d2
	add.w	d2,d2
	move.w	(a4,d2.w),d1
	move.w	2(a4,d2.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5BFC
SSTrackDrawRLE_Read6_Got0:
	; Reads 6 bits from RLE-compressed mappings, 0 bits in bit buffer
	move.b	(a2)+,d2
	ror.b	#2,d2
	move.b	d2,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#2,d3								; Bit buffer now has 2 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5C22
SSTrackDrawRLE_Read6_Got1:
	; Reads 6 bits from RLE-compressed mappings, 1 bit in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$20,d0
	move.b	(a2)+,d2
	ror.b	#3,d2
	move.b	d2,d1
	andi.b	#$1F,d1
	or.b	d1,d0
	moveq	#3,d3								; Bit buffer now has 3 bits
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5C52
SSTrackDrawRLE_Read6_Got2:
	; Reads 6 bits from RLE-compressed mappings, 2 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$30,d0
	move.b	(a2)+,d2
	ror.b	#4,d2
	move.b	d2,d1
	andi.b	#$F,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#4,d3								; Bit buffer now has 4 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5C82
SSTrackDrawRLE_Read6_Got3:
	; Reads 6 bits from RLE-compressed mappings, 3 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$38,d0
	move.b	(a2)+,d2
	rol.b	#3,d2
	move.b	d2,d1
	andi.b	#7,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#5,d3								; Bit buffer now has 5 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5CB2
SSTrackDrawRLE_Read6_Got4:
	; Reads 6 bits from RLE-compressed mappings, 4 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$3C,d0
	move.b	(a2)+,d2
	rol.b	#2,d2
	move.b	d2,d1
	andi.b	#3,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#6,d3								; Bit buffer now has 6 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5CE2
SSTrackDrawRLE_Read6_Got5:
	; Reads 6 bits from RLE-compressed mappings, 5 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$3E,d0
	move.b	(a2)+,d2
	rol.b	#1,d2
	move.b	d2,d1
	andi.b	#1,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#7,d3								; Bit buffer now has 7 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5D12
SSTrackDrawRLE_Read6_Got6:
	; Reads 6 bits from RLE-compressed mappings, 6 bits in bit buffer
	lsr.b	#2,d2
	andi.w	#$3F,d2
	add.w	d2,d2
	add.w	d2,d2
	moveq	#0,d3								; Bit buffer now empty
	move.w	(a4,d2.w),d1
	move.w	2(a4,d2.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================
;loc_5D34
SSTrackDrawRLE_Read6_Got7:
	; Reads 6 bits from RLE-compressed mappings, 7 bits in bit buffer
	ror.b	#2,d2
	move.b	d2,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#1,d3								; Bit buffer now has 1 bit
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,(a6)
	dbf	d0,-

	bra.w	SSTrackDrawLoop_Inner
; ===========================================================================

;loc_5D58
SSTrackDraw_return:
	cmpi.b	#3,(SSTrack_drawing_index).w					; Have we drawn a full frame?
	beq.s	+								; Branch if yes
	move.l	a0,(SSTrack_mappings_bitflags).w				; Save pointer
	move.l	a1,(SSTrack_mappings_uncompressed).w				; Save pointer
	move.l	a2,(SSTrack_mappings_RLE).w					; Save pointer
	lea	(SSDrawRegBuffer_End).w,a3					; Pointer to end of registry buffer
	movem.w	d2-d7,-(a3)							; Save the bit buffers and bit counters
	rts
; ===========================================================================
+
	lea	(SSDrawRegBuffer).w,a2						; Pointer to registry buffer
	moveq	#0,d0
    rept 6
	move.w	d0,(a2)+							; Clear bit buffers and bit counters
    endm
	rts
; ===========================================================================

;loc_5D8A
SSTrackDrawLineFlipLoop:
	adda_.w	#1,a5								; Increment loop counter
	cmpa.w	#0,a5								; Have all 8 lines been drawn?
	beq.w	SSTrackDraw_return						; If yes, return
	lea	(PNT_Buffer).w,a6						; Destination buffer
	swap	d0								; High word starts at 0
	addi.w	#$100,d0							; Adding $100 means seek to end of current line/start of next line
	andi.w	#$F00,d0							; Keep to confines
	adda.w	d0,a6								; Seek to end of current line
	swap	d0								; Leaves the low word of d0 free for use

;loc_5DA8
SSTrackDrawFlipLoop_Inner:
	moveq	#0,d1
	subq.w	#1,d7								; Subtract 1 from bit counter
	bpl.s	+								; Branch if we still have bits we can use
	move.b	(a0)+,d6							; Get a new byte from bit flags
	moveq	#7,d7								; We now have 8 fresh new bits
+
	add.b	d6,d6								; Do we have to use RLE compression?
	bcc.s	SSTrackDrawFlipRLE						; Branch if yes
	subq.b	#1,d5								; Subtract 1 from bit counter
	bpl.s	+								; Branch if we still have bits we can use
	move.b	(a1)+,d4							; Get a new byte from uncompressed mappings pointer
	moveq	#7,d5								; We now have 8 fresh new bits
+
	add.b	d4,d4								; Do we need a 10-bit index?
	bcc.s	+								; Branch if not
	move.w	#$A,d0								; d0 = 10 bits
	sub.b	d5,d0								; d0 = 10 - d5
	subq.b	#3,d0								; d0 =  7 - d5; why not shorten it to 'moveq	#7,d0 \n	sub.b	d5,d0'?
	add.w	d0,d0								; Convert into table index
	move.w	SSTrackDrawFlipUnc_Read10LUT(pc,d0.w),d0
	jmp	SSTrackDrawFlipUnc_Read10LUT(pc,d0.w)
; ===========================================================================
;off_5DD4
SSTrackDrawFlipUnc_Read10LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got7	; 0
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got6	; 1
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got5	; 2
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got4	; 3
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got3	; 4
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got2	; 5
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got1	; 6
		offsetTableEntry.w SSTrackDrawFlipUnc_Read10_Got0	; 7
; ===========================================================================
+
	move.w	#6,d0								; d0 = 6
	sub.b	d5,d0								; d0 = 6 - d5
	addq.b	#1,d0								; d0 = 7 - d5; why not shorten it to 'moveq	#7,d0 \n	sub.b	d5,d0'?
	add.w	d0,d0								; Convert into table index
	move.w	SSTrackDrawFlipUnc_Read6LUT(pc,d0.w),d0
	jmp	SSTrackDrawFlipUnc_Read6LUT(pc,d0.w)
; ===========================================================================
;off_5DF6
SSTrackDrawFlipUnc_Read6LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got7	; 0
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got6	; 1
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got5	; 2
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got4	; 3
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got3	; 4
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got2	; 5
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got1	; 6
		offsetTableEntry.w SSTrackDrawFlipUnc_Read6_Got0	; 7
; ===========================================================================
;loc_5E06
SSTrackDrawFlipRLE:
	subq.b	#1,d3								; Subtract 1 from bit counter
	bpl.s	++								; Branch if we still have bits we can use
	move.b	(a2)+,d2							; Get a new byte from RLE mappings pointer
	cmpi.b	#-1,d2								; Is d2 equal to -1?
	bne.s	+								; Branch if not
	moveq	#0,d3								; Set bit counter to zero
	bra.w	SSTrackDrawLineFlipLoop
; ===========================================================================
+
	moveq	#7,d3								; We now have 8 fresh new bits
+
	add.b	d2,d2								; Do we need a 7-bit index?
	bcc.s	+								; Branch if not
	move.w	#7,d0								; d0 = 7
	sub.b	d3,d0								; d0 = 10 - d3
	add.b	d0,d0								; Convert into table index
	move.w	SSTrackDrawFlipRLE_Read7LUT(pc,d0.w),d0
	jmp	SSTrackDrawFlipRLE_Read7LUT(pc,d0.w)
; ===========================================================================
;off_5E2E
SSTrackDrawFlipRLE_Read7LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got7	; 0
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got6	; 1
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got5	; 2
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got4	; 3
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got3	; 4
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got2	; 5
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got1	; 6
		offsetTableEntry.w SSTrackDrawFlipRLE_Read7_Got0	; 7
; ===========================================================================
+
	move.w	#6,d0								; d0 = 6
	sub.b	d3,d0								; d0 = 6 - d3
	addq.b	#1,d0								; d0 = 7 - d3; why not shorten it to 'moveq	#7,d0 \n	sub.b	d3,d0'?
	add.b	d0,d0								; Convert into table index
	move.w	SSTrackDrawFlipRLE_Read6LUT(pc,d0.w),d0
	jmp	SSTrackDrawFlipRLE_Read6LUT(pc,d0.w)
; ===========================================================================
;off_5E50
SSTrackDrawFlipRLE_Read6LUT:	offsetTable
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got7	; 0
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got6	; 1
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got5	; 2
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got4	; 3
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got3	; 4
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got2	; 5
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got1	; 6
		offsetTableEntry.w SSTrackDrawFlipRLE_Read6_Got0	; 7
; ===========================================================================
;loc_5E60
SSTrackDrawFlipUnc_Read10_Got0:
	; Reads 10 bits from uncompressed mappings, 0 bits in bit buffer
	move.w	#0,d0
	move.b	(a1)+,d0
	lsl.w	#2,d0
	move.b	(a1)+,d4
	rol.b	#2,d4
	move.b	d4,d1
	andi.b	#3,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#6,d5								; Bit buffer now has 6 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5E8A
SSTrackDrawFlipUnc_Read10_Got1:
	; Reads 10 bits from uncompressed mappings, 1 bit in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$200,d0
	move.b	(a1)+,d1
	lsl.w	#1,d1
	or.w	d1,d0
	move.b	(a1)+,d4
	rol.b	#1,d4
	move.b	d4,d1
	andi.b	#1,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#7,d5								; Bit buffer now has 7 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5EBA
SSTrackDrawFlipUnc_Read10_Got2:
	; Reads 10 bits from uncompressed mappings, 2 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$300,d0
	move.b	(a1)+,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#0,d5								; Bit buffer now empty
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5EDA
SSTrackDrawFlipUnc_Read10_Got3:
	; Reads 10 bits from uncompressed mappings, 3 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$380,d0
	move.b	(a1)+,d4
	ror.b	#1,d4
	move.b	d4,d1
	andi.b	#$7F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#1,d5								; Bit buffer now has 1 bit
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5F04
SSTrackDrawFlipUnc_Read10_Got4:
	; Reads 10 bits from uncompressed mappings, 4 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3C0,d0
	move.b	(a1)+,d4
	ror.b	#2,d4
	move.b	d4,d1
	andi.b	#$3F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#2,d5								; Bit buffer now has 2 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5F2E
SSTrackDrawFlipUnc_Read10_Got5:
	; Reads 10 bits from uncompressed mappings, 5 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3E0,d0
	move.b	(a1)+,d4
	ror.b	#3,d4
	move.b	d4,d1
	andi.b	#$1F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#3,d5								; Bit buffer now has 3 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5F58
SSTrackDrawFlipUnc_Read10_Got6:
	; Reads 10 bits from uncompressed mappings, 6 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3F0,d0
	move.b	(a1)+,d4
	ror.b	#4,d4
	move.b	d4,d1
	andi.b	#$F,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#4,d5								; Bit buffer now has 4 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5F82
SSTrackDrawFlipUnc_Read10_Got7:
	; Reads 10 bits from uncompressed mappings, 7 bits in bit buffer
	move.b	d4,d0
	lsl.w	#2,d0
	andi.w	#$3F8,d0
	move.b	(a1)+,d4
	rol.b	#3,d4
	move.b	d4,d1
	andi.b	#7,d1
	or.b	d1,d0
	addi.w	#(SSPNT_UncLUT_Part2-SSPNT_UncLUT)/2,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#5,d5								; Bit buffer now has 5 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5FAC
SSTrackDrawFlipUnc_Read6_Got0:
	; Reads 6 bits from uncompressed mappings, 0 bits in bit buffer
	move.b	(a1)+,d4
	ror.b	#2,d4
	move.b	d4,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#2,d5								; Bit buffer now has 2 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5FC8
SSTrackDrawFlipUnc_Read6_Got1:
	; Reads 6 bits from uncompressed mappings, 1 bit in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$20,d0
	move.b	(a1)+,d4
	ror.b	#3,d4
	move.b	d4,d1
	andi.b	#$1F,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#3,d5								; Bit buffer now has 3 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_5FEE
SSTrackDrawFlipUnc_Read6_Got2:
	; Reads 6 bits from uncompressed mappings, 2 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$30,d0
	move.b	(a1)+,d4
	ror.b	#4,d4
	move.b	d4,d1
	andi.b	#$F,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#4,d5								; Bit buffer now has 4 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6014
SSTrackDrawFlipUnc_Read6_Got3:
	; Reads 6 bits from uncompressed mappings, 3 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$38,d0
	move.b	(a1)+,d4
	rol.b	#3,d4
	move.b	d4,d1
	andi.b	#7,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#5,d5								; Bit buffer now has 5 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_603A
SSTrackDrawFlipUnc_Read6_Got4:
	; Reads 6 bits from uncompressed mappings, 4 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$3C,d0
	move.b	(a1)+,d4
	rol.b	#2,d4
	move.b	d4,d1
	andi.b	#3,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#6,d5								; Bit buffer now has 6 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6060
SSTrackDrawFlipUnc_Read6_Got5:
	; Reads 6 bits from uncompressed mappings, 5 bits in bit buffer
	move.b	d4,d0
	lsr.b	#2,d0
	andi.w	#$3E,d0
	move.b	(a1)+,d4
	rol.b	#1,d4
	move.b	d4,d1
	andi.b	#1,d1
	or.b	d1,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#7,d5								; Bit buffer now has 7 bits
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6086
SSTrackDrawFlipUnc_Read6_Got6:
	; Reads 6 bits from uncompressed mappings, 6 bits in bit buffer
	lsr.b	#2,d4
	andi.w	#$3F,d4
	add.w	d4,d4
	move.w	(a3,d4.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#0,d5								; Bit buffer now empty
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_609E
SSTrackDrawFlipUnc_Read6_Got7:
	; Reads 6 bits from uncompressed mappings, 7 bits in bit buffer
	ror.b	#2,d4
	move.b	d4,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0
	eori.w	#flip_x|palette_line_3,d0
	move.w	d0,-(a6)
	moveq	#1,d5								; Bit buffer now has 1 bit
	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_60B8
SSTrackDrawFlipRLE_Read7_Got0:
	; Reads 7 bits from RLE-compressed mappings, 0 bits in bit buffer
	move.b	(a2)+,d2
	ror.b	#1,d2
	move.b	d2,d0
	andi.w	#$7F,d0
	moveq	#1,d3								; Bit buffer now has 1 bit
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_60EA
SSTrackDrawFlipRLE_Read7_Got1:
	; Reads 7 bits from RLE-compressed mappings, 1 bit in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$40,d1
	move.b	(a2)+,d2
	ror.b	#2,d2
	move.b	d2,d0
	andi.w	#$3F,d0
	or.b	d1,d0
	moveq	#2,d3								; Bit buffer now has 2 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6126
SSTrackDrawFlipRLE_Read7_Got2:
	; Reads 7 bits from RLE-compressed mappings, 2 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$60,d1
	move.b	(a2)+,d2
	ror.b	#3,d2
	move.b	d2,d0
	andi.w	#$1F,d0
	or.b	d1,d0
	moveq	#3,d3								; Bit buffer now has 3 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6162
SSTrackDrawFlipRLE_Read7_Got3:
	; Reads 7 bits from RLE-compressed mappings, 3 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$70,d1
	move.b	(a2)+,d2
	ror.b	#4,d2
	move.b	d2,d0
	andi.w	#$F,d0
	or.b	d1,d0
	moveq	#4,d3								; Bit buffer now has 4 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_619E
SSTrackDrawFlipRLE_Read7_Got4:
	; Reads 7 bits from RLE-compressed mappings, 4 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$78,d1
	move.b	(a2)+,d2
	rol.b	#3,d2
	move.b	d2,d0
	andi.w	#7,d0
	or.b	d1,d0
	moveq	#5,d3								; Bit buffer now has 5 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_61DA
SSTrackDrawFlipRLE_Read7_Got5:
	; Reads 7 bits from RLE-compressed mappings, 5 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$7C,d1
	move.b	(a2)+,d2
	rol.b	#2,d2
	move.b	d2,d0
	andi.w	#3,d0
	or.b	d1,d0
	moveq	#6,d3								; Bit buffer now has 6 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6216
SSTrackDrawFlipRLE_Read7_Got6:
	; Reads 7 bits from RLE-compressed mappings, 6 bits in bit buffer
	move.b	d2,d1
	lsr.b	#1,d1
	andi.b	#$7E,d1
	move.b	(a2)+,d2
	rol.b	#1,d2
	move.b	d2,d0
	andi.w	#1,d0
	or.b	d1,d0
	moveq	#7,d3								; Bit buffer now has 7 bits
	cmpi.b	#$7F,d0
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6252
SSTrackDrawFlipRLE_Read7_Got7:
	; Reads 7 bits from RLE-compressed mappings, 7 bits in bit buffer
	lsr.b	#1,d2
	andi.w	#$7F,d2
	moveq	#0,d3								; Bit buffer now empty
	cmpi.b	#$7F,d2
	beq.w	SSTrackDrawLineFlipLoop
	addi.w	#(SSPNT_RLELUT_Part2-SSPNT_RLELUT)/4,d2
	add.w	d2,d2
	add.w	d2,d2
	move.w	(a4,d2.w),d1
	move.w	2(a4,d2.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6280
SSTrackDrawFlipRLE_Read6_Got0:
	; Reads 6 bits from RLE-compressed mappings, 0 bits in bit buffer
	move.b	(a2)+,d2
	ror.b	#2,d2
	move.b	d2,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#2,d3								; Bit buffer now has 2 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_62A6
SSTrackDrawFlipRLE_Read6_Got1:
	; Reads 6 bits from RLE-compressed mappings, 1 bit in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$20,d0
	move.b	(a2)+,d2
	ror.b	#3,d2
	move.b	d2,d1
	andi.b	#$1F,d1
	or.b	d1,d0
	moveq	#3,d3								; Bit buffer now has 3 bits
	add.w	d0,d0
	add.w	d0,d0
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_62D6
SSTrackDrawFlipRLE_Read6_Got2:
	; Reads 6 bits from RLE-compressed mappings, 2 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$30,d0
	move.b	(a2)+,d2
	ror.b	#4,d2
	move.b	d2,d1
	andi.b	#$F,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#4,d3								; Bit buffer now has 4 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6306
SSTrackDrawFlipRLE_Read6_Got3:
	; Reads 6 bits from RLE-compressed mappings, 3 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$38,d0
	move.b	(a2)+,d2
	rol.b	#3,d2
	move.b	d2,d1
	andi.b	#7,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#5,d3								; Bit buffer now has 5 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6336
SSTrackDrawFlipRLE_Read6_Got4:
	; Reads 6 bits from RLE-compressed mappings, 4 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$3C,d0
	move.b	(a2)+,d2
	rol.b	#2,d2
	move.b	d2,d1
	andi.b	#3,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#6,d3								; Bit buffer now has 6 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6366
SSTrackDrawFlipRLE_Read6_Got5:
	; Reads 6 bits from RLE-compressed mappings, 5 bits in bit buffer
	move.b	d2,d0
	lsr.b	#2,d0
	andi.w	#$3E,d0
	move.b	(a2)+,d2
	rol.b	#1,d2
	move.b	d2,d1
	andi.b	#1,d1
	or.b	d1,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#7,d3								; Bit buffer now has 7 bits
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_6396
SSTrackDrawFlipRLE_Read6_Got6:
	; Reads 6 bits from RLE-compressed mappings, 6 bits in bit buffer
	lsr.b	#2,d2
	andi.w	#$3F,d2
	add.w	d2,d2
	add.w	d2,d2
	moveq	#0,d3								; Bit buffer now empty
	move.w	(a4,d2.w),d1
	move.w	2(a4,d2.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner
; ===========================================================================
;loc_63B8
SSTrackDrawFlipRLE_Read6_Got7:
	; Reads 6 bits from RLE-compressed mappings, 7 bits in bit buffer
	ror.b	#2,d2
	move.b	d2,d0
	andi.w	#$3F,d0
	add.w	d0,d0
	add.w	d0,d0
	moveq	#1,d3								; Bit buffer now has 1 bit
	move.w	(a4,d0.w),d1
	move.w	2(a4,d0.w),d0
	ori.w	#palette_line_3|high_priority,d1

-	move.w	d1,-(a6)
	dbf	d0,-

	bra.w	SSTrackDrawFlipLoop_Inner

; ===========================================================================
; frames of animation of the special stage track
; this chooses how objects curve along the track as well as which track frame to draw
; off_63DC:
Ani_SpecialStageTrack:	offsetTable
	offsetTableEntry.w SSTrackAni_TurnThenRise	; 0
	offsetTableEntry.w SSTrackAni_TurnThenDrop	; 1
	offsetTableEntry.w SSTrackAni_TurnThenStraight	; 2
	offsetTableEntry.w SSTrackAni_Straight		; 3
	offsetTableEntry.w SSTrackAni_StraightThenTurn	; 4
; byte_63E6:
SSTrackAni_TurnThenRise:
	dc.b $26,$27,$28,$29,$2A,$2B,$26 ; turning
	dc.b   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, $A, $B, $C, $D, $E, $F,$10 ; rise
SSTrackAni_TurnThenRise_End:
; byte_63FE:
SSTrackAni_TurnThenDrop:
	dc.b $26,$27,$28,$29,$2A,$2B,$26 ; turning
	dc.b $15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,$20,$21,$22,$23,$24,$25 ; drop
SSTrackAni_TurnThenDrop_End:
; byte_6416:
SSTrackAni_TurnThenStraight:
	dc.b $26,$27,$28,$29,$2A,$2B,$26 ; turning
	dc.b $2C,$2D,$2E,$2F,$30 ; exit turn
SSTrackAni_TurnThenStraight_End:
; byte_6422:
SSTrackAni_Straight:
	dc.b $11,$12,$13,$14,$11,$12,$13,$14 ; straight
	dc.b $11,$12,$13,$14,$11,$12,$13,$14 ; straight
SSTrackAni_Straight_End:
; byte_6432:
SSTrackAni_StraightThenTurn:
	dc.b $11,$12,$13,$14 ; straight
	dc.b $31,$32,$33,$34,$35,$36,$37 ; enter turn
SSTrackAni_StraightThenTurn_End:

	even

; ===========================================================================
; pointers to the mappings for each frame of the special stage track
; indexed into by the numbers used in the above animations
;
; Format of each mappings file:
;	File is divided in 3 segments, with the same structure:
;	Segment structure:
;		4-byte unsigned length of segment (not counting the 4 bytes used for length);
;		the first 2 bytes of each length is ignored, and only the last 2 bytes are
;		actually used.
;		Rest of the segment is mappings data, as follows:
;	1st segment:
;		Mappings data is a bitstream indicating whether to draw a single tile at
;		a time using the uncompressed mappings (see 2nd segment) or a sequence of
;		tiles using the RLE mappings (see 3rd segment).
;	2nd segment:
;		Mappings data is a bitstream: the first bit in each cycle determines how
;		many bits from the stream are to be used as an index to the uncompressed
;		pattern name list SSPNT_UncLUT: if the first bit is set, 10 bits form an
;		index into SSPNT_UncLUT_Part2, otherwise 6 bits are used as an index into
;		SSPNT_UncLUT.
;		These tiles are drawn in palette line 3.
;	3nd segment:
;		Mappings data is a bitstream: the first bit in each cycle determines how
;		many bits from the stream are to be used as an index to the RLE-compressed
;		pattern name list SSPNT_RLELUT: if the first bit is set, 7 bits form an
;		index into SSPNT_RLELUT_Part2, otherwise 6 bits are used as an index into
;		SSPNT_RLELUT.
;		These tiles are drawn in palette line 3, with the high priority bit set.
; off_643E:
Map_SpecialStageTrack:
	dc.l MapSpec_Rise1		;   0
	dc.l MapSpec_Rise2		;   1
	dc.l MapSpec_Rise3		;   2
	dc.l MapSpec_Rise4		;   3
	dc.l MapSpec_Rise5		;   4
	dc.l MapSpec_Rise6		;   5
	dc.l MapSpec_Rise7		;   6
	dc.l MapSpec_Rise8		;   7
	dc.l MapSpec_Rise9		;   8
	dc.l MapSpec_Rise10		;   9
	dc.l MapSpec_Rise11		;  $A
	dc.l MapSpec_Rise12		;  $B
	dc.l MapSpec_Rise13		;  $C
	dc.l MapSpec_Rise14		;  $D	; This may flip the special stage's horizontal orientation
	dc.l MapSpec_Rise15		;  $E
	dc.l MapSpec_Rise16		;  $F
	dc.l MapSpec_Rise17		; $10
	dc.l MapSpec_Straight1		; $11
	dc.l MapSpec_Straight2		; $12	; This may flip the special stage's horizontal orientation
	dc.l MapSpec_Straight3		; $13
	dc.l MapSpec_Straight4		; $14
	dc.l MapSpec_Drop1		; $15
	dc.l MapSpec_Drop2		; $16
	dc.l MapSpec_Drop3		; $17
	dc.l MapSpec_Drop4		; $18
	dc.l MapSpec_Drop5		; $19
	dc.l MapSpec_Drop6		; $1A	; This may flip the special stage's horizontal orientation
	dc.l MapSpec_Drop7		; $1B
	dc.l MapSpec_Drop8		; $1C
	dc.l MapSpec_Drop9		; $1D
	dc.l MapSpec_Drop10		; $1E
	dc.l MapSpec_Drop11		; $1F
	dc.l MapSpec_Drop12		; $20
	dc.l MapSpec_Drop13		; $21
	dc.l MapSpec_Drop14		; $22
	dc.l MapSpec_Drop15		; $23
	dc.l MapSpec_Drop16		; $24
	dc.l MapSpec_Drop17		; $25
	dc.l MapSpec_Turning1		; $26
	dc.l MapSpec_Turning2		; $27
	dc.l MapSpec_Turning3		; $28
	dc.l MapSpec_Turning4		; $29
	dc.l MapSpec_Turning5		; $2A
	dc.l MapSpec_Turning6		; $2B
	dc.l MapSpec_Unturn1		; $2C
	dc.l MapSpec_Unturn2		; $2D
	dc.l MapSpec_Unturn3		; $2E
	dc.l MapSpec_Unturn4		; $2F
	dc.l MapSpec_Unturn5		; $30
	dc.l MapSpec_Turn1		; $31
	dc.l MapSpec_Turn2		; $32
	dc.l MapSpec_Turn3		; $33
	dc.l MapSpec_Turn4		; $34
	dc.l MapSpec_Turn5		; $35
	dc.l MapSpec_Turn6		; $36
	dc.l MapSpec_Turn7		; $37

; These are pattern names. They get sent to either the pattern name table
; buffer or one region of one of the plane A name tables in the special stage.
; They are indexed by the second segment of the mappings in Map_SpecialStageTrack, above.
;word_651E
SSPNT_UncLUT:
	dc.w make_block_tile($0001,0,0,0,1), make_block_tile($0007,0,0,0,1), make_block_tile($002C,0,0,0,1), make_block_tile($000B,0,0,0,1)	; $00
	dc.w make_block_tile($0024,0,0,0,1), make_block_tile($0024,1,0,0,1), make_block_tile($0039,0,0,0,1), make_block_tile($002B,1,0,0,1)	; $04
	dc.w make_block_tile($005D,0,0,0,1), make_block_tile($005D,1,0,0,1), make_block_tile($002B,0,0,0,1), make_block_tile($004A,0,0,0,1)	; $08
	dc.w make_block_tile($0049,0,0,0,1), make_block_tile($0037,0,0,0,1), make_block_tile($0049,1,0,0,1), make_block_tile($0045,0,0,0,1)	; $0C
	dc.w make_block_tile($0045,1,0,0,1), make_block_tile($003A,1,0,0,1), make_block_tile($0048,0,0,0,1), make_block_tile($0050,1,0,0,1)	; $10
	dc.w make_block_tile($0036,0,0,0,1), make_block_tile($0037,1,0,0,1), make_block_tile($003A,0,0,0,1), make_block_tile($0050,0,0,0,1)	; $14
	dc.w make_block_tile($0042,1,0,0,1), make_block_tile($0042,0,0,0,1), make_block_tile($0015,1,0,0,1), make_block_tile($001D,0,0,0,1)	; $18
	dc.w make_block_tile($004B,0,0,0,1), make_block_tile($0017,1,0,0,1), make_block_tile($0048,1,0,0,1), make_block_tile($0036,1,0,0,1)	; $1C
	dc.w make_block_tile($0038,0,0,0,1), make_block_tile($004B,1,0,0,1), make_block_tile($0015,0,0,0,1), make_block_tile($0021,0,0,0,1)	; $20
	dc.w make_block_tile($0017,0,0,0,1), make_block_tile($0033,0,0,0,1), make_block_tile($001A,0,0,0,1), make_block_tile($002A,0,0,0,1)	; $24
	dc.w make_block_tile($005E,0,0,0,1), make_block_tile($0028,0,0,0,1), make_block_tile($0030,0,0,0,1), make_block_tile($0021,1,0,0,1)	; $28
	dc.w make_block_tile($0038,1,0,0,1), make_block_tile($001A,1,0,0,1), make_block_tile($0025,0,0,0,1), make_block_tile($005E,1,0,0,1)	; $2C
	dc.w make_block_tile($0025,1,0,0,1), make_block_tile($0033,1,0,0,1), make_block_tile($0003,0,0,0,1), make_block_tile($0014,1,0,0,1)	; $30
	dc.w make_block_tile($0014,0,0,0,1), make_block_tile($0004,0,0,0,1), make_block_tile($004E,0,0,0,1), make_block_tile($0003,1,0,0,1)	; $34
	dc.w make_block_tile($000C,0,0,0,1), make_block_tile($002A,1,0,0,1), make_block_tile($0002,0,0,0,1), make_block_tile($0051,0,0,0,1)	; $38
	dc.w make_block_tile($0040,0,0,0,1), make_block_tile($003D,0,0,0,1), make_block_tile($0019,0,0,0,1), make_block_tile($0052,0,0,0,1)	; $3C
;word_659E
SSPNT_UncLUT_Part2:
	dc.w make_block_tile($0009,0,0,0,1), make_block_tile($005A,0,0,0,1), make_block_tile($0030,1,0,0,1), make_block_tile($004E,1,0,0,1)	; $40
	dc.w make_block_tile($0052,1,0,0,1), make_block_tile($0051,1,0,0,1), make_block_tile($0009,1,0,0,1), make_block_tile($0040,1,0,0,1)	; $44
	dc.w make_block_tile($002F,0,0,0,1), make_block_tile($005A,1,0,0,1), make_block_tile($0018,1,0,0,1), make_block_tile($0034,0,0,0,1)	; $48
	dc.w make_block_tile($0019,1,0,0,1), make_block_tile($002F,1,0,0,1), make_block_tile($003D,1,0,0,1), make_block_tile($003E,0,0,0,1)	; $4C
	dc.w make_block_tile($0018,0,0,0,1), make_block_tile($000C,1,0,0,1), make_block_tile($0012,0,0,0,1), make_block_tile($0004,1,0,0,1)	; $50
	dc.w make_block_tile($0026,0,0,0,1), make_block_tile($0034,1,0,0,1), make_block_tile($0005,1,0,0,1), make_block_tile($003B,0,0,0,1)	; $54
	dc.w make_block_tile($003E,1,0,0,1), make_block_tile($003B,1,0,0,1), make_block_tile($0000,0,0,0,1), make_block_tile($0002,1,0,0,1)	; $58
	dc.w make_block_tile($0005,0,0,0,1), make_block_tile($000D,0,0,0,1), make_block_tile($0055,0,0,0,1), make_block_tile($00AF,0,0,0,1)	; $5C
	dc.w make_block_tile($001C,0,0,0,1), make_block_tile($001B,0,0,0,1), make_block_tile($000D,1,0,0,1), make_block_tile($0016,0,0,0,1)	; $60
	dc.w make_block_tile($0012,1,0,0,1), make_block_tile($001F,0,0,0,1), make_block_tile($0032,1,0,0,1), make_block_tile($0013,0,0,0,1)	; $64
	dc.w make_block_tile($0092,0,0,0,1), make_block_tile($0026,1,0,0,1), make_block_tile($0010,0,0,0,1), make_block_tile($004D,0,0,0,1)	; $68
	dc.w make_block_tile($0047,0,0,0,1), make_block_tile($0092,1,0,0,1), make_block_tile($0000,1,0,0,1), make_block_tile($0062,0,0,0,1)	; $6C
	dc.w make_block_tile($0066,0,0,0,1), make_block_tile($0090,0,0,0,1), make_block_tile($0008,0,0,0,1), make_block_tile($007C,1,0,0,1)	; $70
	dc.w make_block_tile($0067,1,0,0,1), make_block_tile($00F7,1,0,0,1), make_block_tile($000E,0,0,0,1), make_block_tile($0060,0,0,0,1)	; $74
	dc.w make_block_tile($0032,0,0,0,1), make_block_tile($0094,0,0,0,1), make_block_tile($001C,1,0,0,1), make_block_tile($0105,1,0,0,1)	; $78
	dc.w make_block_tile($00B0,1,0,0,1), make_block_tile($0059,0,0,0,1), make_block_tile($000F,0,0,0,1), make_block_tile($0067,0,0,0,1)	; $7C
	dc.w make_block_tile($0068,0,0,0,1), make_block_tile($0094,1,0,0,1), make_block_tile($007C,0,0,0,1), make_block_tile($00B0,0,0,0,1)	; $80
	dc.w make_block_tile($00B1,0,0,0,1), make_block_tile($0006,0,0,0,1), make_block_tile($0041,1,0,0,1), make_block_tile($0087,0,0,0,1)	; $84
	dc.w make_block_tile($0093,0,0,0,1), make_block_tile($00CC,0,0,0,1), make_block_tile($001F,1,0,0,1), make_block_tile($0068,1,0,0,1)	; $88
	dc.w make_block_tile($0041,0,0,0,1), make_block_tile($008F,0,0,0,1), make_block_tile($0090,1,0,0,1), make_block_tile($00C2,0,0,0,1)	; $8C
	dc.w make_block_tile($0013,1,0,0,1), make_block_tile($00C2,1,0,0,1), make_block_tile($005C,0,0,0,1), make_block_tile($0064,0,0,0,1)	; $90
	dc.w make_block_tile($00D8,0,0,0,1), make_block_tile($001B,1,0,0,1), make_block_tile($00CC,1,0,0,1), make_block_tile($0011,1,0,0,1)	; $94
	dc.w make_block_tile($0055,1,0,0,1), make_block_tile($00E2,1,0,0,1), make_block_tile($00F3,1,0,0,1), make_block_tile($0044,0,0,0,1)	; $98
	dc.w make_block_tile($00D8,1,0,0,1), make_block_tile($0085,0,0,0,1), make_block_tile($00A1,0,0,0,1), make_block_tile($00C1,0,0,0,1)	; $9C
	dc.w make_block_tile($0119,0,0,0,1), make_block_tile($0089,1,0,0,1), make_block_tile($000A,1,0,0,1), make_block_tile($0022,1,0,0,1)	; $A0
	dc.w make_block_tile($003F,0,0,0,1), make_block_tile($005B,0,0,0,1), make_block_tile($007F,0,0,0,1), make_block_tile($0086,1,0,0,1)	; $A4
	dc.w make_block_tile($0008,1,0,0,1), make_block_tile($0080,0,0,0,1), make_block_tile($0066,1,0,0,1), make_block_tile($00E0,1,0,0,1)	; $A8
	dc.w make_block_tile($00C1,1,0,0,1), make_block_tile($0020,0,0,0,1), make_block_tile($0022,0,0,0,1), make_block_tile($0054,0,0,0,1)	; $AC
	dc.w make_block_tile($00D2,0,0,0,1), make_block_tile($0059,1,0,0,1), make_block_tile($00B1,1,0,0,1), make_block_tile($0060,1,0,0,1)	; $B0
	dc.w make_block_tile($0119,1,0,0,1), make_block_tile($00A4,1,0,0,1), make_block_tile($008F,1,0,0,1), make_block_tile($000A,0,0,0,1)	; $B4
	dc.w make_block_tile($0061,0,0,0,1), make_block_tile($0075,0,0,0,1), make_block_tile($0095,0,0,0,1), make_block_tile($00B6,0,0,0,1)	; $B8
	dc.w make_block_tile($00E0,0,0,0,1), make_block_tile($0010,1,0,0,1), make_block_tile($0098,1,0,0,1), make_block_tile($005B,1,0,0,1)	; $BC
	dc.w make_block_tile($00D2,1,0,0,1), make_block_tile($0016,1,0,0,1), make_block_tile($0053,0,0,0,1), make_block_tile($0091,0,0,0,1)	; $C0
	dc.w make_block_tile($0096,0,0,0,1), make_block_tile($00A4,0,0,0,1), make_block_tile($00DD,0,0,0,1), make_block_tile($00E6,0,0,0,1)	; $C4
	dc.w make_block_tile($007A,1,0,0,1), make_block_tile($004D,1,0,0,1), make_block_tile($00E6,1,0,0,1), make_block_tile($0011,0,0,0,1)	; $C8
	dc.w make_block_tile($0057,0,0,0,1), make_block_tile($007A,0,0,0,1), make_block_tile($0086,0,0,0,1), make_block_tile($009E,0,0,0,1)	; $CC
	dc.w make_block_tile($00DA,0,0,0,1), make_block_tile($0058,0,0,0,1), make_block_tile($00DC,0,0,0,1), make_block_tile($00E3,0,0,0,1)	; $D0
	dc.w make_block_tile($0063,1,0,0,1), make_block_tile($003C,0,0,0,1), make_block_tile($0056,0,0,0,1), make_block_tile($0069,0,0,0,1)	; $D4
	dc.w make_block_tile($007E,0,0,0,1), make_block_tile($00AE,0,0,0,1), make_block_tile($00B5,0,0,0,1), make_block_tile($00B8,0,0,0,1)	; $D8
	dc.w make_block_tile($00CD,0,0,0,1), make_block_tile($00FB,0,0,0,1), make_block_tile($00FF,0,0,0,1), make_block_tile($005C,1,0,0,1)	; $DC
	dc.w make_block_tile($00CD,1,0,0,1), make_block_tile($0074,1,0,0,1), make_block_tile($00EA,1,0,0,1), make_block_tile($00FF,1,0,0,1)	; $E0
	dc.w make_block_tile($00B5,1,0,0,1), make_block_tile($0043,0,0,0,1), make_block_tile($006C,0,0,0,1), make_block_tile($0074,0,0,0,1)	; $E4
	dc.w make_block_tile($0077,0,0,0,1), make_block_tile($0089,0,0,0,1), make_block_tile($0097,0,0,0,1), make_block_tile($009F,0,0,0,1)	; $E8
	dc.w make_block_tile($00A0,0,0,0,1), make_block_tile($0113,0,0,0,1), make_block_tile($011B,0,0,0,1), make_block_tile($0078,1,0,0,1)	; $EC
	dc.w make_block_tile($000F,1,0,0,1), make_block_tile($00E1,1,0,0,1), make_block_tile($00FB,1,0,0,1), make_block_tile($0128,1,0,0,1)	; $F0
	dc.w make_block_tile($0063,0,0,0,1), make_block_tile($0084,0,0,0,1), make_block_tile($008D,0,0,0,1), make_block_tile($00CB,0,0,0,1)	; $F4
	dc.w make_block_tile($00D7,0,0,0,1), make_block_tile($00E9,0,0,0,1), make_block_tile($0128,0,0,0,1), make_block_tile($0138,0,0,0,1)	; $F8
	dc.w make_block_tile($00AE,1,0,0,1), make_block_tile($00EC,1,0,0,1), make_block_tile($0031,0,0,0,1), make_block_tile($004C,0,0,0,1)	; $FC
	dc.w make_block_tile($00E2,0,0,0,1), make_block_tile($00EA,0,0,0,1), make_block_tile($0064,1,0,0,1), make_block_tile($0029,0,0,0,1)	; $100
	dc.w make_block_tile($002D,0,0,0,1), make_block_tile($006D,0,0,0,1), make_block_tile($0078,0,0,0,1), make_block_tile($0088,0,0,0,1)	; $104
	dc.w make_block_tile($00B4,0,0,0,1), make_block_tile($00BE,0,0,0,1), make_block_tile($00CF,0,0,0,1), make_block_tile($00E1,0,0,0,1)	; $108
	dc.w make_block_tile($00E4,0,0,0,1), make_block_tile($0054,1,0,0,1), make_block_tile($00D6,1,0,0,1), make_block_tile($00D7,1,0,0,1)	; $10C
	dc.w make_block_tile($0061,1,0,0,1), make_block_tile($012B,1,0,0,1), make_block_tile($0047,1,0,0,1), make_block_tile($0035,0,0,0,1)	; $110
	dc.w make_block_tile($006A,0,0,0,1), make_block_tile($0072,0,0,0,1), make_block_tile($0073,0,0,0,1), make_block_tile($0098,0,0,0,1)	; $114
	dc.w make_block_tile($00D5,0,0,0,1), make_block_tile($00D6,0,0,0,1), make_block_tile($0116,0,0,0,1), make_block_tile($011E,0,0,0,1)	; $118
	dc.w make_block_tile($0126,0,0,0,1), make_block_tile($0127,0,0,0,1), make_block_tile($012F,0,0,0,1), make_block_tile($015D,0,0,0,1)	; $11C
	dc.w make_block_tile($0069,1,0,0,1), make_block_tile($0088,1,0,0,1), make_block_tile($0075,1,0,0,1), make_block_tile($0097,1,0,0,1)	; $120
	dc.w make_block_tile($00B4,1,0,0,1), make_block_tile($00D1,1,0,0,1), make_block_tile($00D4,1,0,0,1), make_block_tile($00D5,1,0,0,1)	; $124
	dc.w make_block_tile($00CB,1,0,0,1), make_block_tile($00E4,1,0,0,1), make_block_tile($0091,1,0,0,1), make_block_tile($0062,1,0,0,1)	; $128
	dc.w make_block_tile($0006,1,0,0,1), make_block_tile($00B8,1,0,0,1), make_block_tile($0065,0,0,0,1), make_block_tile($006E,0,0,0,1)	; $12C
	dc.w make_block_tile($0071,0,0,0,1), make_block_tile($007D,0,0,0,1), make_block_tile($00D1,0,0,0,1), make_block_tile($00E7,0,0,0,1)	; $130
	dc.w make_block_tile($00F9,0,0,0,1), make_block_tile($0108,0,0,0,1), make_block_tile($012E,0,0,0,1), make_block_tile($014B,0,0,0,1)	; $134
	dc.w make_block_tile($0081,1,0,0,1), make_block_tile($0085,1,0,0,1), make_block_tile($0077,1,0,0,1), make_block_tile($007E,1,0,0,1)	; $138
	dc.w make_block_tile($0095,1,0,0,1), make_block_tile($00DF,1,0,0,1), make_block_tile($0087,1,0,0,1), make_block_tile($006C,1,0,0,1)	; $13C
	dc.w make_block_tile($00F5,1,0,0,1), make_block_tile($0108,1,0,0,1), make_block_tile($0079,1,0,0,1), make_block_tile($006D,1,0,0,1)	; $140
	dc.w make_block_tile($012A,1,0,0,1), make_block_tile($00AA,1,0,0,1), make_block_tile($001E,0,0,0,1), make_block_tile($0027,0,0,0,1)	; $144
	dc.w make_block_tile($0046,0,0,0,1), make_block_tile($005F,0,0,0,1), make_block_tile($0070,0,0,0,1), make_block_tile($0079,0,0,0,1)	; $148
	dc.w make_block_tile($009A,0,0,0,1), make_block_tile($00AA,0,0,0,1), make_block_tile($00C3,0,0,0,1), make_block_tile($00D3,0,0,0,1)	; $14C
	dc.w make_block_tile($00D4,0,0,0,1), make_block_tile($00DE,0,0,0,1), make_block_tile($00DF,0,0,0,1), make_block_tile($00F8,0,0,0,1)	; $150
	dc.w make_block_tile($0100,0,0,0,1), make_block_tile($0101,0,0,0,1), make_block_tile($012B,0,0,0,1), make_block_tile($0133,0,0,0,1)	; $154
	dc.w make_block_tile($0136,0,0,0,1), make_block_tile($0143,0,0,0,1), make_block_tile($0151,0,0,0,1), make_block_tile($002E,1,0,0,1)	; $158
	dc.w make_block_tile($009E,1,0,0,1), make_block_tile($0099,1,0,0,1), make_block_tile($00D3,1,0,0,1), make_block_tile($00DD,1,0,0,1)	; $15C
	dc.w make_block_tile($00DE,1,0,0,1), make_block_tile($00E9,1,0,0,1), make_block_tile($00EF,1,0,0,1), make_block_tile($00F0,1,0,0,1)	; $160
	dc.w make_block_tile($00F8,1,0,0,1), make_block_tile($0127,1,0,0,1), make_block_tile($00BE,1,0,0,1), make_block_tile($0096,1,0,0,1)	; $164
	dc.w make_block_tile($004F,0,0,0,1), make_block_tile($006F,0,0,0,1), make_block_tile($0081,0,0,0,1), make_block_tile($008B,0,0,0,1)	; $168
	dc.w make_block_tile($008E,0,0,0,1), make_block_tile($009C,0,0,0,1), make_block_tile($00A3,0,0,0,1), make_block_tile($00B3,0,0,0,1)	; $16C
	dc.w make_block_tile($00C0,0,0,0,1), make_block_tile($00CE,0,0,0,1), make_block_tile($00F0,0,0,0,1), make_block_tile($00F1,0,0,0,1)	; $170
	dc.w make_block_tile($00F5,0,0,0,1), make_block_tile($00F7,0,0,0,1), make_block_tile($0102,0,0,0,1), make_block_tile($0104,0,0,0,1)	; $174
	dc.w make_block_tile($0105,0,0,0,1), make_block_tile($0109,0,0,0,1), make_block_tile($010C,0,0,0,1), make_block_tile($0114,0,0,0,1)	; $178
	dc.w make_block_tile($0118,0,0,0,1), make_block_tile($0120,0,0,0,1), make_block_tile($0124,0,0,0,1), make_block_tile($0125,0,0,0,1)	; $17C
	dc.w make_block_tile($012A,0,0,0,1), make_block_tile($0130,0,0,0,1), make_block_tile($0132,0,0,0,1), make_block_tile($0137,0,0,0,1)	; $180
	dc.w make_block_tile($0159,0,0,0,1), make_block_tile($0165,0,0,0,1), make_block_tile($003F,1,0,0,1), make_block_tile($006B,1,0,0,1)	; $184
	dc.w make_block_tile($0080,1,0,0,1), make_block_tile($0053,1,0,0,1), make_block_tile($00C6,1,0,0,1), make_block_tile($00CF,1,0,0,1)	; $188
	dc.w make_block_tile($00D9,1,0,0,1), make_block_tile($00DC,1,0,0,1), make_block_tile($0056,1,0,0,1), make_block_tile($00B6,1,0,0,1)	; $18C
	dc.w make_block_tile($00F9,1,0,0,1), make_block_tile($0102,1,0,0,1), make_block_tile($0104,1,0,0,1), make_block_tile($0115,1,0,0,1)	; $190
	dc.w make_block_tile($006A,1,0,0,1), make_block_tile($0113,1,0,0,1), make_block_tile($0072,1,0,0,1), make_block_tile($0035,1,0,0,1)	; $194
	dc.w make_block_tile($0138,1,0,0,1), make_block_tile($015D,1,0,0,1), make_block_tile($0143,1,0,0,1), make_block_tile($0023,0,0,0,1)	; $198
	dc.w make_block_tile($0076,0,0,0,1), make_block_tile($007B,0,0,0,1), make_block_tile($008A,0,0,0,1), make_block_tile($009D,0,0,0,1)	; $19C
	dc.w make_block_tile($00A6,0,0,0,1), make_block_tile($00A8,0,0,0,1), make_block_tile($00AC,0,0,0,1), make_block_tile($00B2,0,0,0,1)	; $1A0
	dc.w make_block_tile($00B7,0,0,0,1), make_block_tile($00BB,0,0,0,1), make_block_tile($00BC,0,0,0,1), make_block_tile($00BD,0,0,0,1)	; $1A4
	dc.w make_block_tile($00C6,0,0,0,1), make_block_tile($00E5,0,0,0,1), make_block_tile($00E8,0,0,0,1), make_block_tile($00EE,0,0,0,1)	; $1A8
	dc.w make_block_tile($00F4,0,0,0,1), make_block_tile($010A,0,0,0,1), make_block_tile($010D,0,0,0,1), make_block_tile($0111,0,0,0,1)	; $1AC
	dc.w make_block_tile($0115,0,0,0,1), make_block_tile($011A,0,0,0,1), make_block_tile($011F,0,0,0,1), make_block_tile($0122,0,0,0,1)	; $1B0
	dc.w make_block_tile($0123,0,0,0,1), make_block_tile($0139,0,0,0,1), make_block_tile($013A,0,0,0,1), make_block_tile($013C,0,0,0,1)	; $1B4
	dc.w make_block_tile($0142,0,0,0,1), make_block_tile($0144,0,0,0,1), make_block_tile($0147,0,0,0,1), make_block_tile($0148,0,0,0,1)	; $1B8
	dc.w make_block_tile($015E,0,0,0,1), make_block_tile($015F,0,0,0,1), make_block_tile($0163,0,0,0,1), make_block_tile($0168,0,0,0,1)	; $1BC
	dc.w make_block_tile($016A,0,0,0,1), make_block_tile($016C,0,0,0,1), make_block_tile($0170,0,0,0,1), make_block_tile($00E5,1,0,0,1)	; $1C0
	dc.w make_block_tile($00CE,1,0,0,1), make_block_tile($00EE,1,0,0,1), make_block_tile($00F1,1,0,0,1), make_block_tile($0084,1,0,0,1)	; $1C4
	dc.w make_block_tile($00FD,1,0,0,1), make_block_tile($0100,1,0,0,1), make_block_tile($00B9,1,0,0,1), make_block_tile($0117,1,0,0,1)	; $1C8
	dc.w make_block_tile($0071,1,0,0,1), make_block_tile($0109,1,0,0,1), make_block_tile($010D,1,0,0,1), make_block_tile($0065,1,0,0,1)	; $1CC
	dc.w make_block_tile($0125,1,0,0,1), make_block_tile($0122,1,0,0,1), make_block_tile($0031,1,0,0,1), make_block_tile($003C,1,0,0,1)	; $1D0
	dc.w make_block_tile($010F,1,0,0,1), make_block_tile($00C5,1,0,0,1), make_block_tile($0133,1,0,0,1), make_block_tile($0137,1,0,0,1)	; $1D4
	dc.w make_block_tile($011F,1,0,0,1), make_block_tile($002E,0,0,0,1), make_block_tile($006B,0,0,0,1), make_block_tile($0082,0,0,0,1)	; $1D8
	dc.w make_block_tile($0083,0,0,0,1), make_block_tile($008C,0,0,0,1), make_block_tile($0099,0,0,0,1), make_block_tile($009B,0,0,0,1)	; $1DC
	dc.w make_block_tile($00A2,0,0,0,1), make_block_tile($00A5,0,0,0,1), make_block_tile($00A7,0,0,0,1), make_block_tile($00A9,0,0,0,1)	; $1E0
	dc.w make_block_tile($00AB,0,0,0,1), make_block_tile($00AD,0,0,0,1), make_block_tile($00B9,0,0,0,1), make_block_tile($00BA,0,0,0,1)	; $1E4
	dc.w make_block_tile($00BF,0,0,0,1), make_block_tile($00C4,0,0,0,1), make_block_tile($00C5,0,0,0,1), make_block_tile($00C7,0,0,0,1)	; $1E8
	dc.w make_block_tile($00C8,0,0,0,1), make_block_tile($00C9,0,0,0,1), make_block_tile($00CA,0,0,0,1), make_block_tile($00D0,0,0,0,1)	; $1EC
	dc.w make_block_tile($00D9,0,0,0,1), make_block_tile($00DB,0,0,0,1), make_block_tile($00EB,0,0,0,1), make_block_tile($00EC,0,0,0,1)	; $1F0
	dc.w make_block_tile($00ED,0,0,0,1), make_block_tile($00EF,0,0,0,1), make_block_tile($00F2,0,0,0,1), make_block_tile($00F3,0,0,0,1)	; $1F4
	dc.w make_block_tile($00F6,0,0,0,1), make_block_tile($00FA,0,0,0,1), make_block_tile($00FC,0,0,0,1), make_block_tile($00FD,0,0,0,1)	; $1F8
	dc.w make_block_tile($00FE,0,0,0,1), make_block_tile($0103,0,0,0,1), make_block_tile($0106,0,0,0,1), make_block_tile($0107,0,0,0,1)	; $2FC
	dc.w make_block_tile($010B,0,0,0,1), make_block_tile($010E,0,0,0,1), make_block_tile($010F,0,0,0,1), make_block_tile($0110,0,0,0,1)	; $200
	dc.w make_block_tile($0112,0,0,0,1), make_block_tile($0117,0,0,0,1), make_block_tile($011C,0,0,0,1), make_block_tile($011D,0,0,0,1)	; $204
	dc.w make_block_tile($0121,0,0,0,1), make_block_tile($0129,0,0,0,1), make_block_tile($012C,0,0,0,1), make_block_tile($012D,0,0,0,1)	; $208
	dc.w make_block_tile($0131,0,0,0,1), make_block_tile($0134,0,0,0,1), make_block_tile($0135,0,0,0,1), make_block_tile($013B,0,0,0,1)	; $20C
	dc.w make_block_tile($013D,0,0,0,1), make_block_tile($013E,0,0,0,1), make_block_tile($013F,0,0,0,1), make_block_tile($0140,0,0,0,1)	; $210
	dc.w make_block_tile($0141,0,0,0,1), make_block_tile($0145,0,0,0,1), make_block_tile($0146,0,0,0,1), make_block_tile($0149,0,0,0,1)	; $214
	dc.w make_block_tile($014A,0,0,0,1), make_block_tile($014C,0,0,0,1), make_block_tile($014D,0,0,0,1), make_block_tile($014E,0,0,0,1)	; $218
	dc.w make_block_tile($014F,0,0,0,1), make_block_tile($0150,0,0,0,1), make_block_tile($0152,0,0,0,1), make_block_tile($0153,0,0,0,1)	; $21C
	dc.w make_block_tile($0154,0,0,0,1), make_block_tile($0155,0,0,0,1), make_block_tile($0156,0,0,0,1), make_block_tile($0157,0,0,0,1)	; $220
	dc.w make_block_tile($0158,0,0,0,1), make_block_tile($015A,0,0,0,1), make_block_tile($015B,0,0,0,1), make_block_tile($015C,0,0,0,1)	; $224
	dc.w make_block_tile($0160,0,0,0,1), make_block_tile($0161,0,0,0,1), make_block_tile($0162,0,0,0,1), make_block_tile($0164,0,0,0,1)	; $228
	dc.w make_block_tile($0166,0,0,0,1), make_block_tile($0167,0,0,0,1), make_block_tile($0169,0,0,0,1), make_block_tile($016B,0,0,0,1)	; $22C
	dc.w make_block_tile($016D,0,0,0,1), make_block_tile($016E,0,0,0,1), make_block_tile($016F,0,0,0,1), make_block_tile($0171,0,0,0,1)	; $230
	dc.w make_block_tile($0172,0,0,0,1), make_block_tile($0173,0,0,0,1), make_block_tile($006E,1,0,0,1), make_block_tile($007D,1,0,0,1)	; $234
	dc.w make_block_tile($00C3,1,0,0,1), make_block_tile($00DB,1,0,0,1), make_block_tile($00E7,1,0,0,1), make_block_tile($00E8,1,0,0,1)	; $238
	dc.w make_block_tile($00EB,1,0,0,1), make_block_tile($00ED,1,0,0,1), make_block_tile($00F2,1,0,0,1), make_block_tile($00F6,1,0,0,1)	; $23C
	dc.w make_block_tile($00FA,1,0,0,1), make_block_tile($00FC,1,0,0,1), make_block_tile($00FE,1,0,0,1), make_block_tile($002D,1,0,0,1)	; $240
	dc.w make_block_tile($0103,1,0,0,1), make_block_tile($0106,1,0,0,1), make_block_tile($0107,1,0,0,1), make_block_tile($010B,1,0,0,1)	; $244
	dc.w make_block_tile($0073,1,0,0,1), make_block_tile($009A,1,0,0,1), make_block_tile($0129,1,0,0,1), make_block_tile($012C,1,0,0,1)	; $248
	dc.w make_block_tile($012D,1,0,0,1), make_block_tile($0111,1,0,0,1), make_block_tile($013C,1,0,0,1), make_block_tile($0120,1,0,0,1)	; $24C
	dc.w make_block_tile($0146,1,0,0,1), make_block_tile($00A9,1,0,0,1), make_block_tile($009C,1,0,0,1), make_block_tile($0116,1,0,0,1)	; $250
	dc.w make_block_tile($014F,1,0,0,1), make_block_tile($014C,1,0,0,1), make_block_tile($006F,1,0,0,1), make_block_tile($0158,1,0,0,1)	; $254
	dc.w make_block_tile($0156,1,0,0,1), make_block_tile($0159,1,0,0,1), make_block_tile($015A,1,0,0,1), make_block_tile($0161,1,0,0,1)	; $258
	dc.w make_block_tile($007B,1,0,0,1), make_block_tile($0166,1,0,0,1), make_block_tile($011C,1,0,0,1), make_block_tile($0118,1,0,0,1)	; $25C
	dc.w make_block_tile($00A0,1,0,0,1), make_block_tile($00A3,1,0,0,1), make_block_tile($0167,1,0,0,1), make_block_tile($00A1,1,0,0,1)	; $260

; These are run-length encoded pattern names. They get sent to either the
; pattern name table buffer or one region of one of the plane A name tables
; in the special stage.
; They are indexed by the third segment of the mappings in Map_SpecialStageTrack, above.
; Format: PNT,count
;word_69E6
SSPNT_RLELUT:
	dc.w	make_block_tile($0007,0,0,0,0),$0001,	make_block_tile($0001,0,0,0,0),$0001	; $00
	dc.w	make_block_tile($004A,0,0,0,0),$0001,	make_block_tile($0039,0,0,0,0),$0003	; $02
	dc.w	make_block_tile($0001,0,0,0,0),$0005,	make_block_tile($0028,0,0,0,0),$0007	; $04
	dc.w	make_block_tile($002C,0,0,0,0),$0001,	make_block_tile($0001,0,0,0,0),$0002	; $06
	dc.w	make_block_tile($0028,0,0,0,0),$0005,	make_block_tile($0039,0,0,0,0),$0001	; $08
	dc.w	make_block_tile($0028,0,0,0,0),$0009,	make_block_tile($0001,0,0,0,0),$0004	; $0A
	dc.w	make_block_tile($0028,0,0,0,0),$0006,	make_block_tile($0028,0,0,0,0),$0003	; $0C
	dc.w	make_block_tile($004A,0,0,0,0),$0002,	make_block_tile($0001,0,0,0,0),$0003	; $0E
	dc.w	make_block_tile($0028,0,0,0,0),$0004,	make_block_tile($0039,0,0,0,0),$0002	; $10
	dc.w	make_block_tile($0039,0,0,0,0),$0004,	make_block_tile($0001,0,0,0,0),$0006	; $12
	dc.w	make_block_tile($0007,0,0,0,0),$0002,	make_block_tile($002C,0,0,0,0),$0002	; $14
	dc.w	make_block_tile($0028,0,0,0,0),$0001,	make_block_tile($001D,0,0,0,0),$0001	; $16
	dc.w	make_block_tile($0028,0,0,0,0),$0008,	make_block_tile($0028,0,0,0,0),$0002	; $18
	dc.w	make_block_tile($0007,0,0,0,0),$0003,	make_block_tile($0001,0,0,0,0),$0007	; $1A
	dc.w	make_block_tile($0028,0,0,0,0),$000B,	make_block_tile($0039,0,0,0,0),$0005	; $1C
	dc.w	make_block_tile($001D,0,0,0,0),$0003,	make_block_tile($001D,0,0,0,0),$0004	; $1E
	dc.w	make_block_tile($001D,0,0,0,0),$0002,	make_block_tile($001D,0,0,0,0),$0005	; $20
	dc.w	make_block_tile($0028,0,0,0,0),$000D,	make_block_tile($000B,0,0,0,0),$0001	; $22
	dc.w	make_block_tile($0028,0,0,0,0),$000A,	make_block_tile($0039,0,0,0,0),$0006	; $24
	dc.w	make_block_tile($0039,0,0,0,0),$0007,	make_block_tile($002C,0,0,0,0),$0003	; $26
	dc.w	make_block_tile($001D,0,0,0,0),$0009,	make_block_tile($004A,0,0,0,0),$0003	; $28
	dc.w	make_block_tile($001D,0,0,0,0),$0007,	make_block_tile($0028,0,0,0,0),$000F	; $2A
	dc.w	make_block_tile($001D,0,0,0,0),$000B,	make_block_tile($001D,0,0,0,0),$0011	; $2C
	dc.w	make_block_tile($001D,0,0,0,0),$000D,	make_block_tile($001D,0,0,0,0),$0008	; $2E
	dc.w	make_block_tile($0028,0,0,0,0),$0011,	make_block_tile($001D,0,0,0,0),$0006	; $30
	dc.w	make_block_tile($000B,0,0,0,0),$0002,	make_block_tile($001D,0,0,0,0),$0015	; $32
	dc.w	make_block_tile($0028,0,0,0,0),$000C,	make_block_tile($001D,0,0,0,0),$000A	; $34
	dc.w	make_block_tile($0028,0,0,0,0),$000E,	make_block_tile($0001,0,0,0,0),$0008	; $36
	dc.w	make_block_tile($001D,0,0,0,0),$000F,	make_block_tile($0028,0,0,0,0),$0010	; $38
	dc.w	make_block_tile($0007,0,0,0,0),$0006,	make_block_tile($001D,0,0,0,0),$0013	; $3A
	dc.w	make_block_tile($004A,0,0,0,0),$0004,	make_block_tile($001D,0,0,0,0),$0017	; $3C
	dc.w	make_block_tile($0007,0,0,0,0),$0004,	make_block_tile($000B,0,0,0,0),$0003	; $3E
;word_6AE6
SSPNT_RLELUT_Part2:
	dc.w	make_block_tile($001D,0,0,0,0),$001B,	make_block_tile($004A,0,0,0,0),$0006	; $40
	dc.w	make_block_tile($001D,0,0,0,0),$001D,	make_block_tile($004A,0,0,0,0),$0005	; $42
	dc.w	make_block_tile($0001,0,0,0,0),$0009,	make_block_tile($0007,0,0,0,0),$0005	; $44
	dc.w	make_block_tile($001D,0,0,0,0),$001E,	make_block_tile($001D,0,0,0,0),$0019	; $46
	dc.w	make_block_tile($0001,0,0,0,0),$0011,	make_block_tile($001D,0,0,0,0),$000C	; $48
	dc.w	make_block_tile($001D,0,0,0,0),$007F,	make_block_tile($002C,0,0,0,0),$0004	; $4A
	dc.w	make_block_tile($001D,0,0,0,0),$000E,	make_block_tile($001D,0,0,0,0),$001C	; $4C
	dc.w	make_block_tile($004A,0,0,0,0),$000A,	make_block_tile($001D,0,0,0,0),$001A	; $4E
	dc.w	make_block_tile($004A,0,0,0,0),$0007,	make_block_tile($001D,0,0,0,0),$0018	; $50
	dc.w	make_block_tile($000B,0,0,0,0),$0004,	make_block_tile($001D,0,0,0,0),$0012	; $52
	dc.w	make_block_tile($001D,0,0,0,0),$0010,	make_block_tile($0001,0,0,0,0),$000F	; $54
	dc.w	make_block_tile($000B,0,0,0,0),$0005,	make_block_tile($0001,0,0,0,0),$000D	; $56
	dc.w	make_block_tile($0001,0,0,0,0),$0013,	make_block_tile($004A,0,0,0,0),$0009	; $58
	dc.w	make_block_tile($004A,0,0,0,0),$000B,	make_block_tile($004A,0,0,0,0),$000C	; $5A
	dc.w	make_block_tile($002C,0,0,0,0),$0005,	make_block_tile($001D,0,0,0,0),$0014	; $5C
	dc.w	make_block_tile($000B,0,0,0,0),$0007,	make_block_tile($001D,0,0,0,0),$0016	; $5E
	dc.w	make_block_tile($0001,0,0,0,0),$000C,	make_block_tile($0001,0,0,0,0),$000E	; $60
	dc.w	make_block_tile($004A,0,0,0,0),$0008,	make_block_tile($001D,0,0,0,0),$005F	; $62
	dc.w	make_block_tile($0001,0,0,0,0),$000A,	make_block_tile($000B,0,0,0,0),$0006	; $64
	dc.w	make_block_tile($000B,0,0,0,0),$0008,	make_block_tile($000B,0,0,0,0),$000A	; $66
	dc.w	make_block_tile($0039,0,0,0,0),$0008,	make_block_tile($000B,0,0,0,0),$0009	; $68
	dc.w	make_block_tile($002C,0,0,0,0),$0006,	make_block_tile($0001,0,0,0,0),$0010	; $6A
	dc.w	make_block_tile($000B,0,0,0,0),$000C,	make_block_tile($0001,0,0,0,0),$000B	; $6C
	dc.w	make_block_tile($0001,0,0,0,0),$0012,	make_block_tile($0007,0,0,0,0),$0007	; $6E
	dc.w	make_block_tile($001D,0,0,0,0),$001F,	make_block_tile($0028,0,0,0,0),$0012	; $70
	dc.w	make_block_tile($000B,0,0,0,0),$000B,	make_block_tile($002C,0,0,0,0),$0007	; $72
	dc.w	make_block_tile($002C,0,0,0,0),$000B,	make_block_tile($001D,0,0,0,0),$0023	; $74
	dc.w	make_block_tile($0001,0,0,0,0),$0015,	make_block_tile($002C,0,0,0,0),$0008	; $76
	dc.w	make_block_tile($001D,0,0,0,0),$002E,	make_block_tile($001D,0,0,0,0),$003F	; $78
	dc.w	make_block_tile($0001,0,0,0,0),$0014,	make_block_tile($000B,0,0,0,0),$000D	; $7A
	dc.w	make_block_tile($002C,0,0,0,0),$0009,	make_block_tile($002C,0,0,0,0),$000A	; $7C
	dc.w	make_block_tile($001D,0,0,0,0),$0025,	make_block_tile($001D,0,0,0,0),$0055	; $7E
	dc.w	make_block_tile($001D,0,0,0,0),$0071,	make_block_tile($001D,0,0,0,0),$007C	; $80
	dc.w	make_block_tile($004A,0,0,0,0),$000D,	make_block_tile($002C,0,0,0,0),$000C	; $82
	dc.w	make_block_tile($002C,0,0,0,0),$000F,	make_block_tile($002C,0,0,0,0),$0010	; $84

;unknown
;byte_6BFE:
	dc.b $FF,$FB,$FF,$FB,$FF,$FA,$FF,$FA; 528
	dc.b $FF,$FA,$FF,$FA	; 544
	even
; ===========================================================================
; (!)
;loc_6C0A
SSTrackSetOrientation:
	move.b	(SS_Alternate_HorizScroll_Buf).w,(SS_Last_Alternate_HorizScroll_Buf).w
	moveq	#0,d1
	movea.l	(SSTrack_mappings_bitflags).w,a0				; Get frame mappings pointer
	cmpa.l	#MapSpec_Straight2,a0						; Is the track rising or one of the first straight frame?
	blt.s	+								; Branch if yes
	cmpa.l	#MapSpec_Straight3,a0						; Is it straight path frame 3 or higher?
	bge.s	+								; Branch if yes
	; We only get here for straight frame 2
	movea.l	(SS_CurrentLevelLayout).w,a5					; Get current level layout
	move.b	(SpecialStage_CurrentSegment).w,d1				; Get current segment
	move.b	(a5,d1.w),d1							; Get segment geometry
	bpl.s	+++								; Branch if not flipped
-
	st.b	(SSTrack_Orientation).w						; Mark as being flipped
	move.b	(SSTrack_drawing_index).w,d0					; Get drawing position
	cmp.b	(SS_player_anim_frame_timer).w,d0				; Is it lower than the player's frame?
	blt.w	return_6C9A							; Return if yes
	st.b	(SS_Alternate_HorizScroll_Buf).w				; Use the alternate horizontal scroll buffer
	rts
; ===========================================================================
+
	cmpa.l	#MapSpec_Rise14,a0						; Is the track one of the first 13 rising frames?
	blt.s	+								; Branch if yes
	cmpa.l	#MapSpec_Rise15,a0						; Is it rising frame 15 or higher?
	bge.s	+								; Branch if yes
	; We only get here for straight frame 14
	movea.l	(SS_CurrentLevelLayout).w,a5					; Get current level layout
	move.b	(SpecialStage_CurrentSegment).w,d1				; Get current segment
	move.b	(a5,d1.w),d1							; Get segment geometry
	bpl.s	++								; Branch if not flipped
	bra.s	-
; ===========================================================================
+
	cmpa.l	#MapSpec_Drop6,a0						; Is the track before drop frame 6?
	blt.s	return_6C9A							; Return is yes
	cmpa.l	#MapSpec_Drop7,a0						; Is it drop frame 7 or higher?
	bge.s	return_6C9A							; Return if yes
	; We only get here for straight frame 6
	movea.l	(SS_CurrentLevelLayout).w,a5					; Get current level layout
	move.b	(SpecialStage_CurrentSegment).w,d1				; Get current segment
	move.b	(a5,d1.w),d1							; Get segment geometry
	bmi.s	-								; Branch if flipped
+
	sf.b	(SSTrack_Orientation).w						; Mark as being unflipped
	move.b	(SSTrack_drawing_index).w,d0					; Get drawing position
	cmp.b	(SS_player_anim_frame_timer).w,d0				; Is it lower than the player's frame?
	blt.s	return_6C9A							; Return if yes
	sf.b	(SS_Alternate_HorizScroll_Buf).w				; Don't use the alternate horizontal scroll buffer

return_6C9A:
	rts
; End of function SSTrack_Draw


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Initialize the PNT and H scroll table buffers.

ssInitTableBuffers:
	lea	(SS_Horiz_Scroll_Buf_1).w,a1
	lea	(SS_Horiz_Scroll_Buf_2).w,a2
	moveq	#0,d0								; Scroll of 0 for PNTA and PNTB on lines 0 and 1 (normal) or lines 6 and 7 (flipped)
	moveq	#0,d1								; Scroll of 0 for PNTB on lines 2 and 3 (normal) or lines 4 and 5 (flipped)
	moveq	#0,d2								; Scroll of 0 for PNTB on lines 4 and 5 (normal) or lines 2 and 3 (flipped)
	moveq	#0,d3								; Scroll of 0 for PNTB on lines 6 and 7 (normal) or lines 0 and 1 (flipped)
	move.w	#-$100,d1							; Scroll of 3 screens for PNTA on lines 2 and 3 (normal) or lines 4 and 5 (flipped)
	move.w	#-$200,d2							; Scroll of 2 screens for PNTA on lines 4 and 5 (normal) or lines 2 and 3 (flipped)
	move.w	#-$300,d3							; Scroll of 1 screen for PNTA on lines 6 and 7 (normal) or lines 0 and 1 (flipped)
	swap	d1
	swap	d2
	swap	d3
	moveq	#bytesToXcnt(HorizontalScrollBuffer.len,4*8),d4

-	move.l	d0,(a1)+
	move.l	d0,(a1)+
	move.l	d1,(a1)+
	move.l	d1,(a1)+
	move.l	d2,(a1)+
	move.l	d2,(a1)+
	move.l	d3,(a1)+
	move.l	d3,(a1)+
	move.l	d3,(a2)+
	move.l	d3,(a2)+
	move.l	d2,(a2)+
	move.l	d2,(a2)+
	move.l	d1,(a2)+
	move.l	d1,(a2)+
	move.l	d0,(a2)+
	move.l	d0,(a2)+
	dbf	d4,-

	rts
; End of function ssInitTableBuffers


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Load compressed special stage data into RAM, or VRAM for the art.

ssLdComprsdData:
	lea	(ArtKos_Special).l,a0
	lea	(Chunk_Table).l,a1
	bsr.w	KosDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_VRAM_Start),VRAM,WRITE),(VDP_control_port).l
	lea	(VDP_data_port).l,a1
	movea.l	#Chunk_Table,a0
	move.w	(a0)+,d0
	subq.w	#1,d0

-   rept 7
	move.l	(a0),(a1)
    endm
	move.l	(a0)+,(a1)
	dbf	d0,-

	lea	(MiscKoz_SpecialPerspective).l,a0
	lea	(SSRAM_MiscKoz_SpecialPerspective).l,a1
	bsr.w	KosDec
	lea	(MiscNem_SpecialLevelLayout).l,a0
	lea	(SSRAM_MiscNem_SpecialLevelLayout).w,a4
	bsr.w	NemDecToRAM
	lea	(MiscKoz_SpecialObjectLocations).l,a0
	lea	(SSRAM_MiscKoz_SpecialObjectLocations).w,a1
	bsr.w	KosDec
	rts
; End of function ssLdComprsdData


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_6D52
SSPlaneB_Background:
	move	#$2700,sr

	movea.l	#Chunk_Table+planeLoc(32,0,0),a1
	lea	(MapEng_SpecialBackBottom).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialBack,0,0),d0
	bsr.w	EniDec

	movea.l	#Chunk_Table+planeLoc(32,0,16),a1
	lea	(MapEng_SpecialBack).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialBack,0,0),d0
	bsr.w	EniDec

.c := 0
    rept 128/32
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_SS_Plane_B_Name_Table + planeLoc(128,32*.c,0),VRAM,WRITE),d0
	moveq	#32-1,d1
	moveq	#32-1,d2
	jsrto	PlaneMapToVRAM_H80_SpecialStage, PlaneMapToVRAM_H80_SpecialStage
.c := .c+1
    endm

	move	#$2300,sr
	rts
; End of function SSPlaneB_Background


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_6DD4
SSDecompressPlayerArt:
	lea	(ArtNem_SpecialSonicAndTails).l,a0
	lea	(SSRAM_ArtNem_SpecialSonicAndTails & $FFFFFF).l,a4
	bra.w	NemDecToRAM
; End of function SSDecompressPlayerArt


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_6DE4
SS_ScrollBG:
	bsr.w	SSPlaneB_SetHorizOffset
	bsr.w	SSTrack_SetVscroll
	rts
; End of function SS_ScrollBG

; ===========================================================================
; special stage background vertical and horizontal scroll offsets
off_6DEE:	offsetTable
		offsetTableEntry.w byte_6E04	;  0
		offsetTableEntry.w byte_6E09	;  1
		offsetTableEntry.w byte_6E0E	;  2
		offsetTableEntry.w byte_6E13	;  3
		offsetTableEntry.w byte_6E18	;  4
		offsetTableEntry.w byte_6E1D	;  5
		offsetTableEntry.w byte_6E22	;  6
		offsetTableEntry.w byte_6E27	;  7
		offsetTableEntry.w byte_6E2C	;  8
		offsetTableEntry.w byte_6E31	;  9
		offsetTableEntry.w byte_6E36	; $A
byte_6E04:	dc.b   2,  2,  2,  2,  2
byte_6E09:	dc.b   4,  4,  5,  4,  5
byte_6E0E:	dc.b  $B, $B, $B, $B, $C
byte_6E13:	dc.b   0,  0,  1,  0,  0
byte_6E18:	dc.b   1,  1,  1,  1,  1
byte_6E1D:	dc.b   9,  9,  8,  9,  9
byte_6E22:	dc.b   9,  9,  9,  9, $A
byte_6E27:	dc.b   7,  7,  6,  7,  7
byte_6E2C:	dc.b   0,  1,  1,  1,  0
byte_6E31:	dc.b   4,  3,  3,  3,  4
byte_6E36:	dc.b   0,  0,$FF,  0,  0
	even

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_6E3C
SSPlaneB_SetHorizOffset:
	moveq	#0,d7
	moveq	#0,d6
	moveq	#0,d0
	move.b	(SSTrack_last_anim_frame).w,d2				; Get last track animation frame
	move.b	(SSTrack_anim).w,d0					; Get current track animation
	add.w	d0,d0							; Convert it to an index
	move.w	off_6E54(pc,d0.w),d0
	jmp	off_6E54(pc,d0.w)
; ===========================================================================
off_6E54:	offsetTable
		offsetTableEntry.w +	; 0		; Turn, then rise
		offsetTableEntry.w +	; 1		; Turn, then drop
		offsetTableEntry.w +	; 2		; Turn, then straight
		offsetTableEntry.w ++	; 3 ; rts	; Straight
		offsetTableEntry.w ++	; 4 ; rts	; Straight, then turn
; ===========================================================================
+
	moveq	#0,d1
	cmpi.b	#1,d2							; Was the last frame the first in this segment?
	blt.s	++							; Branch if yes
	moveq	#2,d1
	cmpi.b	#2,d2							; Was the last frame frame 1?
	blt.s	++							; Branch if yes
	moveq	#4,d1
	cmpi.b	#$A,d2							; Was the last frame less than $A?
	blt.s	++							; Branch if yes
	moveq	#2,d1
	cmpi.b	#$B,d2							; Was the last frame $A?
	blt.s	++							; Branch if yes
	moveq	#0,d1
	cmpi.b	#$C,d2							; Was the last frame $B?
	blt.s	++							; Branch if yes
+
	rts
; ===========================================================================
+
	moveq	#0,d0
	moveq	#0,d2
	move.b	(SSTrack_drawing_index).w,d0				; Get drawing position
	lea_	off_6DEE,a0						; a0 = pointer to background scroll data
	adda.w	(a0,d1.w),a0						; a0 = pointer to background scroll data for current animation frame
	move.b	(a0,d0.w),d2						; Get background offset for current frame duration
	tst.b	(SS_Last_Alternate_HorizScroll_Buf).w			; Was the alternate horizontal scroll buffer used last time?
	bne.s	+							; Branch if yes
	tst.b	(SS_Alternate_HorizScroll_Buf).w			; Is the alternate horizontal scroll buffer being used now?
	beq.s	+++							; Branch if not
	bra.s	++
; ===========================================================================
+
	tst.b	(SS_Alternate_HorizScroll_Buf).w			; Is the alternate horizontal scroll buffer still being used?
	bne.s	++							; Branch if yes
	lea	(SS_Horiz_Scroll_Buf_1 + 2).w,a1			; Load horizontal scroll buffer for PNT B
	bra.s	+++
; ===========================================================================
+
	lea	(SS_Horiz_Scroll_Buf_2 + 2).w,a1			; Load alternate horizontal scroll buffer for PNT B
	neg.w	d2							; Change the sign of the background offset
	bra.s	++
; ===========================================================================
+
	lea	(SS_Horiz_Scroll_Buf_1 + 2).w,a1			; Load horizontal scroll buffer for PNT B
	tst.b	(SS_Alternate_HorizScroll_Buf).w			; Is the alternate horizontal scroll buffer being used now?
	beq.s	+							; Branch if not
	lea	(SS_Horiz_Scroll_Buf_2 + 2).w,a1			; Load alternate horizontal scroll buffer for PNT B
	neg.w	d2							; Change the sign of the background offset
+
	move.w	#bytesToLcnt(HorizontalScrollBuffer.len),d0		; 256 lines
-	sub.w	d2,(a1)+						; Change current line's offset
	adda_.l	#2,a1							; Skip PNTA entry
	dbf	d0,-

	rts
; End of function SSPlaneB_SetHorizOffset

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_6EE0
SSTrack_SetVscroll:
	move.w	(Vscroll_Factor_BG).w,(SSTrack_LastVScroll).w		; Save last vertical scroll value
	moveq	#0,d7							; Set flag to decrease vertical scroll
	moveq	#0,d0
	moveq	#0,d2
	move.b	(SSTrack_last_anim_frame).w,d2				; Get last track animation frame
	move.b	(SSTrack_anim).w,d0					; Get current track animation
	add.w	d0,d0							; Convert it to index
	move.w	off_6EFE(pc,d0.w),d0
	jmp	off_6EFE(pc,d0.w)
; ===========================================================================
off_6EFE:	offsetTable
		offsetTableEntry.w loc_6F0A	; 0		; Turn, then rise
		offsetTableEntry.w loc_6F2A	; 1		; Turn, then drop
		offsetTableEntry.w +		; 2 ; rts	; Turn, then straight
		offsetTableEntry.w loc_6F4C	; 3		; Straight
		offsetTableEntry.w +		; 4 ; rts	; Straight, then turn
; ===========================================================================
+
	rts
; ===========================================================================

loc_6F0A:
	move.b	+(pc,d2.w),d1							; Get current frame's vertical scroll offset
	bpl.s	SSTrack_ApplyVscroll						; Branch if positive
	rts
; ===========================================================================
; Special stage vertical scroll index for 'turn, then rise' animation
+
	dc.b  -1
	dc.b  -1	; 1
	dc.b  -1	; 2
	dc.b  -1	; 3
	dc.b  -1	; 4
	dc.b  -1	; 5
	dc.b  -1	; 6
	dc.b  -1	; 7
	dc.b  -1	; 8
	dc.b  -1	; 9
	dc.b   8	; 10
	dc.b   8	; 11
	dc.b   2	; 12
	dc.b   4	; 13
	dc.b   4	; 14
	dc.b   4	; 15
	dc.b   4	; 16
	dc.b   4	; 17
	dc.b   4	; 18
	dc.b  $A	; 19
	dc.b  $C	; 20
	dc.b  $E	; 21
	dc.b $12	; 22
	dc.b $10	; 23
	even
; ===========================================================================

loc_6F2A:
	st.b	d7								; Set flag to increase vertical scroll
	move.b	+(pc,d2.w),d1							; Get current frame's vertical scroll offset
	bpl.s	SSTrack_ApplyVscroll						; Branch if positive
	rts
; ===========================================================================
; Special stage vertical scroll index for 'turn, then drop' animation
+
	dc.b  -1
	dc.b  -1	; 1
	dc.b  -1	; 2
	dc.b  -1	; 3
	dc.b  -1	; 4
	dc.b  -1	; 5
	dc.b  -1	; 6
	dc.b  -1	; 7
	dc.b  -1	; 8
	dc.b  -1	; 9
	dc.b  -1	; 10
	dc.b $10	; 11
	dc.b $12	; 12
	dc.b  $E	; 13
	dc.b  $C	; 14
	dc.b  $A	; 15
	dc.b   4	; 16
	dc.b   4	; 17
	dc.b   4	; 18
	dc.b   4	; 19
	dc.b   4	; 20
	dc.b   4	; 21
	dc.b   2	; 22
	dc.b   0	; 23
	even
; ===========================================================================

loc_6F4C:
	tst.b	(SS_Pause_Only_flag).w						; Is the game paused?
	bne.s	+	; rts							; Return if yes
	move.b	++(pc,d2.w),d1							; Get current frame's vertical scroll offset
	bpl.s	SSTrack_ApplyVscroll						; Branch if positive
+
	rts
; ===========================================================================
; Special stage vertical scroll index for 'straight' animation -- bobbing up and down
+
    rept 4
	dc.b   6
	dc.b   6
	dc.b $14
	dc.b $14
    endm
; ===========================================================================
;loc_6F6A
SSTrack_ApplyVscroll:
	moveq	#0,d0
	moveq	#0,d2
	move.b	(SSTrack_drawing_index).w,d0					; Get drawing position
	lea_	off_6DEE,a0							; a0 = pointer to background scroll data
	adda.w	(a0,d1.w),a0							; a0 = pointer to background scroll data for current animation frame
	move.b	(a0,d0.w),d2							; Get background offset for current frame duration
	tst.b	d7								; Are we supposed to increase the vertical scroll?
	bpl.s	+								; Branch if not
	add.w	d2,(Vscroll_Factor_BG).w					; Increase vertical scroll
	rts
; ===========================================================================
+
	sub.w	d2,(Vscroll_Factor_BG).w					; Decrease vertical scroll
	rts
; End of function SSTrack_SetVscroll

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


; sub_6F8E: SSSingleObjLoad:
SSAllocateObject:
	lea	(SS_Dynamic_Object_RAM).w,a1
	move.w	#(SS_Dynamic_Object_RAM_End-SS_Dynamic_Object_RAM)/object_size-1,d5

-	tst.b	id(a1)
	beq.s	+	; rts
	lea	next_object(a1),a1 ; a1=object
	dbf	d5,-
+
	rts
; End of function sub_6F8E

; ===========================================================================

;loc_6FA4: SSSingleObjLoad2:
SSAllocateObjectAfterCurrent:
	movea.l	a0,a1
	move.w	#SS_Dynamic_Object_RAM_End,d5
	sub.w	a0,d5

    if object_size=$40
	lsr.w	#object_size_bits,d5
	subq.w	#1,d5
	bcs.s	+	; rts
    else
	lsr.w	#6,d5			; divide by $40
	move.b	++(pc,d5.w),d5		; load the right number of objects from table
	bmi.s	+			; if negative, we have failed!
    endif

-	tst.b	id(a1)
	beq.s	+	; rts
	lea	next_object(a1),a1
	dbf	d5,-

+	rts

    if object_size<>$40
+
.a	set	Object_RAM
.b	set	SS_Dynamic_Object_RAM_End
.c	set	.b			; begin from bottom of array and decrease backwards
	rept	(.b-.a+$40-1)/$40	; repeat for all slots, minus exception
.c	set	.c-$40			; address for previous $40 (also skip last part)
	dc.b	(.b-.c-1)/object_size-1	; write possible slots according to object_size division + hack + dbf hack
	endm
	even
    endif
; ===========================================================================
; ----------------------------------------------------------------------------
; Object 5E - HUD from Special Stage
; ----------------------------------------------------------------------------
; Sprite_6FC0:
Obj5E:
	move.b	routine(a0),d0
    if fixBugs
	; See below.
	beq.s	+
	move.w	#object_display_list_size*0,d0
	jmp	(DisplaySprite3).l
+
    else
	bne.w	JmpTo_DisplaySprite
    endif
	move.l	#Obj5E_MapUnc_7070,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialHUD,0,0),art_tile(a0)
	move.b	#4,render_flags(a0)
    if ~~fixBugs
	; Multi-sprite objects cannot use the 'priority' SST as it is
	; overwritten by 'sub3_y_pos'.
	move.b	#0,priority(a0)
    endif
	move.b	#1,routine(a0)
	bset	#6,render_flags(a0)
	moveq	#0,d1
	tst.b	(SS_2p_Flag).w
	beq.s	+
	addq.w	#6,d1
	tst.b	(Graphics_Flags).w
	bpl.s	++
	addq.w	#1,d1
	bra.s	++
; ---------------------------------------------------------------------------
+	move.w	(Player_mode).w,d1
	andi.w	#3,d1
	tst.b	(Graphics_Flags).w
	bpl.s	+
	addq.w	#3,d1 ; set special stage Tails name to "TAILS" instead of MILES
+
	add.w	d1,d1
	moveq	#0,d2
	moveq	#0,d3
	lea	(SSHUDLayout).l,a1
	lea	subspr_data(a0),a2
	adda.w	(a1,d1.w),a1
	move.b	(a1)+,d3
	move.b	d3,mainspr_childsprites(a0)
	subq.w	#1,d3
	moveq	#0,d0
	move.b	(a1)+,d0

-	move.w	d0,(a2,d2.w)
	move.b	(a1)+,sub2_mapframe-sub2_x_pos(a2,d2.w)	; sub2_mapframe
	addq.w	#next_subspr,d2
	dbf	d3,-

	rts
; ===========================================================================
; off_7042:
SSHUDLayout:	offsetTable
		offsetTableEntry.w SSHUD_SonicMilesTotal	; 0
		offsetTableEntry.w SSHUD_Sonic			; 1
		offsetTableEntry.w SSHUD_Miles			; 2
		offsetTableEntry.w SSHUD_SonicTailsTotal	; 3
		offsetTableEntry.w SSHUD_Sonic_2		; 4
		offsetTableEntry.w SSHUD_Tails			; 5
		offsetTableEntry.w SSHUD_SonicMiles		; 6
		offsetTableEntry.w SSHUD_SonicTails		; 7

; byte_7052:
SSHUD_SonicMilesTotal:
	dc.b   3		; Sprite count
	dc.b   $80		; X-pos
	dc.b   0,  1,  3	; Sprite 1 frame, Sprite 2 frame, etc
; byte_7057:
SSHUD_Sonic:
	dc.b   1
	dc.b   $D4
	dc.b   0
; byte_705A:
SSHUD_Miles:
	dc.b   1
	dc.b   $38
	dc.b   1

; byte_705D:
SSHUD_SonicTailsTotal:
	dc.b   3
	dc.b   $80
	dc.b   0,  2,  3
; byte_7062:
SSHUD_Sonic_2:
	dc.b   1
	dc.b   $D4
	dc.b   0
; byte_7065:
SSHUD_Tails:
	dc.b   1
	dc.b   $38
	dc.b   2

; 2 player
; byte_7068:
SSHUD_SonicMiles:
	dc.b   2
	dc.b   $80
	dc.b   0,  1
; byte_706C:
SSHUD_SonicTails:
	dc.b   2
	dc.b   $80
	dc.b   0,  2
; -----------------------------------------------------------------------------------
; sprite mappings
; -----------------------------------------------------------------------------------
Obj5E_MapUnc_7070:	include "mappings/sprite/obj5E.asm"
; ===========================================================================
; ----------------------------------------------------------------------------
; Object 5F - Start banner/"Ending controller" from Special Stage
; ----------------------------------------------------------------------------
; Sprite_70F0:
Obj5F:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj5F_Index(pc,d0.w),d1
	jmp	Obj5F_Index(pc,d1.w)
; ===========================================================================
; off_70FE:
Obj5F_Index:	offsetTable
		offsetTableEntry.w Obj5F_Init	;  0
		offsetTableEntry.w Obj5F_Main	;  2
		offsetTableEntry.w loc_71B4	;  4
		offsetTableEntry.w loc_710A	;  6
		offsetTableEntry.w return_723E	;  8
		offsetTableEntry.w loc_7218	; $A
; ===========================================================================

loc_710A:
	moveq	#0,d0
	move.b	angle(a0),d0
	bsr.w	CalcSine
	muls.w	objoff_14(a0),d0
	muls.w	objoff_14(a0),d1
	asr.w	#8,d0
	asr.w	#8,d1
	add.w	d1,x_pos(a0)
	add.w	d0,y_pos(a0)
	cmpi.w	#0,x_pos(a0)
	blt.w	JmpTo_DeleteObject
	cmpi.w	#$100,x_pos(a0)
	bgt.w	JmpTo_DeleteObject
	cmpi.w	#0,y_pos(a0)
	blt.w	JmpTo_DeleteObject

    if removeJmpTos
JmpTo_DisplaySprite ; JmpTo
    endif

	jmpto	DisplaySprite, JmpTo_DisplaySprite
; ===========================================================================

; loc_714A:
Obj5F_Init:
	tst.b	(SS_2p_Flag).w
	beq.s	+
	move.w	#8,d0
	jsrto	Obj5A_PrintPhrase, JmpTo_Obj5A_PrintPhrase
+	move.w	#$80,x_pos(a0)
	move.w	#-$40,y_pos(a0)
	move.w	#$100,y_vel(a0)
	move.l	#Obj5F_MapUnc_7240,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialStart,0,0),art_tile(a0)
	move.b	#4,render_flags(a0)
	move.b	#1,priority(a0)
	move.b	#2,routine(a0)

; loc_718A:
Obj5F_Main:
	jsrto	ObjectMove, JmpTo_ObjectMove
	cmpi.w	#$48,y_pos(a0)
	blt.w	JmpTo_DisplaySprite
	move.w	#0,y_vel(a0)
	move.w	#$48,y_pos(a0)
	move.b	#4,routine(a0)
	move.b	#$F,objoff_2A(a0)
	jmpto	DisplaySprite, JmpTo_DisplaySprite
; ===========================================================================

loc_71B4:
	subi_.b	#1,objoff_2A(a0)
    if ~~removeJmpTos
	bne.w	JmpTo_DisplaySprite
    else
	bne.s	JmpTo_DisplaySprite
    endif
	moveq	#6,d6

	lea	(Obj5F_MapUnc_7240.frame2).l,a2

	moveq	#2,d3
	move.w	#8,objoff_14(a0)
	move.b	#6,routine(a0)

-	bsr.w	SSAllocateObject
	bne.s	+
	moveq	#0,d0

	move.w	#bytesToLcnt(object_size),d1

-	move.l	(a0,d0.w),(a1,d0.w)
	addq.w	#4,d0
	dbf	d1,-
    if object_size&3
	move.w	(a0,d0.w),(a1,d0.w)
    endif

	move.b	d3,mapping_frame(a1)
	addq.w	#1,d3
	move.w	#-$28,d2
	move.w	8(a2),d1
	bsr.w	CalcAngle
	move.b	d0,angle(a1)
	lea	$A(a2),a2
+	dbf	d6,--

	move.b	#$A,routine(a0)
	move.w	#$1E,objoff_2A(a0)
	rts
; ===========================================================================

loc_7218:
	subi_.w	#1,objoff_2A(a0)
	bpl.s	+++	; rts
	tst.b	(SS_2p_Flag).w
	beq.s	+
	move.w	#$A,d0
	jsrto	Obj5A_PrintPhrase, JmpTo_Obj5A_PrintPhrase
	bra.s	++
; ===========================================================================
+	jsrto	Obj5A_CreateRingReqMessage, JmpTo_Obj5A_CreateRingReqMessage

+	st.b	(SpecialStage_Started).w
	jmpto	DeleteObject, JmpTo_DeleteObject
; ===========================================================================

+	rts
; ===========================================================================

    if removeJmpTos
JmpTo_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
    endif

; ===========================================================================

return_723E:
	rts
; ===========================================================================
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
Obj5F_MapUnc_7240:	include "mappings/sprite/obj5F_a.asm"
; -----------------------------------------------------------------------------------
; sprite mappings
; -----------------------------------------------------------------------------------
Obj5F_MapUnc_72D2:	include "mappings/sprite/obj5F_b.asm"
; ===========================================================================
; ----------------------------------------------------------------------------
; Object 87 - Number of rings in Special Stage
; ----------------------------------------------------------------------------
; Sprite_7356:
Obj87:
	moveq	#0,d0
	move.b	objoff_A(a0),d0
	move.w	Obj87_Index(pc,d0.w),d1
	jmp	Obj87_Index(pc,d1.w)
; ===========================================================================
; off_7364:
Obj87_Index:	offsetTable
		offsetTableEntry.w Obj87_Init	; 0
		offsetTableEntry.w loc_7480	; 2
		offsetTableEntry.w loc_753E	; 4
		offsetTableEntry.w loc_75DE	; 6
; ===========================================================================

; loc_736C:
Obj87_Init:
	move.b	#2,objoff_A(a0)		; => loc_7480
	move.l	#Obj5F_MapUnc_72D2,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialHUD,2,0),art_tile(a0)
	move.b	#4,render_flags(a0)
	bset	#6,render_flags(a0)
	move.b	#2,mainspr_childsprites(a0)
	move.w	#$20,d0
	moveq	#0,d1
	lea	subspr_data(a0),a1
	move.w	#$48,sub2_x_pos-subspr_data(a1)	; sub2_x_pos
	move.w	d0,sub2_y_pos-subspr_data(a1)	; sub2_y_pos
	move.w	d1,mainspr_height-subspr_data(a1) ; mainspr_height and sub2_mapframe
	move.w	#$E0,sub3_x_pos-subspr_data(a1)	; sub3_x_pos
	move.w	d0,sub3_y_pos-subspr_data(a1)	; sub3_y_pos
	move.w	d1,mapping_frame-subspr_data(a1)	; mapping_frame	and sub3_mapframe
	move.w	d0,sub4_y_pos-subspr_data(a1)	; sub4_y_pos
	move.w	d0,sub5_y_pos-subspr_data(a1)	; sub5_y_pos
	move.w	d0,sub6_y_pos-subspr_data(a1)	; sub6_y_pos
	move.w	d0,sub7_y_pos-subspr_data(a1)	; sub7_y_pos
	tst.b	(SS_2p_Flag).w
	bne.s	+++
	cmpi.w	#0,(Player_mode).w
	beq.s	+
	subi_.b	#1,mainspr_childsprites(a0)
	move.w	#$94,sub2_x_pos-subspr_data(a1)	; sub2_x_pos
	rts
; ===========================================================================
+
	bsr.w	SSAllocateObject
	bne.s	+	; rts
	move.b	#ObjID_SSNumberOfRings,id(a1) ; load obj87
	move.b	#4,objoff_A(a1)		; => loc_753E
	move.l	#Obj5F_MapUnc_72D2,mappings(a1)
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialHUD,2,0),art_tile(a1)
	move.b	#4,render_flags(a1)
	bset	#6,render_flags(a1)
	move.b	#1,mainspr_childsprites(a1)
	lea	subspr_data(a1),a2
	move.w	#$80,sub2_x_pos-subspr_data(a2)	; sub2_x_pos
	move.w	d0,sub2_y_pos-subspr_data(a2)	; sub2_y_pos
	move.w	d1,mainspr_height-subspr_data(a2) ; mainspr_height and sub2_mapframe
	move.w	d0,sub3_y_pos-subspr_data(a2)	; sub3_y_pos
	move.w	d0,sub4_y_pos-subspr_data(a2)	; sub4_y_pos
/	rts
; ===========================================================================
+
	bsr.w	SSAllocateObject
	bne.s	-	; rts
	move.b	#ObjID_SSNumberOfRings,id(a1) ; load obj87
	move.b	#6,objoff_A(a1)		; => loc_75DE
	move.l	#Obj5F_MapUnc_72D2,mappings(a1)
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialHUD,2,0),art_tile(a1)
	move.b	#4,render_flags(a1)
	bset	#6,render_flags(a1)
	move.b	#0,mainspr_childsprites(a1)
	lea	subspr_data(a1),a2
	move.w	#$2C,d0
	move.w	#$A,d1
	move.w	d0,sub2_y_pos-subspr_data(a2)	; sub2_y_pos
	move.w	d1,mainspr_height-subspr_data(a2) ; mainspr_height and sub2_mapframe
	move.w	d0,sub3_y_pos-subspr_data(a2)	; sub3_y_pos
	move.w	d1,mapping_frame-subspr_data(a2)	; mapping_frame	and sub3_mapframe
	move.w	d0,sub4_y_pos-subspr_data(a2)	; sub4_y_pos
	move.w	d1,sub4_mapframe-1-subspr_data(a2) ; something and sub4_mapframe
	rts
; ===========================================================================

loc_7480:
	moveq	#0,d0
	moveq	#0,d3
	moveq	#0,d5
	lea	sub2_x_pos(a0),a1
	movea.l	a1,a2
	addq.w	#sub2_mapframe-sub2_x_pos,a2	; a2 = sub2_mapframe(a0)
	cmpi.w	#2,(Player_mode).w
	beq.s	loc_74EA
	move.b	(MainCharacter+ss_rings_hundreds).w,d0
	beq.s	+
	addq.w	#1,d3
	move.b	d0,(a2)
	lea	next_subspr(a2),a2
+	move.b	(MainCharacter+ss_rings_tens).w,d0
	tst.b	d3
	bne.s	+
	tst.b	d0
	beq.s	++
+	addq.w	#1,d3
	move.b	d0,(a2)
	lea	next_subspr(a2),a2
+	addq.w	#1,d3
	move.b	(MainCharacter+ss_rings_units).w,(a2)
	lea	next_subspr(a2),a2
	move.w	d3,d4
	subq.w	#1,d4
	move.w	#$48,d1
	tst.w	(Player_mode).w
	beq.s	+
	addi.w	#$54,d1
/	move.w	d1,(a1,d5.w)
	addi_.w	#8,d1
	addq.w	#next_subspr,d5
	dbf	d4,-
	cmpi.w	#1,(Player_mode).w
	beq.s	loc_7536

loc_74EA:
	moveq	#0,d0
	moveq	#0,d4
	move.b	(Sidekick+ss_rings_hundreds).w,d0
	beq.s	+
	addq.w	#1,d4
	move.b	d0,(a2)
	lea	next_subspr(a2),a2
+	move.b	(Sidekick+ss_rings_tens).w,d0
	tst.b	d4
	bne.s	+
	tst.b	d0
	beq.s	++
+
	addq.w	#1,d4
	move.b	d0,(a2)
	lea	next_subspr(a2),a2
+	move.b	(Sidekick+ss_rings_units).w,(a2)
	addq.w	#1,d4
	add.w	d4,d3
	subq.w	#1,d4
	move.w	#$E0,d1
	tst.w	(Player_mode).w
	beq.s	+
	subi.w	#$44,d1
/	move.w	d1,(a1,d5.w)
	addi_.w	#8,d1
	addq.w	#6,d5
	dbf	d4,-

loc_7536:
	move.b	d3,mainspr_childsprites(a0)
    if fixBugs
	; Multi-sprite objects cannot use the 'priority' SST value, so they
	; must use 'DisplaySprite3' instead of 'DisplaySprite'.
	; This object's 'priority' is overwritten by 'sub3_y_pos', causing it
	; to display on the wrong layer.
	move.w	#object_display_list_size*0,d0
	jmp	(DisplaySprite3).l
    else
	jmpto	DisplaySprite, JmpTo_DisplaySprite
    endif
; ===========================================================================

loc_753E:
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	moveq	#1,d3
	move.b	(MainCharacter+ss_rings_units).w,d0
	add.b	(Sidekick+ss_rings_units).w,d0
	move.b	(MainCharacter+ss_rings_tens).w,d1
	add.b	(Sidekick+ss_rings_tens).w,d1
	move.b	(MainCharacter+ss_rings_hundreds).w,d2
	add.b	(Sidekick+ss_rings_hundreds).w,d2
	cmpi.b	#10,d0
	blo.s	+
	addq.w	#1,d1
	subi.b	#10,d0
+
	tst.b	d1
	beq.s	++
	cmpi.b	#10,d1
	blo.s	+
	addi_.b	#1,d2
	subi.b	#10,d1
+
	addq.w	#1,d3
	tst.b	d2
	beq.s	++
	addq.w	#1,d3
	bra.s	++
; ===========================================================================
+
	tst.b	d2
	beq.s	+
	addq.w	#2,d3
+
	lea	subspr_data(a0),a1
	move.b	d3,mainspr_childsprites(a0)
	cmpi.b	#2,d3
	blt.s	+
	beq.s	++
	move.w	#$78,sub2_x_pos-subspr_data(a1)		; sub2_x_pos
	move.b	d2,sub2_mapframe-subspr_data(a1)	; sub2_mapframe
	move.w	#$80,sub3_x_pos-subspr_data(a1)		; sub3_x_pos
	move.b	d1,sub3_mapframe-subspr_data(a1)	; sub3_mapframe
	move.w	#$88,sub4_x_pos-subspr_data(a1)		; sub4_x_pos
	move.b	d0,sub4_mapframe-subspr_data(a1)	; sub4_mapframe
    if fixBugs
	; Multi-sprite objects cannot use the 'priority' SST value, so they
	; must use 'DisplaySprite3' instead of 'DisplaySprite'.
	; This object's 'priority' is overwritten by 'sub3_y_pos', causing it
	; to display on the wrong layer.
	move.w	#object_display_list_size*0,d0
	jmp	(DisplaySprite3).l
    else
	jmpto	DisplaySprite, JmpTo_DisplaySprite
    endif
; ===========================================================================
+
	move.w	#$80,sub2_x_pos-subspr_data(a1)	; sub2_x_pos
	move.b	d0,sub2_mapframe-subspr_data(a1)	; sub2_mapframe
    if fixBugs
	; Multi-sprite objects cannot use the 'priority' SST value, so they
	; must use 'DisplaySprite3' instead of 'DisplaySprite'.
	; This object's 'priority' is overwritten by 'sub3_y_pos', causing it
	; to display on the wrong layer.
	move.w	#object_display_list_size*0,d0
	jmp	(DisplaySprite3).l
    else
	jmpto	DisplaySprite, JmpTo_DisplaySprite
    endif
; ===========================================================================
+
	move.w	#$7C,sub2_x_pos-subspr_data(a1)		; sub2_x_pos
	move.b	d1,sub2_mapframe-subspr_data(a1)	; sub2_mapframe
	move.w	#$84,sub3_x_pos-subspr_data(a1)		; sub3_x_pos
	move.b	d0,sub3_mapframe-subspr_data(a1)	; sub3_mapframe
    if fixBugs
	; Multi-sprite objects cannot use the 'priority' SST value, so they
	; must use 'DisplaySprite3' instead of 'DisplaySprite'.
	; This object's 'priority' is overwritten by 'sub3_y_pos', causing it
	; to display on the wrong layer.
	move.w	#object_display_list_size*0,d0
	jmp	(DisplaySprite3).l
    else
	jmpto	DisplaySprite, JmpTo_DisplaySprite
    endif
; ===========================================================================

loc_75DE:
	move.b	(SS_2P_BCD_Score).w,d0
	bne.s	+
	rts
; ===========================================================================
+
	lea	sub2_x_pos(a0),a1
	moveq	#0,d2
	move.b	d0,d1
	andi.b	#$F0,d0
	beq.s	+
	addq.w	#1,d2
	move.w	#$20,(a1)	; sub2_x_pos
	lea	next_subspr(a1),a1
	subi.b	#$10,d0
	beq.s	+
	addq.w	#1,d2
	move.w	#$30,(a1)	; sub3_x_pos
	lea	next_subspr(a1),a1
	subi.b	#$10,d0
	beq.s	+
	addq.w	#1,d2
	move.w	#$40,(a1)	; sub4_x_pos
	bra.s	++
; ===========================================================================
+
	andi.b	#$F,d1
	beq.s	+
	addq.w	#1,d2
	move.w	#$B8,(a1)	; sub?_x_pos
	lea	next_subspr(a1),a1
	subi_.b	#1,d1
	beq.s	+
	addq.w	#1,d2
	move.w	#$C8,(a1)	; sub?_x_pos
	lea	next_subspr(a1),a1
	subi_.b	#1,d1
	beq.s	+
	addq.w	#1,d2
	move.w	#$D8,(a1)	; sub?_x_pos
+
	move.b	d2,mainspr_childsprites(a0)
    if fixBugs
	; Multi-sprite objects cannot use the 'priority' SST value, so they
	; must use 'DisplaySprite3' instead of 'DisplaySprite'.
	; This object's 'priority' is overwritten by 'sub3_y_pos', causing it
	; to display on the wrong layer.
	move.w	#object_display_list_size*0,d0
	jmp	(DisplaySprite3).l
    else
	jmpto	DisplaySprite, JmpTo_DisplaySprite
    endif

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_7650
SSSetGeometryOffsets:
	move.b	(SSTrack_drawing_index).w,d0					; Get drawing position
	cmp.b	(SS_player_anim_frame_timer).w,d0				; Compare to player frame duration
	beq.s	+												; If both are equal, branch
	rts
; ===========================================================================
+
	moveq	#0,d0
	move.b	(SSTrack_mapping_frame).w,d0					; Get current track mapping frame
	add.w	d0,d0											; Convert to index
	lea	SSCurveOffsets(pc,d0.w),a2							; Load current curve offsets into a2
	move.b	(a2)+,d0										; Get x offset
	tst.b	(SSTrack_Orientation).w							; Is track flipped?
	beq.s	+												; Branch if not
	neg.b	d0												; Change sign of offset
+
	ext.w	d0												; Extend to word
	addi.w	#$80,d0											; Add 128 (why?)
	move.w	d0,(SS_Offset_X).w								; Set X geometry offset
	move.b	(a2),d0											; Get y offset
	ext.w	d0												; Extend to word
	addi.w	#$36,d0											; Add $36 (why?)
	move.w	d0,(SS_Offset_Y).w								; Set Y geometry offset
	rts
; End of function SSSetGeometryOffsets

; ===========================================================================
; Position offsets to sort-of rotate the plane Sonic/Tails are in
; when the special stage track is curving, so they follow it better.
; Each word seems to be (x_offset, y_offset)
; See also Ani_SpecialStageTrack.
SSCurveOffsets: ; word_768A:
	dc.b $13,   0,   $13,   0,   $13,   0,   $13,   0	; $00
	dc.b   9, -$A,     0,-$1C,     0,-$1C,     0,-$20	; $04
	dc.b   0,-$24,     0,-$2A,     0,-$10,     0,   6	; $08
	dc.b   0,  $E,     0, $10,     0, $12,     0, $12	; $0C
	dc.b   9, $12                                    	; $10; upward curve
	dc.b   0,   0,     0,   0,     0,   0,     0,   0	; $11; straight
	dc.b $13,   0,   $13,   0,   $13,   0,   $13,   0	; $15
	dc.b  $B,  $C,     0,  $C,     0, $12,     0,  $A	; $19
	dc.b   0,   8,     0,   2,     0, $10,     0,-$20	; $1D
	dc.b   0,-$1F,     0,-$1E,     0,-$1B,     0,-$18	; $21
	dc.b   0, -$E                                    	; $25; downward curve
	dc.b $13,   0,   $13,   0,   $13,   0,   $13,   0	; $26
	dc.b $13,   0,   $13,   0                        	; $2B; turning
	dc.b $13,   0,   $13,   0,   $13,   0,   $13,   0	; $2C
	dc.b  $B,   0                                    	; $30; exit turn
	dc.b   0,   0,     0,   0,     0,   0,     0,   0	; $31
	dc.b   0,   0,     0,   0,     3,   0            	; $35; straight
; ===========================================================================
; Subroutine to advance to the next act and get an encoded version
; of the ring requirements.
; Output:
; 	d0, d1: Binary coded decimal version of ring requirements (maximum of 299 rings)
; 	d2: Number of digits in the ring requirements - 1 (minimum 2 digits)
;loc_76FA
SSStartNewAct:
	moveq	#0,d1
	moveq	#1,d2
	move.w	(Current_Special_StageAndAct).w,d0
	move.b	d0,d1
	lsr.w	#8,d0
	add.w	d0,d0
	add.w	d0,d0
	add.w	d1,d0
	tst.w	(Player_mode).w
	bne.s	+
	move.b	SpecialStage_RingReq_Team(pc,d0.w),d1
	bra.s	++
; ===========================================================================
+
	move.b	SpecialStage_RingReq_Alone(pc,d0.w),d1
+
	move.w	d1,(SS_Ring_Requirement).w
	moveq	#0,d0
	cmpi.w	#100,d1
	blt.s	+
	addq.w	#1,d2
    if fixBugs
	; The following code does a more complete binary coded decimal conversion:
-	addi.w	#$100,d0
	subi.w	#100,d1
	cmpi.w	#100,d1
	bge.s	-
    else
	; This code (the original) is limited to 299 rings:
	subi.w	#100,d1
	move.w	#$100,d0
	cmpi.w	#100,d1
	blt.s	+
	subi.w	#100,d1
	addi.w	#$100,d0
    endif
+
	divu.w	#10,d1
	lsl.w	#4,d1
	or.b	d1,d0
	swap	d1
	or.b	d1,d0
	move.w	d0,d1
	addi_.w	#1,(Current_Special_StageAndAct).w
	rts
; ===========================================================================
; ----------------------------------------------------------------------------
; Ring requirement values for Sonic and Tails games
;
; This array stores the number of rings you need to get to complete each round
; of each special stage, while playing with both Sonic and Tails. 4 bytes per
; stage, corresponding to the four possible parts of the level. Last part is unused.
; ----------------------------------------------------------------------------
; Misc_7756:
SpecialStage_RingReq_Team:
	dc.b  40, 80,140,120	; 4
	dc.b  50,100,140,150	; 8
	dc.b  60,110,160,170	; 12
	dc.b  40,100,150,160	; 16
	dc.b  55,110,200,200	; 20
	dc.b  80,140,220,220	; 24
	dc.b 100,190,210,210	; 28
	even
; ----------------------------------------------------------------------------
; Ring requirement values for Sonic or Tails alone games
;
; This array stores the number of rings you need to get to complete each round
; of each special stage, while playing with either Sonic or Tails. 4 bytes per
; stage, corresponding to the four possible parts of the level. Last part is unused.
; ----------------------------------------------------------------------------
; Misc_7772:
SpecialStage_RingReq_Alone:
	dc.b  30, 70,130,110	; 4
	dc.b  50,100,140,140	; 8
	dc.b  50,110,160,160	; 12
	dc.b  40,110,150,150	; 16
	dc.b  50, 90,160,160	; 20
	dc.b  80,140,210,210	; 24
	dc.b 100,150,190,190	; 28
	even

; special stage palette table
; word_778E:
SpecialStage_Palettes:
	dc.w   PalID_SS1
	dc.w   PalID_SS2
	dc.w   PalID_SS3
	dc.w   PalID_SS4
	dc.w   PalID_SS5
	dc.w   PalID_SS6
	dc.w   PalID_SS7
	dc.w   PalID_SS1_2p
	dc.w   PalID_SS2_2p
	dc.w   PalID_SS3_2p

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_77A2
SSInitPalAndData:
	clr.b	(Current_Special_Act).w
	move.b	#-1,(SpecialStage_LastSegment2).w
	move.w	#0,(Ring_count).w
	move.w	#0,(Ring_count_2P).w
	move.b	#0,(Perfect_rings_flag).w
	move.b	#0,(Got_Emerald).w
	move.b	#4,(SS_Star_color_2).w
	lea	(SS2p_RingBuffer).w,a2
	moveq	#0,d0
	move.w	d0,(a2)+
	move.w	d0,(a2)+
	move.w	d0,(a2)+
	move.w	d0,(a2)+
	move.w	d0,(a2)+
	move.w	d0,(a2)+
	moveq	#PalID_SS,d0
	bsr.w	PalLoad_ForFade
	lea_	SpecialStage_Palettes,a1
	moveq	#0,d0
	move.b	(Current_Special_Stage).w,d0
	add.w	d0,d0
	move.w	d0,d1
	tst.b	(SS_2p_Flag).w
	beq.s	+
	cmpi.b	#4,d0
	blo.s	+
	addi_.w	#6,d0
+
	move.w	(a1,d0.w),d0
	bsr.w	PalLoad_ForFade
	lea	(SSRAM_MiscKoz_SpecialObjectLocations).w,a0
	adda.w	(a0,d1.w),a0
	move.l	a0,(SS_CurrentLevelObjectLocations).w
	lea	(SSRAM_MiscNem_SpecialLevelLayout).w,a0
	adda.w	(a0,d1.w),a0
	move.l	a0,(SS_CurrentLevelLayout).w
	rts
; End of function SSInitPalAndData

; ===========================================================================

 ; temporarily remap characters to title card letter format
 ; Characters are encoded as Aa, Bb, Cc, etc. through a macro
 charset 'A',0	; can't have an embedded 0 in a string
 charset 'B',"\4\8\xC\4\x10\x14\x18\x1C\x1E\x22\x26\x2A\4\4\x30\x34\x38\x3C\x40\x44\x48\x4C\x52\x56\4"
 charset 'a',"\4\4\4\4\4\4\4\4\2\4\4\4\6\4\4\4\4\4\4\4\4\4\6\4\4"
 charset '.',"\x5A"

; letter lookup string
llookup	:= "ABCDEFGHIJKLMNOPQRSTUVWXYZ ."

; macro for defining title card letters in conjunction with the remapped character set
titleLetters macro letters
     ;  ". ZYXWVUTSRQPONMLKJIHGFEDCBA"
used := %0110000000000110000000010000	; set to initial state
    irpc char,letters
	if ~~(used&1<<strstr(llookup,"char"))	; has the letter been used already?
used := used|1<<strstr(llookup,"char")	; if not, mark it as used
	dc.b "char"			; output letter code
	if "char"=="."
	dc.b 2			; output character size
	else
	dc.b lowstring("char")	; output letter size
	endif
	endif
    endm
	dc.w $FFFF	; output string terminator
    endm

;word_7822:
SpecialStage_ResultsLetters:
	titleLetters	"ACDGHILMPRSTUW."

 charset ; revert character set

; ===========================================================================

    if gameRevision<2
	nop
    endif

    if ~~removeJmpTos
JmpTo_DisplaySprite ; JmpTo
	jmp	(DisplaySprite).l
JmpTo_LoadTitleCardSS ; JmpTo
	jmp	(LoadTitleCardSS).l
JmpTo_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
JmpTo_Obj5A_CreateRingReqMessage ; JmpTo
	jmp	(Obj5A_CreateRingReqMessage).l
JmpTo_Obj5A_PrintPhrase ; JmpTo
	jmp	(Obj5A_PrintPhrase).l
; sub_7862:
JmpTo_ObjectMove ; JmpTo
	jmp	(ObjectMove).l
JmpTo_Hud_Base ; JmpTo
	jmp	(Hud_Base).l

	align 4
    endif




; ----------------------------------------------------------------------------
; Continue Screen
; ----------------------------------------------------------------------------
; loc_7870:
ContinueScreen:
	bsr.w	Pal_FadeToBlack
	move	#$2700,sr
	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l
	lea	(VDP_control_port).l,a6
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8700,(a6)		; Background palette/color: 0/0
	bsr.w	ClearScreen

	clearRAM Object_RAM,Object_RAM_End

    if fixBugs
	; Clear the DMA queue. This fixes the bug where, if you get a
	; Game Over in Hill Top Zone, then Tails' graphics will be corrupted
	; on the Continue screen.
	; This is caused by HTZ's transforming cloud art being loaded over
	; Tails' Continue art: 'Dynamic_HTZ' is responsible for queueing the
	; art to be transferred with 'QueueDMATransfer', which takes effect
	; around the next frame. The problem here is, the art is queued, you
	; die, get a Game Over, advance to the Continue screen, and then
	; finally the art is loaded.
	clr.w	(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
    endif

	bsr.w	ContinueScreen_LoadLetters
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_ContinueTails),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_ContinueTails).l,a0
	bsr.w	NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_MiniContinue),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_MiniSonic).l,a0
	cmpi.w	#2,(Player_mode).w
	bne.s	+
	lea	(ArtNem_MiniTails).l,a0
+
	bsr.w	NemDec
	moveq	#$A,d1
	jsr	(ContScrCounter).l
	moveq	#PalID_SS1,d0
	bsr.w	PalLoad_ForFade
	move.w	#0,(Target_palette).w
	move.b	#MusID_Continue,d0
	bsr.w	PlayMusic
	move.w	#(11*60)-1,(Demo_Time_left).w	; 11 seconds minus 1 frame
	clr.b	(Level_started_flag).w
	clr.l	(Camera_X_pos_copy).w
	move.l	#$1000000,(Camera_Y_pos_copy).w
	move.b	#ObjID_ContinueChars,(MainCharacter+id).w ; load ObjDB (Sonic on continue screen)
	move.b	#ObjID_ContinueChars,(Sidekick+id).w ; load ObjDB (Tails on continue screen)
	move.b	#6,(Sidekick+routine).w ; => ObjDB_Tails_Init
	move.b	#ObjID_ContinueText,(ContinueText+id).w ; load ObjDA (continue screen text)
	move.b	#ObjID_ContinueIcons,(ContinueIcons+id).w ; load ObjDA (continue icons)
	move.b	#4,(ContinueIcons+routine).w ; => loc_7AD0
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	Pal_FadeFromBlack
-
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	cmpi.b	#4,(MainCharacter+routine).w
	bhs.s	+
	move	#$2700,sr
	move.w	(Demo_Time_left).w,d1
	divu.w	#60,d1
	andi.l	#$F,d1
	jsr	(ContScrCounter).l
	move	#$2300,sr
+
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	cmpi.w	#$180,(Sidekick+x_pos).w
	bhs.s	+
	cmpi.b	#4,(MainCharacter+routine).w
	bhs.s	-
	tst.w	(Demo_Time_left).w
	bne.w	-
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen
	rts
; ---------------------------------------------------------------------------
+
	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)
	move.b	#3,(Life_count).w
	move.b	#3,(Life_count_2P).w
	moveq	#0,d0
	move.w	d0,(Ring_count).w
	move.l	d0,(Timer).w
	move.l	d0,(Score).w
	move.b	d0,(Last_star_pole_hit).w
	move.w	d0,(Ring_count_2P).w
	move.l	d0,(Timer_2P).w
	move.l	d0,(Score_2P).w
	move.b	d0,(Last_star_pole_hit_2P).w
	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w
	subq.b	#1,(Continue_count).w
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_7A04:
ContinueScreen_LoadLetters:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_TitleCard),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_TitleCard).l,a0
	bsr.w	NemDec
	lea	(Level_Layout).w,a4
	lea	(ArtNem_TitleCard2).l,a0
	bsr.w	NemDecToRAM
	lea	(ContinueScreen_AdditionalLetters).l,a0
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ContinueScreen_Additional),VRAM,WRITE),(VDP_control_port).l
	lea	(Level_Layout).w,a1
	lea	(VDP_data_port).l,a6
-
	moveq	#0,d0
	move.b	(a0)+,d0
	bmi.s	+	; rts
	lsl.w	#5,d0
	lea	(a1,d0.w),a2
	moveq	#0,d1
	move.b	(a0)+,d1
	lsl.w	#3,d1
	subq.w	#1,d1

-	move.l	(a2)+,(a6)
	dbf	d1,-

	bra.s	--
; ---------------------------------------------------------------------------
+	rts
; End of function ContinueScreen_LoadLetters

; ===========================================================================

 ; temporarily remap characters to title card letter format
 ; Characters are encoded as Aa, Bb, Cc, etc. through a macro
 charset 'A',0	; can't have an embedded 0 in a string
 charset 'B',"\4\8\xC\4\x10\x14\x18\x1C\x1E\x22\x26\x2A\4\4\x30\x34\x38\x3C\x40\x44\x48\x4C\x52\x56\4"
 charset 'a',"\4\4\4\4\4\4\4\4\2\4\4\4\6\4\4\4\4\4\4\4\4\4\6\4\4"
 charset '.',"\x5A"

; Defines which letters load for the continue screen
; Each letter occurs only once, and  the letters ENOZ (i.e. ZONE) aren't loaded here
; However, this is hidden by the titleLetters macro, and normal titles can be used
; (the macro is defined near SpecialStage_ResultsLetters, which uses it before here)

; word_7A5E:
ContinueScreen_AdditionalLetters:
	titleLetters "CONTINUE"

 charset ; revert character set
; ===========================================================================
; ----------------------------------------------------------------------------
; Object DA - Continue text
; ----------------------------------------------------------------------------
; loc_7A68:
ObjDA: ; (screen-space obj)
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjDA_Index(pc,d0.w),d1
	jmp	ObjDA_Index(pc,d1.w)
; ===========================================================================
; Obj_DA_subtbl:
ObjDA_Index:	offsetTable
		offsetTableEntry.w ObjDA_Init		; 0
		offsetTableEntry.w JmpTo2_DisplaySprite	; 2
		offsetTableEntry.w loc_7AD0		; 4
		offsetTableEntry.w loc_7B46		; 6
; ===========================================================================
; loc_7A7E:
ObjDA_Init:
	addq.b	#2,routine(a0)
	move.l	#ObjDA_MapUnc_7CB6,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtNem_ContinueText,0,1),art_tile(a0)
	jsrto	Adjust2PArtPointer, JmpTo_Adjust2PArtPointer
	move.b	#0,render_flags(a0)
	move.b	#60,width_pixels(a0)
	move.w	#$80+320/2,x_pixel(a0)
	move.w	#$80+64,y_pixel(a0)

JmpTo2_DisplaySprite ; JmpTo
	jmp	(DisplaySprite).l
; ===========================================================================
; word_7AB2:
ObjDA_XPositions:
	dc.w  $116, $12A, $102,	$13E,  $EE, $152,  $DA,	$166
	dc.w   $C6, $17A,  $B2,	$18E,  $9E, $1A2,  $8A;	8
; ===========================================================================

loc_7AD0:
	movea.l	a0,a1
	lea_	ObjDA_XPositions,a2
	moveq	#0,d1
	move.b	(Continue_count).w,d1
	subq.b	#2,d1
	bcc.s	+
	jmp	(DeleteObject).l
; ===========================================================================
+
	moveq	#1,d3
	cmpi.b	#$E,d1
	blo.s	+
	moveq	#0,d3
	moveq	#$E,d1
+
	move.b	d1,d2
	andi.b	#1,d2

-	_move.b	#ObjID_ContinueIcons,id(a1) ; load objDA
	move.w	(a2)+,x_pixel(a1)
	tst.b	d2
	beq.s	+
	subi.w	#$A,x_pixel(a1)
+
	move.w	#$D0,y_pixel(a1)
	move.b	#4,mapping_frame(a1)
	move.b	#6,routine(a1)
	move.l	#ObjDA_MapUnc_7CB6,mappings(a1)
	move.w	#make_art_tile(ArtTile_ArtNem_ContinueText_2,0,1),art_tile(a1)
	jsrto	Adjust2PArtPointer2, JmpTo_Adjust2PArtPointer2
	move.b	#0,render_flags(a1)
	lea	next_object(a1),a1 ; load obj addr
	dbf	d1,-

	lea	-next_object(a1),a1 ; load obj addr
	move.b	d3,subtype(a1)

loc_7B46:
	tst.b	subtype(a0)
	beq.s	+
	cmpi.b	#4,(MainCharacter+routine).w
	blo.s	+
	move.b	(Vint_runcount+3).w,d0
	andi.b	#1,d0
	bne.s	+
	tst.w	(MainCharacter+x_vel).w
	bne.s	JmpTo2_DeleteObject
	rts
; ===========================================================================
+
	move.b	(Vint_runcount+3).w,d0
	andi.b	#$F,d0
	bne.s	JmpTo3_DisplaySprite
	bchg	#0,mapping_frame(a0)

JmpTo3_DisplaySprite ; JmpTo
	jmp	(DisplaySprite).l
; ===========================================================================

JmpTo2_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
; ===========================================================================
; ----------------------------------------------------------------------------
; Object DB - Sonic lying down or Tails nagging (on the continue screen)
; ----------------------------------------------------------------------------
; Sprite_7B82:
ObjDB:
	; a0=character
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjDB_Index(pc,d0.w),d1
	jsr	ObjDB_Index(pc,d1.w)
	jmp	(DisplaySprite).l
; ===========================================================================
; off_7B96: ObjDB_States:
ObjDB_Index:	offsetTable
		offsetTableEntry.w ObjDB_Sonic_Init	;  0
		offsetTableEntry.w ObjDB_Sonic_Wait	;  2
		offsetTableEntry.w ObjDB_Sonic_Run	;  4
		offsetTableEntry.w ObjDB_Tails_Init	;  6
		offsetTableEntry.w ObjDB_Tails_Wait	;  8
		offsetTableEntry.w ObjDB_Tails_Run	; $A
; ===========================================================================
; loc_7BA2:
ObjDB_Sonic_Init:
	addq.b	#2,routine(a0) ; => ObjDB_Sonic_Wait
	move.w	#$9C,x_pos(a0)
	move.w	#$19C,y_pos(a0)
	move.l	#MapUnc_Sonic,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtUnc_Sonic,0,0),art_tile(a0)
	move.b	#4,render_flags(a0)
	move.b	#2,priority(a0)
	move.b	#AniIDSonAni_Lying,anim(a0)

; loc_7BD2:
ObjDB_Sonic_Wait:
	tst.b	(Ctrl_1_Press).w	; is start pressed?
	bmi.s	ObjDB_Sonic_StartRunning ; if yes, branch
	jsr	(Sonic_Animate).l
	jmp	(LoadSonicDynPLC).l
; ---------------------------------------------------------------------------
; loc_7BE4:
ObjDB_Sonic_StartRunning:
	addq.b	#2,routine(a0) ; => ObjDB_Sonic_Run
	move.b	#AniIDSonAni_LieDown,anim(a0)
	clr.w	inertia(a0)
	move.b	#SndID_SpindashRev,d0 ; super peel-out sound
	bsr.w	PlaySound

; loc_7BFA:
ObjDB_Sonic_Run:
	cmpi.w	#$800,inertia(a0)
	bne.s	+
	move.w	#$1000,x_vel(a0)
	bra.s	++
; ---------------------------------------------------------------------------
+
	addi.w	#$20,inertia(a0)
+
	jsr	(ObjectMove).l
	jsr	(Sonic_Animate).l
	jmp	(LoadSonicDynPLC).l
; ===========================================================================
; loc_7C22:
ObjDB_Tails_Init:
	addq.b	#2,routine(a0) ; => ObjDB_Tails_Wait
	move.w	#$B8,x_pos(a0)
	move.w	#$1A0,y_pos(a0)
	move.l	#ObjDA_MapUnc_7CB6,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtNem_ContinueTails,0,0),art_tile(a0)
	move.b	#4,render_flags(a0)
	move.b	#2,priority(a0)
	move.b	#0,anim(a0) ; This is animation 0 of Ani_objDB, not Tails' usual animation script.

; loc_7C52:
ObjDB_Tails_Wait:
	tst.b	(Ctrl_1_Press).w	; is start pressed?
	bmi.s	ObjDB_Tails_StartRunning ; if yes, branch
	lea	(Ani_objDB).l,a1
	jmp	(AnimateSprite).l
; ---------------------------------------------------------------------------
; loc_7C64:
ObjDB_Tails_StartRunning:
	addq.b	#2,routine(a0) ; => ObjDB_Tails_Run
	move.l	#MapUnc_Tails,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtUnc_Tails,0,0),art_tile(a0)
	move.b	#AniIDSonAni_Walk,anim(a0)
	clr.w	inertia(a0)
	move.b	#SndID_SpindashRev,d0 ; super peel-out sound
	bsr.w	PlaySound

; loc_7C88:
ObjDB_Tails_Run:
	cmpi.w	#$720,inertia(a0)
	bne.s	+
	move.w	#$1000,x_vel(a0)
	bra.s	++
; ---------------------------------------------------------------------------
+
	addi.w	#$18,inertia(a0)
+
	jsr	(ObjectMove).l
	jsr	(Tails_Animate).l
	jmp	(LoadTailsDynPLC).l
; ===========================================================================
; animation script for continue screen Tails nagging
; off_7CB0
Ani_objDB:	offsetTable
		offsetTableEntry.w +	; 0
+		dc.b   9,  2,  3,$FF
	even
; -------------------------------------------------------------------------------
; Sprite mappings for text, countdown, stars, and Tails on the continue screen
; Art starts at $A000 in VRAM
; -------------------------------------------------------------------------------
ObjDA_MapUnc_7CB6:	include	"mappings/sprite/objDA.asm"

    if ~~removeJmpTos
JmpTo_Adjust2PArtPointer2 ; JmpTo
	jmp	(Adjust2PArtPointer2).l
JmpTo_Adjust2PArtPointer ; JmpTo
	jmp	(Adjust2PArtPointer).l

	align 4
    endif




; ===========================================================================
; loc_7D50:
TwoPlayerResults:
	bsr.w	Pal_FadeToBlack
	move	#$2700,sr
	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	ClearScreen
	lea	(VDP_control_port).l,a6
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8200|(VRAM_Menu_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_Menu_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $E000
	move.w	#$8200|(VRAM_Menu_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8700,(a6)		; Background palette/color: 0/0
	move.w	#$8C81,(a6)		; H res 40 cells, no interlace, S/H disabled
	move.w	#$9001,(a6)		; Scroll table size: 64x32

	clearRAM Object_Display_Lists,Object_Display_Lists_End
	clearRAM Object_RAM,Object_RAM_End

	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_FontStuff),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_FontStuff).l,a0
	bsr.w	NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_1P2PWins),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_1P2PWins).l,a0
	bsr.w	NemDec
	lea	(Chunk_Table).l,a1
	lea	(MapEng_MenuBack).l,a0
	move.w	#make_art_tile(ArtTile_VRAM_Start,3,0),d0
	bsr.w	EniDec
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_Plane_B_Name_Table,VRAM,WRITE),d0
	moveq	#40-1,d1
	moveq	#28-1,d2
	jsrto	PlaneMapToVRAM_H40, PlaneMapToVRAM_H40
	move.w	(Results_Screen_2P).w,d0
	add.w	d0,d0
	add.w	d0,d0
	add.w	d0,d0
	lea	TwoPlayerResultsPointers(pc),a2
	movea.l	(a2,d0.w),a0
	movea.l	4(a2,d0.w),a2
	lea	(Chunk_Table).l,a1
	move.w	#make_art_tile(ArtTile_VRAM_Start,0,0),d0
	bsr.w	EniDec
	jsr	(a2)	; dynamic call! to Setup2PResults_Act, Setup2PResults_Zone, Setup2PResults_Game, Setup2PResults_SpecialAct, or Setup2PResults_SpecialZone, assuming the pointers in TwoPlayerResultsPointers have not been changed
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(tiles_to_bytes(ArtTile_TwoPlayerResults),VRAM,WRITE),d0
	moveq	#40-1,d1
	moveq	#28-1,d2
	jsrto	PlaneMapToVRAM_H40, PlaneMapToVRAM_H40
	clr.w	(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
	clr.b	(Level_started_flag).w
	clr.w	(Anim_Counters).w
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo_Dynamic_Normal
	moveq	#PLCID_Std1,d0
	bsr.w	LoadPLC2
	moveq	#PalID_Menu,d0
	bsr.w	PalLoad_ForFade
	moveq	#0,d0
	move.b	#MusID_2PResult,d0
	cmp.w	(Level_Music).w,d0
	beq.s	+
	move.w	d0,(Level_Music).w
	bsr.w	PlayMusic
+
	move.w	#(30*60)-1,(Demo_Time_left).w	; 30 seconds
	clr.w	(Two_player_mode).w
	clr.l	(Camera_X_pos).w
	clr.l	(Camera_Y_pos).w
	clr.l	(Vscroll_Factor).w
	clr.l	(Vscroll_Factor_P2).w
	clr.l	(Vscroll_Factor_P2_HInt).w
	move.b	#ObjID_2PResults,(VSResults_HUD+id).w
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	Pal_FadeFromBlack

-	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo_Dynamic_Normal
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	bsr.w	RunPLC_RAM
	tst.l	(Plc_Buffer).w
	bne.s	-
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	andi.b	#button_start_mask,d0
	beq.s	-			; stay on that screen until either player presses start

	move.w	(Results_Screen_2P).w,d0 ; were we at the act results screen? (VsRSID_Act)
	bne.w	TwoPlayerResultsDone_Zone ; if not, branch
	tst.b	(Current_Act).w		; did we just finish act 1?
	bne.s	+			; if not, branch
	addq.b	#1,(Current_Act).w	; go to the next act
	move.b	#1,(Current_Act_2P).w
	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)
	move.b	#0,(Last_star_pole_hit).w
	move.b	#0,(Last_star_pole_hit_2P).w
	moveq	#1,d0
	move.w	d0,(Two_player_mode).w
	move.w	d0,(Two_player_mode_copy).w
	moveq	#0,d0
	move.l	d0,(Score).w
	move.l	d0,(Score_2P).w
	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w
	rts
; ===========================================================================
+	; Displays results for the zone
	move.b	#2,(Current_Act_2P).w
	bsr.w	sub_84A4
	lea	(SS_Total_Won).w,a4
	clr.w	(a4)
	bsr.s	sub_7F9A
	bsr.s	sub_7F9A
	move.b	(a4),d1
	sub.b	1(a4),d1
	beq.s	+		; if there's a tie, branch
	move.w	#VsRSID_Zone,(Results_Screen_2P).w
	move.b	#GameModeID_2PResults,(Game_Mode).w ; => TwoPlayerResults
	rts
; ===========================================================================
+	; There's a tie, play a special stage
	move.b	(Current_Zone_2P).w,d0
	addq.b	#1,d0
	move.b	d0,(Current_Special_Stage).w
	move.w	#VsRSID_SS,(Results_Screen_2P).w
	move.b	#1,(f_bigring).w
	move.b	#GameModeID_SpecialStage,(Game_Mode).w ; => SpecialStage
	moveq	#1,d0
	move.w	d0,(Two_player_mode).w
	move.w	d0,(Two_player_mode_copy).w
	move.b	#0,(Last_star_pole_hit).w
	move.b	#0,(Last_star_pole_hit_2P).w
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_7F9A:
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	beq.s	++
	bcs.s	+
	addq.b	#1,(a4)
	bra.s	++
; ===========================================================================
+
	addq.b	#1,1(a4)
+
	addq.w	#2,a5
	rts
; End of function sub_7F9A

; ===========================================================================

; loc_7FB2:
TwoPlayerResultsDone_Zone:
	subq.w	#1,d0			; were we at the zone results screen? (VsRSID_Zone)
	bne.s	TwoPlayerResultsDone_Game ; if not, branch

; loc_7FB6:
TwoPlayerResultsDone_ZoneOrSpecialStages:
	lea	(Results_Data_2P).w,a4
	moveq	#0,d0
	moveq	#0,d1
    rept 3
	move.w	(a4)+,d0
	add.l	d0,d1
	move.w	(a4)+,d0
	add.l	d0,d1
	addq.w	#2,a4
    endm
	move.w	(a4)+,d0
	add.l	d0,d1
	move.w	(a4)+,d0
	add.l	d0,d1
	swap	d1
	tst.w	d1	; have all levels been completed?
	bne.s	+	; if not, branch
	move.w	#VsRSID_Game,(Results_Screen_2P).w
	move.b	#GameModeID_2PResults,(Game_Mode).w ; => TwoPlayerResults
	rts
; ===========================================================================
+
	tst.w	(Game_Over_2P).w
	beq.s	+		; if there's a Game Over, clear the results
	lea	(Results_Data_2P).w,a1

	moveq	#bytesToWcnt(Results_Data_2P_End-Results_Data_2P),d0
-	move.w	#-1,(a1)+
	dbf	d0,-

	move.b	#3,(Life_count).w
	move.b	#3,(Life_count_2P).w
+
	move.b	#GameModeID_2PLevelSelect,(Game_Mode).w ; => LevelSelectMenu2P
	rts
; ===========================================================================
; loc_8020:
TwoPlayerResultsDone_Game:
	subq.w	#1,d0	; were we at the game results screen? (VsRSID_Game)
	bne.s	TwoPlayerResultsDone_SpecialStage ; if not, branch
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen
	rts
; ===========================================================================
; loc_802C:
TwoPlayerResultsDone_SpecialStage:
	subq.w	#1,d0			; were we at the special stage results screen? (VsRSID_SS)
	bne.w	TwoPlayerResultsDone_SpecialStages ; if not, branch
	cmpi.b	#3,(Current_Zone_2P).w	; do we come from the special stage "zone"?
	beq.s	+			; if yes, branch
	move.w	#VsRSID_Zone,(Results_Screen_2P).w ; show zone results after tiebreaker special stage
	move.b	#GameModeID_2PResults,(Game_Mode).w ; => TwoPlayerResults
	rts
; ===========================================================================
+
	tst.b	(Current_Act_2P).w
	beq.s	+
	cmpi.b	#2,(Current_Act_2P).w
	beq.s	loc_80AC
	bsr.w	sub_84A4
	lea	(SS_Total_Won).w,a4
	clr.w	(a4)
	bsr.s	sub_8094
	bsr.s	sub_8094
	move.b	(a4),d1
	sub.b	1(a4),d1
	bne.s	loc_80AC
+
	addq.b	#1,(Current_Act_2P).w
	addq.b	#1,(Current_Special_Stage).w
	move.w	#VsRSID_SS,(Results_Screen_2P).w
	move.b	#1,(f_bigring).w
	move.b	#GameModeID_SpecialStage,(Game_Mode).w ; => SpecialStage
	move.w	#1,(Two_player_mode).w
	move.w	#0,(Level_Music).w
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_8094:
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	beq.s	++
	bcs.s	+
	addq.b	#1,(a4)
	bra.s	++
; ===========================================================================
+
	addq.b	#1,1(a4)
+
	addq.w	#2,a5
	rts
; End of function sub_8094

; ===========================================================================

loc_80AC:
	move.w	#VsRSID_SSZone,(Results_Screen_2P).w
	move.b	#GameModeID_2PResults,(Game_Mode).w ; => TwoPlayerResults
	rts
; ===========================================================================
; loc_80BA: BranchTo_loc_7FB6:
TwoPlayerResultsDone_SpecialStages:
	; we were at the special stages results screen (VsRSID_SSZone)
	bra.w	TwoPlayerResultsDone_ZoneOrSpecialStages

; ===========================================================================
; ----------------------------------------------------------------------------
; Object 21 - Score/Rings/Time display (in 2P results)
; ----------------------------------------------------------------------------
; Sprite_80BE:
Obj21: ; (screen-space obj)
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj21_Index(pc,d0.w),d1
	jmp	Obj21_Index(pc,d1.w)
; ===========================================================================
; JmpTbl_80CC: Obj21_States:
Obj21_Index:	offsetTable
		offsetTableEntry.w Obj21_Init	; 0
		offsetTableEntry.w Obj21_Main	; 2
; ---------------------------------------------------------------------------
; word_80D0:
Obj21_PositionTable:
	;      x,    y
	dc.w $F0, $148
	dc.w $F0, $130
	dc.w $E0, $148
	dc.w $F0, $148
	dc.w $F0, $148
; ===========================================================================
; loc_80E4:
Obj21_Init:
	addq.b	#2,routine(a0) ; => Obj21_Main
	move.w	(Results_Screen_2P).w,d0
	add.w	d0,d0
	add.w	d0,d0
	move.l	Obj21_PositionTable(pc,d0.w),x_pixel(a0) ; and y_pixel(a0)
	move.l	#Obj21_MapUnc_8146,mappings(a0)
 	move.w	#make_art_tile(ArtTile_ArtNem_1P2PWins,0,0),art_tile(a0)
	jsrto	Adjust2PArtPointer, JmpTo2_Adjust2PArtPointer
	move.b	#0,render_flags(a0)
	move.b	#0,priority(a0)
	moveq	#2,d1
	move.b	(SS_Total_Won).w,d0	; d0 = SS_Total_Won_1P
	sub.b	(SS_Total_Won+1).w,d0	;    - SS_Total_Won_2P
	beq.s	++
	bcs.s	+
	moveq	#0,d1
	bra.s	++
; ---------------------------------------------------------------------------
+
	moveq	#1,d1
+
	move.b	d1,mapping_frame(a0)

; loc_812C:
Obj21_Main:
	andi.w	#tile_mask,art_tile(a0)
	btst	#3,(Vint_runcount+3).w
	beq.s	JmpTo4_DisplaySprite
	ori.w	#palette_line_1,art_tile(a0)

JmpTo4_DisplaySprite ; JmpTo
	jmp	(DisplaySprite).l
; ===========================================================================
; --------------------------------------------------------------------------
; sprite mappings
; --------------------------------------------------------------------------
Obj21_MapUnc_8146:	include "mappings/sprite/obj21.asm"
; ===========================================================================

; loc_819A:
Setup2PResults_Act:
	move.w	#$1F2,d2
	moveq	#0,d0
	bsr.w	sub_8672
	move.w	#$216,d2
	moveq	#0,d1
	move.b	(Current_Act_2P).w,d1
	addq.b	#1,d1
	bsr.w	sub_86B0
	move.w	#$33E,d2
	move.l	(Score).w,d1
	bsr.w	sub_86F6
	move.w	#$352,d2
	move.l	(Score_2P).w,d1
	bsr.w	sub_86F6
	move.w	#$3DA,d2
	moveq	#0,d0
	move.w	(Timer_minute_word).w,d1
	bsr.w	sub_86B0
	move.w	#$3E0,d2
	moveq	#0,d1
	move.b	(Timer_second).w,d1
	bsr.w	sub_86B0
	move.w	#$3E6,d2
	moveq	#0,d1
	move.b	(Timer_frame).w,d1
	mulu.w	#$1B0,d1
	lsr.l	#8,d1
	bsr.w	sub_86B0
	move.w	#$3EE,d2
	moveq	#0,d0
	move.w	(Timer_minute_word_2P).w,d1
	bsr.w	sub_86B0
	move.w	#$3F4,d2
	moveq	#0,d1
	move.b	(Timer_second_2P).w,d1
	bsr.w	sub_86B0
	move.w	#$3FA,d2
	moveq	#0,d1
	move.b	(Timer_frame_2P).w,d1
	mulu.w	#$1B0,d1
	lsr.l	#8,d1
	bsr.w	sub_86B0
	move.w	#$486,d2
	moveq	#0,d0
	move.w	(Ring_count).w,d1
	bsr.w	sub_86B0
	move.w	#$49A,d2
	move.w	(Ring_count_2P).w,d1
	bsr.w	sub_86B0
	move.w	#$526,d2
	moveq	#0,d0
	move.w	(Rings_Collected).w,d1
	bsr.w	sub_86B0
	move.w	#$53A,d2
	move.w	(Rings_Collected_2P).w,d1
	bsr.w	sub_86B0
	move.w	#$5C6,d2
	moveq	#0,d0
	move.w	(Monitors_Broken).w,d1
	bsr.w	sub_86B0
	move.w	#$5DA,d2
	move.w	(Monitors_Broken_2P).w,d1
	bsr.w	sub_86B0
	bsr.w	sub_8476
	move.w	#$364,d2
	move.w	#$6000,d0
	move.l	(Score).w,d1
	sub.l	(Score_2P).w,d1
	bsr.w	sub_8652
	move.w	#$404,d2
	move.l	(Timer_2P).w,d1
	sub.l	(Timer).w,d1
	bsr.w	sub_8652
	move.w	#$4A4,d2
	moveq	#0,d1
	move.w	(Ring_count).w,d1
	sub.w	(Ring_count_2P).w,d1
	bsr.w	sub_8652
	move.w	#$544,d2
	moveq	#0,d1
	move.w	(Rings_Collected).w,d1
	sub.w	(Rings_Collected_2P).w,d1
	bsr.w	sub_8652
	move.w	#$5E4,d2
	moveq	#0,d1
	move.w	(Monitors_Broken).w,d1
	sub.w	(Monitors_Broken_2P).w,d1
	bsr.w	sub_8652
	move.w	#$706,d2
	moveq	#0,d0
	moveq	#0,d1
	move.b	(a4),d1
	bsr.w	sub_86B0
	move.w	#$70E,d2
	moveq	#0,d1
	move.b	1(a4),d1
	bsr.w	sub_86B0
	move.w	(a4),(SS_Total_Won).w
	rts
; ===========================================================================
; loc_82FA:
Setup2PResults_Zone:
	move.w	#$242,d2
	moveq	#0,d0
	bsr.w	sub_8672
	bsr.w	sub_84A4
	lea	(SS_Total_Won).w,a4
	clr.w	(a4)
	move.w	#$398,d6
	bsr.w	sub_854A
	move.w	#$488,d6
	bsr.w	sub_854A
	move.w	#$618,d6
	bsr.w	sub_854A
	rts
; ===========================================================================
; loc_8328:
Setup2PResults_Game:
	lea	(Results_Data_2P).w,a5
	lea	(SS_Total_Won).w,a4
	clr.w	(a4)
	move.w	#$208,d6
	bsr.w	sub_84C4
	move.w	#$258,d6
	bsr.w	sub_84C4
	move.w	#$2A8,d6
	bsr.w	sub_84C4
	move.w	#$348,d6
	bsr.w	sub_84C4
	move.w	#$398,d6
	bsr.w	sub_84C4
	move.w	#$3E8,d6
	bsr.w	sub_84C4
	move.w	#$488,d6
	bsr.w	sub_84C4
	move.w	#$4D8,d6
	bsr.w	sub_84C4
	move.w	#$528,d6
	bsr.w	sub_84C4
	move.w	#$5C8,d6
	bsr.w	sub_84C4
	move.w	#$618,d6
	bsr.w	sub_84C4
	move.w	#$668,d6
	bsr.w	sub_84C4
	move.w	#$70A,d2
	moveq	#0,d0
	moveq	#0,d1
	move.b	(a4),d1
	bsr.w	sub_86B0
	move.w	#$710,d2
	moveq	#0,d1
	move.b	1(a4),d1
	bsr.w	sub_86B0
	rts
; ===========================================================================
; loc_83B0:
Setup2PResults_SpecialAct:
	move.w	#$266,d2
	moveq	#0,d1
	move.b	(Current_Act_2P).w,d1
	addq.b	#1,d1
	bsr.w	sub_86B0
	move.w	#$4D6,d2
	moveq	#0,d0
	move.w	(SS2p_RingBuffer).w,d1		; P1 SS act 1 rings
	bsr.w	sub_86B0
	move.w	#$4E6,d2
	move.w	(SS2p_RingBuffer+2).w,d1	; P2 SS act 1 rings
	bsr.w	sub_86B0
	move.w	#$576,d2
	moveq	#0,d0
	move.w	(SS2p_RingBuffer+4).w,d1	; P1 SS act 2 rings
	bsr.w	sub_86B0
	move.w	#$586,d2
	move.w	(SS2p_RingBuffer+6).w,d1	; P2 SS act 2 rings
	bsr.w	sub_86B0
	move.w	#$616,d2
	moveq	#0,d0
	move.w	(SS2p_RingBuffer+8).w,d1	; P1 SS act 3 rings
	bsr.w	sub_86B0
	move.w	#$626,d2
	move.w	(SS2p_RingBuffer+$A).w,d1	; P2 SS act 3 rings
	bsr.w	sub_86B0
	bsr.w	sub_8476
	move.w	#$6000,d0
	move.w	#$4F0,d2
	moveq	#0,d1
	move.w	(SS2p_RingBuffer).w,d1		; P1 SS act 1 rings
	sub.w	(SS2p_RingBuffer+2).w,d1	; P2 SS act 1 rings
	bsr.w	sub_8652
	move.w	#$590,d2
	moveq	#0,d1
	move.w	(SS2p_RingBuffer+4).w,d1	; P1 SS act 2 rings
	sub.w	(SS2p_RingBuffer+6).w,d1	; P2 SS act 2 rings
	bsr.w	sub_8652
	move.w	#$630,d2
	moveq	#0,d1
	move.w	(SS2p_RingBuffer+8).w,d1	; P1 SS act 3 rings
	sub.w	(SS2p_RingBuffer+$A).w,d1	; P2 SS act 3 rings
	bsr.w	sub_8652
	move.w	(a4),(SS_Total_Won).w
	rts
; ===========================================================================
; loc_8452:
Setup2PResults_SpecialZone:
	bsr.w	sub_84A4
	lea	(SS_Total_Won).w,a4
	clr.w	(a4)
	move.w	#$4D4,d6
	bsr.w	sub_85CE
	move.w	#$574,d6
	bsr.w	sub_85CE
	move.w	#$614,d6
	bsr.w	sub_85CE
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_8476:
	lea	(EHZ_Results_2P).w,a4
	move.b	(Current_Zone_2P).w,d0
	beq.s	+
	lea	(MCZ_Results_2P).w,a4
	subq.b	#1,d0
	beq.s	+
	lea	(CNZ_Results_2P).w,a4
	subq.b	#1,d0
	beq.s	+
	lea	(SS_Results_2P).w,a4
+
	moveq	#0,d0
	move.b	(Current_Act_2P).w,d0
	add.w	d0,d0
	lea	(a4,d0.w),a4
	clr.w	(a4)
	rts
; End of function sub_8476


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_84A4:
	lea	(EHZ_Results_2P).w,a5
	move.b	(Current_Zone_2P).w,d0
	beq.s	+	; rts
	lea	(MCZ_Results_2P).w,a5
	subq.b	#1,d0
	beq.s	+	; rts
	lea	(CNZ_Results_2P).w,a5
	subq.b	#1,d0
	beq.s	+	; rts
	lea	(SS_Results_2P).w,a5
+
	rts
; End of function sub_84A4


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_84C4:
	move.w	(a5),d0
	bmi.s	+
	move.w	d6,d2
	moveq	#0,d0
	moveq	#0,d1
	move.b	(a5),d1
	bsr.w	sub_86B0
	addq.w	#8,d6
	move.w	d6,d2
	moveq	#0,d1
	move.b	1(a5),d1
	bsr.w	sub_86B0
	addi.w	#$12,d6
	move.w	d6,d2
	move.w	#$6000,d0
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	bsr.w	sub_8652
	addq.w	#2,a5
	rts
; ===========================================================================
+
	addq.w	#4,d6
	not.w	d0
	bne.s	+
	lea	(Text2P_NoGame).l,a1
	move.w	d6,d2
	bsr.w	loc_8698
	addi.w	#$16,d6
	move.w	d6,d2
	lea	(Text2P_Blank).l,a1
	bsr.w	loc_8698
	addq.w	#2,a5
	rts
; ===========================================================================
+
	moveq	#0,d0
	lea	(Text2P_GameOver).l,a1
	move.w	d6,d2
	bsr.w	loc_8698
	addi.w	#$16,d6
	move.w	d6,d2
	move.w	#$6000,d0
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	bsr.w	sub_8652
	addq.w	#2,a5
	rts
; End of function sub_84C4


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_854A:
	move.w	(a5),d0
	bmi.s	loc_8582
	move.w	d6,d2
	moveq	#0,d0
	moveq	#0,d1
	move.b	(a5),d1
	bsr.w	sub_86B0
	addq.w	#8,d6
	move.w	d6,d2
	moveq	#0,d1
	move.b	1(a5),d1
	bsr.w	sub_86B0
	addi.w	#$C,d6
	move.w	d6,d2
	move.w	#$6000,d0
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	bsr.w	sub_8652
	addq.w	#2,a5
	rts
; ===========================================================================

loc_8582:
	not.w	d0
	bne.s	loc_85A6
	lea	(Text2P_NoGame).l,a1
	move.w	d6,d2
	bsr.w	loc_8698
	addi.w	#$14,d6
	move.w	d6,d2
	lea	(Text2P_Blank).l,a1
	bsr.w	loc_8698
	addq.w	#2,a5
	rts
; ===========================================================================

loc_85A6:
	moveq	#0,d0
	lea	(Text2P_GameOver).l,a1
	move.w	d6,d2
	bsr.w	loc_8698
	addi.w	#$14,d6
	move.w	d6,d2
	move.w	#$6000,d0
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	bsr.w	sub_8652
	addq.w	#2,a5
	rts
; End of function sub_854A


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_85CE:
	move.w	(a5),d0
	bmi.s	+
	move.w	d6,d2
	moveq	#0,d0
	moveq	#0,d1
	move.b	(a5),d1
	bsr.w	sub_86B0
	addi.w	#$C,d6
	move.w	d6,d2
	moveq	#0,d1
	move.b	1(a5),d1
	bsr.w	sub_86B0
	addi.w	#$10,d6
	move.w	d6,d2
	move.w	#$6000,d0
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	bsr.w	sub_8652
	addq.w	#2,a5
	rts
; ===========================================================================
+
	not.w	d0
	bne.s	loc_862C
	lea	(Text2P_NoGame).l,a1
	move.w	d6,d2
	addq.w	#4,d2
	bsr.w	loc_8698
	addi.w	#$14,d6
	move.w	d6,d2
	lea	(Text2P_Blank).l,a1
	bsr.s	loc_8698
	addq.w	#2,a5
	rts
; ===========================================================================

loc_862C:
	moveq	#0,d0
	lea	(Text2P_GameOver).l,a1
	move.w	d6,d2
	bsr.s	loc_8698
	addi.w	#$14,d6
	move.w	d6,d2
	move.w	#$6000,d0
	moveq	#0,d1
	move.b	(a5),d1
	sub.b	1(a5),d1
	bsr.w	sub_8652
	addq.w	#2,a5
	rts
; End of function sub_85CE


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_8652:
	lea	(Text2P_Tied).l,a1
	beq.s	++
	bcs.s	+
	lea	(Text2P_1P).l,a1
	addq.b	#1,(a4)
	bra.s	++
; ===========================================================================
+
	lea	(Text2P_2P).l,a1
	addq.b	#1,1(a4)
+
	bra.s	loc_8698
; End of function sub_8652


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_8672:
	lea	(Text2P_EmeraldHill).l,a1
	move.b	(Current_Zone_2P).w,d1
	beq.s	loc_8698
	lea	(Text2P_MysticCave).l,a1
	subq.b	#1,d1
	beq.s	loc_8698
	lea	(Text2P_CasinoNight).l,a1
	subq.b	#1,d1
	beq.s	loc_8698
	lea	(Text2P_SpecialStage).l,a1

loc_8698:
	lea	(Chunk_Table).l,a2
	lea	(a2,d2.w),a2
	moveq	#0,d1

	move.b	(a1)+,d1
-	move.b	(a1)+,d0
	move.w	d0,(a2)+
	dbf	d1,-

	rts
; End of function sub_8672


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_86B0:
	lea	(Chunk_Table).l,a2
	lea	(a2,d2.w),a2
	lea	(word_86F0).l,a3
	moveq	#0,d2

	moveq	#2,d5
-	moveq	#0,d3
	move.w	(a3)+,d4

-	sub.w	d4,d1
	bcs.s	+
	addq.w	#1,d3
	bra.s	-
; ---------------------------------------------------------------------------
+
	add.w	d4,d1
	tst.w	d5
	beq.s	++
	tst.w	d3
	beq.s	+
	moveq	#1,d2
+
	tst.w	d2
	beq.s	++
+
	addi.b	#$10,d3
	move.b	d3,d0
	move.w	d0,(a2)
+
	addq.w	#2,a2
	dbf	d5,--

	rts
; End of function sub_86B0

; ===========================================================================
word_86F0:
	dc.w   100
	dc.w	10	; 1
	dc.w	 1	; 2

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_86F6:
	lea	(Chunk_Table).l,a2
	lea	(a2,d2.w),a2
	lea	(dword_8732).l,a3
	moveq	#0,d2

	moveq	#5,d5
-	moveq	#0,d3
	move.l	(a3)+,d4

-	sub.l	d4,d1
	bcs.s	+
	addq.w	#1,d3
	bra.s	-
; ===========================================================================
+
	add.l	d4,d1
	tst.w	d3
	beq.s	+
	moveq	#1,d2
+
	tst.w	d2
	beq.s	+
	addi.b	#$10,d3
	move.b	d3,d0
	move.w	d0,(a2)
+
	addq.w	#2,a2
	dbf	d5,--

	rts
; End of function sub_86F6

; ===========================================================================
dword_8732:
	dc.l 100000
	dc.l  10000
	dc.l   1000
	dc.l    100
	dc.l     10
	dc.l      1

	; set the character set for menu text
	charset '@',"\27\30\31\32\33\34\35\36\37\38\39\40\41\42\43\44\45\46\47\48\49\50\51\52\53\54\55"
	charset '0',"\16\17\18\19\20\21\22\23\24\25"
	charset '*',$1A
	charset ':',$1C
	charset '.',$1D
	charset ' ',0

	; Menu text
Text2P_EmeraldHill:	menutxt	"EMERALD HILL"	; byte_874A:
	rev02even
Text2P_MysticCave:	menutxt	" MYSTIC CAVE"	; byte_8757:
	rev02even
Text2P_CasinoNight:	menutxt	"CASINO NIGHT"	; byte_8764:
	rev02even
Text2P_SpecialStage:	menutxt	"SPECIAL STAGE"	; byte_8771:
	rev02even
Text2P_Special:		menutxt	"   SPECIAL  "	; byte_877F:
	rev02even
Text2P_Zone:		menutxt	"ZONE "		; byte_878C:
	rev02even
Text2P_Stage:		menutxt	"STAGE"		; byte_8792:
	rev02even
Text2P_GameOver:	menutxt	"GAME OVER"	; byte_8798:
	rev02even
Text2P_TimeOver:	menutxt	"TIME OVER"
	rev02even
Text2P_NoGame:		menutxt	"NO GAME"	; byte_87AC:
	rev02even
Text2P_Tied:		menutxt	"TIED"		; byte_87B4:
	rev02even
Text2P_1P:		menutxt	" 1P"		; byte_87B9:
	rev02even
Text2P_2P:		menutxt	" 2P"		; byte_87BD:
	rev02even
Text2P_Blank:		menutxt	"    "		; byte_87C1:
	rev02even

	charset ; reset character set

; ------------------------------------------------------------------------
; MENU ANIMATION SCRIPT
; ------------------------------------------------------------------------
;word_87C6:
Anim_SonicMilesBG:	zoneanimstart
	; Sonic/Miles animated background
	zoneanimdecl  -1, ArtUnc_MenuBack,    1,  6, $A
	dc.b   0,$C7
	dc.b  $A,  5
	dc.b $14,  5
	dc.b $1E,$C7
	dc.b $14,  5
	dc.b  $A,  5
	even

	zoneanimend

; off_87DC:
TwoPlayerResultsPointers:
VsResultsScreen_Act:	dc.l Map_2PActResults, Setup2PResults_Act
VsResultsScreen_Zone:	dc.l Map_2PZoneResults, Setup2PResults_Zone
VsResultsScreen_Game:	dc.l Map_2PGameResults, Setup2PResults_Game
VsResultsScreen_SS:	dc.l Map_2PSpecialStageActResults, Setup2PResults_SpecialAct
VsResultsScreen_SSZone:	dc.l Map_2PSpecialStageZoneResults, Setup2PResults_SpecialZone

; 2P single act results screen (enigma compressed)
; byte_8804:
Map_2PActResults:	BINCLUDE "mappings/misc/2P Act Results.eni"

; 2P zone results screen (enigma compressed)
; byte_88CE:
Map_2PZoneResults:	BINCLUDE "mappings/misc/2P Zone Results.eni"

; 2P game results screen (after all 4 zones) (enigma compressed)
; byte_8960:
Map_2PGameResults:	BINCLUDE "mappings/misc/2P Game Results.eni"

; 2P special stage act results screen (enigma compressed)
; byte_8AA4:
Map_2PSpecialStageActResults:	BINCLUDE "mappings/misc/2P Special Stage Act Results.eni"

; 2P special stage zone results screen (enigma compressed)
; byte_8B30:
Map_2PSpecialStageZoneResults:	BINCLUDE "mappings/misc/2P Special Stage Zone Results.eni"

	even

    if ~~removeJmpTos
JmpTo2_Adjust2PArtPointer ; JmpTo
	jmp	(Adjust2PArtPointer).l
JmpTo_Dynamic_Normal ; JmpTo
	jmp	(Dynamic_Normal).l

	align 4
    endif




; ===========================================================================
; loc_8BD4:
MenuScreen:
	bsr.w	Pal_FadeToBlack
	move	#$2700,sr
	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	ClearScreen
	lea	(VDP_control_port).l,a6
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8200|(VRAM_Menu_Plane_A_Name_Table/$400),(a6)		; PNT A base: $C000
	move.w	#$8400|(VRAM_Menu_Plane_B_Name_Table/$2000),(a6)	; PNT B base: $E000
	move.w	#$8200|(VRAM_Menu_Plane_A_Name_Table/$400),(a6)		; PNT A base: $C000
	move.w	#$8700,(a6)		; Background palette/color: 0/0
	move.w	#$8C81,(a6)		; H res 40 cells, no interlace, S/H disabled
	move.w	#$9001,(a6)		; Scroll table size: 64x32

	clearRAM Object_Display_Lists,Object_Display_Lists_End
	clearRAM Object_RAM,Object_RAM_End

	; load background + graphics of font/LevSelPics
	clr.w	(VDP_Command_Buffer).w
	move.l	#VDP_Command_Buffer,(VDP_Command_Buffer_Slot).w
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_FontStuff),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_FontStuff).l,a0
	bsr.w	NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_MenuBox),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_MenuBox).l,a0
	bsr.w	NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_LevelSelectPics),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_LevelSelectPics).l,a0
	bsr.w	NemDec
	lea	(Chunk_Table).l,a1
	lea	(MapEng_MenuBack).l,a0
	move.w	#make_art_tile(ArtTile_VRAM_Start,3,0),d0
	bsr.w	EniDec
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_Plane_B_Name_Table,VRAM,WRITE),d0
	moveq	#40-1,d1
	moveq	#28-1,d2
	jsrto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40	; fullscreen background

	cmpi.b	#GameModeID_OptionsMenu,(Game_Mode).w	; options menu?
	beq.w	MenuScreen_Options	; if yes, branch

	cmpi.b	#GameModeID_LevelSelect,(Game_Mode).w	; level select menu?
	beq.w	MenuScreen_LevelSelect	; if yes, branch

;MenuScreen_LevSel2P:
	lea	(Chunk_Table).l,a1
	lea	(MapEng_LevSel2P).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_MenuBox,0,0),d0
	bsr.w	EniDec
	lea	(Chunk_Table+$198).l,a1
	lea	(MapEng_LevSel2P).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_MenuBox,1,0),d0
	bsr.w	EniDec
	lea	(Chunk_Table+$330).l,a1
	lea	(MapEng_LevSelIcon).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_LevelSelectPics,0,0),d0
	bsr.w	EniDec
	lea	(Chunk_Table+$498).l,a2

	moveq	#bytesToWcnt(tiles_to_bytes(1)),d1
-	move.w	#make_art_tile(ArtTile_ArtNem_MenuBox+11,1,0),(a2)+
	dbf	d1,-

	bsr.w	Update2PLevSelSelection
	addq.b	#1,(Current_Zone_2P).w
	andi.b	#3,(Current_Zone_2P).w
	bsr.w	ClearOld2PLevSelSelection
	addq.b	#1,(Current_Zone_2P).w
	andi.b	#3,(Current_Zone_2P).w
	bsr.w	ClearOld2PLevSelSelection
	addq.b	#1,(Current_Zone_2P).w
	andi.b	#3,(Current_Zone_2P).w
	bsr.w	ClearOld2PLevSelSelection
	addq.b	#1,(Current_Zone_2P).w
	andi.b	#3,(Current_Zone_2P).w
	clr.w	(Player_mode).w
	clr.b	(Current_Act_2P).w
	clr.w	(Results_Screen_2P).w	; VsRSID_Act
	clr.b	(Level_started_flag).w
	clr.w	(Anim_Counters).w
	clr.w	(Game_Over_2P).w
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo2_Dynamic_Normal
	moveq	#PalID_Menu,d0
	bsr.w	PalLoad_ForFade
	lea	(Normal_palette_line3).w,a1
	lea	(Target_palette_line3).w,a2

	moveq	#bytesToLcnt(tiles_to_bytes(1)),d1
-	move.l	(a1),(a2)+
	clr.l	(a1)+
	dbf	d1,-

	move.b	#MusID_Options,d0
	jsrto	PlayMusic, JmpTo_PlayMusic
	move.w	#(30*60)-1,(Demo_Time_left).w	; 30 seconds
	clr.w	(Two_player_mode).w
	clr.l	(Camera_X_pos).w
	clr.l	(Camera_Y_pos).w
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	Pal_FadeFromBlack

;loc_8DA8:
LevelSelect2P_Main:
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	move	#$2700,sr
	bsr.w	ClearOld2PLevSelSelection
	bsr.w	LevelSelect2P_Controls
	bsr.w	Update2PLevSelSelection
	move	#$2300,sr
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo2_Dynamic_Normal
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	andi.b	#button_start_mask,d0
	bne.s	LevelSelect2P_PressStart
	bra.w	LevelSelect2P_Main
; ===========================================================================
;loc_8DE2:
LevelSelect2P_PressStart:
	bsr.w	Chk2PZoneCompletion
	bmi.s	loc_8DF4
	move.w	#SndID_Error,d0
	jsrto	PlaySound, JmpTo_PlaySound
	bra.w	LevelSelect2P_Main
; ===========================================================================

loc_8DF4:
	moveq	#0,d0
	move.b	(Current_Zone_2P).w,d0
	add.w	d0,d0
	move.w	LevelSelect2P_LevelOrder(pc,d0.w),d0
	bmi.s	loc_8E3A
	move.w	d0,(Current_ZoneAndAct).w
	move.w	#1,(Two_player_mode).w
	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)
	move.b	#0,(Last_star_pole_hit).w
	move.b	#0,(Last_star_pole_hit_2P).w
	moveq	#0,d0
	move.l	d0,(Score).w
	move.l	d0,(Score_2P).w
	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w
	rts
; ===========================================================================

loc_8E3A:
	move.b	#4,(Current_Special_Stage).w
	move.b	#GameModeID_SpecialStage,(Game_Mode).w ; => SpecialStage
	moveq	#1,d0
	move.w	d0,(Two_player_mode).w
	move.w	d0,(Two_player_mode_copy).w
	rts
; ===========================================================================
; word_8E52:
LevelSelect2P_LevelOrder:
	dc.w	emerald_hill_zone_act_1
	dc.w	mystic_cave_zone_act_1
	dc.w	casino_night_zone_act_1
	dc.w	$FFFF

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_8E5A:
LevelSelect2P_Controls:
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	move.b	d0,d1
	andi.b	#button_up_mask|button_down_mask,d0
	beq.s	+
	bchg	#1,(Current_Zone_2P).w

+
	andi.b	#button_left_mask|button_right_mask,d1
	beq.s	+	; rts
	bchg	#0,(Current_Zone_2P).w
+
	rts
; End of function LevelSelect2P_Controls

; ---------------------------------------------------------------------------
; Subroutine to update the 2P level select selection graphically
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_8E7E:
Update2PLevSelSelection:
	moveq	#0,d0
	move.b	(Current_Zone_2P).w,d0
	lsl.w	#4,d0	; 16 bytes per entry
	lea	(LevSel2PIconData).l,a3
	lea	(a3,d0.w),a3
	move.w	#palette_line_3,d0	; highlight text
	lea	(Chunk_Table+$48).l,a2
	movea.l	(a3)+,a1
	bsr.w	MenuScreenTextToRAM
	lea	(Chunk_Table+$94).l,a2
	movea.l	(a3)+,a1
	bsr.w	MenuScreenTextToRAM
	lea	(Chunk_Table+$D8).l,a2
	movea.l	4(a3),a1
	bsr.w	Chk2PZoneCompletion	; has the zone been completed?
	bmi.s	+	; if not, branch
	lea	(Chunk_Table+$468).l,a1	; display large X instead of icon
+
	moveq	#2,d1
-	move.l	(a1)+,(a2)+
	move.l	(a1)+,(a2)+
	lea	$1A(a2),a2
	dbf	d1,-

	lea	(Chunk_Table).l,a1
	move.l	(a3)+,d0
	moveq	#17-1,d1
	moveq	#12-1,d2
	jsrto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40
	lea	(Pal_LevelIcons).l,a1
	moveq	#0,d0
	move.b	(a3),d0
	lsl.w	#5,d0
	lea	(a1,d0.w),a1
	lea	(Normal_palette_line3).w,a2

	moveq	#bytesToLcnt(palette_line_size),d1
-	move.l	(a1)+,(a2)+
	dbf	d1,-

	rts
; End of function Update2PLevSelSelection

; ---------------------------------------------------------------------------
; Subroutine to check if a 2P zone has been completed
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_8EFE:
Chk2PZoneCompletion:
	moveq	#0,d0
	move.b	(Current_Zone_2P).w,d0
	; multiply d0 by 6
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	add.w	d0,d0
	lea	(Results_Data_2P).w,a5
	lea	(a5,d0.w),a5
	move.w	(a5),d0
	add.w	2(a5),d0
	rts
; End of function Chk2PZoneCompletion

; ---------------------------------------------------------------------------
; Subroutine to clear the old 2P level select selection
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_8F1C:
ClearOld2PLevSelSelection:
	moveq	#0,d0
	move.b	(Current_Zone_2P).w,d0
	lsl.w	#4,d0
	lea	(LevSel2PIconData).l,a3
	lea	(a3,d0.w),a3
	moveq	#palette_line_0,d0
	lea	(Chunk_Table+$1E0).l,a2
	movea.l	(a3)+,a1
	bsr.w	MenuScreenTextToRAM
	lea	(Chunk_Table+$22C).l,a2
	movea.l	(a3)+,a1
	bsr.w	MenuScreenTextToRAM
	lea	(Chunk_Table+$270).l,a2
	lea	(Chunk_Table+$498).l,a1
	bsr.w	Chk2PZoneCompletion
	bmi.s	+
	lea	(Chunk_Table+$468).l,a1
+
	moveq	#2,d1
-	move.l	(a1)+,(a2)+
	move.l	(a1)+,(a2)+
	lea	$1A(a2),a2
	dbf	d1,-

	lea	(Chunk_Table+$198).l,a1
	move.l	(a3)+,d0
	moveq	#17-1,d1
	moveq	#12-1,d2
	jmpto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40
; End of function ClearOld2PLevSelSelection

; ===========================================================================
; off_8F7E:
LevSel2PIconData:

; macro to declare icon data for a 2P level select icon
iconData macro txtlabel,txtlabel2,vramAddr,iconPal,iconAddr
	dc.l txtlabel, txtlabel2	; text locations
	dc.l vdpComm(vramAddr,VRAM,WRITE)	; VRAM location to place data
	dc.l iconPal<<24|((iconAddr)&$FFFFFF)	; icon palette and plane data location
    endm

	iconData	Text2P_EmeraldHill,Text2P_Zone, VRAM_Plane_A_Name_Table+planeLoc(64,2,2),   0,Chunk_Table+$330
	iconData	Text2P_MysticCave, Text2P_Zone, VRAM_Plane_A_Name_Table+planeLoc(64,22,2),  5,Chunk_Table+$3A8
	iconData	Text2P_CasinoNight,Text2P_Zone, VRAM_Plane_A_Name_Table+planeLoc(64,2,15),  6,Chunk_Table+$3C0
	iconData	Text2P_Special,    Text2P_Stage,VRAM_Plane_A_Name_Table+planeLoc(64,22,15),12,Chunk_Table+$450

; ---------------------------------------------------------------------------
; Common menu screen subroutine for transferring text to RAM

; ARGUMENTS:
; d0 = starting art tile
; a1 = data source
; a2 = destination
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_8FBE:
MenuScreenTextToRAM:
	moveq	#0,d1
	move.b	(a1)+,d1
-	move.b	(a1)+,d0
	move.w	d0,(a2)+
	dbf	d1,-
	rts
; End of function MenuScreenTextToRAM

; ===========================================================================
; loc_8FCC:
MenuScreen_Options:
	lea	(Chunk_Table).l,a1
	lea	(MapEng_Options).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_MenuBox,0,0),d0
	bsr.w	EniDec
	lea	(Chunk_Table+$160).l,a1
	lea	(MapEng_Options).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_MenuBox,1,0),d0
	bsr.w	EniDec
	clr.b	(Options_menu_box).w
	bsr.w	OptionScreen_DrawSelected
	addq.b	#1,(Options_menu_box).w
	bsr.w	OptionScreen_DrawUnselected
	addq.b	#1,(Options_menu_box).w
	bsr.w	OptionScreen_DrawUnselected
	clr.b	(Options_menu_box).w
	clr.b	(Level_started_flag).w
	clr.w	(Anim_Counters).w
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo2_Dynamic_Normal
	moveq	#PalID_Menu,d0
	bsr.w	PalLoad_ForFade
	move.b	#MusID_Options,d0
	jsrto	PlayMusic, JmpTo_PlayMusic
	clr.w	(Two_player_mode).w
	clr.l	(Camera_X_pos).w
	clr.l	(Camera_Y_pos).w
	clr.w	(Correct_cheat_entries).w
	clr.w	(Correct_cheat_entries_2).w
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
	bsr.w	Pal_FadeFromBlack
; loc_9060:
OptionScreen_Main:
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint
	move	#$2700,sr
	bsr.w	OptionScreen_DrawUnselected
	bsr.w	OptionScreen_Controls
	bsr.w	OptionScreen_DrawSelected
	move	#$2300,sr
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo2_Dynamic_Normal
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	andi.b	#button_start_mask,d0
	bne.s	OptionScreen_Select
	bra.w	OptionScreen_Main
; ===========================================================================
; loc_909A:
OptionScreen_Select:
	move.b	(Options_menu_box).w,d0
	bne.s	OptionScreen_Select_Not1P
	; Start a single player game
	moveq	#0,d0
	move.w	d0,(Two_player_mode).w
	move.w	d0,(Two_player_mode_copy).w
    if emerald_hill_zone_act_1=0
	move.w	d0,(Current_ZoneAndAct).w ; emerald_hill_zone_act_1
    else
	move.w	#emerald_hill_zone_act_1,(Current_ZoneAndAct).w
    endif
    if fixBugs
	; The game forgets to reset these variables here, making it possible
	; for the player to repeatedly soft-reset and play Emerald Hill Zone
	; over and over again, collecting all of the emeralds within the
	; first act. This code is borrowed from similar logic in the title
	; screen, which doesn't make this mistake.
	move.w	d0,(Current_Special_StageAndAct).w
	move.w	d0,(Got_Emerald).w
	move.l	d0,(Got_Emeralds_array).w
	move.l	d0,(Got_Emeralds_array+4).w
    endif
	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)
	rts
; ===========================================================================
; loc_90B6:
OptionScreen_Select_Not1P:
	subq.b	#1,d0
	bne.s	OptionScreen_Select_Other
	; Start a 2P VS game
	moveq	#1,d0
	move.w	d0,(Two_player_mode).w
	move.w	d0,(Two_player_mode_copy).w
    if fixBugs
	; The game forgets to reset these variables here, making it possible
	; for the player to play two player mode with all emeralds collected,
	; allowing them to use Super Sonic. This code is borrowed from
	; similar logic in the title screen, which doesn't make this mistake.
	moveq	#0,d0
	move.w	d0,(Got_Emerald).w
	move.l	d0,(Got_Emeralds_array).w
	move.l	d0,(Got_Emeralds_array+4).w
    endif
	move.b	#GameModeID_2PLevelSelect,(Game_Mode).w ; => LevelSelectMenu2P
	move.b	#0,(Current_Zone_2P).w
	move.w	#0,(Player_mode).w
	rts
; ===========================================================================
; loc_90D8:
OptionScreen_Select_Other:
	; When pressing START on the sound test option, return to the SEGA screen
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_90E0:
OptionScreen_Controls:
	moveq	#0,d2
	move.b	(Options_menu_box).w,d2
	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	btst	#button_up,d0
	beq.s	+
	subq.b	#1,d2
	bcc.s	+
	move.b	#2,d2

+
	btst	#button_down,d0
	beq.s	+
	addq.b	#1,d2
	cmpi.b	#3,d2
	blo.s	+
	moveq	#0,d2

+
	move.b	d2,(Options_menu_box).w
	lsl.w	#2,d2
	move.b	OptionScreen_Choices(pc,d2.w),d3 ; number of choices for the option
	movea.l	OptionScreen_Choices(pc,d2.w),a1 ; location where the choice is stored (in RAM)
	move.w	(a1),d2
	btst	#button_left,d0
	beq.s	+
	subq.b	#1,d2
	bcc.s	+
	move.b	d3,d2

+
	btst	#button_right,d0
	beq.s	+
	addq.b	#1,d2
	cmp.b	d3,d2
	bls.s	+
	moveq	#0,d2

+
    if fixBugs
	; Based on code from the Level Select.
	cmpi.b	#2,(Options_menu_box).w
	bne.s	+
	btst	#button_A,d0
	beq.s	+
	addi.b	#$10,d2
	andi.b	#$7F,d2
    else
	; This code appears to have been carelessly created from a copy of the
	; above block of code. It makes no sense to advance by $10 on options
	; that have only 2 or 3 values. Likewise, the logic for setting the
	; value to 0 when exceeding the maximum bound only makes sense for
	; incrementing by 1, not $10.
	btst	#button_A,d0
	beq.s	+
	addi.b	#$10,d2
	cmp.b	d3,d2
	bls.s	+
	moveq	#0,d2
    endif

+
	move.w	d2,(a1)
	cmpi.b	#2,(Options_menu_box).w
	bne.s	+	; rts
	andi.w	#button_B_mask|button_C_mask,d0
	beq.s	+	; rts
	move.w	(Sound_test_sound).w,d0
	addi.w	#$80,d0
	jsrto	PlayMusic, JmpTo_PlayMusic
	lea	(level_select_cheat).l,a0
	lea	(continues_cheat).l,a2
	lea	(Level_select_flag).w,a1	; Also Slow_motion_flag
	moveq	#0,d2	; flag to tell the routine to enable the continues cheat
	bsr.w	CheckCheats

+
	rts
; End of function OptionScreen_Controls

; ===========================================================================
; word_917A:
OptionScreen_Choices:
	dc.l (3-1)<<24|(Player_option&$FFFFFF)
	dc.l (2-1)<<24|(Two_player_items&$FFFFFF)
	dc.l ($80-1)<<24|(Sound_test_sound&$FFFFFF)

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_9186
OptionScreen_DrawSelected:
	bsr.w	OptionScreen_SelectTextPtr
	moveq	#0,d1
	move.b	(Options_menu_box).w,d1
	lsl.w	#3,d1
	lea	(OptScrBoxData).l,a3
	lea	(a3,d1.w),a3
	move.w	#palette_line_3,d0
	lea	(Chunk_Table+$30).l,a2
	movea.l	(a3)+,a1
	bsr.w	MenuScreenTextToRAM
	lea	(Chunk_Table+$B6).l,a2
	moveq	#0,d1
	cmpi.b	#2,(Options_menu_box).w
	beq.s	+
	move.b	(Options_menu_box).w,d1
	lsl.w	#2,d1
	lea	OptionScreen_Choices(pc),a1
	movea.l	(a1,d1.w),a1
	move.w	(a1),d1
	lsl.w	#2,d1
+
	movea.l	(a4,d1.w),a1
	bsr.w	MenuScreenTextToRAM
	cmpi.b	#2,(Options_menu_box).w
	bne.s	+
	lea	(Chunk_Table+$C2).l,a2
	bsr.w	OptionScreen_HexDumpSoundTest
+
	lea	(Chunk_Table).l,a1
	move.l	(a3)+,d0
	moveq	#22-1,d1
	moveq	#8-1,d2
	jmpto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40
; ===========================================================================

;loc_91F8
OptionScreen_DrawUnselected:
	bsr.w	OptionScreen_SelectTextPtr
	moveq	#0,d1
	move.b	(Options_menu_box).w,d1
	lsl.w	#3,d1
	lea	(OptScrBoxData).l,a3
	lea	(a3,d1.w),a3
	moveq	#palette_line_0,d0
	lea	(Chunk_Table+$190).l,a2
	movea.l	(a3)+,a1
	bsr.w	MenuScreenTextToRAM
	lea	(Chunk_Table+$216).l,a2
	moveq	#0,d1
	cmpi.b	#2,(Options_menu_box).w
	beq.s	+
	move.b	(Options_menu_box).w,d1
	lsl.w	#2,d1
	lea	OptionScreen_Choices(pc),a1
	movea.l	(a1,d1.w),a1
	move.w	(a1),d1
	lsl.w	#2,d1

+
	movea.l	(a4,d1.w),a1
	bsr.w	MenuScreenTextToRAM
	cmpi.b	#2,(Options_menu_box).w
	bne.s	+
	lea	(Chunk_Table+$222).l,a2
	bsr.w	OptionScreen_HexDumpSoundTest

+
	lea	(Chunk_Table+$160).l,a1
	move.l	(a3)+,d0
	moveq	#22-1,d1
	moveq	#8-1,d2
	jmpto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40
; ===========================================================================

;loc_9268
OptionScreen_SelectTextPtr:
	lea	(off_92D2).l,a4
	tst.b	(Graphics_Flags).w
	bpl.s	+
	lea	(off_92DE).l,a4

+
	tst.b	(Options_menu_box).w
	beq.s	+
	lea	(off_92EA).l,a4

+
	cmpi.b	#2,(Options_menu_box).w
	bne.s	+	; rts
	lea	(off_92F2).l,a4

+
	rts
; ===========================================================================

;loc_9296
OptionScreen_HexDumpSoundTest:
	move.w	(Sound_test_sound).w,d1
	move.b	d1,d2
	lsr.b	#4,d1
	bsr.s	+
	move.b	d2,d1

+
	andi.w	#$F,d1
	cmpi.b	#$A,d1
	blo.s	+
	addi.b	#4,d1

+
	addi.b	#$10,d1
	move.b	d1,d0
	move.w	d0,(a2)+
	rts
; ===========================================================================
; off_92BA:
OptScrBoxData:

; macro to declare the data for an options screen box
boxData macro txtlabel,vramAddr
	dc.l txtlabel, vdpComm(vramAddr,VRAM,WRITE)
    endm

	boxData	TextOptScr_PlayerSelect,VRAM_Plane_A_Name_Table+planeLoc(64,9,3)
	boxData	TextOptScr_VsModeItems,VRAM_Plane_A_Name_Table+planeLoc(64,9,11)
	boxData	TextOptScr_SoundTest,VRAM_Plane_A_Name_Table+planeLoc(64,9,19)

off_92D2:
	dc.l TextOptScr_SonicAndMiles
	dc.l TextOptScr_SonicAlone
	dc.l TextOptScr_MilesAlone
off_92DE:
	dc.l TextOptScr_SonicAndTails
	dc.l TextOptScr_SonicAlone
	dc.l TextOptScr_TailsAlone
off_92EA:
	dc.l TextOptScr_AllKindsItems
	dc.l TextOptScr_TeleportOnly
off_92F2:
	dc.l TextOptScr_0
; ===========================================================================
; loc_92F6:
MenuScreen_LevelSelect:
	; Load foreground (sans zone icon)
	lea	(Chunk_Table).l,a1
	lea	(MapEng_LevSel).l,a0	; 2 bytes per 8x8 tile, compressed
	move.w	#make_art_tile(ArtTile_VRAM_Start,0,0),d0
	bsr.w	EniDec

	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_Plane_A_Name_Table,VRAM,WRITE),d0
	moveq	#40-1,d1
	moveq	#28-1,d2	; 40x28 = whole screen
	jsrto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40	; display patterns

	; Draw sound test number
	moveq	#palette_line_0,d3
	bsr.w	LevelSelect_DrawSoundNumber

	; Load zone icon
	lea	(Chunk_Table+planeLoc(40,0,28)).l,a1
	lea	(MapEng_LevSelIcon).l,a0
	move.w	#make_art_tile(ArtTile_ArtNem_LevelSelectPics,0,0),d0
	bsr.w	EniDec

	bsr.w	LevelSelect_DrawIcon

	clr.w	(Player_mode).w
	clr.w	(Results_Screen_2P).w	; VsRSID_Act
	clr.b	(Level_started_flag).w
	clr.w	(Anim_Counters).w

	; Animate background (loaded back in MenuScreen)
	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo2_Dynamic_Normal	; background

	moveq	#PalID_Menu,d0
	bsr.w	PalLoad_ForFade

	lea	(Normal_palette_line3).w,a1
	lea	(Target_palette_line3).w,a2

	moveq	#bytesToLcnt(palette_line_size),d1
-	move.l	(a1),(a2)+
	clr.l	(a1)+
	dbf	d1,-

	move.b	#MusID_Options,d0
	jsrto	PlayMusic, JmpTo_PlayMusic

	move.w	#(30*60)-1,(Demo_Time_left).w	; 30 seconds
	clr.w	(Two_player_mode).w
	clr.l	(Camera_X_pos).w
	clr.l	(Camera_Y_pos).w
	clr.w	(Correct_cheat_entries).w
	clr.w	(Correct_cheat_entries_2).w

	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint

	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l

	bsr.w	Pal_FadeFromBlack

;loc_93AC:
LevelSelect_Main:	; routine running during level select
	move.b	#VintID_Menu,(Vint_routine).w
	bsr.w	WaitForVint

	move	#$2700,sr

	moveq	#palette_line_0,d3
	bsr.w	LevelSelect_MarkFields	; unmark fields
	bsr.w	LevSelControls		; possible change selected fields
	move.w	#palette_line_3,d3
	bsr.w	LevelSelect_MarkFields	; mark fields

	bsr.w	LevelSelect_DrawIcon

	move	#$2300,sr

	lea	(Anim_SonicMilesBG).l,a2
	jsrto	Dynamic_Normal, JmpTo2_Dynamic_Normal

	move.b	(Ctrl_1_Press).w,d0
	or.b	(Ctrl_2_Press).w,d0
	andi.b	#button_start_mask,d0	; start pressed?
	bne.s	LevelSelect_PressStart	; yes
	bra.w	LevelSelect_Main	; no
; ===========================================================================

;loc_93F0:
LevelSelect_PressStart:
	move.w	(Level_select_zone).w,d0
	add.w	d0,d0
	move.w	LevelSelect_Order(pc,d0.w),d0
	bmi.w	LevelSelect_Return	; sound test
	cmpi.w	#$4000,d0
	bne.s	LevelSelect_StartZone

;LevelSelect_SpecialStage:
	move.b	#GameModeID_SpecialStage,(Game_Mode).w ; => SpecialStage
    if emerald_hill_zone_act_1=0
	clr.w	(Current_ZoneAndAct).w ; emerald_hill_zone_act_1
    else
	move.w	#emerald_hill_zone_act_1,(Current_ZoneAndAct).w
    endif
	move.b	#3,(Life_count).w
	move.b	#3,(Life_count_2P).w
	moveq	#0,d0
	move.w	d0,(Ring_count).w
	move.l	d0,(Timer).w
	move.l	d0,(Score).w
	move.w	d0,(Ring_count_2P).w
	move.l	d0,(Timer_2P).w
	move.l	d0,(Score_2P).w
	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w
	move.w	(Player_option).w,(Player_mode).w
	rts
; ===========================================================================

;loc_944C:
LevelSelect_Return:
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen
	rts
; ===========================================================================
; -----------------------------------------------------------------------------
; Level Select Level Order

; One entry per item in the level select menu. Just set the value for the item
; you want to link to the level/act number of the level you want to load when
; the player selects that item.
; -----------------------------------------------------------------------------
;Misc_9454:
LevelSelect_Order:
	dc.w	emerald_hill_zone_act_1
	dc.w	emerald_hill_zone_act_2	; 1
	dc.w	chemical_plant_zone_act_1	; 2
	dc.w	chemical_plant_zone_act_2	; 3
	dc.w	aquatic_ruin_zone_act_1	; 4
	dc.w	aquatic_ruin_zone_act_2	; 5
	dc.w	casino_night_zone_act_1	; 6
	dc.w	casino_night_zone_act_2	; 7
	dc.w	hill_top_zone_act_1	; 8
	dc.w	hill_top_zone_act_2	; 9
	dc.w	mystic_cave_zone_act_1	; 10
	dc.w	mystic_cave_zone_act_2	; 11
	dc.w	oil_ocean_zone_act_1	; 12
	dc.w	oil_ocean_zone_act_2	; 13
	dc.w	metropolis_zone_act_1	; 14
	dc.w	metropolis_zone_act_2	; 15
	dc.w	metropolis_zone_act_3	; 16
	dc.w	sky_chase_zone_act_1	; 17
	dc.w	wing_fortress_zone_act_1	; 18
	dc.w	death_egg_zone_act_1	; 19
	dc.w	$4000	; 20 - special stage
	dc.w	$FFFF	; 21 - sound test
; ===========================================================================

;loc_9480:
LevelSelect_StartZone:
	andi.w	#$3FFF,d0
	move.w	d0,(Current_ZoneAndAct).w
	move.b	#GameModeID_Level,(Game_Mode).w ; => Level (Zone play mode)
	move.b	#3,(Life_count).w
	move.b	#3,(Life_count_2P).w
	moveq	#0,d0
	move.w	d0,(Ring_count).w
	move.l	d0,(Timer).w
	move.l	d0,(Score).w
	move.w	d0,(Ring_count_2P).w
	move.l	d0,(Timer_2P).w
	move.l	d0,(Score_2P).w
	move.b	d0,(Continue_count).w
	move.l	#5000,(Next_Extra_life_score).w
	move.l	#5000,(Next_Extra_life_score_2P).w
	move.b	#MusID_FadeOut,d0
	jsrto	PlaySound, JmpTo_PlaySound
	moveq	#0,d0
	move.w	d0,(Two_player_mode_copy).w
	move.w	d0,(Two_player_mode).w
	rts

; ===========================================================================
; ---------------------------------------------------------------------------
; Change what you're selecting in the level select
; ---------------------------------------------------------------------------
; loc_94DC:
LevSelControls:
	move.b	(Ctrl_1_Press).w,d1
	andi.b	#button_up_mask|button_down_mask,d1
	bne.s	+	; up/down pressed
	subq.w	#1,(LevSel_HoldTimer).w
	bpl.s	LevSelControls_CheckLR

+
	move.w	#$B,(LevSel_HoldTimer).w
	move.b	(Ctrl_1_Held).w,d1
	andi.b	#button_up_mask|button_down_mask,d1
	beq.s	LevSelControls_CheckLR	; up/down not pressed, check for left & right
	move.w	(Level_select_zone).w,d0
	btst	#button_up,d1
	beq.s	+
	subq.w	#1,d0	; decrease by 1
	bcc.s	+	; >= 0?
	moveq	#$15,d0	; set to $15

+
	btst	#button_down,d1
	beq.s	+
	addq.w	#1,d0	; yes, add 1
	cmpi.w	#$16,d0
	blo.s	+	; smaller than $16?
	moveq	#0,d0	; if not, set to 0

+
	move.w	d0,(Level_select_zone).w
	rts
; ===========================================================================
; loc_9522:
LevSelControls_CheckLR:
	cmpi.w	#$15,(Level_select_zone).w	; are we in the sound test?
	bne.s	LevSelControls_SwitchSide	; no
	move.w	(Sound_test_sound).w,d0
	move.b	(Ctrl_1_Press).w,d1
	btst	#button_left,d1
	beq.s	+
	subq.b	#1,d0
	bcc.s	+
	moveq	#$7F,d0

+
	btst	#button_right,d1
	beq.s	+
	addq.b	#1,d0
	cmpi.w	#$80,d0
	blo.s	+
	moveq	#0,d0

+
	btst	#button_A,d1
	beq.s	+
	addi.b	#$10,d0
	andi.b	#$7F,d0

+
	move.w	d0,(Sound_test_sound).w
	andi.w	#button_B_mask|button_C_mask,d1
	beq.s	+	; rts
	move.w	(Sound_test_sound).w,d0
	addi.w	#$80,d0
	jsrto	PlayMusic, JmpTo_PlayMusic
	lea	(debug_cheat).l,a0
	lea	(super_sonic_cheat).l,a2
	lea	(Debug_options_flag).w,a1	; Also S1_hidden_credits_flag
	moveq	#1,d2	; flag to tell the routine to enable the Super Sonic cheat
	bsr.w	CheckCheats

+
	rts
; ===========================================================================
; loc_958A:
LevSelControls_SwitchSide:	; not in soundtest, not up/down pressed
	move.b	(Ctrl_1_Press).w,d1
	andi.b	#button_left_mask|button_right_mask,d1
	beq.s	+				; no direction key pressed
	move.w	(Level_select_zone).w,d0	; left or right pressed
	move.b	LevelSelect_SwitchTable(pc,d0.w),d0 ; set selected zone according to table
	move.w	d0,(Level_select_zone).w
+
	rts
; ===========================================================================
;byte_95A2:
LevelSelect_SwitchTable:
	dc.b $E
	dc.b $F		; 1
	dc.b $11	; 2
	dc.b $11	; 3
	dc.b $12	; 4
	dc.b $12	; 5
	dc.b $13	; 6
	dc.b $13	; 7
	dc.b $14	; 8
	dc.b $14	; 9
	dc.b $15	; 10
	dc.b $15	; 11
	dc.b $C		; 12
	dc.b $D		; 13
	dc.b 0		; 14
	dc.b 1		; 15
	dc.b 1		; 16
	dc.b 2		; 17
	dc.b 4		; 18
	dc.b 6		; 19
	dc.b 8		; 20
	dc.b $A		; 21
	even
; ===========================================================================

;loc_95B8:
LevelSelect_MarkFields:
	lea	(Chunk_Table).l,a4
	lea	(LevSel_MarkTable).l,a5
	lea	(VDP_data_port).l,a6
	moveq	#0,d0
	move.w	(Level_select_zone).w,d0
	lsl.w	#2,d0
	lea	(a5,d0.w),a3
	moveq	#0,d0
	move.b	(a3),d0
	mulu.w	#$50,d0
	moveq	#0,d1
	move.b	1(a3),d1
	add.w	d1,d0
	lea	(a4,d0.w),a1
	moveq	#0,d1
	move.b	(a3),d1
	lsl.w	#7,d1
	add.b	1(a3),d1
	addi.w	#VRAM_Plane_A_Name_Table,d1
	lsl.l	#2,d1
	lsr.w	#2,d1
	ori.w	#vdpComm($0000,VRAM,WRITE)>>16,d1
	swap	d1
	move.l	d1,4(a6)

	moveq	#$D,d2
-	move.w	(a1)+,d0
	add.w	d3,d0
	move.w	d0,(a6)
	dbf	d2,-

	addq.w	#2,a3
	moveq	#0,d0
	move.b	(a3),d0
	beq.s	+
	mulu.w	#$50,d0
	moveq	#0,d1
	move.b	1(a3),d1
	add.w	d1,d0
	lea	(a4,d0.w),a1
	moveq	#0,d1
	move.b	(a3),d1
	lsl.w	#7,d1
	add.b	1(a3),d1
	addi.w	#VRAM_Plane_A_Name_Table,d1
	lsl.l	#2,d1
	lsr.w	#2,d1
	ori.w	#vdpComm($0000,VRAM,WRITE)>>16,d1
	swap	d1
	move.l	d1,4(a6)
	move.w	(a1)+,d0
	add.w	d3,d0
	move.w	d0,(a6)

+
	cmpi.w	#$15,(Level_select_zone).w
	bne.s	+	; rts
	bsr.w	LevelSelect_DrawSoundNumber
+
	rts
; ===========================================================================
;loc_965A:
LevelSelect_DrawSoundNumber:
	move.l	#vdpComm(VRAM_Plane_A_Name_Table+planeLoc(64,34,18),VRAM,WRITE),(VDP_control_port).l
	move.w	(Sound_test_sound).w,d0
	move.b	d0,d2
	lsr.b	#4,d0
	bsr.s	+
	move.b	d2,d0

+
	andi.w	#$F,d0
	cmpi.b	#$A,d0
	blo.s	+
	addi.b	#4,d0

+
	addi.b	#$10,d0
	add.w	d3,d0
	move.w	d0,(a6)
	rts
; ===========================================================================

;loc_9688:
LevelSelect_DrawIcon:
	move.w	(Level_select_zone).w,d0
	lea	(LevSel_IconTable).l,a3
	lea	(a3,d0.w),a3
	lea	(Chunk_Table+planeLoc(40,0,28)).l,a1
	moveq	#0,d0
	move.b	(a3),d0
	lsl.w	#3,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	lea	(a1,d0.w),a1
	move.l	#vdpComm(VRAM_Plane_A_Name_Table+planeLoc(64,27,22),VRAM,WRITE),d0
	moveq	#4-1,d1
	moveq	#3-1,d2
	jsrto	PlaneMapToVRAM_H40, JmpTo_PlaneMapToVRAM_H40
	lea	(Pal_LevelIcons).l,a1
	moveq	#0,d0
	move.b	(a3),d0
	lsl.w	#5,d0
	lea	(a1,d0.w),a1
	lea	(Normal_palette_line3).w,a2

    if fixBugs
	; When the icon changes, the colours are briefly incorrect. This is
	; because there's a delay between the icon being updated and the
	; colours being updated, due to the colours being uploaded to the VDP
	; during V-Int. To avoid this we can upload the colours ourselves right
	; here.
	; Prepare the VDP for data transfer.
	move.l  #vdpComm(2*16*2,CRAM,WRITE),VDP_control_port-VDP_data_port(a6)
    endif

	moveq	#bytesToLcnt(palette_line_size),d1
-
    if fixBugs
	; Upload colours to the VDP.
	move.l	(a1),(a6)
    endif
	move.l	(a1)+,(a2)+
	dbf	d1,-

	rts
; ===========================================================================
;byte_96D8
LevSel_IconTable:
	dc.b   0,0	;0	EHZ
	dc.b   7,7	;2	CPZ
	dc.b   8,8	;4	ARZ
	dc.b   6,6	;6	CNZ
	dc.b   2,2	;8	HTZ
	dc.b   5,5	;$A	MCZ
	dc.b   4,4	;$C	OOZ
	dc.b   1,1,1	;$E	MTZ
	dc.b   9	;$11	SCZ
	dc.b  $A	;$12	WFZ
	dc.b  $B	;$13	DEZ
	dc.b  $C	;$14	Special Stage
	dc.b  $E	;$15	Sound Test
	even
;byte_96EE:
LevSel_MarkTable:	; 4 bytes per level select entry
; line primary, 2*column ($E fields), line secondary, 2*column secondary (1 field)
	dc.b   3,  6,  3,$24	;0
	dc.b   3,  6,  4,$24
	dc.b   6,  6,  6,$24
	dc.b   6,  6,  7,$24
	dc.b   9,  6,  9,$24	;4
	dc.b   9,  6, $A,$24
	dc.b  $C,  6, $C,$24
	dc.b  $C,  6, $D,$24
	dc.b  $F,  6, $F,$24	;8
	dc.b  $F,  6,$10,$24
	dc.b $12,  6,$12,$24
	dc.b $12,  6,$13,$24
	dc.b $15,  6,$15,$24	;$C
	dc.b $15,  6,$16,$24
; --- second column ---
	dc.b   3,$2C,  3,$48
	dc.b   3,$2C,  4,$48
	dc.b   3,$2C,  5,$48	;$10
	dc.b   6,$2C,  0,  0
	dc.b   9,$2C,  0,  0
	dc.b  $C,$2C,  0,  0
	dc.b  $F,$2C,  0,  0	;$14
	dc.b $12,$2C,$12,$48
; ===========================================================================
; loc_9746:
CheckCheats:	; This is called from 2 places: the options screen and the level select screen
	move.w	(Correct_cheat_entries).w,d0	; Get the number of correct sound IDs entered so far
	adda.w	d0,a0				; Skip to the next entry
	move.w	(Sound_test_sound).w,d0		; Get the current sound test sound
	cmp.b	(a0),d0				; Compare it to the cheat
	bne.s	+				; If they're different, branch
	addq.w	#1,(Correct_cheat_entries).w	; Add 1 to the number of correct entries
	tst.b	1(a0)				; Is the next entry 0?
	bne.s	++				; If not, branch
	move.w	#$101,(a1)			; Enable the cheat
	move.b	#SndID_Ring,d0			; Play the ring sound
	jsrto	PlaySound, JmpTo_PlaySound
+
	move.w	#0,(Correct_cheat_entries).w	; Clear the number of correct entries
+
	move.w	(Correct_cheat_entries_2).w,d0	; Do the same procedure with the other cheat
	adda.w	d0,a2
	move.w	(Sound_test_sound).w,d0
	cmp.b	(a2),d0
	bne.s	++
	addq.w	#1,(Correct_cheat_entries_2).w
	tst.b	1(a2)
	bne.s	+++	; rts
	tst.w	d2				; Test this to determine which cheat to enable
	bne.s	+				; If not 0, branch
	move.b	#$F,(Continue_count).w		; Give 15 continues
    if fixBugs
	; Fun fact: this was fixed in the version of Sonic 2 included in
	; Sonic Mega Collection.
	move.b	#SndID_ContinueJingle,d0	; Play the continue jingle
    else
	; The next line causes the bug where the OOZ music plays until reset.
	; Remove "&$7F" to fix the bug.
	move.b	#SndID_ContinueJingle&$7F,d0	; Play the continue jingle
    endif
	jsrto	PlayMusic, JmpTo_PlayMusic
	bra.s	++
; ===========================================================================
+
	move.w	#7,(Got_Emerald).w		; Give 7 emeralds to the player
	move.b	#MusID_Emerald,d0		; Play the emerald jingle
	jsrto	PlayMusic, JmpTo_PlayMusic
+
	move.w	#0,(Correct_cheat_entries_2).w	; Clear the number of correct entries
+
	rts
; ===========================================================================
level_select_cheat:
	; 17th September 1965, the birthdate of one of Sonic 2's developers,
	; Yuji Naka.
	dc.b $19, $65,   9, $17,   0
	rev02even
; byte_97B7
continues_cheat:
	; November 24th, which was Sonic 2's release date in the EU and US.
	dc.b   1,   1,   2,   4,   0
	rev02even
debug_cheat:
	; 24th November 1992 (also known as "Sonic 2sday"), which was
	; Sonic 2's release date in the EU and US.
	dc.b   1,   9,   9,   2,   1,   1,   2,   4,   0
	rev02even
; byte_97C5
super_sonic_cheat:
	; Book of Genesis, 41:26, which makes frequent reference to the
	; number 7. 7 happens to be the number of Chaos Emeralds.
	; The Mega Drive is known as the Genesis in the US.
	dc.b   4,   1,   2,   6,   0
	rev02even

	; set the character set for menu text
	charset '@',"\27\30\31\32\33\34\35\36\37\38\39\40\41\42\43\44\45\46\47\48\49\50\51\52\53\54\55"
	charset '0',"\16\17\18\19\20\21\22\23\24\25"
	charset '*',$1A
	charset ':',$1C
	charset '.',$1D
	charset ' ',0

	; options screen menu text

TextOptScr_PlayerSelect:	menutxt	"* PLAYER SELECT *"	; byte_97CA:
TextOptScr_SonicAndMiles:	menutxt	"SONIC AND MILES"	; byte_97DC:
TextOptScr_SonicAndTails:	menutxt	"SONIC AND TAILS"	; byte_97EC:
TextOptScr_SonicAlone:		menutxt	"SONIC ALONE    "	; byte_97FC:
TextOptScr_MilesAlone:		menutxt	"MILES ALONE    "	; byte_980C:
TextOptScr_TailsAlone:		menutxt	"TAILS ALONE    "	; byte_981C:
TextOptScr_VsModeItems:		menutxt	"* VS MODE ITEMS *"	; byte_982C:
TextOptScr_AllKindsItems:	menutxt	"ALL KINDS ITEMS"	; byte_983E:
TextOptScr_TeleportOnly:	menutxt	"TELEPORT ONLY  "	; byte_984E:
TextOptScr_SoundTest:		menutxt	"*  SOUND TEST   *"	; byte_985E:
TextOptScr_0:			menutxt	"      00       "	; byte_9870:

	charset ; reset character set

; level select picture palettes
; byte_9880:
Pal_LevelIcons:	BINCLUDE "art/palettes/Level Select Icons.bin"

; 2-player level select screen mappings (Enigma compressed)
; byte_9A60:
	even
MapEng_LevSel2P:	BINCLUDE "mappings/misc/Level Select 2P.eni"

; options screen mappings (Enigma compressed)
; byte_9AB2:
	even
MapEng_Options:	BINCLUDE "mappings/misc/Options Screen.eni"

; level select screen mappings (Enigma compressed)
; byte_9ADE:
	even
MapEng_LevSel:	BINCLUDE "mappings/misc/Level Select.eni"

; 1P and 2P level select icon mappings (Enigma compressed)
; byte_9C32:
	even
MapEng_LevSelIcon:	BINCLUDE "mappings/misc/Level Select Icons.eni"
	even

    if ~~removeJmpTos
JmpTo_PlaySound ; JmpTo
	jmp	(PlaySound).l
JmpTo_PlayMusic ; JmpTo
	jmp	(PlayMusic).l
; loc_9C70: JmpTo_PlaneMapToVRAM
JmpTo_PlaneMapToVRAM_H40 ; JmpTo
	jmp	(PlaneMapToVRAM_H40).l
JmpTo2_Dynamic_Normal ; JmpTo
	jmp	(Dynamic_Normal).l

	align 4
    endif




; ===========================================================================
; loc_9C7C:
EndingSequence:
	clearRAM Object_RAM,Object_RAM_End
	clearRAM Misc_Variables,Misc_Variables_End
	clearRAM Camera_RAM,Camera_RAM_End

	move	#$2700,sr
	move.w	(VDP_Reg1_val).w,d0
	andi.b	#$BF,d0
	move.w	d0,(VDP_control_port).l

	stopZ80
	dmaFillVRAM 0,VRAM_Plane_A_Name_Table,VRAM_Plane_Table_Size ; clear Plane A pattern name table
	clr.l	(Vscroll_Factor).w
	clr.l	(unk_F61A).w
	startZ80

	lea	(VDP_control_port).l,a6
	move.w	#$8B03,(a6)		; EXT-INT disabled, V scroll by screen, H scroll by line
	move.w	#$8200|(VRAM_EndSeq_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_EndSeq_Plane_B_Name_Table1/$2000),(a6)	; PNT B base: $E000
	move.w	#$8500|(VRAM_Sprite_Attribute_Table/$200),(a6)		; Sprite attribute table base: $F800
	move.w	#$9001,(a6)		; Scroll table size: 64x32
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8720,(a6)		; Background palette/color: 2/0
	move.w	#$8ADF,(Hint_counter_reserve).w	; H-INT every 224th scanline
	move.w	(Hint_counter_reserve).w,(a6)
	clr.b	(Super_Sonic_flag).w
	cmpi.b	#7,(Emerald_count).w
	bne.s	+
	cmpi.w	#2,(Player_mode).w
	beq.s	+
	st.b	(Super_Sonic_flag).w
	move.b	#-1,(Super_Sonic_palette).w
	move.b	#$F,(Palette_timer).w
	move.w	#$30,(Palette_frame).w
+
	moveq	#0,d0
	cmpi.w	#2,(Player_mode).w
	beq.s	+
	tst.b	(Super_Sonic_flag).w
	bne.s	++
	bra.w	+++

; ===========================================================================
+
	addq.w	#2,d0
+
	addq.w	#2,d0
+
	move.w	d0,(Ending_Routine).w
	bsr.w	EndingSequence_LoadCharacterArt
	bsr.w	EndingSequence_LoadFlickyArt
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_EndingFinalTornado),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingFinalTornado).l,a0
	jsrto	NemDec, JmpTo_NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_EndingPics),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingPics).l,a0
	jsrto	NemDec, JmpTo_NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_EndingMiniTornado),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingMiniTornado).l,a0
	jsrto	NemDec, JmpTo_NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Tornado),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Tornado).l,a0
	jsrto	NemDec, JmpTo_NemDec
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Clouds),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Clouds).l,a0
	jsrto	NemDec, JmpTo_NemDec
	move.w	#death_egg_zone_act_1,(Current_ZoneAndAct).w
	move	#$2300,sr
	moveq	#signextendB(MusID_Ending),d0
	jsrto	PlayMusic, JmpTo2_PlayMusic
	move.l	#$EEE0EEE,d1
	lea	(Normal_palette).w,a1

	moveq	#bytesToLcnt(palette_line_size*4),d0
-	move.l	d1,(a1)+
	dbf	d0,-

	lea	(Pal_AC7E).l,a1
	lea	(Target_palette).w,a2

	moveq	#bytesToLcnt(palette_line_size*4),d0
-	move.l	(a1)+,(a2)+
	dbf	d0,-

	clr.b	(Screen_Shaking_Flag).w
	moveq	#0,d0
	move.w	d0,(Debug_placement_mode).w
	move.w	d0,(Level_Inactive_flag).w
	move.w	d0,(Level_frame_counter).w
	move.w	d0,(Camera_X_pos).w
	move.w	d0,(Camera_Y_pos).w
	move.w	d0,(Camera_X_pos_copy).w
	move.w	d0,(Camera_Y_pos_copy).w
	move.w	d0,(Camera_BG_X_pos).w
	move.w	#$C8,(Camera_BG_Y_pos).w
	move.l	d0,(Vscroll_Factor).w
	move.b	d0,(Horiz_block_crossed_flag_BG).w
	move.b	d0,(Verti_block_crossed_flag_BG).w
	move.w	d0,(Ending_VInt_Subrout).w
	move.w	d0,(Credits_Trigger).w

    if fixBugs
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len
    else
	; The '+4' shouldn't be here; clearRAM accidentally clears an additional 4 bytes
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len+4
    endif

	move.w	#$7FFF,(PalCycle_Timer).w
	lea	(CutScene).w,a1
	move.b	#ObjID_CutScene,id(a1) ; load objCA (end of game cutscene) at $FFFFB100
	move.b	#6,routine(a1)
	move.w	#$60,objoff_3C(a1)
	move.w	#1,objoff_30(a1)
	cmpi.w	#4,(Ending_Routine).w
	bne.s	+
	move.w	#$10,objoff_2E(a1)
	move.w	#$100,objoff_3C(a1)
+
	move.b	#VintID_Ending,(Vint_routine).w
	bsr.w	WaitForVint
	move.w	(VDP_Reg1_val).w,d0
	ori.b	#$40,d0
	move.w	d0,(VDP_control_port).l
-
	move.b	#VintID_Ending,(Vint_routine).w
	bsr.w	WaitForVint
	addq.w	#1,(Level_frame_counter).w
	jsr	(RandomNumber).l
	jsr	(RunObjects).l
	jsr	(BuildSprites).l
	tst.b	(Ending_PalCycle_flag).w
	beq.s	+
	jsrto	PalCycle_Load, JmpTo_PalCycle_Load
+
	bsr.w	EndgameCredits
	tst.w	(Level_Inactive_flag).w
	beq.w	-
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


; sub_9EF4
EndgameCredits:
	tst.b	(Credits_Trigger).w
	beq.w	.return
	bsr.w	Pal_FadeToBlack
	lea	(VDP_control_port).l,a6
	move.w	#$8004,(a6)		; H-INT disabled
	move.w	#$8200|(VRAM_EndSeq_Plane_A_Name_Table/$400),(a6)	; PNT A base: $C000
	move.w	#$8400|(VRAM_EndSeq_Plane_B_Name_Table1/$2000),(a6)	; PNT B base: $E000
	move.w	#$9001,(a6)		; Scroll table size: 64x32
	move.w	#$9200,(a6)		; Disable window
	move.w	#$8B03,(a6)		; EXT-INT disabled, V scroll by screen, H scroll by line
	move.w	#$8700,(a6)		; Background palette/color: 0/0
	clr.b	(Water_fullscreen_flag).w
	move.w	#$8C81,(a6)		; H res 40 cells, no interlace, S/H disabled
	jsrto	ClearScreen, JmpTo_ClearScreen

	clearRAM Object_Display_Lists,Object_Display_Lists_End
	clearRAM Object_RAM,Object_RAM_End
	clearRAM Misc_Variables,Misc_Variables_End
	clearRAM Camera_RAM,Camera_RAM_End

	clr.b	(Screen_Shaking_Flag).w
	moveq	#0,d0
	move.w	d0,(Level_Inactive_flag).w
	move.w	d0,(Level_frame_counter).w
	move.w	d0,(Camera_X_pos).w
	move.w	d0,(Camera_Y_pos).w
	move.w	d0,(Camera_X_pos_copy).w
	move.w	d0,(Camera_Y_pos_copy).w
	move.w	d0,(Camera_BG_X_pos).w
	move.w	d0,(Camera_BG_Y_pos).w
	move.l	d0,(Vscroll_Factor).w
	move.b	d0,(Horiz_block_crossed_flag_BG).w
	move.b	d0,(Verti_block_crossed_flag_BG).w
	move.w	d0,(Ending_VInt_Subrout).w
	move.w	d0,(Credits_Trigger).w

    if fixBugs
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len
    else
	; The '+4' shouldn't be here; clearRAM accidentally clears an additional 4 bytes
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len+4
    endif

	moveq	#signextendB(MusID_Credits),d0
	jsrto	PlaySound, JmpTo2_PlaySound
	clr.w	(Target_palette).w
	move.w	#$EEE,(Target_palette+$C).w
	move.w	#$EE,(Target_palette_line2+$C).w
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_CreditText_CredScr),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_CreditText).l,a0
	jsrto	NemDec, JmpTo_NemDec
	clr.w	(CreditsScreenIndex).w
-
	jsrto	ClearScreen, JmpTo_ClearScreen
	bsr.w	ShowCreditsScreen
	bsr.w	Pal_FadeFromBlack

	; Here's how to calculate new duration values for the below instructions.
	; Each slide of the credits is displayed for $18E frames at 60 FPS, or $144 frames at 50 FPS.
	; We also need to take into account how many frames the fade-in/fade-out take: which is $16 each.
	; Also, there are 21 slides to display.
	; That said, by doing '($18E+$16+$16)*21', we get the total number of frames it takes until
	; the credits reach the Sonic 2 splash (which is technically not an actual slide in the credits).
	; Dividing this by 60 will give us how many seconds it takes. The result being 154.7.
	; Doing the same for 50 FPS, by dividing the result of '($144+$16+$16)*21' by 50, will give us 154.56.
	; Now that we have the time it should take for the credits to end, we can adjust the calculation to account
	; for any slides we may have added. For example, if you added a slide, bringing the total to 22,
	; performing '((154.7*60)/22)-($16+$16)' will give you the new value to put in the 'move.w' instruction below.
	move.w	#$18E,d0
	btst	#6,(Graphics_Flags).w
	beq.s	+
	move.w	#$144,d0

/	move.b	#VintID_Ending,(Vint_routine).w
	bsr.w	WaitForVint
	dbf	d0,-

	bsr.w	Pal_FadeToBlack
	lea	(off_B2CA).l,a1
	addq.w	#1,(CreditsScreenIndex).w
	move.w	(CreditsScreenIndex).w,d0
	lsl.w	#2,d0
	move.l	(a1,d0.w),d0
	bpl.s	--
	bsr.w	Pal_FadeToBlack
	jsrto	ClearScreen, JmpTo_ClearScreen
	move.l	#vdpComm($0000,VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingTitle).l,a0
	jsrto	NemDec, JmpTo_NemDec
	lea	(MapEng_EndGameLogo).l,a0
	lea	(Chunk_Table).l,a1
	move.w	#0,d0
	jsrto	EniDec, JmpTo_EniDec
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_Plane_A_Name_Table+planeLoc(64,12,11),VRAM,WRITE),d0
	moveq	#16-1,d1
	moveq	#6-1,d2
	jsrto	PlaneMapToVRAM_H40, JmpTo2_PlaneMapToVRAM_H40
	clr.w	(CreditsScreenIndex).w
	bsr.w	EndgameLogoFlash

	move.w	#$3B,d0
-	move.b	#VintID_Ending,(Vint_routine).w
	bsr.w	WaitForVint
	dbf	d0,-

	move.w	#$257,d6
-	move.b	#VintID_Ending,(Vint_routine).w
	bsr.w	WaitForVint
	addq.w	#1,(CreditsScreenIndex).w
	bsr.w	EndgameLogoFlash
	cmpi.w	#$5E,(CreditsScreenIndex).w
	blo.s	-
	move.b	(Ctrl_1_Press).w,d1
	andi.b	#button_B_mask|button_C_mask|button_A_mask|button_start_mask,d1
	bne.s	+
	dbf	d6,-
+
	st.b	(Level_Inactive_flag).w
	move.b	#GameModeID_SegaScreen,(Game_Mode).w ; => SegaScreen

.return:
	rts
; End of function EndgameCredits


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_A0C0
EndgameLogoFlash:
	lea	(Normal_palette+2).w,a2
	move.w	(CreditsScreenIndex).w,d0
	cmpi.w	#$24,d0
	bhs.s	EndgameCredits.return
	btst	#0,d0
	bne.s	EndgameCredits.return
	lsr.w	#1,d0
	move.b	byte_A0EC(pc,d0.w),d0
	mulu.w	#$18,d0
	lea	pal_A0FE(pc,d0.w),a1

	moveq	#5,d0
-	move.l	(a1)+,(a2)+
	dbf	d0,-

	rts
; End of function EndgameLogoFlash

; ===========================================================================
byte_A0EC:
	dc.b   0
	dc.b   1	; 1
	dc.b   2	; 2
	dc.b   3	; 3
	dc.b   4	; 4
	dc.b   3	; 5
	dc.b   2	; 6
	dc.b   1	; 7
	dc.b   0	; 8
	dc.b   5	; 9
	dc.b   6	; 10
	dc.b   7	; 11
	dc.b   8	; 12
	dc.b   7	; 13
	dc.b   6	; 14
	dc.b   5	; 15
	dc.b   0	; 16
	dc.b   0	; 17
	even

; palette cycle for the end-of-game logo
pal_A0FE:	BINCLUDE	"art/palettes/Ending Cycle.bin"

; ===========================================================================
; ----------------------------------------------------------------------------
; Object CA - Cut scene at end of game
; ----------------------------------------------------------------------------
; Sprite_A1D6:
ObjCA:
	addq.w	#1,objoff_32(a0)
	; Branch if Tails...
	cmpi.w	#4,(Ending_Routine).w
	beq.s	+
	; ...and branch if not Super Sonic, making the first check redundant.
	; Was Sonic's ending originally *always* going to feature Super Sonic?
	cmpi.w	#2,(Ending_Routine).w
	bne.s	+
	st.b	(Super_Sonic_flag).w
	move.w	#$100,(Ring_count).w
	move.b	#-1,(Super_Sonic_palette).w
+
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjCA_Index(pc,d0.w),d1
	jmp	ObjCA_Index(pc,d1.w)
; ===========================================================================
; off_A208:
ObjCA_Index:	offsetTable
		offsetTableEntry.w ObjCA_Init	;  0
		offsetTableEntry.w loc_A240	;  2
		offsetTableEntry.w loc_A24E	;  4
		offsetTableEntry.w loc_A240	;  6
		offsetTableEntry.w loc_A256	;  8
		offsetTableEntry.w loc_A30A	; $A
		offsetTableEntry.w loc_A34C	; $C
		offsetTableEntry.w loc_A38E	; $E
; ===========================================================================
; loc_A218:
ObjCA_Init:
	moveq	#4,d0
	move.w	#$180,d1
	btst	#6,(Graphics_Flags).w
	beq.s	sub_A22A
	move.w	#$100,d1

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_A22A:

	lea	(EndSeqPaletteChanger).w,a1
	move.b	#ObjID_TtlScrPalChanger,id(a1) ; load objC9 (palette change handler) at $FFFFB0C0
	move.b	d0,subtype(a1)
	addq.b	#2,routine(a0)
	move.w	d1,objoff_3C(a0)
	rts
; End of function sub_A22A

; ===========================================================================

loc_A240:
	subq.w	#1,objoff_3C(a0)
	bmi.s	+
	rts
; ===========================================================================
+
	addq.b	#2,routine(a0)
	rts
; ===========================================================================

loc_A24E:
	moveq	#6,d0
	move.w	#$80,d1
	bra.s	sub_A22A
; ===========================================================================

loc_A256:
	move.w	objoff_2E(a0),d0
	cmpi.w	#$10,d0
	bhs.s	+
	addq.w	#4,objoff_2E(a0)
	clr.b	routine(a0)
	move.l	a0,-(sp)
	movea.l	off_A29C(pc,d0.w),a0
	lea	(Chunk_Table).l,a1
	move.w	#make_art_tile(ArtTile_ArtNem_EndingPics,0,0),d0
	jsrto	EniDec, JmpTo_EniDec
	move	#$2700,sr
	lea	(Chunk_Table).l,a1
	move.l	#vdpComm(VRAM_Plane_A_Name_Table + planeLoc(64,14,8),VRAM,WRITE),d0
	moveq	#12-1,d1
	moveq	#9-1,d2
	jsrto	PlaneMapToVRAM_H40, JmpTo2_PlaneMapToVRAM_H40
	move	#$2300,sr
	movea.l	(sp)+,a0 ; load 0bj address
	rts
; ===========================================================================
off_A29C:
	dc.l MapEng_Ending1
	dc.l MapEng_Ending2
	dc.l MapEng_Ending3
	dc.l MapEng_Ending4
; ===========================================================================
+
	move.w	#2,(Ending_VInt_Subrout).w
	st.b	(Control_Locked).w
	st.b	(Ending_PalCycle_flag).w
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	(Ending_Routine).w,d0
	move.w	ObjCA_State5_States(pc,d0.w),d0
	jsr	ObjCA_State5_States(pc,d0.w)
	move.w	#$80,d1
	bsr.w	sub_A22A
	move.w	#$40,objoff_3C(a0)
	rts
; ===========================================================================
ObjCA_State5_States:	offsetTable
	offsetTableEntry.w loc_A2E0	; 0
	offsetTableEntry.w loc_A2EE	; 2
	offsetTableEntry.w loc_A2F2	; 4
; ===========================================================================

loc_A2E0:
	moveq	#8,d0
-
	move.b	#ObjID_Sonic,id(a1) ; load Sonic object
	move.b	#$81,obj_control(a1)
	rts
; ===========================================================================

loc_A2EE:
	moveq	#$C,d0
	bra.s	-
; ===========================================================================

loc_A2F2:
	moveq	#$E,d0
	move.b	#ObjID_Tails,id(a1) ; load Tails object
	move.b	#$81,obj_control(a1)
	move.b	#ObjID_TailsTails,(Tails_Tails_Cutscene+id).w ; load Obj05 (Tails' tails) at $FFFFB080
	move.w	a1,(Tails_Tails_Cutscene+parent).w
	rts
; ===========================================================================

loc_A30A:
	subq.w	#1,objoff_3C(a0)
	bpl.s	+
	moveq	#$A,d0
	move.w	#$80,d1
	bsr.w	sub_A22A
	move.w	#$C0,objoff_3C(a0)
+
	lea	(MainCharacter).w,a1 ; a1=character
	move.b	#AniIDSonAni_Float2,anim(a1)
	move.w	#$A0,x_pos(a1)
	move.w	#$50,y_pos(a1)
	cmpi.w	#2,(Ending_Routine).w
	bne.s	+	; rts
	move.b	#AniIDSonAni_Walk,anim(a1)
	move.w	#$1000,inertia(a1)
+
	rts
; ===========================================================================

loc_A34C:
	subq.w	#1,objoff_3C(a0)
	bmi.s	+
	moveq	#0,d4
	moveq	#0,d5
	move.w	#0,(Camera_X_pos_diff).w
	move.w	#$100,(Camera_Y_pos_diff).w
	bra.w	SwScrl_DEZ
; ===========================================================================
+
	addq.b	#2,routine(a0)
	move.w	#$100,objoff_3C(a0)
	cmpi.w	#4,(Ending_Routine).w
	bne.s	return_A38C
	move.w	#$880,objoff_3C(a0)
	btst	#6,(Graphics_Flags).w
	beq.s	return_A38C
	move.w	#$660,objoff_3C(a0)

return_A38C:
	rts
; ===========================================================================

loc_A38E:
	btst	#6,(Graphics_Flags).w
	beq.s	+
	cmpi.w	#$E40,objoff_32(a0)
	beq.s	loc_A3BE
	bra.w	++
; ===========================================================================
+
	cmpi.w	#$1140,objoff_32(a0)
	beq.s	loc_A3BE
+
	subq.w	#1,objoff_3C(a0)
	bne.s	+
	lea	(ChildObject_AD62).l,a2
	jsrto	LoadChildObject, JmpTo_LoadChildObject
+
	bra.w	loc_AB9C
; ===========================================================================

loc_A3BE:
	addq.b	#2,routine(a0)
	st.b	(Credits_Trigger).w
	rts
; ===========================================================================
; ----------------------------------------------------------------------------
; Object CC - Trigger for rescue plane and birds from ending sequence
; ----------------------------------------------------------------------------
; Sprite_A3C8:
ObjCC:
	jsrto	ObjB2_Animate_Pilot, JmpTo_ObjB2_Animate_Pilot
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjCC_Index(pc,d0.w),d1
	jmp	ObjCC_Index(pc,d1.w)
; ===========================================================================
; loc_A3DA:
ObjCC_Index:	offsetTable
		offsetTableEntry.w ObjCC_Init	; 0
		offsetTableEntry.w ObjCC_Main	; 2
; ===========================================================================
; loc_A3DE:
ObjCC_Init:
	lea	(ObjB2_SubObjData).l,a1
	jsrto	LoadSubObject_Part3, JmpTo_LoadSubObject_Part3
	cmpi.w	#2,(Player_mode).w
	bne.s	+
	move.b	#4,mapping_frame(a0)
	move.b	#1,anim(a0)
+
	move.w	#-$10,x_pos(a0)
	move.w	#$C0,y_pos(a0)
	move.w	#$100,x_vel(a0)
	move.w	#-$80,y_vel(a0)
	move.b	#$14,objoff_35(a0)
	move.b	#3,priority(a0)
	move.w	#4,(Ending_VInt_Subrout).w
	move.l	a0,-(sp)
	lea	(MapEng_EndingTailsPlane).l,a0
	cmpi.w	#4,(Ending_Routine).w
	bne.s	+
	lea	(MapEng_EndingSonicPlane).l,a0
+
	lea	(Chunk_Table).l,a1
	move.w	#make_art_tile(ArtTile_ArtNem_EndingFinalTornado,0,1),d0
	jsrto	EniDec, JmpTo_EniDec
	movea.l	(sp)+,a0 ; load 0bj address
	move.w	#$C00,(Normal_palette_line3).w
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
; loc_A456:
ObjCC_Main:
	moveq	#0,d0
	move.b	routine_secondary(a0),d0
	move.w	ObjCC_State2_States(pc,d0.w),d1
	jsr	ObjCC_State2_States(pc,d1.w)
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
ObjCC_State2_States: offsetTable
	offsetTableEntry.w loc_A474	;  0
	offsetTableEntry.w loc_A4B6	;  2
	offsetTableEntry.w loc_A5A6	;  4
	offsetTableEntry.w loc_A6C6	;  6
	offsetTableEntry.w loc_A7DE	;  8
	offsetTableEntry.w loc_A83E	; $A
; ===========================================================================

loc_A474:
	cmpi.w	#$A0,x_pos(a0)
	beq.s	+
	jsrto	ObjectMove, JmpTo2_ObjectMove
-
	lea	(Ani_objB2_a).l,a1
	jmpto	AnimateSprite, JmpTo_AnimateSprite
; ===========================================================================
+
	addq.b	#2,routine_secondary(a0)
	move.w	#$480,objoff_3C(a0)
	btst	#6,(Graphics_Flags).w
	beq.s	+
	move.w	#$3D0,objoff_3C(a0)
+
	move.w	#$40,objoff_32(a0)
	st.b	(CutScene+objoff_34).w
	clr.w	x_vel(a0)
	clr.w	y_vel(a0)
	bra.s	-
; ===========================================================================

loc_A4B6:
	bsr.w	sub_ABBA
	bsr.w	sub_A524
	subq.w	#1,objoff_3C(a0)
	bmi.s	+
	bra.s	-
; ===========================================================================
+
	addq.b	#2,routine_secondary(a0)
	move.w	#2,objoff_3C(a0)
	clr.w	objoff_32(a0)
	clr.b	mapping_frame(a0)
	cmpi.w	#2,(Ending_Routine).w
	beq.s	+
	move.b	#7,mapping_frame(a0)
	cmpi.w	#4,(Ending_Routine).w
	bne.s	+
	move.b	#$18,mapping_frame(a0)
+
	clr.b	anim(a0)
	clr.b	anim_frame(a0)
	clr.b	anim_frame_duration(a0)
	move.l	#ObjCF_MapUnc_ADA2,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtKos_LevelArt,0,0),art_tile(a0)
	jsr	(Adjust2PArtPointer).l
	subi.w	#$14,x_pos(a0)
	addi.w	#$14,y_pos(a0)
	bra.w	sub_A58C

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_A524:
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	(Ending_Routine).w,d0
	move.w	off_A534(pc,d0.w),d0
	jmp	off_A534(pc,d0.w)
; End of function sub_A524

; ===========================================================================
off_A534:	offsetTable
		offsetTableEntry.w loc_A53A	; 0
		offsetTableEntry.w loc_A55C	; 2
		offsetTableEntry.w loc_A582	; 4
; ===========================================================================

loc_A53A:
	move.w	y_pos(a0),d0
	subi.w	#$1C,d0
-
	move.w	d0,y_pos(a1)
	move.w	x_pos(a0),x_pos(a1)
	move.l	#(1<<24)|(0<<16)|(AniIDSonAni_Wait<<8)|(AniIDSonAni_Wait<<0),mapping_frame(a1)
	move.w	#$100,anim_frame_duration(a1)
	rts
; ===========================================================================

loc_A55C:
	tst.w	objoff_32(a0)
	beq.s	+
	subq.w	#1,objoff_32(a0)
	addi.l	#$8000,x_pos(a1)
	addq.w	#1,y_pos(a1)
	rts
; ===========================================================================
+
	move.w	#$C0,x_pos(a1)
	move.w	#$90,y_pos(a1)
	rts
; ===========================================================================

loc_A582:
	move.w	y_pos(a0),d0
	subi.w	#$18,d0
	bra.s	-

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_A58C:
	tst.b	(Super_Sonic_flag).w
	bne.w	return_A38C

loc_A594:
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	#$200,x_pos(a1)
	move.w	#0,y_pos(a1)
	rts
; End of function sub_A58C

; ===========================================================================

loc_A5A6:
	bsr.s	sub_A58C
	subq.w	#1,objoff_3C(a0)
	bpl.s	+	; rts
	move.w	#2,objoff_3C(a0)
	move.w	objoff_32(a0),d0
	cmpi.w	#$1C,d0
	bhs.s	++
	addq.w	#1,objoff_32(a0)
	move.w	(Ending_Routine).w,d1
	move.w	off_A5FC(pc,d1.w),d1
	lea	off_A5FC(pc,d1.w),a1
	move.b	(a1,d0.w),mapping_frame(a0)
	add.w	d0,d0
	add.w	d0,d0
	move.l	word_A656(pc,d0.w),d1
	move.w	d1,y_pos(a0)
	swap	d1
	move.w	d1,x_pos(a0)
+
	rts
; ===========================================================================
+
	addq.b	#2,routine_secondary(a0)
	move.w	#$60,objoff_3C(a0)
	clr.b	objoff_31(a0)
	clr.w	objoff_32(a0)
	rts
; ===========================================================================
off_A5FC:	offsetTable
		offsetTableEntry.w byte_A602	; 0
		offsetTableEntry.w byte_A61E	; 2
		offsetTableEntry.w byte_A63A	; 4
byte_A602:
	dc.b   7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  9,  9,  9, $A, $A
	dc.b  $A, $B, $B, $B, $B, $B, $B, $B, $B, $B, $B, $B; 16
byte_A61E:
	dc.b   0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  3,  3
	dc.b   3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4; 16
byte_A63A:
	dc.b $18,$18,$18,$18,$19,$19,$19,$19,$19,$19,$19,  9,  9,  9, $A, $A
	dc.b  $A, $B, $B, $B, $B, $B, $B, $B, $B, $B, $B, $B; 16
	even
word_A656:
	dc.w   $A0,  $70,  $B0,  $70,  $B6,  $71,  $BC,  $72
	dc.w   $C4,  $74,  $C8,  $75,  $CA,  $76,  $CC,  $77; 8
	dc.w   $CE,  $78,  $D0,  $79,  $D2,  $7A,  $D4,  $7B; 16
	dc.w   $D6,  $7C,  $D9,  $7E,  $DC,  $81,  $DE,  $84; 24
	dc.w   $E1,  $87,  $E4,  $8B,  $E7,  $8F,  $EC,  $94; 32
	dc.w   $F0,  $99,  $F5,  $9D,  $F9,  $A4, $100,  $AC; 40
	dc.w  $108,  $B8, $112,  $C4, $11F,  $D3, $12C,  $FA; 48
; ===========================================================================

loc_A6C6:
	subq.w	#1,objoff_3C(a0)
	bmi.s	loc_A720
	tst.b	(Super_Sonic_flag).w
	beq.s	+	; rts
	subq.b	#1,objoff_31(a0)
	bpl.s	+	; rts
	addq.b	#3,objoff_31(a0)
	move.w	objoff_32(a0),d0
	addq.w	#4,objoff_32(a0)
	cmpi.w	#$78,d0
	bhs.s	+	; rts
	cmpi.w	#$C,d0
	blo.s	++
	bsr.w	loc_A594
	move.l	word_A766(pc,d0.w),d1
	move.w	d1,y_pos(a0)
	swap	d1
	move.w	d1,x_pos(a0)
	lsr.w	#2,d0
	move.b	byte_A748(pc,d0.w),mapping_frame(a0)
+
	rts
; ===========================================================================
+
	move.l	word_A766(pc,d0.w),d0
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	d0,y_pos(a1)
	swap	d0
	move.w	d0,x_pos(a1)
	rts
; ===========================================================================

loc_A720:
	addq.b	#2,routine_secondary(a0)
	clr.w	objoff_3C(a0)
	clr.w	objoff_32(a0)
	lea	(ChildObject_AD6E).l,a2
	jsrto	LoadChildObject, JmpTo_LoadChildObject
	tst.b	(Super_Sonic_flag).w
	bne.w	return_A38C
	lea	(ChildObject_AD6A).l,a2
	jmpto	LoadChildObject, JmpTo_LoadChildObject
; ===========================================================================
byte_A748:
	dc.b $12,$12,$12,$12,$12,$12,$12,$13,$13,$13,$13,$13,$13,$14,$14,$14
	dc.b $14,$15,$15,$15,$16,$16,$16,$16,$16,$16,$16,$16,$16,  0; 16
	even
word_A766:
	dc.w   $C0, $90	; 1
	dc.w   $B0, $91	; 3
	dc.w   $A8, $92	; 5
	dc.w   $9B, $96	; 7
	dc.w   $99, $98	; 9
	dc.w   $98, $99	; 11
	dc.w   $99, $9A	; 13
	dc.w   $9B, $9C	; 15
	dc.w   $9F, $9E	; 17
	dc.w   $A4, $A0	; 19
	dc.w   $AC, $A2	; 21
	dc.w   $B7, $A5	; 23
	dc.w   $C4, $A8	; 25
	dc.w   $D3, $AB	; 27
	dc.w   $DE, $AE	; 29
	dc.w   $E8, $B0	; 31
	dc.w   $EF, $B2	; 33
	dc.w   $F4, $B5	; 35
	dc.w   $F9, $B8	; 37
	dc.w   $FC, $BB	; 39
	dc.w   $FE, $BE	; 41
	dc.w   $FF, $C0	; 43
	dc.w  $100, $C2	; 45
	dc.w  $101, $C5	; 47
	dc.w  $102, $C8	; 49
	dc.w  $102, $CC	; 51
	dc.w  $101, $D1	; 53
	dc.w   $FD, $D7	; 55
	dc.w   $F9, $DE	; 57
	dc.w   $F9,$118	; 59
; ===========================================================================

loc_A7DE:
	bsr.w	loc_A594
	subq.w	#1,objoff_3C(a0)
	bpl.s	+	; rts
	move.w	#2,objoff_3C(a0)
	move.w	objoff_32(a0),d0
	cmpi.w	#$1C,d0
	bhs.s	++
	addq.w	#4,objoff_32(a0)
	lea	word_A822(pc,d0.w),a1
	move.w	(a1)+,d0
	add.w	d0,(Horiz_Scroll_Buf).w
	move.w	(a1)+,d0
	add.w	d0,(Vscroll_Factor_FG).w
+
	rts
; ===========================================================================
+
	addq.b	#2,routine_secondary(a0)
	bset	#3,status(a0)
	clr.b	objoff_31(a0)
	clr.w	objoff_32(a0)
	rts
; ===========================================================================
word_A822:
	dc.w  -$3A
	dc.w   $88	; 1
	dc.w   -$C	; 2
	dc.w   $22	; 3
	dc.w	-8	; 4
	dc.w   $10	; 5
	dc.w	-4	; 6
	dc.w	 8	; 7
	dc.w	-2	; 8
	dc.w	 4	; 9
	dc.w	-1	; 10
	dc.w	 2	; 11
	dc.w	-1	; 12
	dc.w	 2	; 13
; ===========================================================================

loc_A83E:
	tst.b	(Super_Sonic_flag).w
	beq.w	return_A38C
	move.b	#$17,mapping_frame(a0)
	subq.b	#1,objoff_31(a0)
	bpl.s	+	; rts
	addq.b	#3,objoff_31(a0)
	move.w	objoff_32(a0),d0
	cmpi.w	#$20,d0
	bhs.s	+	; rts
	addq.w	#4,objoff_32(a0)
	move.l	word_A874(pc,d0.w),d1
	move.w	d1,y_pos(a0)
	swap	d1
	move.w	d1,x_pos(a0)
+
	rts
; ===========================================================================
word_A874:
	dc.w   $60,$88	; 1
	dc.w   $50,$68	; 3
	dc.w   $44,$46	; 5
	dc.w   $3C,$36	; 7
	dc.w   $36,$2A	; 9
	dc.w   $33,$24	; 11
	dc.w   $31,$20	; 13
	dc.w   $30,$1E	; 15

; ===========================================================================
; ----------------------------------------------------------------------------
; Object CE - Sonic and Tails jumping off the plane from ending sequence
; ----------------------------------------------------------------------------
; Sprite_A894:
ObjCE:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjCE_Index(pc,d0.w),d1
	jmp	ObjCE_Index(pc,d1.w)
; ===========================================================================
; off_A8A2:
ObjCE_Index:	offsetTable
		offsetTableEntry.w ObjCE_Init				; 0
		offsetTableEntry.w loc_A902				; 2
		offsetTableEntry.w loc_A936				; 4
		offsetTableEntry.w BranchTo_JmpTo5_DisplaySprite	; 6
; ===========================================================================
; loc_A8AA:
ObjCE_Init:
	lea	(ObjB3_SubObjData).l,a1
	jsrto	LoadSubObject_Part3, JmpTo_LoadSubObject_Part3
	move.l	#ObjCF_MapUnc_ADA2,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtKos_LevelArt,0,1),art_tile(a0)
	move.b	#1,priority(a0)
	jsr	(Adjust2PArtPointer).l
	move.b	#$C,mapping_frame(a0)
	cmpi.w	#4,(Ending_Routine).w
	bne.s	+
	move.b	#$F,mapping_frame(a0)
	move.w	#make_art_tile(ArtTile_ArtKos_LevelArt,1,1),art_tile(a0)
+
	move.w	#$E8,d0
	move.w	d0,x_pos(a0)
	move.w	d0,objoff_30(a0)
	move.w	#$118,d0
	move.w	d0,y_pos(a0)
	move.w	d0,objoff_32(a0)
	rts
; ===========================================================================

loc_A902:
	movea.w	objoff_2C(a0),a1 ; a1=object
	btst	#3,status(a1)
	bne.s	+

loc_A90E:
	move.w	objoff_30(a0),d0
	add.w	(Horiz_Scroll_Buf).w,d0
	move.w	d0,x_pos(a0)
	move.w	objoff_32(a0),d0
	sub.w	(Vscroll_Factor_FG).w,d0
	move.w	d0,y_pos(a0)

BranchTo_JmpTo5_DisplaySprite ; BranchTo
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
+
	addq.b	#2,routine(a0)
	clr.w	objoff_3C(a0)
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================

loc_A936:
	subq.w	#1,objoff_3C(a0)
	bpl.s	BranchTo2_JmpTo5_DisplaySprite
	move.w	#4,objoff_3C(a0)
	move.w	objoff_34(a0),d0
	cmpi.w	#4,d0
	bhs.s	++
	addq.w	#2,objoff_34(a0)
	lea	byte_A980(pc,d0.w),a1
	cmpi.w	#2,(Ending_Routine).w
	bne.s	+
	lea	byte_A984(pc,d0.w),a1
+
	move.b	(a1)+,d0
	ext.w	d0
	add.w	d0,x_pos(a0)
	move.b	(a1)+,d0
	ext.w	d0
	add.w	d0,y_pos(a0)
	addq.b	#1,mapping_frame(a0)

BranchTo2_JmpTo5_DisplaySprite
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
+
	addq.b	#2,routine(a0)
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
byte_A980:
	dc.b   -8,   0
	dc.b -$44,-$38	; 2
byte_A984:
	dc.b   -8,   0
	dc.b -$50,-$40	; 2
; ===========================================================================
; ----------------------------------------------------------------------------
; Object CF - "Plane's helixes" from ending sequence
; ----------------------------------------------------------------------------
; Sprite_A988:
ObjCF:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjCF_Index(pc,d0.w),d1
	jmp	ObjCF_Index(pc,d1.w)
; ===========================================================================
; off_A996:
ObjCF_Index:	offsetTable
		offsetTableEntry.w ObjCF_Init		; 0
		offsetTableEntry.w ObjCF_Animate	; 2
; ===========================================================================
; loc_A99A:
ObjCF_Init:
	lea	(ObjB3_SubObjData).l,a1
	jsrto	LoadSubObject_Part3, JmpTo_LoadSubObject_Part3
	move.l	#ObjCF_MapUnc_ADA2,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtKos_LevelArt,0,1),art_tile(a0)
	move.b	#3,priority(a0)
	jsr	(Adjust2PArtPointer).l
	move.b	#5,mapping_frame(a0)
	move.b	#2,anim(a0)
	move.w	#$10F,d0
	move.w	d0,x_pos(a0)
	move.w	d0,objoff_30(a0)
	move.w	#$15E,d0
	move.w	d0,y_pos(a0)
	move.w	d0,objoff_32(a0)
	rts
; ===========================================================================
; loc_A9E4:
ObjCF_Animate:
	lea	(Ani_objCF).l,a1
	jsrto	AnimateSprite, JmpTo_AnimateSprite
	bra.w	loc_A90E
; ===========================================================================
; ----------------------------------------------------------------------------
; Object CB - Background clouds from ending sequence
; ----------------------------------------------------------------------------
; Sprite_A9F2:
ObjCB:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjCB_Index(pc,d0.w),d1
	jmp	ObjCB_Index(pc,d1.w)
; ===========================================================================
; off_AA00:
ObjCB_Index:	offsetTable
		offsetTableEntry.w ObjCB_Init	; 0
		offsetTableEntry.w loc_AA76	; 2
		offsetTableEntry.w loc_AA8A	; 4
; ===========================================================================
; loc_AA06:
ObjCB_Init:
	lea	(ObjB3_SubObjData).l,a1
	jsrto	LoadSubObject_Part3, JmpTo_LoadSubObject_Part3
	move.w	art_tile(a0),d0
	andi.w	#$1FFF,d0
	ori.w	#palette_mask,d0
	move.w	d0,art_tile(a0)
	move.b	#$30,width_pixels(a0)
	move.l	(RNG_seed).w,d0
	ror.l	#1,d0
	move.l	d0,(RNG_seed).w
	move.w	d0,d1
	andi.w	#3,d0
	move.b	ObjCB_Frames(pc,d0.w),mapping_frame(a0)
	add.w	d0,d0
	move.w	ObjCB_YSpeeds(pc,d0.w),y_vel(a0)
	tst.b	(CutScene+$34).w
	beq.s	+
	andi.w	#$FF,d1
	move.w	d1,y_pos(a0)
	move.w	#$150,x_pos(a0)
	rts
; ===========================================================================
+
	andi.w	#$1FF,d1
	move.w	d1,x_pos(a0)
	move.w	#$100,y_pos(a0)
	rts
; ===========================================================================
; byte_AA6A:
ObjCB_Frames:
	dc.b   0
	dc.b   1	; 1
	dc.b   2	; 2
	dc.b   0	; 3
	even
; word_AA6E:
ObjCB_YSpeeds:
	dc.w -$300
	dc.w -$200	; 1
	dc.w -$100	; 2
	dc.w -$300	; 3
; ===========================================================================

loc_AA76:
	tst.b	(CutScene+objoff_34).w
	beq.s	loc_AA8A
	addq.b	#2,routine(a0)
	move.w	y_vel(a0),x_vel(a0)
	clr.w	y_vel(a0)

loc_AA8A:
	jsrto	ObjectMove, JmpTo2_ObjectMove
	tst.b	(CutScene+objoff_34).w
	beq.s	+
	cmpi.w	#-$20,x_pos(a0)
	blt.w	JmpTo3_DeleteObject
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
+
	tst.w	y_pos(a0)
	bmi.w	JmpTo3_DeleteObject
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
; ----------------------------------------------------------------------------
; Object CD - Birds from ending sequence
; ----------------------------------------------------------------------------
endingbird_delay	= objoff_3C	; delay before doing the next action
; Sprite_AAAE:
ObjCD:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjCD_Index(pc,d0.w),d1
	jmp	ObjCD_Index(pc,d1.w)
; ===========================================================================
; off_AABC:
ObjCD_Index:	offsetTable
		offsetTableEntry.w ObjCD_Init	; 0
		offsetTableEntry.w ObjCD_Main	; 2
; ===========================================================================
; loc_AAC0:
ObjCD_Init:
	lea	(Obj28_SubObjData).l,a1
	jsrto	LoadSubObject_Part3, JmpTo_LoadSubObject_Part3
	move.l	(RNG_seed).w,d0
	ror.l	#3,d0
	move.l	d0,(RNG_seed).w
	move.l	d0,d1
	andi.w	#$7F,d0
	move.w	#-$A0,d2
	add.w	d0,d2
	move.w	d2,x_pos(a0)
	ror.l	#3,d1
	andi.w	#$FF,d1
	moveq	#8,d2
	add.w	d1,d2
	move.w	d2,y_pos(a0)
	move.w	#$100,x_vel(a0)
	moveq	#$20,d0
	cmpi.w	#$20,d1
	blo.s	+
	neg.w	d0
+
	move.w	d0,y_vel(a0)
	move.w	#$C0,endingbird_delay(a0)
	rts
; ===========================================================================
; loc_AB0E:
ObjCD_Main:
	moveq	#0,d0
	move.b	routine_secondary(a0),d0
	move.w	ObjCD_Main_States(pc,d0.w),d1
	jsr	ObjCD_Main_States(pc,d1.w)
	jsrto	ObjectMove, JmpTo2_ObjectMove
	lea	(Ani_objCD).l,a1
	jsrto	AnimateSprite, JmpTo_AnimateSprite
	jmpto	DisplaySprite, JmpTo5_DisplaySprite
; ===========================================================================
ObjCD_Main_States:	offsetTable
	offsetTableEntry.w loc_AB34	; 0
	offsetTableEntry.w loc_AB5C	; 2
	offsetTableEntry.w loc_AB8E	; 4
; ===========================================================================

loc_AB34:
	subq.w	#1,endingbird_delay(a0)
	bpl.s	+	; rts
	addq.b	#2,routine_secondary(a0)
	move.w	y_vel(a0),objoff_2E(a0)
	clr.w	x_vel(a0)
	move.w	y_pos(a0),objoff_32(a0)
	move.w	#$80,y_vel(a0)
	move.w	#$180,endingbird_delay(a0)
+
	rts
; ===========================================================================

loc_AB5C:
	subq.w	#1,endingbird_delay(a0)
	bmi.s	++
	move.w	y_pos(a0),d0
	moveq	#-4,d1
	cmp.w	objoff_32(a0),d0
	bhs.s	+
	neg.w	d1
+
	add.w	d1,y_vel(a0)
	rts
; ===========================================================================
+
	addq.b	#2,routine_secondary(a0)
	move.w	#-$100,x_vel(a0)
	move.w	objoff_2E(a0),y_vel(a0)
	move.w	#$C0,endingbird_delay(a0)
	rts
; ===========================================================================

loc_AB8E:
	subq.w	#1,endingbird_delay(a0)
	bmi.s	+
	rts
; ===========================================================================
+
	addq.w	#4,sp

    if removeJmpTos
JmpTo3_DeleteObject ; JmpTo
    endif

	jmpto	DeleteObject, JmpTo3_DeleteObject
; ===========================================================================

loc_AB9C:
	subq.w	#1,objoff_30(a0)
	bpl.s	+	; rts
	move.l	(RNG_seed).w,d0
	andi.w	#$1F,d0
	move.w	d0,objoff_30(a0)
	lea	(ChildObject_AD5E).l,a2
	jsrto	LoadChildObject, JmpTo_LoadChildObject
+
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_ABBA:
	subq.w	#1,objoff_30(a0)
	bpl.s	+	; rts
	tst.b	objoff_35(a0)
	beq.s	+	; rts
	subq.b	#1,objoff_35(a0)
	move.l	(RNG_seed).w,d0
	andi.w	#$F,d0
	move.w	d0,objoff_30(a0)
	lea	(ChildObject_AD66).l,a2
	jsrto	LoadChildObject, JmpTo_LoadChildObject
+	rts
; End of function sub_ABBA


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


; sub_ABE2:
EndingSequence_LoadCharacterArt:
	move.w	(Ending_Routine).w,d0
	move.w	EndingSequence_LoadCharacterArt_Characters(pc,d0.w),d0
	jmp	EndingSequence_LoadCharacterArt_Characters(pc,d0.w)
; End of function EndingSequence_LoadCharacterArt

; ===========================================================================
EndingSequence_LoadCharacterArt_Characters: offsetTable
	offsetTableEntry.w EndingSequence_LoadCharacterArt_Sonic	; 0
	offsetTableEntry.w EndingSequence_LoadCharacterArt_SuperSonic	; 2
	offsetTableEntry.w EndingSequence_LoadCharacterArt_Tails	; 4
; ===========================================================================
; loc_ABF4:
EndingSequence_LoadCharacterArt_Sonic:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_EndingCharacter),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingSonic).l,a0
	jmpto	NemDec, JmpTo_NemDec
; ===========================================================================
; loc_AC08:
EndingSequence_LoadCharacterArt_SuperSonic:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_EndingCharacter),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingSuperSonic).l,a0
	jmpto	NemDec, JmpTo_NemDec
; ===========================================================================
; loc_AC1C:
EndingSequence_LoadCharacterArt_Tails:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_EndingCharacter),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_EndingTails).l,a0
	jmpto	NemDec, JmpTo_NemDec

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


; sub_AC30:
EndingSequence_LoadFlickyArt:
	move.w	(Ending_Routine).w,d0
	move.w	EndingSequence_LoadFlickyArt_Flickies(pc,d0.w),d0
	jmp	EndingSequence_LoadFlickyArt_Flickies(pc,d0.w)
; End of function EndingSequence_LoadFlickyArt

; ===========================================================================
EndingSequence_LoadFlickyArt_Flickies: offsetTable
	offsetTableEntry.w EndingSequence_LoadFlickyArt_Flicky	; 0
	offsetTableEntry.w EndingSequence_LoadFlickyArt_Eagle	; 2
	offsetTableEntry.w EndingSequence_LoadFlickyArt_Chicken	; 4
; ===========================================================================
; loc_AC42:
EndingSequence_LoadFlickyArt_Flicky:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Animal_2),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Flicky).l,a0
	jmpto	NemDec, JmpTo_NemDec
; ===========================================================================
; loc_AC56:
EndingSequence_LoadFlickyArt_Eagle:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Animal_2),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Eagle).l,a0
	jmpto	NemDec, JmpTo_NemDec
; ===========================================================================
; loc_AC6A:
EndingSequence_LoadFlickyArt_Chicken:
	move.l	#vdpComm(tiles_to_bytes(ArtTile_ArtNem_Animal_2),VRAM,WRITE),(VDP_control_port).l
	lea	(ArtNem_Chicken).l,a0
	jmpto	NemDec, JmpTo_NemDec
; ===========================================================================
Pal_AC7E:	BINCLUDE	"art/palettes/Ending Sonic.bin"
Pal_AC9E:	BINCLUDE	"art/palettes/Ending Tails.bin"
Pal_ACDE:	BINCLUDE	"art/palettes/Ending Background.bin"
Pal_AD1E:	BINCLUDE	"art/palettes/Ending Photos.bin"
Pal_AD3E:	BINCLUDE	"art/palettes/Ending Super Sonic.bin"

ChildObject_AD5E:	childObjectData objoff_3E, ObjID_EndingSeqClouds, $00
ChildObject_AD62:	childObjectData objoff_3E, ObjID_EndingSeqTrigger, $00
ChildObject_AD66:	childObjectData objoff_3E, ObjID_EndingSeqBird, $00
ChildObject_AD6A:	childObjectData objoff_3E, ObjID_EndingSeqSonic, $00
ChildObject_AD6E:	childObjectData objoff_3E, ObjID_TornadoHelixes, $00

; off_AD72:
Obj28_SubObjData:
	subObjData Obj28_MapUnc_11E1C,make_art_tile(ArtTile_ArtNem_Animal_2,0,0),4,2,8,0

; animation script
; byte_AD7C
Ani_objCD:	offsetTable
		offsetTableEntry.w byte_AD7E	; 0
byte_AD7E:	dc.b   5,  0,  1,$FF
	even

; animation script
; off_AD82
Ani_objCF:	offsetTable
		offsetTableEntry.w byte_AD88	; 0
		offsetTableEntry.w byte_AD8E	; 1
		offsetTableEntry.w byte_AD9E	; 2
byte_AD88:	dc.b   3,  0,  0,  1,$FA,  0
byte_AD8E:	dc.b   3,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  3,  3,  4,$FA,  0
byte_AD9E:	dc.b   1,  5,  6,$FF
	even
; -----------------------------------------------------------------------------
; sprite mappings
; -----------------------------------------------------------------------------
ObjCF_MapUnc_ADA2:	include "mappings/sprite/objCF.asm"
; --------------------------------------------------------------------------------------
; Enigma compressed art mappings
; "Sonic the Hedgehog 2" mappings		; MapEng_B23A:
	even
MapEng_EndGameLogo:	BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_B262
ShowCreditsScreen:
	lea	off_B2CA(pc),a1
	move.w	(CreditsScreenIndex).w,d0
	lsl.w	#2,d0
	move.l	(a1,d0.w),d0
	movea.l	d0,a1

loc_B272:
	move	#$2700,sr
	lea	(VDP_data_port).l,a6
-
	move.l	(a1)+,d0
	bmi.s	++
	movea.l	d0,a2
	move.w	(a1)+,d0
	bsr.s	sub_B29E
	move.l	d0,4(a6)
	move.b	(a2)+,d0
	lsl.w	#8,d0
-
	move.b	(a2)+,d0
	bmi.s	+
	move.w	d0,(a6)
	bra.s	-
; ===========================================================================
+	bra.s	--
; ===========================================================================
+
	move	#$2300,sr
	rts
; End of function ShowCreditsScreen


; ---------------------------------------------------------------------------
; Subroutine to convert a VRAM address into a 32-bit VRAM write command word
; Input:
;	d0	VRAM address (word)
; Output:
;	d0	32-bit VDP command word for a VRAM write to specified address.
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


sub_B29E:
	andi.l	#$FFFF,d0
	lsl.l	#2,d0
	lsr.w	#2,d0
	ori.w	#vdpComm($0000,VRAM,WRITE)>>16,d0
	swap	d0
	rts
; End of function sub_B29E

; ===========================================================================

; macro for declaring pointer/position structures for intro/credit text
vram_pnt := VRAM_Plane_A_Name_Table
creditsPtrs macro addr,pos
	if "addr"<>""
		dc.l addr
		dc.w vram_pnt + pos
		shift
		shift
		creditsPtrs ALLARGS
	else
		dc.w -1
	endif
    endm

textLoc function col,line,(($80 * line) + (2 * col))

; intro text pointers (one intro screen)
vram_pnt := VRAM_TtlScr_Plane_A_Name_Table
off_B2B0: creditsPtrs	byte_BD1A,textLoc($0F,$09), byte_BCEE,textLoc($11,$0C), \
			byte_BCF6,textLoc($03,$0F), byte_BCE9,textLoc($12,$12)

; credits screen pointer table
off_B2CA:
	dc.l off_B322, off_B336, off_B34A, off_B358	; 3
	dc.l off_B366, off_B374, off_B388, off_B3A8	; 7
	dc.l off_B3C2, off_B3DC, off_B3F0, off_B41C	; 11
	dc.l off_B436, off_B450, off_B45E, off_B490	; 15
	dc.l off_B4B0, off_B4C4, off_B4F0, off_B51C	; 19
	dc.l off_B548, -1				; 21

; credits text pointers for each screen of credits
vram_pnt := VRAM_Plane_A_Name_Table
off_B322: creditsPtrs	byte_BC46,textLoc($0E,$0B), byte_BC51,textLoc($18,$0B), byte_BC55,textLoc($02,$0F)
off_B336: creditsPtrs	byte_B55C,textLoc($03,$0B), byte_B56F,textLoc($16,$0B), byte_B581,textLoc($06,$0F)
off_B34A: creditsPtrs	byte_B56F,textLoc($0C,$0B), byte_B59F,textLoc($07,$0F)
off_B358: creditsPtrs	byte_B5BC,textLoc($0C,$0B), byte_B5CD,textLoc($06,$0F)
off_B366: creditsPtrs	byte_B5EB,textLoc($05,$0B), byte_B60C,textLoc($07,$0F)
off_B374: creditsPtrs	byte_B628,textLoc($08,$0A), byte_B642,textLoc($04,$0E), byte_B665,textLoc($0A,$10)
off_B388: creditsPtrs	byte_B67B,textLoc($04,$08), byte_B69C,textLoc($11,$0A), byte_B6A4,textLoc($09,$0C), byte_B6BC,textLoc($04,$10), byte_B6DE,textLoc($08,$12)
off_B3A8: creditsPtrs	byte_B6F8,textLoc($0B,$09), byte_B70B,textLoc($09,$0B), byte_B723,textLoc($0A,$0F), byte_B738,textLoc($03,$11)
off_B3C2: creditsPtrs	byte_B75C,textLoc($04,$09), byte_B642,textLoc($04,$0D), byte_B77E,textLoc($07,$0F), byte_B799,textLoc($07,$11)
off_B3DC: creditsPtrs	byte_B7B5,textLoc($08,$0A), byte_B75C,textLoc($04,$0C), byte_B799,textLoc($07,$10)
off_B3F0: creditsPtrs	byte_B7F2,textLoc($09,$06), byte_B6BC,textLoc($04,$0A), byte_B80B,textLoc($0A,$0C), byte_B821,textLoc($09,$0E), byte_B839,textLoc($07,$10), byte_B855,textLoc($0B,$12), byte_B869,textLoc($0B,$14)
off_B41C: creditsPtrs	byte_B7B5,textLoc($09,$09), byte_B87D,textLoc($0A,$0B), byte_B893,textLoc($0B,$0F), byte_B8A8,textLoc($07,$11)
off_B436: creditsPtrs	byte_B8C5,textLoc($06,$09), byte_B8E2,textLoc($05,$0D), byte_B902,textLoc($03,$0F), byte_B90F,textLoc($04,$11)
off_B450: creditsPtrs	byte_B932,textLoc($04,$0B), byte_B954,textLoc($05,$0F)
off_B45E: creditsPtrs	byte_B974,textLoc($04,$05), byte_B995,textLoc($0F,$09), byte_B9A1,textLoc($0F,$0B), byte_B9AD,textLoc($0F,$0D), byte_B9B8,textLoc($10,$0F), byte_B9C1,textLoc($11,$11), byte_B9C8,textLoc($11,$13), byte_B9D0,textLoc($0F,$15)
off_B490: creditsPtrs	byte_B9DB,textLoc($03,$08), byte_BA00,textLoc($08,$0C), byte_BA1B,textLoc($06,$0E), byte_BA3A,textLoc($09,$10), byte_BA52,textLoc($0A,$12)
off_B4B0: creditsPtrs	byte_BA69,textLoc($09,$0A), byte_BA81,textLoc($05,$0E), byte_B7CE,textLoc($03,$10)
off_B4C4: creditsPtrs	byte_B55C,textLoc($0B,$06), byte_BAA2,textLoc($0A,$08), byte_BAB8,textLoc($03,$0C), byte_BADC,textLoc($07,$0E), byte_BAF7,textLoc($05,$10), byte_BB16,textLoc($07,$12), byte_BB32,textLoc($02,$14)
off_B4F0: creditsPtrs	byte_BB58,textLoc($06,$06), byte_BB75,textLoc($12,$08), byte_BB7B,textLoc($06,$0C), byte_BC9F,textLoc($05,$0E), byte_BBD8,textLoc($08,$10), byte_BBF2,textLoc($08,$12), byte_BC0C,textLoc($09,$14)
off_B51C: creditsPtrs	byte_BB58,textLoc($06,$06), byte_BB75,textLoc($12,$08), byte_BB98,textLoc($03,$0C), byte_BBBC,textLoc($07,$0E), byte_BCBE,textLoc($07,$10), byte_BCD9,textLoc($0D,$12), byte_BC25,textLoc($04,$14)
off_B548: creditsPtrs	byte_BC7B,textLoc($0B,$09), byte_BC8F,textLoc($12,$0D), byte_BC95,textLoc($10,$11)

 ; temporarily remap characters to credit text format
 ; let's encode 2-wide characters like Aa, Bb, Cc, etc. and hide it with a macro
 charset '@',"\x3B\2\4\6\8\xA\xC\xE\x10\x12\x13\x15\x17\x19\x1B\x1D\x1F\x21\x23\x25\x27\x29\x2B\x2D\x2F\x31\x33"
 charset 'a',"\3\5\7\9\xB\xD\xF\x11\x12\x14\x16\x18\x1A\x1C\x1E\x20\x22\x24\x26\x28\x2A\x2C\x2E\x30\x32\x34"
 charset '!',"\x3D\x39\x3F\x36"
 charset '\H',"\x39\x37\x38"
 charset '9',"\x3E\x40\x41"
 charset '1',"\x3C\x35"
 charset '.',"\x3A"
 charset ' ',0

 ; macro for defining credit text in conjunction with the remapped character set
vram_src := ArtTile_ArtNem_CreditText_CredScr
creditText macro pal,ss
	if ((vram_src & $FF) <> $0) && ((vram_src & $FF) <> $1)
		fatal "The low byte of vram_src was $\{vram_src & $FF}, but it must be $00 or $01."
	endif
	dc.b (make_art_tile(vram_src,pal,0) & $FF00) >> 8
	irpc char,ss
	dc.b "char"
	switch "char"
	case "I"
	case "1"
		dc.b "!"
	case "2"
		dc.b "$"
	case "9"
		dc.b "#"
	elsecase
l := lowstring("char")
		if l<>"char"
			dc.b l
		endif
	endcase
	endm
	dc.b -1
	rev02even
    endm

; credits text data (palette index followed by a string)
vram_src := ArtTile_ArtNem_CreditText_CredScr
byte_B55C:	creditText 1,"EXECUTIVE"
byte_B56F:	creditText 1,"PRODUCER"
byte_B581:	creditText 0,"HAYAO  NAKAYAMA"
byte_B59F:	creditText 0,"SHINOBU  TOYODA"
byte_B5BC:	creditText 1,"DIRECTOR"
byte_B5CD:	creditText 0,"MASAHARU  YOSHII"
byte_B5EB:	creditText 1,"CHIEF  PROGRAMMER"
byte_B60C:	creditText 0,"YUJI  NAKA (YU2)"
byte_B628:	creditText 1,"GAME  PLANNER"
byte_B642:	creditText 0,"HIROKAZU  YASUHARA"
byte_B665:	creditText 0,"(CAROL  YAS)"
byte_B67B:	creditText 1,"CHARACTER  DESIGN"
byte_B69C:	creditText 1,"AND"
byte_B6A4:	creditText 1,"CHIEF  ARTIST"
byte_B6BC:	creditText 0,"YASUSHI  YAMAGUCHI"
byte_B6DE:	creditText 0,"(JUDY  TOTOYA)"
byte_B6F8:	creditText 1,"ASSISTANT"
byte_B70B:	creditText 1,"PROGRAMMERS"
byte_B723:	creditText 0,"BILL  WILLIS"
byte_B738:	creditText 0,"MASANOBU  YAMAMOTO"
byte_B75C:	creditText 1,"OBJECT  PLACEMENT"
byte_B77E:	creditText 0,"TAKAHIRO  ANTO"
byte_B799:	creditText 0,"YUTAKA  SUGANO"
byte_B7B5:	creditText 1,"SPECIALSTAGE"
byte_B7CE:	creditText 0,"CAROL  ANN  HANSHAW"
byte_B7F2:	creditText 1,"ZONE  ARTISTS"
byte_B80B:	creditText 0,"CRAIG  STITT"
byte_B821:	creditText 0,"BRENDA  ROSS"
byte_B839:	creditText 0,"JINA  ISHIWATARI"
byte_B855:	creditText 0,"TOM  PAYNE"
byte_B869:	creditText 0,"PHENIX  RIE"
byte_B87D:	creditText 1,"ART  AND  CG"
byte_B893:	creditText 0,"TIM  SKELLY"
byte_B8A8:	creditText 0,"PETER  MORAWIEC"
byte_B8C5:	creditText 1,"MUSIC  COMPOSER"
byte_B8E2:	creditText 0,"MASATO  NAKAMURA"
byte_B902:	creditText 0,"( @1992"
byte_B90F:	creditText 0,"DREAMS  COME  TRUE)"
byte_B932:	creditText 1,"SOUND  PROGRAMMER"
byte_B954:	creditText 0,"TOMOYUKI  SHIMADA"
byte_B974:	creditText 1,"SOUND  ASSISTANTS"
byte_B995:	creditText 0,"MACKY"
byte_B9A1:	creditText 0,"JIMITA"
byte_B9AD:	creditText 0,"MILPO"
byte_B9B8:	creditText 0,"IPPO"
byte_B9C1:	creditText 0,"S.O"
byte_B9C8:	creditText 0,"OYZ"
byte_B9D0:	creditText 0,"N.GEE"
byte_B9DB:	creditText 1,"PROJECT  ASSISTANTS"
byte_BA00:	creditText 0,"SYUICHI  KATAGI"
byte_BA1B:	creditText 0,"TAKAHIRO  HAMANO"
byte_BA3A:	creditText 0,"YOSHIKI  OOKA"
byte_BA52:	creditText 0,"STEVE  WOITA"
byte_BA69:	creditText 1,"GAME  MANUAL"
byte_BA81:	creditText 0,"YOUICHI  TAKAHASHI"
byte_BAA2:	creditText 1,"SUPPORTERS"
byte_BAB8:	creditText 0,"DAIZABUROU  SAKURAI"
byte_BADC:	creditText 0,"HISASHI  SUZUKI"
    if gameRevision=0
byte_BAF7:	creditText 0,"TOHMAS  KALINSKE"	; typo
    else
byte_BAF7:	creditText 0,"THOMAS  KALINSKE"
    endif
byte_BB16:	creditText 0,"FUJIO  MINEGISHI"
byte_BB32:	creditText 0,"TAKAHARU UTSUNOMIYA"
byte_BB58:	creditText 1,"SPECIAL  THANKS"
byte_BB75:	creditText 1,"TO"
byte_BB7B:	creditText 0,"CINDY  CLAVERAN"
byte_BB98:	creditText 0,"DEBORAH  MCCRACKEN"
byte_BBBC:	creditText 0,"TATSUO  YAMADA"
byte_BBD8:	creditText 0,"DAISUKE  SAITO"
byte_BBF2:	creditText 0,"KUNITAKE  AOKI"
byte_BC0C:	creditText 0,"TSUNEKO  AOKI"
byte_BC25:	creditText 0,"MASAAKI  KAWAMURA"
byte_BC46:	creditText 0,"SONIC"
byte_BC51:	creditText 1,"2"
byte_BC55:	creditText 0,"CAST  OF  CHARACTERS"
byte_BC7B:	creditText 0,"PRESENTED"
byte_BC8F:	creditText 0,"BY"
byte_BC95:	creditText 0,"SEGA"
byte_BC9F:	creditText 0,"FRANCE  TANTIADO"
byte_BCBE:	creditText 0,"RICK  MACARAEG"
byte_BCD9:	creditText 0,"LOCKY  P"

 charset ; have to revert character set before changing again

 ; temporarily remap characters to intro text format
 charset '@',"\x3A\1\3\5\7\9\xB\xD\xF\x11\x12\x14\x16\x18\x1A\x1C\x1E\x20\x22\x24\x26\x28\x2A\x2C\x2E\x30\x32"
 charset 'a',"\2\4\6\8\xA\xC\xE\x10\x11\x13\x15\x17\x19\x1B\x1D\x1F\x21\x23\x25\x27\x29\x2B\x2D\x2F\x31\x33"
 charset '!',"\x3C\x38\x3E\x35"
 charset '\H',"\x38\x36\x37"
 charset '9',"\x3D\x3F\x40"
 charset '1',"\x3B\x34"
 charset '.',"\x39"
 charset ' ',0

; intro text
vram_src := ArtTile_ArtNem_CreditText
byte_BCE9:	creditText   0,"IN"
byte_BCEE:	creditText   0,"AND"
byte_BCF6:	creditText   0,"MILES 'TAILS' PROWER"
byte_BD1A:	creditText   0,"SONIC"

 charset ; revert character set

	even

; -------------------------------------------------------------------------------
; Nemesis compressed art
; 64 blocks
; Standard font used in credits
; -------------------------------------------------------------------------------
; ArtNem_BD26:
ArtNem_CreditText:	BINCLUDE	"art/nemesis/Credit Text.nem"
	even
; ===========================================================================

    if ~~removeJmpTos
JmpTo5_DisplaySprite ; JmpTo
	jmp	(DisplaySprite).l
JmpTo3_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
JmpTo2_PlaySound ; JmpTo
	jmp	(PlaySound).l
JmpTo_ObjB2_Animate_Pilot ; JmpTo
	jmp	(ObjB2_Animate_Pilot).l
JmpTo_AnimateSprite ; JmpTo
	jmp	(AnimateSprite).l
JmpTo_NemDec ; JmpTo
	jmp	(NemDec).l
JmpTo_EniDec ; JmpTo
	jmp	(EniDec).l
JmpTo_ClearScreen ; JmpTo
	jmp	(ClearScreen).l
JmpTo2_PlayMusic ; JmpTo
	jmp	(PlayMusic).l
JmpTo_LoadChildObject ; JmpTo
	jmp	(LoadChildObject).l
JmpTo2_PlaneMapToVRAM_H40 ; JmpTo
	jmp	(PlaneMapToVRAM_H40).l
JmpTo2_ObjectMove ; JmpTo
	jmp	(ObjectMove).l
JmpTo_PalCycle_Load ; JmpTo
	jmp	(PalCycle_Load).l
JmpTo_LoadSubObject_Part3 ; JmpTo
	jmp	(LoadSubObject_Part3).l

	align 4
    endif




; ---------------------------------------------------------------------------
; Subroutine to load level boundaries and start locations
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_BFBC:
LevelSizeLoad:
	clr.w	(Scroll_flags).w
	clr.w	(Scroll_flags_BG).w
	clr.w	(Scroll_flags_BG2).w
	clr.w	(Scroll_flags_BG3).w
	clr.w	(Scroll_flags_P2).w
	clr.w	(Scroll_flags_BG_P2).w
	clr.w	(Scroll_flags_BG2_P2).w
	clr.w	(Scroll_flags_BG3_P2).w
	clr.w	(Scroll_flags_copy).w
	clr.w	(Scroll_flags_BG_copy).w
	clr.w	(Scroll_flags_BG2_copy).w
	clr.w	(Scroll_flags_BG3_copy).w
	clr.w	(Scroll_flags_copy_P2).w
	clr.w	(Scroll_flags_BG_copy_P2).w
	clr.w	(Scroll_flags_BG2_copy_P2).w
	clr.w	(Scroll_flags_BG3_copy_P2).w
	clr.b	(Deform_lock).w
	clr.b	(Screen_Shaking_Flag_HTZ).w
	clr.b	(Screen_Shaking_Flag).w
	clr.b	(Scroll_lock).w
	clr.b	(Scroll_lock_P2).w
	moveq	#0,d0
	move.b	d0,(Dynamic_Resize_Routine).w ; load level boundaries
    if gameRevision=2
	move.w	d0,(WFZ_LevEvent_Subrout).w
	move.w	d0,(WFZ_BG_Y_Speed).w
	move.w	d0,(Camera_BG_X_offset).w
	move.w	d0,(Camera_BG_Y_offset).w
    endif
	move.w	(Current_ZoneAndAct).w,d0
	ror.b	#1,d0
	lsr.w	#4,d0
	lea	LevelSize(pc,d0.w),a0
	move.l	(a0)+,d0
	move.l	d0,(Camera_Min_X_pos).w
	move.l	d0,(Camera_Min_X_pos_target).w
	move.l	d0,(Tails_Min_X_pos).w
	move.l	(a0)+,d0
	move.l	d0,(Camera_Min_Y_pos).w
	move.l	d0,(Camera_Min_Y_pos_target).w
	move.l	d0,(Tails_Min_Y_pos).w
	move.w	#$1010,(Horiz_block_crossed_flag).w
	move.w	#(224/2)-16,(Camera_Y_pos_bias).w
	move.w	#(224/2)-16,(Camera_Y_pos_bias_P2).w
	bra.w	+
; ===========================================================================
; ----------------------------------------------------------------------------
; LEVEL SIZE ARRAY

; This array defines the screen boundaries for each act in the game.
; ----------------------------------------------------------------------------
;				xstart	xend	ystart	yend	; ZID ; Zone
LevelSize: zoneOrderedTable 2,8	; WrdArr_LvlSize
	; EHZ
	zoneTableEntry.w	$0,	$29A0,	$0,	$320	; Act 1
	zoneTableEntry.w	$0,	$2940,	$0,	$420	; Act 2
	; Zone 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
	; WZ
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
	; Zone 3
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
	; MTZ
	zoneTableEntry.w	$0,	$2280,	-$100,	$800	; Act 1
	zoneTableEntry.w	$0,	$1E80,	-$100,	$800	; Act 2
	; MTZ
	zoneTableEntry.w	$0,	$2A80,	-$100,	$800	; Act 3
	zoneTableEntry.w	$0,	$3FFF,	-$100,	$800	; Act 4
	; WFZ
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
	; HTZ
	zoneTableEntry.w	$0,	$2800,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3280,	$0,	$720	; Act 2
	; HPZ
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
	; Zone 9
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
	; OOZ
	zoneTableEntry.w	$0,	$2F80,	$0,	$680	; Act 1
	zoneTableEntry.w	$0,	$2D00,	$0,	$680	; Act 2
	; MCZ
	zoneTableEntry.w	$0,	$2380,	$3C0,	$720	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$60,	$720	; Act 2
	; CNZ
	zoneTableEntry.w	$0,	$27A0,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$2A80,	$0,	$720	; Act 2
	; CPZ
	zoneTableEntry.w	$0,	$2780,	$0,	$720	; Act 1
	zoneTableEntry.w	$0,	$2A80,	$0,	$720	; Act 2
	; DEZ
	zoneTableEntry.w	$0,	$1000,	$C8,	 $C8	; Act 1
	zoneTableEntry.w	$0,	$1000,  $C8,	 $C8	; Act 2
	; ARZ
	zoneTableEntry.w	$0,	$28C0,	$200,	$600	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$180,	$710	; Act 2
	; SCZ
	zoneTableEntry.w	$0,	$3FFF,	$0,	$000	; Act 1
	zoneTableEntry.w	$0,	$3FFF,	$0,	$720	; Act 2
    zoneTableEnd

; ===========================================================================
+
	tst.b	(Last_star_pole_hit).w		; was a star pole hit yet?
	beq.s	+				; if not, branch
	jsr	(Obj79_LoadData).l		; load the previously saved data
	move.w	(MainCharacter+x_pos).w,d1
	move.w	(MainCharacter+y_pos).w,d0
	bra.s	++
; ===========================================================================
+	; Put the character at the start location for the level
	move.w	(Current_ZoneAndAct).w,d0
	ror.b	#1,d0
	lsr.w	#5,d0
	lea	StartLocations(pc,d0.w),a1
	moveq	#0,d1
	move.w	(a1)+,d1
	move.w	d1,(MainCharacter+x_pos).w
	moveq	#0,d0
	move.w	(a1),d0
	move.w	d0,(MainCharacter+y_pos).w
+
	subi.w	#$A0,d1
	bcc.s	+
	moveq	#0,d1
+
	move.w	(Camera_Max_X_pos).w,d2
	cmp.w	d2,d1
	blo.s	+
	move.w	d2,d1
+
	move.w	d1,(Camera_X_pos).w
	move.w	d1,(Camera_X_pos_P2).w
	subi.w	#$60,d0
	bcc.s	+
	moveq	#0,d0
+
	cmp.w	(Camera_Max_Y_pos).w,d0
	blt.s	+
	move.w	(Camera_Max_Y_pos).w,d0
+
	move.w	d0,(Camera_Y_pos).w
	move.w	d0,(Camera_Y_pos_P2).w
	bsr.w	InitCameraValues
	rts
; End of function LevelSizeLoad

; ===========================================================================
; --------------------------------------------------------------------------------------
; CHARACTER START LOCATION ARRAY

; 2 entries per act, corresponding to the X and Y locations that you want the player to
; appear at when the level starts.
; --------------------------------------------------------------------------------------
StartLocations: zoneOrderedTable 2,4	; WrdArr_StartLoc
	; EHZ
	zoneTableBinEntry	2, "startpos/EHZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/EHZ_2.bin"	; Act 2
	; Zone 1
	zoneTableBinEntry	2, "startpos/01_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/01_2.bin"	; Act 2
	; WZ
	zoneTableBinEntry	2, "startpos/WZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/WZ_2.bin"	; Act 2
	; Zone 3
	zoneTableBinEntry	2, "startpos/03_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/03_2.bin"	; Act 2
	; MTZ
	zoneTableBinEntry	2, "startpos/MTZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/MTZ_2.bin"	; Act 2
	; MTZ
	zoneTableBinEntry	2, "startpos/MTZ_3.bin"	; Act 3
	zoneTableBinEntry	2, "startpos/MTZ_4.bin"	; Act 4
	; WFZ
	zoneTableBinEntry	2, "startpos/WFZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/WFZ_2.bin"	; Act 2
	; HTZ
	zoneTableBinEntry	2, "startpos/HTZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/HTZ_2.bin"	; Act 2
	; HPZ
	zoneTableBinEntry	2, "startpos/HPZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/HPZ_2.bin"	; Act 2
	; Zone 9
	zoneTableBinEntry	2, "startpos/09_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/09_2.bin"	; Act 2
	; OOZ
	zoneTableBinEntry	2, "startpos/OOZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/OOZ_2.bin"	; Act 2
	; MCZ
	zoneTableBinEntry	2, "startpos/MCZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/MCZ_2.bin"	; Act 2
	; CNZ
	zoneTableBinEntry	2, "startpos/CNZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/CNZ_2.bin"	; Act 2
	; CPZ
	zoneTableBinEntry	2, "startpos/CPZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/CPZ_2.bin"	; Act 2
	; DEZ
	zoneTableBinEntry	2, "startpos/DEZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/DEZ_2.bin"	; Act 2
	; ARZ
	zoneTableBinEntry	2, "startpos/ARZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/ARZ_2.bin"	; Act 2
	; SCZ
	zoneTableBinEntry	2, "startpos/SCZ_1.bin"	; Act 1
	zoneTableBinEntry	2, "startpos/SCZ_2.bin"	; Act 2
    zoneTableEnd

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_C258:
InitCameraValues:
	tst.b	(Last_star_pole_hit).w	; was a star pole hit yet?
	bne.s	+			; if yes, branch
	move.w	d0,(Camera_BG_Y_pos).w
	move.w	d0,(Camera_BG2_Y_pos).w
	move.w	d1,(Camera_BG_X_pos).w
	move.w	d1,(Camera_BG2_X_pos).w
	move.w	d1,(Camera_BG3_X_pos).w
	move.w	d0,(Camera_BG_Y_pos_P2).w
	move.w	d0,(Camera_BG2_Y_pos_P2).w
	move.w	d1,(Camera_BG_X_pos_P2).w
	move.w	d1,(Camera_BG2_X_pos_P2).w
	move.w	d1,(Camera_BG3_X_pos_P2).w
+
	moveq	#0,d2
	move.b	(Current_Zone).w,d2
	add.w	d2,d2
	move.w	InitCam_Index(pc,d2.w),d2
	jmp	InitCam_Index(pc,d2.w)
; End of function InitCameraValues

; ===========================================================================
; off_C296:
InitCam_Index: zoneOrderedOffsetTable 2,1
	zoneOffsetTableEntry.w InitCam_EHZ	; EHZ
	zoneOffsetTableEntry.w InitCam_Null0	; Zone 1
	zoneOffsetTableEntry.w InitCam_WZ	; WZ
	zoneOffsetTableEntry.w InitCam_Null0	; Zone 3
	zoneOffsetTableEntry.w InitCam_Std	; MTZ1,2
	zoneOffsetTableEntry.w InitCam_Std	; MTZ3
	zoneOffsetTableEntry.w InitCam_Null1	; WFZ
	zoneOffsetTableEntry.w InitCam_HTZ	; HTZ
	zoneOffsetTableEntry.w InitCam_HPZ	; HPZ
	zoneOffsetTableEntry.w InitCam_Null2	; Zone 9
	zoneOffsetTableEntry.w InitCam_OOZ	; OOZ
	zoneOffsetTableEntry.w InitCam_MCZ	; MCZ
	zoneOffsetTableEntry.w InitCam_CNZ	; CNZ
	zoneOffsetTableEntry.w InitCam_CPZ	; CPZ
	zoneOffsetTableEntry.w InitCam_Null3	; DEZ
	zoneOffsetTableEntry.w InitCam_ARZ	; ARZ
	zoneOffsetTableEntry.w InitCam_SCZ	; SCZ
    zoneTableEnd
; ===========================================================================
;loc_C2B8:
InitCam_EHZ:
	clr.l	(Camera_BG_X_pos).w
	clr.l	(Camera_BG_Y_pos).w
	clr.l	(Camera_BG2_Y_pos).w
	clr.l	(Camera_BG3_Y_pos).w
	lea	(TempArray_LayerDef).w,a2
	clr.l	(a2)+
	clr.l	(a2)+
	clr.l	(a2)+
	clr.l	(Camera_BG_X_pos_P2).w
	clr.l	(Camera_BG_Y_pos_P2).w
	clr.l	(Camera_BG2_Y_pos_P2).w
	clr.l	(Camera_BG3_Y_pos_P2).w
	rts
; ===========================================================================
; wtf:
InitCam_Null0:
    if gameRevision=0
	rts
    endif
; ===========================================================================
; Wood_Zone_BG:
InitCam_WZ:
    if gameRevision=0
	asr.w	#2,d0
	addi.w	#$400,d0
	move.w	d0,(Camera_BG_Y_pos).w
	asr.w	#3,d1
	move.w	d1,(Camera_BG_X_pos).w
	rts
    endif
; ===========================================================================
;loc_C2E4:
InitCam_Std:
	asr.w	#2,d0
	move.w	d0,(Camera_BG_Y_pos).w
	asr.w	#3,d1
	move.w	d1,(Camera_BG_X_pos).w
	rts
; ===========================================================================
;return_C2F2:
InitCam_Null1:
	rts
; ===========================================================================
;loc_C2F4:
InitCam_HTZ:
	clr.l	(Camera_BG_X_pos).w
	clr.l	(Camera_BG_Y_pos).w
	clr.l	(Camera_BG2_Y_pos).w
	clr.l	(Camera_BG3_Y_pos).w
	lea	(TempArray_LayerDef).w,a2
	clr.l	(a2)+
	clr.l	(a2)+
	clr.l	(a2)+
	clr.l	(Camera_BG_X_pos_P2).w
	clr.l	(Camera_BG_Y_pos_P2).w
	clr.l	(Camera_BG2_Y_pos_P2).w
	clr.l	(Camera_BG3_Y_pos_P2).w
	rts
; ===========================================================================
; Hidden_Palace_Zone_BG:
InitCam_HPZ:
    if gameRevision=0
	asr.w	#1,d0
	move.w	d0,(Camera_BG_Y_pos).w
	clr.l	(Camera_BG_X_pos).w
	rts
    endif
; ===========================================================================
; Leftover Spring Yard Zone code from Sonic 1

; Unknown_Zone_BG:
;InitCam_SYZ:
    if gameRevision=0
	asl.l	#4,d0
	move.l	d0,d2
	asl.l	#1,d0
	add.l	d2,d0
	asr.l	#8,d0
	addq.w	#1,d0
	move.w	d0,(Camera_BG_Y_pos).w
	clr.l	(Camera_BG_X_pos).w
	rts
    endif

; ===========================================================================
;return_C320:
InitCam_Null2:
	rts
; ===========================================================================
;loc_C322:
InitCam_OOZ:
	lsr.w	#3,d0
	addi.w	#$50,d0
	move.w	d0,(Camera_BG_Y_pos).w
	clr.l	(Camera_BG_X_pos).w
	rts
; ===========================================================================
;loc_C332:
InitCam_MCZ:
	clr.l	(Camera_BG_X_pos).w
	clr.l	(Camera_BG_X_pos_P2).w
	tst.b	(Current_Act).w
	bne.s	+
	divu.w	#3,d0
	subi.w	#$140,d0
	move.w	d0,(Camera_BG_Y_pos).w
	move.w	d0,(Camera_BG_Y_pos_P2).w
	rts
; ===========================================================================
+
	divu.w	#6,d0
	subi.w	#$10,d0
	move.w	d0,(Camera_BG_Y_pos).w
	move.w	d0,(Camera_BG_Y_pos_P2).w
	rts
; ===========================================================================
;loc_C364:
InitCam_CNZ:
	clr.l	(Camera_BG_X_pos).w
	clr.l	(Camera_BG_Y_pos).w
	clr.l	(Camera_BG_Y_pos_P2).w
	rts
; ===========================================================================
;loc_C372:
InitCam_CPZ:
	lsr.w	#2,d0
	move.w	d0,(Camera_BG_Y_pos).w
	move.w	d0,(Camera_BG_Y_pos_P2).w
	lsr.w	#1,d1
	move.w	d1,(Camera_BG2_X_pos).w
	lsr.w	#2,d1
	move.w	d1,(Camera_BG_X_pos).w
	rts
; ===========================================================================
;return_C38A:
InitCam_Null3:
	rts
; ===========================================================================
;loc_C38C:
InitCam_ARZ:
	tst.b	(Current_Act).w
	beq.s	+
	subi.w	#$E0,d0
	lsr.w	#1,d0
	move.w	d0,(Camera_BG_Y_pos).w
	bra.s	loc_C3A6
; ===========================================================================
+
	subi.w	#$180,d0
	move.w	d0,(Camera_BG_Y_pos).w

loc_C3A6:
	muls.w	#$119,d1
	asr.l	#8,d1
	move.w	d1,(Camera_BG_X_pos).w
	move.w	d1,(Camera_ARZ_BG_X_pos).w
	clr.w	(Camera_BG_X_pos+2).w
	clr.w	(Camera_ARZ_BG_X_pos+2).w
	clr.l	(Camera_BG2_Y_pos).w
	clr.l	(Camera_BG3_Y_pos).w
	rts
; ===========================================================================
;loc_C3C6:
InitCam_SCZ:
	clr.l	(Camera_BG_X_pos).w
	clr.l	(Camera_BG_Y_pos).w
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; sub_C3D0:
DeformBgLayer:
	tst.b	(Deform_lock).w
	beq.s	+
	rts
; ---------------------------------------------------------------------------
+
	clr.w	(Scroll_flags).w
	clr.w	(Scroll_flags_BG).w
	clr.w	(Scroll_flags_BG2).w
	clr.w	(Scroll_flags_BG3).w
	clr.w	(Scroll_flags_P2).w
	clr.w	(Scroll_flags_BG_P2).w
	clr.w	(Scroll_flags_BG2_P2).w
	clr.w	(Scroll_flags_BG3_P2).w
	clr.w	(Camera_X_pos_diff).w
	clr.w	(Camera_Y_pos_diff).w
	clr.w	(Camera_X_pos_diff_P2).w
	clr.w	(Camera_Y_pos_diff_P2).w

	; Sky Chase Zone handles scrolling manually, in 'SwScrl_SCZ'.
	cmpi.b	#sky_chase_zone,(Current_Zone).w
	bne.w	+
	tst.w	(Debug_placement_mode).w
	beq.w	loc_C4D0
+
	tst.b	(Scroll_lock).w
	bne.s	DeformBgLayerAfterScrollVert
	lea	(MainCharacter).w,a0 ; a0=character
	lea	(Camera_X_pos).w,a1
	lea	(Camera_Boundaries).w,a2
	lea	(Scroll_flags).w,a3
	lea	(Camera_X_pos_diff).w,a4
	lea	(Camera_Delay).w,a5
	lea	(Sonic_Pos_Record_Buf).w,a6
	cmpi.w	#2,(Player_mode).w
	bne.s	+
	lea	(Camera_Delay_P2).w,a5
	lea	(Tails_Pos_Record_Buf).w,a6
+
	bsr.w	ScrollHoriz
	lea	(Horiz_block_crossed_flag).w,a2
	bsr.w	SetHorizScrollFlags
	lea	(Camera_Y_pos).w,a1
	lea	(Camera_Boundaries).w,a2
	lea	(Camera_Y_pos_diff).w,a4
	move.w	(Camera_Y_pos_bias).w,d3
	cmpi.w	#2,(Player_mode).w
	bne.s	+
	move.w	(Camera_Y_pos_bias_P2).w,d3
+
	bsr.w	ScrollVerti
	lea	(Verti_block_crossed_flag).w,a2
	bsr.w	SetVertiScrollFlags

DeformBgLayerAfterScrollVert:
	tst.w	(Two_player_mode).w
	beq.s	loc_C4D0
	tst.b	(Scroll_lock_P2).w
	bne.s	loc_C4D0
	lea	(Sidekick).w,a0 ; a0=character
	lea	(Camera_X_pos_P2).w,a1
	lea	(Camera_Boundaries_P2).w,a2
	lea	(Scroll_flags_P2).w,a3
	lea	(Camera_X_pos_diff_P2).w,a4
	lea	(Camera_Delay_P2).w,a5
	lea	(Tails_Pos_Record_Buf).w,a6
	bsr.w	ScrollHoriz
	lea	(Horiz_block_crossed_flag_P2).w,a2
	bsr.w	SetHorizScrollFlags
	lea	(Camera_Y_pos_P2).w,a1
	lea	(Camera_Boundaries_P2).w,a2
	lea	(Camera_Y_pos_diff_P2).w,a4
	move.w	(Camera_Y_pos_bias_P2).w,d3
	bsr.w	ScrollVerti
	lea	(Verti_block_crossed_flag_P2).w,a2
	bsr.w	SetVertiScrollFlags

loc_C4D0:
	bsr.w	RunDynamicLevelEvents
	move.w	(Camera_Y_pos).w,(Vscroll_Factor_FG).w
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w
	move.l	(Camera_X_pos).w,(Camera_X_pos_copy).w
	move.l	(Camera_Y_pos).w,(Camera_Y_pos_copy).w
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	add.w	d0,d0
	move.w	SwScrl_Index(pc,d0.w),d0
	jmp	SwScrl_Index(pc,d0.w)
; End of function DeformBgLayer

; ===========================================================================
; ---------------------------------------------------------------------------
; JUMP TABLE FOR SOFTWARE SCROLL MANAGERS
;
; "Software scrolling" is my term for what Nemesis (and by extension, the rest
; of the world) calls "rasterized layer deformation".* Software scroll managers
; are needed to achieve certain special camera effects - namely, locking the
; screen for a boss fight and defining the limits of said screen lock, or in
; the case of Sky Chase Zone ($10), moving the camera at a fixed rate through
; a predefined course.
; They are also used for things like controlling the parallax scrolling and
; water ripple effects in EHZ, and moving the clouds in HTZ and the stars in DEZ.
; ---------------------------------------------------------------------------
SwScrl_Index: zoneOrderedOffsetTable 2,1	; JmpTbl_SwScrlMgr
	zoneOffsetTableEntry.w SwScrl_EHZ	; EHZ
	zoneOffsetTableEntry.w SwScrl_Minimal	; Zone 1
	zoneOffsetTableEntry.w SwScrl_WZ	; WZ
	zoneOffsetTableEntry.w SwScrl_Minimal	; Zone 3
	zoneOffsetTableEntry.w SwScrl_MTZ	; MTZ1,2
	zoneOffsetTableEntry.w SwScrl_MTZ	; MTZ3
	zoneOffsetTableEntry.w SwScrl_WFZ	; WFZ
	zoneOffsetTableEntry.w SwScrl_HTZ	; HTZ
	zoneOffsetTableEntry.w SwScrl_HPZ	; HPZ
	zoneOffsetTableEntry.w SwScrl_Minimal	; Zone 9
	zoneOffsetTableEntry.w SwScrl_OOZ	; OOZ
	zoneOffsetTableEntry.w SwScrl_MCZ	; MCZ
	zoneOffsetTableEntry.w SwScrl_CNZ	; CNZ
	zoneOffsetTableEntry.w SwScrl_CPZ	; CPZ
	zoneOffsetTableEntry.w SwScrl_DEZ	; DEZ
	zoneOffsetTableEntry.w SwScrl_ARZ	; ARZ
	zoneOffsetTableEntry.w SwScrl_SCZ	; SCZ
    zoneTableEnd
; ===========================================================================
; loc_C51E:
SwScrl_Title:
	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Automatically scroll the background.
	addq.w	#1,(Camera_X_pos).w

	; Calculate the background X position from the foreground X position.
	move.w	(Camera_X_pos).w,d2
	neg.w	d2
	asr.w	#2,d2

	; Update the background's (and foreground's) horizontal scrolling.
	lea	(Horiz_Scroll_Buf).w,a1

	; Do 160 lines that don't move.
	moveq	#0,d0
	move.w	#160-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	; Do 32 lines that scroll with the camera.
	move.w	d2,d0
	move.w	#32-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d0,d3
	; Make the 'ripple' animate every 8 frames.
	move.b	(Vint_runcount+3).w,d1
	andi.w	#7,d1
	bne.s	+
	subq.w	#1,(TempArray_LayerDef).w
+
	move.w	(TempArray_LayerDef).w,d1
	andi.w	#$1F,d1
	lea	SwScrl_RippleData(pc),a2
	lea	(a2,d1.w),a2

	; Do 16 lines that scroll with the camera and 'ripple'.
	move.w	#16-1,d1
-	move.b	(a2)+,d0
	ext.w	d0
	add.w	d3,d0
	move.l	d0,(a1)+
	dbf	d1,-

	; The remaining 16 lines are not set.

	rts
; ===========================================================================
; loc_C57E:
SwScrl_EHZ:
	; Use different background scrolling code for two player mode.
	tst.w	(Two_player_mode).w
	bne.w	SwScrl_EHZ_2P

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This creates an elaborate parallax effect.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	move.w	d0,d2
	swap	d0
	move.w	#0,d0

	; Do 22 lines.
	move.w	#22-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#6,d0

	; Do 58 lines.
	move.w	#58-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d0,d3

	; Make the 'ripple' animate every 8 frames.
	move.b	(Vint_runcount+3).w,d1
	andi.w	#7,d1
	bne.s	+
	subq.w	#1,(TempArray_LayerDef).w
+
	move.w	(TempArray_LayerDef).w,d1
	andi.w	#$1F,d1
	lea	(SwScrl_RippleData).l,a2
	lea	(a2,d1.w),a2

	; Do 21 lines.
	move.w	#21-1,d1
-	move.b	(a2)+,d0
	ext.w	d0
	add.w	d3,d0
	move.l	d0,(a1)+
	dbf	d1,-

	move.w	#0,d0

	; Do 11 lines.
	move.w	#11-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#4,d0

	; Do 16 lines.
	move.w	#16-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#4,d0
	move.w	d0,d1
	asr.w	#1,d1
	add.w	d1,d0

	; Do 16 lines.
	move.w	#16-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.l	d0,d4
	swap	d4
	move.w	d2,d0
	asr.w	#1,d0
	move.w	d2,d1
	asr.w	#3,d1
	sub.w	d1,d0
	ext.l	d0
	asl.l	#8,d0
	divs.w	#$30,d0
	ext.l	d0
	asl.l	#8,d0
	moveq	#0,d3
	move.w	d2,d3
	asr.w	#3,d3

	; Do 15 lines.
	move.w	#15-1,d1
-	move.w	d4,(a1)+
	move.w	d3,(a1)+
	swap	d3
	add.l	d0,d3
	swap	d3
	dbf	d1,-

	; Do 18 lines.
	move.w	#18/2-1,d1
-	move.w	d4,(a1)+
	move.w	d3,(a1)+
	move.w	d4,(a1)+
	move.w	d3,(a1)+
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	dbf	d1,-

	; Do 45 lines.
	move.w	#45/3-1,d1
-	move.w	d4,(a1)+
	move.w	d3,(a1)+
	move.w	d4,(a1)+
	move.w	d3,(a1)+
	move.w	d4,(a1)+
	move.w	d3,(a1)+
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	dbf	d1,-

	; 22+58+21+11+16+16+15+18+45=222.
	; Only 222 out of 224 lines have been processed.

    if fixBugs
	; The bottom two lines haven't had their H-scroll values set.
	; Knuckles in Sonic 2 fixes this with the following code:
	move.w	d4,(a1)+
	move.w	d3,(a1)+
	move.w	d4,(a1)+
	move.w	d3,(a1)+
    endif

	rts
; ===========================================================================
; horizontal offsets for the water rippling effect
; byte_C682:
SwScrl_RippleData:
	dc.b   1,  2,  1,  3,  1,  2,  2,  1,  2,  3,  1,  2,  1,  2,  0,  0; 16
	dc.b   2,  0,  3,  2,  2,  3,  2,  2,  1,  3,  0,  0,  1,  0,  1,  3; 32
	dc.b   1,  2,  1,  3,  1,  2,  2,  1,  2,  3,  1,  2,  1,  2,  0,  0; 48
	dc.b   2,  0,  3,  2,  2,  3,  2,  2,  1,  3,  0,  0,  1,  0,  1,  3; 64
	dc.b   1,  2	; 66
	even
; ===========================================================================
; loc_C6C4:
SwScrl_EHZ_2P:
	; Make the 'ripple' animate every 8 frames.
	move.b	(Vint_runcount+3).w,d1
	andi.w	#7,d1
	bne.s	+
	subq.w	#1,(TempArray_LayerDef).w
+
	; Do Player 1's screen.

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This creates an elaborate parallax effect.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_X_pos).w,d0
	; Do 11 lines.
	move.w	#11-1,d1
	bsr.s	.doBackground

	; Do Player 2's screen.

	; Update the background's vertical scrolling.
	moveq	#0,d0
	move.w	d0,(Vscroll_Factor_P2_BG).w
	subi.w	#224,(Vscroll_Factor_P2_BG).w

	; Update the foregrounds's vertical scrolling.
	move.w	(Camera_Y_pos_P2).w,(Vscroll_Factor_P2_FG).w
	subi.w	#224,(Vscroll_Factor_P2_FG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor_P2).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This creates an elaborate parallax effect.
	; Tails' screen is slightly taller, to fill the gap between the two
	; screens.
	lea	(Horiz_Scroll_Buf+(112-4)*2*2).w,a1
	move.w	(Camera_X_pos_P2).w,d0
	; Do 11+4 lines.
	move.w	#11+4-1,d1

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_C71A:
.doBackground:
	neg.w	d0
	move.w	d0,d2
	swap	d0
	move.w	#0,d0

-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#6,d0

	; Do 29 lines.
	move.w	#29-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d0,d3
	move.w	(TempArray_LayerDef).w,d1
	andi.w	#$1F,d1
	lea_	SwScrl_RippleData,a2
	lea	(a2,d1.w),a2

	; Do 11 lines.
	move.w	#11-1,d1
-	move.b	(a2)+,d0
	ext.w	d0
	add.w	d3,d0
	move.l	d0,(a1)+
	dbf	d1,-

	move.w	#0,d0

	; Do 5 lines.
	move.w	#5-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#4,d0

	; Do 8 lines.
	move.w	#8-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#4,d0
	move.w	d0,d1
	asr.w	#1,d1
	add.w	d1,d0

	; Do 8 lines.
	move.w	#8-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	move.w	d2,d0
	asr.w	#1,d0
	move.w	d2,d1
	asr.w	#3,d1
	sub.w	d1,d0
	ext.l	d0
	asl.l	#8,d0
	divs.w	#$30,d0
	ext.l	d0
	asl.l	#8,d0
	moveq	#0,d3
	move.w	d2,d3
	asr.w	#3,d3

	; Do 40 lines.
	move.w	#40-1,d1
-	move.w	d2,(a1)+
	move.w	d3,(a1)+
	swap	d3
	add.l	d0,d3
	swap	d3
	dbf	d1,-

	; 11+29+11+5+8+8+40=112.
	; No missing lines here.

	rts
; End of function sub_C71A

; ===========================================================================
; unused...
; loc_C7BA: SwScrl_Lev2:
SwScrl_WZ:
    if gameRevision<2
	; Just a duplicate of 'SwScrl_Minimal'.

	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#5,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#6,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	#224-1,d1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos).w,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-
    endif

	rts
; ===========================================================================
; loc_C7F2:
SwScrl_MTZ:
	; Just a duplicate of 'SwScrl_Minimal'.

	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#5,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#6,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	#224-1,d1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos).w,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-

	rts
; ===========================================================================
; loc_C82A:
SwScrl_WFZ:
	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_BG_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#8,d4
	moveq	#scroll_flag_bg1_left,d6
	bsr.w	SetHorizScrollFlagsBG

	; Ditto.
	move.w	(Camera_BG_Y_pos_diff).w,d5
	ext.l	d5
	lsl.l	#8,d5
	moveq	#scroll_flag_bg1_up_whole_row_2,d6
	bsr.w	SetVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	move.l	(Camera_BG_X_pos).w,d0
	; This can be removed if the getaway ship's entry uses d0 instead.
	move.l	d0,d1
	lea	(TempArray_LayerDef).w,a2
	move.l	d0,(a2)+				; Static parts of BG (generally no clouds in them)
	move.l	d1,(a2)+				; Eggman's getaway ship
	; Note: this is bugged: this tallies only the cloud speeds. It works fine
	; if you are standing still, but makes the clouds move faster when going
	; right and slower when going left. This is exactly the opposite of what
	; should happen.
	addi.l	#$8000,(a2)+			; Larger clouds
	addi.l	#$4000,(a2)+			; Medium clouds
	addi.l	#$2000,(a2)+			; Small clouds
	lea	(SwScrl_WFZ_Transition_Array).l,a3
	cmpi.w	#$2700,(Camera_X_pos).w
	bhs.s	.got_array
	lea	(SwScrl_WFZ_Normal_Array).l,a3

.got_array:
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1
	andi.w	#$7FF,d1
	moveq	#0,d0
	moveq	#0,d3

	; Find the first visible scrolling section
.seg_loop:
	move.b	(a3)+,d0		; Number of lines in this segment
	addq.w	#1,a3			; Skip index
	sub.w	d0,d1			; Does this segment have any visible lines?
	bcc.s	.seg_loop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	move.w	#224-1,d2		; Number of rows in hscroll buffer
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.b	-1(a3),d3		; Fetch TempArray_LayerDef index
	move.w	(a2,d3.w),d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.row_loop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.next_row		; Branch if not
	move.b	(a3)+,d1		; Fetch a new line count
	move.b	(a3)+,d3		; Fetch TempArray_LayerDef index
	move.w	(a2,d3.w),d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.next_row:
	dbf	d2,.row_loop

	rts
; ===========================================================================
; WFZ BG scrolling data
; Each pair of bytes corresponds to one scrolling segment of the BG, and
; the bytes have the following meaning:
; 	number of lines, index into TempArray_LayerDef
; byte_C8CA
SwScrl_WFZ_Transition_Array:
	dc.b $C0,  0
	dc.b $C0,  0
	dc.b $80,  0
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $80,  4
	dc.b $80,  4
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $C0,  0
	dc.b $C0,  0
	dc.b $80,  0
;byte_C916
SwScrl_WFZ_Normal_Array:
	dc.b $C0,  0
	dc.b $C0,  0
	dc.b $80,  0
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
	dc.b $C0,  0
	dc.b $C0,  0
	dc.b $80,  0
; This array is missing data for the last $80 lines compared to the transition array.
; This causes the lower clouds to read data from the start of SwScrl_HTZ.
; These are the missing entries:
    if fixBugs
	dc.b $20,  8
	dc.b $30, $C
	dc.b $30,$10
    endif
; ===========================================================================
; loc_C964:
SwScrl_HTZ:
	; Use different background scrolling code for two player mode...
	; despite the fact that Hill Top Zone is not normally playable in
	; two-player mode.
	tst.w	(Two_player_mode).w
	bne.w	SwScrl_HTZ_2P

	tst.b	(Screen_Shaking_Flag_HTZ).w
	bne.w	HTZ_Screen_Shake

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This creates an elaborate parallax effect.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	move.w	d0,d2
	swap	d0
	move.w	d2,d0
	asr.w	#3,d0

	; Do 128 lines that move together with the camera.
	move.w	#128-1,d1
-	move.l	d0,(a1)+
	dbf	d1,-

	; The remaining lines of code in this function compose the animating clouds.
	move.l	d0,d4
	move.w	(TempArray_LayerDef+$22).w,d0
	addq.w	#4,(TempArray_LayerDef+$22).w

	; Get delta between camera X and the cloud scroll value.
	sub.w	d0,d2

	; This big block of code divides and then multiplies the delta by roughly 2.28,
	; effectively subtracting 'delta modulo 2.28' from the delta.
	; I have no idea why this is necessary.

	; Start by reducing to 44% (100% divided by 2.28)...
	move.w	d2,d0
    if fixBugs
	; See below.
	moveq	#0,d1
    endif
	move.w	d0,d1
	asr.w	#1,d0 ; Divide d0 by 2
    if fixBugs
	; See below.
	swap	d1
	asr.l	#4,d1 ; Divide d1 by 16, preserving the remainder in the lower 16 bits
	swap	d1
    else
	asr.w	#4,d1 ; Divide d1 by 16, discarding the remainder
    endif
	sub.w	d1,d0 ; 100 / 2 - 100 / 16 = 44
	ext.l	d0
	; ...then increase the result to 228%, effectively undoing the reduction to 44% from earlier (0.44 x 2.28 = 1).
	asl.l	#8,d0 ; Multiply by 256
	divs.w	#256*44/100,d0 ; Divide by 112, which is 44% of 256
	ext.l	d0

	; We are done subtracting 'delta modulo 2.28' from the delta.

	; Multiply the delta by 256.
	asl.l	#8,d0

	lea	(TempArray_LayerDef).w,a2	; See 'Dynamic_HTZ.doCloudArt'.

    if fixBugs
	move.l	d1,d3 ; d1 holds the original, pre-modulo delta divided by 16.
    else
	; d3 is used as a fixed-point accumulator here, with the upper 16 bits
	; holding the integer part, and the lower 16 bits holding the decimal
	; part. This accumulator is initialised to the value of the delta
	; divided by 16, however, the decimal part of this division was not
	; preserved. This loss of precision causes the clouds to scroll with a
	; visible jerkiness.
	moveq	#0,d3
	move.w	d1,d3 ; d1 holds the original, pre-modulo delta divided by 16.
    endif

    rept 3
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,(a2)+
    endm
	move.w	d3,(a2)+
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3

	moveq	#4-1,d1
-	move.w	d3,(a2)+
	move.w	d3,(a2)+
	move.w	d3,(a2)+
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	dbf	d1,-

	; Do 8 lines.
	add.l	d0,d0
	add.l	d0,d0
	move.w	d3,d4
	move.l	d4,(a1)+
	move.l	d4,(a1)+
	move.l	d4,(a1)+
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,d4
	move.l	d4,(a1)+
	move.l	d4,(a1)+
	move.l	d4,(a1)+
	move.l	d4,(a1)+
	move.l	d4,(a1)+

	; Do 7 lines.
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,d4

	move.w	#7-1,d1
-	move.l	d4,(a1)+
	dbf	d1,-

	; Do 8 lines.
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	move.w	d3,d4

	move.w	#8-1,d1
-	move.l	d4,(a1)+
	dbf	d1,-

	; Do 10 lines.
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	move.w	d3,d4

	move.w	#10-1,d1
-	move.l	d4,(a1)+
	dbf	d1,-

	; Do 15 lines.
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	move.w	d3,d4

	move.w	#15-1,d1
-	move.l	d4,(a1)+
	dbf	d1,-

	; Do 48 lines.
	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3

	move.w	#3-1,d2
-	move.w	d3,d4

	move.w	#16-1,d1
-	move.l	d4,(a1)+
	dbf	d1,-

	swap	d3
	add.l	d0,d3
	add.l	d0,d3
	add.l	d0,d3
	add.l	d0,d3
	swap	d3
	dbf	d2,--

	; 128 + 8 + 7 + 8 + 10 + 15 + 48 = 224
	; All lines have been written.

	rts
; ===========================================================================

;loc_CA92:
HTZ_Screen_Shake:
	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_BG_X_pos_diff).w,d4
	ext.l	d4
	lsl.l	#8,d4
	moveq	#scroll_flag_bg1_left,d6
	bsr.w	SetHorizScrollFlagsBG

	; Ditto.
	move.w	(Camera_BG_Y_pos_diff).w,d5
	ext.l	d5
	lsl.l	#8,d5
	moveq	#scroll_flag_bg1_up,d6
	bsr.w	SetVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w ; Redundant.
	; Update the foreground's vertical scrolling.
	move.w	(Camera_Y_pos).w,(Vscroll_Factor_FG).w
	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	moveq	#0,d2
	tst.b	(Screen_Shaking_Flag).w
	beq.s	+

	; Make the screen shake.
	move.w	(Level_frame_counter).w,d0
	andi.w	#$3F,d0
	lea_	SwScrl_RippleData,a1
	lea	(a1,d0.w),a1
	moveq	#0,d0
	move.b	(a1)+,d0
	add.w	d0,(Vscroll_Factor_FG).w
	add.w	d0,(Vscroll_Factor_BG).w
	add.w	d0,(Camera_Y_pos_copy).w
	move.b	(a1)+,d2
	add.w	d2,(Camera_X_pos_copy).w
+
	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	#224-1,d1
	move.w	(Camera_X_pos).w,d0
	add.w	d2,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos).w,d0
	add.w	d2,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-

	rts
; ===========================================================================
; Unused background code for Hill Top Zone in two player mode!
; Unfortunately, it doesn't do anything very interesting: it's just a basic,
; flat background with no parallax effect.
; loc_CB10:
SwScrl_HTZ_2P:
	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#6,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#2,d5
	moveq	#0,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; ...But then immediately wipe them. Strange.
	; I guess the only reason 'SetHorizVertiScrollFlagsBG' is called is
	; so that 'Camera_BG_X_pos' and 'Camera_BG_Y_pos' are updated?
	move.b	#0,(Scroll_flags_BG).w

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	#112-1,d1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos).w,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-

	; Update 'Camera_BG_X_pos_P2'.
	move.w	(Camera_X_pos_diff_P2).w,d4
	ext.l	d4
	asl.l	#6,d4
	add.l	d4,(Camera_BG_X_pos_P2).w

	; Update the background's vertical scrolling.
	moveq	#0,d0
	move.w	d0,(Vscroll_Factor_P2_BG).w
	subi.w	#224,(Vscroll_Factor_P2_BG).w

	; Update the foreground's vertical scrolling.
	move.w	(Camera_Y_pos_P2).w,(Vscroll_Factor_P2_FG).w
	subi.w	#224,(Vscroll_Factor_P2_FG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor_P2).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	; Tails' screen is slightly taller, to fill the gap between the two
	; screens.
	lea	(Horiz_Scroll_Buf+(112-4)*2*2).w,a1
	move.w	#112+4-1,d1
	move.w	(Camera_X_pos_P2).w,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos_P2).w,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-

	rts
; ===========================================================================
; unused...
; loc_CBA0:
SwScrl_HPZ:
	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#6,d4
	moveq	#scroll_flag_bg1_left,d6
	bsr.w	SetHorizScrollFlagsBG

	; Ditto.
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#7,d5
	moveq	#scroll_flag_bg1_up_whole_row_2,d6
	bsr.w	SetVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Rather than scroll each individual line of the background, this
	; zone scrolls entire blocks of lines (16 lines) at once. The scroll
	; value of each row is written to 'TempArray_LayerDef', before it is
	; applied to 'Horiz_Scroll_Buf' in 'SwScrl_HPZ_Continued'. This is
	; vaguely similar to how Chemical Plant Zone scrolls its background,
	; even overflowing 'Horiz_Scroll_Buf' in the same way.
	lea	(TempArray_LayerDef).w,a1
	move.w	(Camera_X_pos).w,d2
	neg.w	d2

	; Do 8 line blocks.
	move.w	d2,d0
	asr.w	#1,d0

	move.w	#8-1,d1
-	move.w	d0,(a1)+
	dbf	d1,-

	; Do 7 line blocks.
	; This also does the 7 line blocks that get skipped later.
	move.w	d2,d0
	asr.w	#3,d0
	sub.w	d2,d0
	ext.l	d0
	asl.l	#3,d0
	divs.w	#8,d0
	ext.l	d0
	asl.l	#4,d0
	asl.l	#8,d0
	moveq	#0,d3
	move.w	d2,d3
	asr.w	#1,d3
	lea	(TempArray_LayerDef+(8+7+26+7)*2).w,a2
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,(a1)+
	move.w	d3,(a1)+
	move.w	d3,(a1)+
	move.w	d3,-(a2)
	move.w	d3,-(a2)
	move.w	d3,-(a2)
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,(a1)+
	move.w	d3,(a1)+
	move.w	d3,-(a2)
	move.w	d3,-(a2)
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,(a1)+
	move.w	d3,-(a2)
	swap	d3
	add.l	d0,d3
	swap	d3
	move.w	d3,(a1)+
	move.w	d3,-(a2)

	; Do 26 line blocks.
	move.w	(Camera_BG_X_pos).w,d0
	neg.w	d0

	move.w	#26-1,d1
-	move.w	d0,(a1)+
	dbf	d1,-

	; Skip 7 line blocks which were done earlier.
	adda.w	#7*2,a1

	; Do 24 line blocks.
	move.w	d2,d0
	asr.w	#1,d0

	move.w	#24-1,d1
-	move.w	d0,(a1)+
	dbf	d1,-

	; We're done creating the line block scroll values: now to apply them
	; to 'Horiz_Scroll_Buf'.

	; Take the background's Y position, and use it to select a line block
	; in 'TempArray_LayerDef'. Since each line block is 16 lines long,
	; this code essentially divides the Y position by 16, and then
	; multiples it by 2 to turn it into an offset into
	; 'TempArray_LayerDef'.
	lea	(TempArray_LayerDef).w,a2
	move.w	(Camera_BG_Y_pos).w,d0
	move.w	d0,d2
	andi.w	#$3F0,d0
	lsr.w	#3,d0
	lea	(a2,d0.w),a2

	; Begin filling 'Horiz_Scroll_Buf' starting with the line block
	; scroll data pointed to by 'a2'.
	bra.w	SwScrl_HPZ_Continued
; ===========================================================================
; loc_CC66:
SwScrl_OOZ:
    if fixBugs
	; As described below, part of Oil Ocean Zone's background is rendered
	; unused because the basic background drawer that this zone uses is
	; unable to draw it without making the clouds and sun disappear.
	; However, it is possible to fix this by using the advanced
	; background drawer that Chemical Plant Zone uses.

	; Update scroll flags, to dynamically load more of the background as
	; the player moves around.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#5,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#5,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Move BG1's scroll flags into BG3...
	move.b	(Scroll_flags_BG).w,(Scroll_flags_BG3).w

	; ...then clear BG1's scroll flags.
	; This zone basically uses its own dynamic background loader.
	clr.b	(Scroll_flags_BG).w
    else
	; Update 'Camera_BG_X_pos', since there's no call to
	; 'SetHorizScrollFlagsBG' or 'SetHorizVertiScrollFlagsBG' to do it
	; for us.
	move.w	(Camera_X_pos_diff).w,d0
	ext.l	d0
	asl.l	#5,d0
	add.l	d0,(Camera_BG_X_pos).w

	; Set the flags to dynamically load the background as it moves.
	; Note that this is only done vertically: Oil Ocean Zone does have
	; extra background art that can only be seen with horizontal dynamic
	; loading, but, because of this, it is never seen.
	move.w	(Camera_Y_pos_diff).w,d0
	ext.l	d0
	asl.l	#5,d0
	move.l	(Camera_BG_Y_pos).w,d3
	add.l	d3,d0
	moveq	#scroll_flag_bg1_up_whole_row,d6
	bsr.w	SetVertiScrollFlagsBG2
    endif

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; Curiously, Oil Ocean Zone fills 'Horiz_Scroll_Buf' starting from
	; the end and working backwards towards the beginning, unlike other
	; zones.
	lea	(Horiz_Scroll_Buf+224*2*2).w,a1

	; Set up the foreground part of the horizontal scroll value.
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0

	; Set up the background part of the horizontal scroll value.
	move.w	(Camera_BG_X_pos).w,d7
	neg.w	d7

	; Figure out how many lines to do for the bottom (factory) part the
	; background.
	move.w	(Camera_BG_Y_pos).w,d1
	subi.w	#80,d1
	bcc.s	+
	moveq	#0,d1
+
	subi.w	#176,d1
	bcs.s	+
	moveq	#0,d1
+
	; This will keep track of how many lines we have left to output.
	move.w	#224-1,d6

	; Do the factory part of the background.
	add.w	d6,d1
	move.w	d7,d0
	bsr.s	.doLines

	; Now do some clouds.
	bsr.s	.doMediumClouds
	bsr.s	.doSlowClouds
	bsr.s	.doFastClouds

	; Do another slow cloud layer, except 7 lines tall instead of 8.
	move.w	d7,d0
	asr.w	#4,d0
	moveq	#7-1,d1
	bsr.s	.doLines

	; Make the sun's heat haze effect animate every 8 frames.
	move.b	(Vint_runcount+3).w,d1
	andi.w	#7,d1
	bne.s	+
	subq.w	#1,(TempArray_LayerDef).w
+
	; Do the sun.
	move.w	(TempArray_LayerDef).w,d1
	andi.w	#$1F,d1
	lea	SwScrl_RippleData(pc),a2
	lea	(a2,d1.w),a2

	moveq	#33-1,d1
-	move.b	(a2)+,d0
	ext.w	d0
	move.l	d0,-(a1)
	subq.w	#1,d6
	bmi.s	+	; rts
	dbf	d1,-

	; Do some more clouds.
	bsr.s	.doMediumClouds
	bsr.s	.doSlowClouds
	bsr.s	.doFastClouds
	bsr.s	.doSlowClouds
	bsr.s	.doMediumClouds

	; Do the final, empty part of the sky.
	move.w	d7,d0
	moveq	#72-1,d1
	bsr.s	.doLines
+
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_CD0A: OOZ_BGScroll_FastClouds:
.doFastClouds:
	move.w	d7,d0
	asr.w	#2,d0
	bra.s	+
; End of function .doFastClouds


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_CD10: OOZ_BGScroll_MediumClouds:
.doMediumClouds:
	move.w	d7,d0
	asr.w	#3,d0
	bra.s	+
; End of function .doMediumClouds


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_CD16: OOZ_BGScroll_SlowClouds:
.doSlowClouds:
	move.w	d7,d0
	asr.w	#4,d0

+
	; Each 'layer' of cloud is 8 lines thick.
	moveq	#8-1,d1
; End of function .doSlowClouds


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; Scrolls min(d6,d1+1) lines by an (constant) amount specified in d0

; sub_CD1C: OOZ_BGScroll_Lines:
.doLines:
	; Output a line.
	move.l	d0,-(a1)

	; If we've reach 224 lines, bail.
	subq.w	#1,d6
	bmi.s	+

	; Do the next line.
	dbf	d1,.doLines

	rts
; ===========================================================================
+
	; Do not return to 'SwScrl_OOZ'.
	addq.l	#4,sp
	rts
; End of function .doLines

; ===========================================================================
; loc_CD2C:
SwScrl_MCZ:
	; Use different background scrolling code for two player mode.
	tst.w	(Two_player_mode).w
	bne.w	SwScrl_MCZ_2P

	; Set the flags to dynamically load the background as it moves.
	; Note that this is only done vertically: Mystic Cave Zone's
	; background repeats horizontally, so dynamic horizontal loading is
	; not needed.
	move.w	(Camera_Y_pos).w,d0
	move.l	(Camera_BG_Y_pos).w,d3
	; Curiously, the background moves vertically at different speeds
	; depending on what the current act is.
	tst.b	(Current_Act).w
	bne.s	+
	divu.w	#3,d0
	subi.w	#320,d0
	bra.s	++
+
	divu.w	#6,d0
	subi.w	#16,d0
+
	swap	d0
	moveq	#scroll_flag_bg1_up_whole_row_2,d6
	bsr.w	SetVertiScrollFlagsBG2

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Handle the screen shaking during the boss fight.
	moveq	#0,d2
    if fixBugs
	; The screen shaking is not applied to the background parallax
	; scrolling, causing it to distort. This is trivial to fix: just add
	; the Y component of the shaking to the camera's Y position.
	moveq	#0,d3
    endif
	tst.b	(Screen_Shaking_Flag).w
	beq.s	+

	move.w	(Level_frame_counter).w,d0
	andi.w	#$3F,d0
	lea_	SwScrl_RippleData,a1
	lea	(a1,d0.w),a1
	moveq	#0,d0
	move.b	(a1)+,d0
	add.w	d0,(Vscroll_Factor_FG).w
	add.w	d0,(Vscroll_Factor_BG).w
	add.w	d0,(Camera_Y_pos_copy).w
    if fixBugs
	; Ditto.
	move.w	d0,d3
    endif
	move.b	(a1)+,d2
	add.w	d2,(Camera_X_pos_copy).w
+
	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	lea	(TempArray_LayerDef).w,a2
	lea	15*2(a2),a3
	move.w	(Camera_X_pos).w,d0

	; This code is duplicated twice in 'SwScrl_MCZ_2P'.
	ext.l	d0
	asl.l	#4,d0
	divs.w	#10,d0
	ext.l	d0
	asl.l	#4,d0
	asl.l	#8,d0
	move.l	d0,d1
	swap	d1

	move.w	d1,(a3)+
	move.w	d1,7*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,6*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,5*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,4*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,3*2(a2)
	move.w	d1,8*2(a2)
	move.w	d1,14*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,2*2(a2)
	move.w	d1,9*2(a2)
	move.w	d1,13*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,1*2(a2)
	move.w	d1,10*2(a2)
	move.w	d1,12*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,0*2(a2)
	move.w	d1,11*2(a2)
	; Duplicate code end.

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	lea	(SwScrl_MCZ_RowHeights).l,a3
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1
    if fixBugs
	; Ditto.
	add.w	d3,d1
    endif
	moveq	#0,d0

	; Find the first visible scrolling section
.segmentLoop:
	move.b	(a3)+,d0		; Number of lines in this segment
	addq.w	#2,a2
	sub.w	d0,d1			; Does this segment have any visible lines?
	bcc.s	.segmentLoop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	subq.w	#2,a2
	move.w	#224-1,d2		; Number of rows in hscroll buffer
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.rowLoop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.nextRow		; Branch if not
	move.b	(a3)+,d1		; Fetch a new line count
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.nextRow:
	dbf	d2,.rowLoop

	rts
; ===========================================================================
; byte_CE6C:
SwScrl_MCZ_RowHeights:
	dc.b 37
	dc.b 23	; 1
	dc.b 18	; 2
	dc.b  7	; 3
	dc.b  7	; 4
	dc.b  2	; 5
	dc.b  2	; 6
	dc.b 48	; 7
	dc.b 13	; 8
	dc.b 19	; 9
	dc.b 32	; 10
	dc.b 64	; 11
	dc.b 32	; 12
	dc.b 19	; 13
	dc.b 13	; 14
	dc.b 48	; 15
	dc.b  2	; 16
	dc.b  2	; 17
	dc.b  7	; 18
	dc.b  7	; 19
	dc.b 32	; 20
	dc.b 18	; 21
	dc.b 23	; 22
	dc.b 37	; 23
	even
; ===========================================================================
; loc_CE84:
SwScrl_MCZ_2P:
	; Note that the flags to dynamically load the background as it moves
	; aren't set here. This is because the background is not dynamically
	; loaded in two player mode: instead, the whole background is
	; pre-loaded into Plane B. This is possible because Plane B is larger
	; in two player mode (able to hold 512x512 pixels instead of 512x256).
	moveq	#0,d0
	move.w	(Camera_Y_pos).w,d0
	; Curiously, the background moves vertically at different speeds
	; depending on what the current act is.
	tst.b	(Current_Act).w
	bne.s	+
	divu.w	#3,d0
	subi.w	#320,d0
	bra.s	++
+
	divu.w	#6,d0
	subi.w	#16,d0
+
	; Update 'Camera_BG_Y_pos'.
	move.w	d0,(Camera_BG_Y_pos).w

	; Update the background's vertical scrolling.
	move.w	d0,(Vscroll_Factor_BG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor).w

	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	lea	(TempArray_LayerDef).w,a2
	lea	15*2(a2),a3
	move.w	(Camera_X_pos).w,d0

	; A huuuuuuuuuuuuge chunk of duplicate code from 'SwScrl_MCZ'.
	ext.l	d0
	asl.l	#4,d0
	divs.w	#10,d0
	ext.l	d0
	asl.l	#4,d0
	asl.l	#8,d0
	move.l	d0,d1
	swap	d1

	move.w	d1,(a3)+
	move.w	d1,7*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,6*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,5*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,4*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,3*2(a2)
	move.w	d1,8*2(a2)
	move.w	d1,14*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,2*2(a2)
	move.w	d1,9*2(a2)
	move.w	d1,13*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,1*2(a2)
	move.w	d1,10*2(a2)
	move.w	d1,12*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,0*2(a2)
	move.w	d1,11*2(a2)
	; Duplicate code end.

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	lea	(SwScrl_MCZ2P_RowHeights).l,a3
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1
	lsr.w	#1,d1

	moveq	#0,d0

	; Find the first visible scrolling section
.segmentLoop:
	move.b	(a3)+,d0		; Number of lines in this segment
	addq.w	#2,a2
	sub.w	d0,d1			; Does this segment have any visible lines?
	bcc.s	.segmentLoop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	subq.w	#2,a2
	move.w	#112-1,d2		; Number of rows in hscroll buffer
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.rowLoop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.nextRow		; Branch if not
	move.b	(a3)+,d1		; Fetch a new line count
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.nextRow:
	dbf	d2,.rowLoop

	bra.s	+
; ===========================================================================
; byte_CF90:
SwScrl_MCZ2P_RowHeights:
	dc.b 19
	dc.b 11	; 1
	dc.b  9	; 2
	dc.b  4	; 3
	dc.b  3	; 4
	dc.b  1	; 5
	dc.b  1	; 6
	dc.b 24	; 7
	dc.b  6	; 8
	dc.b 10	; 9
	dc.b 16	; 10
	dc.b 32	; 11
	dc.b 16	; 12
	dc.b 10	; 13
	dc.b  6	; 14
	dc.b 24	; 15
	dc.b  1	; 16
	dc.b  1	; 17
	dc.b  3	; 18
	dc.b  4	; 19
	dc.b 16	; 20
	dc.b  9	; 21
	dc.b 11	; 22
	dc.b 19	; 23
	even
; ===========================================================================
+
	; Note that the flags to dynamically load the background as it moves
	; aren't set here. This is because the background is not dynamically
	; loaded in two player mode: instead, the whole background is
	; pre-loaded into Plane B. This is possible because Plane B is larger
	; in two player mode (able to hold 512x512 pixels instead of 512x256).
	moveq	#0,d0
	move.w	(Camera_Y_pos_P2).w,d0
	; Curiously, the background moves vertically at different speeds
	; depending on what the current act is.
	tst.b	(Current_Act).w
	bne.s	+
	divu.w	#3,d0
	subi.w	#320,d0
	bra.s	++
+
	divu.w	#6,d0
	subi.w	#16,d0
+
	; Update 'Camera_BG_Y_pos_P2'.
	move.w	d0,(Camera_BG_Y_pos_P2).w

	; Update the background's vertical scrolling.
	move.w	d0,(Vscroll_Factor_P2_BG).w
	subi.w	#224,(Vscroll_Factor_P2_BG).w

	; Update the foreground's vertical scrolling.
	move.w	(Camera_Y_pos_P2).w,(Vscroll_Factor_P2_FG).w
	subi.w	#224,(Vscroll_Factor_P2_FG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor_P2).w

	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	lea	(TempArray_LayerDef).w,a2
	lea	15*2(a2),a3
	move.w	(Camera_X_pos_P2).w,d0

	; A huuuuuuuuuuuuge chunk of duplicate code from 'SwScrl_MCZ'.
	ext.l	d0
	asl.l	#4,d0
	divs.w	#10,d0
	ext.l	d0
	asl.l	#4,d0
	asl.l	#8,d0
	move.l	d0,d1
	swap	d1

	move.w	d1,(a3)+
	move.w	d1,7*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,6*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,5*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,4*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,3*2(a2)
	move.w	d1,8*2(a2)
	move.w	d1,14*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,2*2(a2)
	move.w	d1,9*2(a2)
	move.w	d1,13*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,1*2(a2)
	move.w	d1,10*2(a2)
	move.w	d1,12*2(a2)

	swap	d1
	add.l	d0,d1
	swap	d1
	move.w	d1,(a3)+
	move.w	d1,0*2(a2)
	move.w	d1,11*2(a2)
	; Duplicate code end.

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	; Tails' screen is slightly taller, to fill the gap between the two
	; screens.
	lea_	SwScrl_MCZ2P_RowHeights+1,a3
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf+(112-4)*2*2).w,a1
	move.w	(Camera_BG_Y_pos_P2).w,d1
	lsr.w	#1,d1
	; Extend the first segment of 'SwScrl_MCZ2P_RowHeights' by 4 lines.
	moveq	#19+4,d0
	bra.s	.useOwnSegmentSize
; ===========================================================================

.segmentLoop:
	; Find the first visible scrolling section
	move.b	(a3)+,d0		; Number of lines in this segment

.useOwnSegmentSize:
	addq.w	#2,a2
	sub.w	d0,d1			; Does this segment have any visible lines?
	bcc.s	.segmentLoop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	subq.w	#2,a2
	move.w	#112+4-1,d2		; Number of rows in hscroll buffer
	move.w	(Camera_X_pos_P2).w,d0
	neg.w	d0
	swap	d0
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.rowLoop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.nextRow		; Branch if not
	move.b	(a3)+,d1		; Fetch a new line count
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.nextRow:
	dbf	d2,.rowLoop

	rts
; ===========================================================================
; loc_D0C6:
SwScrl_CNZ:
	; Use different background scrolling code for two player mode.
	tst.w	(Two_player_mode).w
	bne.w	SwScrl_CNZ_2P

	; Update 'Camera_BG_Y_pos'.
	move.w	(Camera_Y_pos).w,d0
	lsr.w	#6,d0
	move.w	d0,(Camera_BG_Y_pos).w

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	move.w	(Camera_X_pos).w,d2
	bsr.w	SwScrl_CNZ_GenerateScrollValues

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	lea	(SwScrl_CNZ_RowHeights).l,a3
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1

	moveq	#0,d0

	; Find the first visible scrolling section
.segmentLoop:
	move.b	(a3)+,d0		; Number of lines in this segment
	addq.w	#2,a2
	sub.w	d0,d1			; Does this segment have any visible lines?
	bcc.s	.segmentLoop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	subq.w	#2,a2
	move.w	#224-1,d2		; Number of rows in hscroll buffer
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.rowLoop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.nextRow		; Branch if not

.nextSegment:
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP
	move.b	(a3)+,d1		; Fetch a new line count
	beq.s	.isRipplingSegment	; Branch if special segment

.nextRow:
	dbf	d2,.rowLoop

	rts
; ===========================================================================

.isRipplingSegment:
	; This row is 16 pixels tall.
	move.w	#16-1,d1
	move.w	d0,d3
	; Animate the rippling effect every 8 frames.
	move.b	(Vint_runcount+3).w,d0
	lsr.w	#3,d0
	neg.w	d0
	andi.w	#$1F,d0
	lea_	SwScrl_RippleData,a4
	lea	(a4,d0.w),a4

.rippleLoop:
	move.b	(a4)+,d0
	ext.w	d0
	add.w	d3,d0
	move.l	d0,(a1)+
	dbf	d1,.rippleLoop

	; We've done 16 lines, so subtract them from the counter.
	subi.w	#16,d2
	bra.s	.nextSegment
; ===========================================================================
; byte_D156:
SwScrl_CNZ_RowHeights:
	dc.b  16
	dc.b  16
	dc.b  16
	dc.b  16
	dc.b  16
	dc.b  16
	dc.b  16
	dc.b  16
	dc.b   0	; Special (actually has a height of 16)
	dc.b 240
	even

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_D160:
SwScrl_CNZ_GenerateScrollValues:
	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	lea	(TempArray_LayerDef).w,a1
	move.w	d2,d0
	asr.w	#3,d0
	sub.w	d2,d0
	ext.l	d0
	asl.l	#5,d0
	asl.l	#8,d0
	moveq	#0,d3
	move.w	d2,d3

	move.w	#7-1,d1
-	move.w	d3,(a1)+
	swap	d3
	add.l	d0,d3
	swap	d3
	dbf	d1,-

	move.w	d2,d0
	asr.w	#3,d0
	move.w	d0,4(a1)
	asr.w	#1,d0
	move.w	d0,(a1)+
	move.w	d0,(a1)+
	rts
; End of function sub_D160

; ===========================================================================
; loc_D194:
SwScrl_CNZ_2P:
	; Do player 1's background.

	; Update 'Camera_BG_Y_pos'.
	move.w	(Camera_Y_pos).w,d0
	lsr.w	#6,d0
	move.w	d0,(Camera_BG_Y_pos).w

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor).w

	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	move.w	(Camera_X_pos).w,d2
	bsr.w	SwScrl_CNZ_GenerateScrollValues

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1
	moveq	#0,d0
	move.w	(Camera_X_pos).w,d0
	move.w	#112-1,d2
	lea	(SwScrl_CNZ2P_RowHeights_P1).l,a3
	bsr.s	.doBackground

	; Do player 2's background.

	; Update 'Camera_BG_Y_pos'.
	move.w	(Camera_Y_pos_P2).w,d0
	lsr.w	#6,d0
	move.w	d0,(Camera_BG_Y_pos_P2).w

	; Update the background's vertical scrolling.
	move.w	d0,(Vscroll_Factor_P2_BG).w
	subi.w	#224,(Vscroll_Factor_P2_BG).w

	; Update the foreground's vertical scrolling.
	move.w	(Camera_Y_pos_P2).w,(Vscroll_Factor_P2_FG).w
	subi.w	#224,(Vscroll_Factor_P2_FG).w

	; Only allow the screen to vertically scroll two pixels at a time.
	andi.l	#$FFFEFFFE,(Vscroll_Factor_P2).w

	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	move.w	(Camera_X_pos_P2).w,d2
	bsr.w	SwScrl_CNZ_GenerateScrollValues

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	; Tails' screen is slightly taller, to fill the gap between the two
	; screens.
	lea	(Horiz_Scroll_Buf+(112-4)*2*2).w,a1
	move.w	(Camera_BG_Y_pos_P2).w,d1
	moveq	#0,d0
	move.w	(Camera_X_pos_P2).w,d0
	move.w	#112+4-1,d2
	lea	(SwScrl_CNZ2P_RowHeights_P2).l,a3

    if fixBugs
	; Use a similar trick to Mystic Cave Zone: override the first value
	; in the code here.
	lsr.w	#1,d1
	lea	(TempArray_LayerDef).w,a2
	; Extend the first segment of 'SwScrl_CNZ2P_RowHeights' by 4 lines.
	move.w	#8+4,d3
	bra.s	.useOwnSegmentSize
    endif

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_D216:
.doBackground:
	lsr.w	#1,d1
	lea	(TempArray_LayerDef).w,a2
	moveq	#0,d3

	; Find the first visible scrolling section
.segmentLoop:
	move.b	(a3)+,d3		; Number of lines in this segment

.useOwnSegmentSize:
	addq.w	#2,a2
	sub.w	d3,d1			; Does this segment have any visible lines?
	bcc.s	.segmentLoop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	subq.w	#2,a2
	neg.w	d0
	swap	d0
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.rowLoop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.nextRow		; Branch if not

.nextSegment:
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP
	move.b	(a3)+,d1		; Fetch a new line count
	beq.s	.isRipplingSegment	; Branch if special segment

.nextRow:
	dbf	d2,.rowLoop

	rts
; ===========================================================================

.isRipplingSegment:
	; This row is 8 pixels tall.
	move.w	#8-1,d1
	move.w	d0,d3
	; Animate the rippling effect every 8 frames.
	move.b	(Vint_runcount+3).w,d0
	lsr.w	#3,d0
	neg.w	d0
	andi.w	#$1F,d0
	lea_	SwScrl_RippleData,a4
	lea	(a4,d0.w),a4

.rippleLoop:
	move.b	(a4)+,d0
	ext.w	d0
	add.w	d3,d0
	move.l	d0,(a1)+
	dbf	d1,.rippleLoop

	; We've done 8 lines, so subtract them from the counter.
	subq.w	#8,d2
	bra.s	.nextSegment
; End of function sub_D216

; ===========================================================================
    if ~~fixBugs
	; This doesn't have the effect that the developers intended: rather
	; than just extend the topmost segment, it creates additional
	; segments which cause the later segments to use the wrong scroll
	; values.
	dc.b   4
SwScrl_CNZ2P_RowHeights_P2:
	dc.b   4
    endif
SwScrl_CNZ2P_RowHeights_P1:
	dc.b   8
    if fixBugs
	; See above.
SwScrl_CNZ2P_RowHeights_P2:
    endif
	dc.b   8
	dc.b   8
	dc.b   8
	dc.b   8
	dc.b   8
	dc.b   8
	dc.b   8
	dc.b   0	; Special (actually has a height of 8)
	dc.b 120
	even
; ===========================================================================
; loc_D27C:
SwScrl_CPZ:
	; Update scroll flags, to dynamically load more of the background as
	; the player moves around.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#5,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#6,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Ditto.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#7,d4
	moveq	#scroll_flag_advanced_bg2_left,d6
	bsr.w	SetHorizScrollFlagsBG2

	; Update 'Camera_BG2_Y_pos'.
	move.w	(Camera_BG_Y_pos).w,d0
	move.w	d0,(Camera_BG2_Y_pos).w

	; Update the background's vertical scrolling.
	move.w	d0,(Vscroll_Factor_BG).w

	; Merge BG1's and BG2's scroll flags into BG3...
	move.b	(Scroll_flags_BG).w,d0
	or.b	(Scroll_flags_BG2).w,d0
	move.b	d0,(Scroll_flags_BG3).w

	; ...then clear BG1's and BG2's scroll flags.
	; This zone basically uses its own dynamic background loader.
	clr.b	(Scroll_flags_BG).w
	clr.b	(Scroll_flags_BG2).w

	; Every 8 frames, subtract 1 from 'TempArray_LayerDef'.
	; This animates the 'special line block'.
	move.b	(Vint_runcount+3).w,d1
	andi.w	#7,d1
	bne.s	+
	subq.w	#1,(TempArray_LayerDef).w
+
	lea	(CPZ_CameraSections+1).l,a0
	move.w	(Camera_BG_Y_pos).w,d0
	move.w	d0,d2
	andi.w	#$3F0,d0
	lsr.w	#4,d0
	lea	(a0,d0.w),a0	; 'a0' goes completely unused after this...
	move.w	d0,d4
	; 'd4' now holds the index of the current line block.

	lea	(Horiz_Scroll_Buf).w,a1

    if fixBugs
	move.w	#224/16-1,d1
    else
	; The '+1' is so that, if one block is partially-offscreen at the
	; top, then another will fill the gap at the bottom of the screen.
	; This causes 'Horiz_Scroll_Buf' to overflow due to a lack of
	; bounds-checking. This was likely a deliberate optimisation. Still,
	; it's possible to avoid this without any performance penalty with a
	; little extra code. See below.
	move.w	#224/16+1-1,d1
    endif

	; Set up the foreground part of the horizontal scroll value.
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0

	; Get the offset into the starting block.
	andi.w	#$F,d2

    if fixBugs
	; See above.

	; Back this up, because we'll need it later.
	move.w	d2,d5
	; If this is 0, then we won't need to do an extra block, so skip
	; ahead.
	beq.s	.doLineBlocks
	; Process the first set of line blocks.
	bsr.s	.doLineBlocks

	; Do one last line block.
	moveq	#1-1,d1

	; Invert 'd2' to get the number of lines in the first block that we
	; skipped, so that we can do them now.
	move.w	#$10,d2
	sub.w	d5,d2

	; Process the final line block.
.doLineBlocks:
    endif

	; Behaviour depends on which line block we're processing.
	move.w	(Camera_BG_X_pos).w,d0
	cmpi.b	#18,d4
	beq.s	.doPartialSpecialLineBlock
	blo.s	+
	move.w	(Camera_BG2_X_pos).w,d0
+
	neg.w	d0

	add.w	d2,d2
	jmp	.doPartialLineBlock(pc,d2.w)
; ===========================================================================

.doFullLineBlock:
	; Behaviour depends on which line block we're processing.
	move.w	(Camera_BG_X_pos).w,d0
	cmpi.b	#18,d4
	beq.s	.doFullSpecialLineBlock
	blo.s	+
	move.w	(Camera_BG2_X_pos).w,d0
+
	neg.w	d0

	; This works like a Duff's Device.
.doPartialLineBlock:
    rept 16
	move.l	d0,(a1)+
    endm
	addq.b	#1,d4	; Next line block.
	dbf	d1,.doFullLineBlock
	rts
; ===========================================================================
; loc_D34A:
.doPartialSpecialLineBlock:
	; Invert the offset into the starting block to obtain the number of
	; lines to output minus 1.
	move.w	#$F,d0
	sub.w	d2,d0
	move.w	d0,d2
	bra.s	+
; ===========================================================================
.doFullSpecialLineBlock:
	; A block is 16 lines.
	move.w	#16-1,d2
+
	; The special block row has a ripple effect applied to it.
	move.w	(Camera_BG_X_pos).w,d3
	neg.w	d3
	move.w	(TempArray_LayerDef).w,d0
	andi.w	#$1F,d0
	lea_	SwScrl_RippleData,a2
	lea	(a2,d0.w),a2

.doLine:
	move.b	(a2)+,d0
	ext.w	d0
	add.w	d3,d0
	move.l	d0,(a1)+
	dbf	d2,.doLine

	addq.b	#1,d4	; Next block.
	dbf	d1,.doFullLineBlock
	rts
; ===========================================================================
; loc_D382:
SwScrl_DEZ:
	; Update scroll flags, to dynamically load more of the background as
	; the player moves around.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#8,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#8,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

    if fixBugs
	; The screen shaking is not applied to the background parallax
	; scrolling, causing it to distort. This is trivial to fix: just add
	; the Y component of the shaking to the camera's Y position.
	; This block of code also has to be moved to the start of this
	; function.

	; Handle screen shaking when the final boss explodes.
	moveq	#0,d2
	moveq	#0,d3
	tst.b	(Screen_Shaking_Flag).w
	beq.s	++	; rts
	subq.w	#1,(DEZ_Shake_Timer).w
	bpl.s	+
	clr.b	(Screen_Shaking_Flag).w
+
	move.w	(Level_frame_counter).w,d0
	andi.w	#$3F,d0
	lea_	SwScrl_RippleData,a1
	lea	(a1,d0.w),a1
	moveq	#0,d0
	move.b	(a1)+,d0
	add.w	d0,(Vscroll_Factor_FG).w
	add.w	d0,(Vscroll_Factor_BG).w
	add.w	d0,(Camera_Y_pos_copy).w
	move.w	d0,d3
	move.b	(a1)+,d2
	add.w	d2,(Camera_X_pos_copy).w
+
    endif

	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	move.w	(Camera_X_pos).w,d4
	lea	(TempArray_LayerDef).w,a2

	; Empty space with no stars.
	move.w	d4,(a2)+

	; These seemingly random numbers control how fast each row of stars
	; scrolls by.
	addq.w	#3,(a2)+
	addq.w	#2,(a2)+
	addq.w	#4,(a2)+
	addq.w	#1,(a2)+
	addq.w	#2,(a2)+
	addq.w	#4,(a2)+
	addq.w	#3,(a2)+
	addq.w	#4,(a2)+
	addq.w	#2,(a2)+
	addq.w	#6,(a2)+
	addq.w	#3,(a2)+
	addq.w	#4,(a2)+
	addq.w	#1,(a2)+
	addq.w	#2,(a2)+
	addq.w	#4,(a2)+
	addq.w	#3,(a2)+
	addq.w	#2,(a2)+
	addq.w	#3,(a2)+
	addq.w	#4,(a2)+
	addq.w	#1,(a2)+
	addq.w	#3,(a2)+
	addq.w	#4,(a2)+
	addq.w	#2,(a2)+
	addq.w	#1,(a2)

	; This is to make one row go at half speed (1 pixel every other
	; frame).
	move.w	(a2)+,d0
	moveq	#0,d1
	move.w	d0,d1
	lsr.w	#1,d0
	move.w	d0,(a2)+

	; More star speeds...
	addq.w	#3,(a2)+
	addq.w	#2,(a2)+
	addq.w	#4,(a2)+

	; Now do Earth.
	swap	d1
	move.l	d1,d0
	lsr.l	#3,d1
	sub.l	d1,d0
	swap	d0
	move.w	d0,4(a2)

	swap	d0
	sub.l	d1,d0
	swap	d0
	move.w	d0,2(a2)

	swap	d0
	sub.l	d1,d0
	swap	d0
	move.w	d0,(a2)+

	; Skip past the rows we just did.
	addq.w	#2*2,a2

	addq.w	#1,(a2)+

	; Do the sky.
	move.w	d4,(a2)+
	move.w	d4,(a2)+
	move.w	d4,(a2)+

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	lea	(SwScrl_DEZ_RowHeights).l,a3
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1
    if fixBugs
	; Apply screen shaking effect to the background parallax scrolling.
	add.w	d3,d1
    endif

	moveq	#0,d0

	; Find the first visible scrolling section
.segmentLoop:
	move.b	(a3)+,d0		; Number of lines in this segment
	addq.w	#2,a2
	sub.w	d0,d1			; Does this segment have any visible lines?
	bcc.s	.segmentLoop		; Branch if not

	neg.w	d1			; d1 = number of lines to draw in this segment
	subq.w	#2,a2
	move.w	#224-1,d2		; Number of rows in hscroll buffer
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.rowLoop:
	move.l	d0,(a1)+
	subq.w	#1,d1			; Has the current segment finished?
	bne.s	.nextRow		; Branch if not
	move.b	(a3)+,d1		; Fetch a new line count
	move.w	(a2)+,d0		; Fetch scroll value for this row...
	neg.w	d0			; ...and flip sign for VDP

.nextRow:
	dbf	d2,.rowLoop

    if ~~fixBugs
	; The screen shaking is not applied to the background parallax
	; scrolling, causing it to distort. This is trivial to fix: just add
	; the Y component of the shaking to the camera's Y position.
	; This block of code also has to be moved to the start of this
	; function.

	; Handle screen shaking when the final boss explodes.
	moveq	#0,d2
	tst.b	(Screen_Shaking_Flag).w
	beq.s	++	; rts
	subq.w	#1,(DEZ_Shake_Timer).w
	bpl.s	+
	clr.b	(Screen_Shaking_Flag).w
+
	move.w	(Level_frame_counter).w,d0
	andi.w	#$3F,d0
	lea_	SwScrl_RippleData,a1
	lea	(a1,d0.w),a1
	moveq	#0,d0
	move.b	(a1)+,d0
	add.w	d0,(Vscroll_Factor_FG).w
	add.w	d0,(Vscroll_Factor_BG).w
	add.w	d0,(Camera_Y_pos_copy).w
	move.b	(a1)+,d2
	add.w	d2,(Camera_X_pos_copy).w
+
    endif

	rts
; ===========================================================================
; byte_D48A:
SwScrl_DEZ_RowHeights:
	; Empty space.
	dc.b 128
	; Stars.
	dc.b   8	; 1
	dc.b   8	; 2
	dc.b   8	; 3
	dc.b   8	; 4
	dc.b   8	; 5
	dc.b   8	; 6
	dc.b   8	; 7
	dc.b   8	; 8
	dc.b   8	; 9
	dc.b   8	; 10
	dc.b   8	; 11
	dc.b   8	; 12
	dc.b   8	; 13
	dc.b   8	; 14
	dc.b   8	; 15
	dc.b   8	; 16
	dc.b   8	; 17
	dc.b   8	; 18
	dc.b   8	; 19
	dc.b   8	; 20
	dc.b   8	; 21
	dc.b   8	; 22
	dc.b   8	; 23
	dc.b   8	; 24
	dc.b   8	; 25
	dc.b   8	; 26
	dc.b   8	; 27
	dc.b   8	; 28
	; The edge of Earth.
	dc.b   3	; 29
	dc.b   5	; 30
	dc.b   8	; 31
	dc.b  16	; 32
	; The sky.
	dc.b 128	; 33
	dc.b 128	; 34
	dc.b 128	; 35
	even
; ===========================================================================
; loc_D4AE:
SwScrl_ARZ:
	; Update scroll flags, to dynamically load more of the background as
	; the player moves around.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	muls.w	#281,d4
	moveq	#scroll_flag_bg1_left,d6
	bsr.w	SetHorizScrollFlagsBG_ARZ

	; Ditto.
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#7,d5
	; Curiously, the background moves vertically at different speeds
	; depending on what the current act is.
	tst.b	(Current_Act).w
	bne.s	+
	asl.l	#1,d5
+
	moveq	#scroll_flag_bg1_up_whole_row_2,d6
	bsr.w	SetVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Handle the screen shaking during the boss fight.
	moveq	#0,d2
    if fixBugs
	; The screen shaking is not applied to the background parallax
	; scrolling, causing it to distort. This is trivial to fix: just add
	; the Y component of the shaking to the camera's Y position.
	moveq	#0,d3
    endif
	tst.b	(Screen_Shaking_Flag).w
	beq.s	.screenNotShaking

	move.w	(Level_frame_counter).w,d0
	andi.w	#$3F,d0
	lea_	SwScrl_RippleData,a1
	lea	(a1,d0.w),a1
	moveq	#0,d0
	; Shake camera Y-pos
	move.b	(a1)+,d0
	add.w	d0,(Vscroll_Factor_FG).w
	add.w	d0,(Vscroll_Factor_BG).w
	add.w	d0,(Camera_Y_pos_copy).w
    if fixBugs
	; Ditto
	move.w d0,d3
    endif
	; Shake camera X-pos
	move.b	(a1)+,d2
	add.w	d2,(Camera_X_pos_copy).w

.screenNotShaking:
	; Populate a list of horizontal scroll values for each row.
	; The background is broken up into multiple rows of arbitrary
	; heights, with each row getting its own scroll value.
	; This is used to create an elaborate parallax effect.
	lea	(TempArray_LayerDef).w,a2	; Starts at BG scroll row 1
	lea	3*2(a2),a3			; Starts at BG scroll row 4

	; Set up the speed of each row (there are 16 rows in total)
	move.w	(Camera_X_pos).w,d0
	ext.l	d0
	asl.l	#4,d0
	divs.w	#10,d0
	ext.l	d0
	asl.l	#4,d0
	asl.l	#8,d0
	move.l	d0,d1

	; Set row 4's speed
	swap	d1
	move.w	d1,(a3)+	; Top row of background moves 10 times slower than foreground
	swap	d1
	add.l	d1,d1
	add.l	d0,d1
	; Set rows 5-10's speed
    rept 6
	swap	d1
	move.w	d1,(a3)+	; Next row moves 3 times faster than top row, then next row is 4 times faster, then 5, etc.
	swap	d1
	add.l	d0,d1
    endm
	; Set row 11's speed
	swap	d1
	move.w	d1,(a3)+

	; These instructions reveal that ARZ had slightly different scrolling,
	; at one point:
	; Above the background's mountains is a row of leaves, which is actually
	; composed of three separately-scrolling rows. According to this code,
	; the first and third rows were meant to scroll at a different speed to the
	; second. Possibly due to how bad it looks, the speed values are overwritten
	; a few instructions later, so all three move at the same speed.
	; This code seems to pre-date the Simon Wai build, which uses the same
	; scrolling as the final game.
	move.w	d1,(a2)		; Set row 1's speed
	move.w	d1,4(a2)	; Set row 3's speed

	move.w	(Camera_BG_X_pos).w,d0
	move.w	d0,2(a2)	; Set row 2's speed
	move.w	d0,$16(a2)	; Set row 12's speed
	_move.w	d0,0(a2)	; Overwrite row 1's speed (now same as row 2's)
	move.w	d0,4(a2)	; Overwrite row 3's speed (now same as row 2's)
	move.w	d0,12*2(a2)	; Set row 13's speed
	move.w	d0,13*2(a2)	; Set row 14's speed
	move.w	d0,14*2(a2)	; Set row 15's speed
	move.w	d0,15*2(a2)	; Set row 16's speed

	; Use the list of row scroll values and a list of row heights to fill
	; 'Horiz_Scroll_Buf'.
	lea	(SwScrl_ARZ_RowHeights).l,a3
	lea	(TempArray_LayerDef).w,a2
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	(Camera_BG_Y_pos).w,d1
    if fixBugs
	; Ditto
	add.w	d3,d1
    endif
	moveq	#0,d0

	; Find which row of background is visible at the top of the screen
.findTopRowLoop:
	move.b	(a3)+,d0	; Get row height
	addq.w	#2,a2		; Next row speed (note: is off by 2. This is fixed below)
	sub.w	d0,d1
	bcc.s	.findTopRowLoop	; If current row is above the screen, loop and do next row

	neg.w	d1		; d1 now contains how many pixels of the row is currently on-screen
	subq.w	#2,a2		; Get correct row speed

	move.w	#224-1,d2 	; Height of screen
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0		; Store FG X-pos in upper 16-bits...
	move.w	(a2)+,d0	; ...and BG X-pos in lower 16 bits, as Horiz_Scroll_Buf expects it
	neg.w	d0

-	move.l	d0,(a1)+	; Write 1 FG Horizontal Scroll value, and 1 BG Horizontal Scroll value
	subq.w	#1,d1		; Loop until row at top of screen is done
	bne.s	+
	move.b	(a3)+,d1	; Once that row is done, go to next row...
	move.w	(a2)+,d0	; ...and use next speed
	neg.w	d0
+	dbf	d2,-		; Loop until Horiz_Scroll_Buf is full

	rts
; ===========================================================================
; byte_D5CE:
SwScrl_ARZ_RowHeights:
	dc.b 176
	dc.b 112	; 1
	dc.b  48	; 2
	dc.b  96	; 3
	dc.b  21	; 4
	dc.b  12	; 5
	dc.b  14	; 6
	dc.b   6	; 7
	dc.b  12	; 8
	dc.b  31	; 9
	dc.b  48	; 10
	dc.b 192	; 11
	dc.b 240	; 12
	dc.b 240	; 13
	dc.b 240	; 14
	dc.b 240	; 15
	even
; ===========================================================================
; loc_D5DE:
SwScrl_SCZ:
	tst.w	(Debug_placement_mode).w
	bne.w	SwScrl_Minimal

	; Set the flags to dynamically load the foreground manually. This is
	; normally done in 'DeformBgLayer'.
	lea	(Camera_X_pos).w,a1
	lea	(Scroll_flags).w,a3
	lea	(Camera_X_pos_diff).w,a4
	move.w	(Tornado_Velocity_X).w,d0
	move.w	(a1),d4
	add.w	(a1),d0
	move.w	d0,d1
	sub.w	(a1),d1
	asl.w	#8,d1
	move.w	d0,(a1)
	move.w	d1,(a4)
	lea	(Horiz_block_crossed_flag).w,a2
	bsr.w	SetHorizScrollFlags

	; Ditto.
	lea	(Camera_Y_pos).w,a1
	lea	(Camera_Y_pos_diff).w,a4
	move.w	(Tornado_Velocity_Y).w,d0
	move.w	(a1),d4
	add.w	(a1),d0
	move.w	d0,d1
	sub.w	(a1),d1
	asl.w	#8,d1
	move.w	d0,(a1)
	move.w	d1,(a4)
	lea	(Verti_block_crossed_flag).w,a2
	bsr.w	SetVertiScrollFlags

	; Update scroll flags, to dynamically load more of the background as
	; the player moves around.
	move.w	(Camera_X_pos_diff).w,d4
	beq.s	+
	move.w	#$100,d4
+
	ext.l	d4
	asl.l	#7,d4
	moveq	#0,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	#224-1,d1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos).w,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-

	rts
; ===========================================================================
; loc_D666:
SwScrl_Minimal:
	; Set the flags to dynamically load the background as it moves.
	move.w	(Camera_X_pos_diff).w,d4
	ext.l	d4
	asl.l	#5,d4
	move.w	(Camera_Y_pos_diff).w,d5
	ext.l	d5
	asl.l	#6,d5
	bsr.w	SetHorizVertiScrollFlagsBG

	; Update the background's vertical scrolling.
	move.w	(Camera_BG_Y_pos).w,(Vscroll_Factor_BG).w

	; Update the background's (and foreground's) horizontal scrolling.
	; This is very basic: there is no parallax effect here.
	lea	(Horiz_Scroll_Buf).w,a1
	move.w	#224-1,d1
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0
	move.w	(Camera_BG_X_pos).w,d0
	neg.w	d0

-	move.l	d0,(a1)+
	dbf	d1,-

	rts
; ===========================================================================
; unused...
; loc_D69E:
SwScrl_HPZ_Continued:
	lea	(Horiz_Scroll_Buf).w,a1

    if fixBugs
	move.w	#224/16-1,d1
    else
	; The '+1' is so that, if one block is partially-offscreen at the
	; top, then another will fill the gap at the bottom of the screen.
	; This causes 'Horiz_Scroll_Buf' to overflow due to a lack of
	; bounds-checking. This was likely a deliberate optimisation. Still,
	; it's possible to avoid this without any performance penalty with a
	; little extra code. See below.
	move.w	#224/16+1-1,d1
    endif

	; Set up the foreground part of the horizontal scroll value.
	move.w	(Camera_X_pos).w,d0
	neg.w	d0
	swap	d0

	andi.w	#$F,d2

    if fixBugs
	; See above.

	; Back this up, because we'll need it later.
	move.w	d2,d5
	; If this is 0, then we won't need to do an extra block, so skip
	; ahead.
	beq.s	.doLineBlocks
	; Process the first set of line blocks.
	bsr.s	.doLineBlocks

	; Do one last line block.
	moveq	#1-1,d1

	; Invert 'd2' to get the number of lines in the first block that we
	; skipped, so that we can do them now.
	move.w	#$10,d2
	sub.w	d5,d2

	; Process the final line block.
.doLineBlocks:
    endif

	; Turn d2 into an offset into '.doPartialLineBlock' (each instruction
	; is 2 bytes long).
	add.w	d2,d2
	; Get line block scroll value.
	move.w	(a2)+,d0
	; Output the first line block.
	jmp	.doPartialLineBlock(pc,d2.w)
; ===========================================================================

.doFullLineBlock:
	; Get next line block scroll value.
	move.w	(a2)+,d0

	; This works like a Duff's Device.
.doPartialLineBlock:
    rept 16
	move.l	d0,(a1)+
    endm
	dbf	d1,.doFullLineBlock

	rts

; ---------------------------------------------------------------------------
; Subroutine to set horizontal scroll flags
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_D6E2:
SetHorizScrollFlags:
	move.w	(a1),d0		; get camera X pos
	andi.w	#$10,d0
	move.b	(a2),d1
	eor.b	d1,d0		; has the camera crossed a 16-pixel boundary?
	bne.s	++		; if not, branch
	eori.b	#$10,(a2)
	move.w	(a1),d0		; get camera X pos
	sub.w	d4,d0		; subtract previous camera X pos
	bpl.s	+		; branch if the camera has moved forward
	bset	#scroll_flag_fg_left,(a3)	; set moving back in level bit
	rts
; ===========================================================================
+
	bset	#scroll_flag_fg_right,(a3)	; set moving forward in level bit
+
	rts
; End of function SetHorizScrollFlags

; ---------------------------------------------------------------------------
; Subroutine to scroll the camera horizontally
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_D704:
ScrollHoriz:
	move.w	(a1),d4		; get camera X pos
	tst.b	(Teleport_flag).w
	bne.s	.return		; if a teleport is in progress, return
    if fixBugs
	; To prevent the bug that is described below, this caps the position
	; array index offset so that it does not access position data from
	; before the spin dash was performed. Note that this required
	; modifications to 'Sonic_UpdateSpindash' and 'Tails_UpdateSpindash'.
	move.b	Horiz_scroll_delay_val-Camera_Delay(a5),d1	; should scrolling be delayed?
	beq.s	.scrollNotDelayed				; if not, branch
	lsl.b	#2,d1		; multiply by 4, the size of a position buffer entry
	subq.b	#1,Horiz_scroll_delay_val-Camera_Delay(a5)	; reduce delay value
	move.b	Sonic_Pos_Record_Index+1-Camera_Delay(a5),d0
	sub.b	Horiz_scroll_delay_val+1-Camera_Delay(a5),d0
	cmp.b	d0,d1
	blo.s	.doNotCap
	move.b	d0,d1
.doNotCap:
    else
	; The intent of this code is to make the camera briefly lag behind the
	; player right after releasing a spin dash, however it does this by
	; simply making the camera use position data from previous frames. This
	; means that if the camera had been moving recently enough, then
	; releasing a spin dash will cause the camera to jerk around instead of
	; remain still. This can be encountered by running into a wall, and
	; quickly turning around and spin dashing away. Sonic 3 would have had
	; this same issue with the Fire Shield's dash abiliity, but it shoddily
	; works around the issue by resetting the old position values to the
	; current position (see 'Reset_Player_Position_Array').
	move.w	Horiz_scroll_delay_val-Camera_Delay(a5),d1	; should scrolling be delayed?
	beq.s	.scrollNotDelayed				; if not, branch
	subi.w	#$100,d1					; reduce delay value
	move.w	d1,Horiz_scroll_delay_val-Camera_Delay(a5)
	moveq	#0,d1
	move.b	Horiz_scroll_delay_val-Camera_Delay(a5),d1	; get delay value
	lsl.b	#2,d1		; multiply by 4, the size of a position buffer entry
	addq.b	#4,d1
    endif
	move.w	Sonic_Pos_Record_Index-Camera_Delay(a5),d0	; get current position buffer index
	sub.b	d1,d0
	move.w	(a6,d0.w),d0	; get Sonic's position a certain number of frames ago
	andi.w	#$3FFF,d0
	bra.s	.checkIfShouldScroll	; use that value for scrolling
; ===========================================================================
; loc_D72E:
.scrollNotDelayed:
	move.w	x_pos(a0),d0
; loc_D732:
.checkIfShouldScroll:
	sub.w	(a1),d0
	subi.w	#(320/2)-16,d0		; is the player less than 144 pixels from the screen edge?
	blt.s	.scrollLeft	; if he is, scroll left
	subi.w	#16,d0		; is the player more than 159 pixels from the screen edge?
	bge.s	.scrollRight	; if he is, scroll right
	clr.w	(a4)		; otherwise, don't scroll
; return_D742:
.return:
	rts
; ===========================================================================
; loc_D744:
.scrollLeft:
	cmpi.w	#-16,d0
	bgt.s	.maxNotReached
	move.w	#-16,d0		; limit scrolling to 16 pixels per frame
; loc_D74E:
.maxNotReached:
	add.w	(a1),d0						; get new camera position
	cmp.w	Camera_Min_X_pos-Camera_Boundaries(a2),d0	; is it greater than the minimum position?
	bgt.s	.doScroll					; if it is, branch
	move.w	Camera_Min_X_pos-Camera_Boundaries(a2),d0	; prevent camera from going any further back
	bra.s	.doScroll
; ===========================================================================
; loc_D758:
.scrollRight:
	cmpi.w	#16,d0
	blo.s	.maxNotReached2
	move.w	#16,d0
; loc_D762:
.maxNotReached2:
	add.w	(a1),d0						; get new camera position
	cmp.w	Camera_Max_X_pos-Camera_Boundaries(a2),d0	; is it less than the max position?
	blt.s	.doScroll					; if it is, branch
	move.w	Camera_Max_X_pos-Camera_Boundaries(a2),d0	; prevent camera from going any further forward
; loc_D76E:
.doScroll:
	move.w	d0,d1
	sub.w	(a1),d1		; subtract old camera position
	asl.w	#8,d1		; shift up by a byte
	move.w	d0,(a1)		; set new camera position
	move.w	d1,(a4)		; set difference between old and new positions
	rts
; End of function ScrollHoriz

; ---------------------------------------------------------------------------
; Subroutine to scroll the camera vertically
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; The upper 16 bits of Camera_Y_pos is the actual Y-pos, the lower ones seem
; unused, yet this code goes to a strange extent to manage them.
;sub_D77A:
ScrollVerti:
	moveq	#0,d1
	move.w	y_pos(a0),d0
	sub.w	(a1),d0		; subtract camera Y pos
	cmpi.w	#-$100,(Camera_Min_Y_pos).w ; does the level wrap vertically?
	bne.s	.noWrap		; if not, branch
	andi.w	#$7FF,d0
; loc_D78E:
.noWrap:
	btst	#2,status(a0)	; is the player rolling?
	beq.s	.notRolling	; if not, branch
	subq.w	#5,d0		; subtract difference between standing and rolling heights
    if fixBugs
	; Tails is shorter than Sonic, so the above subtraction actually
	; causes the camera to jolt slightly when he goes from standing to
	; rolling, and vice versa. Not even Sonic 3 & Knuckles fixed this.
	; To fix this, just adjust the subtraction to suit Tails (who is four
	; pixels shorter).
	cmpi.b	#ObjID_Tails,id(a0)
	bne.s	.notRolling
	addq.w	#4,d0
    endif
; loc_D798:
.notRolling:
	btst	#1,status(a0)			; is the player in the air?
	beq.s	.checkBoundaryCrossed_onGround	; if not, branch
;.checkBoundaryCrossed_inAir:
	; If Sonic's in the air, he has $20 pixels above and below him to move without disturbing the camera.
	; The camera movement is also only capped at $10 pixels.
	addi.w	#$20,d0
	sub.w	d3,d0
	bcs.s	.doScroll_fast	; If Sonic is above the boundary, scroll to catch up to him
	subi.w	#$40,d0
	bcc.s	.doScroll_fast	; If Sonic is below the boundary, scroll to catch up to him
	tst.b	(Camera_Max_Y_Pos_Changing).w	; is the max Y pos changing?
	bne.s	.scrollUpOrDown_maxYPosChanging	; if it is, branch
	bra.s	.doNotScroll
; ===========================================================================
; loc_D7B6:
.checkBoundaryCrossed_onGround:
	; On the ground, the camera follows Sonic very strictly.
	sub.w	d3,d0				; subtract camera bias
	bne.s	.decideScrollType		; If Sonic has moved, scroll to catch up to him
	tst.b	(Camera_Max_Y_Pos_Changing).w	; is the max Y pos changing?
	bne.s	.scrollUpOrDown_maxYPosChanging	; if it is, branch
; loc_D7C0:
.doNotScroll:
	clr.w	(a4)		; clear Y position difference (Camera_Y_pos_diff)
	rts
; ===========================================================================
; loc_D7C4:
.decideScrollType:
	cmpi.w	#(224/2)-16,d3	; is the camera bias normal?
	bne.s	.doScroll_slow	; if not, branch
	mvabs.w	inertia(a0),d1	; get player ground velocity, force it to be positive
	cmpi.w	#$800,d1	; is the player travelling very fast?
	bhs.s	.doScroll_fast	; if he is, branch
;.doScroll_medium:
	move.w	#6<<8,d1	; If player is going too fast, cap camera movement to 6 pixels per frame
	cmpi.w	#6,d0		; is player going down too fast?
	bgt.s	.scrollDown_max	; if so, move camera at capped speed
	cmpi.w	#-6,d0		; is player going up too fast?
	blt.s	.scrollUp_max	; if so, move camera at capped speed
	bra.s	.scrollUpOrDown	; otherwise, move camera at player's speed
; ===========================================================================
; loc_D7EA:
.doScroll_slow:
	move.w	#2<<8,d1	; If player is going too fast, cap camera movement to 2 pixels per frame
	cmpi.w	#2,d0		; is player going down too fast?
	bgt.s	.scrollDown_max	; if so, move camera at capped speed
	cmpi.w	#-2,d0		; is player going up too fast?
	blt.s	.scrollUp_max	; if so, move camera at capped speed
	bra.s	.scrollUpOrDown	; otherwise, move camera at player's speed
; ===========================================================================
; loc_D7FC:
.doScroll_fast:
	; related code appears in ScrollBG
	; S3K uses 24 instead of 16
	move.w	#16<<8,d1	; If player is going too fast, cap camera movement to $10 pixels per frame
	cmpi.w	#16,d0		; is player going down too fast?
	bgt.s	.scrollDown_max	; if so, move camera at capped speed
	cmpi.w	#-16,d0		; is player going up too fast?
	blt.s	.scrollUp_max	; if so, move camera at capped speed
	bra.s	.scrollUpOrDown	; otherwise, move camera at player's speed
; ===========================================================================
; loc_D80E:
.scrollUpOrDown_maxYPosChanging:
	moveq	#0,d0		; Distance for camera to move = 0
	move.b	d0,(Camera_Max_Y_Pos_Changing).w	; clear camera max Y pos changing flag
; loc_D814:
.scrollUpOrDown:
	moveq	#0,d1
	move.w	d0,d1		; get position difference
	add.w	(a1),d1		; add old camera Y position
	tst.w	d0		; is the camera to scroll down?
	bpl.w	.scrollDown	; if it is, branch
	bra.w	.scrollUp
; ===========================================================================
; loc_D824:
.scrollUp_max:
	neg.w	d1	; make the value negative (since we're going backwards)
	ext.l	d1
	asl.l	#8,d1	; move this into the upper word, so it lines up with the actual y_pos value in Camera_Y_pos
	add.l	(a1),d1	; add the two, getting the new Camera_Y_pos value
	swap	d1	; actual Y-coordinate is now the low word
; loc_D82E:
.scrollUp:
	cmp.w	Camera_Min_Y_pos-Camera_Boundaries(a2),d1	; is the new position less than the minimum Y pos?
	bgt.s	.doScroll	; if not, branch
	cmpi.w	#-$100,d1
	bgt.s	.minYPosReached
	andi.w	#$7FF,d1
	andi.w	#$7FF,(a1)
	bra.s	.doScroll
; ===========================================================================
; loc_D844:
.minYPosReached:
	move.w	Camera_Min_Y_pos-Camera_Boundaries(a2),d1	; prevent camera from going any further up
	bra.s	.doScroll
; ===========================================================================
; loc_D84A:
.scrollDown_max:
	ext.l	d1
	asl.l	#8,d1		; move this into the upper word, so it lines up with the actual y_pos value in Camera_Y_pos
	add.l	(a1),d1		; add the two, getting the new Camera_Y_pos value
	swap	d1		; actual Y-coordinate is now the low word
; loc_D852:
.scrollDown:
	cmp.w	Camera_Max_Y_pos-Camera_Boundaries(a2),d1	; is the new position greater than the maximum Y pos?
	blt.s	.doScroll	; if not, branch
	subi.w	#$800,d1
	bcs.s	.maxYPosReached
	subi.w	#$800,(a1)
	bra.s	.doScroll
; ===========================================================================
; loc_D864:
.maxYPosReached:
	move.w	Camera_Max_Y_pos-Camera_Boundaries(a2),d1	; prevent camera from going any further down
; loc_D868:
.doScroll:
	move.w	(a1),d4		; get old pos (used by SetVertiScrollFlags)
	swap	d1		; actual Y-coordinate is now the high word, as Camera_Y_pos expects it
	move.l	d1,d3
	sub.l	(a1),d3
	ror.l	#8,d3
	move.w	d3,(a4)		; set difference between old and new positions
	move.l	d1,(a1)		; set new camera Y pos
	rts
; End of function ScrollVerti

; ---------------------------------------------------------------------------
; Subroutine to set vertical scroll flags
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


SetVertiScrollFlags:
	move.w	(a1),d0		; get camera Y pos
	andi.w	#$10,d0
	move.b	(a2),d1
	eor.b	d1,d0		; has the camera crossed a 16-pixel boundary?
	bne.s	++		; if not, branch
	eori.b	#$10,(a2)
	move.w	(a1),d0		; get camera Y pos
	sub.w	d4,d0		; subtract old camera Y pos
	bpl.s	+		; branch if the camera has scrolled down
	bset	#scroll_flag_fg_up,(a3)	; set moving up in level bit
	rts
; ===========================================================================
+
	bset	#scroll_flag_fg_down,(a3)	; set moving down in level bit
+
	rts
; End of function SetVertiScrollFlags


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; d4 is horizontal, d5 vertical, derived from $FFFFEEB0 & $FFFFEEB2 respectively

;sub_D89A: ;Hztl_Vrtc_Bg_Deformation:
SetHorizVertiScrollFlagsBG: ; used by lev2, MTZ, HTZ, CPZ, DEZ, SCZ, Minimal
	move.l	(Camera_BG_X_pos).w,d2
	move.l	d2,d0
	add.l	d4,d0	; add x-shift for this frame
	move.l	d0,(Camera_BG_X_pos).w
	move.l	d0,d1
	swap	d1
	andi.w	#$10,d1
	move.b	(Horiz_block_crossed_flag_BG).w,d3
	eor.b	d3,d1
	bne.s	++
	eori.b	#$10,(Horiz_block_crossed_flag_BG).w
	sub.l	d2,d0
	bpl.s	+
	bset	#scroll_flag_bg1_left,(Scroll_flags_BG).w
	bra.s	++
; ===========================================================================
+
	bset	#scroll_flag_bg1_right,(Scroll_flags_BG).w
+
	move.l	(Camera_BG_Y_pos).w,d3
	move.l	d3,d0
	add.l	d5,d0	; add y-shift for this frame
	move.l	d0,(Camera_BG_Y_pos).w
	move.l	d0,d1
	swap	d1
	andi.w	#$10,d1
	move.b	(Verti_block_crossed_flag_BG).w,d2
	eor.b	d2,d1
	bne.s	++	; rts
	eori.b	#$10,(Verti_block_crossed_flag_BG).w
	sub.l	d3,d0
	bpl.s	+
	bset	#scroll_flag_bg1_up,(Scroll_flags_BG).w
	rts
; ===========================================================================
+
	bset	#scroll_flag_bg1_down,(Scroll_flags_BG).w
+
	rts
; End of function SetHorizVertiScrollFlagsBG


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_D904: ;Horizontal_Bg_Deformation:
SetHorizScrollFlagsBG:	; used by WFZ, HTZ, HPZ
	move.l	(Camera_BG_X_pos).w,d2
	move.l	d2,d0
	add.l	d4,d0	; add x-shift for this frame
	move.l	d0,(Camera_BG_X_pos).w
	move.l	d0,d1
	swap	d1
	andi.w	#$10,d1
	move.b	(Horiz_block_crossed_flag_BG).w,d3
	eor.b	d3,d1
	bne.s	++	; rts
	eori.b	#$10,(Horiz_block_crossed_flag_BG).w
	sub.l	d2,d0
	bpl.s	+
	bset	d6,(Scroll_flags_BG).w
	bra.s	++	; rts
; ===========================================================================
+
	addq.b	#1,d6
	bset	d6,(Scroll_flags_BG).w
+
	rts
; End of function SetHorizScrollFlagsBG


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_D938: ;Vertical_Bg_Deformation1:
SetVertiScrollFlagsBG:		;	used by WFZ, HTZ, HPZ, ARZ
	move.l	(Camera_BG_Y_pos).w,d3
	move.l	d3,d0
	add.l	d5,d0	; add y-shift for this frame

;loc_D940: ;Vertical_Bg_Deformation2:
SetVertiScrollFlagsBG2:
	move.l	d0,(Camera_BG_Y_pos).w
	; What this does is set a specific bit in `Scroll_flags_BG`
	; every time the background crosses a vertical 16-pixel boundary
	move.l	d0,d1
	swap	d1
	andi.w	#$10,d1
	move.b	(Verti_block_crossed_flag_BG).w,d2
	eor.b	d2,d1
	bne.s	++	; rts

	eori.b	#$10,(Verti_block_crossed_flag_BG).w
	sub.l	d3,d0
	bpl.s	+
	; Background has moved down
	bset	d6,(Scroll_flags_BG).w
	rts
; ===========================================================================
+
	; Background has moved up
	addq.b	#1,d6
	bset	d6,(Scroll_flags_BG).w
+
	rts
; End of function SetVertiScrollFlagsBG


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_D96C: ;ARZ_Bg_Deformation:
SetHorizScrollFlagsBG_ARZ:	; only used by ARZ
	move.l	(Camera_ARZ_BG_X_pos).w,d0
	add.l	d4,d0
	move.l	d0,(Camera_ARZ_BG_X_pos).w
	lea	(Camera_BG_X_pos).w,a1
	move.w	(a1),d2
	move.w	(Camera_ARZ_BG_X_pos).w,d0
	sub.w	d2,d0
	blo.s	+	; Background has moved to the right
	bhi.s	++	; Background has moved to the left
	rts
; ===========================================================================
+
	; Limit the background's scrolling speed (my guess is that
	; the game can't load more than one column of blocks per frame)
	cmpi.w	#-16,d0
	bgt.s	++
	move.w	#-16,d0
	bra.s	++
; ===========================================================================
+
	cmpi.w	#16,d0
	blo.s	+
	move.w	#16,d0
+
	add.w	(a1),d0
	move.w	d0,(a1)
	move.w	d0,d1
	andi.w	#$10,d1
	move.b	(Horiz_block_crossed_flag_BG).w,d3
	eor.b	d3,d1
	bne.s	++	; rts
	eori.b	#$10,(Horiz_block_crossed_flag_BG).w
	sub.w	d2,d0
	bpl.s	+
	bset	d6,(Scroll_flags_BG).w
	bra.s	++	; rts
; ===========================================================================
+
	addq.b	#1,d6
	bset	d6,(Scroll_flags_BG).w
+
	rts
; End of function SetHorizScrollFlagsBG_ARZ


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_D9C8: ;CPZ_Bg_Deformation:
SetHorizScrollFlagsBG2:	; only used by CPZ
	move.l	(Camera_BG2_X_pos).w,d2
	move.l	d2,d0
	add.l	d4,d0
	move.l	d0,(Camera_BG2_X_pos).w
	move.l	d0,d1
	swap	d1
	andi.w	#$10,d1
	move.b	(Horiz_block_crossed_flag_BG2).w,d3
	eor.b	d3,d1
	bne.s	++	; rts
	eori.b	#$10,(Horiz_block_crossed_flag_BG2).w
	sub.l	d2,d0
	bpl.s	+
	bset	d6,(Scroll_flags_BG2).w
	bra.s	++	; rts
; ===========================================================================
+
	addq.b	#1,d6
	bset	d6,(Scroll_flags_BG2).w
+
	rts
; End of function SetHorizScrollFlagsBG2

; ===========================================================================
; some apparently unused code
;SetHorizScrollFlagsBG3:
	move.l	(Camera_BG3_X_pos).w,d2
	move.l	d2,d0
	add.l	d4,d0
	move.l	d0,(Camera_BG3_X_pos).w
	move.l	d0,d1
	swap	d1
	andi.w	#$10,d1
	move.b	(Horiz_block_crossed_flag_BG3).w,d3
	eor.b	d3,d1
	bne.s	++	; rts
	eori.b	#$10,(Horiz_block_crossed_flag_BG3).w
	sub.l	d2,d0
	bpl.s	+
	bset	d6,(Scroll_flags_BG3).w
	bra.s	++	; rts
; ===========================================================================
+
	addq.b	#1,d6
	bset	d6,(Scroll_flags_BG3).w
+
	rts
; ===========================================================================
; Unused - dead code leftover from S1:
	lea	(VDP_control_port).l,a5
	lea	(VDP_data_port).l,a6
	lea	(Scroll_flags_BG).w,a2
	lea	(Camera_BG_X_pos).w,a3
	lea	(Level_Layout+$80).w,a4
	move.w	#vdpComm(VRAM_Plane_B_Name_Table,VRAM,WRITE)>>16,d2
	bsr.w	Draw_BG1
	lea	(Scroll_flags_BG2).w,a2
	lea	(Camera_BG2_X_pos).w,a3
	bra.w	Draw_BG2

; ===========================================================================




; ---------------------------------------------------------------------------
; Subroutine to display correct tiles as you move
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; loc_DA5C:
LoadTilesAsYouMove:
	lea	(VDP_control_port).l,a5
	lea	(VDP_data_port).l,a6
	lea	(Scroll_flags_BG_copy).w,a2
	lea	(Camera_BG_copy).w,a3
	lea	(Level_Layout+$80).w,a4	; first background line
	move.w	#vdpComm(VRAM_Plane_B_Name_Table,VRAM,WRITE)>>16,d2
	bsr.w	Draw_BG1

	lea	(Scroll_flags_BG2_copy).w,a2	; referred to in CPZ deformation routine, but cleared right after
	lea	(Camera_BG2_copy).w,a3
	bsr.w	Draw_BG2	; Essentially unused, though

	lea	(Scroll_flags_BG3_copy).w,a2
	lea	(Camera_BG3_copy).w,a3
	bsr.w	Draw_BG3	; used in CPZ deformation routine

	tst.w	(Two_player_mode).w
	beq.s	+
	lea	(Scroll_flags_copy_P2).w,a2
	lea	(Camera_P2_copy).w,a3	; second player camera
	lea	(Level_Layout).w,a4
	move.w	#vdpComm(VRAM_Plane_A_Name_Table_2P,VRAM,WRITE)>>16,d2
	bsr.w	Draw_FG_P2

+
	lea	(Scroll_flags_copy).w,a2
	lea	(Camera_RAM_copy).w,a3
	lea	(Level_Layout).w,a4
	move.w	#vdpComm(VRAM_Plane_A_Name_Table,VRAM,WRITE)>>16,d2

	tst.b	(Screen_redraw_flag).w
	beq.s	Draw_FG

	move.b	#0,(Screen_redraw_flag).w

	moveq	#-16,d4	; X (relative to camera)
	moveq	#(1+224/16+1)-1,d6 ; Cover the screen, plus an extra row at the top and bottom.
; loc_DACE:
Draw_All:
	; Redraw the whole screen.
	movem.l	d4-d6,-(sp)
	moveq	#-16,d5	; X (relative)
	move.w	d4,d1
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	d1,d4
	moveq	#-16,d5	; X (relative)
	bsr.w	DrawBlockRow	; draw the current row
	movem.l	(sp)+,d4-d6
	addi.w	#16,d4		; move onto the next row
	dbf	d6,Draw_All	; repeat for all rows

	move.b	#0,(Scroll_flags_copy).w

	rts
; ===========================================================================
; loc_DAF6:
Draw_FG:
	tst.b	(a2)		; is any scroll flag set?
	beq.s	return_DB5A	; if not, branch

	bclr	#scroll_flag_fg_up,(a2)	; has the level scrolled up?
	beq.s	+			; if not, branch
	moveq	#-16,d4
	moveq	#-16,d5
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4
	moveq	#-16,d5
	bsr.w	DrawBlockRow	; redraw upper row
+
	bclr	#scroll_flag_fg_down,(a2)	; has the level scrolled down?
	beq.s	+			; if not, branch
	move.w	#224,d4
	moveq	#-16,d5
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#224,d4
	moveq	#-16,d5
	bsr.w	DrawBlockRow	; redraw bottom row
+
	bclr	#scroll_flag_fg_left,(a2)	; has the level scrolled to the left?
	beq.s	+			; if not, branch
	moveq	#-16,d4
	moveq	#-16,d5
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4
	moveq	#-16,d5
	bsr.w	DrawBlockColumn	; redraw left-most column
+
	bclr	#scroll_flag_fg_right,(a2)	; has the level scrolled to the right?
	beq.s	return_DB5A		; if not, return
	moveq	#-16,d4
	move.w	#320,d5
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4
	move.w	#320,d5
	bsr.w	DrawBlockColumn	; redraw right-most column

return_DB5A:
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_DB5C:
Draw_FG_P2:
	tst.b	(a2)
	beq.s	return_DBC0

	bclr	#scroll_flag_fg_up,(a2)
	beq.s	+
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer2
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	DrawBlockRow
+
	bclr	#scroll_flag_fg_down,(a2)
	beq.s	+
	move.w	#224,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer2
	move.w	#224,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	DrawBlockRow
+
	bclr	#scroll_flag_fg_left,(a2)
	beq.s	+
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer2
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	DrawBlockColumn
+
	bclr	#scroll_flag_fg_right,(a2)
	beq.s	return_DBC0
	moveq	#-16,d4	; Y offset
	move.w	#320,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer2
	moveq	#-16,d4	; Y offset
	move.w	#320,d5	; X offset
	bsr.w	DrawBlockColumn

return_DBC0:
	rts
; End of function Draw_FG_P2


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_DBC2:
Draw_BG1:
	tst.b	(a2)
	beq.w	return_DC90

	bclr	#scroll_flag_bg1_up,(a2)
	beq.s	+
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	DrawBlockRow
+
	bclr	#scroll_flag_bg1_down,(a2)
	beq.s	+
	move.w	#224,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#224,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	DrawBlockRow
+
	bclr	#scroll_flag_bg1_left,(a2)
	beq.s	+
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	DrawBlockColumn
+
	bclr	#scroll_flag_bg1_right,(a2)
	beq.s	+
	moveq	#-16,d4	; Y offset
	move.w	#320,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4	; Y offset
	move.w	#320,d5	; X offset
	bsr.w	DrawBlockColumn
+
	bclr	#scroll_flag_bg1_up_whole_row,(a2)
	beq.s	+
	moveq	#-16,d4		; Y offset
	moveq	#0,d5		; X (absolute)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteX
	moveq	#-16,d4
	moveq	#0,d5
	moveq	#512/16-1,d6	; The entire width of the plane in blocks minus 1.
	bsr.w	DrawBlockRow.AbsoluteXCustomWidth
+
	bclr	#scroll_flag_bg1_down_whole_row,(a2)
	beq.s	+
	move.w	#224,d4		; Y offset
	moveq	#0,d5		; X (absolute)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteX
	move.w	#224,d4
	moveq	#0,d5
	moveq	#512/16-1,d6	; The entire width of the plane in blocks minus 1.
	bsr.w	DrawBlockRow.AbsoluteXCustomWidth
+
	; This should be no different than 'scroll_flag_bg1_up_whole_row'.
	; The only difference between the two is that this has a relative X
	; coordinate, but that doesn't matter since the entire row is copied
	; anyway.
	bclr	#scroll_flag_bg1_up_whole_row_2,(a2)
	beq.s	+
	moveq	#-16,d4		; Y offset (relative to camera)
	moveq	#-16,d5		; X offset (relative to camera)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	moveq	#-16,d4
	moveq	#-16,d5
	moveq	#512/16-1,d6	; The entire width of the plane in blocks minus 1.
	bsr.w	DrawBlockRow_CustomWidth
+
	; This should be no different than 'scroll_flag_bg1_down_whole_row'.
	; The only difference between the two is that this has a relative X
	; coordinate, but that doesn't matter since the entire row is copied
	; anyway.
	bclr	#scroll_flag_bg1_down_whole_row_2,(a2)
	beq.s	return_DC90
	move.w	#224,d4		; Y offset (relative to camera)
	moveq	#-16,d5		; X offset (relative to camera)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#224,d4
	moveq	#-16,d5
	moveq	#512/16-1,d6	; The entire width of the plane in blocks minus 1.
	bsr.w	DrawBlockRow_CustomWidth

return_DC90:
	rts
; End of function Draw_BG1


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_DC92:
Draw_BG2:
	tst.b	(a2)
	beq.w	++	; rts

	; Leftover from Sonic 1: was used by Green Hill Zone and Spring Yard Zone.
	bclr	#scroll_flag_bg2_left,(a2)
	beq.s	+
	move.w	#112,d4	; Y offset
	moveq	#-16,d5	; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#112,d4	; Y offset
	moveq	#-16,d5	; X offset
	moveq	#3-1,d6	; Only three blocks, which works out to 48 pixels in height.
	bsr.w	DrawBlockColumn.CustomHeight
+
	bclr	#scroll_flag_bg2_right,(a2)
	beq.s	+
	move.w	#112,d4	; Y offset
	move.w	#320,d5		; X offset
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#112,d4	; Y offset
	move.w	#320,d5	; X offset
	moveq	#3-1,d6	; Only three blocks, which works out to 48 pixels in height.
	bsr.w	DrawBlockColumn.CustomHeight
+
	rts
; End of function Draw_BG2

; ===========================================================================
; Scrap Brain Zone 1 block positioning array -- S1 left-over
; Each entry is an index into BGCameraLookup; used to decide the camera to use
; for given block for reloading BG. A entry of 0 means assume X = 0 for section,
; but otherwise loads camera Y for selected camera.
; Note that this list is 32 blocks long, which is enough to span the entire
; two-chunk-tall background.
;byte_DCD6
SBZ_CameraSections:
	; BG1 (draw whole row)
	dc.b 0	; 0
	dc.b 0	; 1
	dc.b 0	; 2
	dc.b 0	; 3
	dc.b 0	; 4
	; BG3
	dc.b 6	; 5
	dc.b 6	; 6
	dc.b 6	; 7
	dc.b 6	; 8
	dc.b 6	; 9
	dc.b 6	; 10
	dc.b 6	; 11
	dc.b 6	; 12
	dc.b 6	; 13
	dc.b 6	; 14
	; BG2
	dc.b 4	; 15
	dc.b 4	; 16
	dc.b 4	; 17
	dc.b 4	; 18
	dc.b 4	; 19
	dc.b 4	; 20
	dc.b 4	; 21
	; BG1
	dc.b 2	; 22
	dc.b 2	; 23
	dc.b 2	; 24
	dc.b 2	; 25
	dc.b 2	; 26
	dc.b 2	; 27
	dc.b 2	; 28
	dc.b 2	; 29
	dc.b 2	; 30
	dc.b 2	; 31
	dc.b 2	; 32

	; Total height: 2 256x256 chunks.
	; This matches the height of the background.

	even

; ===========================================================================
	; Scrap Brain Zone 1 drawing code -- Sonic 1 left-over.

;Draw_BG2_SBZ:
	; Chemical Plant Zone uses a lighty-modified version this code.
	; This is an advanced form of the usual background-drawing code that
	; allows each row of blocks to update and scroll independently...
	; kind of. There are only three possible 'cameras' that each row can
	; align itself with. Still, each row is free to decide which camera
	; it aligns with.
	; This could have really benefitted Oil Ocean Zone's background,
	; which has a section that goes unseen because the regular background
	; drawer is too primitive to display it without making the sun and
	; clouds disappear. Using this would have avoided that.

	; Handle loading the rows as the camera moves up and down.
	moveq	#-16,d4	; Y offset (relative to camera)
	bclr	#scroll_flag_advanced_bg_up,(a2)
	bne.s	.doUpOrDown
	bclr	#scroll_flag_advanced_bg_down,(a2)
	beq.s	.checkIfShouldDoLeftOrRight
	move.w	#224,d4	; Y offset (relative to camera)

.doUpOrDown:
	lea_	SBZ_CameraSections+1,a0
	move.w	(Camera_BG_Y_pos).w,d0
	add.w	d4,d0
	andi.w	#$1F0,d0	; After right-shifting, the is a mask of $1F. Since SBZ_CameraSections is $20 items long, this is correct.
	lsr.w	#4,d0
	move.b	(a0,d0.w),d0
	lea	(BGCameraLookup).l,a3
	movea.w	(a3,d0.w),a3	; Camera, either BG, BG2 or BG3 depending on Y
	beq.s	.doWholeRow
	moveq	#-16,d5	; X offset (relative to camera)
	movem.l	d4-d5,-(sp)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	movem.l	(sp)+,d4-d5
	bsr.w	DrawBlockRow
	bra.s	.checkIfShouldDoLeftOrRight
; ===========================================================================

.doWholeRow:
	moveq	#0,d5	; X (absolute)
	movem.l	d4-d5,-(sp)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteX
	movem.l	(sp)+,d4-d5
	moveq	#512/16-1,d6	; The entire width of the plane in blocks minus 1.
	bsr.w	DrawBlockRow.AbsoluteXCustomWidth

.checkIfShouldDoLeftOrRight:
	; If there are other scroll flags set, then go do them.
	tst.b	(a2)
	bne.s	.doLeftOrRight
	rts
; ===========================================================================

.doLeftOrRight:
	moveq	#-16,d4 ; Y offset

	; Load left column.
	moveq	#-16,d5 ; X offset
	move.b	(a2),d0
	andi.b	#(1<<scroll_flag_advanced_bg1_right)|(1<<scroll_flag_advanced_bg2_right)|(1<<scroll_flag_advanced_bg3_right),d0
	beq.s	+
	lsr.b	#1,d0	; Make the left and right flags share the same bits, to simplify a calculation later.
	move.b	d0,(a2)
	; Load right column.
	move.w	#320,d5 ; X offset
+
	; Select the correct starting background section, and then begin
	; drawing the column.
	lea_	SBZ_CameraSections,a0
	move.w	(Camera_BG_Y_pos).w,d0
	andi.w	#$1F0,d0	; After right-shifting, the is a mask of $1F. Since SBZ_CameraSections is $20 items long, this is correct.
	lsr.w	#4,d0
	lea	(a0,d0.w),a0
	bra.w	DrawBlockColumn_Advanced
; end unused routine

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_DD82:
Draw_BG3:
	tst.b	(a2)
	beq.w	++	; rts

	cmpi.b	#chemical_plant_zone,(Current_Zone).w
	beq.w	Draw_BG3_CPZ
    if fixBugs
	cmpi.b	#oil_ocean_zone,(Current_Zone).w
	beq.w	Draw_BG3_OOZ
    endif

	; Leftover from Sonic 1: was used by Green Hill Zone.
	bclr	#scroll_flag_bg3_left,(a2)
	beq.s	+
	move.w	#64,d4	; Y offset (relative to camera)
	moveq	#-16,d5	; X offset (relative to camera)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#64,d4	; Y offset (relative to camera)
	moveq	#-16,d5	; X offset (relative to camera)
	moveq	#3-1,d6
	bsr.w	DrawBlockColumn.CustomHeight
+
	bclr	#scroll_flag_bg3_right,(a2)
	beq.s	+
	move.w	#64,d4	; Y offset (relative to camera)
	move.w	#320,d5	; X offset (relative to camera)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	#64,d4	; Y offset (relative to camera)
	move.w	#320,d5	; X offset (relative to camera)
	moveq	#3-1,d6
	bsr.w	DrawBlockColumn.CustomHeight
+
	rts
; ===========================================================================
; Chemical Plant Zone block positioning array
; Each entry is an index into BGCameraLookup; used to decide the camera to use
; for given block for reloading BG. Unlike the Scrap Brain Zone version, 0
; does not make X = 0: it's just a duplicate of 2.
;byte_DDD0
CPZ_CameraSections:
	; BG1
	dc.b 2	; 0
	dc.b 2	; 1
	dc.b 2	; 2
	dc.b 2	; 3
	dc.b 2	; 4
	dc.b 2	; 5
	dc.b 2	; 6
	dc.b 2	; 7
	dc.b 2	; 8
	dc.b 2	; 9
	dc.b 2	; 10
	dc.b 2	; 11
	dc.b 2	; 12
	dc.b 2	; 13
	dc.b 2	; 14
	dc.b 2	; 15
	dc.b 2	; 16
	dc.b 2	; 17
	dc.b 2	; 18
	dc.b 2	; 19
	; BG2
	dc.b 4	; 20
	dc.b 4	; 21
	dc.b 4	; 22
	dc.b 4	; 23
	dc.b 4	; 24
	dc.b 4	; 25
	dc.b 4	; 26
	dc.b 4	; 27
	dc.b 4	; 28
	dc.b 4	; 29
	dc.b 4	; 30
	dc.b 4	; 31
	dc.b 4	; 32
	dc.b 4	; 33
	dc.b 4	; 34
	dc.b 4	; 35
	dc.b 4	; 36
	dc.b 4	; 37
	dc.b 4	; 38
	dc.b 4	; 39
	dc.b 4	; 40
	dc.b 4	; 41
	dc.b 4	; 42
	dc.b 4	; 43
	dc.b 4	; 44
	dc.b 4	; 45
	dc.b 4	; 46
	dc.b 4	; 47
	dc.b 4	; 48
	dc.b 4	; 49
	dc.b 4	; 50
	dc.b 4	; 51
	dc.b 4	; 52
	dc.b 4	; 53
	dc.b 4	; 54
	dc.b 4	; 55
	dc.b 4	; 56
	dc.b 4	; 57
	dc.b 4	; 58
	dc.b 4	; 59
	dc.b 4	; 60
	dc.b 4	; 61
	dc.b 4	; 62
	dc.b 4	; 63
	dc.b 4	; 64

	; Total height: 8 128x128 chunks.
	; CPZ's background is only 7 chunks tall, but extending to
	; 8 is necessary for wrapping to be achieved using bitmasks.

	even

; ===========================================================================
; loc_DE12:
Draw_BG3_CPZ:
	; This is a lighty-modified duplicate of Scrap Brain Zone's drawing
	; code (which is still in this game - it's labelled 'Draw_BG2_SBZ').
	; This is an advanced form of the usual background-drawing code that
	; allows each row of blocks to update and scroll independently...
	; kind of. There are only three possible 'cameras' that each row can
	; align itself with. Still, each row is free to decide which camera
	; it aligns with.
	; This could have really benefitted Oil Ocean Zone's background,
	; which has a section that goes unseen because the regular background
	; drawer is too primitive to display it without making the sun and
	; clouds disappear. Using this would have avoided that.
	; This code differs from the Scrap Brain Zone version by being
	; hardcoded to a different table ('CPZ_CameraSections' instead of
	; 'SBZ_CameraSections'), and lacking support for redrawing the whole
	; row when it uses "camera 0".

	; Handle loading the rows as the camera moves up and down.
	moveq	#-16,d4	; Y offset
	bclr	#scroll_flag_advanced_bg_up,(a2)
	bne.s	.doUpOrDown
	bclr	#scroll_flag_advanced_bg_down,(a2)
	beq.s	.checkIfShouldDoLeftOrRight
	move.w	#224,d4	; Y offset

.doUpOrDown:
	; Select the correct camera, so that the X value of the loaded row is
	; right.
	lea_	CPZ_CameraSections+1,a0
	move.w	(Camera_BG_Y_pos).w,d0
	add.w	d4,d0
	andi.w	#$3F0,d0	; After right-shifting, the is a mask of $3F. Since CPZ_CameraSections is $40 items long, this is correct.
	lsr.w	#4,d0
	move.b	(a0,d0.w),d0
	movea.w	BGCameraLookup(pc,d0.w),a3	; Camera, either BG, BG2 or BG3 depending on Y
	moveq	#-16,d5	; X offset
	movem.l	d4-d5,-(sp)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	movem.l	(sp)+,d4-d5
	bsr.w	DrawBlockRow

.checkIfShouldDoLeftOrRight:
	; If there are other scroll flags set, then go do them.
	tst.b	(a2)
	bne.s	.doLeftOrRight
	rts
; ===========================================================================

.doLeftOrRight:
	moveq	#-16,d4 ; Y offset

	; Load left column.
	moveq	#-16,d5 ; X offset
	move.b	(a2),d0
	andi.b	#(1<<scroll_flag_advanced_bg1_right)|(1<<scroll_flag_advanced_bg2_right)|(1<<scroll_flag_advanced_bg3_right),d0
	beq.s	+
	lsr.b	#1,d0	; Make the left and right flags share the same bits, to simplify a calculation later.
	move.b	d0,(a2)
	; Load right column.
	move.w	#320,d5 ; X offset
+
	; Select the correct starting background section, and then begin
	; drawing the column.
	lea_	CPZ_CameraSections,a0
	move.w	(Camera_BG_Y_pos).w,d0
    if fixBugs
	andi.w	#$3F0,d0	; After right-shifting, the is a mask of $3F. Since CPZ_CameraSections is $40 items long, this is correct.
    else
	; After right-shifting, the is a mask of $7F. Since CPZ_CameraSections
	; is $40 items long, this is incorrect, and will cause accesses to
	; exceed the bounds of CPZ_CameraSections and read invalid data. This
	; is most notably a problem in Marble Zone's version of this code.
	andi.w	#$7F0,d0
    endif
	lsr.w	#4,d0
	lea	(a0,d0.w),a0
	bra.w	DrawBlockColumn_Advanced
; ===========================================================================
;word_DE7E
BGCameraLookup:
	dc.w Camera_BG_copy	; BG Camera
	dc.w Camera_BG_copy	; BG Camera
	dc.w Camera_BG2_copy	; BG2 Camera
	dc.w Camera_BG3_copy	; BG3 Camera
; ===========================================================================
; loc_DE86:
DrawBlockColumn_Advanced:
	tst.w	(Two_player_mode).w
	bne.s	.doubleResolution

	moveq	#(1+224/16+1)-1,d6	; Enough blocks to cover the screen, plus one more on the top and bottom.
	move.l	#vdpCommDelta($0080),d7

-
	; If the block is not part of the row that needs updating, then skip
	; drawing it.
	moveq	#0,d0
	move.b	(a0)+,d0
	btst	d0,(a2)
	beq.s	+

	; Get the correct camera and draw this block.
	movea.w	BGCameraLookup(pc,d0.w),a3	; Camera, either BG, BG2 or BG3 depending on Y
	movem.l	d4-d5/a0,-(sp)
	movem.l	d4-d5,-(sp)
	bsr.w	GetBlock
	movem.l	(sp)+,d4-d5
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	bsr.w	ProcessAndWriteBlock_Vertical
	movem.l	(sp)+,d4-d5/a0
+
	; Move onto the next block down.
	addi.w	#16,d4
	dbf	d6,-

	; Clear the scroll flags now that we're done here.
	clr.b	(a2)

	rts
; ===========================================================================

.doubleResolution:
	moveq	#(1+224/16+1)-1,d6	; Enough blocks to cover the screen, plus one more on the top and bottom.
	move.l	#vdpCommDelta($0080),d7

-
	; If the block is not part of the row that needs updating, then skip
	; drawing it.
	moveq	#0,d0
	move.b	(a0)+,d0
	btst	d0,(a2)
	beq.s	+

	; Get the correct camera and draw this block.
	movea.w	BGCameraLookup(pc,d0.w),a3	; Camera, either BG, BG2 or BG3 depending on Y
	movem.l	d4-d5/a0,-(sp)
	movem.l	d4-d5,-(sp)
	bsr.w	GetBlock
	movem.l	(sp)+,d4-d5
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	bsr.w	ProcessAndWriteBlock_DoubleResolution_Vertical
	movem.l	(sp)+,d4-d5/a0
+
	; Move onto the next block down.
	addi.w	#16,d4
	dbf	d6,-

	; Clear the scroll flags now that we're done here.
	clr.b	(a2)

	rts
; End of function Draw_BG3


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
    if fixBugs
	; See 'SwScrl_OOZ'.
	; This uses the same drawing method as Chemical Plant Zone to enable
	; the unused part of Oil Ocean Zone's background to be drawn without
	; it causing the clouds and sun to disappear.

; Oil Ocean Zone block positioning array
; Each entry is an index into BGCameraLookup; used to decide the camera to use
; for given block for reloading BG. A entry of 0 means assume X = 0 for section,
; but otherwise loads camera Y for selected camera.

OOZ_CameraSections:
	; BG1 (draw whole row) for the sky.
	dc.b 0	; 0
	dc.b 0	; 1
	dc.b 0	; 2
	dc.b 0	; 3
	dc.b 0	; 4
	dc.b 0	; 5
	dc.b 0	; 6
	dc.b 0	; 7
	dc.b 0	; 8
	dc.b 0	; 9
	dc.b 0	; 10
	dc.b 0	; 11
	dc.b 0	; 12
	dc.b 0	; 13
	dc.b 0	; 14
	dc.b 0	; 15
	dc.b 0	; 16
	; BG1 for the factory.
	dc.b 2	; 17
	dc.b 2	; 18
	dc.b 2	; 19
	dc.b 2	; 20
	dc.b 2	; 21
	dc.b 2	; 22
	dc.b 2	; 23
	dc.b 2	; 24
	dc.b 2	; 25
	dc.b 2	; 26
	dc.b 2	; 27
	dc.b 2	; 28
	dc.b 2	; 29
	dc.b 2	; 30
	dc.b 2	; 31
	dc.b 2	; 32

	; Total height: 4 128x128 chunks.
	; This matches the height of the background.

	even

; ===========================================================================

Draw_BG3_OOZ:
	; This is a lighty-modified duplicate of Scrap Brain Zone's drawing
	; code (which is still in this game - it's labelled 'Draw_BG2_SBZ').
	; This is an advanced form of the usual background-drawing code that
	; allows each row of blocks to update and scroll independently...
	; kind of. There are only three possible 'cameras' that each row can
	; align itself with. Still, each row is free to decide which camera
	; it aligns with.

	; Handle loading the rows as the camera moves up and down.
	moveq	#-16,d4	; Y offset
	bclr	#scroll_flag_advanced_bg_up,(a2)
	bne.s	.doUpOrDown
	bclr	#scroll_flag_advanced_bg_down,(a2)
	beq.s	.checkIfShouldDoLeftOrRight
	move.w	#224,d4	; Y offset

.doUpOrDown:
	; Select the correct camera, so that the X value of the loaded row is
	; right.
	lea_	OOZ_CameraSections+1,a0
	move.w	(Camera_BG_Y_pos).w,d0
	add.w	d4,d0
	andi.w	#$1F0,d0	; After right-shifting, the is a mask of $1F. Since OOZ_CameraSections is $20 items long, this is correct.
	lsr.w	#4,d0
	move.b	(a0,d0.w),d0
	lea	BGCameraLookup(pc),a3
	movea.w	(a3,d0.w),a3	; Camera, either BG, BG2 or BG3 depending on Y
	beq.s	.doWholeRow
	moveq	#-16,d5	; X offset
	movem.l	d4-d5,-(sp)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	movem.l	(sp)+,d4-d5
	bsr.w	DrawBlockRow
	bra.s	.checkIfShouldDoLeftOrRight
; ===========================================================================

.doWholeRow:
	moveq	#0,d5	; X (absolute)
	movem.l	d4-d5,-(sp)
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteX
	movem.l	(sp)+,d4-d5
	moveq	#512/16-1,d6	; The entire width of the plane in blocks minus 1.
	bsr.w	DrawBlockRow.AbsoluteXCustomWidth

.checkIfShouldDoLeftOrRight:
	; If there are other scroll flags set, then go do them.
	tst.b	(a2)
	bne.s	.doLeftOrRight
	rts
; ===========================================================================

.doLeftOrRight:
	moveq	#-16,d4 ; Y offset

	; Load left column.
	moveq	#-16,d5 ; X offset
	move.b	(a2),d0
	andi.b	#(1<<scroll_flag_advanced_bg1_right)|(1<<scroll_flag_advanced_bg2_right)|(1<<scroll_flag_advanced_bg3_right),d0
	beq.s	+
	lsr.b	#1,d0	; Make the left and right flags share the same bits, to simplify a calculation later.
	move.b	d0,(a2)
	; Load right column.
	move.w	#320,d5 ; X offset
+
	; Select the correct starting background section, and then begin
	; drawing the column.
	lea_	OOZ_CameraSections,a0
	move.w	(Camera_BG_Y_pos).w,d0
	andi.w	#$1F0,d0	; After right-shifting, the is a mask of $1F. Since OOZ_CameraSections is $20 items long, this is correct.
	lsr.w	#4,d0
	lea	(a0,d0.w),a0
	bra.w	DrawBlockColumn_Advanced
    endif

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_DF04: DrawBlockCol1:
DrawBlockColumn:
	moveq	#(1+224/16+1)-1,d6 ; Enough blocks to cover the screen, plus one more on the top and bottom.
; DrawBlockCol2:
.CustomHeight:
	add.w	(a3),d5		; add camera X pos
	add.w	4(a3),d4	; add camera Y pos
	move.l	#vdpCommDelta(64*2),d7	; store VDP command for line increment
	move.l	d0,d1		; copy byte-swapped VDP command for later access
	bsr.w	GetAddressOfBlockInChunk

	tst.w	(Two_player_mode).w
	bne.s	.doubleResolution

-	move.w	(a0),d3		; get ID of the 16x16 block
	andi.w	#$3FF,d3
	lsl.w	#3,d3		; multiply by 8, the size in bytes of a 16x16
	lea	(Block_Table).w,a1
	adda.w	d3,a1		; a1 = address of the current 16x16 in the block table
	move.l	d1,d0
	bsr.w	ProcessAndWriteBlock_Vertical
	adda.w	#128/16*2,a0	; move onto the 16x16 vertically below this one
	addi.w	#64*2*2,d1	; draw on alternate 8x8 lines
	andi.w	#(64*32*2)-1,d1	; wrap around plane (assumed to be in 64x32 mode)
	addi.w	#16,d4		; add 16 to Y offset
	move.w	d4,d0
	andi.w	#$70,d0		; have we reached a new 128x128?
	bne.s	+		; if not, branch
	bsr.w	GetAddressOfBlockInChunk	; otherwise, renew the block address
+	dbf	d6,-		; repeat 16 times

	rts
; ===========================================================================

.doubleResolution:
-	move.w	(a0),d3
	andi.w	#$3FF,d3
	lsl.w	#3,d3
	lea	(Block_Table).w,a1
	adda.w	d3,a1
	move.l	d1,d0
	bsr.w	ProcessAndWriteBlock_DoubleResolution_Vertical
	adda.w	#128/16*2,a0
	addi.w	#$80,d1
	andi.w	#(64*32*2)-1,d1
	addi.w	#16,d4
	move.w	d4,d0
	andi.w	#$70,d0
	bne.s	+
	bsr.w	GetAddressOfBlockInChunk
+	dbf	d6,-

	rts
; End of function DrawBlockColumn


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_DF8A: DrawTiles_Vertical: DrawBlockRow:
DrawBlockRow_CustomWidth:
	add.w	(a3),d5
	add.w	4(a3),d4
	bra.s	DrawBlockRow.AbsoluteXAbsoluteYCustomWidth
; End of function DrawBlockRow_CustomWidth


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_DF92: DrawTiles_Vertical1: DrawBlockRow1:
DrawBlockRow:
	moveq	#(1+320/16+1)-1,d6 ; Just enough blocks to cover the screen.
	add.w	(a3),d5		; add X pos
; loc_DF96: DrawTiles_Vertical2: DrawBlockRow2:
.AbsoluteXCustomWidth:
	add.w	4(a3),d4	; add Y pos
; loc_DF9A: DrawTiles_Vertical3: DrawBlockRow3:
.AbsoluteXAbsoluteYCustomWidth:
	tst.w	(Two_player_mode).w
	bne.s	.doubleResolution

	move.l	a2,-(sp)
	move.w	d6,-(sp)
	lea	(Block_cache).w,a2
	move.l	d0,d1
	or.w	d2,d1
	swap	d1		; make VRAM write command
	move.l	d1,-(sp)
	move.l	d1,(a5)		; set up a VRAM write at that address
	swap	d1
	bsr.w	GetAddressOfBlockInChunk

-	move.w	(a0),d3		; get ID of the 16x16 block
	andi.w	#$3FF,d3
	lsl.w	#3,d3		; multiply by 8, the size in bytes of a 16x16
	lea	(Block_Table).w,a1
	adda.w	d3,a1		; a1 = address of current 16x16 in the block table
	bsr.w	ProcessAndWriteBlock_Horizontal
	addq.w	#2,a0		; move onto next 16x16
	addq.b	#4,d1		; increment VRAM write address
	bpl.s	+
	andi.b	#$7F,d1		; restrict to a single 8x8 line
	swap	d1
	move.l	d1,(a5)		; set up a VRAM write at a new address
	swap	d1
+
	addi.w	#16,d5		; add 16 to X offset
	move.w	d5,d0
	andi.w	#$70,d0		; have we reached a new 128x128?
	bne.s	+		; if not, branch
	bsr.w	GetAddressOfBlockInChunk	; otherwise, renew the block address
+
	dbf	d6,-		; repeat 22 times

	move.l	(sp)+,d1
	addi.l	#vdpCommDelta(64*2),d1	; move onto next line
	lea	(Block_cache).w,a2
	move.l	d1,(a5)		; write to this VRAM address
	swap	d1
	move.w	(sp)+,d6

-	move.l	(a2)+,(a6)	; write stored 8x8s
	addq.b	#4,d1		; increment VRAM write address
	bmi.s	+
	ori.b	#$80,d1		; force to bottom 8x8 line
	swap	d1
	move.l	d1,(a5)		; set up a VRAM write at a new address
	swap	d1
+
	dbf	d6,-		; repeat 22 times

	movea.l	(sp)+,a2
	rts
; ===========================================================================
; loc_E018: DrawBlockRow_2P:
.doubleResolution:
	move.l	d0,d1
	or.w	d2,d1
	swap	d1
	move.l	d1,(a5)
	swap	d1
	tst.b	d1
	bmi.s	+++

	bsr.w	GetAddressOfBlockInChunk

-	move.w	(a0),d3
	andi.w	#$3FF,d3
	lsl.w	#3,d3
	lea	(Block_Table).w,a1
	adda.w	d3,a1
	bsr.w	ProcessAndWriteBlock_DoubleResolution_Horizontal
	addq.w	#2,a0
	addq.b	#4,d1
	bpl.s	+
	andi.b	#$7F,d1
	swap	d1
	move.l	d1,(a5)
	swap	d1
+
	addi.w	#16,d5
	move.w	d5,d0
	andi.w	#$70,d0
	bne.s	+
	bsr.w	GetAddressOfBlockInChunk
+	dbf	d6,-

	rts
; ===========================================================================
+
	bsr.w	GetAddressOfBlockInChunk

-	move.w	(a0),d3
	andi.w	#$3FF,d3
	lsl.w	#3,d3
	lea	(Block_Table).w,a1
	adda.w	d3,a1
	bsr.w	ProcessAndWriteBlock_DoubleResolution_Horizontal
	addq.w	#2,a0
	addq.b	#4,d1
	bmi.s	+
	ori.b	#$80,d1
	swap	d1
	move.l	d1,(a5)
	swap	d1
+
	addi.w	#16,d5
	move.w	d5,d0
	andi.w	#$70,d0
	bne.s	+
	bsr.w	GetAddressOfBlockInChunk
+	dbf	d6,-

	rts
; End of function DrawBlockRow


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E09E: GetBlockAddr:
GetAddressOfBlockInChunk:
	movem.l	d4-d5,-(sp)
	move.w	d4,d3		; d3 = camera Y pos + offset
	add.w	d3,d3
	andi.w	#$F00,d3	; limit to units of $100 ($100 = size of a row of FG and BG 128x128s in level layout table)
	lsr.w	#3,d5		; divide by 8
	move.w	d5,d0
	lsr.w	#4,d0		; divide by 16 (overall division of 128)
	andi.w	#$7F,d0
	add.w	d3,d0		; get offset of current 128x128 in the level layout table
	moveq	#-1,d3
	clr.w	d3		; d3 = $FFFF0000
	move.b	(a4,d0.w),d3	; get tile ID of the current 128x128 tile
	lsl.w	#7,d3		; multiply by 128, the size in bytes of a 128x128 in RAM
	andi.w	#$70,d4		; round down to nearest 16-pixel boundary
	andi.w	#$E,d5		; force this to be a multiple of 16
	add.w	d4,d3		; add vertical offset of current 16x16
	add.w	d5,d3		; add horizontal offset of current 16x16
	movea.l	d3,a0		; store address, in the metablock table, of the current 16x16
	movem.l	(sp)+,d4-d5
	rts
; End of function GetAddressOfBlockInChunk


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E0D4: ProcessAndWriteBlock:
ProcessAndWriteBlock_Horizontal:
	; Compared to 'ProcessAndWriteBlock_Vertical', this caches the bottom
	; two tiles far later writing. This avoids the need to constantly
	; alternate VRAM destinations.
	btst	#3,(a0)		; is this 16x16 to be Y-flipped?
	bne.s	.flipY		; if it is, branch
	btst	#2,(a0)		; is this 16x16 to be X-flipped?
	bne.s	.flipX		; if it is, branch
	move.l	(a1)+,(a6)	; write top two 8x8s to VRAM
	move.l	(a1)+,(a2)+	; store bottom two 8x8s for later writing
	rts
; ===========================================================================
; ProcessAndWriteBlock_FlipX:
.flipX:
	move.l	(a1)+,d3
	eori.l	#(flip_x<<16)|flip_x,d3	; toggle X-flip flag of the 8x8s
	swap	d3		; swap the position of the 8x8s
	move.l	d3,(a6)		; write top two 8x8s to VRAM
	move.l	(a1)+,d3
	eori.l	#(flip_x<<16)|flip_x,d3
	swap	d3
	move.l	d3,(a2)+	; store bottom two 8x8s for later writing
	rts
; ===========================================================================
; ProcessAndWriteBlock_FlipY:
.flipY:
	btst	#2,(a0)		; is this 16x16 to be X-flipped as well?
	bne.s	.flipXY		; if it is, branch
	move.l	(a1)+,d0
	move.l	(a1)+,d3
	eori.l	#(flip_y<<16)|flip_y,d3	; toggle Y-flip flag of the 8x8s
	move.l	d3,(a6)		; write bottom two 8x8s to VRAM
	eori.l	#(flip_y<<16)|flip_y,d0
	move.l	d0,(a2)+	; store top two 8x8s for later writing
	rts
; ===========================================================================
; ProcessAndWriteBlock_FlipXY:
.flipXY:
	move.l	(a1)+,d0
	move.l	(a1)+,d3
	eori.l	#((flip_x|flip_y)<<16)|flip_x|flip_y,d3	; toggle X and Y-flip flags of the 8x8s
	swap	d3
	move.l	d3,(a6)		; write bottom two 8x8s to VRAM
	eori.l	#((flip_x|flip_y)<<16)|flip_x|flip_y,d0
	swap	d0
	move.l	d0,(a2)+	; store top two 8x8s for later writing
	rts
; End of function ProcessAndWriteBlock_Horizontal


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

;sub_E136: ProcessAndWriteBlock_2P:
ProcessAndWriteBlock_DoubleResolution_Horizontal:
	; In two player mode, the VDP's Interlace Mode 2 is enabled, making
	; tiles twice as tall (16x8 instead of 8x8). Because of this, blocks
	; are now composed of only two tiles, arranged side by side.
	btst	#3,(a0)
	bne.s	.flipY
	btst	#2,(a0)
	bne.s	.flipX
	move.l	(a1)+,(a6)
	rts
; ===========================================================================
; loc_E146:
.flipX:
	move.l	(a1)+,d3
	eori.l	#(flip_x<<16)|flip_x,d3
	swap	d3
	move.l	d3,(a6)
	rts
; ===========================================================================
; loc_E154:
.flipY:
	btst	#2,(a0)
	bne.s	.flipXY
	move.l	(a1)+,d3
	eori.l	#(flip_y<<16)|flip_y,d3
	move.l	d3,(a6)
	rts
; ===========================================================================
;loc_E166:
.flipXY:
	move.l	(a1)+,d3
	eori.l	#((flip_x|flip_y)<<16)|flip_x|flip_y,d3
	swap	d3
	move.l	d3,(a6)
	rts
; End of function ProcessAndWriteBlock_DoubleResolution_Horizontal


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E174: ProcessAndWriteBlock2:
ProcessAndWriteBlock_Vertical:
	or.w	d2,d0
	swap	d0		; make VRAM write command
	btst	#3,(a0)		; is the 16x16 to be Y-flipped?
	bne.s	.flipY		; if it is, branch
	btst	#2,(a0)		; is the 16x16 to be X-flipped?
	bne.s	.flipX		; if it is, branch
	move.l	d0,(a5)		; write to this VRAM address
	move.l	(a1)+,(a6)	; write top two 8x8s
	add.l	d7,d0		; move onto next line
	move.l	d0,(a5)
	move.l	(a1)+,(a6)	; write bottom two 8x8s
	rts
; ===========================================================================
; ProcessAndWriteBlock2_FlipX:
.flipX:
	move.l	d0,(a5)
	move.l	(a1)+,d3
	eori.l	#(flip_x<<16)|flip_x,d3	; toggle X-flip flag of the 8x8s
	swap	d3		; swap the position of the 8x8s
	move.l	d3,(a6)		; write top two 8x8s
	add.l	d7,d0		; move onto next line
	move.l	d0,(a5)
	move.l	(a1)+,d3
	eori.l	#(flip_x<<16)|flip_x,d3
	swap	d3
	move.l	d3,(a6)		; write bottom two 8x8s
	rts
; ===========================================================================
; ProcessAndWriteBlock2_FlipY:
.flipY:
	btst	#2,(a0)		; is the 16x16 to be X-flipped as well?
	bne.s	.flipXY		; if it is, branch
	move.l	d5,-(sp)
	move.l	d0,(a5)
	move.l	(a1)+,d5
	move.l	(a1)+,d3
	eori.l	#(flip_y<<16)|flip_y,d3	; toggle Y-flip flag of 8x8s
	move.l	d3,(a6)		; write bottom two 8x8s
	add.l	d7,d0		; move onto next line
	move.l	d0,(a5)
	eori.l	#(flip_y<<16)|flip_y,d5
	move.l	d5,(a6)		; write top two 8x8s
	move.l	(sp)+,d5
	rts
; ===========================================================================
;ProcessAndWriteBlock2_FlipXY:
.flipXY:
	move.l	d5,-(sp)
	move.l	d0,(a5)
	move.l	(a1)+,d5
	move.l	(a1)+,d3
	eori.l	#((flip_x|flip_y)<<16)|flip_x|flip_y,d3	; toggle X and Y-flip flags of 8x8s
	swap	d3		; swap the position of the 8x8s
	move.l	d3,(a6)		; write bottom two 8x8s
	add.l	d7,d0
	move.l	d0,(a5)
	eori.l	#((flip_x|flip_y)<<16)|flip_x|flip_y,d5
	swap	d5
	move.l	d5,(a6)		; write top two 8x8s
	move.l	(sp)+,d5
	rts
; End of function ProcessAndWriteBlock_Vertical


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;sub_E1FA: ProcessAndWriteBlock2_2P:
ProcessAndWriteBlock_DoubleResolution_Vertical:
	or.w	d2,d0
	swap	d0
	btst	#3,(a0)
	bne.s	.flipY
	btst	#2,(a0)
	bne.s	.flipX
	move.l	d0,(a5)
	move.l	(a1)+,(a6)
	rts
; ===========================================================================
; loc_E210:
.flipX:
	move.l	d0,(a5)
	move.l	(a1)+,d3
	eori.l	#(flip_x<<16)|flip_x,d3
	swap	d3
	move.l	d3,(a6)
	rts
; ===========================================================================
; loc_E220:
.flipY:
	btst	#2,(a0)
	bne.s	.flipXY
	move.l	d0,(a5)
	move.l	(a1)+,d3
	eori.l	#(flip_y<<16)|flip_y,d3
	move.l	d3,(a6)
	rts
; ===========================================================================
; loc_E234:
.flipXY:
	move.l	d0,(a5)
	move.l	(a1)+,d3
	eori.l	#((flip_x|flip_y)<<16)|flip_x|flip_y,d3
	swap	d3
	move.l	d3,(a6)
	rts
; End of function ProcessAndWriteBlock_DoubleResolution_Vertical


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E244: GetBlockPtr:
GetBlock:
	add.w	(a3),d5
	add.w	4(a3),d4
	lea	(Block_Table).w,a1
	move.w	d4,d3		; d3 = camera Y pos + offset
	add.w	d3,d3
	andi.w	#$F00,d3	; limit to units of $100 ($100 = $80 * 2, $80 = height of a 128x128)
	lsr.w	#3,d5		; divide by 8
	move.w	d5,d0
	lsr.w	#4,d0		; divide by 16 (overall division of 128)
	andi.w	#$7F,d0
	add.w	d3,d0		; get offset of current 128x128 in the level layout table
	moveq	#-1,d3
	clr.w	d3		; d3 = $FFFF0000
	move.b	(a4,d0.w),d3	; get tile ID of the current 128x128 tile
	lsl.w	#7,d3		; multiply by 128, the size in bytes of a 128x128 in RAM
	andi.w	#$70,d4		; round down to nearest 16-pixel boundary
	andi.w	#$E,d5		; force this to be a multiple of 16
	add.w	d4,d3		; add vertical offset of current 16x16
	add.w	d5,d3		; add horizontal offset of current 16x16
	movea.l	d3,a0		; store address, in the metablock table, of the current 16x16
	move.w	(a0),d3
	andi.w	#$3FF,d3
	lsl.w	#3,d3
	adda.w	d3,a1
	rts
; End of function GetBlock


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E286: Calc_VRAM_Pos: CalcBlockVRAMPos:
CalculateVRAMAddressOfBlockForPlayer1:
	add.w	(a3),d5		; add X pos
; CalcBlockVRAMPos2:
.AbsoluteX:
	tst.w	(Two_player_mode).w
	bne.s	.AbsoluteX_DoubleResolution
	add.w	4(a3),d4	; add Y pos
; CalcBlockVRAMPos_NoCamera:
.AbsoluteXAbsoluteY:
	andi.w	#$F0,d4		; round down to the nearest 16-pixel boundary
	andi.w	#$1F0,d5	; round down to the nearest 16-pixel boundary
	lsl.w	#4,d4		; make it into units of $100 - the height in plane A of a 16x16
	lsr.w	#2,d5		; make it into units of 4 - the width in plane A of a 16x16
	add.w	d5,d4		; combine the two to get final address
	; access a VDP address in plane name table A ($C000) or B ($E000) if d2 has bit 13 unset or set
	moveq	#vdpComm(VRAM_Plane_A_Name_Table,VRAM,WRITE)&$FFFF,d0
	swap	d0
	move.w	d4,d0		; make word-swapped VDP command
	rts
; ===========================================================================
; loc_E2A8: CalcBlockVRAMPos_2P:
.AbsoluteX_DoubleResolution:
	add.w	4(a3),d4
; loc_E2AC: CalcBlockVRAMPos_2P_NoCamera:
.AbsoluteXAbsoluteY_DoubleResolution:
	andi.w	#$1F0,d4
	andi.w	#$1F0,d5
	lsl.w	#3,d4
	lsr.w	#2,d5
	add.w	d5,d4
	; access a VDP address in plane name table A ($C000) or B ($E000) if d2 has bit 13 unset or set
	moveq	#vdpComm(VRAM_Plane_A_Name_Table,VRAM,WRITE)&$FFFF,d0
	swap	d0
	move.w	d4,d0
	rts
; End of function CalculateVRAMAddressOfBlockForPlayer1


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


;loc_E2C2: CalcBlockVRAMPosB:
CalculateVRAMAddressOfBlockForPlayer2:
	tst.w	(Two_player_mode).w
	bne.s	.doubleResolution

;.regularResolution:
	add.w	4(a3),d4
	add.w	(a3),d5
	andi.w	#$F0,d4
	andi.w	#$1F0,d5
	lsl.w	#4,d4
	lsr.w	#2,d5
	add.w	d5,d4
	; access a VDP address in 2p plane name table A ($A000) or B ($8000) if d2 has bit 13 unset or set
	moveq	#vdpComm(VRAM_Plane_A_Name_Table_2P,VRAM,WRITE)&$FFFF,d0
	swap	d0
	move.w	d4,d0
	rts
; ===========================================================================
; interestingly, this subroutine was in the Sonic 1 ROM, unused
.doubleResolution:
	add.w	4(a3),d4
	add.w	(a3),d5
	andi.w	#$1F0,d4
	andi.w	#$1F0,d5
	lsl.w	#3,d4
	lsr.w	#2,d5
	add.w	d5,d4
	; access a VDP address in 2p plane name table A ($A000) or B ($8000) if d2 has bit 13 unset or set
	moveq	#vdpComm(VRAM_Plane_A_Name_Table_2P,VRAM,WRITE)&$FFFF,d0
	swap	d0
	move.w	d4,d0
	rts
; End of function CalculateVRAMAddressOfBlockForPlayer2

; ===========================================================================
; Loads the background in its initial state into VRAM (plane B).
; Especially important for levels that never re-load the background dynamically
;loc_E300:
DrawInitialBG:
	lea	(VDP_control_port).l,a5
	lea	(VDP_data_port).l,a6
	lea	(Camera_BG_X_pos).w,a3
	lea	(Level_Layout+$80).w,a4	; background
	move.w	#vdpComm(VRAM_Plane_B_Name_Table,VRAM,WRITE)>>16,d2
    if fixBugs
	; The purpose of this function is to dynamically load a portion of
	; the background, based on where the BG camera is pointing. This
	; makes plenty of sense for levels that dynamically load their
	; background to Plane B. However, not all levels do this: some are
	; content with just loading their entire (small) background to
	; Plane B and leaving it there, untouched.
	; Unfortunately, that does not mesh well with this function: if the
	; camera is too high or too low, then only part of the background
	; will be properly loaded. This bug most visibly manifests itself in
	; Casino Night Zone Act 1, where the background abruptly cuts off at
	; the bottom.
	; To work around this, an ugly hack was added, to cause the function
	; to load a portion of the background 16 pixels lower than normal.
	; However, this hack applies to both Act 1 AND Act 2, resulting in
	; Act 2's background being cut off at the top.
	; Sonic 3 & Knuckles fixed this problem for good by giving each zone
	; its own background initialisation function (see 'LevelSetup' in the
	; Sonic & Knuckles disassembly). This fix won't go quite that far,
	; but it will give these 'static' backgrounds their own
	; initialisation logic, much like two player Mystic Cave Zone does.
	move.b	(Current_Zone).w,d0
	cmpi.b	#emerald_hill_zone,d0
	beq.w	DrawInitialBG_LoadWholeBackground_512x256
	cmpi.b	#casino_night_zone,d0
	beq.w	DrawInitialBG_LoadWholeBackground_512x256
	cmpi.b	#hill_top_zone,d0
	beq.w	DrawInitialBG_LoadWholeBackground_512x256
    else
	; This is a nasty hack to work around the bug described above.
	moveq	#0,d4
	cmpi.b	#casino_night_zone,(Current_Zone).w
	beq.w	++
    endif
	tst.w	(Two_player_mode).w
	beq.w	+
	cmpi.b	#mystic_cave_zone,(Current_Zone).w
	beq.w	DrawInitialBG_LoadWholeBackground_512x512
+
	moveq	#-16,d4
+
	moveq	#256/16-1,d6 ; Height of plane in blocks minus 1.
-	movem.l	d4-d6,-(sp)
	moveq	#0,d5
	move.w	d4,d1
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1
	move.w	d1,d4
	moveq	#0,d5
	moveq	#512/16-1,d6 ; Width of plane in blocks minus 1.
	move	#$2700,sr
	bsr.w	DrawBlockRow_CustomWidth
	move	#$2300,sr
	movem.l	(sp)+,d4-d6
	addi.w	#16,d4
	dbf	d6,-

	rts
; ===========================================================================
	; Dead code for initialising the second player's portion of Plane B.
	; I wonder why this is unused?
	moveq	#-16,d4

	moveq	#256/16-1,d6 ; Height of plane in blocks minus 1.
-	movem.l	d4-d6,-(sp)
	moveq	#0,d5
	move.w	d4,d1
	bsr.w	CalculateVRAMAddressOfBlockForPlayer2
	move.w	d1,d4
	moveq	#0,d5
	moveq	#512/16-1,d6 ; Width of plane in blocks minus 1.
	move	#$2700,sr
	bsr.w	DrawBlockRow_CustomWidth
	move	#$2300,sr
	movem.l	(sp)+,d4-d6
	addi.w	#16,d4
	dbf	d6,-

	rts
; ===========================================================================
; loc_E396:
DrawInitialBG_LoadWholeBackground_512x512:
	; Mystic Cave Zone loads its entire background at once in two player
	; mode, since the plane is big enough to fit it, unlike in one player
	; mode (512x512 instead of 512x256).
	moveq	#0,d4	; Absolute plane Y coordinate.

	moveq	#512/16-1,d6 ; Height of plane in blocks minus 1.
-	movem.l	d4-d6,-(sp)
	moveq	#0,d5
	move.w	d4,d1
	bsr.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteXAbsoluteY_DoubleResolution
	move.w	d1,d4
	moveq	#0,d5
	moveq	#512/16-1,d6 ; Width of plane in blocks minus 1.
	move	#$2700,sr
	bsr.w	DrawBlockRow.AbsoluteXAbsoluteYCustomWidth
	move	#$2300,sr
	movem.l	(sp)+,d4-d6
	addi.w	#16,d4
	dbf	d6,-

	rts
; ===========================================================================
    if fixBugs
DrawInitialBG_LoadWholeBackground_512x256:
	moveq	#0,d4	; Absolute plane Y coordinate.

	moveq	#256/16-1,d6 ; Height of plane in blocks minus 1.
-	movem.l	d4-d6,-(sp)
	moveq	#0,d5
	move.w	d4,d1
	; This is just a fancy efficient way of doing 'if true then call this, else call that'.
	pea	+(pc)
	tst.w	(Two_player_mode).w
	beq.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteXAbsoluteY
	bra.w	CalculateVRAMAddressOfBlockForPlayer1.AbsoluteXAbsoluteY_DoubleResolution
+
	move.w	d1,d4
	moveq	#0,d5
	moveq	#512/16-1,d6 ; Width of plane in blocks minus 1.
	move	#$2700,sr
	bsr.w	DrawBlockRow.AbsoluteXAbsoluteYCustomWidth
	move	#$2300,sr
	movem.l	(sp)+,d4-d6
	addi.w	#16,d4
	dbf	d6,-

	rts
    endif
; ===========================================================================

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; loadZoneBlockMaps

; Loads block and bigblock mappings for the current Zone.

loadZoneBlockMaps:
	moveq	#0,d0
	move.b	(Current_Zone).w,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	lea	(LevelArtPointers).l,a2
	lea	(a2,d0.w),a2
	move.l	a2,-(sp)
	addq.w	#4,a2
	move.l	(a2)+,d0
	andi.l	#$FFFFFF,d0	; pointer to block mappings
	movea.l	d0,a0
	lea	(Block_Table).w,a1
	jsrto	KosDec, JmpTo_KosDec	; load block maps
	cmpi.b	#hill_top_zone,(Current_Zone).w
	bne.s	+
	lea	(Block_Table+$980).w,a1
	lea	(BM16_HTZ).l,a0
	jsrto	KosDec, JmpTo_KosDec	; patch for Hill Top Zone block map
+
	tst.w	(Two_player_mode).w
	beq.s	+
	; In 2P mode, adjust the block table to halve the pattern index on each block
	lea	(Block_Table).w,a1

	move.w	#bytesToWcnt(Block_Table_End-Block_Table),d2
-	move.w	(a1),d0		; read an entry
	move.w	d0,d1
	andi.w	#$F800,d0	; filter for upper five bits
	andi.w	#$7FF,d1	; filter for lower eleven bits (patternIndex)
	lsr.w	#1,d1		; halve the pattern index
	or.w	d1,d0		; put the parts back together
	move.w	d0,(a1)+	; change the entry with the adjusted value
	dbf	d2,-
+
	move.l	(a2)+,d0
	andi.l	#$FFFFFF,d0	; pointer to chunk mappings
	movea.l	d0,a0
	lea	(Chunk_Table).l,a1
	jsrto	KosDec, JmpTo_KosDec
	bsr.w	loadLevelLayout
	movea.l	(sp)+,a2	; zone specific pointer in LevelArtPointers
	addq.w	#4,a2
	moveq	#0,d0
	move.b	(a2),d0	; PLC2 ID
	beq.s	+
	jsrto	LoadPLC, JmpTo_LoadPLC
+
	addq.w	#4,a2
	moveq	#0,d0
	move.b	(a2),d0	; palette ID
	jsrto	PalLoad_Now, JmpTo_PalLoad_Now
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||


loadLevelLayout:
	moveq	#0,d0
	move.w	(Current_ZoneAndAct).w,d0
	ror.b	#1,d0
	lsr.w	#6,d0
	lea	(Off_Level).l,a0
	move.w	(a0,d0.w),d0
	lea	(a0,d0.l),a0
	lea	(Level_Layout).w,a1
	jmpto	KosDec, JmpTo_KosDec
; End of function loadLevelLayout

; ===========================================================================

;loadLevelLayout_Sonic1:
	; This loads level layout data in Sonic 1's format. Curiously, this
	; function has been changed since Sonic 1: in particular, it repeats
	; the rows of the source data to fill the rows of the destination
	; data, which provides some explanation for why so many of Sonic 2's
	; backgrounds are repeated in their layout data. This repeating is
	; needed to prevent Hidden Palace Zone's background from disappearing
	; when the player moves to the left.

	; Clear layout data.
	lea	(Level_Layout).w,a3
	move.w	#bytesToLcnt(Level_Layout_End-Level_Layout),d1
	moveq	#0,d0
-	move.l	d0,(a3)+
	dbf	d1,-

	; The rows of the foreground and background layouts are interleaved
	; in memory. This is done here:
	lea	(Level_Layout).w,a3	; Foreground.
	moveq	#0,d1			; Index into 'Off_Level' to get level foreground layout.
	bsr.w	.loadLayout
	lea	(Level_Layout+$80).w,a3	; Background.
	moveq	#2,d1			; Index into 'Off_Level' to get level background layout.

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E4A2:
.loadLayout:
	; This expects 'Off_Level' to be in the format that it was in
	; Sonic 1.
	moveq	#0,d0
	move.w	(Current_ZoneAndAct).w,d0
	ror.b	#1,d0
	lsr.w	#5,d0
	add.w	d1,d0
	lea	(Off_Level).l,a1
	move.w	(a1,d0.w),d0
	lea	(a1,d0.l),a1

	moveq	#0,d1
	move.w	d1,d2
	move.b	(a1)+,d1	; Layout width.
	move.b	(a1)+,d2	; Layout height.
	move.l	d1,d5
	addq.l	#1,d5
	moveq	#0,d3
	move.w	#$80,d3	; Size of layout row in memory.
	divu.w	d5,d3	; Get how many times to repeat the source row to fill the destination row.
	subq.w	#1,d3	; Turn into loop counter.

.nextRow:
	movea.l	a3,a0

	move.w	d3,d4
.repeatRow:
	move.l	a1,-(sp)

	move.w	d1,d0
.nextByte:
	move.b	(a1)+,(a0)+
	dbf	d0,.nextByte

	movea.l	(sp)+,a1
	dbf	d4,.repeatRow

	lea	(a1,d5.w),a1	; Next row in source data.
	lea	$100(a3),a3	; Next row in destination data.
	dbf	d2,.nextRow

	rts
; End of function .loadLayout

; ===========================================================================

;ConvertChunksFrom256x256To128x128:
	; This converts Sonic 1-style 256x256 chunks to Sonic 2-style 128x128
	; chunks.

	; Destination of 128x128 chunks.
	lea	($FE0000).l,a1
	lea	($FE0000+8*8*2).l,a2
	; Source of 256x256 chunks.
	lea	(Chunk_Table).l,a3

	move.w	#64-1,d1	; Process 64 256x256 chunks.
-	bsr.w	ConvertHalfOf256x256ChunkToTwo128x128Chunks
	bsr.w	ConvertHalfOf256x256ChunkToTwo128x128Chunks
	dbf	d1,-

	lea	($FE0000).l,a1
	lea	($FF0000).l,a2

	; Insert a blank chunk at the start of chunk table.
	move.w	#bytesToWcnt(8*8*2),d1
-	move.w	#0,(a2)+
	dbf	d1,-

	; Copy the actual chunks to after this blank chunk.
	move.w	#bytesToWcnt($8000-(8*8*2)),d1
-	move.w	(a1)+,(a2)+
	dbf	d1,-

	rts
; ===========================================================================

;EliminateChunkDuplicates:
	; This is a chunk de-duplicator.

	; Copy first chunk into 'Chunk_Table'.
	lea	($FE0000).l,a1
	lea	(Chunk_Table).l,a3

	moveq	#bytesToLcnt(8*8*2),d0
-	move.l	(a1)+,(a3)+
	dbf	d0,-

	moveq	#0,d7	; This holds how many chunks have been copied minus 1.
	lea	($FE0000).l,a1
	move.w	#$100-1,d5	; $100 chunks
;loc_E55A:
.nextChunk:
	lea	(Chunk_Table).l,a3
	move.w	d7,d6

.doNextComparison:
	movem.l	a1-a3,-(sp)

	; Compare chunks.
	move.w	#bytesToWcnt(8*8*2),d0
-	cmpm.w	(a1)+,(a3)+
	bne.s	+
	dbf	d0,-

	; The chunks match.
	movem.l	(sp)+,a1-a3
	adda.w	#8*8*2,a1
	dbf	d5,.nextChunk

	bra.s	++
; ===========================================================================
+
	; No match: check the next chunk.
	movem.l	(sp)+,a1-a3
	adda.w	#8*8*2,a3
	dbf	d6,.doNextComparison

	; Not a single match.

	; Add this chunk to the output.
	moveq	#bytesToLcnt(8*8*2),d0
-	move.l	(a1)+,(a3)+
	dbf	d0,-

	addq.l	#1,d7	; One more chunk has been added.
	dbf	d5,.nextChunk
/
	bra.s	-	; infinite loop

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_E59C:
ConvertHalfOf256x256ChunkToTwo128x128Chunks:
	moveq	#8-1,d0	 ; 8 rows.
-
	; Do a row of chunk 1 (a chunk is 8 blocks wide and tall).
	move.l	(a3)+,(a1)+
	move.l	(a3)+,(a1)+
	move.l	(a3)+,(a1)+
	move.l	(a3)+,(a1)+
	; Do a row of chunk 2.
	move.l	(a3)+,(a2)+
	move.l	(a3)+,(a2)+
	move.l	(a3)+,(a2)+
	move.l	(a3)+,(a2)+
	dbf	d0,-

	adda.w	#8*8*2,a1
	adda.w	#8*8*2,a2

	rts
; End of function ConvertHalfOf256x256ChunkToTwo128x128Chunks

; ===========================================================================

    if gameRevision=0
	nop
    endif

    if ~~removeJmpTos
; JmpTo_PalLoad2
JmpTo_PalLoad_Now ; JmpTo
	jmp	(PalLoad_Now).l
JmpTo_LoadPLC ; JmpTo
	jmp	(LoadPLC).l
JmpTo_KosDec ; JmpTo
	jmp	(KosDec).l

	align 4
    endif





	include "_inc/Dynamic Level Events.asm"

	include "objects/11 Bridges.asm"

	include "objects/15 ARZ Swinging Platforms.asm"

	include "objects/17 Unused S1 Rotating Log Helix.asm"

	include "objects/18 Stationary Floating Platform.asm"

	include "objects/1A & 1F Collapsing Platforms.asm"

	include "objects/1C & 71 Scenery.asm"

	include "objects/2A MCZ Stomper.asm"

	include "objects/1D CPZ One-Way Barrier.asm"

	include "objects/28 & 29 Animals and Points.asm"

	include "objects/25 37 & DC Rings.asm"

	include "objects/26 & 2E Monitors.asm"

	include "objects/0E 0F & C9 Title Screen Objects.asm"

	include "objects/34 Title Cards.asm"

	include "objects/39 Game Over.asm"

	include "objects/3A & 6F Results Screen Cards.asm"

	include "objects/36 Spikes.asm"

	include "objects/3B Unused S1 GHZ Rock.asm"

	include "objects/3C Unused S1 Breakable Wall.asm"

	include "_inc/Run Objects.asm"	; Also contains the object pointer list.


; ---------------------------------------------------------------------------
; Subroutine to make an object move and fall downward increasingly fast
; This moves the object horizontally and vertically
; and also applies gravity to its speed
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_16380: ObjectFall:
ObjectMoveAndFall:
	move.l	x_pos(a0),d2	; load x position
	move.l	y_pos(a0),d3	; load y position
	move.w	x_vel(a0),d0	; load x speed
	ext.l	d0
	asl.l	#8,d0	; shift velocity to line up with the middle 16 bits of the 32-bit position
	add.l	d0,d2	; add x speed to x position	; note this affects the subpixel position x_sub(a0) = 2+x_pos(a0)
	move.w	y_vel(a0),d0	; load y speed
	addi.w	#$38,y_vel(a0)	; increase vertical speed (apply gravity)
	ext.l	d0
	asl.l	#8,d0	; shift velocity to line up with the middle 16 bits of the 32-bit position
	add.l	d0,d3	; add old y speed to y position	; note this affects the subpixel position y_sub(a0) = 2+y_pos(a0)
	move.l	d2,x_pos(a0)	; store new x position
	move.l	d3,y_pos(a0)	; store new y position
	rts
; End of function ObjectMoveAndFall
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; ---------------------------------------------------------------------------
; Subroutine translating object speed to update object position
; This moves the object horizontally and vertically
; but does not apply gravity to it
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_163AC: SpeedToPos:
ObjectMove:
	move.l	x_pos(a0),d2	; load x position
	move.l	y_pos(a0),d3	; load y position
	move.w	x_vel(a0),d0	; load horizontal speed
	ext.l	d0
	asl.l	#8,d0	; shift velocity to line up with the middle 16 bits of the 32-bit position
	add.l	d0,d2	; add to x-axis position	; note this affects the subpixel position x_sub(a0) = 2+x_pos(a0)
	move.w	y_vel(a0),d0	; load vertical speed
	ext.l	d0
	asl.l	#8,d0	; shift velocity to line up with the middle 16 bits of the 32-bit position
	add.l	d0,d3	; add to y-axis position	; note this affects the subpixel position y_sub(a0) = 2+y_pos(a0)
	move.l	d2,x_pos(a0)	; update x-axis position
	move.l	d3,y_pos(a0)	; update y-axis position
	rts
; End of function ObjectMove
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; ---------------------------------------------------------------------------
; Routines to mark an enemy/monitor/ring/platform as destroyed
; ---------------------------------------------------------------------------

; ===========================================================================
; input: a0 = the object
; loc_163D2:
MarkObjGone:
	tst.w	(Two_player_mode).w	; is it two player mode?
	beq.s	+			; if not, branch
	bra.w	DisplaySprite
+
	move.w	x_pos(a0),d0
	andi.w	#$FF80,d0
	sub.w	(Camera_X_pos_coarse).w,d0
	cmpi.w	#$80+320+$40+$80,d0	; This gives an object $80 pixels of room offscreen before being unloaded (the $40 is there to round up 320 to a multiple of $80)
	bhi.w	+
	bra.w	DisplaySprite

+	lea	(Object_Respawn_Table).w,a2
	moveq	#0,d0
	move.b	respawn_index(a0),d0
	beq.s	+
	bclr	#7,Obj_respawn_data-Object_Respawn_Table(a2,d0.w)
+
	bra.w	DeleteObject
; ===========================================================================
; input: d0 = the object's x position
; loc_1640A:
MarkObjGone2:
	tst.w	(Two_player_mode).w
	beq.s	+
	bra.w	DisplaySprite
+
	andi.w	#$FF80,d0
	sub.w	(Camera_X_pos_coarse).w,d0
	cmpi.w	#$80+320+$40+$80,d0	; This gives an object $80 pixels of room offscreen before being unloaded (the $40 is there to round up 320 to a multiple of $80)
	bhi.w	+
	bra.w	DisplaySprite
+
	lea	(Object_Respawn_Table).w,a2
	moveq	#0,d0
	move.b	respawn_index(a0),d0
	beq.s	+
	bclr	#7,Obj_respawn_data-Object_Respawn_Table(a2,d0.w)
+
	bra.w	DeleteObject
; ===========================================================================
; input: a0 = the object
; does nothing instead of calling DisplaySprite in the case of no deletion
; loc_1643E:
MarkObjGone3:
	tst.w	(Two_player_mode).w
	beq.s	+
	rts
+
	move.w	x_pos(a0),d0
	andi.w	#$FF80,d0
	sub.w	(Camera_X_pos_coarse).w,d0
	cmpi.w	#$80+320+$40+$80,d0	; This gives an object $80 pixels of room offscreen before being unloaded (the $40 is there to round up 320 to a multiple of $80)
	bhi.w	+
	rts
+
	lea	(Object_Respawn_Table).w,a2
	moveq	#0,d0
	move.b	respawn_index(a0),d0
	beq.s	+
	bclr	#7,Obj_respawn_data-Object_Respawn_Table(a2,d0.w)
+
	bra.w	DeleteObject
; ===========================================================================
; input: a0 = the object
; loc_16472:
MarkObjGone_P1:
	tst.w	(Two_player_mode).w
	bne.s	MarkObjGone_P2
	move.w	x_pos(a0),d0
	andi.w	#$FF80,d0
	sub.w	(Camera_X_pos_coarse).w,d0
	cmpi.w	#$80+320+$40+$80,d0	; This gives an object $80 pixels of room offscreen before being unloaded (the $40 is there to round up 320 to a multiple of $80)
	bhi.w	+
	bra.w	DisplaySprite
+
	lea	(Object_Respawn_Table).w,a2
	moveq	#0,d0
	move.b	respawn_index(a0),d0
	beq.s	+
	bclr	#7,Obj_respawn_data-Object_Respawn_Table(a2,d0.w)
+
	bra.w	DeleteObject
; ---------------------------------------------------------------------------
; input: a0 = the object
; loc_164A6:
MarkObjGone_P2:
	move.w	x_pos(a0),d0
	andi.w	#$FF00,d0
	move.w	d0,d1
	sub.w	(Camera_X_pos_coarse).w,d0
	cmpi.w	#$300,d0
	bhi.w	+
	bra.w	DisplaySprite
+
	sub.w	(Camera_X_pos_coarse_P2).w,d1
	cmpi.w	#$300,d1
	bhi.w	+
	bra.w	DisplaySprite
+
	lea	(Object_Respawn_Table).w,a2
	moveq	#0,d0
	move.b	respawn_index(a0),d0
	beq.s	+
	bclr	#7,Obj_respawn_data-Object_Respawn_Table(a2,d0.w)
+
	bra.w	DeleteObject ; useless branch...

; ---------------------------------------------------------------------------
; Subroutine to delete an object
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; freeObject:
DeleteObject:
	movea.l	a0,a1

; sub_164E8:
DeleteObject2:
	moveq	#0,d1

	moveq	#bytesToLcnt(next_object),d0 ; we want to clear up to the next object
	; delete the object by setting all of its bytes to 0
-	move.l	d1,(a1)+
	dbf	d0,-
    if object_size&3
	move.w	d1,(a1)+
    endif

	rts
; End of function DeleteObject2




; ---------------------------------------------------------------------------
; Subroutine to display a sprite/object, when a0 is the object RAM
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_164F4:
DisplaySprite:
	lea	(Object_Display_Lists).w,a1
	move.w	priority(a0),d0
	lsr.w	#8-object_display_list_size_bits,d0
	andi.w	#(1<<total_object_display_lists_bits-1)<<object_display_list_size_bits,d0
	adda.w	d0,a1
	cmpi.w	#object_display_list_size-2,(a1)
	bhs.s	.return
	addq.w	#2,(a1)
	adda.w	(a1),a1
	move.w	a0,(a1)

.return:
	rts
; End of function DisplaySprite

; ---------------------------------------------------------------------------
; Subroutine to display a sprite/object, when a1 is the object RAM
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_16512:
DisplaySprite2:
	lea	(Object_Display_Lists).w,a2
	move.w	priority(a1),d0
	lsr.w	#8-object_display_list_size_bits,d0
	andi.w	#(1<<total_object_display_lists_bits-1)<<object_display_list_size_bits,d0
	adda.w	d0,a2
	cmpi.w	#object_display_list_size-2,(a2)
	bhs.s	.return
	addq.w	#2,(a2)
	adda.w	(a2),a2
	move.w	a1,(a2)

.return:
	rts
; End of function DisplaySprite2

; ---------------------------------------------------------------------------
; Subroutine to display a sprite/object, when a0 is the object RAM
; and d0 is already priority*$80
; ---------------------------------------------------------------------------

; loc_16530:
DisplaySprite3:
	lea	(Object_Display_Lists).w,a1
	adda.w	d0,a1
	cmpi.w	#object_display_list_size-2,(a1)
	bhs.s	.return
	addq.w	#2,(a1)
	adda.w	(a1),a1
	move.w	a0,(a1)

.return:
	rts


	include "objects/sub AnimateSprite.asm"

	include "_inc/BuildSprites.asm"

	include "_inc/Rings Manager.asm"

	include "_inc/CNZ Special Bumpers.asm"

	include "_inc/Object Manager.asm"

	include "objects/41 Springs.asm"

	include "objects/0D Signpost.asm"


; ---------------------------------------------------------------------------
; Solid object subroutines (includes spikes, blocks, rocks etc)
; These check collision of Sonic/Tails with objects on the screen
;
; input variables:
; d1 = object width / 2
; d2 = object height / 2 (when jumping)
; d3 = object height / 2 (when walking)
; d4 = object x-axis position
;
; address registers:
; a0 = the object to check collision with
; a1 = Sonic or Tails (set inside these subroutines)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_19718:
SolidObject:
	; Collide player 1.
	lea	(MainCharacter).w,a1
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)	; Backup input registers.
	bsr.s	+
	movem.l	(sp)+,d1-d4	; Restore input registers.

	; Collide player 2.
	lea	(Sidekick).w,a1
	tst.b	render_flags(a1)
	bpl.w	return_19776	; Don't bother if Tails is not on-screen.
	addq.b	#p2_standing_bit-p1_standing_bit,d6
+
	btst	d6,status(a0)
	beq.w	SolidObject_OnScreenTest
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_1975A
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_1975A
	cmp.w	d2,d0
	blo.s	loc_1976E

loc_1975A:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_1976E:
	move.w	d4,d2
	bsr.w	MvSonicOnPtfm
	moveq	#0,d4

return_19776:
	rts

; ===========================================================================
; there are a few slightly different SolidObject functions
; specialized for certain objects, in this case, obj74 and obj30
; These check for solidity even if the object is off-screen
; loc_19778: SolidObject74_30:
SolidObject_Always:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	SolidObject_Always_SingleCharacter
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6
;loc_1978E:
SolidObject_Always_SingleCharacter:
	btst	d6,status(a0)
	beq.w	SolidObject_cont
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_197B2
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_197B2
	cmp.w	d2,d0
	blo.s	loc_197C6

loc_197B2:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_197C6:
	move.w	d4,d2
	bsr.w	MvSonicOnPtfm
	moveq	#0,d4
	rts

; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to collide Sonic/Tails with the top of a sloped solid like diagonal springs
; ---------------------------------------------------------------------------
;
; input variables:
; d1 = object width
; d2 = object height / 2 (when jumping)
; d3 = object height / 2 (when walking)
; d4 = object x-axis position
;
; address registers:
; a0 = the object to check collision with
; a1 = Sonic or Tails (set inside these subroutines)
; a2 = height data for slope
; loc_197D0: SolidObject86_30:
SlopedSolid:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	SlopedSolid_SingleCharacter
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6

; this gets called from a few more places...
; loc_197E6: SolidObject_Simple:
SlopedSolid_SingleCharacter:
	btst	d6,status(a0)
	beq.w	SlopedSolid_cont
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_1980A
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_1980A
	cmp.w	d2,d0
	blo.s	loc_1981E

loc_1980A:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_1981E:
	move.w	d4,d2
	bsr.w	MvSonicOnSlope
	moveq	#0,d4
	rts

; ===========================================================================
; unused/dead code for some SolidObject check
; This is for a sloped object that is sloped at the top and at the bottom.
; SolidObject_Unk: loc_19828:
;DoubleSlopedSolid:
	; a0=object
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	+
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6
+
	btst	d6,status(a0)
	beq.w	DoubleSlopedSolid_cont
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_19862
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_19862
	cmp.w	d2,d0
	blo.s	loc_19876

loc_19862:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_19876:
	move.w	d4,d2
	bsr.w	MvSonicOnDoubleSlope
	moveq	#0,d4
	rts

; ===========================================================================
; loc_19880:
SolidObject45:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	loc_19896
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6

loc_19896:
	btst	d6,status(a0)
	beq.w	SolidObject45_cont
	btst	#1,status(a1)
	bne.s	loc_198B8
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_198B8
	add.w	d1,d1
	cmp.w	d1,d0
	blo.s	loc_198CC

loc_198B8:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_198CC:
	; Inlined call to MvSonicOnPtfm
	move.w	y_pos(a0),d0
	sub.w	d2,d0
	add.w	d3,d0
	moveq	#0,d1
	move.b	y_radius(a1),d1
	sub.w	d1,d0
	move.w	d0,y_pos(a1)
	sub.w	x_pos(a0),d4
	sub.w	d4,x_pos(a1)
	moveq	#0,d4
	rts
; ===========================================================================
; loc_198EC: SolidObject45_alt:
SolidObject45_cont:
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.w	SolidObject_TestClearPush
	move.w	d1,d4
	add.w	d4,d4
	cmp.w	d4,d0
	bhi.w	SolidObject_TestClearPush
	move.w	y_pos(a0),d5
	add.w	d3,d5
	move.b	y_radius(a1),d3
	ext.w	d3
	add.w	d3,d2
	move.w	y_pos(a1),d3
	sub.w	d5,d3
	addq.w	#4,d3
	add.w	d2,d3
	bmi.w	SolidObject_TestClearPush
	move.w	d2,d4
	add.w	d4,d4
	cmp.w	d4,d3
	bhs.w	SolidObject_TestClearPush
	bra.w	SolidObject_ChkBounds
; ===========================================================================
; loc_1992E: SolidObject86_30_alt:
SlopedSolid_cont:
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.w	SolidObject_TestClearPush
	move.w	d1,d3
	add.w	d3,d3
	cmp.w	d3,d0
	bhi.w	SolidObject_TestClearPush
	move.w	d0,d5
	btst	#0,render_flags(a0)
	beq.s	+
	not.w	d5
	add.w	d3,d5
+
	lsr.w	#1,d5
	move.b	(a2,d5.w),d3
	sub.b	(a2),d3
	ext.w	d3
	move.w	y_pos(a0),d5
	sub.w	d3,d5
	move.b	y_radius(a1),d3
	ext.w	d3
	add.w	d3,d2
	move.w	y_pos(a1),d3
	sub.w	d5,d3
	addq.w	#4,d3
	add.w	d2,d3
	bmi.w	SolidObject_TestClearPush
	move.w	d2,d4
	add.w	d4,d4
	cmp.w	d4,d3
	bhs.w	SolidObject_TestClearPush
	bra.w	SolidObject_ChkBounds
; ===========================================================================
; unused/dead code
; loc_19988: SolidObject_Unk_cont:
DoubleSlopedSolid_cont:
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.w	SolidObject_TestClearPush
	move.w	d1,d3
	add.w	d3,d3
	cmp.w	d3,d0
	bhi.w	SolidObject_TestClearPush
	move.w	d0,d5
	btst	#0,render_flags(a0)
	beq.s	+
	not.w	d5
	add.w	d3,d5
+
	andi.w	#$FFFE,d5
	move.b	(a2,d5.w),d3
	move.b	1(a2,d5.w),d2
	ext.w	d2
	ext.w	d3
	move.w	y_pos(a0),d5
	sub.w	d3,d5
	move.w	y_pos(a1),d3
	sub.w	d5,d3
	move.b	y_radius(a1),d5
	ext.w	d5
	add.w	d5,d3
	addq.w	#4,d3
	bmi.w	SolidObject_TestClearPush
	add.w	d5,d2
	move.w	d2,d4
	add.w	d5,d4
	cmp.w	d4,d3
	bhs.w	SolidObject_TestClearPush
	bra.w	SolidObject_ChkBounds
; ===========================================================================
; loc_199E8: SolidObject_cont:
SolidObject_OnScreenTest:
	; If the object is not on-screen, then don't try to collide with it.
	; This is presumably an optimisation, but this means that if Sonic
	; outruns the screen then he can phase through solid objects.
	tst.b	render_flags(a0)
	bpl.w	SolidObject_TestClearPush
;loc_199F0:
SolidObject_cont:
	; We now perform the X portion of a bounding box check. To do this, we assume a
	; coordinate system where the X origin is at the object's left edge.
	move.w	x_pos(a1),d0			; load Sonic's X position...
	sub.w	x_pos(a0),d0			; ...and calculate his x position relative to the object.
	add.w	d1,d0				; Put object's left edge at (0,0).  This is also Sonic's distance to the object's left edge.
	bmi.w	SolidObject_TestClearPush	; Branch if Sonic is outside the object's left edge.
	move.w	d1,d3
	add.w	d3,d3				; Calculate object's width.
	cmp.w	d3,d0
	bhi.w	SolidObject_TestClearPush	; Branch if Sonic is outside the object's right edge.
	; We now perform the y portion of a bounding box check. To do this, we assume a
	; coordinate system where the y origin is at the highest y position relative to the object
	; at which Sonic would still collide with it.  This point is
	;   y_pos(object) - width(object)/2 - y_radius(Sonic) - 4,
	; where object is stored in (a0), Sonic in (a1), and height(object)/2 in d2.  This way
	; of doing it causes the object's hitbox to be vertically off-center by -4 pixels.
	move.b	y_radius(a1),d3			; load Sonic's Y radius.
	ext.w	d3
	add.w	d3,d2				; Calculate maximum distance for a top collision.
	move.w	y_pos(a1),d3			; load Sonic's y position...
	sub.w	y_pos(a0),d3			; ...and calculate his y position relative to the object.
	addq.w	#4,d3				; Assume a slightly lower position for Sonic.
	add.w	d2,d3				; Make the highest position where Sonic would still be colliding with the object (0,0).
	bmi.w	SolidObject_TestClearPush	; Branch if Sonic is above this point.
	andi.w	#$7FF,d3
	move.w	d2,d4
	add.w	d4,d4				; Calculate minimum distance for a bottom collision.
	cmp.w	d4,d3
	bhs.w	SolidObject_TestClearPush	; Branch if Sonic is below this point.
;loc_19A2E:
SolidObject_ChkBounds:
	tst.b	obj_control(a1)
	bmi.w	SolidObject_TestClearPush	; Branch if object collisions are disabled for Sonic.
	cmpi.b	#6,routine(a1)			; Is Sonic dead?
	bhs.w	SolidObject_NoCollision		; If yes, branch.
	tst.w	(Debug_placement_mode).w
	bne.w	SolidObject_NoCollision		; Branch if in Debug Mode.

	move.w	d0,d5
	cmp.w	d0,d1
	bhs.s	.isToTheLeft		; Branch if Sonic is to the object's left.

;.isToTheRight:
	add.w	d1,d1
	sub.w	d1,d0
	move.w	d0,d5			; Calculate Sonic's distance to the object's right edge...
	neg.w	d5			; ...and calculate the absolute value.

.isToTheLeft:
	move.w	d3,d1
	cmp.w	d3,d2
	bhs.s	.isAbove

;.isBelow:
	subq.w	#4,d3
	sub.w	d4,d3
	move.w	d3,d1
	neg.w	d1

.isAbove:
	; Now...
	; 'd0' contains Sonic's distance to the nearest object horizontal edge.
	; 'd5' contains the absolute version of 'd0'.
	; 'd3' contains Sonic's distance to the nearest object vertical edge.
	; 'd1' contains the absolute version of 'd3'.
	cmp.w	d1,d5
	bhi.w	SolidObject_TopBottom		; Branch, if horizontal distance is greater than vertical distance.
; loc_19A6A:
SolidObject_LeftRight:
	; If Sonic is extremely close to the top or bottom, then branch.
	; I guess the point of this is to let Sonic walk over objects that
	; are barely poking out of the ground?
	cmpi.w	#4,d1
	bls.s	SolidObject_SideAir

	tst.w	d0			; Where is Sonic?
	beq.s	SolidObject_AtEdge	; If at the object's edge, branch
	bmi.s	SolidObject_InsideRight	; If in the right side of the object, branch

;SolidObject_InsideLeft:
	tst.w	x_vel(a1)		; Is Sonic moving left?
	bmi.s	SolidObject_AtEdge	; If yes, branch
	bra.s	SolidObject_StopCharacter
; ===========================================================================
; loc_19A7E:
SolidObject_InsideRight:
	tst.w	x_vel(a1)		; is Sonic moving right?
	bpl.s	SolidObject_AtEdge	; if yes, branch
; loc_19A84:
SolidObject_StopCharacter:
	move.w	#0,inertia(a1)
	move.w	#0,x_vel(a1)		; stop Sonic moving
; loc_19A90:
SolidObject_AtEdge:
	sub.w	d0,x_pos(a1)		; correct Sonic's position
	btst	#1,status(a1)		; is Sonic in the air?
	bne.s	SolidObject_SideAir	; if yes, branch
	move.l	d6,d4
	addq.b	#pushing_bit_delta,d4	; Character is pushing, not standing
	bset	d4,status(a0)		; make object be pushed
	bset	#5,status(a1)		; make Sonic push object
	move.w	d6,d4
	addi.b	#($10-p1_standing_bit+p1_touch_side_bit),d4
	bset	d4,d6	; This sets bits 0 (Sonic) or 1 (Tails) of high word of d6
	moveq	#1,d4	; return side collision
	rts
; ===========================================================================
; loc_19AB6:
SolidObject_SideAir:
	bsr.s	Solid_NotPushing
	move.w	d6,d4
	addi.b	#($10-p1_standing_bit+p1_touch_side_bit),d4
	bset	d4,d6	; This sets bits 0 (Sonic) or 1 (Tails) of high word of d6
	moveq	#1,d4	; return side collision
	rts
; ===========================================================================
;loc_19AC4:
SolidObject_TestClearPush:
	move.l	d6,d4
	addq.b	#pushing_bit_delta,d4
	btst	d4,status(a0)		; is Sonic pushing?
	beq.s	SolidObject_NoCollision	; if not, branch
	cmpi.b	#AniIDSonAni_Roll,anim(a1)
	beq.s	Solid_NotPushing
    if fixBugs
	; Prevent Sonic or Tails from entering their running animation when
	; stood next to solid objects while charging a Spin Dash, dying, or
	; drowning. One way to see this bug is by charging a Spin Dash while
	; next to one of Mystic Cave Zone's crushing pillars.
	cmpi.b	#AniIDSonAni_Spindash,anim(a1)
	beq.s	Solid_NotPushing
	cmpi.b	#AniIDSonAni_Death,anim(a1)
	beq.s	Solid_NotPushing
	cmpi.b	#AniIDSonAni_Drown,anim(a1)
	beq.s	Solid_NotPushing
    endif
	move.w	#(AniIDSonAni_Walk<<8)|(AniIDSonAni_Run<<0),anim(a1) ; use walking animation (and force it to restart)
; loc_19ADC:
Solid_NotPushing:
	move.l	d6,d4
	addq.b	#pushing_bit_delta,d4
	bclr	d4,status(a0)	; clear pushing flag
	bclr	#5,status(a1)	; clear Sonic's pushing flag
; loc_19AEA:
SolidObject_NoCollision:
	moveq	#0,d4	; return no collision
	rts
; ===========================================================================
; loc_19AEE:
SolidObject_TopBottom:
	tst.w	d3				; is Sonic below the object?
	bmi.s	SolidObject_InsideBottom	; if yes, branch

;SolidObject_InsideTop:
	cmpi.w	#$10,d3				; has Sonic landed on the object?
	blo.s	SolidObject_Landed		; if yes, branch
	cmpi.b	#ObjID_LauncherSpring,id(a0)
	bne.s	SolidObject_TestClearPush
	cmpi.w	#$14,d3				; has Sonic landed on the object?
	blo.s	SolidObject_Landed		; if yes, branch
	bra.s	SolidObject_TestClearPush
; ===========================================================================
; loc_19B06:
SolidObject_InsideBottom:
	tst.w	y_vel(a1)		; is Sonic moving vertically?
	beq.s	SolidObject_Squash	; if not, branch
	bpl.s	loc_19B1C		; if moving downwards, branch
	tst.w	d3			; is Sonic above the object?
	bpl.s	loc_19B1C		; if yes, branch (this will never be true)
    if ~~fixBugs
	; This is in the wrong place: Sonic will not be pushed out of objects
	; from above if he's not moving upwards against it!
	; This is much more noticable when playing as Knuckles, as he'll be
	; able to phase through objects when climbing up walls.
	; 'Knuckles in Sonic 2' and 'Sonic 3 & Knuckles' tried to fix this,
	; but didn't do it very well.
	sub.w	d3,y_pos(a1)		; Push Sonic out of the object.
    endif
	move.w	#0,y_vel(a1)		; Stop Sonic from moving.

loc_19B1C:
    if fixBugs
	; See above.
	sub.w	d3,y_pos(a1)		; Push Sonic out of the object.
    endif
	move.w	d6,d4
	addi.b	#($10-p1_standing_bit+p1_touch_bottom_bit),d4
	bset	d4,d6	; This sets bits 2 (Sonic) or 3 (Tails) of high word of d6
	moveq	#-2,d4			; Return bottom collision.
	rts
; ===========================================================================
; loc_19B28:
SolidObject_Squash:
	btst	#1,status(a1)	; is Sonic in the air?
	bne.s	loc_19B1C	; if yes, branch
	mvabs.w	d0,d4

	; Hey, look: it's the two lines of code that the Taxman/Stealth
	; remasters forgot to copy.
	; If Sonic is near the left or right edge of the object, then don't
	; kill him, instead just push him away horizontally.
	cmpi.w	#$10,d4
	blo.w	SolidObject_LeftRight

	move.l	a0,-(sp)
    if fixBugs
	; a2 needs to be set here, otherwise KillCharacter
	; will access a dangling pointer!
	movea.l	a0,a2
    endif
	movea.l	a1,a0
	jsr	(KillCharacter).l
	movea.l	(sp)+,a0 ; load 0bj address
	move.w	d6,d4
	addi.b	#($10-p1_standing_bit+p1_touch_bottom_bit),d4
	bset	d4,d6	; This sets bits 2 (Sonic) or 3 (Tails) of high word of d6
	moveq	#-2,d4			; Return bottom collision.
	rts
; ===========================================================================
; loc_19B56:
SolidObject_Landed:
	subq.w	#4,d3
	moveq	#0,d1
	move.b	width_pixels(a0),d1
	move.w	d1,d2
	add.w	d2,d2
	add.w	x_pos(a1),d1
	sub.w	x_pos(a0),d1
	bmi.s	SolidObject_Miss	; if Sonic is right of object, branch
	cmp.w	d2,d1			; is Sonic left of object?
	bhs.s	SolidObject_Miss	; if yes, branch
	tst.w	y_vel(a1)		; is Sonic moving upwards?
	bmi.s	SolidObject_Miss	; if yes, branch
	sub.w	d3,y_pos(a1)		; correct Sonic's position
	subq.w	#1,y_pos(a1)
	bsr.w	RideObject_SetRide
	move.w	d6,d4
	addi.b	#($10-p1_standing_bit+p1_touch_top_bit),d4
	bset	d4,d6	; This sets bits 4 (Sonic) or 5 (Tails) of high word of d6
	moveq	#-1,d4			; return top collision
	rts
; ===========================================================================
; loc_19B8E:
SolidObject_Miss:
	moveq	#0,d4	; return no collision
	rts
; ===========================================================================

; Subroutine to change Sonic's position with a platform
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; loc_19B92:

MvSonicOnPtfm:
	move.w	y_pos(a0),d0
	sub.w	d3,d0
	bra.s	loc_19BA2
; ===========================================================================
	; a couple lines of unused/leftover/dead code from Sonic 1 ; a0=object
	move.w	y_pos(a0),d0
	subi.w	#9,d0

loc_19BA2:
	tst.b	obj_control(a1)
	bmi.s	return_19BCA
	cmpi.b	#6,routine(a1)
	bhs.s	return_19BCA
	tst.w	(Debug_placement_mode).w
	bne.s	return_19BCA
	moveq	#0,d1
	move.b	y_radius(a1),d1
	sub.w	d1,d0
	move.w	d0,y_pos(a1)
	sub.w	x_pos(a0),d2
	sub.w	d2,x_pos(a1)

return_19BCA:
	rts
; ===========================================================================
;loc_19BCC:
MvSonicOnSlope:
	btst	#3,status(a1)
	beq.s	return_19C0C
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	lsr.w	#1,d0
	btst	#0,render_flags(a0)
	beq.s	loc_19BEC
	not.w	d0
	add.w	d1,d0

loc_19BEC:
	move.b	(a2,d0.w),d1
	ext.w	d1
	move.w	y_pos(a0),d0
	sub.w	d1,d0
	moveq	#0,d1
	move.b	y_radius(a1),d1
	sub.w	d1,d0
	move.w	d0,y_pos(a1)
	sub.w	x_pos(a0),d2
	sub.w	d2,x_pos(a1)

return_19C0C:
	rts
; ===========================================================================
; unused/dead code.
; loc_19C0E:
MvSonicOnDoubleSlope:
	btst	#3,status(a1)
	beq.s	return_19C0C
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	btst	#0,render_flags(a0)
	beq.s	loc_19C2C
	not.w	d0
	add.w	d1,d0

loc_19C2C:
	andi.w	#$FFFE,d0
	bra.s	loc_19BEC
; ===========================================================================

; ---------------------------------------------------------------------------
; Subroutine to collide Sonic/Tails with the top of a platform
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
;
; input variables:
; d1 = object width
; d3 = object height / 2
; d4 = object x-axis position
;
; address registers:
; a0 = the object to check collision with
; a1 = Sonic or Tails (set inside these subroutines)
; loc_19C32:
PlatformObject:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	PlatformObject_SingleCharacter
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6
; loc_19C48:
PlatformObject_SingleCharacter:
	btst	d6,status(a0)
	beq.w	PlatformObject_cont
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	+
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	+
	cmp.w	d2,d0
	blo.s	loc_19C80
+

	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_19C80:
	move.w	d4,d2
	bsr.w	MvSonicOnPtfm
	moveq	#0,d4
	rts
; ===========================================================================

; ---------------------------------------------------------------------------
; Subroutine to collide Sonic/Tails with the top of a sloped platform like a seesaw
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
;
; input variables:
; d1 = object width
; d3 = object height
; d4 = object x-axis position
;
; address registers:
; a0 = the object to check collision with
; a1 = Sonic or Tails (set inside these subroutines)
; a2 = height data for slope
; loc_19C8A: SlopeObject:
SlopedPlatform:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	SlopedPlatform_SingleCharacter
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6
; loc_19CA0:
SlopedPlatform_SingleCharacter:
	btst	d6,status(a0)
	beq.w	SlopedPlatform_cont
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_19CC4
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_19CC4
	cmp.w	d2,d0
	blo.s	loc_19CD8

loc_19CC4:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ---------------------------------------------------------------------------
loc_19CD8:
	move.w	d4,d2
	bsr.w	MvSonicOnSlope
	moveq	#0,d4
	rts
; ===========================================================================
; Identical to PlatformObject.
;loc_19CE2:
PlatformObject2:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	loc_19CF8
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6

loc_19CF8:
	btst	d6,status(a0)
	beq.w	PlatformObject2_cont
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_19D1C
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_19D1C
	cmp.w	d2,d0
	blo.s	loc_19D30

loc_19D1C:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)
	moveq	#0,d4
	rts
; ===========================================================================

loc_19D30:
	move.w	d4,d2
	bsr.w	MvSonicOnPtfm
	moveq	#0,d4
	rts
; ===========================================================================
; Almost identical to PlatformObject, except that this function does nothing if
; the character is already standing on a platform. Used only by the elevators
; in CNZ.
;loc_19D3A:
PlatformObjectD5:
	lea	(MainCharacter).w,a1 ; a1=character
	moveq	#p1_standing_bit,d6
	movem.l	d1-d4,-(sp)
	bsr.s	loc_19D50
	movem.l	(sp)+,d1-d4
	lea	(Sidekick).w,a1 ; a1=character
	addq.b	#1,d6

loc_19D50:
	btst	d6,status(a0)
	bne.s	loc_19D62
	btst	#3,status(a1)
	bne.s	loc_19D8E
	bra.w	PlatformObject_cont
; ===========================================================================

loc_19D62:
	move.w	d1,d2
	add.w	d2,d2
	btst	#1,status(a1)
	bne.s	loc_19D7E
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	loc_19D7E
	cmp.w	d2,d0
	blo.s	loc_19D92

loc_19D7E:
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	d6,status(a0)

loc_19D8E:
	moveq	#0,d4
	rts
; ===========================================================================

loc_19D92:
	move.w	d4,d2
	bsr.w	MvSonicOnPtfm
	moveq	#0,d4
	rts
; ===========================================================================
; Used only by EHZ/HPZ log bridges. Very similar to PlatformObject_cont, but
; d2 already has the full width of the log.
;loc_19D9C:
PlatformObject11_cont:
	tst.w	y_vel(a1)
	bmi.w	return_19E8E
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.w	return_19E8E
	cmp.w	d2,d0
	bhs.w	return_19E8E
	bra.s	loc_19DD8
; ===========================================================================
;loc_19DBA:
PlatformObject_cont:
	tst.w	y_vel(a1)
	bmi.w	return_19E8E
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.w	return_19E8E
	add.w	d1,d1
	cmp.w	d1,d0
	bhs.w	return_19E8E

loc_19DD8:
	move.w	y_pos(a0),d0
	sub.w	d3,d0
;loc_19DDE:
PlatformObject_ChkYRange:
	move.w	y_pos(a1),d2
	move.b	y_radius(a1),d1
	ext.w	d1
	add.w	d2,d1
	addq.w	#4,d1
	sub.w	d1,d0
	bhi.w	return_19E8E
	cmpi.w	#-$10,d0
	blo.w	return_19E8E
	tst.b	obj_control(a1)
	bmi.w	return_19E8E
	cmpi.b	#6,routine(a1)
	bhs.w	return_19E8E
	add.w	d0,d2
	addq.w	#3,d2
	move.w	d2,y_pos(a1)
;loc_19E14:
RideObject_SetRide:
	btst	#3,status(a1)
	beq.s	loc_19E30
	moveq	#0,d0
	move.b	interact(a1),d0
    if object_size=$40
	lsl.w	#object_size_bits,d0
    else
	mulu.w	#object_size,d0
    endif
	addi.l	#Object_RAM,d0
	movea.l	d0,a3	; a3=object
	bclr	d6,status(a3)

loc_19E30:
    if object_size<>$40
	moveq	#0,d0 ; Clear the high word for the coming division.
    endif
	move.w	a0,d0
	subi.w	#Object_RAM,d0
    if object_size=$40
	lsr.w	#object_size_bits,d0
    else
	divu.w	#object_size,d0
    endif
	andi.w	#$7F,d0
	move.b	d0,interact(a1)
	move.b	#0,angle(a1)
	move.w	#0,y_vel(a1)
	move.w	x_vel(a1),inertia(a1)
	btst	#1,status(a1)
	beq.s	loc_19E7E
	move.l	a0,-(sp)
	movea.l	a1,a0
	move.w	a0,d1
	subi.w	#Object_RAM,d1
	bne.s	loc_19E76
	cmpi.w	#2,(Player_mode).w
	beq.s	loc_19E76
	jsr	(Sonic_ResetOnFloor_Part2).l
	bra.s	loc_19E7C
; ===========================================================================

loc_19E76:
	jsr	(Tails_ResetOnFloor_Part2).l

loc_19E7C:
	movea.l	(sp)+,a0 ; a0=character

loc_19E7E:
	bset	#3,status(a1)
	bclr	#1,status(a1)
	bset	d6,status(a0)

return_19E8E:
	rts
; ===========================================================================
;loc_19E90:
SlopedPlatform_cont:
	tst.w	y_vel(a1)
	bmi.w	return_19E8E
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.s	return_19E8E
	add.w	d1,d1
	cmp.w	d1,d0
	bhs.s	return_19E8E
	btst	#0,render_flags(a0)
	beq.s	loc_19EB6
	not.w	d0
	add.w	d1,d0

loc_19EB6:
	lsr.w	#1,d0
	move.b	(a2,d0.w),d3
	ext.w	d3
	move.w	y_pos(a0),d0
	sub.w	d3,d0
	bra.w	PlatformObject_ChkYRange
; ===========================================================================
; Basically identical to PlatformObject_cont
;loc_19EC8:
PlatformObject2_cont:
	tst.w	y_vel(a1)
	bmi.w	return_19E8E
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	add.w	d1,d0
	bmi.w	return_19E8E
	add.w	d1,d1
	cmp.w	d1,d0
	bhs.w	return_19E8E
	move.w	y_pos(a0),d0
	sub.w	d3,d0
	bra.w	PlatformObject_ChkYRange
; ===========================================================================
; If a character is being dragged through terrain by this object, drop the
; character on terrain instead.
;loc_19EF0:
DropOnFloor:
	lea	(MainCharacter).w,a1 ; a1=character
	btst	#p1_standing_bit,status(a0)
	beq.s	loc_19F1E
	jsr	(ChkFloorEdge2).l
	tst.w	d1
	beq.s	loc_19F08
	bpl.s	loc_19F1E

loc_19F08:
	lea	(MainCharacter).w,a1 ; a1=character
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	#p1_standing_bit,status(a0)

loc_19F1E:
	lea	(Sidekick).w,a1 ; a1=character
	btst	#p2_standing_bit,status(a0)
	beq.s	loc_19F4C
	jsr	(ChkFloorEdge2).l
	tst.w	d1
	beq.s	loc_19F36
	bpl.s	loc_19F4C

loc_19F36:
	lea	(Sidekick).w,a1 ; a1=character
	bclr	#3,status(a1)
	bset	#1,status(a1)
	bclr	#p2_standing_bit,status(a0)

loc_19F4C:
	moveq	#0,d4
	rts
; ===========================================================================

	include "objects/01 Sonic.asm"

	include "objects/02 Tails.asm"

	include "objects/05 Tails tails.asm"

	include "objects/0A Drown Countdown.asm"

	include "objects/38 Shield.asm"

	include "objects/35 Invincibility Stars.asm"

	include "objects/08 Splash and Dust.asm"

	include "objects/7E Super Sonic stars.asm"

    if gameRevision<2
	nop
    endif

	include "objects/sub Collision System.asm"

	include "objects/79 Starposts.asm"

	include "objects/7D Hidden Bonuses (Unused).asm"

	include "objects/44 CNZ Round Bumper.asm"

	include "objects/24 ARZ Bubbles.asm"

	include	"objects/03 Collision Switcher.asm"

	include "objects/0B CPZ Pipe Tip.asm"				; check name

	include "objects/0C CPZ Small Platform (Unused).asm"

	include "objects/12 HPZ Emerald (Unused).asm"

	include "objects/13 HPZ Waterfall (Unused).asm"

	include "objects/04 Water Surface.asm"

	include "objects/49 EHZ Waterfall.asm"

	include "objects/31 Lava Tag.asm"

	include "objects/74 Invisible Solid Block.asm"

	include "objects/7C CPZ Pylon.asm"

	include "objects/27 Explosion.asm"

	include "objects/84 Pinball Tag.asm"

	include "objects/8B Cycling Palette Switcher.asm"

	include "objects/06 Spiral Pathway.asm"

	include "objects/14 HTZ Seesaw.asm"
	
	include "objects/16 HTZ Ski Lift.asm"
	
	include "objects/19 Platforms (CPZ OOZ WFZ).asm"

	include "objects/1B CPZ Speed Booster.asm"

	include "objects/1D CPZ Blue Balls.asm"

	include "objects/1E CPZ Spin Tube.asm"

	include "objects/20 HTZ Boss Lava Bubble.asm"

	include "objects/2F HTZ Smashable Ground.asm"

	include "objects/32 Smashable Block.asm"

	include "objects/30 HTZ Rising Lava.asm"

	include "objects/33 OOZ Green Platform.asm"

	include "objects/43 OOZ Sliding Spikes.asm"

	include "objects/07 OOZ Oil Ocean.asm"

	include "objects/45 OOZ Pressure Spring.asm"

	include "objects/46 OOZ Unused Rolling Ball.asm"
	
	include "objects/47 Button.asm"

	include "objects/3D OOZ Breakable Launching Block.asm"

	include "objects/48 OOZ Rotating Sphere Cannon.asm"

	include "objects/22 ARZ Arrow Shooter.asm"

	include "objects/23 ARZ Pillar With Droppable Piece.asm"

	include "objects/2B ARZ Rising Pillar.asm"

	include "objects/2C ARZ Leaves.asm"

	include "objects/40 Red Springboard.asm"

	include "objects/42 MTZ Steam Spring.asm"

	include "objects/64 MTZ Twin Stompers.asm"

	include "objects/65 MTZ Long Moving Platform.asm"

	include "objects/66 MTZ Yellow Wall Spring.asm"

	include "objects/67 MTZ Teleporter.asm"

	include "objects/68 MTZ Spiked Block.asm"

	include "objects/6D MTZ Floor Spike.asm"

	include "objects/69 MTZ Spinning Nut.asm"

	include "objects/6A MTZ Moving Platform.asm"

	include "objects/6B MTZ Immobile Platform.asm"

	include "objects/6C MTZ Pulley Platform.asm"

	include "objects/6E MTZ Circular Moving Platform.asm"

	include "objects/70 MTZ Rotating Cog.asm"

	include "objects/72 CNZ Conveyor Belt.asm"

	include "objects/73 MCZ Unused Rotating Rings.asm"

	include "objects/75 MCZ Brick.asm"

	include "objects/76 MCZ Protruding Spike Block.asm"

	include "objects/77 MCZ Bridge.asm"

	include "objects/78 CPZ Stairs.asm"

	include "objects/7A CPZ Water Platform.asm"

	include "objects/7B CPZ Warp Pipe Spring.asm"

	include "objects/7F MCZ Vine Switch.asm"

	include "objects/80 MCZ Vine Pulley.asm"

	include "objects/81 MCZ Drawbridge.asm"

	include "objects/82 ARZ Swinging Platform.asm"

	include "objects/83 ARZ Adjoined Platforms.asm"

	include "objects/3F OOZ Fan.asm"

	include "objects/85 CNZ Plunger Spring.asm"

	include "objects/86 CNZ Pinball Flipper.asm"

	include "objects/D2 CNZ Flashing Green Blocks.asm"

	include "objects/D3 CNZ Slot Machine Bomb Prize.asm"

	include "objects/D4 CNZ Big Blue Block.asm"

	include "objects/D5 CNZ Elevator.asm"

	include "objects/D6 CNZ Pokey.asm"

	include "objects/sub Slot Machine System.asm"

	include "objects/D7 CNZ Bumper.asm"

	include "objects/D8 CNZ Colored Points Block.asm"

	include "objects/D9 Invisible Grabbable Blocks.asm"

	include "objects/4A OOZ Octus Badnik.asm"

	include "objects/50 OOZ Aquis Badnik.asm"

	include "objects/4B EHZ Buzzer Badnik.asm"

	include "objects/5C EHZ Masher Badnik.asm"

	include "objects/58 Boss Explosion.asm"

	include "objects/5D CPZ Boss.asm"

	include "objects/56 EHZ Boss.asm"

	include "objects/52 HTZ Boss.asm"

	include "objects/89 ARZ Boss.asm"

	include "objects/57 MCZ Boss.asm"

	include "objects/51 CNZ Boss.asm"

	include "objects/53 & 54 MTZ Boss.asm"

	include "objects/55 OOZ Boss.asm"

	include "objects/09 63 10 & 88 Players in Special Stage.asm"

	include "objects/5B 60 & 61 Bombs and Rings in Special Stage.asm"

	include "objects/5A Messages in Special Stage.asm"

	include "objects/59 Chaos Emerald in Special Stage.asm"


; ---------------------------------------------------------------------------
; LoadSubObject
; loads information from a sub-object into this object a0
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_365F4:
LoadSubObject:
	moveq	#0,d0
	move.b	subtype(a0),d0
; loc_365FA:
LoadSubObject_Part2:
	move.w	SubObjData_Index(pc,d0.w),d0
	lea	SubObjData_Index(pc,d0.w),a1
; loc_36602:
LoadSubObject_Part3:
	move.l	(a1)+,mappings(a0)
	move.w	(a1)+,art_tile(a0)
	jsr	(Adjust2PArtPointer).l
	move.b	(a1)+,d0
	or.b	d0,render_flags(a0)
	move.b	(a1)+,priority(a0)
	move.b	(a1)+,width_pixels(a0)
	move.b	(a1),collision_flags(a0)
	addq.b	#2,routine(a0)
	rts

; ===========================================================================
; table that maps from the subtype ID to which address to load the data from
; the format of the data there is
;	dc.l Pointer_To_Sprite_Mappings
;	dc.w VRAM_Location
;	dc.b render_flags, priority, width_pixels, collision_flags
;
; for whatever reason, only Obj8C and later have entries in this table

; off_36628:
SubObjData_Index: offsetTable
	offsetTableEntry.w Obj8C_SubObjData	; $0
	offsetTableEntry.w Obj8D_SubObjData	; $2
	offsetTableEntry.w Obj90_SubObjData	; $4
	offsetTableEntry.w Obj90_SubObjData2	; $6
	offsetTableEntry.w Obj91_SubObjData	; $8
	offsetTableEntry.w Obj92_SubObjData	; $A
	offsetTableEntry.w Invalid_SubObjData	; $C
	offsetTableEntry.w Obj94_SubObjData	; $E
	offsetTableEntry.w Obj94_SubObjData2	; $10
	offsetTableEntry.w Obj99_SubObjData2	; $12
	offsetTableEntry.w Obj99_SubObjData	; $14
	offsetTableEntry.w Obj9A_SubObjData	; $16
	offsetTableEntry.w Obj9B_SubObjData	; $18
	offsetTableEntry.w Obj9C_SubObjData	; $1A
	offsetTableEntry.w Obj9A_SubObjData2	; $1C
	offsetTableEntry.w Obj9D_SubObjData	; $1E
	offsetTableEntry.w Obj9D_SubObjData2	; $20
	offsetTableEntry.w Obj9E_SubObjData	; $22
	offsetTableEntry.w Obj9F_SubObjData	; $24
	offsetTableEntry.w ObjA0_SubObjData	; $26
	offsetTableEntry.w ObjA1_SubObjData	; $28
	offsetTableEntry.w ObjA2_SubObjData	; $2A
	offsetTableEntry.w ObjA3_SubObjData	; $2C
	offsetTableEntry.w ObjA4_SubObjData	; $2E
	offsetTableEntry.w ObjA4_SubObjData2	; $30
	offsetTableEntry.w ObjA5_SubObjData	; $32
	offsetTableEntry.w ObjA6_SubObjData	; $34
	offsetTableEntry.w ObjA7_SubObjData	; $36
	offsetTableEntry.w ObjA7_SubObjData2	; $38
	offsetTableEntry.w ObjA8_SubObjData	; $3A
	offsetTableEntry.w ObjA8_SubObjData2	; $3C
	offsetTableEntry.w ObjA7_SubObjData3	; $3E
	offsetTableEntry.w ObjAC_SubObjData	; $40
	offsetTableEntry.w ObjAD_SubObjData	; $42
	offsetTableEntry.w ObjAD_SubObjData2	; $44
	offsetTableEntry.w ObjAD_SubObjData3	; $46
	offsetTableEntry.w ObjAF_SubObjData2	; $48
	offsetTableEntry.w ObjAF_SubObjData	; $4A
	offsetTableEntry.w ObjB0_SubObjData	; $4C
	offsetTableEntry.w ObjB1_SubObjData	; $4E
	offsetTableEntry.w ObjB2_SubObjData	; $50
	offsetTableEntry.w ObjB2_SubObjData	; $52
	offsetTableEntry.w ObjB2_SubObjData	; $54
	offsetTableEntry.w ObjBC_SubObjData2	; $56
	offsetTableEntry.w ObjBC_SubObjData2	; $58
	offsetTableEntry.w ObjB3_SubObjData	; $5A
	offsetTableEntry.w ObjB2_SubObjData2	; $5C
	offsetTableEntry.w ObjB3_SubObjData	; $5E
	offsetTableEntry.w ObjB3_SubObjData	; $60
	offsetTableEntry.w ObjB3_SubObjData	; $62
	offsetTableEntry.w ObjB4_SubObjData	; $64
	offsetTableEntry.w ObjB5_SubObjData	; $66
	offsetTableEntry.w ObjB5_SubObjData	; $68
	offsetTableEntry.w ObjB6_SubObjData	; $6A
	offsetTableEntry.w ObjB6_SubObjData	; $6C
	offsetTableEntry.w ObjB6_SubObjData	; $6E
	offsetTableEntry.w ObjB6_SubObjData	; $70
	offsetTableEntry.w ObjB7_SubObjData	; $72
	offsetTableEntry.w ObjB8_SubObjData	; $74
	offsetTableEntry.w ObjB9_SubObjData	; $76
	offsetTableEntry.w ObjBA_SubObjData	; $78
	offsetTableEntry.w ObjBB_SubObjData	; $7A
	offsetTableEntry.w ObjBC_SubObjData2	; $7C
	offsetTableEntry.w ObjBD_SubObjData	; $7E
	offsetTableEntry.w ObjBD_SubObjData	; $80
	offsetTableEntry.w ObjBE_SubObjData	; $82
	offsetTableEntry.w ObjBE_SubObjData2	; $84
	offsetTableEntry.w ObjC0_SubObjData	; $86
	offsetTableEntry.w ObjC1_SubObjData	; $88
	offsetTableEntry.w ObjC2_SubObjData	; $8A
	offsetTableEntry.w Invalid_SubObjData2	; $8C
	offsetTableEntry.w ObjB8_SubObjData2	; $8E
	offsetTableEntry.w ObjC3_SubObjData	; $90
	offsetTableEntry.w ObjC5_SubObjData	; $92
	offsetTableEntry.w ObjC5_SubObjData2	; $94
	offsetTableEntry.w ObjC5_SubObjData3	; $96
	offsetTableEntry.w ObjC5_SubObjData3	; $98
	offsetTableEntry.w ObjC5_SubObjData3	; $9A
	offsetTableEntry.w ObjC5_SubObjData3	; $9C
	offsetTableEntry.w ObjC5_SubObjData3	; $9E
	offsetTableEntry.w ObjC6_SubObjData2	; $A0
	offsetTableEntry.w ObjC5_SubObjData4	; $A2
	offsetTableEntry.w ObjAF_SubObjData3	; $A4
	offsetTableEntry.w ObjC6_SubObjData3	; $A6
	offsetTableEntry.w ObjC6_SubObjData4	; $A8
	offsetTableEntry.w ObjC6_SubObjData	; $AA
	offsetTableEntry.w ObjC8_SubObjData	; $AC
; ===========================================================================
; ---------------------------------------------------------------------------
; Get Orientation To Player
; Returns the horizontal and vertical distances of the closest player object.
;
; input variables:
;  a0 = object
;
; returns:
;  a1 = address of closest player character
;  d0 = 0 if player is left from object, 2 if right
;  d1 = 0 if player is above object, 2 if below
;  d2 = closest character's horizontal distance to object
;  d3 = closest character's vertical distance to object
;
; writes:
;  d0, d1, d2, d3, d4, d5
;  a1
;  a2 = sidekick
; ---------------------------------------------------------------------------
;loc_366D6:
Obj_GetOrientationToPlayer:
	moveq	#0,d0
	moveq	#0,d1
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	x_pos(a0),d2
	sub.w	x_pos(a1),d2
	mvabs.w	d2,d4	; absolute horizontal distance to main character
	lea	(Sidekick).w,a2 ; a2=character
	move.w	x_pos(a0),d3
	sub.w	x_pos(a2),d3
	mvabs.w	d3,d5	; absolute horizontal distance to sidekick
	cmp.w	d5,d4	; get shorter distance
	bls.s	+	; branch, if main character is closer
	; if sidekick is closer
	movea.l	a2,a1
	move.w	d3,d2
+
	tst.w	d2	; is player to enemy's left?
	bpl.s	+	; if not, branch
	addq.w	#2,d0
+
	move.w	y_pos(a0),d3
	sub.w	y_pos(a1),d3	; vertical distance to closest character
	bhs.s	+	; branch, if enemy is under
	addq.w	#2,d1
+
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Cap Object Speed
; Prevents an object from going over a specified speed value.
;
; input variables:
;  d0 = max x velocity
;  d1 = max y velocity
;
;  a0 = object
;
; writes:
;  d0, d1, d2, d3
; ---------------------------------------------------------------------------
; loc_3671A:
Obj_CapSpeed:
	move.w	x_vel(a0),d2
	bpl.s	+	; branch, if object is moving right
	; going left
	neg.w	d0	; set opposite direction
	cmp.w	d0,d2	; is object's current x velocity lower than max?
	bhs.s	++	; if yes, branch
	move.w	d0,d2	; else, cap speed
	bra.w	++
; ===========================================================================
+	; going right
	cmp.w	d0,d2	; is object's current x velocity lower than max?
	bls.s	+	; if yes, branch
	move.w	d0,d2	; else, cap speed
+
	move.w	y_vel(a0),d3
	bpl.s	+	; branch, if object is moving down
	; going up
	neg.w	d1	; set opposite direction
	cmp.w	d1,d3	; is object's current y velocity lower than max?
	bhs.s	++	; if yes, branch
	move.w	d1,d3	; else, cap speed
	bra.w	++
; ===========================================================================
+	; going down
	cmp.w	d1,d3	; is object's current y velocity lower than max?
	bls.s	+	; if yes, branch
	move.w	d1,d3	; else, cap speed
+	; update speed
	move.w	d2,x_vel(a0)
	move.w	d3,y_vel(a0)
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Movement Stop
; Stops an object's movement.
;
; input variables:
;  a0 = object
;
; writes:
;  d0 = 0
; ---------------------------------------------------------------------------
;loc_36754:
Obj_MoveStop:
	moveq	#0,d0
	move.w	d0,x_vel(a0)
	move.w	d0,y_vel(a0)
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Align Child XY
; Moves a referenced object to the position of the current object with
; variable x and y offset.
;
; input variables:
;  d0 = x offset
;  d1 = y offset
;
;  a0 = parent object
;  a1 = child object
;
; writes:
;  d2 = new x position
;  d3 = new y position
; ---------------------------------------------------------------------------
;loc_36760:
Obj_AlignChildXY:
	move.w	x_pos(a0),d2
	add.w	d0,d2
	move.w	d2,x_pos(a1)
	move.w	y_pos(a0),d3
	add.w	d1,d3
	move.w	d3,y_pos(a1)
	rts
; ===========================================================================

loc_36776:
	move.w	(Tornado_Velocity_X).w,d0
	add.w	d0,x_pos(a0)
	move.w	(Tornado_Velocity_Y).w,d0
	add.w	d0,y_pos(a0)
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Delete If Behind Screen
; deletes an object if it scrolls off the left side of the screen
;
; input variables:
;  a0 = object
;
; writes:
;  d0
; ---------------------------------------------------------------------------
;loc_36788:
Obj_DeleteBehindScreen:
	tst.w	(Two_player_mode).w
	beq.s	+
	jmp	(DisplaySprite).l
+
	; when not in two player mode
	move.w	x_pos(a0),d0
	andi.w	#$FF80,d0
	sub.w	(Camera_X_pos_coarse).w,d0
	bmi.w	JmpTo64_DeleteObject
	jmp	(DisplaySprite).l
; ===========================================================================

; loc_367AA:
InheritParentXYFlip:
	move.b	render_flags(a0),d0
	andi.b	#$FC,d0
	move.b	status(a0),d2
	andi.b	#$FC,d2
	move.b	render_flags(a1),d1
	andi.b	#3,d1
	or.b	d1,d0
	or.b	d1,d2
	move.b	d0,render_flags(a0)
	move.b	d2,status(a0)
	rts
; ===========================================================================

;loc_367D0:
LoadChildObject:
	jsr	(AllocateObjectAfterCurrent).l
	bne.s	+	; rts
	move.w	(a2)+,d0
	move.w	a1,(a0,d0.w) ; store pointer to child in parent's SST
	_move.b	(a2)+,id(a1) ; load obj
	move.b	(a2)+,subtype(a1)
	move.w	a0,objoff_2C(a1) ; store pointer to parent in child's SST
	move.w	x_pos(a0),x_pos(a1)
	move.w	y_pos(a0),y_pos(a1)
+
	rts
; ===========================================================================
	; unused/dead code ; a0=object
	bsr.w	Obj_GetOrientationToPlayer
	bclr	#0,render_flags(a0)
	bclr	#0,status(a0)
	tst.w	d0
	beq.s	return_36818
	bset	#0,render_flags(a0)
	bset	#0,status(a0)

return_36818:
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Create Projectiles
; Creates a specified number of generic moving projectiles.
;
; input variables:
;  d2 = subtype, used for object initialization (refer to LoadSubObject)
;  d6 = number of projectiles to create -1
;
;  a2 = projectile stat list
;   format:
;   dc.b x_offset, y_offset, x_vel, y_vel, mapping_frame, render_flags
;
; writes:
;  d0
;  d1 = index in list
;  d6 = num objects
;
;  a1 = addres of new projectile
;  a3 = movement type (ObjectMove)
; ---------------------------------------------------------------------------
;loc_3681A:
Obj_CreateProjectiles:
	moveq	#0,d1
	; loop creates d6+1 projectiles
-
	jsr	(AllocateObjectAfterCurrent).l
	bne.s	return_3686E
	_move.b	#ObjID_Projectile,id(a1) ; load obj98
	move.b	d2,subtype(a1)	; used for object initialization
	move.w	x_pos(a0),x_pos(a1)	; align objects
	move.w	y_pos(a0),y_pos(a1)
	lea	(ObjectMove).l,a3	; set movement type
	move.l	a3,objoff_2A(a1)
	lea	(a2,d1.w),a3	; get address in list
	move.b	(a3)+,d0	; get x offset
	ext.w	d0
	add.w	d0,x_pos(a1)
	move.b	(a3)+,d0	; get y offset
	ext.w	d0
	add.w	d0,y_pos(a1)
	move.b	(a3)+,x_vel(a1)	; set movement values
	move.b	(a3)+,y_vel(a1)
	move.b	(a3)+,mapping_frame(a1)	; set map frame
	move.b	(a3)+,render_flags(a1)	; set render flags
	addq.w	#6,d1
	dbf	d6,-

return_3686E:
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to animate a sprite using an animation script
; Works like AnimateSprite, except for:
; * this function does not change render flags to match orientation given by
;   the status byte;
; * the function returns 0 on d0 if it changed the mapping frame, or 1 if an
;   end-of-animation flag was found ($FC to $FF);
; * it is only used by Mecha Sonic;
; * some of the end-of-animation flags work differently.
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_36870:
AnimateSprite_Checked:
	moveq	#0,d0
	move.b	anim(a0),d0		; move animation number to d0
	cmp.b	prev_anim(a0),d0	; is animation set to change?
	beq.s	AnimChk_Run		; if not, branch
	move.b	d0,prev_anim(a0)	; set previous animation to current animation
	move.b	#0,anim_frame(a0)	; reset animation
	move.b	#0,anim_frame_duration(a0)	; reset frame duration

AnimChk_Run:
	subq.b	#1,anim_frame_duration(a0)	; subtract 1 from frame duration
	bpl.s	AnimChk_Wait	; if time remains, branch
	add.w	d0,d0
	adda.w	(a1,d0.w),a1	; calculate address of appropriate animation script
	move.b	(a1),anim_frame_duration(a0)	; load frame duration
	moveq	#0,d1
	move.b	anim_frame(a0),d1	; load current frame number
	move.b	1(a1,d1.w),d0		; read sprite number from script
	bmi.s	AnimChk_End_FF		; if animation is complete, branch
;loc_368A8
AnimChk_Next:
	move.b	d0,mapping_frame(a0)	; load sprite number
	addq.b	#1,anim_frame(a0)	; next frame number
;loc_368B0
AnimChk_Wait:
	moveq	#0,d0	; Return 0
	rts
; ---------------------------------------------------------------------------
;loc_368B4
AnimChk_End_FF:
	addq.b	#1,d0		; is the end flag = $FF?
	bne.s	AnimChk_End_FE	; if not, branch
	move.b	#0,anim_frame(a0)	; restart the animation
	move.b	1(a1),d0	; read sprite number
	bsr.s	AnimChk_Next
	moveq	#1,d0	; Return 1
	rts
; ---------------------------------------------------------------------------
;loc_368C8
AnimChk_End_FE:
	addq.b	#1,d0		; is the end flag = $FE?
	bne.s	AnimChk_End_FD	; if not, branch
	addq.b	#2,routine(a0)	; jump to next routine
	move.b	#0,anim_frame_duration(a0)
	addq.b	#1,anim_frame(a0)
	moveq	#1,d0	; Return 1
	rts
; ---------------------------------------------------------------------------
;loc_368DE
AnimChk_End_FD:
	addq.b	#1,d0		; is the end flag = $FD?
	bne.s	AnimChk_End_FC	; if not, branch
	addq.b	#2,routine_secondary(a0)	; jump to next routine
	moveq	#1,d0	; Return 1
	rts
; ---------------------------------------------------------------------------
;loc_368EA
AnimChk_End_FC:
	addq.b	#1,d0		; is the end flag = $FC?
	bne.s	AnimChk_End	; if not, branch
	move.b	#1,anim_frame_duration(a0)	; Force frame duration to 1
	moveq	#1,d0	; Return 1

AnimChk_End:
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Delete If Off-Screen
; deletes an object if it is too far away from the screen
;
; input variables:
;  a0 = object
;
; writes:
;  d0
; ---------------------------------------------------------------------------
;loc_368F8:
Obj_DeleteOffScreen:
	tst.w	(Two_player_mode).w
	beq.s	+
	jmp	(DisplaySprite).l
+
	; when not in two player mode
	move.w	x_pos(a0),d0
	andi.w	#$FF80,d0
	sub.w	(Camera_X_pos_coarse).w,d0
	cmpi.w	#$280,d0
	bhi.w	JmpTo64_DeleteObject
	jmp	(DisplaySprite).l
; ===========================================================================

    if removeJmpTos
JmpTo65_DeleteObject ; JmpTo
    endif

JmpTo64_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
; ===========================================================================

	include "objects/8C ARZ Whisp Badnik.asm"

	include "objects/8D 8F & 90 ARZ Grounder Badnik.asm"

	include "objects/91 ARZ Chop Chop Badnik.asm"

	include "objects/92 & 93 HTZ Spiker Badnik.asm"

	include "objects/95 HTZ Sol Badnik.asm"

	include "objects/94 96 & 97 HTZ Rexon Badnik.asm"

	include "objects/98 Enemy Projectile.asm"

	include "objects/99 SCZ Nebula Badnik.asm"

	include "objects/9A & 9B SCZ Turtloid Badnik.asm"

	include "objects/9C SCZ Badnik Jet.asm"

	include "objects/9D EHZ Coconuts Badnik.asm"

	include "objects/9E MCZ Crawlton Badnik.asm"

	include "objects/9F & A0 MTZ Shellcracker Badnik.asm"

	include "objects/A1 & A2 MTZ Slicer Badnik.asm"

	include "objects/A3 MCZ Flasher Badnik.asm"

	include "objects/A4 MTZ Asteron Badnik.asm"

	include "objects/A5 & A6 CPZ Spiny Badnik.asm"

	include "objects/A7 A8 A9 & AA CPZ Grabber Badnik.asm"

	include "objects/AC SCZ Balkiry Badnik.asm"

	include "objects/AD & AE SCZ Clucker Badnik.asm"

	include "objects/AF DEZ Silver Sonic.asm"

	include "objects/B0 SEGA Screen Sonic.asm"

	include "objects/B1 TM Symbol Mask.asm"

	include "objects/B2 Tornado Plane.asm"

	include "objects/B3 SCZ Clouds.asm"

	include "objects/B4 & B5 WFZ Propellers.asm"

	include "objects/B6 WFZ Tilting Platform.asm"

	include "objects/B7 WFZ Unused Vertical Laser.asm"

	include "objects/B8 WFZ Wall Turret.asm"

	include "objects/B9 WFZ Intro Laser.asm"

	include "objects/BA WFZ Wheel.asm"

	include "objects/BB Unknown Unused Object.asm"

	include "objects/BC WFZ Eggman Ship Flame.asm"

	include "objects/BD WFZ Metal Platforms.asm"

	include "objects/BE WFZ Lateral Cannon Platforms.asm"

	include "objects/BF WFZ Unused Stick Badnik.asm"

	include "objects/C0 WFZ Launcher.asm"

	include "objects/C1 WFZ Breakable Plating.asm"

	include "objects/C2 WFZ Bustable Rivet.asm"

	include "objects/C3 & C4 Plane's Smoke.asm"

	include "objects/C5 WFZ Boss.asm"

	include "objects/C6 Eggman.asm"

	include "objects/C8 CNZ Crawl Badnik.asm"

	include "objects/C7 Death Egg Boss Robot.asm"

	include "_inc/Sprite Upscaling.asm"

	include "objects/8A Unused S1 Credits Text.asm"

	include "objects/3E Egg Prison.asm"

	include "objects/sub TouchResponse.asm"

	include "_inc/Dynamic Art.asm"

	include "_inc/BuildHUD.asm"

	include "objects/sub DebugMode.asm"

	include "_inc/LevelHeaders.asm"

	include "_inc/Pattern Loading Requests.asm"

;---------------------------------------------------------------------------------------
; Collision Data
;---------------------------------------------------------------------------------------
ColCurveMap:		BINCLUDE	"collision/Curve and resistance mapping.bin"
	even
ColArrayVertical:	BINCLUDE	"collision/Collision array - Vertical.bin"
ColArrayHorizontal:	BINCLUDE	"collision/Collision array - Horizontal.bin"
	even

; These are all compressed in the Kosinski format.
ColP_EHZHTZ:	BINCLUDE	"collision/EHZ and HTZ primary 16x16 collision index.kos"
	even
ColS_EHZHTZ:	BINCLUDE	"collision/EHZ and HTZ secondary 16x16 collision index.kos"
	even
ColP_WZ:	;BINCLUDE	"collision/WZ primary 16x16 collision index.kos"
	;even
ColP_MTZ:	BINCLUDE	"collision/MTZ primary 16x16 collision index.kos"
	even
ColP_HPZ:	;BINCLUDE	"collision/HPZ primary 16x16 collision index.kos"
	;even
ColS_HPZ:	;BINCLUDE	"collision/HPZ secondary 16x16 collision index.kos"
	;even
ColP_OOZ:	BINCLUDE	"collision/OOZ primary 16x16 collision index.kos"
	even
ColP_MCZ:	BINCLUDE	"collision/MCZ primary 16x16 collision index.kos"
	even
ColP_CNZ:	BINCLUDE	"collision/CNZ primary 16x16 collision index.kos"
	even
ColS_CNZ:	BINCLUDE	"collision/CNZ secondary 16x16 collision index.kos"
	even
ColP_CPZDEZ:	BINCLUDE	"collision/CPZ and DEZ primary 16x16 collision index.kos"
	even
ColS_CPZDEZ:	BINCLUDE	"collision/CPZ and DEZ secondary 16x16 collision index.kos"
	even
ColP_ARZ:	BINCLUDE	"collision/ARZ primary 16x16 collision index.kos"
	even
ColS_ARZ:	BINCLUDE	"collision/ARZ secondary 16x16 collision index.kos"
	even
ColP_WFZSCZ:	BINCLUDE	"collision/WFZ and SCZ primary 16x16 collision index.kos"
	even
ColS_WFZSCZ:	BINCLUDE	"collision/WFZ and SCZ secondary 16x16 collision index.kos"
	even
ColP_Invalid:




;---------------------------------------------------------------------------------------
; Offset index of level layouts
; Two entries per zone, pointing to the level layouts for acts 1 and 2 of each zone
; respectively.
;---------------------------------------------------------------------------------------
Off_Level: zoneOrderedOffsetTable 2,2
	; EHZ
	zoneOffsetTableEntry.w Level_EHZ1	; Act 1
	zoneOffsetTableEntry.w Level_EHZ2	; Act 2
	; Zone 1
	zoneOffsetTableEntry.w Level_Invalid	; Act 1
	zoneOffsetTableEntry.w Level_Invalid	; Act 2
	; WZ
	zoneOffsetTableEntry.w Level_Invalid	; Act 1
	zoneOffsetTableEntry.w Level_Invalid	; Act 2
	; Zone 3
	zoneOffsetTableEntry.w Level_Invalid	; Act 1
	zoneOffsetTableEntry.w Level_Invalid	; Act 2
	; MTZ
	zoneOffsetTableEntry.w Level_MTZ1	; Act 1
	zoneOffsetTableEntry.w Level_MTZ2	; Act 2
	; MTZ
	zoneOffsetTableEntry.w Level_MTZ3	; Act 3
	zoneOffsetTableEntry.w Level_MTZ3	; Act 4
	; WFZ
	zoneOffsetTableEntry.w Level_WFZ	; Act 1
	zoneOffsetTableEntry.w Level_WFZ	; Act 2
	; HTZ
	zoneOffsetTableEntry.w Level_HTZ1	; Act 1
	zoneOffsetTableEntry.w Level_HTZ2	; Act 2
	; HPZ
	zoneOffsetTableEntry.w Level_HPZ1	; Act 1
	zoneOffsetTableEntry.w Level_HPZ1	; Act 2
	; Zone 9
	zoneOffsetTableEntry.w Level_Invalid	; Act 1
	zoneOffsetTableEntry.w Level_Invalid	; Act 2
	; OOZ
	zoneOffsetTableEntry.w Level_OOZ1	; Act 1
	zoneOffsetTableEntry.w Level_OOZ2	; Act 2
	; MCZ
	zoneOffsetTableEntry.w Level_MCZ1	; Act 1
	zoneOffsetTableEntry.w Level_MCZ2	; Act 2
	; CNZ
	zoneOffsetTableEntry.w Level_CNZ1	; Act 1
	zoneOffsetTableEntry.w Level_CNZ2	; Act 2
	; CPZ
	zoneOffsetTableEntry.w Level_CPZ1	; Act 1
	zoneOffsetTableEntry.w Level_CPZ2	; Act 2
	; DEZ
	zoneOffsetTableEntry.w Level_DEZ	; Act 1
	zoneOffsetTableEntry.w Level_DEZ	; Act 2
	; ARZ
	zoneOffsetTableEntry.w Level_ARZ1	; Act 1
	zoneOffsetTableEntry.w Level_ARZ2	; Act 2
	; SCZ
	zoneOffsetTableEntry.w Level_SCZ	; Act 1
	zoneOffsetTableEntry.w Level_SCZ	; Act 2
    zoneTableEnd

; These are all compressed in the Kosinski format.
Level_Invalid:
Level_EHZ1:	BINCLUDE	"level/layout/EHZ_1.kos"
	even
Level_EHZ2:	BINCLUDE	"level/layout/EHZ_2.kos"
	even
Level_MTZ1:	BINCLUDE	"level/layout/MTZ_1.kos"
	even
Level_MTZ2:	BINCLUDE	"level/layout/MTZ_2.kos"
	even
Level_MTZ3:	BINCLUDE	"level/layout/MTZ_3.kos"
	even
Level_WFZ:	BINCLUDE	"level/layout/WFZ.kos"
	even
Level_HTZ1:	BINCLUDE	"level/layout/HTZ_1.kos"
	even
Level_HTZ2:	BINCLUDE	"level/layout/HTZ_2.kos"
	even
Level_HPZ1:	;BINCLUDE	"level/layout/HPZ_1.kos"
	;even
Level_OOZ1:	BINCLUDE	"level/layout/OOZ_1.kos"
	even
Level_OOZ2:	BINCLUDE	"level/layout/OOZ_2.kos"
	even
Level_MCZ1:	BINCLUDE	"level/layout/MCZ_1.kos"
	even
Level_MCZ2:	BINCLUDE	"level/layout/MCZ_2.kos"
	even
Level_CNZ1:	BINCLUDE	"level/layout/CNZ_1.kos"
	even
Level_CNZ2:	BINCLUDE	"level/layout/CNZ_2.kos"
	even
Level_CPZ1:	BINCLUDE	"level/layout/CPZ_1.kos"
	even
Level_CPZ2:	BINCLUDE	"level/layout/CPZ_2.kos"
	even
Level_DEZ:	BINCLUDE	"level/layout/DEZ.kos"
	even
Level_ARZ1:	BINCLUDE	"level/layout/ARZ_1.kos"
	even
Level_ARZ2:	BINCLUDE	"level/layout/ARZ_2.kos"
	even
Level_SCZ:	BINCLUDE	"level/layout/SCZ.kos"
	even




;---------------------------------------------------------------------------------------
; Animated Level Art
;---------------------------------------------------------------------------------------
; EHZ and HTZ
ArtUnc_Flowers1:	BINCLUDE	"art/uncompressed/EHZ and HTZ flowers - 1.bin"
ArtUnc_Flowers2:	BINCLUDE	"art/uncompressed/EHZ and HTZ flowers - 2.bin"
ArtUnc_Flowers3:	BINCLUDE	"art/uncompressed/EHZ and HTZ flowers - 3.bin"
ArtUnc_Flowers4:	BINCLUDE	"art/uncompressed/EHZ and HTZ flowers - 4.bin"
ArtUnc_EHZPulseBall:	BINCLUDE	"art/uncompressed/Pulsing ball against checkered background (EHZ).bin"
ArtNem_HTZCliffs:	BINCLUDE	"art/nemesis/Dynamically reloaded cliffs in HTZ background.nem"
	even
ArtUnc_HTZClouds:	BINCLUDE	"art/uncompressed/Background clouds (HTZ).bin"

; MTZ
ArtUnc_MTZCylinder:	BINCLUDE	"art/uncompressed/Spinning metal cylinder (MTZ).bin"
ArtUnc_Lava:		BINCLUDE	"art/uncompressed/Lava.bin"
ArtUnc_MTZAnimBack:	BINCLUDE	"art/uncompressed/Animated section of MTZ background.bin"

; HPZ
ArtUnc_HPZPulseOrb:	;BINCLUDE	"art/uncompressed/Pulsing orb (HPZ).bin"

; OOZ
ArtUnc_OOZPulseBall:	BINCLUDE	"art/uncompressed/Pulsing ball (OOZ).bin"
ArtUnc_OOZSquareBall1:	BINCLUDE	"art/uncompressed/Square rotating around ball in OOZ - 1.bin"
ArtUnc_OOZSquareBall2:	BINCLUDE	"art/uncompressed/Square rotating around ball in OOZ - 2.bin"
ArtUnc_Oil1:		BINCLUDE	"art/uncompressed/Oil - 1.bin"
ArtUnc_Oil2:		BINCLUDE	"art/uncompressed/Oil - 2.bin"

; CNZ
ArtUnc_CNZFlipTiles:	BINCLUDE	"art/uncompressed/Flipping foreground section (CNZ).bin"
ArtUnc_CNZSlotPics:	BINCLUDE	"art/uncompressed/Slot pictures.bin"
ArtUnc_CPZAnimBack:	BINCLUDE	"art/uncompressed/Animated background section (CPZ and DEZ).bin"

; ARZ
ArtUnc_Waterfall1:	BINCLUDE	"art/uncompressed/ARZ waterfall patterns - 1.bin"
ArtUnc_Waterfall2:	BINCLUDE	"art/uncompressed/ARZ waterfall patterns - 2.bin"
ArtUnc_Waterfall3:	BINCLUDE	"art/uncompressed/ARZ waterfall patterns - 3.bin"

;---------------------------------------------------------------------------------------
; Player Assets
;---------------------------------------------------------------------------------------
	align $20
ArtUnc_Sonic:			BINCLUDE	"art/uncompressed/Sonic's art.bin"
	align $20
ArtUnc_Tails:			BINCLUDE	"art/uncompressed/Tails's art.bin"

MapUnc_Sonic:			include		"mappings/sprite/Sonic.asm"

MapRUnc_Sonic:			include		"mappings/spriteDPLC/Sonic.asm"

ArtNem_Shield:			BINCLUDE	"art/nemesis/Shield.nem"
	even
ArtNem_Invincible_stars:	BINCLUDE	"art/nemesis/Invincibility stars.nem"
	even
ArtUnc_SplashAndDust:		BINCLUDE	"art/uncompressed/Splash and skid dust.bin"

ArtNem_SuperSonic_stars:	BINCLUDE	"art/nemesis/Super Sonic stars.nem"
	even
MapUnc_Tails:			include		"mappings/sprite/Tails.asm"

MapRUnc_Tails:			include		"mappings/spriteDPLC/Tails.asm"

;---------------------------------------------------------------------------------------
; Sega Screen Assets
;---------------------------------------------------------------------------------------
ArtNem_SEGA:			BINCLUDE	"art/nemesis/SEGA.nem"
	even
ArtNem_IntroTrails:		BINCLUDE	"art/nemesis/Shaded blocks from intro.nem"
	even
MapEng_SEGA:			BINCLUDE	"mappings/misc/SEGA mappings.eni"
	even

;---------------------------------------------------------------------------------------
; Title Screen Assets
;---------------------------------------------------------------------------------------
MapEng_TitleScreen:		BINCLUDE	"mappings/misc/Mappings for title screen background.eni"
	even
MapEng_TitleBack:		BINCLUDE	"mappings/misc/Mappings for title screen background 2.eni" ; title screen background (smaller part, water/horizon)
	even
MapEng_TitleLogo:		BINCLUDE	"mappings/misc/Sonic the Hedgehog 2 title screen logo mappings.eni"
	even
ArtNem_Title:			BINCLUDE	"art/nemesis/Main patterns from title screen.nem"
	even
ArtNem_TitleSprites:		BINCLUDE	"art/nemesis/Sonic and Tails from title screen.nem"
	even
ArtNem_MenuJunk:		BINCLUDE	"art/nemesis/A few menu blocks.nem"
	even

;---------------------------------------------------------------------------------------
; General Level Assets
;---------------------------------------------------------------------------------------
ArtNem_Button:			BINCLUDE	"art/nemesis/Button.nem"
	even
ArtNem_VrtclSprng:		BINCLUDE	"art/nemesis/Vertical spring.nem"
	even
ArtNem_HrzntlSprng:		BINCLUDE	"art/nemesis/Horizontal spring.nem"
	even
ArtNem_DignlSprng:		BINCLUDE	"art/nemesis/Diagonal spring.nem"
	even
ArtNem_HUD:			BINCLUDE	"art/nemesis/HUD.nem" ; Score, Rings, Time
	even
ArtNem_Sonic_life_counter:	BINCLUDE	"art/nemesis/Sonic lives counter.nem"
	even
ArtNem_Ring:			BINCLUDE	"art/nemesis/Ring.nem"
	even
ArtNem_Powerups:		BINCLUDE	"art/nemesis/Monitor and contents.nem"
	even
ArtNem_Spikes:			BINCLUDE	"art/nemesis/Spikes.nem"
	even
ArtNem_Numbers:			BINCLUDE	"art/nemesis/Numbers.nem"
	even
ArtNem_Checkpoint:		BINCLUDE	"art/nemesis/Star pole.nem"
	even
ArtNem_Signpost:		BINCLUDE	"art/nemesis/Signpost.nem" ; For one-player mode.
	even
ArtUnc_Signpost:		BINCLUDE	"art/uncompressed/Signpost.bin" ; For two-player mode.
	even
ArtNem_LeverSpring:		BINCLUDE	"art/nemesis/Lever spring.nem"
	even
ArtNem_HorizSpike:		BINCLUDE	"art/nemesis/Long horizontal spike.nem"
	even
ArtNem_BigBubbles:		BINCLUDE	"art/nemesis/Bubble generator.nem" ; Bubble from underwater
	even
ArtNem_Bubbles:			BINCLUDE	"art/nemesis/Bubbles.nem" ; Bubbles from character
	even
ArtUnc_Countdown:		BINCLUDE	"art/uncompressed/Numbers for drowning countdown.bin"
	even
ArtNem_Game_Over:		BINCLUDE	"art/nemesis/Game and Time Over text.nem"
	even
ArtNem_Explosion:		BINCLUDE	"art/nemesis/Explosion.nem"
	even
ArtNem_MilesLife:		BINCLUDE	"art/nemesis/Miles life counter.nem"
	even
ArtNem_Capsule:			BINCLUDE	"art/nemesis/Egg Prison.nem"
	even
ArtNem_ContinueTails:		BINCLUDE	"art/nemesis/Tails on continue screen.nem"
	even
ArtNem_MiniSonic:		BINCLUDE	"art/nemesis/Sonic continue.nem"
	even
ArtNem_TailsLife:		BINCLUDE	"art/nemesis/Tails life counter.nem"
	even
ArtNem_MiniTails:		BINCLUDE	"art/nemesis/Tails continue.nem"
	even

;---------------------------------------------------------------------------------------
; Menu Assets
;---------------------------------------------------------------------------------------
ArtNem_FontStuff:		BINCLUDE	"art/nemesis/Standard font.nem"
	even
ArtNem_1P2PWins:		BINCLUDE	"art/nemesis/1P and 2P wins text from 2P mode.nem"
	even
MapEng_MenuBack:		BINCLUDE	"mappings/misc/Sonic and Miles animated background.eni"
	even
ArtUnc_MenuBack:		BINCLUDE	"art/uncompressed/Sonic and Miles animated background.bin"
	even
ArtNem_TitleCard:		BINCLUDE	"art/nemesis/Title card.nem"
	even
ArtNem_TitleCard2:		BINCLUDE	"art/nemesis/Font using large broken letters.nem"
	even
ArtNem_MenuBox:			BINCLUDE	"art/nemesis/A menu box with a shadow.nem"
	even
ArtNem_LevelSelectPics:		BINCLUDE	"art/nemesis/Pictures in level preview box from level select.nem"
	even
ArtNem_ResultsText:		BINCLUDE	"art/nemesis/End of level results text.nem" ; Text for Sonic or Tails Got Through Act and Bonus/Perfect
	even
ArtNem_SpecialStageResults:	BINCLUDE	"art/nemesis/Special stage results screen art and some emeralds.nem"
	even
ArtNem_Perfect:			BINCLUDE	"art/nemesis/Perfect text.nem"
	even

;---------------------------------------------------------------------------------------
; Small Animal Assets
;---------------------------------------------------------------------------------------
ArtNem_Flicky:			BINCLUDE	"art/nemesis/Flicky.nem"
	even
ArtNem_Squirrel:		BINCLUDE	"art/nemesis/Squirrel.nem" ; Ricky
	even
ArtNem_Mouse:			BINCLUDE	"art/nemesis/Mouse.nem"    ; Micky
	even
ArtNem_Chicken:			BINCLUDE	"art/nemesis/Chicken.nem"  ; Cucky
	even
ArtNem_Monkey:			BINCLUDE	"art/nemesis/Monkey.nem"   ; Wocky
	even
ArtNem_Eagle:			BINCLUDE	"art/nemesis/Eagle.nem"    ; Locky
	even
ArtNem_Pig:			BINCLUDE	"art/nemesis/Pig.nem"      ; Picky
	even
ArtNem_Seal:			BINCLUDE	"art/nemesis/Seal.nem"     ; Rocky
	even
ArtNem_Penguin:			BINCLUDE	"art/nemesis/Penguin.nem"  ; Pecky
	even
ArtNem_Turtle:			BINCLUDE	"art/nemesis/Turtle.nem"   ; Tocky
	even
ArtNem_Bear:			BINCLUDE	"art/nemesis/Bear.nem"     ; Becky
	even
ArtNem_Rabbit:			BINCLUDE	"art/nemesis/Rabbit.nem"   ; Pocky
	even

;---------------------------------------------------------------------------------------
; WFZ Assets
;---------------------------------------------------------------------------------------
ArtNem_WfzSwitch:		BINCLUDE	"art/nemesis/WFZ boss chamber switch.nem" ; Rivet thing that you bust to get inside the ship
	even
ArtNem_BreakPanels:		BINCLUDE	"art/nemesis/Breakaway panels from WFZ.nem"
	even

;---------------------------------------------------------------------------------------
; OOZ Assets
;---------------------------------------------------------------------------------------
ArtNem_SpikyThing:		BINCLUDE	"art/nemesis/Spiked ball from OOZ.nem"
	even
ArtNem_BurnerLid:		BINCLUDE	"art/nemesis/Burner Platform from OOZ.nem"
	even
ArtNem_StripedBlocksVert:	BINCLUDE	"art/nemesis/Striped blocks from CPZ.nem"
	even
ArtNem_Oilfall:			BINCLUDE	"art/nemesis/Cascading oil hitting oil from OOZ.nem"
	even
ArtNem_Oilfall2:		BINCLUDE	"art/nemesis/Cascading oil from OOZ.nem"
	even
ArtNem_BallThing:		BINCLUDE	"art/nemesis/Ball on spring from OOZ (beta holdovers).nem"
	even
ArtNem_LaunchBall:		BINCLUDE	"art/nemesis/Transporter ball from OOZ.nem"
	even
ArtNem_OOZPlatform:		BINCLUDE	"art/nemesis/OOZ collapsing platform.nem"
	even
ArtNem_PushSpring:		BINCLUDE	"art/nemesis/Push spring from OOZ.nem"
	even
ArtNem_OOZSwingPlat:		BINCLUDE	"art/nemesis/Swinging platform from OOZ.nem"
	even
ArtNem_StripedBlocksHoriz:	BINCLUDE	"art/nemesis/4 stripy blocks from OOZ.nem"
	even
ArtNem_OOZElevator:		BINCLUDE	"art/nemesis/Rising platform from OOZ.nem"
	even
ArtNem_OOZFanHoriz:		BINCLUDE	"art/nemesis/Fan from OOZ.nem"
	even
ArtNem_OOZBurn:			BINCLUDE	"art/nemesis/Green flame from OOZ burners.nem"
	even

;---------------------------------------------------------------------------------------
; CNZ Assets
;---------------------------------------------------------------------------------------
ArtNem_CNZSnake:		BINCLUDE	"art/nemesis/Caterpiller platforms from CNZ.nem" ; Patterns for appearing and disappearing string of platforms
	even
ArtNem_CNZBonusSpike:		BINCLUDE	"art/nemesis/Spikey ball from CNZ slots.nem"
	even
ArtNem_BigMovingBlock:		BINCLUDE	"art/nemesis/Moving block from CNZ and CPZ.nem"
	even
ArtNem_CNZElevator:		BINCLUDE	"art/nemesis/CNZ elevator.nem"
	even
ArtNem_CNZCage:			BINCLUDE	"art/nemesis/CNZ slot machine bars.nem"
	even
ArtNem_CNZHexBumper:		BINCLUDE	"art/nemesis/Hexagonal bumper from CNZ.nem"
	even
ArtNem_CNZRoundBumper:		BINCLUDE	"art/nemesis/Round bumper from CNZ.nem"
	even
ArtNem_CNZDiagPlunger:		BINCLUDE	"art/nemesis/Diagonal impulse spring from CNZ.nem"
	even
ArtNem_CNZVertPlunger:		BINCLUDE	"art/nemesis/Vertical impulse spring.nem"
	even
ArtNem_CNZMiniBumper:		BINCLUDE	"art/nemesis/Drop target from CNZ.nem" ; Weird blocks that you hit 3 times to get rid of
	even
ArtNem_CNZFlipper:		BINCLUDE	"art/nemesis/Flippers.nem"
	even

;---------------------------------------------------------------------------------------
; CPZ Assets
;---------------------------------------------------------------------------------------
ArtNem_CPZElevator:		BINCLUDE	"art/nemesis/Large moving platform from CPZ.nem"
	even
ArtNem_WaterSurface:		BINCLUDE	"art/nemesis/Top of water in HPZ and CNZ.nem"
	even
ArtNem_CPZBooster:		BINCLUDE	"art/nemesis/Speed booster from CPZ.nem"
	even
ArtNem_CPZDroplet:		BINCLUDE	"art/nemesis/CPZ worm enemy.nem"
	even
ArtNem_CPZMetalThings:		BINCLUDE	"art/nemesis/CPZ metal things.nem" ; Girder, cylinders
	even
ArtNem_CPZMetalBlock:		BINCLUDE	"art/nemesis/CPZ large moving platform blocks.nem"
	even
ArtNem_ConstructionStripes:	BINCLUDE	"art/nemesis/Stripy blocks from CPZ.nem"
	even
ArtNem_CPZAnimatedBits:		BINCLUDE	"art/nemesis/Small yellow moving platform from CPZ.nem"
	even
ArtNem_CPZStairBlock:		BINCLUDE	"art/nemesis/Moving block from CPZ.nem"
	even
ArtNem_CPZTubeSpring:		BINCLUDE	"art/nemesis/CPZ spintube exit cover.nem"
	even

;---------------------------------------------------------------------------------------
; ARZ Assets
;---------------------------------------------------------------------------------------
ArtNem_WaterSurface2:		BINCLUDE	"art/nemesis/Top of water in ARZ.nem"
	even
ArtNem_Leaves:			BINCLUDE	"art/nemesis/Leaves in ARZ.nem"
	even
ArtNem_ArrowAndShooter:		BINCLUDE	"art/nemesis/Arrow shooter and arrow from ARZ.nem"
	even
ArtNem_ARZBarrierThing:		BINCLUDE	"art/nemesis/One way barrier from ARZ.nem" ; Unused
	even

;---------------------------------------------------------------------------------------
; EHZ/OOZ Badnik Assets
;---------------------------------------------------------------------------------------
; These Badniks being grouped together here is unusual, but can be explained by two things:
; 1. This is where all Badnik tiles were kept in the earliest prototypes.
; 2. These are the only Badniks left from those prototypes.
ArtNem_Buzzer:			BINCLUDE	"art/nemesis/Buzzer enemy.nem"
	even
ArtNem_Octus:			BINCLUDE	"art/nemesis/Octopus badnik from OOZ.nem"
	even
ArtNem_Aquis:			BINCLUDE	"art/nemesis/Seahorse from OOZ.nem"
	even
ArtNem_Masher:			BINCLUDE	"art/nemesis/EHZ Pirahna badnik.nem"
	even

;---------------------------------------------------------------------------------------
; Boss Assets
;---------------------------------------------------------------------------------------
ArtNem_Eggpod:			BINCLUDE	"art/nemesis/Eggpod.nem" ; Robotnik's main ship
	even
ArtNem_CPZBoss:			BINCLUDE	"art/nemesis/CPZ boss.nem"
	even
ArtNem_FieryExplosion:		BINCLUDE	"art/nemesis/Large explosion.nem"
	even
ArtNem_EggpodJets:		BINCLUDE	"art/nemesis/Horizontal jet.nem"
	even
ArtNem_BossSmoke:		BINCLUDE	"art/nemesis/Smoke trail from CPZ and HTZ bosses.nem"
	even
ArtNem_EHZBoss:			BINCLUDE	"art/nemesis/EHZ boss.nem"
	even
ArtNem_EggChoppers:		BINCLUDE	"art/nemesis/Chopper blades for EHZ boss.nem"
	even
ArtNem_HTZBoss:			BINCLUDE	"art/nemesis/HTZ boss.nem"
	even
ArtNem_ARZBoss:			BINCLUDE	"art/nemesis/ARZ boss.nem"
	even
ArtNem_MCZBoss:			BINCLUDE	"art/nemesis/MCZ boss.nem"
	even
ArtNem_CNZBoss:			BINCLUDE	"art/nemesis/CNZ boss.nem"
	even
ArtNem_OOZBoss:			BINCLUDE	"art/nemesis/OOZ boss.nem"
	even
ArtNem_MTZBoss:			BINCLUDE	"art/nemesis/MTZ boss.nem"
	even
ArtUnc_FallingRocks:		BINCLUDE	"art/uncompressed/Falling rocks and stalactites from MCZ.bin"
	even

;---------------------------------------------------------------------------------------
; ARZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_Whisp:			BINCLUDE	"art/nemesis/Blowfly from ARZ.nem"
	even
ArtNem_Grounder:		BINCLUDE	"art/nemesis/Grounder from ARZ.nem"
	even
ArtNem_ChopChop:		BINCLUDE	"art/nemesis/Shark from ARZ.nem"
	even

;---------------------------------------------------------------------------------------
; HTZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_Rexon:			BINCLUDE	"art/nemesis/Rexxon (lava snake) from HTZ.nem"
	even
ArtNem_Spiker:			BINCLUDE	"art/nemesis/Driller badnik from HTZ.nem"
	even

;---------------------------------------------------------------------------------------
; SCZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_Nebula:			BINCLUDE	"art/nemesis/Bomber badnik from SCZ.nem"
	even
ArtNem_Turtloid:		BINCLUDE	"art/nemesis/Turtle badnik from SCZ.nem"
	even

;---------------------------------------------------------------------------------------
; EHZ Badnik Assets (again)
;---------------------------------------------------------------------------------------
ArtNem_Coconuts:		BINCLUDE	"art/nemesis/Coconuts badnik from EHZ.nem"
	even

;---------------------------------------------------------------------------------------
; MCZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_Crawlton:		BINCLUDE	"art/nemesis/Snake badnik from MCZ.nem"
	even
ArtNem_Flasher:			BINCLUDE	"art/nemesis/Firefly from MCZ.nem"
	even

;---------------------------------------------------------------------------------------
; MTZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_MtzMantis:		BINCLUDE	"art/nemesis/Praying mantis badnik from MTZ.nem"
	even
ArtNem_Shellcracker:		BINCLUDE	"art/nemesis/Shellcracker badnik from MTZ.nem"
	even
ArtNem_MtzSupernova:		BINCLUDE	"art/nemesis/Exploding star badnik from MTZ.nem"
	even

;---------------------------------------------------------------------------------------
; CPZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_Spiny:			BINCLUDE	"art/nemesis/Weird crawling badnik from CPZ.nem"
	even
ArtNem_Grabber:			BINCLUDE	"art/nemesis/Spider badnik from CPZ.nem"
	even

;---------------------------------------------------------------------------------------
; WFZ Badnik Assets
;---------------------------------------------------------------------------------------
ArtNem_WfzScratch:		BINCLUDE	"art/nemesis/Scratch from WFZ.nem" ; Chicken badnik
	even
ArtNem_Balkrie:			BINCLUDE	"art/nemesis/Balkrie (jet badnik) from SCZ.nem" ; This SCZ badnik is here for some reason.
	even

;---------------------------------------------------------------------------------------
; WFZ/DEZ Assets
; It seems that these were haphazardly thrown together instead of neatly-split like the
; other zones' assets.
;---------------------------------------------------------------------------------------
ArtNem_SilverSonic:		BINCLUDE	"art/nemesis/Silver Sonic.nem"
	even
ArtNem_Tornado:			BINCLUDE	"art/nemesis/The Tornado.nem" ; Sonic's plane.
	even
ArtNem_WfzWallTurret:		BINCLUDE	"art/nemesis/Wall turret from WFZ.nem"
	even
ArtNem_WfzHook:			BINCLUDE	"art/nemesis/Hook on chain from WFZ.nem"
	even
ArtNem_WfzGunPlatform:		BINCLUDE	"art/nemesis/Retracting platform from WFZ.nem"
	even
ArtNem_WfzConveyorBeltWheel:	BINCLUDE	"art/nemesis/Wheel for belt in WFZ.nem"
	even
ArtNem_WfzFloatingPlatform:	BINCLUDE	"art/nemesis/Moving platform from WFZ.nem"
	even
ArtNem_WfzVrtclLazer:		BINCLUDE	"art/nemesis/Unused vertical laser in WFZ.nem"
	even
ArtNem_Clouds:			BINCLUDE	"art/nemesis/Clouds.nem"
	even
ArtNem_WfzHrzntlLazer:		BINCLUDE	"art/nemesis/Red horizontal laser from WFZ.nem"
	even
ArtNem_WfzLaunchCatapult:	BINCLUDE	"art/nemesis/Catapult that shoots Sonic to the side from WFZ.nem"
	even
ArtNem_WfzBeltPlatform:		BINCLUDE	"art/nemesis/Platform on belt in WFZ.nem"
	even
ArtNem_WfzUnusedBadnik:		BINCLUDE	"art/nemesis/Unused badnik from WFZ.nem" ; This is not grouped with the zone's badniks, suggesting that it's not a badnik at all.
	even
ArtNem_WfzVrtclPrpllr:		BINCLUDE	"art/nemesis/Vertical spinning blades in WFZ.nem"
	even
ArtNem_WfzHrzntlPrpllr:		BINCLUDE	"art/nemesis/Horizontal spinning blades in WFZ.nem"
	even
ArtNem_WfzTiltPlatforms:	BINCLUDE	"art/nemesis/Tilting plaforms in WFZ.nem"
	even
ArtNem_WfzThrust:		BINCLUDE	"art/nemesis/Thrust from Robotnik's getaway ship in WFZ.nem"
	even
ArtNem_WFZBoss:			BINCLUDE	"art/nemesis/WFZ boss.nem"
	even
ArtNem_RobotnikUpper:		BINCLUDE	"art/nemesis/Robotnik's head.nem"
	even
ArtNem_RobotnikRunning:		BINCLUDE	"art/nemesis/Robotnik.nem"
	even
ArtNem_RobotnikLower:		BINCLUDE	"art/nemesis/Robotnik's lower half.nem"
	even
ArtNem_DEZWindow:		BINCLUDE	"art/nemesis/Window in back that Robotnik looks through in DEZ.nem"
	even
ArtNem_DEZBoss:			BINCLUDE	"art/nemesis/Eggrobo.nem"
	even
; This last-minute badnik addition was mistakenly included with the WFZ/DEZ assets instead of in its own 'CNZ Badnik Assets' section.
ArtNem_Crawl:			BINCLUDE	"art/nemesis/Bouncer badnik from CNZ.nem"
	even
ArtNem_TornadoThruster:		BINCLUDE	"art/nemesis/Rocket thruster for Tornado.nem"
	even

;---------------------------------------------------------------------------------------
; Ending Assets
;---------------------------------------------------------------------------------------
MapEng_Ending1:			BINCLUDE	"mappings/misc/End of game sequence frame 1.eni"
	even
MapEng_Ending2:			BINCLUDE	"mappings/misc/End of game sequence frame 2.eni"
	even
MapEng_Ending3:			BINCLUDE	"mappings/misc/End of game sequence frame 3.eni"
	even
MapEng_Ending4:			BINCLUDE	"mappings/misc/End of game sequence frame 4.eni"
	even
MapEng_EndingTailsPlane:	BINCLUDE	"mappings/misc/Closeup of Tails flying plane in ending sequence.eni"
	even
MapEng_EndingSonicPlane:	BINCLUDE	"mappings/misc/Closeup of Sonic flying plane in ending sequence.eni"
	even
; Strange unused mappings (duplicates of MapEng_EndGameLogo)
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even
				BINCLUDE	"mappings/misc/Sonic 2 end of game logo.eni"
	even

ArtNem_EndingPics:		BINCLUDE	"art/nemesis/Movie sequence at end of game.nem"
	even
ArtNem_EndingFinalTornado:	BINCLUDE	"art/nemesis/Final image of Tornado with it and Sonic facing screen.nem"
	even
ArtNem_EndingMiniTornado:	BINCLUDE	"art/nemesis/Small pictures of Tornado in final ending sequence.nem"
	even
ArtNem_EndingSonic:		BINCLUDE	"art/nemesis/Small pictures of Sonic and final image of Sonic.nem"
	even
ArtNem_EndingSuperSonic:	BINCLUDE	"art/nemesis/Small pictures of Sonic and final image of Sonic in Super Sonic mode.nem"
	even
ArtNem_EndingTails:		BINCLUDE	"art/nemesis/Final image of Tails.nem"
	even
ArtNem_EndingTitle:		BINCLUDE	"art/nemesis/Sonic the Hedgehog 2 image at end of credits.nem"
	even


; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; LEVEL ART AND BLOCK MAPPINGS (16x16 and 128x128)
;
; #define BLOCK_TBL_LEN  // table length unknown
; #define BIGBLOCK_TBL_LEN // table length unknown
; typedef uint16_t uword
;
; struct blockMapElement {
;  uword unk : 5;    // u
;  uword patternIndex : 11; };  // i
; // uuuu uiii iiii iiii
;
; blockMapElement (*blockMapTable)[BLOCK_TBL_LEN][4] = 0xFFFF9000
;
; struct bigBlockMapElement {
;  uword : 4
;  uword blockMapIndex : 12; };  //I
; // 0000 IIII IIII IIII
;
; bigBlockMapElement (*bigBlockMapTable)[BIGBLOCK_TBL_LEN][64] = 0xFFFF0000
;
; /*
; This data determines how the level blocks will be constructed graphically. There are
; two kinds of block mappings: 16x16 and 128x128.
;
; 16x16 blocks are made up of four cells arranged in a square (thus, 16x16 pixels).
; Two bytes are used to define each cell, so the block is 8 bytes long. It can be
; represented by the bitmap blockMapElement, of which the members are:
;
; unk
;  These bits have to do with pattern orientation. I do not know their exact
;  meaning.
; patternIndex
;  The pattern's address divided by $20. Otherwise said: an index into the
;  pattern array.
;
; Each mapping can be expressed as an array of four blockMapElements, while the
; whole table is expressed as a two-dimensional array of blockMapElements (blockMapTable).
; The maps are read in left-to-right, top-to-bottom order.
;
; 128x128 maps are basically lists of indices into blockMapTable. The levels are built
; out of these "big blocks", rather than the "small" 16x16 blocks. bigBlockMapTable is,
; predictably, the table of big block mappings.
; Each big block is 8 16x16 blocks, or 16 cells, square. This produces a total of 16
; blocks or 64 cells.
; As noted earlier, each element of the table provides 'i' for blockMapTable[i][j].
; */

; All of these are compressed in the Kosinski format.

BM16_EHZ:	BINCLUDE	"mappings/16x16/EHZ.kos"
ArtKos_EHZ:	BINCLUDE	"art/kosinski/EHZ_HTZ.kos"
BM16_HTZ:	BINCLUDE	"mappings/16x16/HTZ.kos"
ArtKos_HTZ:	BINCLUDE	"art/kosinski/HTZ_Supp.kos" ; HTZ pattern suppliment to EHZ level patterns
BM128_EHZ:	BINCLUDE	"mappings/128x128/EHZ_HTZ.kos"

BM16_MTZ:	BINCLUDE	"mappings/16x16/MTZ.kos"
ArtKos_MTZ:	BINCLUDE	"art/kosinski/MTZ.kos"
BM128_MTZ:	BINCLUDE	"mappings/128x128/MTZ.kos"

BM16_HPZ:	;BINCLUDE	"mappings/16x16/HPZ.kos"
ArtKos_HPZ:	;BINCLUDE	"art/kosinski/HPZ.kos"
BM128_HPZ:	;BINCLUDE	"mappings/128x128/HPZ.kos"

BM16_OOZ:	BINCLUDE	"mappings/16x16/OOZ.kos"
ArtKos_OOZ:	BINCLUDE	"art/kosinski/OOZ.kos"
BM128_OOZ:	BINCLUDE	"mappings/128x128/OOZ.kos"

BM16_MCZ:	BINCLUDE	"mappings/16x16/MCZ.kos"
ArtKos_MCZ:	BINCLUDE	"art/kosinski/MCZ.kos"
BM128_MCZ:	BINCLUDE	"mappings/128x128/MCZ.kos"

BM16_CNZ:	BINCLUDE	"mappings/16x16/CNZ.kos"
ArtKos_CNZ:	BINCLUDE	"art/kosinski/CNZ.kos"
BM128_CNZ:	BINCLUDE	"mappings/128x128/CNZ.kos"

BM16_CPZ:	BINCLUDE	"mappings/16x16/CPZ_DEZ.kos"
ArtKos_CPZ:	BINCLUDE	"art/kosinski/CPZ_DEZ.kos"
BM128_CPZ:	BINCLUDE	"mappings/128x128/CPZ_DEZ.kos"

; This file contains $320 blocks, overflowing the 'Block_table' buffer. This causes
; 'TempArray_LayerDef' to be overwritten with (empty) block data.
; If only 'fixBugs' could fix this...
BM16_ARZ:	BINCLUDE	"mappings/16x16/ARZ.kos"
ArtKos_ARZ:	BINCLUDE	"art/kosinski/ARZ.kos"
BM128_ARZ:	BINCLUDE	"mappings/128x128/ARZ.kos"

BM16_WFZ:	BINCLUDE	"mappings/16x16/WFZ_SCZ.kos"
ArtKos_SCZ:	BINCLUDE	"art/kosinski/WFZ_SCZ.kos"
ArtKos_WFZ:	BINCLUDE	"art/kosinski/WFZ_Supp.kos" ; WFZ pattern suppliment to SCZ tiles
BM128_WFZ:	BINCLUDE	"mappings/128x128/WFZ_SCZ.kos"

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;-----------------------------------------------------------------------------------
; Special Stage Assets
;-----------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------
; Exit curve + slope up
;-----------------------------------------------------------------------------------
MapSpec_Rise1:		BINCLUDE	"mappings/special stage/Slope up - Frame 1.bin"
MapSpec_Rise2:		BINCLUDE	"mappings/special stage/Slope up - Frame 2.bin"
MapSpec_Rise3:		BINCLUDE	"mappings/special stage/Slope up - Frame 3.bin"
MapSpec_Rise4:		BINCLUDE	"mappings/special stage/Slope up - Frame 4.bin"
MapSpec_Rise5:		BINCLUDE	"mappings/special stage/Slope up - Frame 5.bin"
MapSpec_Rise6:		BINCLUDE	"mappings/special stage/Slope up - Frame 6.bin"
MapSpec_Rise7:		BINCLUDE	"mappings/special stage/Slope up - Frame 7.bin"
MapSpec_Rise8:		BINCLUDE	"mappings/special stage/Slope up - Frame 8.bin"
MapSpec_Rise9:		BINCLUDE	"mappings/special stage/Slope up - Frame 9.bin"
MapSpec_Rise10:		BINCLUDE	"mappings/special stage/Slope up - Frame 10.bin"
MapSpec_Rise11:		BINCLUDE	"mappings/special stage/Slope up - Frame 11.bin"
MapSpec_Rise12:		BINCLUDE	"mappings/special stage/Slope up - Frame 12.bin"
MapSpec_Rise13:		BINCLUDE	"mappings/special stage/Slope up - Frame 13.bin"
MapSpec_Rise14:		BINCLUDE	"mappings/special stage/Slope up - Frame 14.bin"
MapSpec_Rise15:		BINCLUDE	"mappings/special stage/Slope up - Frame 15.bin"
MapSpec_Rise16:		BINCLUDE	"mappings/special stage/Slope up - Frame 16.bin"
MapSpec_Rise17:		BINCLUDE	"mappings/special stage/Slope up - Frame 17.bin"

;-----------------------------------------------------------------------------------
; Straight path
;-----------------------------------------------------------------------------------
MapSpec_Straight1:	BINCLUDE	"mappings/special stage/Straight path - Frame 1.bin"
MapSpec_Straight2:	BINCLUDE	"mappings/special stage/Straight path - Frame 2.bin"
MapSpec_Straight3:	BINCLUDE	"mappings/special stage/Straight path - Frame 3.bin"
MapSpec_Straight4:	BINCLUDE	"mappings/special stage/Straight path - Frame 4.bin"

;-----------------------------------------------------------------------------------
; Exit curve + slope down
;-----------------------------------------------------------------------------------
MapSpec_Drop1:		BINCLUDE	"mappings/special stage/Slope down - Frame 1.bin"
MapSpec_Drop2:		BINCLUDE	"mappings/special stage/Slope down - Frame 2.bin"
MapSpec_Drop3:		BINCLUDE	"mappings/special stage/Slope down - Frame 3.bin"
MapSpec_Drop4:		BINCLUDE	"mappings/special stage/Slope down - Frame 4.bin"
MapSpec_Drop5:		BINCLUDE	"mappings/special stage/Slope down - Frame 5.bin"
MapSpec_Drop6:		BINCLUDE	"mappings/special stage/Slope down - Frame 6.bin"
MapSpec_Drop7:		BINCLUDE	"mappings/special stage/Slope down - Frame 7.bin"
MapSpec_Drop8:		BINCLUDE	"mappings/special stage/Slope down - Frame 8.bin"
MapSpec_Drop9:		BINCLUDE	"mappings/special stage/Slope down - Frame 9.bin"
MapSpec_Drop10:		BINCLUDE	"mappings/special stage/Slope down - Frame 10.bin"
MapSpec_Drop11:		BINCLUDE	"mappings/special stage/Slope down - Frame 11.bin"
MapSpec_Drop12:		BINCLUDE	"mappings/special stage/Slope down - Frame 12.bin"
MapSpec_Drop13:		BINCLUDE	"mappings/special stage/Slope down - Frame 13.bin"
MapSpec_Drop14:		BINCLUDE	"mappings/special stage/Slope down - Frame 14.bin"
MapSpec_Drop15:		BINCLUDE	"mappings/special stage/Slope down - Frame 15.bin"
MapSpec_Drop16:		BINCLUDE	"mappings/special stage/Slope down - Frame 16.bin"
MapSpec_Drop17:		BINCLUDE	"mappings/special stage/Slope down - Frame 17.bin"

;-----------------------------------------------------------------------------------
; Curved path
;-----------------------------------------------------------------------------------
MapSpec_Turning1:	BINCLUDE	"mappings/special stage/Curve right - Frame 1.bin"
MapSpec_Turning2:	BINCLUDE	"mappings/special stage/Curve right - Frame 2.bin"
MapSpec_Turning3:	BINCLUDE	"mappings/special stage/Curve right - Frame 3.bin"
MapSpec_Turning4:	BINCLUDE	"mappings/special stage/Curve right - Frame 4.bin"
MapSpec_Turning5:	BINCLUDE	"mappings/special stage/Curve right - Frame 5.bin"
MapSpec_Turning6:	BINCLUDE	"mappings/special stage/Curve right - Frame 6.bin"

;-----------------------------------------------------------------------------------
; Exit curve
;-----------------------------------------------------------------------------------
MapSpec_Unturn1:	BINCLUDE	"mappings/special stage/Curve right - Frame 7.bin"
MapSpec_Unturn2:	BINCLUDE	"mappings/special stage/Curve right - Frame 8.bin"
MapSpec_Unturn3:	BINCLUDE	"mappings/special stage/Curve right - Frame 9.bin"
MapSpec_Unturn4:	BINCLUDE	"mappings/special stage/Curve right - Frame 10.bin"
MapSpec_Unturn5:	BINCLUDE	"mappings/special stage/Curve right - Frame 11.bin"

;-----------------------------------------------------------------------------------
; Enter curve
;-----------------------------------------------------------------------------------
MapSpec_Turn1:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 1.bin"
MapSpec_Turn2:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 2.bin"
MapSpec_Turn3:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 3.bin"
MapSpec_Turn4:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 4.bin"
MapSpec_Turn5:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 5.bin"
MapSpec_Turn6:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 6.bin"
MapSpec_Turn7:		BINCLUDE	"mappings/special stage/Begin curve right - Frame 7.bin"

;--------------------------------------------------------------------------------------
; Special stage level patterns
; Note: Only one line of each tile is stored in this archive. The other 7 lines are
;  the same as this one line, so to get the full tiles, each line needs to be
;  duplicated 7 times over.					; ArtKoz_DCA38:
;--------------------------------------------------------------------------------------
ArtKos_Special:			BINCLUDE	"art/kosinski/SpecStag.kos"
	even

ArtNem_SpecialBack:		BINCLUDE	"art/nemesis/Background art for special stage.nem"
	even
MapEng_SpecialBack:		BINCLUDE	"mappings/misc/Main background mappings for special stage.eni"
	even
MapEng_SpecialBackBottom:	BINCLUDE	"mappings/misc/Lower background mappings for special stage.eni"
	even
ArtNem_SpecialHUD:		BINCLUDE	"art/nemesis/Sonic and Miles number text from special stage.nem"
	even
ArtNem_SpecialStart:		BINCLUDE	"art/nemesis/Start text from special stage.nem" ; Also includes checkered flag
	even
ArtNem_SpecialStars:		BINCLUDE	"art/nemesis/Stars in special stage.nem"
	even
ArtNem_SpecialPlayerVSPlayer:	BINCLUDE	"art/nemesis/Special stage Player VS Player text.nem"
	even
ArtNem_SpecialRings:		BINCLUDE	"art/nemesis/Special stage ring art.nem"
	even
ArtNem_SpecialFlatShadow:	BINCLUDE	"art/nemesis/Horizontal shadow from special stage.nem"
	even
ArtNem_SpecialDiagShadow:	BINCLUDE	"art/nemesis/Diagonal shadow from special stage.nem"
	even
ArtNem_SpecialSideShadow:	BINCLUDE	"art/nemesis/Vertical shadow from special stage.nem"
	even
ArtNem_SpecialExplosion:	BINCLUDE	"art/nemesis/Explosion from special stage.nem"
	even
ArtNem_SpecialBomb:		BINCLUDE	"art/nemesis/Bomb from special stage.nem"
	even
ArtNem_SpecialEmerald:		BINCLUDE	"art/nemesis/Emerald from special stage.nem"
	even
ArtNem_SpecialMessages:		BINCLUDE	"art/nemesis/Special stage messages and icons.nem"
	even
ArtNem_SpecialSonicAndTails:	BINCLUDE	"art/nemesis/Sonic and Tails animation frames in special stage.nem" ; [fixBugs] In this file, Tails' arms are tan instead of orange.
	even
ArtNem_SpecialTailsText:	BINCLUDE	"art/nemesis/Tails text patterns from special stage.nem"
	even
MiscKoz_SpecialPerspective:	BINCLUDE	"misc/Special stage object perspective data.kos"
	even
MiscNem_SpecialLevelLayout:	BINCLUDE	"misc/Special stage level layouts.nem"
	even
MiscKoz_SpecialObjectLocations:	BINCLUDE	"misc/Special stage object location lists.kos"
	even

;--------------------------------------------------------------------------------------
; Filler (free space) (unnecessary; could be replaced with "even")
;--------------------------------------------------------------------------------------
	align $100




;--------------------------------------------------------------------------------------
; Offset index of ring locations
;  The first commented number on each line is an array index; the second is the
;  associated zone.
;--------------------------------------------------------------------------------------
Off_Rings: zoneOrderedOffsetTable 2,2
	; EHZ
	zoneOffsetTableEntry.w  Rings_EHZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_EHZ_2	; Act 2
	; Zone 1
	zoneOffsetTableEntry.w  Rings_Lev1_1	; Act 1
	zoneOffsetTableEntry.w  Rings_Lev1_2	; Act 2
	; WZ
	zoneOffsetTableEntry.w  Rings_WZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_WZ_2	; Act 2
	; Zone 3
	zoneOffsetTableEntry.w  Rings_Lev3_1	; Act 1
	zoneOffsetTableEntry.w  Rings_Lev3_2	; Act 2
	; MTZ
	zoneOffsetTableEntry.w  Rings_MTZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_MTZ_2	; Act 2
	; MTZ
	zoneOffsetTableEntry.w  Rings_MTZ_3	; Act 3
	zoneOffsetTableEntry.w  Rings_MTZ_4	; Act 4
	; WFZ
	zoneOffsetTableEntry.w  Rings_WFZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_WFZ_2	; Act 2
	; HTZ
	zoneOffsetTableEntry.w  Rings_HTZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_HTZ_2	; Act 2
	; HPZ
	zoneOffsetTableEntry.w  Rings_HPZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_HPZ_2	; Act 2
	; Zone 9
	zoneOffsetTableEntry.w  Rings_Lev9_1	; Act 1
	zoneOffsetTableEntry.w  Rings_Lev9_2	; Act 2
	; OOZ
	zoneOffsetTableEntry.w  Rings_OOZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_OOZ_2	; Act 2
	; MCZ
	zoneOffsetTableEntry.w  Rings_MCZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_MCZ_2	; Act 2
	; CNZ
	zoneOffsetTableEntry.w  Rings_CNZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_CNZ_2	; Act 2
	; CPZ
	zoneOffsetTableEntry.w  Rings_CPZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_CPZ_2	; Act 2
	; DEZ
	zoneOffsetTableEntry.w  Rings_DEZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_DEZ_2	; Act 2
	; ARZ
	zoneOffsetTableEntry.w  Rings_ARZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_ARZ_2	; Act 2
	; SCZ
	zoneOffsetTableEntry.w  Rings_SCZ_1	; Act 1
	zoneOffsetTableEntry.w  Rings_SCZ_2	; Act 2
    zoneTableEnd

Rings_EHZ_1:	BINCLUDE	"level/rings/EHZ_1.bin"
Rings_EHZ_2:	BINCLUDE	"level/rings/EHZ_2.bin"
Rings_Lev1_1:	BINCLUDE	"level/rings/01_1.bin"
Rings_Lev1_2:	BINCLUDE	"level/rings/01_2.bin"
Rings_WZ_1:	BINCLUDE	"level/rings/WZ_1.bin"
Rings_WZ_2:	BINCLUDE	"level/rings/WZ_2.bin"
Rings_Lev3_1:	BINCLUDE	"level/rings/03_1.bin"
Rings_Lev3_2:	BINCLUDE	"level/rings/03_2.bin"
Rings_MTZ_1:	BINCLUDE	"level/rings/MTZ_1.bin"
Rings_MTZ_2:	BINCLUDE	"level/rings/MTZ_2.bin"
Rings_MTZ_3:	BINCLUDE	"level/rings/MTZ_3.bin"
Rings_MTZ_4:	BINCLUDE	"level/rings/MTZ_4.bin"
Rings_HTZ_1:	BINCLUDE	"level/rings/HTZ_1.bin"
Rings_HTZ_2:	BINCLUDE	"level/rings/HTZ_2.bin"
Rings_HPZ_1:	BINCLUDE	"level/rings/HPZ_1.bin"
Rings_HPZ_2:	BINCLUDE	"level/rings/HPZ_2.bin"
Rings_Lev9_1:	BINCLUDE	"level/rings/09_1.bin"
Rings_Lev9_2:	BINCLUDE	"level/rings/09_2.bin"
Rings_OOZ_1:	BINCLUDE	"level/rings/OOZ_1.bin"
Rings_OOZ_2:	BINCLUDE	"level/rings/OOZ_2.bin"
Rings_MCZ_1:	BINCLUDE	"level/rings/MCZ_1.bin"
Rings_MCZ_2:	BINCLUDE	"level/rings/MCZ_2.bin"
Rings_CNZ_1:	BINCLUDE	"level/rings/CNZ_1.bin"
Rings_CNZ_2:	BINCLUDE	"level/rings/CNZ_2.bin"
Rings_CPZ_1:	BINCLUDE	"level/rings/CPZ_1.bin"
Rings_CPZ_2:	BINCLUDE	"level/rings/CPZ_2.bin"
Rings_DEZ_1:	BINCLUDE	"level/rings/DEZ_1.bin"
Rings_DEZ_2:	BINCLUDE	"level/rings/DEZ_2.bin"
Rings_WFZ_1:	BINCLUDE	"level/rings/WFZ_1.bin"
Rings_WFZ_2:	BINCLUDE	"level/rings/WFZ_2.bin"
Rings_ARZ_1:	BINCLUDE	"level/rings/ARZ_1.bin"
Rings_ARZ_2:	BINCLUDE	"level/rings/ARZ_2.bin"
Rings_SCZ_1:	BINCLUDE	"level/rings/SCZ_1.bin"
Rings_SCZ_2:	BINCLUDE	"level/rings/SCZ_2.bin"

; --------------------------------------------------------------------------------------
; Filler (free space) (unnecessary; could be replaced with "even")
; --------------------------------------------------------------------------------------
	align $200

; --------------------------------------------------------------------------------------
; Offset index of object locations
; --------------------------------------------------------------------------------------
Off_Objects: zoneOrderedOffsetTable 2,2
	; EHZ
	zoneOffsetTableEntry.w  Objects_EHZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_EHZ_2	; Act 2
	; Zone 1
	zoneOffsetTableEntry.w  Objects_Null	; Act 1
	zoneOffsetTableEntry.w  Objects_Null	; Act 2
	; WZ
	zoneOffsetTableEntry.w  Objects_Null	; Act 1
	zoneOffsetTableEntry.w  Objects_Null	; Act 2
	; Zone 3
	zoneOffsetTableEntry.w  Objects_Null	; Act 1
	zoneOffsetTableEntry.w  Objects_Null	; Act 2
	; MTZ
	zoneOffsetTableEntry.w  Objects_MTZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_MTZ_2	; Act 2
	; MTZ
	zoneOffsetTableEntry.w  Objects_MTZ_3	; Act 3
	zoneOffsetTableEntry.w  Objects_MTZ_3	; Act 4
	; WFZ
	zoneOffsetTableEntry.w  Objects_WFZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_WFZ_2	; Act 2
	; HTZ
	zoneOffsetTableEntry.w  Objects_HTZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_HTZ_2	; Act 2
	; HPZ
	zoneOffsetTableEntry.w  Objects_HPZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_HPZ_2	; Act 2
	; Zone 9
	zoneOffsetTableEntry.w  Objects_Null	; Act 1
	zoneOffsetTableEntry.w  Objects_Null	; Act 2
	; OOZ
	zoneOffsetTableEntry.w  Objects_OOZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_OOZ_2	; Act 2
	; MCZ
	zoneOffsetTableEntry.w  Objects_MCZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_MCZ_2	; Act 2
	; CNZ
	zoneOffsetTableEntry.w  Objects_CNZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_CNZ_2	; Act 2
	; CPZ
	zoneOffsetTableEntry.w  Objects_CPZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_CPZ_2	; Act 2
	; DEZ
	zoneOffsetTableEntry.w  Objects_DEZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_DEZ_2	; Act 2
	; ARZ
	zoneOffsetTableEntry.w  Objects_ARZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_ARZ_2	; Act 2
	; SCZ
	zoneOffsetTableEntry.w  Objects_SCZ_1	; Act 1
	zoneOffsetTableEntry.w  Objects_SCZ_2	; Act 2
    zoneTableEnd

	; These things act as boundaries for the object layout parser, so it doesn't read past the end/beginning of the file
	ObjectLayoutBoundary
Objects_EHZ_1:	BINCLUDE	"level/objects/EHZ_1.bin"
	ObjectLayoutBoundary

    if gameRevision=0
; A collision switcher was improperly placed
Objects_EHZ_2:	BINCLUDE	"level/objects/EHZ_2 (REV00).bin"
    else
Objects_EHZ_2:	BINCLUDE	"level/objects/EHZ_2.bin"
    endif

	ObjectLayoutBoundary
Objects_MTZ_1:	BINCLUDE	"level/objects/MTZ_1.bin"
	ObjectLayoutBoundary
Objects_MTZ_2:	BINCLUDE	"level/objects/MTZ_2.bin"
	ObjectLayoutBoundary
Objects_MTZ_3:	BINCLUDE	"level/objects/MTZ_3.bin"
	ObjectLayoutBoundary

    if gameRevision=0
; The lampposts were bugged: their 'remember state' flags weren't set
Objects_WFZ_1:	BINCLUDE	"level/objects/WFZ_1 (REV00).bin"
    else
Objects_WFZ_1:	BINCLUDE	"level/objects/WFZ_1.bin"
    endif

	ObjectLayoutBoundary
Objects_WFZ_2:	BINCLUDE	"level/objects/WFZ_2.bin"
	ObjectLayoutBoundary
Objects_HTZ_1:	BINCLUDE	"level/objects/HTZ_1.bin"
	ObjectLayoutBoundary
Objects_HTZ_2:	BINCLUDE	"level/objects/HTZ_2.bin"
	ObjectLayoutBoundary
Objects_HPZ_1:	BINCLUDE	"level/objects/HPZ_1.bin"
	ObjectLayoutBoundary
Objects_HPZ_2:	BINCLUDE	"level/objects/HPZ_2.bin"
	ObjectLayoutBoundary
	; Oddly, there's a gap for another layout here
	ObjectLayoutBoundary
Objects_OOZ_1:	BINCLUDE	"level/objects/OOZ_1.bin"
	ObjectLayoutBoundary
Objects_OOZ_2:	BINCLUDE	"level/objects/OOZ_2.bin"
	ObjectLayoutBoundary
Objects_MCZ_1:	BINCLUDE	"level/objects/MCZ_1.bin"
	ObjectLayoutBoundary
Objects_MCZ_2:	BINCLUDE	"level/objects/MCZ_2.bin"
	ObjectLayoutBoundary

    if gameRevision=0
; The signposts are too low, causing them to poke out the bottom of the ground
Objects_CNZ_1:	BINCLUDE	"level/objects/CNZ_1 (REV00).bin"
	ObjectLayoutBoundary
Objects_CNZ_2:	BINCLUDE	"level/objects/CNZ_2 (REV00).bin"
    else
Objects_CNZ_1:	BINCLUDE	"level/objects/CNZ_1.bin"
	ObjectLayoutBoundary
Objects_CNZ_2:	BINCLUDE	"level/objects/CNZ_2.bin"
    endif

	ObjectLayoutBoundary
Objects_CPZ_1:	BINCLUDE	"level/objects/CPZ_1.bin"
	ObjectLayoutBoundary
Objects_CPZ_2:	BINCLUDE	"level/objects/CPZ_2.bin"
	ObjectLayoutBoundary
Objects_DEZ_1:	BINCLUDE	"level/objects/DEZ_1.bin"
	ObjectLayoutBoundary
Objects_DEZ_2:	BINCLUDE	"level/objects/DEZ_2.bin"
	ObjectLayoutBoundary
Objects_ARZ_1:	BINCLUDE	"level/objects/ARZ_1.bin"
	ObjectLayoutBoundary
Objects_ARZ_2:	BINCLUDE	"level/objects/ARZ_2.bin"
	ObjectLayoutBoundary
Objects_SCZ_1:	BINCLUDE	"level/objects/SCZ_1.bin"
	ObjectLayoutBoundary
Objects_SCZ_2:	BINCLUDE	"level/objects/SCZ_2.bin"
	ObjectLayoutBoundary
Objects_Null:
	ObjectLayoutBoundary
	; Another strange space for a layout
	ObjectLayoutBoundary
	; And another
	ObjectLayoutBoundary
	; And another
	ObjectLayoutBoundary

; --------------------------------------------------------------------------------------
; Filler (free space) (unnecessary; could be replaced with "even")
; --------------------------------------------------------------------------------------
	align $1000




; ---------------------------------------------------------------------------
; Subroutine to load the sound driver
; ---------------------------------------------------------------------------
; sub_EC000:
SoundDriverLoad:
	move	sr,-(sp)
	movem.l	d0-a6,-(sp)
	move	#$2700,sr
	lea	(Z80_Bus_Request).l,a3
	lea	(Z80_Reset).l,a2
	moveq	#0,d2
	move.w	#$100,d1
	move.w	d1,(a3)	; get Z80 bus
	move.w	d1,(a2)	; release Z80 reset (was held high by console on startup)
-	btst	d2,(a3)
	bne.s	-	; wait until the 68000 has the bus
	jsr	DecompressSoundDriver(pc)
	btst	#0,(VDP_control_port+1).l	; check video mode
	sne	(Z80_RAM+zPalModeByte).l	; set if PAL
	move.w	d2,(a2)	; hold Z80 reset
	move.w	d2,(a3)	; release Z80 bus
	moveq	#signextendB($E6),d0
-	dbf	d0,-	; wait for 2,314 cycles
	move.w	d1,(a2)	; release Z80 reset
	movem.l	(sp)+,d0-a6
	move	(sp)+,sr
	rts

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||
; Handles the decompression of the sound driver (Saxman compression, an LZSS variant)
; https://segaretro.org/Saxman_compression

; a4 == start of decompressed data (used for dictionary match offsets)
; a5 == current address of end of decompressed data
; a6 == current address in compressed sound driver
; d3 == length of match minus 1
; d4 == offset into decompressed data of dictionary match
; d5 == number of bytes decompressed so far
; d6 == descriptor field
; d7 == bytes left to decompress

; Interestingly, this appears to be a direct translation of the Z80 version in the sound driver
; (or maybe the Z80 version is a direct translation of this...)

; loc_EC04A:
DecompressSoundDriver:
	lea	Snd_Driver(pc),a6
; WARNING: the build script needs editing if you rename this label
movewZ80CompSize:	move.w	#Snd_Driver_End-Snd_Driver,d7 ; patched (by build.lua) after compression since the exact size can't be known beforehand
	moveq	#0,d6	; The decompressor knows it's run out of descriptor bits when it starts reading 0's in bit 8
	lea	(Z80_RAM).l,a5
	moveq	#0,d5
	lea	(Z80_RAM).l,a4
; loc_EC062:
SaxDec_Loop:
	lsr.w	#1,d6	; Next descriptor bit
	btst	#8,d6	; Check if we've run out of bits
	bne.s	+	; (lsr 'shifts in' 0's)
	jsr	SaxDec_GetByte(pc)
	move.b	d0,d6
	ori.w	#$FF00,d6	; These set bits will disappear from the high byte as the register is shifted
+
	btst	#0,d6
	beq.s	SaxDec_ReadCompressed

; SaxDec_ReadUncompressed:
	jsr	SaxDec_GetByte(pc)
	move.b	d0,(a5)+
	addq.w	#1,d5
	bra.w	SaxDec_Loop
; ---------------------------------------------------------------------------
; loc_EC086:
SaxDec_ReadCompressed:
	jsr	SaxDec_GetByte(pc)
	moveq	#0,d4
	move.b	d0,d4
	jsr	SaxDec_GetByte(pc)
	move.b	d0,d3
	andi.w	#$F,d3
	addq.w	#2,d3	; d3 is the length of the match minus 1
	andi.w	#$F0,d0
	lsl.w	#4,d0
	add.w	d0,d4
	addi.w	#$12,d4
	andi.w	#$FFF,d4	; d4 is the offset into the current $1000-byte window
	; This part is a little tricky. You see, d4 currently contains the low three nibbles of an offset into the decompressed data,
	; where the dictionary match lies. The way the high nibble is decided is first by taking it from d5 - the offset of the end
	; of the decompressed data so far. Then, we see if the resulting offset in d4 is somehow higher than d5.
	; If it is, then it's invalid... *unless* you subtract $1000 from it, in which case it refers to data in the previous $1000 block of bytes.
	; This is all just a really gimmicky way of having an offset with a range of $1000 bytes from the end of the decompressed data.
	; If, however, we cannot subtract $1000 because that would put the pointer before the start of the decompressed data, then
	; this is actually a 'zero-fill' match, which encodes a series of zeroes.
	move.w	d5,d0
	andi.w	#$F000,d0
	add.w	d0,d4
	cmp.w	d4,d5
	bhs.s	SaxDec_IsDictionaryReference
	subi.w	#$1000,d4
	bcc.s	SaxDec_IsDictionaryReference

; SaxDec_IsSequenceOfZeroes:
	add.w	d3,d5
	addq.w	#1,d5

-	move.b	#0,(a5)+
	dbf	d3,-

	bra.w	SaxDec_Loop
; ---------------------------------------------------------------------------
; loc_EC0CC:
SaxDec_IsDictionaryReference:
	add.w	d3,d5
	addq.w	#1,d5

-	move.b	(a4,d4.w),(a5)+
	addq.w	#1,d4
	dbf	d3,-

	bra.w	SaxDec_Loop
; End of function DecompressSoundDriver


; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; sub_EC0DE:
SaxDec_GetByte:
	move.b	(a6)+,d0
	subq.w	#1,d7	; Decrement remaining number of bytes
	bne.s	+
	addq.w	#4,sp	; Exit the decompressor by meddling with the stack
+
	rts
; End of function SaxDec_GetByte

; ===========================================================================
; ---------------------------------------------------------------------------
; S2 sound driver (Sound driver compression (slightly modified Saxman))
; ---------------------------------------------------------------------------
; loc_EC0E8:
Snd_Driver:
	save
	include "s2.sounddriver.asm" ; CPU Z80
	restore
	padding off
	!org (Snd_Driver+Size_of_Snd_driver_guess) ; don't worry; I know what I'm doing


; loc_ED04C:
Snd_Driver_End:




; ---------------------------------------------------------------------------
; Filler (free space)
; ---------------------------------------------------------------------------
	; the DAC data has to line up with the end of the bank.

	; actually it only has to fit within one bank, but we'll line it up to the end anyway
	; because the padding gives the sound driver some room to grow
	cnop -Size_of_DAC_samples, $8000

; ---------------------------------------------------------------------------
; DAC samples
; ---------------------------------------------------------------------------
; loc_ED100:
SndDAC_Start:

SndDAC_Kick:
	BINCLUDE	"sound/DAC/Kick.dpcm"
SndDAC_Kick_End

SndDAC_Snare:
	BINCLUDE	"sound/DAC/Snare.dpcm"
SndDAC_Snare_End

SndDAC_Timpani:
	BINCLUDE	"sound/DAC/Timpani.dpcm"
SndDAC_Timpani_End

SndDAC_Tom:
	BINCLUDE	"sound/DAC/Tom.dpcm"
SndDAC_Tom_End

SndDAC_Clap:
	BINCLUDE	"sound/DAC/Clap.dpcm"
SndDAC_Clap_End

SndDAC_Scratch:
	BINCLUDE	"sound/DAC/Scratch.dpcm"
SndDAC_Scratch_End

SndDAC_Bongo:
	BINCLUDE	"sound/DAC/Bongo.dpcm"
SndDAC_Bongo_End

SndDAC_End

	if SndDAC_End - SndDAC_Start > $8000
		fatal "DAC samples must fit within $8000 bytes, but you have $\{SndDAC_End-SndDAC_Start } bytes of DAC samples."
	endif
	if SndDAC_End - SndDAC_Start > Size_of_DAC_samples
		fatal "Size_of_DAC_samples = $\{Size_of_DAC_samples}, but you have $\{SndDAC_End-SndDAC_Start} bytes of DAC samples."
	endif

; ---------------------------------------------------------------------------
; Music pointers
; ---------------------------------------------------------------------------
; loc_F0000:
MusicPoint1:	startBank
MusPtr_Continue:	rom_ptr_z80	Mus_Continue


Mus_Continue:   BINCLUDE	"sound/music/compressed/9C - Continue.sax"

	finishBank

	align $20

; --------------------------------------------------------------------------------------
; EHZ/HTZ Assets
; --------------------------------------------------------------------------------------
ArtNem_HtzFireball1:		BINCLUDE	"art/nemesis/Fireball 1.nem"
	even
ArtNem_Waterfall:		BINCLUDE	"art/nemesis/Waterfall tiles.nem"
	even
ArtNem_HtzFireball2:		BINCLUDE	"art/nemesis/Fireball 2.nem"
	even
ArtNem_EHZ_Bridge:		BINCLUDE	"art/nemesis/EHZ bridge.nem"
	even
ArtNem_HtzZipline:		BINCLUDE	"art/nemesis/HTZ zip-line platform.nem"
	even
ArtNem_HtzValveBarrier:		BINCLUDE	"art/nemesis/One way barrier from HTZ.nem"
	even
ArtNem_HtzSeeSaw:		BINCLUDE	"art/nemesis/See-saw in HTZ.nem"
	even
				BINCLUDE	"art/nemesis/Fireball 3.nem" ; Unused
	even
ArtNem_HtzRock:			BINCLUDE	"art/nemesis/Rock from HTZ.nem"
	even
ArtNem_Sol:			BINCLUDE	"art/nemesis/Sol badnik from HTZ.nem" ; Not grouped with the other badniks for some reason...
	even

; --------------------------------------------------------------------------------------
; MTZ Assets
; --------------------------------------------------------------------------------------
ArtNem_MtzWheel:		BINCLUDE	"art/nemesis/Large spinning wheel from MTZ.nem"
	even
ArtNem_MtzWheelIndent:		BINCLUDE	"art/nemesis/Large spinning wheel from MTZ - indent.nem"
	even
ArtNem_MtzSpikeBlock:		BINCLUDE	"art/nemesis/MTZ spike block.nem"
	even
ArtNem_MtzSteam:		BINCLUDE	"art/nemesis/Steam from MTZ.nem"
	even
ArtNem_MtzSpike:		BINCLUDE	"art/nemesis/Spike from MTZ.nem"
	even
ArtNem_MtzAsstBlocks:		BINCLUDE	"art/nemesis/Similarly shaded blocks from MTZ.nem"
	even
ArtNem_MtzLavaBubble:		BINCLUDE	"art/nemesis/Lava bubble from MTZ.nem"
	even
ArtNem_LavaCup:			BINCLUDE	"art/nemesis/Lava cup from MTZ.nem"
	even
ArtNem_BoltEnd_Rope:		BINCLUDE	"art/nemesis/Bolt end and rope from MTZ.nem"
	even	
ArtNem_MtzCog:			BINCLUDE	"art/nemesis/Small cog from MTZ.nem"
	even
ArtNem_MtzSpinTubeFlash:	BINCLUDE	"art/nemesis/Spin tube flash from MTZ.nem"
	even

; --------------------------------------------------------------------------------------
; MCZ Assets
; --------------------------------------------------------------------------------------
ArtNem_Crate:			BINCLUDE	"art/nemesis/Large wooden box from MCZ.nem"
	even
ArtNem_MCZCollapsePlat:		BINCLUDE	"art/nemesis/Collapsing platform from MCZ.nem"
	even
ArtNem_VineSwitch:		BINCLUDE	"art/nemesis/Pull switch from MCZ.nem"
	even
ArtNem_VinePulley:		BINCLUDE	"art/nemesis/Vine that lowers from MCZ.nem"
	even
ArtNem_MCZGateLog:		BINCLUDE	"art/nemesis/Drawbridge logs from MCZ.nem"
	even

; ----------------------------------------------------------------------------------
; Filler (free space)
; ----------------------------------------------------------------------------------
	; the PCM data has to line up with the end of the bank.
	cnop -Size_of_SEGA_sound, $8000

; -------------------------------------------------------------------------------
; Sega Intro Sound
; 8-bit unsigned raw audio at 16Khz
; -------------------------------------------------------------------------------
; loc_F1E8C:
Snd_Sega:	BINCLUDE	"sound/PCM/SEGA.pcm"
Snd_Sega_End:

	if Snd_Sega_End - Snd_Sega > $8000
		fatal "Sega sound must fit within $8000 bytes, but you have a $\{Snd_Sega_End-Snd_Sega} byte Sega sound."
	endif
	if Snd_Sega_End - Snd_Sega > Size_of_SEGA_sound
		fatal "Size_of_SEGA_sound = $\{Size_of_SEGA_sound}, but you have a $\{Snd_Sega_End-Snd_Sega} byte Sega sound."
	endif

; ------------------------------------------------------------------------------
; Music pointers
; ------------------------------------------------------------------------------
; loc_F8000:
MusicPoint2:	startBank
MusPtr_CNZ_2P:		rom_ptr_z80	Mus_CNZ_2P
MusPtr_EHZ:		rom_ptr_z80	Mus_EHZ
MusPtr_MTZ:		rom_ptr_z80	Mus_MTZ
MusPtr_CNZ:		rom_ptr_z80	Mus_CNZ
MusPtr_MCZ:		rom_ptr_z80	Mus_MCZ
MusPtr_MCZ_2P:		rom_ptr_z80	Mus_MCZ_2P
MusPtr_ARZ:		rom_ptr_z80	Mus_ARZ
MusPtr_DEZ:		rom_ptr_z80	Mus_DEZ
MusPtr_SpecStage:	rom_ptr_z80	Mus_SpecStage
MusPtr_Options:		rom_ptr_z80	Mus_Options
MusPtr_Ending:		rom_ptr_z80	Mus_Ending
MusPtr_EndBoss:		rom_ptr_z80	Mus_EndBoss
MusPtr_CPZ:		rom_ptr_z80	Mus_CPZ
MusPtr_Boss:		rom_ptr_z80	Mus_Boss
MusPtr_SCZ:		rom_ptr_z80	Mus_SCZ
MusPtr_OOZ:		rom_ptr_z80	Mus_OOZ
MusPtr_WFZ:		rom_ptr_z80	Mus_WFZ
MusPtr_EHZ_2P:		rom_ptr_z80	Mus_EHZ_2P
MusPtr_2PResult:	rom_ptr_z80	Mus_2PResult
MusPtr_SuperSonic:	rom_ptr_z80	Mus_SuperSonic
MusPtr_HTZ:		rom_ptr_z80	Mus_HTZ
MusPtr_ExtraLife:	rom_ptr_z80	Mus_ExtraLife
MusPtr_Title:		rom_ptr_z80	Mus_Title
MusPtr_EndLevel:	rom_ptr_z80	Mus_EndLevel
MusPtr_GameOver:	rom_ptr_z80	Mus_GameOver
MusPtr_Invincible:	rom_ptr_z80	Mus_Invincible
MusPtr_Emerald:		rom_ptr_z80	Mus_Emerald
MusPtr_HPZ:		rom_ptr_z80	Mus_HPZ
MusPtr_Drowning:	rom_ptr_z80	Mus_Drowning
MusPtr_Credits:		rom_ptr_z80	Mus_Credits

; loc_F803C:
Mus_HPZ:	BINCLUDE	"sound/music/compressed/90 - HPZ.sax"
Mus_Drowning:	BINCLUDE	"sound/music/compressed/9F - Drowning.sax"
Mus_Invincible:	BINCLUDE	"sound/music/compressed/97 - Invincible.sax"
Mus_CNZ_2P:	BINCLUDE	"sound/music/compressed/88 - CNZ 2P.sax"
Mus_EHZ:	BINCLUDE	"sound/music/compressed/82 - EHZ.sax"
Mus_MTZ:	BINCLUDE	"sound/music/compressed/85 - MTZ.sax"
Mus_CNZ:	BINCLUDE	"sound/music/compressed/89 - CNZ.sax"
Mus_MCZ:	BINCLUDE	"sound/music/compressed/8B - MCZ.sax"
Mus_MCZ_2P:	BINCLUDE	"sound/music/compressed/83 - MCZ 2P.sax"
Mus_ARZ:	BINCLUDE	"sound/music/compressed/87 - ARZ.sax"
Mus_DEZ:	BINCLUDE	"sound/music/compressed/8A - DEZ.sax"
Mus_SpecStage:	BINCLUDE	"sound/music/compressed/92 - Special Stage.sax"
Mus_Options:	BINCLUDE	"sound/music/compressed/91 - Options.sax"
Mus_Ending:	BINCLUDE	"sound/music/compressed/95 - Ending.sax"
Mus_EndBoss:	BINCLUDE	"sound/music/compressed/94 - Final Boss.sax"
Mus_CPZ:	BINCLUDE	"sound/music/compressed/8E - CPZ.sax"
Mus_Boss:	BINCLUDE	"sound/music/compressed/93 - Boss.sax"
Mus_SCZ:	BINCLUDE	"sound/music/compressed/8D - SCZ.sax"
Mus_OOZ:	BINCLUDE	"sound/music/compressed/84 - OOZ.sax"
Mus_WFZ:	BINCLUDE	"sound/music/compressed/8F - WFZ.sax"
Mus_EHZ_2P:	BINCLUDE	"sound/music/compressed/8C - EHZ 2P.sax"
Mus_2PResult:	BINCLUDE	"sound/music/compressed/81 - 2 Player Menu.sax"
Mus_SuperSonic:	BINCLUDE	"sound/music/compressed/96 - Super Sonic.sax"
Mus_HTZ:	BINCLUDE	"sound/music/compressed/86 - HTZ.sax"
Mus_Title:	BINCLUDE	"sound/music/compressed/99 - Title Screen.sax"
Mus_EndLevel:	BINCLUDE	"sound/music/compressed/9A - End of Act.sax"

Mus_ExtraLife:	include		"sound/music/98 - Extra Life.asm"
Mus_GameOver:	include		"sound/music/9B - Game Over.asm"
Mus_Emerald:	include		"sound/music/9D - Got Emerald.asm"
Mus_Credits:	include		"sound/music/9E - Credits.asm"

; ------------------------------------------------------------------------------------------
; Sound effect pointers
; ------------------------------------------------------------------------------------------
; WARNING the sound driver treats certain sounds specially
; going by the ID of the sound.
; SndID_Ring, SndID_RingLeft, SndID_Gloop, SndID_SpindashRev
; are referenced by the sound driver directly.
; If needed you can change this in s2.sounddriver.asm


; NOTE: the exact order of this list determines the priority of each sound, since it determines the sound's SndID.
;       a sound can get dropped if a higher-priority sound is already playing.
;	see zSFXPriority for the priority allocation itself.
; loc_FEE91: SoundPoint:
SoundIndex:
SndPtr_Jump:		rom_ptr_z80	Sound20	; jumping sound
SndPtr_Checkpoint:	rom_ptr_z80	Sound21	; checkpoint ding-dong sound
SndPtr_SpikeSwitch:	rom_ptr_z80	Sound22	; spike switch sound
SndPtr_Hurt:		rom_ptr_z80	Sound23	; hurt sound
SndPtr_Skidding:	rom_ptr_z80	Sound24	; skidding sound
SndPtr_MissileDissolve:	rom_ptr_z80	Sound25	; missile dissolve sound from Sonic 1 (unused)
SndPtr_HurtBySpikes:	rom_ptr_z80	Sound26	; spiky impalement sound
SndPtr_Sparkle:		rom_ptr_z80	Sound27	; sparkling sound
SndPtr_Beep:		rom_ptr_z80	Sound28	; short beep
SndPtr_Bwoop:		rom_ptr_z80	Sound29	; bwoop (unused)
SndPtr_Splash:		rom_ptr_z80	Sound2A	; splash sound
SndPtr_Swish:		rom_ptr_z80	Sound2B	; swish
SndPtr_BossHit:		rom_ptr_z80	Sound2C	; boss hit
SndPtr_InhalingBubble:	rom_ptr_z80	Sound2D	; inhaling a bubble
SndPtr_ArrowFiring:
SndPtr_LavaBall:	rom_ptr_z80	Sound2E	; arrow firing
SndPtr_Shield:		rom_ptr_z80	Sound2F	; shield sound
SndPtr_LaserBeam:	rom_ptr_z80	Sound30	; laser beam
SndPtr_Zap:		rom_ptr_z80	Sound31	; zap (unused)
SndPtr_Drown:		rom_ptr_z80	Sound32	; drownage
SndPtr_FireBurn:	rom_ptr_z80	Sound33	; fire + burn
SndPtr_Bumper:		rom_ptr_z80	Sound34	; bumper bing
SndPtr_Ring:
SndPtr_RingRight:	rom_ptr_z80	Sound35	; ring sound
SndPtr_SpikesMove:	rom_ptr_z80	Sound36
SndPtr_Rumbling:	rom_ptr_z80	Sound37	; rumbling
			rom_ptr_z80	Sound38	; (unused)
SndPtr_Smash:		rom_ptr_z80	Sound39	; smash/breaking
			rom_ptr_z80	Sound3A	; nondescript ding (unused)
SndPtr_DoorSlam:	rom_ptr_z80	Sound3B	; door slamming shut
SndPtr_SpindashRelease:	rom_ptr_z80	Sound3C	; spindash unleashed
SndPtr_Hammer:		rom_ptr_z80	Sound3D	; slide-thunk
SndPtr_Roll:		rom_ptr_z80	Sound3E	; rolling sound
SndPtr_ContinueJingle:	rom_ptr_z80	Sound3F	; got continue
SndPtr_CasinoBonus:	rom_ptr_z80	Sound40	; short bonus ding
SndPtr_Explosion:	rom_ptr_z80	Sound41	; badnik bust
SndPtr_WaterWarning:	rom_ptr_z80	Sound42	; warning ding-ding
SndPtr_EnterGiantRing:	rom_ptr_z80	Sound43	; special stage ring flash (mostly unused)
SndPtr_BossExplosion:	rom_ptr_z80	Sound44	; thunk
SndPtr_TallyEnd:	rom_ptr_z80	Sound45	; cha-ching
SndPtr_RingSpill:	rom_ptr_z80	Sound46	; losing rings
			rom_ptr_z80	Sound47	; chain pull chink-chink (unused)
SndPtr_Flamethrower:	rom_ptr_z80	Sound48	; flamethrower
SndPtr_Bonus:		rom_ptr_z80	Sound49	; bonus pwoieeew (mostly unused)
SndPtr_SpecStageEntry:	rom_ptr_z80	Sound4A	; special stage entry
SndPtr_SlowSmash:	rom_ptr_z80	Sound4B	; slower smash/crumble
SndPtr_Spring:		rom_ptr_z80	Sound4C	; spring boing
SndPtr_Blip:		rom_ptr_z80	Sound4D	; selection blip
SndPtr_RingLeft:	rom_ptr_z80	Sound4E	; another ring sound (only plays in the left speaker?)
SndPtr_Signpost:	rom_ptr_z80	Sound4F	; signpost spin sound
SndPtr_CNZBossZap:	rom_ptr_z80	Sound50	; mosquito zapper
			rom_ptr_z80	Sound51	; (unused)
			rom_ptr_z80	Sound52	; (unused)
SndPtr_Signpost2P:	rom_ptr_z80	Sound53
SndPtr_OOZLidPop:	rom_ptr_z80	Sound54	; OOZ lid pop sound
SndPtr_SlidingSpike:	rom_ptr_z80	Sound55
SndPtr_CNZElevator:	rom_ptr_z80	Sound56
SndPtr_PlatformKnock:	rom_ptr_z80	Sound57
SndPtr_BonusBumper:	rom_ptr_z80	Sound58	; CNZ bonusy bumper sound
SndPtr_LargeBumper:	rom_ptr_z80	Sound59	; CNZ baaang bumper sound
SndPtr_Gloop:		rom_ptr_z80	Sound5A	; CNZ gloop / water droplet sound
SndPtr_PreArrowFiring:	rom_ptr_z80	Sound5B
SndPtr_Fire:		rom_ptr_z80	Sound5C
SndPtr_ArrowStick:	rom_ptr_z80	Sound5D	; chain clink
SndPtr_Helicopter:
SndPtr_WingFortress:	rom_ptr_z80	Sound5E	; helicopter
SndPtr_SuperTransform:	rom_ptr_z80	Sound5F
SndPtr_SpindashRev:	rom_ptr_z80	Sound60	; spindash charge
SndPtr_Rumbling2:	rom_ptr_z80	Sound61	; rumbling
SndPtr_CNZLaunch:	rom_ptr_z80	Sound62
SndPtr_Flipper:		rom_ptr_z80	Sound63	; CNZ blooing bumper
SndPtr_HTZLiftClick:	rom_ptr_z80	Sound64	; HTZ track click sound
SndPtr_Leaves:		rom_ptr_z80	Sound65	; kicking up leaves sound
SndPtr_MegaMackDrop:	rom_ptr_z80	Sound66	; leaf splash?
SndPtr_DrawbridgeMove:	rom_ptr_z80	Sound67
SndPtr_QuickDoorSlam:	rom_ptr_z80	Sound68	; door slamming quickly (unused)
SndPtr_DrawbridgeDown:	rom_ptr_z80	Sound69
SndPtr_LaserBurst:	rom_ptr_z80	Sound6A	; robotic laser burst
SndPtr_Scatter:
SndPtr_LaserFloor:	rom_ptr_z80	Sound6B	; scatter
SndPtr_Teleport:	rom_ptr_z80	Sound6C
SndPtr_Error:		rom_ptr_z80	Sound6D	; error sound
SndPtr_MechaSonicBuzz:	rom_ptr_z80	Sound6E	; Silver Sonic buzz saw
SndPtr_LargeLaser:	rom_ptr_z80	Sound6F
SndPtr_OilSlide:	rom_ptr_z80	Sound70
SndPtr__End:

Sound20:	include "sound/sfx/A0 - Jump.asm"
Sound21:	include "sound/sfx/A1 - Checkpoint.asm"
Sound22:	include "sound/sfx/A2 - Spike Switch.asm"
Sound23:	include "sound/sfx/A3 - Hurt.asm"
Sound24:	include "sound/sfx/A4 - Skidding.asm"
Sound25:	include "sound/sfx/A5 - Block Push.asm"
Sound26:	include "sound/sfx/A6 - Hurt by Spikes.asm"
Sound27:	include "sound/sfx/A7 - Sparkle.asm"
Sound28:	include "sound/sfx/A8 - Beep.asm"
Sound29:	include "sound/sfx/A9 - Special Stage Item (Unused).asm"
Sound2A:	include "sound/sfx/AA - Splash.asm"
Sound2B:	include "sound/sfx/AB - Swish.asm"
Sound2C:	include "sound/sfx/AC - Boss Hit.asm"
Sound2D:	include "sound/sfx/AD - Inhaling Bubble.asm"
Sound2E:	include "sound/sfx/AE - Lava Ball.asm"
Sound2F:	include "sound/sfx/AF - Shield.asm"
Sound30:	include "sound/sfx/B0 - Laser Beam.asm"
Sound31:	include "sound/sfx/B1 - Electricity (Unused).asm"
Sound32:	include "sound/sfx/B2 - Drown.asm"
Sound33:	include "sound/sfx/B3 - Fire Burn.asm"
Sound34:	include "sound/sfx/B4 - Bumper.asm"
Sound35:	include "sound/sfx/B5 - Ring.asm"
Sound36:	include "sound/sfx/B6 - Spikes Move.asm"
Sound37:	include "sound/sfx/B7 - Rumbling.asm"
Sound38:	include "sound/sfx/B8 - Unknown (Unused).asm"
Sound39:	include "sound/sfx/B9 - Smash.asm"
Sound3A:	include "sound/sfx/BA - Special Stage Glass (Unused).asm"
Sound3B:	include "sound/sfx/BB - Door Slam.asm"
Sound3C:	include "sound/sfx/BC - Spin Dash Release.asm"
Sound3D:	include "sound/sfx/BD - Hammer.asm"
Sound3E:	include "sound/sfx/BE - Roll.asm"
Sound3F:	include "sound/sfx/BF - Continue Jingle.asm"
Sound40:	include "sound/sfx/C0 - Casino Bonus.asm"
Sound41:	include "sound/sfx/C1 - Explosion.asm"
Sound42:	include "sound/sfx/C2 - Water Warning.asm"
Sound43:	include "sound/sfx/C3 - Enter Giant Ring (Unused).asm"
Sound44:	include "sound/sfx/C4 - Boss Explosion.asm"
Sound45:	include "sound/sfx/C5 - Tally End.asm"
Sound46:	include "sound/sfx/C6 - Ring Spill.asm"
Sound47:	include "sound/sfx/C7 - Chain Rise (Unused).asm"
Sound48:	include "sound/sfx/C8 - Flamethrower.asm"
Sound49:	include "sound/sfx/C9 - Hidden Bonus (Unused).asm"
Sound4A:	include "sound/sfx/CA - Special Stage Entry.asm"
Sound4B:	include "sound/sfx/CB - Slow Smash.asm"
Sound4C:	include "sound/sfx/CC - Spring.asm"
Sound4D:	include "sound/sfx/CD - Switch.asm"
Sound4E:	include "sound/sfx/CE - Ring Left Speaker.asm"
Sound4F:	include "sound/sfx/CF - Signpost.asm"
Sound50:	include "sound/sfx/D0 - CNZ Boss Zap.asm"
Sound51:	include "sound/sfx/D1 - Unknown (Unused).asm"
Sound52:	include "sound/sfx/D2 - Unknown (Unused).asm"
Sound53:	include "sound/sfx/D3 - Signpost 2P.asm"
Sound54:	include "sound/sfx/D4 - OOZ Lid Pop.asm"
Sound55:	include "sound/sfx/D5 - Sliding Spike.asm"
Sound56:	include "sound/sfx/D6 - CNZ Elevator.asm"
Sound57:	include "sound/sfx/D7 - Platform Knock.asm"
Sound58:	include "sound/sfx/D8 - Bonus Bumper.asm"
Sound59:	include "sound/sfx/D9 - Large Bumper.asm"
Sound5A:	include "sound/sfx/DA - Gloop.asm"
Sound5B:	include "sound/sfx/DB - Pre-Arrow Firing.asm"
Sound5C:	include "sound/sfx/DC - Fire.asm"
Sound5D:	include "sound/sfx/DD - Arrow Stick.asm"
Sound5E:	include "sound/sfx/DE - Helicopter.asm"
Sound5F:	include "sound/sfx/DF - Super Transform.asm"
Sound60:	include "sound/sfx/E0 - Spin Dash Rev.asm"
Sound61:	include "sound/sfx/E1 - Rumbling 2.asm"
Sound62:	include "sound/sfx/E2 - CNZ Launch.asm"
Sound63:	include "sound/sfx/E3 - Flipper.asm"
Sound64:	include "sound/sfx/E4 - HTZ Lift Click.asm"
Sound65:	include "sound/sfx/E5 - Leaves.asm"
Sound66:	include "sound/sfx/E6 - Mega Mack Drop.asm"
Sound67:	include "sound/sfx/E7 - Drawbridge Move.asm"
Sound68:	include "sound/sfx/E8 - Quick Door Slam.asm"
Sound69:	include "sound/sfx/E9 - Drawbridge Down.asm"
Sound6A:	include "sound/sfx/EA - Laser Burst.asm"
Sound6B:	include "sound/sfx/EB - Scatter.asm"
Sound6C:	include "sound/sfx/EC - Teleport.asm"
Sound6D:	include "sound/sfx/ED - Error.asm"
Sound6E:	include "sound/sfx/EE - Mecha Sonic Buzz.asm"
Sound6F:	include "sound/sfx/EF - Large Laser.asm"
Sound70:	include "sound/sfx/F0 - Oil Slide.asm"

	finishBank

; end of 'ROM'
	if padToPowerOfTwo && (*)&(*-1)
		cnop	-1,2<<lastbit(*-1)
		dc.b	0
paddingSoFar	:= paddingSoFar+1
	else
		even
	endif
EndOfRom:
	if MOMPASS=2
		; "About" because it will be off by the same amount that Size_of_Snd_driver_guess is incorrect (if you changed it), and because I may have missed a small amount of internal padding somewhere
		message "ROM size is $\{EndOfRom-StartOfRom} bytes (\{(EndOfRom-StartOfRom)/1024.0} KiB). About $\{paddingSoFar} bytes are padding. "
	endif
	; share these symbols externally (WARNING: don't rename, move or remove these labels!)
	shared movewZ80CompSize
	END
