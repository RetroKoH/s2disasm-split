loc_35F76:
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	move.w	word_35F92(pc,d0.w),(Normal_palette_line4+$16).w
	move.w	word_35F92+2(pc,d0.w),(Normal_palette_line4+$18).w
	move.w	word_35F92+4(pc,d0.w),(Normal_palette_line4+$1A).w
	rts
; ===========================================================================
; Special Stage Chaos Emerald palette
word_35F92:	BINCLUDE	"art/palettes/SS Emerald.bin"
; ===========================================================================
; ----------------------------------------------------------------------------
; Object 59 - Emerald from Special Stage
; ----------------------------------------------------------------------------
; Sprite_35FBC:
Obj59:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj59_Index(pc,d0.w),d1
	jmp	Obj59_Index(pc,d1.w)
; ===========================================================================
; off_35FCA:
Obj59_Index:	offsetTable
		offsetTableEntry.w Obj59_Init	; 0
		offsetTableEntry.w loc_36022	; 2
		offsetTableEntry.w loc_3533A	; 4
		offsetTableEntry.w loc_36160	; 6
		offsetTableEntry.w loc_36172	; 8
; ===========================================================================
; loc_35FD4:
Obj59_Init:
	st.b	(SS_NoCheckpointMsg_flag).w
	st.b	(SS_Pause_Only_flag).w
	subi_.w	#1,objoff_2A(a0)
	cmpi.w	#-$3C,objoff_2A(a0)
	beq.s	+
	rts
; ---------------------------------------------------------------------------
+
	moveq	#0,d0
	move.b	(Current_Special_Stage).w,d0
	bsr.s	loc_35F76
	addq.b	#2,routine(a0)
	move.l	#Obj59_MapUnc_3625A,mappings(a0)
	move.w	#make_art_tile(ArtTile_ArtNem_SpecialEmerald,3,0),art_tile(a0)
	move.b	#4,render_flags(a0)
	move.b	#4,priority(a0)
	move.w	#$36,objoff_30(a0)
	move.b	#$40,angle(a0)
	bsr.w	loc_3529C

loc_36022:
	bsr.w	loc_360F0
	bsr.w	loc_3512A
	bsr.w	loc_3603C
	lea	(off_36228).l,a1
	bsr.w	loc_3539E
	bra.w	JmpTo44_DisplaySprite
; ===========================================================================

loc_3603C:
	move.w	d7,-(sp)
	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	moveq	#0,d5
	moveq	#0,d6
	moveq	#0,d7
	movea.l	(SS_CurrentPerspective).w,a1
	adda_.l	#2,a1
	move.w	objoff_30(a0),d0
	subq.w	#1,d0
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	move.b	(a1,d0.w),d2
	move.b	1(a1,d0.w),d3
	move.b	2(a1,d0.w),d4
	move.b	3(a1,d0.w),d5
	move.w	d5,d6
	swap	d5
	move.w	d6,d5
	move.w	d4,d6
	swap	d4
	move.w	d6,d4
	bpl.s	loc_36088
	cmpi.b	#$48,d3
	blo.s	loc_36088
	ext.w	d3

loc_36088:
	move.w	d4,d6
	add.w	d4,d4
	add.w	d6,d4
	lsr.w	#2,d4
	move.w	d5,d6
	add.w	d5,d5
	add.w	d6,d5
	lsr.w	#2,d5
	move.b	angle(a0),d0
	jsrto	CalcSine, JmpTo14_CalcSine
	muls.w	d4,d1
	muls.w	d5,d0
	asr.l	#8,d0
	asr.l	#8,d1
	add.w	d2,d1
	add.w	d3,d0
	move.w	d1,x_pos(a0)
	move.w	d0,y_pos(a0)
	move.b	d1,objoff_3A(a0)
	move.b	d0,objoff_3B(a0)
	swap	d4
	swap	d5
	movea.l	objoff_34(a0),a1 ; a1=object
	move.b	angle(a0),d0
	jsrto	CalcSine, JmpTo14_CalcSine
	move.w	d4,d6
	lsr.w	#2,d6
	add.w	d6,d4
	muls.w	d4,d1
	move.w	d5,d6
	asr.w	#2,d6
	add.w	d6,d5
	muls.w	d5,d0
	asr.l	#8,d0
	asr.l	#8,d1
	add.w	d2,d1
	add.w	d3,d0
	move.w	d1,x_pos(a1)
	move.w	d0,y_pos(a1)
	move.w	(sp)+,d7
	rts
; ===========================================================================

loc_360F0:
	cmpi.b	#3,anim(a0)
	blo.s	return_36140
	tst.b	objoff_3E(a0)
	bne.s	loc_3610C
	move.w	#MusID_FadeOut,d0
	jsr	(PlayMusic).l
	st.b	objoff_3E(a0)

loc_3610C:
	cmpi.b	#6,anim(a0)
	blo.s	return_36140
	move.w	(Ring_count).w,d2
	add.w	(Ring_count_2P).w,d2
	cmp.w	(SS_Ring_Requirement).w,d2
	blt.s	loc_36142
	cmpi.b	#9,anim(a0)
	blo.s	return_36140
	move.w	#$63,objoff_2A(a0)
	move.b	#8,routine(a0)
	move.w	#MusID_Emerald,d0
	jsr	(PlayMusic).l

return_36140:
	rts
; ===========================================================================

loc_36142:
	move.l	#0,(SS_New_Speed_Factor).w
	move.b	#6,routine(a0)
	move.w	#$4F,objoff_2A(a0)
	move.w	#6,d0
	bsr.w	loc_35D6E
	rts
; ===========================================================================

loc_36160:
	subi_.w	#1,objoff_2A(a0)
	bpl.w	JmpTo44_DisplaySprite
	st.b	(SS_Check_Rings_flag).w
	bra.w	SSClearObjs
; ===========================================================================

loc_36172:
	subi_.w	#1,objoff_2A(a0)
	bpl.s	loc_361A4
	moveq	#0,d0
	move.b	(Current_Special_Stage).w,d0
	lea	(Got_Emeralds_array).w,a0
	st.b	(a0,d0.w)
	st.b	(Got_Emerald).w
	addi_.b	#1,(Current_Special_Stage).w
	addi_.b	#1,(Emerald_count).w
	st.b	(SS_Check_Rings_flag).w
	bsr.w	SSClearObjs
	move.l	(sp)+,d0
	rts
; ===========================================================================

loc_361A4:
	addi_.b	#1,objoff_3C(a0)
	moveq	#0,d0
	moveq	#0,d2
	move.b	objoff_3B(a0),d2
	move.b	objoff_3C(a0),d0
	lsr.w	#2,d0
	andi.w	#3,d0
	add.b	byte_361C8(pc,d0.w),d2
	move.w	d2,y_pos(a0)
	bra.w	JmpTo44_DisplaySprite
; ===========================================================================
byte_361C8:
	dc.b $FF
	dc.b   0	; 1
	dc.b   1	; 2
	dc.b   0	; 3
	even
; ===========================================================================
;loc_361CC
SSClearObjs:
	movea.l	#(Object_RAM&$FFFFFF),a1

	move.w	#(Object_RAM_End-Object_RAM)/$10-1,d0
	moveq	#0,d1

loc_361D8:
    rept $10/4
	move.l	d1,(a1)+
    endm
	dbf	d0,loc_361D8
.c := ((Object_RAM_End-Object_RAM)#$10)/4
    if .c
    rept .c
	move.l	d1,(a1)+
    endm
    endif
.c := ((Object_RAM_End-Object_RAM)#$10)&2
    if .c
    rept .c
	move.w	d1,(a1)+
    endm
    endif

    if fixBugs
	clearRAM Sprite_Table,Sprite_Table_End
    else
	; The '+4' shouldn't be here; clearRAM accidentally clears an additional 4 bytes.
	clearRAM Sprite_Table,Sprite_Table_End+4
    endif

	rts
; ===========================================================================
	; unused/dead code ; a0=object
	cmpi.b	#$B,(SSTrack_drawing_index).w
	blo.s	loc_36208
	subi.l	#$4445,objoff_30(a0)
	bra.s	loc_36210
; ---------------------------------------------------------------------------
loc_36208:
	subi.l	#$4444,objoff_30(a0)
loc_36210:
	move.w	objoff_30(a0),d0
	cmpi.w	#$1D,d0
	ble.s	+
	moveq	#$1E,d0
+
	lea_	byte_35180,a1
	move.b	(a1,d0.w),anim(a0)
	rts
	; end of unused code

; ===========================================================================
; animation script for object 59
off_36228:	offsetTable
		offsetTableEntry.w byte_3623C	; 0
		offsetTableEntry.w byte_3623F	; 1
		offsetTableEntry.w byte_36242	; 2
		offsetTableEntry.w byte_36245	; 3
		offsetTableEntry.w byte_36248	; 4
		offsetTableEntry.w byte_3624B	; 5
		offsetTableEntry.w byte_3624E	; 6
		offsetTableEntry.w byte_36251	; 7
		offsetTableEntry.w byte_36254	; 8
		offsetTableEntry.w byte_36257	; 9
byte_3623C:
	dc.b  $B,  0,$FF
	rev02even
byte_3623F:
	dc.b  $B,  1,$FF
	rev02even
byte_36242:
	dc.b  $B,  2,$FF
	rev02even
byte_36245:
	dc.b  $B,  3,$FF
	rev02even
byte_36248:
	dc.b  $B,  4,$FF
	rev02even
byte_3624B:
	dc.b  $B,  5,$FF
	rev02even
byte_3624E:
	dc.b  $B,  6,$FF
	rev02even
byte_36251:
	dc.b  $B,  7,$FF
	rev02even
byte_36254:
	dc.b  $B,  8,$FF
	rev02even
byte_36257:
	dc.b  $B,  9,$FF
	even
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
Obj59_MapUnc_3625A:	include "mappings/sprite/obj59.asm"

; animation script:
; off_362D2:
Ani_obj5B_obj60:offsetTable
		offsetTableEntry.w byte_362E8	;  0
		offsetTableEntry.w byte_362EE	;  1
		offsetTableEntry.w byte_362F4	;  2
		offsetTableEntry.w byte_362FA	;  3
		offsetTableEntry.w byte_36300	;  4
		offsetTableEntry.w byte_36306	;  5
		offsetTableEntry.w byte_3630C	;  6
		offsetTableEntry.w byte_36312	;  7
		offsetTableEntry.w byte_36318	;  8
		offsetTableEntry.w byte_3631E	;  9
		offsetTableEntry.w byte_36324	; $A
byte_362E8: dc.b   5,  0, $A,$14, $A,$FF
	rev02even
byte_362EE: dc.b   5,  1, $B,$15, $B,$FF
	rev02even
byte_362F4: dc.b   5,  2, $C,$16, $C,$FF
	rev02even
byte_362FA: dc.b   5,  3, $D,$17, $D,$FF
	rev02even
byte_36300: dc.b   5,  4, $E,$18, $E,$FF
	rev02even
byte_36306: dc.b   5,  5, $F,$19, $F,$FF
	rev02even
byte_3630C: dc.b   5,  6,$10,$1A,$10,$FF
	rev02even
byte_36312: dc.b   5,  7,$11,$1B,$11,$FF
	rev02even
byte_36318: dc.b   5,  8,$12,$1C,$12,$FF
	rev02even
byte_3631E: dc.b   5,  9,$13,$1D,$13,$FF
	rev02even
byte_36324: dc.b   1,$1E,$1F,$20,$FF
	even
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
Obj5A_Obj5B_Obj60_MapUnc_3632A:	include "mappings/sprite/obj5A_5B_60.asm"

; animation script:
; off_364CE:
Ani_obj61:	offsetTable
		offsetTableEntry.w byte_364E4	;  0
		offsetTableEntry.w byte_364E7	;  1
		offsetTableEntry.w byte_364EA	;  2
		offsetTableEntry.w byte_364ED	;  3
		offsetTableEntry.w byte_364F0	;  4
		offsetTableEntry.w byte_364F3	;  5
		offsetTableEntry.w byte_364F6	;  6
		offsetTableEntry.w byte_364F9	;  7
		offsetTableEntry.w byte_364FC	;  8
		offsetTableEntry.w byte_364FF	;  9
		offsetTableEntry.w byte_36502	; $A
byte_364E4: dc.b  $B,  0,$FF
	rev02even
byte_364E7: dc.b  $B,  1,$FF
	rev02even
byte_364EA: dc.b  $B,  2,$FF
	rev02even
byte_364ED: dc.b  $B,  3,$FF
	rev02even
byte_364F0: dc.b  $B,  4,$FF
	rev02even
byte_364F3: dc.b  $B,  5,$FF
	rev02even
byte_364F6: dc.b  $B,  6,$FF
	rev02even
byte_364F9: dc.b  $B,  7,$FF
	rev02even
byte_364FC: dc.b  $B,  8,$FF
	rev02even
byte_364FF: dc.b  $B,  9,$FF
	rev02even
byte_36502: dc.b   2, $A, $B, $C,$FF
	even
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
Obj61_MapUnc_36508:	include "mappings/sprite/obj61.asm"
; ===========================================================================

JmpTo44_DisplaySprite ; JmpTo
	jmp	(DisplaySprite).l
; ===========================================================================

    if ~~removeJmpTos
JmpTo63_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
JmpTo24_AnimateSprite ; JmpTo
	jmp	(AnimateSprite).l
JmpTo_SSStartNewAct ; JmpTo
	jmp	(SSStartNewAct).l
JmpTo_CalcAngle ; JmpTo
	jmp	(CalcAngle).l
JmpTo14_CalcSine ; JmpTo
	jmp	(CalcSine).l
JmpTo7_ObjectMoveAndFall ; JmpTo
	jmp	(ObjectMoveAndFall).l
JmpTo_SSAllocateObjectAfterCurrent ; JmpTo
	jmp	(SSAllocateObjectAfterCurrent).l
JmpTo2_SSAllocateObject ; JmpTo
	jmp	(SSAllocateObject).l

	align 4
    endif
; ===========================================================================