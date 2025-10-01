; ----------------------------------------------------------------------------
; Object B4 - Vertical propeller from WFZ
; ----------------------------------------------------------------------------
; Sprite_3B36A:
ObjB4:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjB4_Index(pc,d0.w),d1
	jmp	ObjB4_Index(pc,d1.w)
; ===========================================================================
; off_3B378:
ObjB4_Index:	offsetTable
		offsetTableEntry.w ObjB4_Init	; 0
		offsetTableEntry.w ObjB4_Main	; 2
; ===========================================================================
; loc_3B37C:
ObjB4_Init:
	bsr.w	LoadSubObject
	bclr	#1,render_flags(a0)
	beq.s	+
	clr.b	collision_flags(a0)
+
	rts
; ===========================================================================
; loc_3B38E:
ObjB4_Main:
	lea	(Ani_objB4).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	move.b	(Vint_runcount+3).w,d0
	andi.b	#$1F,d0
	bne.s	+
	moveq	#signextendB(SndID_Helicopter),d0
	jsrto	PlaySoundLocal, JmpTo_PlaySoundLocal
+
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; off_3B3AC:
ObjB4_SubObjData:
	subObjData ObjB4_MapUnc_3B3BE,make_art_tile(ArtTile_ArtNem_WfzVrtclPrpllr,1,1),4,4,4,$A8
; animation script
; off_3B3B6:
Ani_objB4:	offsetTable
		offsetTableEntry.w +	; 0
+		dc.b   1,  0,  1,  2,$FF,  0
		even
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
ObjB4_MapUnc_3B3BE:	include "mappings/sprite/objB4.asm"
; ===========================================================================
; ----------------------------------------------------------------------------
; Object B5 - Horizontal propeller from WFZ
; ----------------------------------------------------------------------------
; Sprite_3B3FA:
ObjB5:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjB5_Index(pc,d0.w),d1
	jmp	ObjB5_Index(pc,d1.w)
; ===========================================================================
; off_3B408:
ObjB5_Index:	offsetTable
		offsetTableEntry.w ObjB5_Init		; 0
		offsetTableEntry.w ObjB5_Main		; 2 - used in WFZ
		offsetTableEntry.w ObjB5_Animate	; 4 - used in SCZ, no effect on players
; ===========================================================================
; loc_3B40E:
ObjB5_Init:
	bsr.w	LoadSubObject
	move.b	#4,anim(a0)
	move.b	subtype(a0),d0
	subi.b	#$64,d0
	move.b	d0,routine(a0)
	rts
; ===========================================================================
; loc_3B426:
ObjB5_Main:
	moveq	#0,d0
	move.b	routine_secondary(a0),d0
	move.w	off_3B442(pc,d0.w),d1
	jsr	off_3B442(pc,d1.w)
	lea	(Ani_objB5).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
off_3B442:	offsetTable
		offsetTableEntry.w +	; 0
; ===========================================================================
+	bra.w	ObjB5_CheckPlayers
; ===========================================================================
; loc_3B448:
ObjB5_Animate:
	lea	(Ani_objB5).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; loc_3B456:
ObjB5_CheckPlayers:
	cmpi.b	#4,anim(a0)
	bne.s	++	; rts
	lea	(MainCharacter).w,a1 ; a1=character
	bsr.w	ObjB5_CheckPlayer
	lea	(Sidekick).w,a1 ; a1=character
; loc_3B46A:
ObjB5_CheckPlayer:
	move.w	x_pos(a1),d0
	sub.w	x_pos(a0),d0
	addi.w	#$40,d0
	cmpi.w	#$80,d0
	bhs.s	++	; rts
	moveq	#0,d1
	move.b	(Oscillating_Data+$14).w,d1
	add.w	y_pos(a1),d1
	addi.w	#$60,d1
	sub.w	y_pos(a0),d1
	bcs.s	++	; rts
	cmpi.w	#$90,d1
	bhs.s	++	; rts
	subi.w	#$60,d1
	bcs.s	+
	not.w	d1
	add.w	d1,d1
+
	addi.w	#$60,d1
	neg.w	d1
	asr.w	#4,d1
	add.w	d1,y_pos(a1)
	bset	#1,status(a1)
	move.w	#0,y_vel(a1)
	move.w	#1,inertia(a1)
	tst.b	flip_angle(a1)
	bne.s	+	; rts
	move.b	#1,flip_angle(a1)
	move.b	#AniIDSonAni_Float2,anim(a1)
	move.b	#$7F,flips_remaining(a1)
	move.b	#8,flip_speed(a1)
+
	rts
; ===========================================================================
; off_3B4DE:
ObjB5_SubObjData:
	subObjData ObjB5_MapUnc_3B548,make_art_tile(ArtTile_ArtNem_WfzHrzntlPrpllr,1,1),4,4,$40,0

; animation script
; off_3B4E8:
Ani_objB5:	offsetTable
		offsetTableEntry.w byte_3B4FC	; 0
		offsetTableEntry.w byte_3B506	; 1
		offsetTableEntry.w byte_3B50E	; 2
		offsetTableEntry.w byte_3B516	; 3
		offsetTableEntry.w byte_3B51C	; 4
		offsetTableEntry.w byte_3B524	; 5
		offsetTableEntry.w byte_3B52A	; 6
		offsetTableEntry.w byte_3B532	; 7
		offsetTableEntry.w byte_3B53A	; 8
		offsetTableEntry.w byte_3B544	; 9
byte_3B4FC:	dc.b   7,  0,  1,  2,  3,  4,  5,$FD,  1,  0
byte_3B506:	dc.b   4,  0,  1,  2,  3,  4,$FD,  2
byte_3B50E:	dc.b   3,  5,  0,  1,  2,$FD,  3,  0
byte_3B516:	dc.b   2,  3,  4,  5,$FD,  4
byte_3B51C:	dc.b   1,  0,  1,  2,  3,  4,  5,$FF
byte_3B524:	dc.b   2,  5,  4,  3,$FD,  6
byte_3B52A:	dc.b   3,  2,  1,  0,  5,$FD,  7,  0
byte_3B532:	dc.b   4,  4,  3,  2,  1,  0,$FD,  8
byte_3B53A:	dc.b   7,  5,  4,  3,  2,  1,  0,$FD,  9,  0
byte_3B544:	dc.b $7E,  0,$FF
		even
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
ObjB5_MapUnc_3B548:	include "mappings/sprite/objB5.asm"
; ===========================================================================