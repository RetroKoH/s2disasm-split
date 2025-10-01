; ----------------------------------------------------------------------------
; Object C1 - Breakable plating from WFZ
; (and what Sonic hangs onto on the back of Robotnik's getaway ship)
; ----------------------------------------------------------------------------
; Sprite_3C0AC:
ObjC1:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjC1_Index(pc,d0.w),d1
	jmp	ObjC1_Index(pc,d1.w)
; ===========================================================================
; off_3C0BA:
ObjC1_Index:	offsetTable
		offsetTableEntry.w ObjC1_Init	; 0
		offsetTableEntry.w ObjC1_Main	; 2
		offsetTableEntry.w ObjC1_Breakup	; 4
; ===========================================================================
; loc_3C0C0:
ObjC1_Init:
	move.w	#($44<<1),d0
	bsr.w	LoadSubObject_Part2
	moveq	#0,d0
	move.b	subtype(a0),d0
	mulu.w	#60,d0
	move.w	d0,objoff_30(a0)

ObjC1_Main:
	tst.b	objoff_32(a0)
	beq.s	loc_3C140
	tst.w	objoff_30(a0)
	beq.s	+
	subq.w	#1,objoff_30(a0)
	beq.s	loc_3C12E
+
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	y_pos(a0),d0
	subi.w	#$18,d0
	btst	#button_up,(Ctrl_1_Held).w
	beq.s	+
	subq.w	#1,y_pos(a1)
	cmp.w	y_pos(a1),d0
	blo.s	+
	move.w	d0,y_pos(a1)
+
	addi.w	#$30,d0
	btst	#button_down,(Ctrl_1_Held).w
	beq.s	+
	addq.w	#1,y_pos(a1)
	cmp.w	y_pos(a1),d0
	bhs.s	+
	move.w	d0,y_pos(a1)
+
	move.b	(Ctrl_1_Press_Logical).w,d0
	andi.w	#button_B_mask|button_C_mask|button_A_mask,d0
	beq.s	BranchTo16_JmpTo39_MarkObjGone

loc_3C12E:
	clr.b	collision_flags(a0)
	clr.b	(MainCharacter+obj_control).w
	clr.b	(WindTunnel_holding_flag).w
	clr.b	objoff_32(a0)
	bra.s	loc_3C19A
; ===========================================================================

loc_3C140:
	tst.b	collision_property(a0)
	beq.s	BranchTo16_JmpTo39_MarkObjGone
	lea	(MainCharacter).w,a1 ; a1=character
	move.w	x_pos(a0),d0
	subi.w	#$14,d0
	cmp.w	x_pos(a1),d0
	bhs.s	BranchTo16_JmpTo39_MarkObjGone
	clr.b	collision_property(a0)
	cmpi.b	#4,routine(a1)
	bhs.s	BranchTo16_JmpTo39_MarkObjGone
	clr.w	x_vel(a1)
	clr.w	y_vel(a1)
	move.w	x_pos(a0),d0
	subi.w	#$14,d0
	move.w	d0,x_pos(a1)
	bset	#0,status(a1)
	move.b	#AniIDSonAni_Hang,anim(a1)
	move.b	#1,(MainCharacter+obj_control).w
	move.b	#1,(WindTunnel_holding_flag).w
	move.b	#1,objoff_32(a0)

BranchTo16_JmpTo39_MarkObjGone
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================

loc_3C19A:
	lea	(byte_3C1E4).l,a4
	lea	(byte_3C1E0).l,a2
	bsr.w	loc_3C1F4

ObjC1_Breakup:
	tst.b	objoff_3F(a0)
	beq.s	+
	subq.b	#1,objoff_3F(a0)
	bra.s	++
; ===========================================================================
+
	jsrto	ObjectMove, JmpTo26_ObjectMove
	addi_.w	#8,y_vel(a0)
	lea	(Ani_objC1).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
+
	tst.b	render_flags(a0)
	bpl.w	JmpTo65_DeleteObject
	jmpto	DisplaySprite, JmpTo45_DisplaySprite
; ===========================================================================
; animation script
; off_3C1D6:
Ani_objC1:	offsetTable
		offsetTableEntry.w +	; 0
+		dc.b   3,  2,  3,  4,  5,  1,$FF
		even

; unknown
byte_3C1E0:
	dc.b   0
	dc.b   4	; 1
	dc.b $18	; 2
	dc.b $20	; 3
	even
byte_3C1E4:
	dc.w  -$10
	dc.w  -$10	; 2
	dc.w  -$10	; 4
	dc.w   $10	; 6
	dc.w  -$30	; 8
	dc.w  -$10	; 10
	dc.w  -$30	; 12
	dc.w   $10	; 14
; ===========================================================================

loc_3C1F4:
	move.w	x_pos(a0),d2
	move.w	y_pos(a0),d3
	move.b	priority(a0),d4
	subq.b	#1,d4
	moveq	#3,d1
	movea.l	a0,a1
	bra.s	loc_3C20E
; ===========================================================================

loc_3C208:
	jsrto	AllocateObjectAfterCurrent, JmpTo25_AllocateObjectAfterCurrent
	bne.s	loc_3C26C

loc_3C20E:
	move.b	#4,routine(a1)
	_move.b	id(a0),id(a1) ; load obj
	move.l	mappings(a0),mappings(a1)
	move.w	art_tile(a0),art_tile(a1)
	move.b	#$84,render_flags(a1)
	move.w	x_pos(a0),x_pos(a1)
	move.w	y_pos(a0),y_pos(a1)
	move.w	(a4)+,d0
	add.w	d2,d0
	move.w	d0,x_pos(a1)
	move.w	(a4)+,d0
	add.w	d3,d0
	move.w	d0,y_pos(a1)
	move.b	d4,priority(a1)
	move.b	#$10,width_pixels(a1)
	move.b	#1,mapping_frame(a1)
	move.w	#-$400,x_vel(a1)
	move.w	#0,y_vel(a1)
	move.b	(a2)+,objoff_3F(a1)
	dbf	d1,loc_3C208

loc_3C26C:
	move.w	#SndID_SlowSmash,d0
	jmp	(PlaySound).l
; ===========================================================================
; off_3C276:
ObjC1_SubObjData:
	subObjData ObjC1_MapUnc_3C280,make_art_tile(ArtTile_ArtNem_BreakPanels,3,1),4,4,$40,$E1
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
ObjC1_MapUnc_3C280:	include "mappings/sprite/objC1.asm"
; ===========================================================================