; ----------------------------------------------------------------------------
; Object 8D - Grounder in wall, from ARZ
; ----------------------------------------------------------------------------
; Sprite_36A76:
Obj8D:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj8D_Index(pc,d0.w),d1
	jmp	Obj8D_Index(pc,d1.w)
; ===========================================================================
; off_36A84:
Obj8D_Index:	offsetTable
		offsetTableEntry.w Obj8D_Init		;  0
		offsetTableEntry.w loc_36ADC		;  2
		offsetTableEntry.w Obj8D_Animate	;  4
		offsetTableEntry.w loc_36B0E		;  6
		offsetTableEntry.w loc_36B34		;  8
		offsetTableEntry.w loc_36B6A		; $A
; ===========================================================================
; loc_36A90:
Obj8D_Init:
	bsr.w	LoadSubObject
	bclr	#1,render_flags(a0)
	beq.s	+
	bclr	#1,status(a0)
	andi.w	#drawing_mask,art_tile(a0)
+
	move.b	#$14,y_radius(a0)
	move.b	#$10,x_radius(a0)
	jsr	(ObjCheckFloorDist).l
	tst.w	d1
	bpl.s	+
	add.w	d1,y_pos(a0)
	move.w	#0,y_vel(a0)
+
	_move.b	id(a0),d0
	subi.b	#ObjID_GrounderInWall,d0
	beq.w	loc_36C64
	move.b	#6,routine(a0)
	rts
; ===========================================================================

loc_36ADC:
	bsr.w	Obj_GetOrientationToPlayer
	abs.w	d2
	cmpi.w	#$60,d2
	bls.s	+
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
+
	addq.b	#2,routine(a0)
	st.b	objoff_2B(a0)
	bsr.w	loc_36C2C
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; loc_36B00:
Obj8D_Animate:
	lea	(Ani_obj8D_b).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================

loc_36B0E:
	addq.b	#2,routine(a0)
	bsr.w	Obj_GetOrientationToPlayer
	move.w	Obj8D_Directions(pc,d0.w),x_vel(a0)
	bclr	#0,status(a0)
	tst.w	d0
	beq.s	+
	bset	#0,status(a0)
+
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; word_36B30:
Obj8D_Directions:
	dc.w -$100
	dc.w  $100	; 1
; ===========================================================================

loc_36B34:
	jsrto	ObjectMove, JmpTo26_ObjectMove
	jsr	(ObjCheckFloorDist).l
	cmpi.w	#-1,d1
	blt.s	loc_36B5C
	cmpi.w	#$C,d1
	bge.s	loc_36B5C
	add.w	d1,y_pos(a0)
	lea	(Ani_obj8D_a).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================

loc_36B5C:
	addq.b	#2,routine(a0)
	move.b	#$3B,objoff_2A(a0)
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================

loc_36B6A:
	subq.b	#1,objoff_2A(a0)
	bmi.s	loc_36B74
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================

loc_36B74:
	move.b	#8,routine(a0)
	neg.w	x_vel(a0)
	bchg	#0,status(a0)
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; ----------------------------------------------------------------------------
; Object 8F - Wall behind which Grounder hides, from ARZ
; ----------------------------------------------------------------------------
; Sprite_36B88:
Obj8F:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj8F_Index(pc,d0.w),d1
	jmp	Obj8F_Index(pc,d1.w)
; ===========================================================================
; off_36B96:
Obj8F_Index:	offsetTable
		offsetTableEntry.w Obj8F_Init	; 0
		offsetTableEntry.w loc_36BA6	; 2
		offsetTableEntry.w Obj8F_Move	; 4
; ===========================================================================
; loc_36B9C:
Obj8F_Init:
	bsr.w	LoadSubObject
	clr.w	art_tile(a0)
	rts
; ===========================================================================

loc_36BA6:
	movea.w	objoff_2C(a0),a1 ; a1=object
	tst.b	objoff_2B(a1)
	bne.s	+
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
+
	addq.b	#2,routine(a0)
	move.w	objoff_2E(a0),d0
	move.b	Obj8F_Directions(pc,d0.w),x_vel(a0)
	move.b	Obj8F_Directions+1(pc,d0.w),y_vel(a0)
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; byte_36BCC:
Obj8F_Directions:
	dc.b  1,-2	; 0
	dc.b  1,-1	; 2
	dc.b -1,-2	; 4
	dc.b -1,-1	; 6
; ===========================================================================
; ----------------------------------------------------------------------------
; Object 90 - Rocks thrown by Grounder behind wall, from ARZ
; ----------------------------------------------------------------------------
; Sprite_36BD4:
Obj90:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj90_Index(pc,d0.w),d1
	jmp	Obj90_Index(pc,d1.w)
; ===========================================================================
; off_36BE2:
Obj90_Index:	offsetTable
		offsetTableEntry.w Obj90_Init	; 0
		offsetTableEntry.w Obj90_Move	; 2
; ===========================================================================
; loc_36BE6:
Obj90_Init:
	bsr.w	LoadSubObject
	move.w	#make_art_tile(ArtTile_ArtNem_Grounder,2,0),art_tile(a0)
	move.w	objoff_2E(a0),d0
	move.b	Obj90_Directions(pc,d0.w),x_vel(a0)
	move.b	Obj90_Directions+1(pc,d0.w),y_vel(a0)
	lsr.w	#1,d0
	move.b	Obj90_Frames(pc,d0.w),mapping_frame(a0)
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; byte_36C0C:
Obj90_Frames:
	dc.b   0
	dc.b   2	; 1
	dc.b   0	; 2
	dc.b   1	; 3
	dc.b   0	; 4
	dc.b   0	; 5
; ===========================================================================
; byte_36C12:
Obj90_Directions:
	dc.b  -1, -4
	dc.b   4, -3	; 2
	dc.b   2,  0	; 4
	dc.b  -3, -1	; 6
	dc.b  -3, -3	; 8
	even
; ===========================================================================
; loc_36C1C:
Obj8F_Move:
Obj90_Move:
	tst.b	render_flags(a0)
	bpl.w	JmpTo65_DeleteObject
	jsrto	ObjectMoveAndFall, JmpTo8_ObjectMoveAndFall
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================

loc_36C2C:
	moveq	#0,d1

	moveq	#4,d6
-	jsrto	AllocateObject, JmpTo19_AllocateObject
	bne.s	+	; rts
	bsr.w	loc_36C40
	dbf	d6,-
+
	rts
; ===========================================================================

loc_36C40:
	_move.b	#ObjID_GrounderRocks,id(a1) ; load obj90
	move.b	#6,subtype(a1) ; <== Obj90_SubObjData2
	move.w	a0,objoff_2C(a1)
	move.w	d1,objoff_2E(a1)
	move.w	x_pos(a0),x_pos(a1)
	move.w	y_pos(a0),y_pos(a1)
	addq.w	#2,d1
	rts
; ===========================================================================

loc_36C64:
	moveq	#0,d1

	moveq	#3,d6
-	jsrto	AllocateObject, JmpTo19_AllocateObject
	bne.s	+	; rts
	bsr.w	loc_36C78
	dbf	d6,-
+
	rts
; ===========================================================================

loc_36C78:
	_move.b	#ObjID_GrounderWall,id(a1) ; load obj8F
	move.b	#4,subtype(a1) ; <== Obj90_SubObjData
	move.w	a0,objoff_2C(a1)
	move.w	d1,objoff_2E(a1)
	move.l	x_pos(a0),d0
	swap	d0
	moveq	#0,d2
	move.b	byte_36CBC(pc,d1.w),d2
	ext.w	d2
	add.w	d2,d0
	swap	d0
	move.l	d0,x_pos(a1)
	move.l	y_pos(a0),d0
	swap	d0
	moveq	#0,d2
	move.b	byte_36CBC+1(pc,d1.w),d2
	ext.w	d2
	add.w	d2,d0
	swap	d0
	move.l	d0,y_pos(a1)
	addq.w	#2,d1
	rts
; ===========================================================================
byte_36CBC:
	dc.b    0,-$14
	dc.b  $10,  -4	; 2
	dc.b    0,  $C	; 4
	dc.b -$10,  -4	; 6
; off_36CC4:
Obj8D_SubObjData:
	subObjData Obj8D_MapUnc_36CF0,make_art_tile(ArtTile_ArtNem_Grounder,1,1),4,5,$10,2
; off_36CCE:
Obj90_SubObjData:
	subObjData Obj90_MapUnc_36D00,make_art_tile(ArtTile_ArtKos_LevelArt,0,0),$84,4,$10,0
; off_36CD8:
Obj90_SubObjData2:
	subObjData Obj90_MapUnc_36CFA,make_art_tile(ArtTile_ArtNem_Grounder,1,1),$84,4,8,0

; animation script
Ani_obj8D_a:	offsetTable
		offsetTableEntry.w +	; 0
+		dc.b   3,  2,  3,  4,$FF
		even
; animation script
; off_36CEA:
Ani_obj8D_b:	offsetTable
		offsetTableEntry.w +
+		dc.b   7,  0,  1,$FC
		even
; -----------------------------------------------------------------------------
; sprite mappings (obj8D)
; -----------------------------------------------------------------------------
Obj8D_MapUnc_36CF0:	mappingsTable
	mappingsTableEntry.w	word_36D02
	mappingsTableEntry.w	word_36D24
	mappingsTableEntry.w	word_36D46
	mappingsTableEntry.w	word_36D58
	mappingsTableEntry.w	word_36D6A
; -----------------------------------------------------------------------------
; sprite mappings (obj90)
; -----------------------------------------------------------------------------
Obj90_MapUnc_36CFA:	mappingsTable
	mappingsTableEntry.w	word_36D7C
	mappingsTableEntry.w	word_36D86
	mappingsTableEntry.w	word_36D90
; -----------------------------------------------------------------------------
; sprite mappings (obj90)
; -----------------------------------------------------------------------------
Obj90_MapUnc_36D00:	mappingsTable
	mappingsTableEntry.w	word_36D9A

word_36D02:	spriteHeader
	spritePiece	-8, -$C, 1, 1, 0, 0, 0, 0, 0
	spritePiece	-$10, -4, 2, 3, 1, 0, 0, 0, 0
	spritePiece	0, -$C, 1, 1, 0, 1, 0, 0, 0
	spritePiece	0, -4, 2, 3, 1, 1, 0, 0, 0
word_36D02_End

word_36D24:	spriteHeader
	spritePiece	-8, -$14, 1, 1, 7, 0, 0, 0, 0
	spritePiece	-$10, -$C, 2, 4, 8, 0, 0, 0, 0
	spritePiece	0, -$14, 1, 1, 7, 1, 0, 0, 0
	spritePiece	0, -$C, 2, 4, 8, 1, 0, 0, 0
word_36D24_End

word_36D46:	spriteHeader
	spritePiece	-$10, -$14, 4, 4, $10, 0, 0, 0, 0
	spritePiece	-$10, $C, 4, 1, $20, 0, 0, 0, 0
word_36D46_End

word_36D58:	spriteHeader
	spritePiece	-$10, -$14, 4, 4, $10, 0, 0, 0, 0
	spritePiece	-$10, $C, 4, 1, $24, 0, 0, 0, 0
word_36D58_End

word_36D6A:	spriteHeader
	spritePiece	-$10, -$14, 4, 4, $10, 0, 0, 0, 0
	spritePiece	-$10, $C, 4, 1, $28, 0, 0, 0, 0
word_36D6A_End

word_36D7C:	spriteHeader
	spritePiece	-8, -8, 2, 2, $2C, 0, 0, 0, 0
word_36D7C_End

word_36D86:	spriteHeader
	spritePiece	-4, -4, 1, 1, $30, 0, 0, 0, 0
word_36D86_End

word_36D90:	spriteHeader
	spritePiece	-4, -4, 1, 1, $31, 0, 0, 0, 0
word_36D90_End

word_36D9A:	spriteHeader
	spritePiece	-$10, -8, 2, 2, $93, 0, 0, 2, 0
	spritePiece	0, -8, 2, 2, $97, 0, 0, 2, 0
word_36D9A_End

	even

; ===========================================================================