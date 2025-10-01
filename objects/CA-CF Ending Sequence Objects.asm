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