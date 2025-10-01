; ----------------------------------------------------------------------------
; Object B0 - Sonic on the Sega screen
; ----------------------------------------------------------------------------
; Sprite_3A1DC:
ObjB0:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjB0_Index(pc,d0.w),d1
	jmp	ObjB0_Index(pc,d1.w)
; ===========================================================================
; off_3A1EA:
ObjB0_Index:	offsetTable
		offsetTableEntry.w ObjB0_Init		;  0
		offsetTableEntry.w ObjB0_RunLeft	;  2
		offsetTableEntry.w ObjB0_MidWipe	;  4
		offsetTableEntry.w ObjB0_RunRight	;  6
		offsetTableEntry.w ObjB0_EndWipe	;  8
		offsetTableEntry.w return_3A3F6		; $A
; ===========================================================================

ObjB0_Init:
	bsr.w	LoadSubObject
	move.w	#$1E8,x_pixel(a0)
	move.w	#$F0,y_pixel(a0)
	move.w	#$B,objoff_2A(a0)
	move.w	#2,(SegaScr_VInt_Subrout).w
	bset	#0,render_flags(a0)
	bset	#0,status(a0)

	; Initialize streak horizontal offsets for Sonic going left.
	; 9 full lines (8 pixels) + 6 pixels, 2-byte interleaved entries for PNT A and PNT B
	lea	(Horiz_Scroll_Buf + 2 * 2 * (9 * 8 + 6)).w,a1
	lea	Streak_Horizontal_offsets(pc),a2
	moveq	#0,d0
	moveq	#35-1,d6	; Number of streaks-1
-	move.b	(a2)+,d0
	add.w	d0,(a1)
	addq.w	#2 * 2 * 2,a1	; Advance to next streak 2 pixels down
	dbf	d6,-

	lea	off_3A294(pc),a1 ; pointers to mapping DPLC data
	lea	(ArtUnc_Sonic).l,a3
	lea	(Chunk_Table).l,a5
	moveq	#4-1,d5 ; there are 4 mapping frames to loop over

	; this copies the tiles that we want to scale up from ROM to RAM
;loc_3A246:
;CopySpriteTilesToRAMForSegaScreen:
-	movea.l	(a1)+,a2
	move.w	(a2)+,d6 ; get the number of pieces in this mapping frame
	subq.w	#1,d6
-	move.w	(a2)+,d0
	move.w	d0,d1
	; Depending on the exact location (and size) of the art being used,
	; you may encounter an overflow in the original code which garbles
	; the enlarged Sonic. The following code fixes this:
    if fixBugs
	andi.l	#$FFF,d0
	lsl.l	#5,d0
	lea	(a3,d0.l),a4 ; source ROM address of tiles to copy
    else
	andi.w	#$FFF,d0
	lsl.w	#5,d0
	lea	(a3,d0.w),a4 ; source ROM address of tiles to copy
    endif
	andi.w	#$F000,d1 ; abcd000000000000
	rol.w	#4,d1	  ; (this calculation can be done smaller and faster
	addq.w	#1,d1	  ; by doing rol.w #7,d1 addq.w #7,d1
	lsl.w	#3,d1	  ; instead of these 4 lines)
	subq.w	#1,d1	  ; 000000000abcd111 ; number of dwords to copy minus 1
-	move.l	(a4)+,(a5)+
	dbf	d1,- ; copy all of the pixels in this piece into the temp buffer
	dbf	d6,-- ; loop per piece in the frame
	dbf	d5,--- ; loop per mapping frame

	; this scales up the tiles by 2
;ScaleUpSpriteTiles:
	move.w	d7,-(sp)
	moveq	#0,d0
	moveq	#0,d1
	lea	SonicRunningSpriteScaleData(pc),a6
	moveq	#4*2-1,d7 ; there are 4 sprite mapping frames with 2 pieces each
-	movea.l	(a6)+,a1 ; source in RAM of tile graphics to enlarge
	movea.l	(a6)+,a2 ; destination in RAM of enlarged graphics
	move.b	(a6)+,d0 ; width of the sprite piece to enlarge (minus 1)
	move.b	(a6)+,d1 ; height of the sprite piece to enlarge (minus 1)
	bsr.w	Scale_2x
	dbf	d7,- ; loop over each piece
	move.w	(sp)+,d7

	rts
; ===========================================================================
off_3A294:
	dc.l MapRUnc_Sonic.frame45
	dc.l MapRUnc_Sonic.frame46
	dc.l MapRUnc_Sonic.frame47
	dc.l MapRUnc_Sonic.frame48

map_piece macro width,height
	dc.l copysrc,copydst
	dc.b width-1,height-1
copysrc := copysrc + tiles_to_bytes(width * height)
copydst := copydst + tiles_to_bytes(width * height) * 2 * 2
    endm
;word_3A2A4:
SonicRunningSpriteScaleData:
copysrc := Chunk_Table
copydst := Chunk_Table + $B00
SegaScreenScaledSpriteDataStart = copydst
	rept 4 ; repeat 4 times since there are 4 frames to scale up
	; piece 1 of each frame (the smaller top piece):
	map_piece 3,2
	; piece 2 of each frame (the larger bottom piece):
	map_piece 4,4
	endm
SegaScreenScaledSpriteDataEnd = copydst
	if copysrc > SegaScreenScaledSpriteDataStart
	fatal "Scale copy source overran allocated size. Try changing the initial value of copydst to Chunk_Table+$\{copysrc-Chunk_Table}"
	endif
; ===========================================================================

ObjB0_RunLeft:
	subi.w	#$20,x_pos(a0)
	subq.w	#1,objoff_2A(a0)
	bmi.s	loc_3A312
	bsr.w	ObjB0_Move_Streaks_Left
	lea	(Ani_objB0).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	DisplaySprite, JmpTo45_DisplaySprite
; ===========================================================================

loc_3A312:
	addq.b	#2,routine(a0)
	move.w	#$C,objoff_2A(a0)
	move.b	#1,objoff_2C(a0)
	move.b	#-1,objoff_2D(a0)
	jmpto	DisplaySprite, JmpTo45_DisplaySprite
; ===========================================================================

ObjB0_MidWipe:
	tst.w	objoff_2A(a0)
	beq.s	loc_3A33A
	subq.w	#1,objoff_2A(a0)
	bsr.w	ObjB0_Move_Streaks_Left

loc_3A33A:
	lea	word_3A49E(pc),a1
	bsr.w	loc_3A44E
	bne.s	loc_3A346
	rts
; ===========================================================================

loc_3A346:
	addq.b	#2,routine(a0)
	bchg	#0,render_flags(a0)
	move.w	#$B,objoff_2A(a0)
	move.w	#4,(SegaScr_VInt_Subrout).w
	subi.w	#$28,x_pos(a0)
	bchg	#0,render_flags(a0)
	bchg	#0,status(a0)

    if fixBugs
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+HorizontalScrollBuffer.len
    else
	; This clears a lot more than the horizontal scroll buffer, which is $400 bytes.
	; This is because the loop counter is erroneously set to $400, instead of ($400/4)-1.
	clearRAM Horiz_Scroll_Buf,Horiz_Scroll_Buf+(HorizontalScrollBuffer.len*4+4)
    endif

	; Initialize streak horizontal offsets for Sonic going right.
	; 9 full lines (8 pixels) + 7 pixels, 2-byte interleaved entries for PNT A and PNT B
	lea	(Horiz_Scroll_Buf + 2 * 2 * (9 * 8 + 7)).w,a1
	lea	Streak_Horizontal_offsets(pc),a2
	moveq	#0,d0
	moveq	#35-1,d6	; Number of streaks-1

loc_3A38A:
	move.b	(a2)+,d0
	sub.w	d0,(a1)
	addq.w	#2 * 2 * 2,a1	; Advance to next streak 2 pixels down
	dbf	d6,loc_3A38A
	rts
; ===========================================================================

ObjB0_RunRight:
	subq.w	#1,objoff_2A(a0)
	bmi.s	loc_3A3B4
	addi.w	#$20,x_pos(a0)
	bsr.w	ObjB0_Move_Streaks_Right
	lea	(Ani_objB0).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	DisplaySprite, JmpTo45_DisplaySprite
; ===========================================================================

loc_3A3B4:
	addq.b	#2,routine(a0)
	move.w	#$C,objoff_2A(a0)
	move.b	#1,objoff_2C(a0)
	move.b	#-1,objoff_2D(a0)
	rts
; ===========================================================================

ObjB0_EndWipe:
	tst.w	objoff_2A(a0)
	beq.s	loc_3A3DA
	subq.w	#1,objoff_2A(a0)
	bsr.w	ObjB0_Move_Streaks_Right

loc_3A3DA:
	lea	word_3A514(pc),a1
	bsr.w	loc_3A44E
	bne.s	loc_3A3E6
	rts
; ===========================================================================

loc_3A3E6:
	addq.b	#2,routine(a0)
	st.b	(SegaScr_PalDone_Flag).w
	move.b	#SndID_SegaSound,d0
	jsrto	PlaySound, JmpTo12_PlaySound

return_3A3F6:
	rts
; ===========================================================================