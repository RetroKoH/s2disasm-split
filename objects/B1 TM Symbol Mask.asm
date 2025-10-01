; ----------------------------------------------------------------------------
; Object B1 - Object that hides TM symbol on JP region
; ----------------------------------------------------------------------------
; Sprite_3A3F8:
ObjB1:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjB1_Index(pc,d0.w),d1
	jmp	ObjB1_Index(pc,d1.w)
; ===========================================================================
; off_3A406:
ObjB1_Index:	offsetTable
		offsetTableEntry.w ObjB1_Init	; 0
		offsetTableEntry.w ObjB1_Main	; 2
; ===========================================================================
; loc_3A40A:
ObjB1_Init:
	bsr.w	LoadSubObject
	move.b	#4,mapping_frame(a0)
	move.w	#$174,x_pixel(a0)
	move.w	#$D8,y_pixel(a0)
	rts
; ===========================================================================
; BranchTo4_JmpTo45_DisplaySprite
ObjB1_Main:
	jmpto	DisplaySprite, JmpTo45_DisplaySprite
; ===========================================================================

ObjB0_Move_Streaks_Left:
	; 9 full lines (8 pixels) + 6 pixels, 2-byte interleaved entries for PNT A and PNT B
	lea	(Horiz_Scroll_Buf + 2 * 2 * (9 * 8 + 6)).w,a1

	move.w	#35-1,d6	; Number of streaks-1
-	subi.w	#$20,(a1)
	addq.w	#2 * 2 * 2,a1	; Advance to next streak 2 pixels down
	dbf	d6,-
	rts
; ===========================================================================

ObjB0_Move_Streaks_Right:
	; 9 full lines (8 pixels) + 7 pixels, 2-byte interleaved entries for PNT A and PNT B
	lea	(Horiz_Scroll_Buf + 2 * 2 * (9 * 8 + 7)).w,a1

	move.w	#35-1,d6	; Number of streaks-1
-	addi.w	#$20,(a1)
	addq.w	#2 * 2 * 2,a1	; Advance to next streak 2 pixels down
	dbf	d6,-
	rts
; ===========================================================================

loc_3A44E:
	subq.b	#1,objoff_2C(a0)
	bne.s	loc_3A496
	moveq	#0,d0
	move.b	objoff_2D(a0),d0
	addq.b	#1,d0
	cmp.b	1(a1),d0
	blo.s	loc_3A468
	tst.b	3(a1)
	bne.s	loc_3A49A

loc_3A468:
	move.b	d0,objoff_2D(a0)
	_move.b	0(a1),objoff_2C(a0)
	lea	6(a1),a2		; This loads a palette: Sega Screen 2.bin or Sega Screen 3.bin
	moveq	#0,d1
	move.b	2(a1),d1
	move.w	d1,d2
	tst.w	d0
	beq.s	loc_3A48C

loc_3A482:
	subq.b	#1,d0
	beq.s	loc_3A48A
	add.w	d2,d1
	bra.s	loc_3A482
; ===========================================================================

loc_3A48A:
	adda.w	d1,a2

loc_3A48C:
	movea.w	4(a1),a3

loc_3A490:
	move.w	(a2)+,(a3)+
	subq.w	#2,d2
	bne.s	loc_3A490

loc_3A496:
	moveq	#0,d0
	rts
; ===========================================================================

loc_3A49A:
	moveq	#1,d0
	rts
; ===========================================================================

; probably some sort of description of how to use the following palette
word_3A49E:
	dc.b   4	; 0	; How many frames before each iteration
	dc.b   7	; 1	; How many iterations
	dc.b $10	; 2	; Number of colors * 2 to skip each iteration
	dc.b $FF	; 3	; Some sort of flag
	dc.w Normal_palette+$10	; 4	; First target palette entry

; Palette for the SEGA screen (background and pre-wipe foreground) (7 frames)
;pal_3A4A4:
	BINCLUDE	"art/palettes/Sega Screen 2.bin"


; probably some sort of description of how to use the following palette
word_3A514:
	dc.b   4	; 0	; How many frames before each iteration
	dc.b   7	; 1	; How many iterations
	dc.b $10	; 2	; Number of colors * 2 to skip each iteration
	dc.b $FF	; 3	; Some sort of flag
	dc.w Normal_palette	; 4	; First target palette entry

; Palette for the SEGA screen (wiping and post-wipe foreground) (7 frames)
;pal_3A51A:
	BINCLUDE	"art/palettes/Sega Screen 3.bin"

; off_3A58A:
ObjB0_SubObjData:
	subObjData ObjB1_MapUnc_3A5A6,make_art_tile(ArtTile_ArtUnc_Giant_Sonic,2,1),0,1,$10,0

; off_3A594:
ObjB1_SubObjData:
	subObjData ObjB1_MapUnc_3A5A6,make_art_tile(ArtTile_ArtNem_Sega_Logo+2,0,0),0,2,8,0

; animation script
; off_3A59E:
Ani_objB0:	offsetTable
		offsetTableEntry.w +	; 0
+		dc.b   0,  0,  1,  2,  3,$FF
		even

; ------------------------------------------------------------------------------
; sprite mappings
; Gigantic Sonic (2x size) mappings for the SEGA screen
; also has the "trademark hider" mappings
; ------------------------------------------------------------------------------
ObjB1_MapUnc_3A5A6:	include "mappings/sprite/objB1.asm"
; ===========================================================================
;loc_3A68A
SegaScr_VInt:
	move.w	(SegaScr_VInt_Subrout).w,d0
	beq.w	return_37A48
	clr.w	(SegaScr_VInt_Subrout).w
	move.w	off_3A69E-2(pc,d0.w),d0
	jmp	off_3A69E(pc,d0.w)
; ===========================================================================
off_3A69E:	offsetTable
		offsetTableEntry.w loc_3A6A2	; 0
		offsetTableEntry.w loc_3A6D4	; 2
; ===========================================================================

loc_3A6A2:
	dma68kToVDP SegaScreenScaledSpriteDataStart,tiles_to_bytes(ArtTile_ArtUnc_Giant_Sonic),\
	            SegaScreenScaledSpriteDataEnd-SegaScreenScaledSpriteDataStart,VRAM

	lea	ObjB1_Streak_fade_to_right(pc),a1
	; 9 full lines ($100 bytes each) plus $28 8-pixel cells
	move.l	#vdpComm(VRAM_SegaScr_Plane_A_Name_Table + planeLoc(128,40,9),VRAM,WRITE),d0	; $49500003
	bra.w	loc_3A710
; ===========================================================================

loc_3A6D4:
	dmaFillVRAM 0,VRAM_SegaScr_Plane_A_Name_Table,VRAM_SegaScr_Plane_Table_Size ; clear Plane A pattern name table

	lea	ObjB1_Streak_fade_to_left(pc),a1
	; $49A00003; 9 full lines ($100 bytes each) plus $50 8-pixel cells
	move.l	#vdpComm(VRAM_SegaScr_Plane_A_Name_Table + planeLoc(128,80,9),VRAM,WRITE),d0
	bra.w	loc_3A710
loc_3A710:
	lea	(VDP_data_port).l,a6
	; This is the line delta; for each line, the code below
	; writes $30 entries, leaving $50 untouched.
	move.l	#vdpCommDelta(planeLoc(128,0,1)),d6	; $1000000
	moveq	#7,d1	; Inner loop: repeat 8 times
	moveq	#9,d2	; Outer loop: repeat $A times
-
	move.l	d0,4(a6)	; Send command to VDP: set address to write to
	move.w	d1,d3		; Reset inner loop counter
	movea.l	a1,a2		; Reset data pointer
-
	move.w	(a2)+,d4	; Read one pattern name table entry
	bclr	#$A,d4		; Test bit $A and clear (flag for end of line)
	beq.s	+			; Branch if bit was clear
	bsr.w	loc_3A742	; Fill rest of line with this set of pixels
+
	move.w	d4,(a6)		; Write PNT entry
	dbf	d3,-
	add.l	d6,d0		; Point to the next VRAM area to be written to
	dbf	d2,--
	rts
; ===========================================================================

loc_3A742:
	moveq	#$28,d5		; Fill next $29 entries...
-
	move.w	d4,(a6)		; ...using the PNT entry that had bit $A set
	dbf	d5,-
	rts
; ===========================================================================
; Pattern A name table entries, with special flag detailed below
; These are used for the streaks, and point to VRAM in the $1000-$10FF range
ObjB1_Streak_fade_to_right:
	dc.w make_block_tile(ArtTile_ArtNem_Trails+0,0,0,1,1)	; 0
	dc.w make_block_tile(ArtTile_ArtNem_Trails+1,0,0,1,1)	; 2
	dc.w make_block_tile(ArtTile_ArtNem_Trails+2,0,0,1,1)	; 4
	dc.w make_block_tile(ArtTile_ArtNem_Trails+3,0,0,1,1)	; 6
	dc.w make_block_tile(ArtTile_ArtNem_Trails+4,0,0,1,1)	; 8
	dc.w make_block_tile(ArtTile_ArtNem_Trails+5,0,0,1,1)	; 10
	dc.w make_block_tile(ArtTile_ArtNem_Trails+6,0,0,1,1)	; 12
	dc.w make_block_tile(ArtTile_ArtNem_Trails+7,0,0,1,1) | (1 << $A)	; 14	; Bit $A is used as a flag to use this tile $29 times
ObjB1_Streak_fade_to_left:
	dc.w make_block_tile(ArtTile_ArtNem_Trails+7,0,0,1,1) | (1 << $A)	;  0	; Bit $A is used as a flag to use this tile $29 times
	dc.w make_block_tile(ArtTile_ArtNem_Trails+6,0,0,1,1)	; 2
	dc.w make_block_tile(ArtTile_ArtNem_Trails+5,0,0,1,1)	; 4
	dc.w make_block_tile(ArtTile_ArtNem_Trails+4,0,0,1,1)	; 6
	dc.w make_block_tile(ArtTile_ArtNem_Trails+3,0,0,1,1)	; 8
	dc.w make_block_tile(ArtTile_ArtNem_Trails+2,0,0,1,1)	; 10
	dc.w make_block_tile(ArtTile_ArtNem_Trails+1,0,0,1,1)	; 12
	dc.w make_block_tile(ArtTile_ArtNem_Trails+0,0,0,1,1)	; 14
Streak_Horizontal_offsets:
	dc.b $12
	dc.b   4	; 1
	dc.b   4	; 2
	dc.b   2	; 3
	dc.b   2	; 4
	dc.b   2	; 5
	dc.b   2	; 6
	dc.b   0	; 7
	dc.b   0	; 8
	dc.b   0	; 9
	dc.b   0	; 10
	dc.b   0	; 11
	dc.b   0	; 12
	dc.b   0	; 13
	dc.b   0	; 14
	dc.b   4	; 15
	dc.b   4	; 16
	dc.b   6	; 17
	dc.b  $A	; 18
	dc.b   8	; 19
	dc.b   6	; 20
	dc.b   4	; 21
	dc.b   4	; 22
	dc.b   4	; 23
	dc.b   4	; 24
	dc.b   6	; 25
	dc.b   6	; 26
	dc.b   8	; 27
	dc.b   8	; 28
	dc.b  $A	; 29
	dc.b  $A	; 30
	dc.b  $C	; 31
	dc.b  $E	; 32
	dc.b $10	; 33
	dc.b $16	; 34
	dc.b   0	; 35
	even




; ===========================================================================