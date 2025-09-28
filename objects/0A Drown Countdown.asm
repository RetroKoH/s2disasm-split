; ----------------------------------------------------------------------------
; Object 0A - Small bubbles from Sonic's face while underwater
; ----------------------------------------------------------------------------
obj0a_time_until_freeze             = objoff_2C
obj0a_current_dplc                  = objoff_2E
obj0a_original_x_pos                = objoff_30
obj0a_seconds_between_numbers_timer = objoff_32
obj0a_seconds_between_numbers       = objoff_33
obj0a_total_bubbles_to_spawn        = objoff_34
obj0a_flags                         = objoff_36
obj0a_timer                         = objoff_38
obj0a_next_bubble_timer             = objoff_3A
;obj0a_character                    = objoff_3C

; Sprite_1D320:
Obj0A:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	Obj0A_Index(pc,d0.w),d1
	jmp	Obj0A_Index(pc,d1.w)
; ===========================================================================
; off_1D32E: Obj0A_States:
Obj0A_Index:	offsetTable
		offsetTableEntry.w Obj0A_Init		;   0
		offsetTableEntry.w Obj0A_Animate	;   2
		offsetTableEntry.w Obj0A_ChkWater	;   4
		offsetTableEntry.w Obj0A_Display	;   6
		offsetTableEntry.w JmpTo5_DeleteObject	;   8
		offsetTableEntry.w Obj0A_Countdown	;  $A
		offsetTableEntry.w Obj0A_AirLeft	;  $C
		offsetTableEntry.w Obj0A_DisplayNumber	;  $E
		offsetTableEntry.w JmpTo5_DeleteObject	; $10
; ===========================================================================
; loc_1D340: Obj0A_Main:
Obj0A_Init:
	addq.b	#2,routine(a0) ; Obj0A_Animate
	; Use different mappings depending on which player the bubbles
	; are coming from.
	move.l	#Obj24_MapUnc_1FBF6,mappings(a0)
	tst.b	obj0a_character+3(a0)
	beq.s	+
	move.l	#Obj24_MapUnc_1FC18,mappings(a0)
+
	move.w	#make_art_tile(ArtTile_ArtNem_BigBubbles,0,1),art_tile(a0)
	move.b	#$84,render_flags(a0)
	move.b	#16,width_pixels(a0)
	move.b	#1,priority(a0)
	move.b	subtype(a0),d0
	bpl.s	loc_1D388
	addq.b	#8,routine(a0) ; Obj0A_Countdown
	andi.w	#$7F,d0
	; Yes, this is actually configurable, but it is normally only ever
	; set to 2 seconds. The countdown starts at 12 seconds remaining, and
	; the numbers count from 5 to 0, so 2 seconds is ideal.
	move.b	d0,obj0a_seconds_between_numbers(a0)
	bra.w	Obj0A_Countdown
; ===========================================================================

loc_1D388:
	move.b	d0,anim(a0)
	move.w	x_pos(a0),obj0a_original_x_pos(a0)
	move.w	#-$88,y_vel(a0)

; loc_1D398:
Obj0A_Animate:
	lea	(Ani_obj0A).l,a1
	jsr	(AnimateSprite).l

; loc_1D3A4:
Obj0A_ChkWater:
	move.w	(Water_Level_1).w,d0
	cmp.w	y_pos(a0),d0		; has bubble reached the water surface?
	blo.s	Obj0A_Wobble		; if not, branch
	; pop the bubble:
	move.b	#6,routine(a0) ; Obj0A_Display
	addq.b	#7,anim(a0)
	cmpi.b	#$D,anim(a0)
	beq.s	Obj0A_Display
	blo.s	Obj0A_Display
	move.b	#$D,anim(a0)
	bra.s	Obj0A_Display
; ===========================================================================
; loc_1D3CA:
Obj0A_Wobble:
	; If in a wind-tunnel, then make the bubbles move to the right.
	tst.b	(WindTunnel_flag).w
	beq.s	+
	addq.w	#4,obj0a_original_x_pos(a0)
+
	; Wiggle the bubble left and right.
	move.b	angle(a0),d0
	addq.b	#1,angle(a0)
	andi.w	#$7F,d0
	lea	(Obj0A_WobbleData).l,a1
	move.b	(a1,d0.w),d0
	ext.w	d0
	add.w	obj0a_original_x_pos(a0),d0
	move.w	d0,x_pos(a0)

    if fixBugs
	; This isn't actually a bugfix: it's just that a later bugfix pushes
	; this call out of range, so it has to be extended to a word.
	bsr.w	Obj0A_BecomeNumberMaybe
    else
	bsr.s	Obj0A_BecomeNumberMaybe
    endif
	jsr	(ObjectMove).l
	tst.b	render_flags(a0)
	bpl.s	JmpTo4_DeleteObject
	jmp	(DisplaySprite).l
; ===========================================================================

JmpTo4_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
; ===========================================================================
; loc_1D40E:
Obj0A_DisplayNumber:
	movea.l	obj0a_character(a0),a2 ; a2=character
	cmpi.b	#12,air_left(a2)
	bhi.s	JmpTo5_DeleteObject

; loc_1D41A:
Obj0A_Display:
	bsr.s	Obj0A_BecomeNumberMaybe
	lea	(Ani_obj0A).l,a1
	jsr	(AnimateSprite).l
    if fixBugs
	; If you stand in very shallow water and begin drowning, the
	; countdown graphics will appear incorrectly. The cause is a missing
	; call to 'Obj0A_LoadCountdownArt'.
	bsr.w	Obj0A_LoadCountdownArt
    endif
	jmp	(DisplaySprite).l
; ===========================================================================

JmpTo5_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
; ===========================================================================
; loc_1D434:
Obj0A_AirLeft:
	movea.l	obj0a_character(a0),a2 ; a2=character
	cmpi.b	#12,air_left(a2)	; check air remaining
	bhi.s	JmpTo6_DeleteObject	; if higher than $C, branch
	subq.w	#1,obj0a_timer(a0)
	bne.s	Obj0A_Display2
	move.b	#$E,routine(a0) ; Obj0A_DisplayNumber
	addq.b	#7,anim(a0)
	bra.s	Obj0A_Display
; ===========================================================================
; loc_1D452:
Obj0A_Display2:
	lea	(Ani_obj0A).l,a1
	jsr	(AnimateSprite).l
	bsr.w	Obj0A_LoadCountdownArt
	tst.b	render_flags(a0)
	bpl.s	JmpTo6_DeleteObject
	jmp	(DisplaySprite).l
; ===========================================================================

JmpTo6_DeleteObject ; JmpTo
	jmp	(DeleteObject).l
; ===========================================================================
; loc_1D474: Obj0A_ShowNumber:
Obj0A_BecomeNumberMaybe:
	tst.w	obj0a_timer(a0)
	beq.s	return_1D4BE
	subq.w	#1,obj0a_timer(a0)
	bne.s	return_1D4BE
	cmpi.b	#7,anim(a0)
	bhs.s	return_1D4BE

	; Turn this bubble into a number.
	move.w	#15,obj0a_timer(a0)
	clr.w	y_vel(a0)
	move.b	#$80,render_flags(a0)

	move.w	x_pos(a0),d0
	sub.w	(Camera_X_pos).w,d0
	addi.w	#$80,d0
	move.w	d0,x_pixel(a0)

	move.w	y_pos(a0),d0
	sub.w	(Camera_Y_pos).w,d0
	addi.w	#$80,d0
	move.w	d0,y_pixel(a0)

	move.b	#$C,routine(a0) ; Obj0A_AirLeft

return_1D4BE:
	rts
; ===========================================================================
; byte_1D4C0:
Obj0A_WobbleData:
	dc.b  0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2;16
	dc.b  2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3;32
	dc.b  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2;48
	dc.b  2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0;64
	dc.b  0,-1,-1,-1,-1,-1,-2,-2,-2,-2,-2,-3,-3,-3,-3,-3;80
	dc.b -3,-3,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4;96
	dc.b -4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-3;112
	dc.b -3,-3,-3,-3,-3,-3,-2,-2,-2,-2,-2,-1,-1,-1,-1,-1;128

	; Unused leftover from Sonic 1.
	; This was used by Labyrinth Zone's water ripple effect in REV01.
	dc.b  0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2;144
	dc.b  2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3;160
	dc.b  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2;176
	dc.b  2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0;192
	dc.b  0,-1,-1,-1,-1,-1,-2,-2,-2,-2,-2,-3,-3,-3,-3,-3;208
	dc.b -3,-3,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4;224
	dc.b -4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-3;240
	dc.b -3,-3,-3,-3,-3,-3,-2,-2,-2,-2,-2,-1,-1,-1,-1,-1;256
; ===========================================================================
; the countdown numbers go over the dust and splash effect tiles in VRAM
; loc_1D5C0:
Obj0A_LoadCountdownArt:
	moveq	#0,d1
	move.b	mapping_frame(a0),d1
	cmpi.b	#8,d1
	blo.s	return_1D604
	cmpi.b	#$E,d1
	bhs.s	return_1D604
	cmp.b	obj0a_current_dplc(a0),d1
	beq.s	return_1D604
	move.b	d1,obj0a_current_dplc(a0)
	subq.w	#8,d1
	move.w	d1,d0
	add.w	d1,d1
	add.w	d0,d1
	lsl.w	#6,d1
	addi.l	#ArtUnc_Countdown,d1
	move.w	#tiles_to_bytes(ArtTile_ArtNem_SonicDust),d2
	tst.b	obj0a_character+3(a0)
	beq.s	+
	move.w	#tiles_to_bytes(ArtTile_ArtNem_TailsDust),d2
+
	move.w	#tiles_to_bytes(6)/2,d3	; DMA transfer length (in words)
	jsr	(QueueDMATransfer).l

return_1D604:
	rts
; ===========================================================================

; loc_1D606:
Obj0A_Countdown:
	movea.l	obj0a_character(a0),a2 ; a2=character

	; If the player has drowned, and the object is waiting until the
	; world should pause, then go deal with that.
	tst.w	obj0a_time_until_freeze(a0)
	bne.w	Obj0A_PlayerHasDrowned

	cmpi.b	#6,routine(a2) ; If player is dead, return.
	bhs.w	return_1D81C
	btst	#6,status(a2)
	beq.w	return_1D81C

	; Wait a second.
	subq.w	#1,obj0a_timer(a0)
	bpl.w	Obj0A_MakeBubbleMaybe
	move.w	#60-1,obj0a_timer(a0)

	move.w	#1,obj0a_flags(a0)

	; Randomly spawn either one or two bubbles.
	jsr	(RandomNumber).l
	andi.w	#1,d0
	move.b	d0,obj0a_total_bubbles_to_spawn(a0)

	moveq	#0,d0
	move.b	air_left(a2),d0	; check air remaining
	cmpi.w	#25,d0
	beq.s	Obj0A_WarnSound	; play ding sound when there are 25 seconds left
	cmpi.w	#20,d0
	beq.s	Obj0A_WarnSound	; play ding sound when there are 20 seconds left
	cmpi.w	#15,d0
	beq.s	Obj0A_WarnSound	; play ding sound when there are 15 seconds left
	cmpi.w	#12,d0
	bhi.s	Obj0A_ReduceAir	; play drowning theme when there are 12 seconds left
	bne.s	+
	; Play countdown music if this is player 1.
	tst.b	obj0a_character+3(a0)
	bne.s	+
	move.w	#MusID_Countdown,d0
	jsr	(PlayMusic).l
+
	subq.b	#1,obj0a_seconds_between_numbers_timer(a0)
	bpl.s	Obj0A_ReduceAir
	move.b	obj0a_seconds_between_numbers(a0),obj0a_seconds_between_numbers_timer(a0)
	; Set the flag to create a number.
	bset	#7,obj0a_flags(a0)
	bra.s	Obj0A_ReduceAir
; ===========================================================================
; loc_1D68C:
Obj0A_WarnSound:
	; If this is player 1, then play the "ding-ding" warning sound.
	tst.b	obj0a_character+3(a0)
	bne.s	Obj0A_ReduceAir
	move.w	#SndID_WaterWarning,d0
	jsr	(PlaySound).l

; loc_1D69C:
Obj0A_ReduceAir:
	subq.b	#1,air_left(a2)		; subtract 1 from air remaining
	bcc.w	BranchTo_Obj0A_MakeBubbleNow	; if air is above 0, branch
	; Drown the player.
	move.b	#$81,obj_control(a2)	; lock controls
	move.w	#SndID_Drown,d0
	jsr	(PlaySound).l		; play drowning sound
	move.b	#10,obj0a_total_bubbles_to_spawn(a0) ; spawn ten bubbles
	move.w	#1,obj0a_flags(a0)
	move.w	#60*2,obj0a_time_until_freeze(a0) ; two seconds until the world pauses
	movea.l	a2,a1
	bsr.w	ResumeMusic
	move.l	a0,-(sp)
	movea.l	a2,a0
	bsr.w	Sonic_ResetOnFloor_Part2
	move.b	#AniIDSonAni_Drown,anim(a0)	; use Sonic's drowning animation
	bset	#1,status(a0)
	bset	#high_priority_bit,art_tile(a0)
	move.w	#0,y_vel(a0)
	move.w	#0,x_vel(a0)
	move.w	#0,inertia(a0)
	movea.l	(sp)+,a0 ; load 0bj address ; restore a0 = obj0A
	cmpa.w	#MainCharacter,a2
	bne.s	+	; if it isn't player 1, branch
	move.b	#1,(Deform_lock).w
+
	rts
; ===========================================================================
; loc_1D708:
Obj0A_PlayerHasDrowned:
	subq.w	#1,obj0a_time_until_freeze(a0)
	bne.s	+
	; Signal that the player is dead.
	move.b	#6,routine(a2)
	rts
; ---------------------------------------------------------------------------
+	; Move the player downwards as they drown.
	move.l	a0,-(sp)
	movea.l	a2,a0
	jsr	(ObjectMove).l
	addi.w	#$10,y_vel(a0)
	movea.l	(sp)+,a0 ; load 0bj address
	bra.s	Obj0A_MakeBubbleMaybe
; ===========================================================================
; BranchTo_Obj0A_MakeItem
BranchTo_Obj0A_MakeBubbleNow ; BranchTo
	bra.s	Obj0A_MakeBubbleNow
; ===========================================================================
;loc_1D72C:
Obj0A_MakeBubbleMaybe:
	tst.w	obj0a_flags(a0)
	beq.w	return_1D81C
	subq.w	#1,obj0a_next_bubble_timer(a0)
	bpl.w	return_1D81C

; loc_1D73C: Obj0A_MakeItem:
Obj0A_MakeBubbleNow:
	jsr	(RandomNumber).l
	andi.w	#$F,d0
	addq.w	#8,d0
	move.w	d0,obj0a_next_bubble_timer(a0)

	jsr	(AllocateObject).l
	bne.w	return_1D81C
	_move.b	id(a0),id(a1)		; load obj0A
	move.w	x_pos(a2),x_pos(a1)	; match its X position to Sonic
	moveq	#6,d0
	btst	#0,status(a2)
	beq.s	+
	neg.w	d0
	move.b	#$40,angle(a1)
+
	add.w	d0,x_pos(a1)
	move.w	y_pos(a2),y_pos(a1)
	move.l	obj0a_character(a0),obj0a_character(a1)
	move.b	#6,subtype(a1)	; Small bubble?

	tst.w	obj0a_time_until_freeze(a0)
	beq.w	Obj0A_MakeNumberBubbleMaybe

	; The player has drowned.

	; Shorten bubble timer, to make bubbles spawn faster.
	andi.w	#7,obj0a_next_bubble_timer(a0)
	addi.w	#0,obj0a_next_bubble_timer(a0)	; Pointless

	move.w	y_pos(a2),d0
	subi.w	#12,d0
	move.w	d0,y_pos(a1)

	jsr	(RandomNumber).l
	move.b	d0,angle(a1)

	move.w	(Level_frame_counter).w,d0
	andi.b	#3,d0
	bne.s	Obj0A_DoneCreatingBubble

	move.b	#$E,subtype(a1)	; Big bubble?
	bra.s	Obj0A_DoneCreatingBubble
; ---------------------------------------------------------------------------
; loc_1D7C6:
Obj0A_MakeNumberBubbleMaybe:
	; The player has not drowned.

	; If it's not time to create a number bubble, then skip this.
	btst	#7,obj0a_flags(a0)
	beq.s	Obj0A_DoneCreatingBubble

	moveq	#0,d2
	move.b	air_left(a2),d2
	cmpi.b	#12,d2
	bhs.s	Obj0A_DoneCreatingBubble

	; This player is about to drown.
	lsr.w	#1,d2
	jsr	(RandomNumber).l
	andi.w	#3,d0
	bne.s	+
	bset	#6,obj0a_flags(a0) ; This flag prevents more than one number bubble from spawning at once.
	bne.s	Obj0A_DoneCreatingBubble
	move.b	d2,subtype(a1)
	move.w	#28,obj0a_timer(a1) ; Make this bubble turn into a number later.
+
	tst.b	obj0a_total_bubbles_to_spawn(a0)
	bne.s	Obj0A_DoneCreatingBubble
	bset	#6,obj0a_flags(a0) ; This flag prevents more than one number bubble from spawning at once.
	bne.s	Obj0A_DoneCreatingBubble
	move.b	d2,subtype(a1)
	move.w	#28,obj0a_timer(a1) ; Make this bubble turn into a number later.
; loc_1D812:
Obj0A_DoneCreatingBubble:
	subq.b	#1,obj0a_total_bubbles_to_spawn(a0)
	bpl.s	return_1D81C
	; Don't spawn any more bubbles.
	clr.w	obj0a_flags(a0)

return_1D81C:
	rts
; ===========================================================================

; ---------------------------------------------------------------------------
; Subroutine to play music after a countdown (when Sonic leaves the water)
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_1D81E:
ResumeMusic:
	cmpi.b	#12,air_left(a1)
	bhi.s	ResumeMusic_Done	; branch if countdown hasn't started yet

	cmpa.w	#MainCharacter,a1
	bne.s	ResumeMusic_Done	; branch if it isn't player 1

	move.w	(Level_Music).w,d0	; prepare to play current level's music

	btst	#status_sec_isInvincible,status_secondary(a1)
	beq.s	+		; branch if Sonic is not invincible
	move.w	#MusID_Invincible,d0	; prepare to play invincibility music
+
	tst.b	(Super_Sonic_flag).w
	beq.w	+		; branch if it isn't Super Sonic
	move.w	#MusID_SuperSonic,d0	; prepare to play Super Sonic music
+
	tst.b	(Current_Boss_ID).w
	beq.s	+		; branch if not in a boss fight
	move.w	#MusID_Boss,d0	; prepare to play boss music
+
	jsr	(PlayMusic).l
; return_1D858:
ResumeMusic_Done:
	move.b	#30,air_left(a1)	; reset air to full
	rts

; ===========================================================================
; animation script for the bubbles
; off_1D860:
Ani_obj0A:	offsetTable
		offsetTableEntry.w byte_1D87E	;  0
		offsetTableEntry.w byte_1D887	;  1
		offsetTableEntry.w byte_1D890	;  2
		offsetTableEntry.w byte_1D899	;  3
		offsetTableEntry.w byte_1D8A2	;  4
		offsetTableEntry.w byte_1D8AB	;  5
		offsetTableEntry.w byte_1D8B4	;  6
		offsetTableEntry.w byte_1D8B9	;  7
		offsetTableEntry.w byte_1D8C1	;  8
		offsetTableEntry.w byte_1D8C9	;  9
		offsetTableEntry.w byte_1D8D1	; $A
		offsetTableEntry.w byte_1D8D9	; $B
		offsetTableEntry.w byte_1D8E1	; $C
		offsetTableEntry.w byte_1D8E9	; $D
		offsetTableEntry.w byte_1D8EB	; $E
byte_1D87E:	dc.b   5,  0,  1,  2,  3,  4,  8,  8,$FC
	rev02even
byte_1D887:	dc.b   5,  0,  1,  2,  3,  4,  9,  9,$FC
	rev02even
byte_1D890:	dc.b   5,  0,  1,  2,  3,  4, $A, $A,$FC
	rev02even
byte_1D899:	dc.b   5,  0,  1,  2,  3,  4, $B, $B,$FC
	rev02even
byte_1D8A2:	dc.b   5,  0,  1,  2,  3,  4, $C, $C,$FC
	rev02even
byte_1D8AB:	dc.b   5,  0,  1,  2,  3,  4, $D, $D,$FC
	rev02even
byte_1D8B4:	dc.b  $E,  0,  1,  2,$FC
	rev02even
byte_1D8B9:	dc.b   7,$10,  8,$10,  8,$10,  8,$FC
	rev02even
byte_1D8C1:	dc.b   7,$10,  9,$10,  9,$10,  9,$FC
	rev02even
byte_1D8C9:	dc.b   7,$10, $A,$10, $A,$10, $A,$FC
	rev02even
byte_1D8D1:	dc.b   7,$10, $B,$10, $B,$10, $B,$FC
	rev02even
byte_1D8D9:	dc.b   7,$10, $C,$10, $C,$10, $C,$FC
	rev02even
byte_1D8E1:	dc.b   7,$10, $D,$10, $D,$10, $D,$FC
	rev02even
byte_1D8E9:	dc.b  $E,$FC
	rev02even
byte_1D8EB:	dc.b  $E,  1,  2,  3,  4,$FC
	even
; ===========================================================================