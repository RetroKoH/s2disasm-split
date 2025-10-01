; ---------------------------------------------------------------------------
; Objects Manager
; Subroutine that keeps track of any objects that need to remember
; their state, such as monitors or enemies.
;
; input variables:
;  -none-
;
; writes:
;  d0, d1
;  d2 = respawn index of object to load
;  d6 = camera position
;
;  a0 = address in object placement list
;  a2 = respawn table
; ---------------------------------------------------------------------------

; loc_17AA4
ObjectsManager:
	moveq	#0,d0
	move.b	(Obj_placement_routine).w,d0
	move.w	ObjectsManager_States(pc,d0.w),d0
	jmp	ObjectsManager_States(pc,d0.w)
; ===========================================================================
ObjectsManager_States: offsetTable
	offsetTableEntry.w ObjectsManager_Init		; 0
	offsetTableEntry.w ObjectsManager_Main		; 2
	offsetTableEntry.w ObjectsManager_2P_Main	; 4
; ===========================================================================
; loc_17AB8
ObjectsManager_Init:
	addq.b	#2,(Obj_placement_routine).w
	move.w	(Current_ZoneAndAct).w,d0 ; If level == $0F01 (ARZ 2)...
	ror.b	#1,d0			; then this yields $0F80...
	lsr.w	#6,d0			; and this yields $003E.
	lea	(Off_Objects).l,a0	; Next, we load the first pointer in the object layout list pointer index,
	movea.l	a0,a1			; then copy it for quicker use later.
	adda.w	(a0,d0.w),a0		; (Point1 * 2) + $003E
	tst.w	(Two_player_mode).w	; skip if not in 2-player vs mode
	beq.s	+
	cmpi.b	#casino_night_zone,(Current_Zone).w	; skip if not Casino Night Zone
	bne.s	+
	lea	(Objects_CNZ1_2P).l,a0	; CNZ 1 2-player object layout
	tst.b	(Current_Act).w		; skip if not past act 1
	beq.s	+
	lea	(Objects_CNZ2_2P).l,a0	; CNZ 2 2-player object layout
+
	; initialize each object load address with the first object in the layout
	move.l	a0,(Obj_load_addr_right).w
	move.l	a0,(Obj_load_addr_left).w
	move.l	a0,(Obj_load_addr_right_P2).w
	move.l	a0,(Obj_load_addr_left_P2).w
	lea	(Object_Respawn_Table).w,a2
	move.w	#$0101,(a2)+	; the first two bytes are not used as respawn values
	; instead, they are used to keep track of the current respawn indexes

    if fixBugs
	move.w	#bytesToLcnt(Obj_respawn_data_End-Obj_respawn_data),d0 ; set loop counter
    else
	; This clears longwords, but the loop counter is measured in words!
	; This causes $17C bytes to be cleared instead of $BE.
	move.w	#bytesToWcnt(Obj_respawn_data_End-Obj_respawn_data),d0 ; set loop counter
    endif

-	clr.l	(a2)+		; loop clears all other respawn values
	dbf	d0,-

    if fixBugs
	; Clear the last word, since the above loop only does longwords.
    if (Obj_respawn_data_End-Obj_respawn_data)&2
	clr.w	(a2)+
    endif
    endif

	lea	(Obj_respawn_index).w,a2	; reset a2
	moveq	#0,d2
	move.w	(Camera_X_pos).w,d6
	subi.w	#$80,d6	; look one chunk to the left
	bcc.s	+	; if the result was negative,
	moveq	#0,d6	; cap at zero
+
	andi.w	#$FF80,d6	; limit to increments of $80 (width of a chunk)
	movea.l	(Obj_load_addr_right).w,a0	; load address of object placement list

-	; at the beginning of a level this gives respawn table entries to any object that is one chunk
	; behind the left edge of the screen that needs to remember its state (Monitors, Badniks, etc.)
	cmp.w	(a0),d6		; is object's x position >= d6?
	bls.s	loc_17B3E	; if yes, branch
	tst.b	2(a0)	; does the object get a respawn table entry?
	bpl.s	+	; if not, branch
	move.b	(a2),d2
	addq.b	#1,(a2)	; respawn index of next object to the right
+
	addq.w	#6,a0	; next object
	bra.s	-
; ---------------------------------------------------------------------------

loc_17B3E:
	move.l	a0,(Obj_load_addr_right).w	; remember rightmost object that has been processed, so far (we still need to look forward)
	move.l	a0,(Obj_load_addr_right_P2).w
	movea.l	(Obj_load_addr_left).w,a0	; reset a0
	subi.w	#$80,d6		; look even farther left (any object behind this is out of range)
	bcs.s	loc_17B62	; branch, if camera position would be behind level's left boundary

-	; count how many objects are behind the screen that are not in range and need to remember their state
	cmp.w	(a0),d6		; is object's x position >= d6?
	bls.s	loc_17B62	; if yes, branch
	tst.b	2(a0)	; does the object get a respawn table entry?
	bpl.s	+	; if not, branch
	addq.b	#1,1(a2)	; respawn index of current object to the left

+
	addq.w	#6,a0
	bra.s	-	; continue with next object
; ---------------------------------------------------------------------------

loc_17B62:
	move.l	a0,(Obj_load_addr_left).w	; remember current object from the left
	move.l	a0,(Obj_load_addr_left_P2).w
	move.w	#-1,(Camera_X_pos_last).w	; make sure ObjectsManager_GoingForward is run
	move.w	#-1,(Camera_X_pos_last_P2).w
	tst.w	(Two_player_mode).w	; is it two player mode?
	beq.s	ObjectsManager_Main	; if not, branch
	addq.b	#2,(Obj_placement_routine).w
	bra.w	ObjectsManager_2P_Init
; ---------------------------------------------------------------------------
; loc_17B84
ObjectsManager_Main:
	move.w	(Camera_X_pos).w,d1
	subi.w	#$80,d1
	andi.w	#$FF80,d1
	move.w	d1,(Camera_X_pos_coarse).w

	lea	(Obj_respawn_index).w,a2
	moveq	#0,d2
	move.w	(Camera_X_pos).w,d6
	andi.w	#$FF80,d6
	cmp.w	(Camera_X_pos_last).w,d6	; is the X range the same as last time?
	beq.w	ObjectsManager_SameXRange	; if yes, branch (rts)
	bge.s	ObjectsManager_GoingForward	; if new pos is greater than old pos, branch

	; if the player is moving back
;ObjectsManager_GoingBackward:
	move.w	d6,(Camera_X_pos_last).w	; remember current position for next time

	movea.l	(Obj_load_addr_left).w,a0	; get current object from the left
	subi.w	#$80,d6		; look one chunk to the left
	bcs.s	.done1		; branch, if camera position would be behind level's left boundary

.nextObject1:
	; load all objects left of the screen that are now in range
	cmp.w	-6(a0),d6	; is the previous object's X pos less than d6?
	bge.s	.done1		; if it is, branch
	subq.w	#6,a0		; get object's address
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn1	; if not, branch
	subq.b	#1,1(a2)	; respawn index of this object
	move.b	1(a2),d2
.noRespawn1:
	bsr.w	ChkLoadObj	; load object
	bne.s	.fullSST	; branch, if SST is full
	subq.w	#6,a0
	bra.s	.nextObject1	; continue with previous object
; ---------------------------------------------------------------------------

.fullSST:
	; undo a few things, if the object couldn't load
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn3	; if not, branch
	addq.b	#1,1(a2)	; since we didn't load the object, undo last change
.noRespawn3:
	addq.w	#6,a0		; go back to last object
; loc_17BE6:
.done1:
	move.l	a0,(Obj_load_addr_left).w	; remember current object from the left

	movea.l	(Obj_load_addr_right).w,a0	; get next object from the right
	addi.w	#$300,d6			; look two chunks beyond the right edge of the screen

.nextObject2:
	; subtract number of objects that have been moved out of range (from the right side)
	cmp.w	-6(a0),d6	; is the previous object's X pos less than d6?
	bgt.s	.done2		; if it is, branch
	tst.b	-4(a0)		; does the previous object get a respawn table entry?
	bpl.s	.noRespawn2	; if not, branch
	subq.b	#1,(a2)		; respawn index of next object to the right
.noRespawn2:
	subq.w	#6,a0
	bra.s	.nextObject2	; continue with previous object
; ---------------------------------------------------------------------------
; loc_17C04:
.done2:
	move.l	a0,(Obj_load_addr_right).w	; remember next object from the right
	rts
; ---------------------------------------------------------------------------

ObjectsManager_GoingForward:
	move.w	d6,(Camera_X_pos_last).w

	movea.l	(Obj_load_addr_right).w,a0	; get next object from the right
	addi.w	#$280,d6			; look two chunks forward

.nextObject1:
	; load all objects right of the screen that are now in range
	cmp.w	(a0),d6		; is object's x position >= d6?
	bls.s	.done1		; if yes, branch
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn1	; if not, branch
	move.b	(a2),d2		; respawn index of this object
	addq.b	#1,(a2)		; respawn index of next object to the right
.noRespawn1:
	bsr.w	ChkLoadObj	; load object (and get address of next object)
	beq.s	.nextObject1	; continue loading objects, if the SST isn't full
; loc_17C2A:
.done1:
	move.l	a0,(Obj_load_addr_right).w	; remember next object from the right

	movea.l	(Obj_load_addr_left).w,a0	; get current object from the left
	subi.w	#$300,d6			; look one chunk behind the left edge of the screen
	bcs.s	.done2				; branch, if camera position would be behind level's left boundary

.nextObject2:
	; subtract number of objects that have been moved out of range (from the left)
	cmp.w	(a0),d6		; is object's x position >= d6?
	bls.s	.done2		; if yes, branch
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn2	; if not, branch
	addq.b	#1,1(a2)	; respawn index of next object to the left
.noRespawn2:
	addq.w	#6,a0
	bra.s	.nextObject2	; continue with previous object
; ---------------------------------------------------------------------------
; loc_17C4A:
.done2:
	move.l	a0,(Obj_load_addr_left).w	; remember current object from the left

ObjectsManager_SameXRange:
	rts
; ---------------------------------------------------------------------------
; loc_17C50
ObjectsManager_2P_Init:
	; Reset all of the 2P object manager variables to $FF.
	moveq	#-1,d0

	; Some code to generate an unrolled loop of instructions which clear
	; the 2P object manager variables.
.c := 0
    rept (Object_manager_2P_RAM_End-Object_manager_2P_RAM)/4
	move.l	d0,(Object_manager_2P_RAM+.c).w
.c := .c+4
    endm

    if (Object_manager_2P_RAM_End-Object_manager_2P_RAM)&2
	move.w	d0,(Object_manager_2P_RAM+.c).w
.c := .c+2
    endif

    if (Object_manager_2P_RAM_End-Object_manager_2P_RAM)&1
	move.b	d0,(Object_manager_2P_RAM+.c).w
    endif

	move.w	#0,(Camera_X_pos_last).w
	move.w	#0,(Camera_X_pos_last_P2).w
	lea	(Obj_respawn_index).w,a2
	move.w	(a2),(Obj_respawn_index_P2).w	; mirrior first two bytes (respawn indices) for player 2(?)
	moveq	#0,d2
	; run initialization for player 1
	lea	(Obj_respawn_index).w,a5
	lea	(Object_Manager_Addresses).w,a4
	lea	(Player_1_loaded_object_blocks).w,a1	; = -1, -1, -1
	lea	(Player_2_loaded_object_blocks).w,a6	; = -1, -1, -1
	moveq	#-2,d6
	bsr.w	ObjMan2P_GoingForward
	lea	(Player_1_loaded_object_blocks).w,a1
	moveq	#-1,d6
	bsr.w	ObjMan2P_GoingForward
	lea	(Player_1_loaded_object_blocks).w,a1
	moveq	#0,d6
	bsr.w	ObjMan2P_GoingForward
	; run initialization for player 2
	lea	(Obj_respawn_index_P2).w,a5
	lea	(Object_Manager_Addresses_P2).w,a4
	lea	(Player_2_loaded_object_blocks).w,a1
	lea	(Player_1_loaded_object_blocks).w,a6
	moveq	#-2,d6
	bsr.w	ObjMan2P_GoingForward
	lea	(Player_2_loaded_object_blocks).w,a1
	moveq	#-1,d6
	bsr.w	ObjMan2P_GoingForward
	lea	(Player_2_loaded_object_blocks).w,a1
	moveq	#0,d6
	bsr.w	ObjMan2P_GoingForward

; loc_17CCC
ObjectsManager_2P_Main:
	move.w	(Camera_X_pos).w,d1
	andi.w	#$FF00,d1
	move.w	d1,(Camera_X_pos_coarse).w

	move.w	(Camera_X_pos_P2).w,d1
	andi.w	#$FF00,d1
	move.w	d1,(Camera_X_pos_coarse_P2).w

	move.b	(Camera_X_pos).w,d6	; get upper byte of camera positon
	andi.w	#$FF,d6
	move.w	(Camera_X_pos_last).w,d0
	cmp.w	(Camera_X_pos_last).w,d6	; is the X range the same as last time?
	beq.s	+				; if yes, branch
	move.w	d6,(Camera_X_pos_last).w	; remember current position for next time
	lea	(Obj_respawn_index).w,a5
	lea	(Object_Manager_Addresses).w,a4
	lea	(Player_1_loaded_object_blocks).w,a1
	lea	(Player_2_loaded_object_blocks).w,a6
	bsr.s	ObjectsManager_2P_Run
+
	move.b	(Camera_X_pos_P2).w,d6	; get upper byte of camera positon
	andi.w	#$FF,d6
	move.w	(Camera_X_pos_last_P2).w,d0
	cmp.w	(Camera_X_pos_last_P2).w,d6	; is the X range the same as last time?
	beq.s	return_17D34			; if yes, branch (rts)
	move.w	d6,(Camera_X_pos_last_P2).w
	lea	(Obj_respawn_index_P2).w,a5
	lea	(Object_Manager_Addresses_P2).w,a4
	lea	(Player_2_loaded_object_blocks).w,a1
	lea	(Player_1_loaded_object_blocks).w,a6
	bsr.s	ObjectsManager_2P_Run

return_17D34:
	rts
; ===========================================================================

ObjectsManager_2P_Run:
	lea	(Obj_respawn_index).w,a2
	moveq	#0,d2
	cmp.w	d0,d6				; is the X range the same as last time?
	beq.w	ObjectsManager_SameXRange	; if yes, branch (rts)
	bge.w	ObjMan2P_GoingForward	; if new pos is greater than old pos, branch
	; if the player is moving back

;ObjMan2P_GoingBackward:
	; Slide the object block indices to the right, and insert the new object block at the left.
	move.b	2(a1),d2
	move.b	1(a1),2(a1)
	move.b	(a1),1(a1)
	move.b	d6,(a1)
	; d2 now hold the index of the object block to be unloaded, which was pushed out of the right side.

	; Check if the other player has the to-be-unloaded object block loaded.
	cmp.b	(a6),d2
	beq.s	.blockNeededByOtherPlayer
	cmp.b	1(a6),d2
	beq.s	.blockNeededByOtherPlayer
	cmp.b	2(a6),d2
	beq.s	.blockNeededByOtherPlayer
	; If the other player does not have this object block loaded, then we're free to unload it.
	bsr.w	ObjectsManager_2P_UnloadObjectBlock
	bra.s	.haveEmptyObjectBlock
; ---------------------------------------------------------------------------

.blockNeededByOtherPlayer:
	bsr.w	ObjectsManager_2P_FindEmptyObjectBlock
; loc_17D70:
.haveEmptyObjectBlock:
	bsr.w	ObjectsManager_2P_IsObjectBlockAlreadyLoaded
	bne.s	.blockNotAlreadyLoaded

	; Block is already loaded: just update the pointer and respawn index without actually loading anything.
	movea.l	4(a4),a0

.nextObject1:
	cmp.b	-6(a0),d6	; is the previous object's X pos less than d6?
	bne.s	.done1		; if it is, branch
	tst.b	-4(a0)		; does the previous object get a respawn table entry?
	bpl.s	.noRespawn1	; if not, branch
	subq.b	#1,1(a5)	; respawn index of next object to the left
.noRespawn1:
	subq.w	#6,a0
	bra.s	.nextObject1	; continue with previous object
; ---------------------------------------------------------------------------
; loc_17D8E:
.done1:
	move.l	a0,4(a4)	; remember next object from the right

	bra.s	.unloadObjects
; ---------------------------------------------------------------------------
; loc_17D94:
.blockNotAlreadyLoaded:
	; Block is not already loaded: load all of the objects in the block.
	movea.l	4(a4),a0

	; Mark object block as occupied.
	move.b	d6,(a1)

.nextObject2:
	; load all objects left of the screen that are now in range
	cmp.b	-6(a0),d6	; is the previous object's X pos less than d6?
	bne.s	.done2		; if it is, branch
	subq.w	#6,a0		; get object's address
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn2	; if not, branch
	subq.b	#1,1(a5)	; respawn index of this object
	move.b	1(a5),d2
.noRespawn2:
	bsr.w	ChkLoadObj_2P	; load object
	bne.s	.fullSST	; branch, if SST is full
	subq.w	#6,a0
	bra.s	.nextObject2	; continue with previous object
; ---------------------------------------------------------------------------
; loc_17DBA:
.fullSST:
	; undo a few things, if the object couldn't load
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn4	; if not, branch
	addq.b	#1,1(a5)	; since we didn't load the object, undo last change
.noRespawn4:
	addq.w	#6,a0		; go back to last object
; loc_17DC6:
.done2:
	move.l	a0,4(a4)	; remember current object from the left
; loc_17DCA:
.unloadObjects:
	movea.l	(a4),a0		; get next object from the right
	addq.w	#3,d6		; look two chunks beyond the right edge of the screen

.nextObject3:
	; subtract number of objects that have been moved out of range (from the right side)
	cmp.b	-6(a0),d6	; is the previous object's X pos less than d6?
	bne.s	.done3		; if it is, branch
	tst.b	-4(a0)		; does the previous object get a respawn table entry?
	bpl.s	.noRespawn3	; if not, branch
	subq.b	#1,(a5)		; respawn index of next object to the left
.noRespawn3:
	subq.w	#6,a0
	bra.s	.nextObject3	; continue with previous object
; ---------------------------------------------------------------------------
; loc_17DE0:
.done3:
	move.l	a0,(a4)		; remember next object from the right
	rts
; ===========================================================================
;loc_17DE4:
ObjMan2P_GoingForward:
	addq.w	#2,d6		; look forward two chunks

	; Slide the object block indices to the left, and insert the new object block at the right.
	move.b	(a1),d2
	move.b	1(a1),(a1)
	move.b	2(a1),1(a1)
	move.b	d6,2(a1)
	; d2 now hold the index of the object block to be unloaded, which was pushed out of the right side.

	; Check if the other player has the to-be-unloaded object block loaded.
	cmp.b	(a6),d2
	beq.s	.blockNeededByOtherPlayer
	cmp.b	1(a6),d2
	beq.s	.blockNeededByOtherPlayer
	cmp.b	2(a6),d2
	beq.s	.blockNeededByOtherPlayer
	; If the other player does not have this object block loaded, then we're free to unload it.
	bsr.w	ObjectsManager_2P_UnloadObjectBlock
	bra.s	.haveEmptyObjectBlock
; ---------------------------------------------------------------------------

.blockNeededByOtherPlayer:
	bsr.w	ObjectsManager_2P_FindEmptyObjectBlock
; loc_17E10:
.haveEmptyObjectBlock:
	bsr.w	ObjectsManager_2P_IsObjectBlockAlreadyLoaded
	bne.s	.blockNotAlreadyLoaded

	; Block is already loaded: just update the pointer and respawn index without actually loading anything.
	movea.l	(a4),a0

.nextObject1:
	cmp.b	(a0),d6		; is the object's X pos greater than d6?
	bne.s	.done1		; if it is, branch
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn1	; if not, branch
	addq.b	#1,(a5)		; respawn index of next object to the right
.noRespawn1:
	addq.w	#6,a0
	bra.s	.nextObject1	; continue with next object
; ===========================================================================
; loc_17E28:
.done1:
	move.l	a0,(a4)		; remember next object from the right

	bra.s	.unloadObjects
; ===========================================================================
; loc_17E2C:
.blockNotAlreadyLoaded:
	movea.l	(a4),a0
	move.b	d6,(a1)

.nextObject2:
	; load all objects right of the screen that are now in range
	cmp.b	(a0),d6		; is object's x position >= d6?
	bne.s	.done2		; if yes, branch
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn2	; if not, branch
	move.b	(a5),d2		; respawn index of this object
	addq.b	#1,(a5)		; respawn index of next object to the left
.noRespawn2:
	bsr.w	ChkLoadObj_2P	; load object (and get address of next object)
	beq.s	.nextObject2	; continue loading objects, if the SST isn't full
; loc_17E44:
.done2:
	move.l	a0,(a4)		; remember current object from the right
; loc_17E46:
.unloadObjects:
	movea.l	4(a4),a0	; get next object from the left
	subq.w	#3,d6		; look one chunk behind the left edge of the screen
	bcs.s	.done3		; branch, if camera position would be behind level's left boundary
; loc_17E4E:
.nextObject3:
	; subtract number of objects that have been moved out of range (from the left)
	cmp.b	(a0),d6		; is object's x position >= d6?
	bne.s	.done3		; if yes, branch
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	.noRespawn3	; if not, branch
	addq.b	#1,1(a5)	; respawn index of next object to the right
; loc_17E5C:
.noRespawn3:
	addq.w	#6,a0
	bra.s	.nextObject3	; continue with previous object
; ---------------------------------------------------------------------------
; loc_17E60:
.done3:
	move.l	a0,4(a4)	; remember current object from the left
	rts

; ===========================================================================
;loc_17E66: ObjMan_2P_UnkSub1:
ObjectsManager_2P_IsObjectBlockAlreadyLoaded:
	; Preserve 'a1'.
	move.l	a1,-(sp)

	; 'Object_RAM_block_indices' is a list of blocks which are already loaded.
	lea	(Object_RAM_block_indices).w,a1
	; Check index 1.
	cmp.b	(a1)+,d6
	beq.s	.blockAlreadyLoaded
	; Check index 2.
	cmp.b	(a1)+,d6
	beq.s	.blockAlreadyLoaded
	; Check index 3.
	cmp.b	(a1)+,d6
	beq.s	.blockAlreadyLoaded
	; Check index 4.
	cmp.b	(a1)+,d6
	beq.s	.blockAlreadyLoaded
	; Check index 5.
	cmp.b	(a1)+,d6
	beq.s	.blockAlreadyLoaded
	; Check index 6.
	cmp.b	(a1)+,d6
	beq.s	.blockAlreadyLoaded
	; Make it so that a 'bne' instruction after the call to this function will branch.
	moveq	#1,d0

.blockAlreadyLoaded:
	; Restore 'a1'.
	movea.l	(sp)+,a1
	rts
; ===========================================================================
;loc_17E8A: ObjMan_2P_UnkSub2:
ObjectsManager_2P_FindEmptyObjectBlock:
	lea	(Object_RAM_block_indices).w,a1
	; Check block 1.
	lea	(Dynamic_Object_RAM_2P_End+(12*0)*object_size).w,a3
	tst.b	(a1)+
	bmi.s	.foundBlock
	; Check block 2.
	lea	(Dynamic_Object_RAM_2P_End+(12*1)*object_size).w,a3
	tst.b	(a1)+
	bmi.s	.foundBlock
	; Check block 3.
	lea	(Dynamic_Object_RAM_2P_End+(12*2)*object_size).w,a3
	tst.b	(a1)+
	bmi.s	.foundBlock
	; Check block 4.
	lea	(Dynamic_Object_RAM_2P_End+(12*3)*object_size).w,a3
	tst.b	(a1)+
	bmi.s	.foundBlock
	; Check block 5.
	lea	(Dynamic_Object_RAM_2P_End+(12*4)*object_size).w,a3
	tst.b	(a1)+
	bmi.s	.foundBlock
	; Check block 6.
	lea	(Dynamic_Object_RAM_2P_End+(12*5)*object_size).w,a3
	tst.b	(a1)+
	bmi.s	.foundBlock
	; This code should never be reached.
	nop
	nop

.foundBlock:
	; Rewind a little so that 'a1' points to the object block index that we found.
	subq.w	#1,a1
	rts
; ===========================================================================
; this sub-routine appears to determine which 12-slot block of object RAM
; corresponds to the current out-of-range camera positon (in d2) and deletes
; the objects in this block. This most likely takes over the functionality
; of markObjGone, as that routine isn't called in two player mode.
;loc_17EC6: ObjectsManager_2P_UnkSub3:
ObjectsManager_2P_UnloadObjectBlock:
	; Find which object block holds this object block index.
	lea	(Object_RAM_block_indices).w,a1
	; Check block 1.
	lea	(Dynamic_Object_RAM_2P_End+(12*0)*object_size).w,a3
	cmp.b	(a1)+,d2
	beq.s	.foundBlock
	; Check block 2.
	lea	(Dynamic_Object_RAM_2P_End+(12*1)*object_size).w,a3
	cmp.b	(a1)+,d2
	beq.s	.foundBlock
	; Check block 3.
	lea	(Dynamic_Object_RAM_2P_End+(12*2)*object_size).w,a3
	cmp.b	(a1)+,d2
	beq.s	.foundBlock
	; Check block 4.
	lea	(Dynamic_Object_RAM_2P_End+(12*3)*object_size).w,a3
	cmp.b	(a1)+,d2
	beq.s	.foundBlock
	; Check block 5.
	lea	(Dynamic_Object_RAM_2P_End+(12*4)*object_size).w,a3
	cmp.b	(a1)+,d2
	beq.s	.foundBlock
	; Check block 6.
	lea	(Dynamic_Object_RAM_2P_End+(12*5)*object_size).w,a3
	cmp.b	(a1)+,d2
	beq.s	.foundBlock
	; This code should never be reached.
	nop
	nop

.foundBlock:
	; Mark this object block as empty.
	move.b	#-1,-(a1)

	; Delete all objects in this block.
	movem.l	a1/a3,-(sp)
	moveq	#0,d1		; used later to delete objects
	moveq	#12-1,d2	; The number of objects per block

;loc_17F0A: ObjMan2P_UnkSub3_DeleteBlockLoop:
.deleteBlockLoop:
	tst.b	id(a3)
	beq.s	.skipObject	; branch if slot is empty
	movea.l	a3,a1
	moveq	#0,d0
	move.b	respawn_index(a1),d0	; does object remember its state?
	beq.s	.doesNotRememberState	; if not, branch
	bclr	#7,2(a2,d0.w)	; else, clear entry in respawn table

.doesNotRememberState:
	; inlined DeleteObject2:
	moveq	#bytesToLcnt(next_object),d0 ; we want to clear up to the next object
	; note: d1 is already 0

	; delete the object by setting all of its bytes to 0
.clearObjectLoop:
	move.l	d1,(a1)+
	dbf	d0,.clearObjectLoop
    if object_size&3
	move.w	d1,(a1)+
    endif

;loc_17F26: ObjMan2P_UnkSub3_DeleteBlock_SkipObj:
.skipObject:
	lea	next_object(a3),a3
	dbf	d2,.deleteBlockLoop

	moveq	#0,d2
	movem.l	(sp)+,a1/a3

	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Subroutine to check if an object needs to be loaded.
;
; input variables:
;  d2 = respawn index of object to be loaded
;
;  a0 = address in object placement list
;  a2 = object respawn table
;
; writes:
;  d0, d1
;  a1 = object
; ---------------------------------------------------------------------------
;loc_17F36:
ChkLoadObj:
	tst.b	2(a0)	; does the object get a respawn table entry?
	bpl.s	+	; if not, branch
	bset	#7,2(a2,d2.w)	; mark object as loaded
	beq.s	+		; branch if it wasn't already loaded
	addq.w	#6,a0	; next object
	moveq	#0,d0	; let the objects manager know that it can keep going
	rts
; ---------------------------------------------------------------------------

+
	bsr.w	AllocateObject	; find empty slot
	bne.s	return_17F7E	; branch, if there is no room left in the SST
	move.w	(a0)+,x_pos(a1)
	move.w	(a0)+,d0	; there are three things stored in this word
	bpl.s	+		; branch, if the object doesn't get a respawn table entry
	move.b	d2,respawn_index(a1)
+
	move.w	d0,d1		; copy for later
	andi.w	#$FFF,d0	; get y-position
	move.w	d0,y_pos(a1)
	rol.w	#3,d1	; adjust bits
	andi.b	#3,d1	; get render flags
	move.b	d1,render_flags(a1)
	move.b	d1,status(a1)
	_move.b	(a0)+,id(a1) ; load obj
	move.b	(a0)+,subtype(a1)
	moveq	#0,d0

return_17F7E:
	rts
; ===========================================================================
;loc_17F80:
ChkLoadObj_2P:
	tst.b	2(a0)		; does the object get a respawn table entry?
	bpl.s	+		; if not, branch
	bset	#7,2(a2,d2.w)	; mark object as loaded
	beq.s	+		; branch if it wasn't already loaded
	addq.w	#6,a0	; next object
	moveq	#0,d0	; let the objects manager know that it can keep going
	rts
; ---------------------------------------------------------------------------

+
	btst	#4,2(a0)
	beq.s	+			; if this branch isn't taken, then this object would
	bsr.w	AllocateObject		; not be loaded into one of the 12 byte blocks after
	bne.s	return_17FD8		; Dynamic_Object_RAM_2P_End and would most likely end
	bra.s	ChkLoadObj_2P_LoadData	; up somewhere before this in Dynamic_Object_RAM
; ---------------------------------------------------------------------------

+
	bsr.w	AllocateObject_2P	; find empty slot in current 12 object block
	bne.s	return_17FD8	; branch, if there is no room left in this block
;loc_17FAA:
ChkLoadObj_2P_LoadData:
	move.w	(a0)+,x_pos(a1)
	move.w	(a0)+,d0	; there are three things stored in this word
	bpl.s	+		; branch, if the object doesn't get a respawn table entry
	move.b	d2,respawn_index(a1)
+
	move.w	d0,d1		; copy for later
	andi.w	#$FFF,d0	; get y-position
	move.w	d0,y_pos(a1)
	rol.w	#3,d1	; adjust bits
	andi.b	#3,d1	; get render flags
	move.b	d1,render_flags(a1)
	move.b	d1,status(a1)
	_move.b	(a0)+,id(a1) ; load obj
	move.b	(a0)+,subtype(a1)
	moveq	#0,d0

return_17FD8:
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Single object loading subroutine
; Find an empty object array
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_17FDA: ; allocObject: ; SingleObjLoad:
AllocateObject:
	lea	(Dynamic_Object_RAM).w,a1 ; a1=object
	move.w	#(Dynamic_Object_RAM_End-Dynamic_Object_RAM)/object_size-1,d0 ; search to end of table
	tst.w	(Two_player_mode).w
	beq.s	+
	move.w	#(Dynamic_Object_RAM_2P_End-Dynamic_Object_RAM)/object_size-1,d0 ; search to $BF00 exclusive

/
	tst.b	id(a1)	; is object RAM slot empty?
	beq.s	return_17FF8	; if yes, branch
	lea	next_object(a1),a1 ; load obj address ; goto next object RAM slot
	dbf	d0,-	; repeat until end

return_17FF8:
	rts
; ===========================================================================
; ---------------------------------------------------------------------------
; Single object loading subroutine
; Find an empty object array AFTER the current one in the table
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_17FFA: ; allocObjectAfterCurrent: ; SingleObjLoad2:
AllocateObjectAfterCurrent:
	movea.l	a0,a1
	move.w	#Dynamic_Object_RAM_End,d0	; $D000
	sub.w	a0,d0	; subtract current object location
    if object_size=$40
	lsr.w	#object_size_bits,d0	; divide by $40
	subq.w	#1,d0	; keep from going over the object zone
	bcs.s	return_18014
    else
	lsr.w	#6,d0			; divide by $40
	move.b	+(pc,d0.w),d0		; load the right number of objects from table
	bmi.s	return_18014		; if negative, we have failed!
    endif

-
	tst.b	id(a1)	; is object RAM slot empty?
	beq.s	return_18014	; if yes, branch
	lea	next_object(a1),a1 ; load obj address ; goto next object RAM slot
	dbf	d0,-	; repeat until end

return_18014:
	rts

    if object_size<>$40
+
.a	set	Dynamic_Object_RAM
.b	set	Dynamic_Object_RAM_End
.c	set	.b			; begin from bottom of array and decrease backwards
	rept	(.b-.a+$40-1)/$40	; repeat for all slots, minus exception
.c	set	.c-$40			; address for previous $40 (also skip last part)
	dc.b	(.b-.c-1)/object_size-1	; write possible slots according to object_size division + hack + dbf hack
	endm
	even
    endif
; ===========================================================================
; ---------------------------------------------------------------------------
; Single object loading subroutine
; Find an empty object at or within < 12 slots after a3
; ---------------------------------------------------------------------------

; ||||||||||||||| S U B R O U T I N E |||||||||||||||||||||||||||||||||||||||

; loc_18016: ; SingleObjLoad3:
AllocateObject_2P:
	movea.l	a3,a1
	move.w	#12-1,d0

-
	tst.b	id(a1)	; is object RAM slot empty?
	beq.s	return_18028	; if yes, branch
	lea	next_object(a1),a1 ; load obj address ; goto next object RAM slot
	dbf	d0,-	; repeat until end

return_18028:
	rts
; ===========================================================================

;---------------------------------------------------------------------------------------
; CNZ object layouts for 2-player mode (various objects were deleted)
;---------------------------------------------------------------------------------------

; Macro for marking the boundaries of an object layout file
ObjectLayoutBoundary macro
	dc.w	$FFFF, $0000, $0000
    endm

    if fixBugs
	; Sonic Team forgot to put a boundary marker here, meaning the game
	; could potentially read past the start of the file and load random
	; objects.
	ObjectLayoutBoundary
    endif

; byte_1802A;
    if gameRevision=0
Objects_CNZ1_2P:	BINCLUDE	"level/objects/CNZ_1_2P (REV00).bin"
    else
    ; a Crawl badnik was moved slightly further away from a ledge
    ; 2 flippers were moved closer to a wall
Objects_CNZ1_2P:	BINCLUDE	"level/objects/CNZ_1_2P.bin"
    endif

	ObjectLayoutBoundary

; byte_18492:
    if gameRevision=0
Objects_CNZ2_2P:	BINCLUDE	"level/objects/CNZ_2_2P (REV00).bin"
    else
    ; 4 Crawl badniks were slightly moved, placing them closer/farther away from ledges
    ; 2 flippers were moved away from a wall to keep players from getting stuck behind them
Objects_CNZ2_2P:	BINCLUDE	"level/objects/CNZ_2_2P.bin"
    endif

	ObjectLayoutBoundary

; ===========================================================================