; ----------------------------------------------------------------------------
; Object BF - Rotaty-stick badnik from WFZ
; ----------------------------------------------------------------------------
; Sprite_3BEAA:
ObjBF:
	moveq	#0,d0
	move.b	routine(a0),d0
	move.w	ObjBF_Index(pc,d0.w),d1
	jmp	ObjBF_Index(pc,d1.w)
; ===========================================================================
; off_3BEB8:
ObjBF_Index:	offsetTable
		offsetTableEntry.w ObjBF_Init		; 0
		offsetTableEntry.w ObjBF_Animate	; 2
; ===========================================================================
; BranchTo9_LoadSubObject
ObjBF_Init:
	bra.w	LoadSubObject
; ===========================================================================
; loc_3BEC0:
ObjBF_Animate:
	lea	(Ani_objBF).l,a1
	jsrto	AnimateSprite, JmpTo25_AnimateSprite
	jmpto	MarkObjGone, JmpTo39_MarkObjGone
; ===========================================================================
; off_3BECE:
ObjBE_SubObjData2:
	subObjData ObjBF_MapUnc_3BEE0,make_art_tile(ArtTile_ArtNem_WfzUnusedBadnik,3,1),4,4,4,4
; animation script
; off_3BED8:
Ani_objBF:	offsetTable
		offsetTableEntry.w +	; 0
+		dc.b   1,  0,  1,  2,$FF
		even
; ----------------------------------------------------------------------------
; sprite mappings
; ----------------------------------------------------------------------------
ObjBF_MapUnc_3BEE0:	include "mappings/sprite/objBF.asm"
; ===========================================================================