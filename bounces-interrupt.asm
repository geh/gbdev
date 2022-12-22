INCLUDE "hardware.inc"

; CONSTANTS

; DIRECTIONS
DEF NE    EQU $00
DEF SE    EQU $01
DEF SW    EQU $02
DEF NW    EQU $03

; OBJECT COUNT (MAX: 40)
DEF OBJCOUNT EQU 40

; BOUNCE AREA LIMITS
DEF WESTLIMIT EQU 8
DEF EASTLIMIT EQU 160
DEF NORTHLIMIT EQU 16
DEF SOUTHLIMIT EQU 144+8 

; VARIABLES AND STRUCTURES STORED IN RAM
DEF SHADOW_OAM EQU _RAM              ; 160 bytes
DEF DIRARRAY EQU  SHADOW_OAM+160     ; 40 bytes

; VBLANK INTERRUPT HANDLER

SECTION "Vblank", ROM0[$40]
  jp Shadow_OAM_Copy

; BEGINNING OF CODE
SECTION "Header", ROM0[$100]

	jp EntryPoint
	ds $150 - @, 0 ; Make room for the header

EntryPoint:
	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

	; Do not turn the LCD off outside of VBlank
.wait:
	ld a, [rLY]
	cp 144
	jp nz, .wait

	
; Turn the LCD off
	ld a, 0
	ld [rLCDC], a

; Set OBJ1 palette 
        ld a,%11100100
        ld [rOBP1],a

	; Copy the tile data for objects
	ld de, ObjTiles
	ld hl, $8000
	ld bc, ObjTilesEnd - ObjTiles
CopyObjTiles:
	ld a, [de]
	ldi [hl], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, CopyObjTiles

; clear OAM
        ld hl,_OAMRAM
        ld b, 40*4
        ld a, 0
OAM_clear_loop:
        ld [hl], a
        inc hl
        dec b
        jp nz,OAM_clear_loop

; clear SHADOW OAM
        ld hl,SHADOW_OAM
        ld b, 40*4
        ld a, 0
SHADOW_OAM_clear_loop:
        ld [hl], a
        inc hl
        dec b
        jp nz,SHADOW_OAM_clear_loop


; 1. INITIALIZE SHADOW_OAM AND DIRARRAY
; FOR EACH OBJ (FROM 0 TO OBJCOUNT-1)
;   OBJ.X   = RANDOM(WESTLIMIT, EASTLIMIT)
;   OBJ.Y   = RANDOM(NORTHLIMIT, SOUTHLIMIT)
;   OBJ.DIR = RANDOM(NE,SE,SW,NW)
  ld hl,DIRARRAY
  ld de,SHADOW_OAM
  ld b,0
init_obj_loop:
  call dir_random  
  ld [hl],a
  inc hl
  call y_random
  ld [de],a
  inc de
  call x_random
  ld [de],a
  inc de
  ld a, 0
  ld [de],a ; use first tile
  inc de
  ld [de],a ; set object flags to zero
  inc de

  inc b
  ld a,b
  cp OBJCOUNT
  jp nz, init_obj_loop

  ; prepare OBJ1 palette 
  ld a,%11100100
  ld [rOBP1],a

  ; Turn the LCD on
  ld a, LCDCF_ON | LCDCF_OBJON
  ld [rLCDC], a

  ld a, IEF_VBLANK 
  ld [rIE], a
  ei  

; 2. MAIN LOOP
main_loop:
; UPDATE OBJECTS IN SHADOW_OAM
  ld hl,DIRARRAY
  ld de,SHADOW_OAM
  ld b,0
update_loop:
  ld a,[hl] ; get OBJ direction
  cp NE
  jp z, update_NE
  cp SE
  jp z, update_SE
  cp SW
  jp z, update_SW
  cp NW
  jp z, update_NW
end_update:
  inc hl ; next DIRARRAY location
  inc de
  inc de
  inc de
  inc de ; next SHADOW_OAM location

  inc b
  ld a,b
  cp OBJCOUNT
  jp nz, update_loop
; END OF UPDATE OBJECTS IN SHADOW_OAM

  halt
  jp main_loop

Shadow_OAM_Copy:
  ld de, SHADOW_OAM
  ld hl, _OAMRAM
  ld b, OBJCOUNT
.loop:
  ld a, [de]
  inc e
  ld [hli], a
  ld a, [de]
  inc e
  ld [hli], a
  ld a, [de]
  inc e
  ld [hli], a
  ld a, [de]
  inc e
  ld [hli], a
  dec b
  jr nz, .loop
  reti 

;****************
;   ROUTINES
;****************

; input:
;   hl: OBJ entry in DIRARRAY
;   de: OBJ entry in OAM
update_NE:
  ld a,[de] ; Y
  cp NORTHLIMIT
  jp nz, update_NE_1
  ld a, SE
  ld [hl], a
  jp end_update
update_NE_1:
  inc de
  ld a,[de] ; X
  cp EASTLIMIT
  jp nz, update_NE_2
  ld a, NW
  ld [hl],a 
  dec de ; restore de value
  jp end_update
update_NE_2:
  inc a      ; INCREMENT X
  ld [de], a ; STORE X 
  dec de
  ld a,[de]
  dec a
  ld [de],a  ; DECREMENT Y 
  jp end_update

; input:
;   hl: OBJ entry in DIRARRAY
;   de: OBJ entry in OAM
update_SE:
  ld a,[de] ; Y
  cp SOUTHLIMIT
  jp nz, update_SE_1
  ld a, NE
  ld [hl], a
  jp end_update
update_SE_1:
  inc de
  ld a,[de] ; X
  cp EASTLIMIT
  jp nz, update_SE_2
  ld a, SW
  ld [hl],a 
  dec de ; restore de value
  jp end_update
update_SE_2:
  inc a      ; INCREMENT X
  ld [de], a ; STORE X 
  dec de
  ld a,[de]
  inc a
  ld [de],a  ; INCREMENT Y 
  jp end_update

; input:
;   hl: OBJ entry in DIRARRAY
;   de: OBJ entry in OAM
update_SW:
  ld a,[de] ; Y
  cp SOUTHLIMIT
  jp nz, update_SW_1
  ld a, NW
  ld [hl], a
  jp end_update
update_SW_1:
  inc de
  ld a,[de] ; X
  cp WESTLIMIT
  jp nz, update_SW_2
  ld a, SE
  ld [hl],a 
  dec de ; restore de value
  jp end_update
update_SW_2:
  dec a
  ld [de], a ; DECREMENT X 
  dec de
  ld a,[de]
  inc a
  ld [de],a  ; INCREMENT Y 
  jp end_update

; input:
;   hl: OBJ entry in DIRARRAY
;   de: OBJ entry in OAM
update_NW:
  ld a,[de] ; Y
  cp NORTHLIMIT
  jp nz, update_NW_1
  ld a, SW
  ld [hl], a
  jp end_update
update_NW_1:
  inc de
  ld a,[de] ; X
  cp WESTLIMIT
  jp nz, update_NW_2
  ld a, NE
  ld [hl],a 
  dec de ; restore de value
  jp end_update
update_NW_2:
  dec a
  ld [de], a ; DECREMENT X 
  dec de
  ld a,[de]
  dec a
  ld [de],a  ; DECREMENT Y 
  jp end_update


; return in a a random value within [WESTLIMIT, EASTLIMIT]
x_random:
  call random_byte
  ; make A fit into [0, EASTLIMIT-WESTLIMIT] interval
  cp EASTLIMIT-WESTLIMIT
  jp c, x_random_1
  sub EASTLIMIT-WESTLIMIT
x_random_1:
  add WESTLIMIT
  ret

; return in a a random value within [NORTHLIMIT, SOUTHLIMIT]
y_random:
  call random_byte
  cp SOUTHLIMIT-NORTHLIMIT
  jp c, x_random_1
  sub SOUTHLIMIT-NORTHLIMIT
y_random_1:
  add NORTHLIMIT
  ret

; return in a a random value within [0,3]
dir_random:
  call random_byte
  rra
  and %00000011
  ret

random_byte:
  ld a,[rDIV]
  xor b
  xor e
  ret

SECTION "Object tile data", ROM0
ObjTiles:
        db $7e,$7e, $ff,$ff, $c3,$c3, $c3,$c3, $c3,$c3, $c3,$c3, $ff,$ff, $7e,$7e
ObjTilesEnd:


