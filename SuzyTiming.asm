    opt c+


    org $200

    sei
    ldx #31
@   stz $fda0,x	; GREEN0
    dex
    bpl @-

    lda #1
    jsr $fe00
    ldx #0
    ldy #0
loop
    lda $fcb2
dst = *+2
    sta $0400,x
    inx
    bne loop
    inc dst
    iny
    cpy #4
    bcc loop
    ldy #0
    inc sector
sector equ *+1
    lda #1
    cmp #(_end/1024)+1
    bcs @+
    phx
    phy
    jsr $fe00
    ply
    plx
    bra loop
@   jmp $400

    opt h-

    org $67

    .align 1024,$ff

    org $400

    lda #LYNX.@DISPCTL(DISP_COLOR|DISP_FOURBIT|DMA_ENABLE)
    sta LYNX.DISPCTL
    lda #LYNX.@SERCTL(TXOPEN)
    sta LYNX.SERCTL
    lda #LYNX.@IO(READ_ENABLE|RESTLESS|CART_POWER_OFF)
    sta LYNX.IODIR
    lda #LYNX.@IO(RESTLESS|CART_ADDR_DATA)
    sta LYNX.IODAT
    lda #$ff
    sta LYNX.MSTEREO
    stz LYNX.MPAN
    stz LYNX.AUDIO3_VOLCNTRL
    stz LYNX.AUDIO2_VOLCNTRL
    stz LYNX.AUDIO1_VOLCNTRL
    stz LYNX.AUDIO0_VOLCNTRL
    lda #LYNX.@SUZYBUSEN(ENABLE)
    sta LYNX.SUZYBUSEN
    lda #$F3
    sta LYNX.SPRINIT
    lda #LYNX.@SPRSYS(NO_COLLIDE|UNSAFEACCESSRST)
    sta LYNX.SPRSYS
    stz LYNX.HOFF
    stz LYNX.HOFF+1
    stz LYNX.VOFF
    stz LYNX.VOFF+1
    lda #$7F
    sta LYNX.HSIZOFF
    sta LYNX.VSIZOFF
    stz LYNX.SDONEACK
    lda #2
    sta LYNX.COLLOFF

    lda #LYNX.@MAPCTL(VECTOR_SPACE)
    sta LYNX.MAPCTL
    lda #$ff
    sta LYNX.INTRST

    ;video timers
    lda #LYNX.@TIM_CONTROLA(ENABLE_RELOAD|ENABLE_COUNT)
    sta LYNX.HCOUNT_CONTROLA
    lda #158
    sta LYNX.HCOUNT_BACKUP
    lda #LYNX.@TIM_CONTROLA(ENABLE_RELOAD|ENABLE_COUNT|AUD_LINKING)
    sta LYNX.VCOUNT_CONTROLA
    lda #104 ;backup value for vertical scan timer (== 102 vertical lines plus 2)
    sta LYNX.VCOUNT_BACKUP
    lda #41
    sta LYNX.PBKUP

    stz LYNX.DISPADR
    stz LYNX.VIDBAS
    stz LYNX.SPRHSIZ
    stz LYNX.SPRVSIZ
    lda #1
    sta LYNX.SPRHSIZ+1
    sta LYNX.SPRVSIZ+1

    ldx #31
@   lda sprpal,x
    sta LYNX.GREEN0,x
    dex
    bpl @-

drawbuf = $d000
dispbuf = $b000


    lda #>dispbuf
    sta LYNX.DISPADR+1
    lda #>drawbuf
    sta LYNX.VIDBAS+1

pptr                = $80
joy                 = $82
counter             = $84
ptrbak              = $86
cbak                = $88


    lda #<irq
    sta LYNX.CPU_IRQ
    lda #>irq
    sta LYNX.CPU_IRQ+1
    stz counter
    stz counter+1

mainLoop
   lda LYNX.JOYSTICK
   sta joy
   asl joy
   bcc ssub
   subtractline
ssub
   asl joy
   bcc sadd
   addline
sadd
   asl joy
   bcc sd
   decrement
sd asl joy
   bcc si
   increment
si


    lda #255
    sta LYNX.TIMER5_BACKUP
    lda counter
    sta LYNX.TIMER5_COUNT
    stz LYNX.TIMER7_BACKUP
    lda counter+1
    sta LYNX.TIMER7_COUNT

    lda #LYNX.@TIM_CONTROLA(ENABLE_INT|RESET_DONE|ENABLE_COUNT|AUD_LINKING)
    sta LYNX.TIMER7_CONTROLA
    ldx #LYNX.@TIM_CONTROLA(ENABLE_RELOAD|ENABLE_COUNT)


    cli
    lda #100
@   cmp LYNX.VCOUNT_COUNT
    bne @-


    lda #<imageSCB
    sta LYNX.SCBNEXT
    lda #>imageSCB
    sta LYNX.SCBNEXT+1
    lda #LYNX.@SPRGO(SPRITE_GO)
    sta LYNX.SPRGO

    stx LYNX.TIMER5_CONTROLA
    stz LYNX.CPUSLEEP
    bra *

irq
    lda #$ff
    sta LYNX.INTRST
    stz LYNX.TIMER7_CONTROLA
    stz LYNX.TIMER5_CONTROLA

    copy

@   stz LYNX.CPUSLEEP
    lda LYNX.SPRSYS
    and #LYNX.@SPRSYS(SPRITEWORKING)
    bne @- 
    stz LYNX.SDONEACK

    lda counter
    sta cbak
    lda counter+1
    sta cbak+1
    jsr print

    jmp mainLoop



imageSCB:
    .by LYNX.@SPRCTL0(FOUR_PER_PIXEL)
    .by LYNX.@SPRCTL1(RELOAD_HV)          ;SPRCTL1
    .by 0  ;SPRCOL
    .wo clearSCB ;clearSCB  ;Next SCB (0=none)
    .wo sprdat
    .wo 58 ;Xpos
    .wo 0 ;Yos
    .wo $100
    .wo $100
    .he 01 23 45 67 89 ab cd ef

clearSCB:
    .by LYNX.@SPRCTL0(ONE_PER_PIXEL|BACKNONCOLL_SPRITE) ;SPRCTL0
    .by LYNX.@SPRCTL1(REUSE_PALETTE|RELOAD_HV)          ;SPRCTL1
    .by 0                                               ;SPRCOL
    .wo 0                                               ;Next SCB (0=none)
    .wo clearSprite                                     ;Sprite pointer
    .wo 58                                              ;Xpos
    .wo 0                                               ;Yos
    .wo 102/2*256
    .wo 102*256

clearSprite:
    .by 2,%00001000
    .he 00

.proc print
    lda #<dispbuf
    sta ptrbak
@   lda cbak+1
    cmp #>10000
    bne @+
    lda cbak
    cmp #<10000
@   bcc l10000
    sec
    lda cbak
    sbc #<10000
    sta cbak
    lda cbak+1
    sbc #>10000
    sta cbak+1
    lda ptrbak
    sta pptr
    clc
    adc #2
    sta ptrbak
    lda #>dispbuf
    sta pptr+1
    ldx #2
    jsr j

l10000
    lda cbak+1
    cmp #>1000
    bne @+
    lda cbak
    cmp #<1000
@   bcs g1000
    lda ptrbak
    beq l1000
g1000
    ldx #0
g1000l
    lda cbak+1
    cmp #>1000
    bne @+
    lda cbak
    cmp #<1000
@   bcc g1000e
    inx
    inx
    sec
    lda cbak
    sbc #<1000
    sta cbak
    lda cbak+1
    sbc #>1000
    sta cbak+1
    bra g1000l
g1000e
    lda ptrbak
    sta pptr
    clc
    adc #2
    sta ptrbak
    lda #>dispbuf
    sta pptr+1
    jsr j

l1000
    lda cbak+1
    cmp #>100
    bne @+
    lda cbak
    cmp #<100
@   bcs g100
    lda ptrbak
    beq l100
g100
    ldx #0
g100l
    lda cbak+1
    cmp #>100
    bne @+
    lda cbak
    cmp #<100
@   bcc g100e
    inx
    inx
    sec
    lda cbak
    sbc #<100
    sta cbak
    lda cbak+1
    sbc #>100
    sta cbak+1
    bra g100l
g100e
    lda ptrbak
    sta pptr
    clc
    adc #2
    sta ptrbak
    lda #>dispbuf
    sta pptr+1
    jsr j

l100
    lda cbak+1
    cmp #>10
    bne @+
    lda cbak
    cmp #<10
@   bcs g10
    lda ptrbak
    beq l10
g10
    ldx #0
g10l
    lda cbak+1
    cmp #>10
    bne @+
    lda cbak
    cmp #<10
@   bcc g10e
    inx
    inx
    sec
    lda cbak
    sbc #<10
    sta cbak
    lda cbak+1
    sbc #>10
    sta cbak+1
    bra g10l
g10e
    lda ptrbak
    sta pptr
    clc
    adc #2
    sta ptrbak
    lda #>dispbuf
    sta pptr+1
    jsr j
l10
    lda cbak
    asl
    tax
    lda ptrbak
    sta pptr
    lda #>dispbuf
    sta pptr+1
j
    jmp (tab,x)
tab
    .wo p0
    .wo p1
    .wo p2
    .wo p3
    .wo p4
    .wo p5
    .wo p6
    .wo p7
    .wo p8
    .wo p9

p0
    lda     #$0f
    sta (pptr)
    ldy #1
    lda     #$00
    sta (pptr),y
    ldy #80
    lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #160
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #240
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$0f
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    rts
p1
    lda     #$0f
    sta (pptr)
    ldy #1
    lda     #$00
    sta (pptr),y
    ldy #80
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #160
    lda     #$0f
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #240
    lda     #$0f
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    rts
p2
    lda     #$ff
    sta (pptr)
    ldy #1
    lda     #$00
    sta (pptr),y
    ldy #80
    ;lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #160
    lda     #$0f
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #240
    lda     #$f0
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    rts
p3
    lda     #$ff
    sta (pptr)
    ldy #1
    lda     #$f0
    sta (pptr),y
    ldy #80
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #160
    lda     #$0f
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #240
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    rts
p4
    lda     #$f0
    sta (pptr)
    ldy #1
    ;lda     #$f0
    sta (pptr),y
    ldy #80
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #160
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #240
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    rts
p5
    lda     #$ff
    sta (pptr)
    ldy #1
    lda     #$f0
    sta (pptr),y
    ldy #80
    ;lda     #$f0
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #160
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #240
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    rts
p6
    lda     #$0f
    sta (pptr)
    ldy #1
    lda     #$f0
    sta (pptr),y
    ldy #80
    ;lda     #$f0
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #160
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #240
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    rts
p7
    lda     #$ff
    sta (pptr)
    ldy #1
    lda     #$f0
    sta (pptr),y
    ldy #80
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #160
    lda     #$0f
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #240
    lda     #$f0
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$f0
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    rts
p8
    lda     #$ff
    sta (pptr)
    ldy #1
    lda     #$f0
    sta (pptr),y
    ldy #80
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #160
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #240
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    rts
p9
    lda     #$ff
    sta (pptr)
    ldy #1
    lda     #$f0
    sta (pptr),y
    ldy #80
    ;lda     #$f0
    sta (pptr),y
    iny
    ;lda     #$f0
    sta (pptr),y
    ldy #160
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #240
    lda     #$00
    sta (pptr),y
    iny
    lda     #$f0
    sta (pptr),y
    ldy #320-256
    inc pptr+1
    lda     #$ff
    sta (pptr),y
    iny
    lda     #$00
    sta (pptr),y
    rts
.endp

sprpal
    ins 'DNRJM.spr', 8, 32
sprdat
    ins 'DNRJM.spr', 40

_end

.macro decrement
    lda counter+1
    ora counter
    beq @+
    dew counter
@
.endm

.macro increment
    inw counter
.endm

.macro subtractline
    lda counter
    sec
    sbc #100
    sta counter
    bcs @+
    dec counter+1
@   lda counter+1
    cmp #255
    bne @+
    stz counter
    stz counter+1
@
.endm

.macro addline
    lda counter
    clc
    adc #100
    sta counter
    bcc @+
    inc counter+1
@
.endm

.macro copy
    ldx #0
@
    lda drawbuf+$000,x
    sta dispbuf+$000,x
    lda drawbuf+$100,x
    sta dispbuf+$100,x
    lda drawbuf+$200,x
    sta dispbuf+$200,x
    lda drawbuf+$300,x
    sta dispbuf+$300,x
    lda drawbuf+$400,x
    sta dispbuf+$400,x
    lda drawbuf+$500,x
    sta dispbuf+$500,x
    lda drawbuf+$600,x
    sta dispbuf+$600,x
    lda drawbuf+$700,x
    sta dispbuf+$700,x
    lda drawbuf+$800,x
    sta dispbuf+$800,x
    lda drawbuf+$900,x
    sta dispbuf+$900,x
    lda drawbuf+$a00,x
    sta dispbuf+$a00,x
    lda drawbuf+$b00,x
    sta dispbuf+$b00,x
    lda drawbuf+$c00,x
    sta dispbuf+$c00,x
    lda drawbuf+$d00,x
    sta dispbuf+$d00,x
    lda drawbuf+$e00,x
    sta dispbuf+$e00,x
    lda drawbuf+$f00,x
    sta dispbuf+$f00,x
    lda drawbuf+$1000,x
    sta dispbuf+$1000,x
    lda drawbuf+$1100,x
    sta dispbuf+$1100,x
    lda drawbuf+$1200,x
    sta dispbuf+$1200,x
    lda drawbuf+$1300,x
    sta dispbuf+$1300,x
    lda drawbuf+$1400,x
    sta dispbuf+$1400,x
    lda drawbuf+$1500,x
    sta dispbuf+$1500,x
    lda drawbuf+$1600,x
    sta dispbuf+$1600,x
    lda drawbuf+$1700,x
    sta dispbuf+$1700,x
    lda drawbuf+$1800,x
    sta dispbuf+$1800,x
    lda drawbuf+$1900,x
    sta dispbuf+$1900,x
    lda drawbuf+$1a00,x
    sta dispbuf+$1a00,x
    lda drawbuf+$1b00,x
    sta dispbuf+$1b00,x
    lda drawbuf+$1c00,x
    sta dispbuf+$1c00,x
    lda drawbuf+$1d00,x
    sta dispbuf+$1d00,x
    lda drawbuf+$1e00,x
    sta dispbuf+$1e00,x
    lda drawbuf+$1f00,x
    sta dispbuf+$1f00,x
    inx
    beq @+
    jmp @-
@
.endm


    icl 'lynxhard.asm'
