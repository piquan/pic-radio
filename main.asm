#include "p16f1619.inc"

;;; I/O peripherials:
;; Antenna: RC1
;; XXX It would be better to use RC4 or RC5 for the antenna,
;; since those can be put in high drive strength mode.  But
;; note that the board has assignments for those already.
;; Modulation debug port: RC2
;; LED D4: RA5 : Toggle on each note
;; LED D5: RA1 : On when note ready
;; LED D6: RA2 : On when playing a note (vs rest)
;; LED D7: RC5 : On when running
;; SW: RC4
;; POT: AN4 (Pin: RC0)

;;; Curiosity board ports

led_d4_bit:     equ     LATA5
led_d4_port:    equ     PORTA
led_d4_tris:    equ     TRISA
led_d4_lat:     equ     LATA
led_d4_ansel:   equ     ANSELA
led_d4_wpu:     equ     WPUA
led_d4_odcon:   equ     ODCONA
led_d4_slrcon:  equ     SLRCONA
led_d4_inlvl:   equ     INLVLA

led_d5_bit:     equ     LATA1
led_d5_port:    equ     PORTA
led_d5_tris:    equ     TRISA
led_d5_lat:     equ     LATA
led_d5_ansel:   equ     ANSELA
led_d5_wpu:     equ     WPUA
led_d5_odcon:   equ     ODCONA
led_d5_slrcon:  equ     SLRCONA
led_d5_inlvl:   equ     INLVLA

led_d6_bit:     equ     LATA2
led_d6_port:    equ     PORTA
led_d6_tris:    equ     TRISA
led_d6_lat:     equ     LATA
led_d6_ansel:   equ     ANSELA
led_d6_wpu:     equ     WPUA
led_d6_odcon:   equ     ODCONA
led_d6_slrcon:  equ     SLRCONA
led_d6_inlvl:   equ     INLVLA

led_d7_bit:     equ     LATC5
led_d7_port:    equ     PORTC
led_d7_tris:    equ     TRISC
led_d7_lat:     equ     LATC
led_d7_ansel:   equ     ANSELC
led_d7_wpu:     equ     WPUC
led_d7_odcon:   equ     ODCONC
led_d7_slrcon:  equ     SLRCONC
led_d7_inlvl:   equ     INLVLC

s1_bit:         equ     LATC4
s1_iocp:        equ     IOCCP
s1_iocn:        equ     IOCCN
s1_iocf:        equ     IOCCF
s1_port:        equ     PORTC
s1_tris:        equ     TRISC
s1_lat:         equ     LATC
s1_ansel:       equ     ANSELC
s1_wpu:         equ     WPUC
s1_odcon:       equ     ODCONC
s1_slrcon:      equ     SLRCONC
s1_inlvl:       equ     INLVLC

pot1_adcon:     equ     b'00100' << 2
pot1_bit:       equ     LATC0
pot1_port:      equ     PORTC
pot1_tris:      equ     TRISC
pot1_lat:       equ     LATC
pot1_ansel:     equ     ANSELC
pot1_wpu:       equ     WPUC
pot1_odcon:     equ     ODCONC
pot1_slrcon:    equ     SLRCONC
pot1_inlvl:     equ     INLVLC

;;; I/O ports for this application

ant_bit:        equ     LATC1
ant_port:       equ     PORTC
ant_tris:       equ     TRISC
ant_lat:        equ     LATC
ant_ansel:      equ     ANSELC
ant_wpu:        equ     WPUC
ant_odcon:      equ     ODCONC
ant_slrcon:     equ     SLRCONC
ant_inlvl:      equ     INLVLC

moddbg_bit:     equ     LATC2
moddbg_port:    equ     PORTC
moddbg_tris:    equ     TRISC
moddbg_lat:     equ     LATC
moddbg_ansel:   equ     ANSELC
moddbg_wpu:     equ     WPUC
moddbg_odcon:   equ     ODCONC
moddbg_slrcon:  equ     SLRCONC
moddbg_inlvl:   equ     INLVLC

led_noteprog_bit:   equ led_d4_bit
led_noteprog_port:  equ led_d4_port
led_noteprog_tris:  equ led_d4_tris
led_noteprog_lat:   equ led_d4_lat
led_noteprog_ansel: equ led_d4_ansel
led_noteprog_wpu:   equ led_d4_wpu
led_noteprog_odcon: equ led_d4_odcon
led_noteprog_slrcon:equ led_d4_slrcon
led_noteprog_inlvl: equ led_d4_inlvl

led_noterdy_bit:    equ led_d5_bit
led_noterdy_port:   equ led_d5_port
led_noterdy_tris:   equ led_d5_tris
led_noterdy_lat:    equ led_d5_lat
led_noterdy_ansel:  equ led_d5_ansel
led_noterdy_wpu:    equ led_d5_wpu
led_noterdy_odcon:  equ led_d5_odcon
led_noterdy_slrcon: equ led_d5_slrcon
led_noterdy_inlvl:  equ led_d5_inlvl

led_noterest_bit:   equ led_d6_bit
led_noterest_port:  equ led_d6_port
led_noterest_tris:  equ led_d6_tris
led_noterest_lat:   equ led_d6_lat
led_noterest_ansel: equ led_d6_ansel
led_noterest_wpu:   equ led_d6_wpu
led_noterest_odcon: equ led_d6_odcon
led_noterest_slrcon:equ led_d6_slrcon
led_noterest_inlvl: equ led_d6_inlvl

led_running_bit:    equ led_d7_bit
led_running_port:   equ led_d7_port
led_running_tris:   equ led_d7_tris
led_running_lat:    equ led_d7_lat
led_running_ansel:  equ led_d7_ansel
led_running_wpu:    equ led_d7_wpu
led_running_odcon:  equ led_d7_odcon
led_running_slrcon: equ led_d7_slrcon
led_running_inlvl:  equ led_d7_inlvl
	
;;; Timers for this application
mod_tmr_int_reg:        equ     PIE1
mod_tmr_ie_bit:         equ     TMR1IE
mod_tmr_if_bit:         equ     TMR1IF
mod_tmr_con_reg:        equ     T1CON
mod_tmr_on_bit:         equ     TMR1ON
mod_tmr_gcon_reg:       equ     T1GCON
mod_tmr_ge_bit:         equ     TMR1GE
mod_tmr_hi:             equ     TMR1H
mod_tmr_lo:             equ     TMR1L

note_tmr_int_reg:       equ     PIE5
note_tmr_ie_bit:        equ     TMR3IE
note_tmr_if_bit:        equ     TMR3IF
note_tmr_con_reg:       equ     T3CON
note_tmr_gcon_reg:      equ     T3GCON
note_tmr_ge_bit:        equ     TMR3GE
note_tmr_hi:            equ     TMR3H
note_tmr_lo:            equ     TMR3L
	
;;;
;;; Other constants
;;; 
	
notedurenc_end   equ    0xF
notedurenc_octup equ    0xE
notedurenc_octdn equ    0xD
;; A rest is 0xC (the first nybble is the duration).  This is
;; encoded using the pitch table, so we don't skip the duration
;; management.

;;;
;;; Macros
;;;
	
;; FIXME Instead of using this table lookup code, structure the tables by
;; starting them with a BRW, and just call there.
table_lookup_w macro table
        pagesel table
        addlw   table
        btfsc   STATUS, C
        incf    PCLATH, F
        callw
        endm

table_lookup_f macro table, reg
        banksel reg
        movf    reg, W
        table_lookup_w table
        endm

;;;
;;; Config
;;;

        ;; My board requires _FOSC_INTOSC and _LVP_ON.
        ;; FIXME Review these configs.
        __config _CONFIG1, _FOSC_INTOSC & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _BOREN_ON & _CLKOUTEN_OFF & _IESO_ON & _FCMEN_ON
        __config _CONFIG2, _WRT_OFF & _PPS1WAY_OFF & _ZCD_OFF & _PLLEN_OFF & _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON
        __config _CONFIG3, _WDTCPS_WDTCPS1F & _WDTE_OFF & _WDTCWS_WDTCWSSW & _WDTCCS_SWC
	
;;;
;;; Variables
;;;
;; FIXME Is there a way to define variables close to where we'll use
;; them?

                udata
;; The NEXT* variables are used to communicate from the main loop
;; to the note-changing ISR (the one on the note duration timer).
;; They hold the duration and frequency of the next note (as high
;; and low bytes).
nextdh          res     1
nextdl          res     1
nextfh          res     1
nextfl          res     1
;; NEXTRDY communicates from the note-changing ISR to the main loop.
;; The ISR clears this register when it needs another note.  The main
;; loop can watch this to tell when it needs to load another note into
;; the other NEXT* registers.
nextrdy         res     1
;; Frequency of the current note being played.  Populated by the
;; note-changing ISR, and consumed by the modulation timer ISR.
freqh           res     1
freql           res     1
;; The current octave used to interpret notes in the piano roll.
octave          res     1
;; Loop variable; used locally
octave_tmp      res     1
;; Current offset in the piano roll.
notepos         res     1
;; The *ENC* variables are used in the main loop while translating
;; from the piano roll to the NEXT* variables.
notedurenc      res     1       ;Note from the piano roll
encnote         res     1       ;The low half (the note)
encdur          res     1       ;The high half (duration), shifted to 0-0xf
	
;;;
;;; Vectors
;;;
;; Well... I'll call them "vectors".  I'm putting a vector in RST_VEC,
;; but contrary to practice on many architectures, I currently am
;; putting the entire ISR in ISR_VEC.  Is that common on PIC?

;; Set up the reset vector at 0
;; The CODE psueudo-op is of the form "[label] CODE [address]".  If the
;; label is omitted, it defaults to ".code"; if the address is omitted,
;; it is chosen by the linker.  We put our main program in the code
;; segment named ".code" (by default).  But if we try to put some parts
;; of .code here and some later, then the linker will complain that
;; .code is non-contiguous.  So, we give it the label "rst_vec".  That
;; label is otherwise unused, but it keeps the linker from trying to
;; combine this bit of code with our main program.
rst_vec code    0x0000
        pagesel start
        goto    start
	
;; We put the entire ISR here, rather than the tradition (on many
;; architectures) of just having jump tables at the ISR vector.
isr_vec code    0x0004
        banksel PIR1
        btfss   PIR1, TMR1IF
        goto    isr_vec_modtim_end

        ;; Modulation timer handler
        bcf     PIR1, TMR1IF    ;Clear the interrupt bit
        movlw   1               ;Toggle the OSCTUNE bit
        banksel OSCTUNE         ; FIXME Make the deviation a configurable
        xorwf   OSCTUNE, F      ; constant
        movlw	1 << moddbg_bit ;Toggle the modulation debugging port
        banksel moddbg_lat
        xorwf	moddbg_lat, F
        banksel T1CON           ;Disable timer1
        bcf     T1CON, TMR1ON
        banksel freqh           ;Reset the values in timer1
        movfw   freqh           ; FIXME The time for which timer1 isn't
        banksel TMR1H           ; running isn't accounted for in the
        movwf   TMR1H           ; frequency table.  We may want to add
        banksel freql           ; an offset once I've stopped messing with
        movfw   freql           ; this code.
        banksel TMR1L
        movwf   TMR1L
        banksel T1CON           ;Enable timer1
        bsf     T1CON, TMR1ON
        retfie
	
isr_vec_modtim_end:
        banksel PIR5
        btfss   PIR5, TMR3IF
        goto    isr_vec_durtim_end
	
        ;; Note duration timer handler
        bcf     PIR5, TMR3IF
        banksel T1CON           ;Disable timer 1
        bcf     T1CON, TMR1ON
        banksel nextfh          ;Is the next note a rest?
        movfw   nextfh
        banksel nextfl
        iorwf   nextfl, W
        btfss   STATUS, Z
        bra     isnote
        ;; It's a rest.
        banksel T1CON           ;Turn off timer1.  FIXME Redundant
        bcf     T1CON, TMR1ON
        banksel OSCTUNE         ;Reset to the carrier frequency
        clrf    OSCTUNE
        banksel PIR1            ;Make sure there's not a
        bcf     PIR1, TMR1IF    ; modulation timer interrupt pending
        banksel led_noterest_lat ;Turn off the "playing a note" LED
        bcf     led_noterest_lat, led_noterest_bit
        bra     nextloaded
isnote:
        banksel nextfh          ;Load the next note into the modulation timer
        movfw   nextfh          ;FIXME This makes a slight skip.  Nick has
        banksel freqh           ; a few ideas on dealing with this.  My
        movwf   freqh           ; preference is to only trigger interrupts
        banksel nextfl          ; for the modulation timer, and check to see
        movfw   nextfl          ; if the duration timer has expired at the
        banksel freql           ; modulation boundary.
        movwf   freql
        banksel T1CON           ;Enable the modulation timer
        bsf     T1CON, TMR1ON
        banksel led_noterest_lat ;Turn on the "playing a note" LED
        bsf     led_noterest_lat, led_noterest_bit
nextloaded:
        banksel T3CON           ;Disable the duration timer
        bcf     T3CON, TMR3ON
        banksel nextdh          ;Load the next note's duration into the timer
        movfw   nextdh
        banksel TMR3H
        movwf   TMR3H
        banksel nextdl
        movfw   nextdl
        banksel TMR3L
        movwf   TMR3L
        banksel T3CON           ;Enable the duration timer
        bsf     T3CON, TMR3ON
        movlw   1 << led_noteprog_bit ;Toggle the note progress LED
        banksel led_noteprog_lat
        xorwf   led_noteprog_lat, F
        banksel led_noterdy_lat ;Clear the "note ready" LED
        bcf     led_noterdy_lat, led_noterdy_bit
        banksel nextrdy         ;Inform the main loop that we need the
        clrf    nextrdy         ; next note
        retfie
isr_vec_durtim_end:

        ;; If you get here, there's a problem: there's an interrupt
        ;; that we're going to ignore.  Since its interrupt flag is
        ;; still set, we're about to go into an infinite loop if we
        ;; just RETFIE.  Instead, go into an infinite loop here so
        ;; you can find the problem.
        ;; FIXME Is this the right way to do an infinite loop?
        bra     $

;;;
;;; Code
;;;

        code                    ;Linker-chosen address for .code
start:
        movlw	b'11111000'     ;Engage the 32 MHz Fosc clock
        banksel	OSCCON
        movwf	OSCCON

        ;; Make sure PPSLOCK is cleared
        if 0
        ;; FIXME I think this is unnecessary; we put it in while debugging
        ;; a problem, and I haven't yet tested whether we can take it out.
        bcf INTCON, GIE      ;Suspend interrupts
        banksel PPSLOCK      ;Set bank
        movlw 0x55           ;Required dance, next 5 instructions
        movwf PPSLOCK
        movlw 0xAA
        movwf PPSLOCK
        ; Set PPSLOCKED bit to disable writes or
        ; Clear PPSLOCKED bit to enable writes
        bcf PPSLOCK, PPSLOCKED
        bsf INTCON, GIE         ; restore interrupts
	endif

        ;; Clear all I/O latches
        banksel LATA
        clrf    LATA
        banksel LATB
        clrf    LATB
        banksel LATC
        clrf    LATC
        
        ;; Un-tristate the output ports
        banksel ant_tris
        bcf     ant_tris, ant_bit
        banksel moddbg_tris
        bcf     moddbg_tris, moddbg_bit
        banksel led_noteprog_tris
        bcf     led_noteprog_tris, led_noteprog_bit
        banksel led_noterdy_tris
        bcf     led_noterdy_tris, led_noterdy_bit
        banksel led_noterest_tris
        bcf     led_noterest_tris, led_noterest_bit
        banksel led_running_tris
        bcf     led_running_tris, led_running_bit

        ;; Set up CLC1 to send Fosc to the antenna directly.
        banksel CLC1CON         ;Disable CLC1
        clrf    CLC1CON
        banksel CLC1SEL0        ;Set CLC1's input d1 as FOSC
        movlw   b'100001'
        movwf   CLC1SEL0
        banksel CLC1GLS0        ;Enable the gate for to pass input d1 to g1
        clrf	CLC1GLS0
        bsf	CLC1GLS0, LC1G1D1T
        banksel CLC1GLS1        ;Disable all other gates
        clrf    CLC1GLS1
        banksel CLC1GLS2
        clrf    CLC1GLS2
        banksel CLC1GLS3
        clrf    CLC1GLS3        ;Set the polarity for g1 to no
        banksel CLC1POL         ; inversions on input or output
        clrf    CLC1POL
        banksel CLC1CON         ;Set the logic circuit to OR-XOR
        movlw   b'00000001'
        movwf	CLC1CON
        banksel RC1PPS          ;Set the PPS to send LC0's output to pin RC1
        movlw   b'00000100'
        movwf   RC1PPS
        banksel CLC1CON         ;Enable CLC1
        bsf     CLC1CON, LC1EN
	
        ;; Cue the main loop to load a new note ASAP.
        banksel nextrdy
        bsf     nextrdy, 0

        ;; Inititalize global variables (if needed)
        movlw   4               ;Select the octave from middle C up
        banksel octave
        movwf   octave
        banksel notepos         ;Start at the first note
        clrf    notepos

        ;; Initialize the interrupt handlers
        banksel PIE1            ;Enable modulation timer
        bsf     PIE1, TMR1IE
        banksel PIE5            ;Enable duration timer
        bsf     PIE5, TMR3IE
        banksel INTCON          ;Enable interrupts on timer expiration
        bsf     INTCON, PEIE
        bsf     INTCON, GIE

        ;; Prepare the modulation timer.  We don't enable
        ;; it yet, but rather wait for the duration timer to do so
        ;; when it loads the first note.
        ;; 
        ;; The modulation timer's frequency is Fosc/8, which gives us
        ;; a range of several octaves roughly centered on middle C
        ;; using a 16-bit timer.
        ;;
        ;; Note that Fosc will be changing between 32 MHz and 31.92 MHz.
        ;; During a note, it will do so with approximately an even duty
        ;; cycle, so our frequency doesn't get offset.  During a rest,
        ;; we don't depend on Fosc.  FIXME clarify prev paragraph
        ;; FIXME nb Duration timer doesn't suffer from this; see below
        banksel T1CON
        movlw   b'01110000'     ;Fosc, scale down 1:8
        movwf   T1CON
        banksel T1GCON
        bcf     T1GCON, TMR1GE

        ;; Prepare the duration timer.
	;; This is the slowest timer we have available: LFINTOSC/8.
	;; Moreover, LFINTOSC doesn't depend on OSCTUNE.  That's
        ;; important (although possibly not perceptible), because
        ;; during a note, we keep changing OSCTUNE, but during a rest,
        ;; we don't.  Using LFINTOSC lets our rest duration stay
        ;; consistent with our note duration.
        banksel T3CON
        movlw   b'11110000'     ;LFINTOSC (32kHz), scale down 1:8
        movwf   T3CON
        banksel T3GCON
        bcf     T3GCON, TMR3GE
        banksel TMR3H           ;Load the timer's initial values.
        movlw   0xFD            ; This is just a brief moment to
        movwf   TMR3H           ; give the main loop time to load
        banksel TMR3L           ; the first note into the NEXT*
        movlw   0x00            ; variables.
        movwf   TMR3L
        banksel T3CON           ;Enable the duration timer
        bsf     T3CON, TMR3ON

        banksel LATC            ;Turn on the "running" LED
        bsf     LATC, LATC5

mainloop:
        ;; All this happens while the previous note is playing (or,
        ;; for the first note, in a delay we pre-arranged), so
        ;; there's no time pressure.
        
        ;; Load the next note from the piano roll.
        table_lookup_f pianoroll, notepos
        incf    notepos, F
        banksel notedurenc
        movwf   notedurenc

        ;; Is this a special value?
        xorlw   notedurenc_end
        btfss   STATUS, Z
        bra     not_notedurenc_end
        ;; Handler for NOTEDURENC_END: end of song.
	;; Sleep until S1 is pressed, then reset.
        ;; Note that this is called as soon as we read this note from
        ;; the piano roll, so there usually should be a "dummy" note
        ;; before this one.
        banksel INTCON
        bcf     INTCON, GIE     ;Disable interrupts
        bsf     INTCON, IOCIE   ;Enable interrupt-on-change
        banksel s1_iocp         ;Enable interrupt-on-change for S1
        bsf     s1_iocp, s1_bit
        banksel s1_port
wait_for_s1:                    ;Sleep until S1 is pressed
        ;; When the GIE bit is cleared, we won't go to the ISR vector.
        ;; But we will immediately exit a SLEEP if there's a pending
        ;; interrupt.  FIXME We should have turned off INTCON's PEIE
        ;; before getting here, to avoid interrupts from the timers.
        ;; We also should have disabled the transmitter.
        sleep
        btfss   s1_port, s1_bit
        bra     wait_for_s1
        ;; The switch was pressed.
        if 0
        ;; If we were doing something germane after the switch was
        ;; pressed (like going to the next song in a list), we'd
        ;; need to restore the interrupt configuration.  This code
        ;; is currently disabled.
        banksel INTCON
        bcf     INTCON, IOCIE  ;Disable interrupt-on-change
        bsf     INTCON, GIE    ;Enable interrupts
        endif
        reset                   ;Reset the CPU
not_notedurenc_end:
        movfw   notedurenc
        xorlw   notedurenc_octup
        btfss   STATUS, Z
        bra     not_notedurenc_octup
        ;; Handler for notedurenc_octup
        incf    octave, F
        bra     mainloop
not_notedurenc_octup:
        movfw   notedurenc
        xorlw   notedurenc_octdn
        btfss   STATUS, Z
        bra     not_notedurenc_octdn
        ;; Handler for notedurenc_octdn
        decf    octave, F
        bra     mainloop
not_notedurenc_octdn:
        ;; End of special value handling
	
        ;; Handle a normal note.
        ;; The low nybble of W is the encoded next note to play, and
        ;; the high nybble is the encoded duration.  Separate these,
        ;; and save them as ENCNOTE and ENCDUR.
        banksel notedurenc
        movfw   notedurenc
        andlw   0x0F
        banksel encnote
        movwf   encnote
        banksel notedurenc
        movfw   notedurenc
        banksel encdur
        movwf   encdur
        lsrf    encdur, F
        lsrf    encdur, F
        lsrf    encdur, F
        lsrf    encdur, F

        ;; Decode the note from ENCNOTE; save the timer durations into
        ;; NEXTFH and NEXTFL.  (They'll need additional massaging.)
        table_lookup_f notedechi, encnote
        banksel nextfh
        movwf   nextfh
        table_lookup_f notedeclo, encnote
        banksel nextfl
        movwf   nextfl
	
        ;; Check to see if this is a rest, in which case we handle
        ;; it slightly differently.
        banksel nextfh
        movf    nextfh, W
        banksel nextfl
        iorwf   nextfl, W
        btfsc   STATUS, Z
        bra     isrest
	
        ;; It's a normal note.  Shift the note value based on the octave.
        ;; 
        ;; (Prerequisite knowledge: Western music divides notes into
        ;; octaves, a repeating pattern of 12 notes.  You can see this
        ;; repeating pattern on a piano.  Going up by one octave doubles
        ;; the frequency.)
        ;; 
        ;; At this point, the NOTEFH and NOTEFL variables are
	;; populated based on the period (not frequency) of the lowest
        ;; note, since that gives us the highest precision.  We'll
        ;; shift that period right based on the octave currently
        ;; requested.  (This may lose precision, but the ear loses
	;; precision at high frequencies anyway.)        
        banksel octave
        movf    octave, W
        btfsc   STATUS, Z       ;Special case for OCTAVE==0
        bra     octave_done     ; FIXME Can we avoid this?
        banksel octave_tmp
        movwf   octave_tmp
octave_loop:
        banksel nextfh
        lsrf    nextfh, F
        banksel nextfl
        rrf     nextfl, F
        banksel octave_tmp
        decfsz  octave_tmp, F
        bra     octave_loop
octave_done:
	
        ;; We now have the (octave-adjusted) period in the NEXTFH and
        ;; NEXTFL registers.  But the timer will expire after counting
        ;; up from these values until it rolls over to 0, so what we
        ;; actually want is 0x10000-(nextfh:nextfl).  I currently cheat
        ;; this by just complementing these registers.
        ;; FIXME Don't cheat.
        banksel	nextfh
        comf	nextfh, F
        banksel nextfl
        comf	nextfl, F
	
isrest:                         ;FIXME Change the name
        ;; We now have the correct frequency loaded into NEXTFH
        ;; and NEXTFL,

        ;; Decode the duration.
        table_lookup_f  durdechi, encdur
        banksel nextdh
        movwf   nextdh
        table_lookup_f  durdeclo, encdur
        banksel nextdl
        movwf   nextdl

        ;; Mark that we've got a note loaded, and wait for the note
        ;; to be read
        banksel LATA
        bsf     LATA, LATA1
        banksel nextrdy
        movlw   0x01
        movwf   nextrdy
        pagesel mainloop
note_wait:
        btfss   nextrdy, 0
        goto    mainloop
        bra     note_wait

pianoroll:
        dt	0xc0, 0xc0, 0xc0, notedurenc_octdn, 0xc9, 0xc9, 0xc9
        dt      0xc5, 0xc5, 0xc5, 0xc0, 0xc0, 0xc0
        dt	0xc2, 0xc4, 0xc5, 0xc2, 0xc2, 0xc5
        dt	0xc0, 0xc0, 0xc0, 0xcc, 0xcc, 0xcc
	
        ;dt      0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc

        dt	0xc7, 0xc7, 0xc7, notedurenc_octup, 0xc0, 0xc0, 0xc0
        dt      notedurenc_octdn, 0xc9, 0xc9, 0xc9, 0xc5, 0xc5, 0xc5
        dt	0xc2, 0xc4, 0xc5, 0xc7, 0xc7, 0xc9
        dt	0xc7, 0xc7, 0xc7, 0xcc, 0xcc, 0xc7

        ;dt      0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc
	
        dt      0xd9, 0xdc, 0xc9, 0xc7
        dt      notedurenc_octup, 0xc0, 0xc0, notedurenc_octdn, 0xc9
        dt      0xc7, 0xc5, 0xc5, 0xcc, 0xcc, 0xc7
        dt      0xc9, 0xc9, 0xc5, 0xc2, 0xc2, 0xc5
        dt      0xc2, 0xc0, 0xc0, 0xcc, 0xcc, 0xc0

        ;dt      0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc
	
        dt      0xc7, 0xc7, notedurenc_octup, 0xc0, notedurenc_octdn
        dt      0xc9, 0xc9, 0xc5
        dt      0xc7, 0xc7, notedurenc_octup, 0xc0, notedurenc_octdn
        dt      0xd9, 0xdc, 0xc9, 0xca

        ;dt      0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc
	
        dt      notedurenc_octup, 0xc0, notedurenc_octdn, 0xc9, 0xc5
        dt      0xc7, 0xc7, 0xc0
        dt      0xc5, 0xc5, 0xc5, 0xcc, 0xcc, 0xcc
        
        ;dt      0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc
	
        dt	0x00, notedurenc_end

notedechi:
        dt      0xee, 0xe1, 0xd4, 0xc8, 0xbd, 0xb2
        dt      0xa8, 0x9f, 0x96, 0x8e, 0x86, 0x7e, 0x00
notedeclo:
        dt      0xe4, 0x7c, 0xd4, 0xe2, 0x9c, 0xf7
        dt      0xec, 0x71, 0x7e, 0x0c, 0x13, 0x8c, 0x00

durdechi:
        dt      0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff
        dt	0xd5, 0xee, 0xf5, 0xfa, 0xfd, 0xfe, 0xff, 0xff
durdeclo:
        dt      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        dt      0x56, 0xab, 0x56, 0xab, 0x56, 0xab, 0x56, 0xab

        end

;; Local Variables:
;; compile-command: "~/src/learn-pic/ide-container/mplabx /opt/microchip/mplabx/v4.20/mplab_platform/bin/make -C /home/user/MPLABXProjects/blink.X"
;; End:
