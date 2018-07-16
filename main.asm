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

;;; 
;;; Config and vectors
;;; 
	
        ; We need FOSC_INTOSC.
        __CONFIG _CONFIG1, _FOSC_INTOSC & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _BOREN_ON & _CLKOUTEN_OFF & _IESO_ON & _FCMEN_ON
        ; Make sure LVP stays on.
        __CONFIG _CONFIG2, _WRT_OFF & _PPS1WAY_OFF & _ZCD_OFF & _PLLEN_OFF & _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON 
        __CONFIG _CONFIG3, _WDTCPS_WDTCPS1F & _WDTE_OFF & _WDTCWS_WDTCWSSW & _WDTCCS_SWC 

; Set up the reset vector at 0
; We give this a name, because otherwise the assembler will try to put it in
; the .code segment, and then when combined with the main program, that would
; become non-contiguous.
RST_VEC CODE    0x0000
        GOTO    START

; The ISR is at 0x0004, but we don't actually use it.
; The 16Fxxxx devices don't need any context saving, BTW.
ISR_VEC CODE    0x0004
	BANKSEL PIR1
        BTFSS   PIR1, TMR1IF
        GOTO    ISR_VEC_MODTIM_END
        ;; Clear the interrupt bit
        BCF     PIR1, TMR1IF
        ;; Toggle the OSCTUNE bit
        MOVLW   1
        BANKSEL OSCTUNE
        XORWF   OSCTUNE, F
        ;; Toggle the debugging port
	BANKSEL	LATC
	MOVLW	4
        XORWF	LATC, F
        ;; Disable timer1
        BANKSEL T1CON
        BCF     T1CON, TMR1ON
        ;; Reset the timer
        BANKSEL FREQH
        MOVFW   FREQH
        BANKSEL TMR1H
        MOVWF   TMR1H
        BANKSEL FREQL
        MOVFW   FREQL
        BANKSEL TMR1L
        MOVWF   TMR1L
        ;; Enable timer1
        BANKSEL T1CON
        BSF     T1CON, TMR1ON
        RETFIE
ISR_VEC_MODTIM_END:
        BANKSEL PIR5
        BTFSS   PIR5, TMR3IF
        GOTO    ISR_VEC_DURTIM_END
        BCF     PIR5, TMR3IF
	;; Prepare to reconfigure timer 1.
        BANKSEL T1CON
        BCF     T1CON, TMR1ON
        ;; Is the next note a rest?
        BANKSEL NEXTFH
        MOVFW   NEXTFH
        BANKSEL NEXTFL
        IORWF   NEXTFL, W
        BTFSS   STATUS, Z
        BRA     ISNOTE
        ;; It's a rest.  Turn timer1 off, and reset to the carrier.
        BANKSEL T1CON
        BCF     T1CON, TMR1ON
        BANKSEL OSCTUNE
        CLRF    OSCTUNE
	;; Make sure we don't have a timer interrupt pending.
        BANKSEL PIR1
        BCF     PIR1, TMR1IF
        ;; Turn off the "playing a note" LED
        BANKSEL LATA
        BCF     LATA, LATA2
        BRA     NEXTLOADED
ISNOTE:
        ;; Load the next note
        BANKSEL NEXTFH
        MOVFW   NEXTFH
        BANKSEL FREQH
        MOVWF   FREQH
        BANKSEL NEXTFL
        MOVFW   NEXTFL
        BANKSEL FREQL
        MOVWF   FREQL
	;; Enable timer1.
        BANKSEL T1CON
        BSF     T1CON, TMR1ON
        ;; Turn on the "playing a note" LED
        BANKSEL LATA
        BSF     LATA, LATA2
NEXTLOADED:     
        ;; Disable timer3
        BANKSEL T3CON
        BCF     T3CON, TMR3ON
        ;; Reset the timer
        BANKSEL NEXTDH
        MOVFW   NEXTDH
        BANKSEL TMR3H
        MOVWF   TMR3H
        BANKSEL NEXTDL
        MOVFW   NEXTDL
        BANKSEL TMR3L
        MOVWF   TMR3L
        ;; Enable timer3
        BANKSEL T3CON
        BSF     T3CON, TMR3ON
        ;; Toggle the note progress LED
	BANKSEL LATA
        MOVLW   h'20'
        XORWF   LATA, F	
        ;; Clear the "note ready" LED
        BANKSEL LATA
        BCF     LATA, LATA1
        ;; Inform the main loop that we need the next note
        BANKSEL NEXTRDY
        CLRF    NEXTRDY
        RETFIE
ISR_VEC_DURTIM_END:
        RETFIE

;;; 
;;; Variables
;;; 

        UDATA
NEXTDH  RES     1
NEXTDL  RES     1
NEXTFH  RES     1
NEXTFL  RES     1
NEXTRDY RES     1
FREQH   RES     1
FREQL   RES     1
OCTAVE  RES     1
OCTAVE_TMP RES  1
NOTEPOS RES     1
NOTEDURENC RES  1
ENCNOTE RES     1
ENCDUR  RES     1
	
NOTEDURENC_RESET EQU 0xF
NOTEDURENC_OCTUP EQU 0xE
NOTEDURENC_OCTDN EQU 0xD

;;;
;;; Code
;;;
        
	CODE	; Linker-chosen address for .code
START:   
        ;; Set the OSCCON register to engage the 32 MHz FOSC clock.
	BANKSEL	OSCCON
	MOVLW	b'11111000'
	MOVWF	OSCCON
    
	;; Make sure PPSLOCK is cleared
	;; FIXME I think this is unnecessary.
        BCF INTCON, GIE      ; suspend interrupts
        BANKSEL PPSLOCK      ; set bank
        ;; required sequence, next 5 instructions
        MOVLW 0x55
        MOVWF PPSLOCK
        MOVLW 0xAA
        MOVWF PPSLOCK
        ;; Set PPSLOCKED bit to disable writes or
        ;; Clear PPSLOCKED bit to enable writes
        BCF PPSLOCK, PPSLOCKED
        BSF INTCON, GIE         ; restore interrupts

	;;
	;; The next block of code sets up CLC1 to send Fosc to port C1.
	;; There's currently no gating or modulation on it.
        ;; (Thought: gating circuitry might be used to adjust the
        ;; frequency, instead of using OSCTUNE.)
	;;
	
        ;; Disable CLC1
        BANKSEL CLC1CON
        MOVLW   0
        MOVWF   CLC1CON
	
        ;; Set CLC1's input d1 as FOSC
        BANKSEL CLC1SEL0
	MOVLW   b'100001'
        MOVWF   CLC1SEL0
        ;; Enable the gate for to pass input d1 to g1
        BANKSEL CLC1GLS0
	CLRF	CLC1GLS0
	BSF	CLC1GLS0, LC1G1D1T
        ;; Disable all other gates
        BANKSEL CLC1GLS1
        CLRF    CLC1GLS1
        BANKSEL CLC1GLS2
        CLRF    CLC1GLS2
        BANKSEL CLC1GLS3
        CLRF    CLC1GLS3
        ;; Set the polarity for g1 to no inversions on input or output
        BANKSEL CLC1POL
        CLRF    CLC1POL
        ;; Set the logic circuit to OR-XOR
        BANKSEL CLC1CON
        MOVLW   b'00000001'
	MOVWF	CLC1CON
        ;; Set the PPS to send LC0's output to pin RC1
        BANKSEL RC1PPS
	MOVLW   b'00000100'
        MOVWF   RC1PPS
	
	;; Clear the tristate for RC1 and RC2
	BANKSEL PORTC
	CLRF    PORTC
	BANKSEL LATC
	CLRF    LATC
	BANKSEL ANSELC
	CLRF    ANSELC
	BANKSEL TRISC
        BCF     TRISC, TRISC1   ;Antenna
        BCF     TRISC, TRISC2   ;Debug port for modulating signal
        BCF     TRISC, TRISC5   ;LED D7 on when running

        ;; Enable CLC1
        BANKSEL CLC1CON
        BSF     CLC1CON, LC1EN
    
	;;
	;; End CLC1 config
	;;
	
        ;; Clear the tristate for the LEDs on port A
        BANKSEL PORTA
	CLRF    PORTA
	BANKSEL LATA
	CLRF    LATA
	BANKSEL ANSELA
	CLRF    ANSELA
	BANKSEL TRISA
        BCF     TRISA, TRISA1   ;LED D5 on when note ready
        BCF     TRISA, TRISA2   ;LED D6 on when playing a note (vs rest)
        BCF     TRISA, TRISA5   ;LED D4 toggle on each note
	
        ;; Initialize the interrupt handler
        BANKSEL PIE1
        BSF     PIE1, TMR1IE
        BSF     PIE5, TMR3IE
        BANKSEL INTCON
        BSF     INTCON, PEIE
        BSF     INTCON, GIE
	
        ;; Prepare timer1 (modulation half-period).  We don't enable
        ;; it yet, but rather wait for timer 3 to do so.
	BANKSEL T1CON
        MOVLW   b'01110000'
        MOVWF   T1CON
        BANKSEL T1GCON
        BCF     T1GCON, TMR1GE
	
        ;; Prepare timer3 (duration)
	BANKSEL T3CON
        MOVLW   b'11110000'
        MOVWF   T3CON
        BANKSEL T3GCON
        BCF     T3GCON, TMR3GE
        ;; Load timer3's initial values.  This is a short moment to
        ;; let the main loop load the first note into NEXT*.
        BANKSEL TMR3H
        MOVLW   0xF0
        MOVWF   TMR3H
        BANKSEL TMR3L
        MOVLW   0x00
        MOVWF   TMR3L
	
        ;; Cue the main loop to load a new note ASAP.
	BANKSEL NEXTRDY
        BSF     NEXTRDY, 0
	
        ;; Initialize the octave
        MOVLW   4
        BANKSEL OCTAVE
        MOVWF   OCTAVE
	BANKSEL NOTEPOS
        CLRF    NOTEPOS

        ;; Enable timer3
        BANKSEL T3CON
        BSF     T3CON, TMR3ON

	;; Enable the "running" LED
	BANKSEL LATC
        BSF     LATC, LATC5
	
MAINLOOP:	
	;; Load the next note from the piano roll.
        PAGESEL PIANOROLL
        MOVLW   PIANOROLL
        BANKSEL NOTEPOS
        ADDWF   NOTEPOS, W
        BTFSC   STATUS, C
        INCF    PCLATH, F
        CALLW
        INCF    NOTEPOS, F
	BANKSEL NOTEDURENC
        MOVWF   NOTEDURENC

	;; Is this a special value?
        XORLW   NOTEDURENC_RESET
        BTFSS   STATUS, Z
        BRA     NOT_NOTEDURENC_RESET
	RESET
NOT_NOTEDURENC_RESET:
        MOVFW   NOTEDURENC
        XORLW   NOTEDURENC_OCTUP
        BTFSS   STATUS, Z
        BRA     NOT_NOTEDURENC_OCTUP
        INCF    OCTAVE, F
        BRA     MAINLOOP
NOT_NOTEDURENC_OCTUP:
        MOVFW   NOTEDURENC
        XORLW   NOTEDURENC_OCTDN
        BTFSS   STATUS, Z
        BRA     NOT_NOTEDURENC_OCTDN
        DECF    OCTAVE, F	
        BRA     MAINLOOP
NOT_NOTEDURENC_OCTDN:

        ;; The low high of W is now the encoded next note to play, and
        ;; the high half is the encoded duration.  Separate these.
	BANKSEL NOTEDURENC
        MOVFW   NOTEDURENC
        ANDLW   0x0F
        BANKSEL ENCNOTE
        MOVWF   ENCNOTE
	BANKSEL NOTEDURENC
        MOVFW   NOTEDURENC
        BANKSEL ENCDUR
        MOVWF   ENCDUR
	LSRF    ENCDUR, F
	LSRF    ENCDUR, F
	LSRF    ENCDUR, F
	LSRF    ENCDUR, F

	;; Decode the note.
        PAGESEL NOTEDECHI
        MOVLW   NOTEDECHI
        BANKSEL ENCNOTE
        ADDWF   ENCNOTE, W
        BTFSC   STATUS, C
        INCF    PCLATH, F
        CALLW
	BANKSEL NEXTFH
        MOVWF   NEXTFH
        PAGESEL NOTEDECLO
        MOVLW   NOTEDECLO
        BANKSEL ENCNOTE
        ADDWF   ENCNOTE, W
        BTFSC   STATUS, C
        INCF    PCLATH, F
        CALLW
	BANKSEL NEXTFL
        MOVWF   NEXTFL
	
        BANKSEL NEXTFH
        MOVF    NEXTFH, W
        BANKSEL NEXTFL
        IORWF   NEXTFL, W
        BTFSC   STATUS, Z
        BRA     ISREST
	
	;; Shift the note value based on the octave.
        BANKSEL OCTAVE
        MOVF    OCTAVE, W
        BTFSC   STATUS, Z
        BRA     OCTAVE_DONE
        BANKSEL OCTAVE_TMP
        MOVWF   OCTAVE_TMP
OCTAVE_LOOP:	
        BANKSEL NEXTFH
        LSRF    NEXTFH, F
        BANKSEL NEXTFL
        RRF     NEXTFL, F
        BANKSEL OCTAVE_TMP
        DECFSZ  OCTAVE_TMP, F
        BRA     OCTAVE_LOOP
OCTAVE_DONE:    

	BANKSEL	NEXTFH
	COMF	NEXTFH, F
	BANKSEL NEXTFL
	COMF	NEXTFL, F
    
ISREST:
        ;; Decode the duration.
        PAGESEL DURDECHI
        MOVLW   DURDECHI
        BANKSEL ENCDUR
        ADDWF   ENCDUR, W
        BTFSC   STATUS, C
        INCF    PCLATH, F
        CALLW
	BANKSEL NEXTDH
        MOVWF   NEXTDH
        PAGESEL DURDECLO
        MOVLW   DURDECLO
        BANKSEL ENCDUR
        ADDWF   ENCDUR, W
        BTFSC   STATUS, C
        INCF    PCLATH, F
        CALLW
	BANKSEL NEXTDL
        MOVWF   NEXTDL
	
        ;; Mark that we've got a note loaded, and wait for the note
        ;; to be read
        BANKSEL LATA
        BSF     LATA, LATA1
	BANKSEL NEXTRDY
        MOVLW   0x01
        MOVWF   NEXTRDY
        PAGESEL MAINLOOP
NOTE_WAIT:
	SLEEP
        BTFSS   NEXTRDY, 0
	GOTO    MAINLOOP
        BRA     NOTE_WAIT

PIANOROLL:
	DT	0xc0, 0xc0, 0xc0, NOTEDURENC_OCTDN, 0xc9, 0xc9, 0xc9
        DT      0xc5, 0xc5, 0xc5, 0xc0, 0xc0, 0xc0
	DT	0xc2, 0xc4, 0xc5, 0xc2, 0xc2, 0xc5
	DT	0xc0, 0xc0, 0xc0, 0xcc, 0xcc, 0xcc
	
	DT	0xc7, 0xc7, 0xc7, NOTEDURENC_OCTUP, 0xc0, 0xc0, 0xc0
        DT      NOTEDURENC_OCTDN, 0xc9, 0xc9, 0xc9, 0xc5, 0xc5, 0xc5
	DT	0xc2, 0xc4, 0xc5, 0xc7, 0xc7, 0xc9
	DT	0xc7, 0xc7, 0xc7, 0xcc, 0xcc, 0xc7
	
        DT      0xd9, 0xdc, 0xc9, 0xc7
        DT      NOTEDURENC_OCTUP, 0xc0, 0xc0, NOTEDURENC_OCTDN, 0xc9
        DT      0xc7, 0xc5, 0xc5, 0xcc, 0xcc, 0xc7
        DT      0xc9, 0xc9, 0xc5, 0xc2, 0xc2, 0xc5
        DT      0xc2, 0xc0, 0xc0, 0xcc, 0xcc, 0xc0

        DT      0xc7, 0xc7, NOTEDURENC_OCTUP, 0xc0, NOTEDURENC_OCTDN
        DT      0xc9, 0xc9, 0xc5
        DT      0xc7, 0xc7, NOTEDURENC_OCTUP, 0xc0, NOTEDURENC_OCTDN
	DT      0xd9, 0xdc, 0xc9, 0xca
	
        DT      NOTEDURENC_OCTUP, 0xc0, NOTEDURENC_OCTDN, 0xc9, 0xc5
        DT      0xc7, 0xc7, 0xc0
        DT      0xc5, 0xc5, 0xc5, 0xcc, 0xcc, 0xcc

	DT	0x00, NOTEDURENC_RESET
	
NOTEDECHI:
        DT      0xEE, 0xE1, 0xD4, 0xC8, 0xBD, 0xB2
        DT      0xA8, 0x9F, 0x96, 0x8E, 0x86, 0x7E, 0x00
NOTEDECLO:
        DT      0xE4, 0x7C, 0xD4, 0xE2, 0x9C, 0xF7
        DT      0xEC, 0x71, 0x7E, 0x0C, 0x13, 0x8C, 0x00

DURDECHI:
        DT      0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0xFF
	DT	0xD5, 0xEE, 0xF5, 0xFA, 0xFD, 0xFE, 0xFF, 0xFF
DURDECLO:
        DT      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        DT      0x56, 0xAB, 0x56, 0xAB, 0x56, 0xAB, 0x56, 0xAB
        
        END

;; Local Variables:
;; compile-command: "~/src/learn-pic/ide-container/mplabx /opt/microchip/mplabx/v4.20/mplab_platform/bin/make -C /home/user/MPLABXProjects/blink.X"
;; End:
