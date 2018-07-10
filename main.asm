#include "p16f1619.inc" 

;;; I/O peripherials:
;; Antenna: RC1
        ;; XXX It would be better to use RC4 or RC5 for the antenna,
        ;; since those can be put in high drive strength mode.  But
        ;; note that the board has assignments for those already.
;; Modulation debug port: RC2
;; LED D4: RA5
;; LED D5: RA1
;; LED D6: RA2
;; LED D7: RC5
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
        ;; Is the next note a rest?
        BANKSEL NEXTFH
        MOVFW   NEXTFH
        BANKSEL NEXTFL
        IORWF   NEXTFL, W
        BTFSS   STATUS, Z
        BRA     ISNOTE
        ;; It's a rest.  Leave the transmitter on, but disable timer1.
        BANKSEL T1CON
        BCF     T1CON, TMR1ON
        BANKSEL OSCTUNE
        CLRF    OSCTUNE
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
	;; Disable timer1.
        BANKSEL T1CON
        BSF     T1CON, TMR1ON
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
	MOVLW   b'11111001'
	MOVWF   TRISC

        ;; Enable CLC1
        BANKSEL CLC1CON
        BSF     CLC1CON, LC1EN
    
	;;
	;; End CLC1 config
	;;
	
        ;; Clear the tristate for RA5 (LED D4)
        BANKSEL PORTA
	CLRF    PORTA
	BANKSEL LATA
	CLRF    LATA
	BANKSEL ANSELA
	CLRF    ANSELA
	BANKSEL TRISA
        BCF     TRISA, TRISA5
	
        ;; Initialize the interrupt handler
        BANKSEL PIE1
        BSF     PIE1, TMR1IE
        BSF     PIE5, TMR3IE
        BANKSEL INTCON
        BSF     INTCON, PEIE
        BSF     INTCON, GIE
	
        ;; Prepare timer1 (modulation half-period)
	BANKSEL T1CON
        MOVLW   b'01110000'
        MOVWF   T1CON
        BANKSEL T1GCON
        BCF     T1GCON, TMR1GE
        ;; Load timer1's initial values
        BANKSEL TMR1H
        MOVLW   0xee
        MOVWF   TMR1H
        BANKSEL TMR1L
        MOVLW   0x3f
        MOVWF   TMR1L
	
        ;; Prepare timer3 (duration)
	BANKSEL T3CON
        MOVLW   b'11110000'
        MOVWF   T3CON
        BANKSEL T3GCON
        BCF     T3GCON, TMR3GE
        ;; Load timer3's initial values
        BANKSEL TMR3H
        MOVLW   0xFF
        MOVWF   TMR3H
        BANKSEL TMR3L
        MOVLW   0x00
        MOVWF   TMR3L
	
        ;; Load the next note values
        BANKSEL NEXTFH
        MOVLW   0xee
        MOVWF   NEXTFH
        BANKSEL NEXTFL
        MOVLW   0x3f
        MOVWF   NEXTFL
        BANKSEL NEXTDH
        MOVLW   0xff
        MOVWF   NEXTDH
        BANKSEL NEXTDL
        MOVLW   0x00
        MOVWF   NEXTDL
	BANKSEL NEXTRDY
        BSF     NEXTRDY, 0
	
        ;; Enable timer1
        BANKSEL T1CON
        BSF     T1CON, TMR1ON
        ;; Enable timer3
        BANKSEL T3CON
        BSF     T3CON, TMR3ON

	;; Enable the note progress LED
	BANKSEL LATA
        BSF     LATA, LATA5

MAINLOOP:
PLAYLOW:
	BANKSEL NEXTRDY
        PAGESEL PLAYLOW
        BTFSC   NEXTRDY, 0
	GOTO    PLAYLOW

        BANKSEL NEXTFH
        MOVLW   0xdc
        MOVWF   NEXTFH
        BANKSEL NEXTFL
        MOVLW   0x7e
        MOVWF   NEXTFL
        DECF    NEXTDH
	BANKSEL NEXTRDY
        BSF     NEXTRDY, 0

PLAYHIGH:
	BANKSEL NEXTRDY
        PAGESEL PLAYHIGH
        BTFSC   NEXTRDY, 0
	GOTO    PLAYHIGH

        BANKSEL NEXTFH
        MOVLW   0x00
        MOVWF   NEXTFH
        BANKSEL NEXTFL
        MOVLW   0x00
        MOVWF   NEXTFL
        DECF    NEXTDH
	BANKSEL NEXTRDY
        BSF     NEXTRDY, 0

	PAGESEL PLAYLOW
        GOTO    PLAYLOW

        END

;; Local Variables:
;; compile-command: "~/src/learn-pic/ide-container/mplabx /opt/microchip/mplabx/v4.20/mplab_platform/bin/make -C /home/user/MPLABXProjects/blink.X"
;; End:
