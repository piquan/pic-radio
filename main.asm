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
        RETFIE

;;; 
;;; Variables
;;; 

        UDATA
PLAYI   RES	1
DELAYI  RES     1
PLAYCNT RES     1
DELAYCNT RES    1
SMDELAYCNT EQU 0x20

;;;
;;; Code
;;;
        
	CODE	; Linker-chosen address for .code
	
	;; DELAYSM: Do a quick delay loop
DELAYSM:
        MOVLW   SMDELAYCNT
        PAGESEL DELAYSM_LOOP
DELAYSM_LOOP:
	;; Insert NOPs here to slow this own a bit
        DECFSZ  WREG, F
	GOTO    DELAYSM_LOOP
	RETURN

	;; DELAY: Delay for somewhat longer than DELAYSM.  This is
	;; tuned to progress through the sine wave at the right speed.
DELAY:
        BANKSEL DELAYCNT
        MOVFW   DELAYCNT
        BANKSEL DELAYI
        MOVWF   DELAYI
DELAY_LOOP:
        PAGESEL DELAYSM
        CALL    DELAYSM
        PAGESEL DELAY_LOOP
        DECFSZ  DELAYI, F
	GOTO    DELAY_LOOP
	RETURN

	;; Play a note
PLAY:
        BANKSEL PLAYCNT
        MOVFW   PLAYCNT
        BANKSEL PLAYI
        MOVWF   PLAYI
PLAYLOOP:
        MOVLW   1
        BANKSEL OSCTUNE
        XORWF   OSCTUNE, F
	CALL    DELAY

        ;; Toggle RC2, so we can check the modulation frequency
        ;; on an oscilloscope more easily.  You can put a speaker on
	;; this if you want to hear the frequency (down an octave).
	BANKSEL	LATC
	MOVLW	4
        XORWF	LATC, F
	
        PAGESEL DELAY
        CALL    DELAY
	
        PAGESEL PLAYLOOP
        BANKSEL PLAYI
        DECFSZ  PLAYI, F
        GOTO    PLAYLOOP
        BANKSEL OSCTUNE
        CLRF    OSCTUNE
	RETURN

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
	
        ;; Initialize the sound
        BANKSEL PLAYCNT
        MOVLW   0x80
        MOVWF   PLAYCNT
        BANKSEL DELAYCNT
        MOVLW   0x40
        MOVWF   DELAYCNT

MAINLOOP:
	
        BANKSEL LATA
        BSF     LATA, LATA5
        PAGESEL PLAY
	CALL	PLAY
        BANKSEL LATA
        BCF     LATA, LATA5
        PAGESEL PLAY
	CALL	PLAY
        PAGESEL MAINLOOP
        BANKSEL DELAYCNT
	DECFSZ  DELAYCNT
        GOTO    MAINLOOP
        MOVLW   0x40
        MOVWF   DELAYCNT
        GOTO    MAINLOOP

        END

;; Local Variables:
;; compile-command: "~/src/learn-pic/ide-container/mplabx /opt/microchip/mplabx/v4.20/mplab_platform/bin/make -C /home/user/MPLABXProjects/blink.X"
;; End:
