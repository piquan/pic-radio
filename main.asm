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

;;;
;;; Code
;;;
        
	CODE	; Linker-chosen address for .code
	
	;; DELAYSM: Do a quick delay loop
DELAYSM:
        CLRW
DELAYSM_LOOP:
	;; Insert NOPs here to slow this own a bit
        DECFSZ  WREG, F
	GOTO    DELAYSM_LOOP
	RETURN

	;; DELAY: Delay for somewhat longer than DELAYSM.  This is
	;; tuned to progress through the sine wave at the right speed.
DELAY:
        BANKSEL DELAYI
	;; Change the value here to slow this down a lot.  0x20 gives about
	;; a D#4 for the base frequency.  I'll probably tune this to a C
	;; once I've got it working better.
        MOVLW   0x20            ;0x20
        MOVWF   DELAYI
DELAY_LOOP:
        PAGESEL DELAYSM
        CALL    DELAYSM
        DECFSZ  DELAYI, F
	GOTO    DELAY_LOOP
	RETURN

	;; Play a note for 255 cycles.
PLAY:
        BANKSEL PLAYI
        CLRF    PLAYI
PLAYLOOP:
        ;; Adjust the oscillator frequency based on the phase of the inner
        ;; loop.  FIXME This is controlling both the transmitter oscillator
        ;; and the CPU clock.  This code doesn't account for the fact that
        ;; we'll be running at different speeds in different phases of the
        ;; loop.
        PAGESEL SINE            ;Can't hoist; we INCF PCLATH later.
        MOVLW   SINE
        ;; FIXME To change the modulation frequency:
        ;; * Take an argument as MODFREQ
        ;; * Keep a register MODRES that's initialized to 0
        ;; * Each iteration, add MODFREQ to MODRES
        ;; * If it carries, add 1 to W at this point in the code.
        ;; To tune to k half-steps above the baseline, set MODFREQ to
        ;; 256*(1-(\sqrt[12]{2}^k)).  If baseline is C, that gives us:
        ;;     C: 0    C#: 15   D: 31    D#: 48
        ;;     E: 67   F: 86    F#: 106  G: 128
        ;;     G#: 150 A: 175   A#: 200  B: 227
        ;; Calcuated via the following code:
        ;;     (loop
        ;;        for k from 0 to 12
        ;;        collect (round (* 256 (- (expt 2 (/ k 12)) 1))))
	;; This only lets you go up by just less than one octave.  To
	;; get the octave note, always add 1 to W at this point.  You
	;; can pretty much fake that by putting 254 into MODFREQ.
        ;; 
        ;; To access notes in the higher octave, pass in an OCTAVE
	;; argument, and BTFSC OCTAVE, 0; LSLF W, W; after the previous
	;; steps.  For higher octaves, repeat this several times for
	;; different bits, and encode the number of octaves as the
        ;; number of bits that are set in the OCTAVE register.
        ;; 
        ;; (Note: to play Daisy Bell in F4, you need C4-C5.)
        BANKSEL PLAYI
        ADDWF   PLAYI, W
        BTFSC   STATUS, C
        INCF    PCLATH, F
        CALLW
        ;; If you want to adjust the amplitude, now's the time.
        BANKSEL OSCTUNE
        MOVWF   OSCTUNE

        ;; Toggle RC2, so we can check the modulation frequency
        ;; on an oscilloscope more easily.  Remember that this will
        ;; run at *half* the frequency of the sine wave.
        ;; You can put a speaker on this if you want to hear the
        ;; frequency (down an octave).
	BANKSEL	LATC
	MOVLW	4
        XORWF	LATC, F
	
        PAGESEL DELAY
        CALL    DELAY

        DECFSZ  PLAYI, F
        PAGESEL PLAYLOOP
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
	MOVLW   b'11011111'
	MOVWF   TRISA
	
MAINLOOP:
	
        BANKSEL LATA
        MOVLW   h'00'
        MOVWF   LATA
        PAGESEL PLAY
	CALL	PLAY
        BANKSEL LATA
        MOVLW   h'20'
        MOVWF   LATA
        PAGESEL PLAY
	CALL	PLAY

	GOTO MAINLOOP

SINE    CODE
;; The table here is de-sine-d (sorry) to be used with the OSCTUNE
;; register, which takes a 6-bit, 2s-complement signed value.  The input
;; to the table is an unsigned 8-bit number, since it's coming from the
;; W register.
;; 
;; The table was generated with this code:
;;     (format t "SINE:~{   DT      0x~16,2,'0r, 0x~16,2,'0r, 0x~16,2,'0r, 0x~16,2,'0r, 0x~16,2,'0r, 0x~16,2,'0r, 0x~16,2,'0r, 0x~16,2,'0r~%~#[~:;     ~]~}"
;;             (loop
;;                 for i from 0 to 255
;;                 for s = (+ 31.5 (* 31.5 (sin (* i (/ (* 2 pi) 255)))))
;;                 collect (logand #x3f (+ #x20 (round s)))))
;; Some other format strings I've used:
;;     "SINE:   DT      ~{0x~16,2,'0r~#[~:;, ~]~}~%"
;;     "SINE:~{   DT      ~8@{0x~16,2,'0r~#[~:;, ~]~}~%~#[~:;     ~]~}"
	
;; FIXME Sign-extend the 0x20 bit so that this can be right-shifted
;; to adjust the amplitude.
SINE:   DT      0x00, 0x00, 0x01, 0x02, 0x03, 0x03, 0x04, 0x05
        DT      0x06, 0x06, 0x07, 0x08, 0x09, 0x09, 0x0A, 0x0B
        DT      0x0C, 0x0C, 0x0D, 0x0E, 0x0E, 0x0F, 0x10, 0x10
        DT      0x11, 0x12, 0x12, 0x13, 0x14, 0x14, 0x15, 0x15
        DT      0x16, 0x16, 0x17, 0x17, 0x18, 0x18, 0x19, 0x19
        DT      0x1A, 0x1A, 0x1B, 0x1B, 0x1B, 0x1C, 0x1C, 0x1C
        DT      0x1D, 0x1D, 0x1D, 0x1D, 0x1E, 0x1E, 0x1E, 0x1E
        DT      0x1E, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F
        DT      0x1F, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, 0x1E
        DT      0x1E, 0x1E, 0x1E, 0x1E, 0x1E, 0x1D, 0x1D, 0x1D
        DT      0x1D, 0x1C, 0x1C, 0x1C, 0x1B, 0x1B, 0x1A, 0x1A
        DT      0x1A, 0x19, 0x19, 0x18, 0x18, 0x17, 0x17, 0x16
        DT      0x16, 0x15, 0x14, 0x14, 0x13, 0x13, 0x12, 0x11
        DT      0x11, 0x10, 0x0F, 0x0F, 0x0E, 0x0D, 0x0D, 0x0C
        DT      0x0B, 0x0B, 0x0A, 0x09, 0x08, 0x08, 0x07, 0x06
        DT      0x05, 0x05, 0x04, 0x03, 0x02, 0x01, 0x01, 0x00
        DT      0x3F, 0x3E, 0x3E, 0x3D, 0x3C, 0x3B, 0x3A, 0x3A
        DT      0x39, 0x38, 0x37, 0x37, 0x36, 0x35, 0x34, 0x34
        DT      0x33, 0x32, 0x32, 0x31, 0x30, 0x30, 0x2F, 0x2E
        DT      0x2E, 0x2D, 0x2C, 0x2C, 0x2B, 0x2B, 0x2A, 0x29
        DT      0x29, 0x28, 0x28, 0x27, 0x27, 0x26, 0x26, 0x25
        DT      0x25, 0x25, 0x24, 0x24, 0x23, 0x23, 0x23, 0x22
        DT      0x22, 0x22, 0x22, 0x21, 0x21, 0x21, 0x21, 0x21
        DT      0x21, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20
        DT      0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x21
        DT      0x21, 0x21, 0x21, 0x21, 0x22, 0x22, 0x22, 0x22
        DT      0x23, 0x23, 0x23, 0x24, 0x24, 0x24, 0x25, 0x25
        DT      0x26, 0x26, 0x27, 0x27, 0x28, 0x28, 0x29, 0x29
        DT      0x2A, 0x2A, 0x2B, 0x2B, 0x2C, 0x2D, 0x2D, 0x2E
        DT      0x2F, 0x2F, 0x30, 0x31, 0x31, 0x32, 0x33, 0x33
        DT      0x34, 0x35, 0x36, 0x36, 0x37, 0x38, 0x39, 0x39
        DT      0x3A, 0x3B, 0x3C, 0x3C, 0x3D, 0x3E, 0x3F, 0x3F

        END

;; Local Variables:
;; compile-command: "~/src/learn-pic/ide-container/mplabx /opt/microchip/mplabx/v4.20/mplab_platform/bin/make -C /home/user/MPLABXProjects/blink.X"
;; End:
