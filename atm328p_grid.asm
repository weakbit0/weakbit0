; 
; atm328p_grid.asm
;
; Created: 10.04.2023
; Author : weakbit
;
;for Windows
;.include "C:\Program Files (x86)\Atmel\Studio\7.0\packs\atmel\ATmega_DFP\1.3.300\avrasm\inc\m328pdef.inc"
;for Linux
.include "m328pdef.inc"

;reg def - registers are all 8bit
.def	temp		= r16	;temorary data storage
.def	temp2		= r17	
.def	temp3		= r18
.def	counter		= r19	;blinky blinky counter
.def	status		= r20	;MSB(7),(6),(5),(4),(3),from_pcint0(2),from_int1(1),from_int0(0)LSB  

;equate port, bit def
.equ	bit_red_led	= 0	;portx red LED
.equ	bit_yellow_led	= 1	;portx yellow LED
.equ	bit_green_led	= 2	;portx green LED
.equ 	bit_white_led	= 3   	;portx white LED
.equ	bit_blue_led	= 4	;portx blue LED
.equ	bit_oszi	= 5	;portx oszilloscope pin out measurement


;status reg
.equ	bit_from_int0	= 0	;signal a extern int0
.equ	bit_from_int1	= 1	;singal a extern int1
.equ	bit_from_pcint20= 2	;signal a extern pcint20

;numbers & names
.equ	null		= 0x00	;hex number for '0'
.equ	one		= 0x01	;hex number for '1'
.equ	eight		= 0x08	;hex number for '8'
.equ	ff		= 0xff	;hex number for '255' this is 0xff

;compare end mask
;here comes the bis mask

;port def
;portb
.equ	init_portb	= 0b00000000	;(7),(6),(5),(4),(3),(2),(1),(0)
.equ	init_ddrb	= 0b00000000	;(7),(6),(5),(4),(3),(2),(1),(0)
;portc
.equ    init_portc      = 0b00100000    ;(7),(6),oszi(5),blue(4),white(3),green(2),yellow(1),red(0) LED out
.equ    init_ddrc       = 0b00111111    ;(7),(6),out(5),out(4),out(3),out(2),out(1),out(0)
;portd
.equ    init_portd      = 0b00000000    ;(7),(6),(5),pcint20(4),int1(3),int0(2),(1),(0)
.equ    init_ddrd       = 0b10000000    ;open(7),(6),(5),pcint20_in(4),int1_in(3),int0_in(2),(1),(0)

;timer2 page 116
;tccr2a page 127			;COM2A1(7),COM2A0(6),COM2B1(5),COM2B0(4),-(3),-(2),WGM21(1),WGM20(0)
.equ	tim2_wgm21	= 0b00000010	;TCCR2A <= WGM21(1)=1
;tccr2b page 130
.equ	tim2_stop	= 0x00
.equ	tim2_ctc_cs1024	= 0b00000111	;COM2A1(7),COM2A0(6),COM2B1(5),COM2B0(4),-(3),-(2),WGM21(1),WGM20(0)
.equ	tim2_ctc_cs256  = 0b00000110	;TCCR2B

;ocr2a page 131				;clock is 8MHz 1/8MHz= 0,000000125s => 125ns is 1cycle
.equ	tim2_ocr2a_comp1024	= 0x4e	;10ms/125ns => 0,01/0,000000125= 80000/1024= 78,125times dec2hex 0x4e
					; 78*125ns= 0,00000975s => 9,75µs*1024(prescaler)= 0,009984s 9,984ms
;timsk2 page 132
.equ	tim2_ocie2a	= 0b00000010	;TIMSK2 OCIE2B(2),OCIE2A(1),TOIE2(0)
;start counter
.equ	tim2_count	= 0x00		;timer2 is 8bit 

;sleep - idle page 53 SMCR
.equ	idle_mode_ena	= 0b00000001	;sleep ena in SMCR -(7),-(6),-(5),-(4),SM2(3),SM1(2),SM0(1),SE(0)

;interrupts
;EICRA page 54 External Interrupt Register A;-(7),-(6),-(5),-(4),ISC11(3),ISC10(2),ISC01(1),ISC00(0)
;1 1 The rising edge of INT1 generates a interrupt request.
.equ	int0a1_riseedge = 0b00001111	;external int0 rise edge, int1 rise edge
;EIMSK page 55
.equ	int0a1_eimsk	= 0b00000011	;int1(1),int0(0) enable external int1, int0
;PCICR page 56 - Pin Change Interrupt Control Register
.equ	pcint0_ena		= 0b00000001	;PCIE2(2),PCIE1(1),PCIE0(0) pcsk0
.equ	pcint20_ena		= 0b00000100	;PCIE2(2) pcmsk2, PCIE1(1), PCIE0(0)
;PCMSK2 page 57 - Pin change mask register
.equ	pcint0_pcmsk0		= 0b00000001	;PCINT7(7)....PCINT0(0)
;PCMSK2 page 57 - Pin change mask register
.equ 	pcint20_pcmsk20		= 0b00010000	;PCINT23(7).....PCINT16(0)


;here start the program! - after Reset the PC (Prorgramm Counter is on addr 0x0000 [16bit addr])

	.org 0x00	;here is the PC addr 0x0000
start:			;this is a level: on addr 0x0000
	rjmp reset_vector	;this command jump to the level with the name 'reset_vector'

;interupt vectors

;blinky
	.org 0x0e
	rjmp tim2_compa_handler	;this is the inrrupt vector after all 10ms i come here



;interrupt handler list
tim2_compa_handler:
	cbi portc,bit_oszi	;bit_oszi(5) is now low falling edge
	nop			;no operation only the time is gone +125ns
	nop
	nop
	sbi portc,bit_oszi	;bit_oszi(5) is now high rissing edge
control:
	;rcall subroutine	;here are all subprogramms are comming in

	reti			;return interrupt form timer2


;here is the initialisation of the program
	.org 0x100
reset_vector:
	ldi temp,high(ramend)	;load the highbyte form the RAM-End vector
	out SPH,temp		;put it in the stack pointer high
	ldi temp,low(ramend)	;load the lowbyte from the RAM-End vector
	out SPL,temp		;put it in the stack pointer low
				;this is very importand! never forget this to do.

;ports init
init_port:
	ldi temp,init_ddrb	;load the init value of DDRB to temp 
	out ddrb,temp		;and give it out to the register for DDRB - Data Direction Register portB
	ldi temp,init_portb	;load the init value of PORTB to temp
	out portb,temp		;give it out to the register for PORTB - this are the bits outside to PORTB
	ldi temp,init_ddrc
	out ddrc,temp
	ldi temp,init_portc
	out portc,temp
	ldi temp,init_ddrd
	out ddrd,temp
	ldi temp,init_portc
	out portc,temp

;status
	ldi temp,null		;load imidiate 0x00 into temp
	mov status,temp		;
;timer2
	ldi temp,tim2_Count	;start position 0x00
	sts tcnt2,temp		;timer2 counter=0

	ldi temp,tim2_wgm21
	sts tccr2a,temp		;WGM21(1) page 127

	ldi temp,tim2_ocr2a_comp1024	;OCRA2A max. count 10ms
	sts ocr2a,temp
	
	ldi temp,tim2_ctc_cs1024	;command to start the counter in prescaler mode
	sts tccr2b,temp			;start counter clk/1024

	ldi temp,tim2_ocie2a		;interrupt enable command
	sts timsk2,temp			;OCIE2a()

	sei				;global interrupt ena

;sleep
	ldi temp,idle_mode_ena		;sleep enable
	out smcr,temp

zzz:
	jmp zzz				;jump 4ever to zzz (mean sleep)
	sleep














































































































