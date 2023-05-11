; 
; 7segment_counter.asm
;
; Created: 11.05.2023	- 11.May.2023
; Author : weakbit
;
;for Windows
;.include "C:\Program Files (x86)\Atmel\Studio\7.0\packs\atmel\ATmega_DFP\1.3.300\avrasm\inc\m328pdef.inc"
;for Linux
.include "m328pdef.inc"

;History:
;21.04.2023	LED-Counter
;11.05.2023	7segemtn_countdown

;reg def - registers are all 8bit
.def	temp		= r16	;temorary data storage
.def	temp2		= r17	
.def	temp3		= r18
.def	counter		= r19	;blinky blinky counter
.def	counter2	= r20	;counter for LED-time (sub2)
.def	counter_led	= r21	;counter value	(sub2)	
.def	status		= r22	;MSB(7),(6),(5),(4),(3),from_pcint0(2),from_int1(1),from_int0(0)LSB  

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
.equ    two		= 0x02  ;
.equ	three		= 0x03
.equ	four		= 0x04
.equ	five		= 0x05
.equ	six		= 0x06
.equ	seven		= 0x07
.equ	eight		= 0x08	;hex number for '8'
.equ	nine		= 0x09
.equ	char_a		= 0x0a
.equ	char_b		= 0x0b
.equ	char_c		= 0x0c	
.equ	char_d		= 0x0d	
.equ	char_e		= 0x0e
.equ	char_f		= 0x0f

.equ	ff		= 0xff	;hex number for '255' this is 0xff
.equ	blinky_time	= 0x1e	;300ms = 30 = 0x1e LED timing         (subroutine1)
.equ	led_100ms_time	= 0x0a  ;100ms = 10 = 0x0a LED-Counter timing (sub2)
.equ	led_500ms_time	= 0x32  ;500ms = 50 = 0x32 7segment count-down time
.equ	init_led_value	= 0x00  ;start at 0x00(0b00000000)[00] and count till 0xff(0b11111111)[255]
				;MSB bit7[128],6[64],5[32],4[16],3[8],2[4],1[2],0[1]LSB
				;0b01101011 = 64+32+8+2+1 = 107 = 0x6b 

.equ	mask_low_digit  = 0b00001111 ;low nibble mask out - 7segment counter low byte

;7segment bit-pattern		;bit-pattern
;    a				a = 0
;   ---				b = 1
; f|   |b			c = 2
;   -g-				d = 3
; e|   |c			e = 4
;   ---				f = 5
;    d .h			g = 6
;				h = 7
;                           hgfedcba
.equ	led_7seg_zero	= 0b00111111	;number '0'
.equ	led_7seg_one	= 0b00000110	;number '1'
.equ	led_7seg_two	= 0b01011011	;number '2'
.equ	led_7seg_three  = 0b01001111	;number '3'
.equ	led_7seg_four	= 0b01100110	;number '4'
.equ	led_7seg_five	= 0b01101101	;number '5'
.equ	led_7seg_six	= 0b01111101	;number '6'
.equ	led_7seg_seven  = 0b00000111	;number '7'
.equ	led_7seg_eight  = 0b01111111	;number '8'
.equ	led_7seg_nine	= 0b01101111	;number '9'
.equ	led_7seg_a	= 0b01110111	;char 'A'
.equ    led_7seg_b	= 0b01111100	;char 'b'
.equ	led_7seg_c	= 0b00111001	;char 'C'
.equ	led_7seg_d	= 0b01011110	;char 'd'
.equ	led_7seg_e	= 0b01111001	;char 'E'
.equ	led_7seg_f	= 0b01110001	;char 'F'

;compare end mask
;here comes the bis mask

;port def
;portb
.equ	init_portb	= 0b00000000	;LED7(7),LED6(6),LED5(5),LED4(4),LED3(3),LED2(2),LED1(1),LED0(0)
.equ	init_ddrb	= 0b11111111	;out(7),out(6),out(5),out(4),out(3),out(2),out(1),out(0)
;portc
.equ    init_portc      = 0b00100000    ;(7),(6),oszi(5),blue(4),white(3),green(2),yellow(1),red(0) LED out
.equ    init_ddrc       = 0b11111111    ;(7),(6),out(5),out(4),out(3),out(2),out(1),out(0)
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
;-----------------------------------------------------------------------------------------------------------
	.org 0x00	;here is the PC addr 0x0000 16bit addr
 start:			;this is a level: on addr 0x0000
	rjmp reset_vector	;this command jump to the level with the name 'reset_vector'

;interupt vectors

;blinky interrupt register page 49
	.org 0x0e
	rjmp tim2_compa_handler	;this is the inrrupt vector after all 10ms i come here



;interrupt handler list
tim2_compa_handler:
	cbi portc,bit_oszi	;bit_oszi(5) is now low falling edge
	nop			;no operation only the time is gone +125ns
	nop			;1/8Mhz = 125ns
	nop
	sbi portc,bit_oszi	;bit_oszi(5) is now high rissing edge
control:
	;rcall subroutine	;here are all subprogramms are comming in
	rcall blinky		;(subroutine1)
	rcall led_counter	;(subroutine2)
	rcall led_7segment	;(subroutine3)

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
	ldi temp,init_portd
	out portd,temp
;init blinky time
	ldi counter,blinky_time	;load with 300ms

;init led-timer value
	ldi counter_led,init_led_value	;load the start value of the led-counter
	ldi counter2,led_100ms_time	;load the timing 100ms for the led-counter

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

;##############################################################################################################

blinky:
					;300ms/10ms= 30times
	cpi counter,null		;compare counter with '0x00'
	breq blinky_change		;if counter is 0x00 then jump to blinky_change
	dec counter			;counter-1 (all the loops)
	jmp blinky_end
					;atmel-0856-avr-instruction-set-manual.pdf - Instruction set for 328p assembly
blinky_change:
	sbic portc,bit_red_led		;skip if bit is '0' that mean the LED is off to the switch on LED
	jmp blinky_led_off
blinky_led_on:
	sbi portc,bit_red_led		;set bit in portc,bit_red_led - switch the LED on
	ldi counter,blinky_time		;reload the counter with the 300ms blinky_time
	jmp blinky_end

blinky_led_off:
	cbi portc,bit_red_led		;clear '0' the bit_red_led
	ldi counter,blinky_time		;reload the cpounter with the 300ms blinky_time
	;jmp blinky_end			;jump to the end but this must not be writen

blinky_end:
	ret				;return where it come from
;-----------------------------------------------------------------------------------------
led_counter:				;100ms/10ms= 10times
	cpi counter2,null		;compare counter2 with '0x00'
	breq led_counter_increment	;if the counter2 is equale 0x00 the jump to 'led_counter_increment'
	dec counter2			;counter2-1 (all the loops decrement it)
	jmp led_counter_end		;if nothing to do then jump to the end

led_counter_increment:
	dec counter_led			;counter_led-1 - for the count-down
;	out portb,counter_led		;move the value of the counter_led out to the portb - disable this in case of the bit-pattern for the 7segment led
					;there are the LED connected via a Resistor
	ldi counter2,led_500ms_time	;relaod the counter2 with the time for 500ms

led_counter_end:
	ret				;returns where it comes from
;----------------------------------------------------------------------------------------
;7segment LED routine
led_7segment:
	mov temp2,counter_led		;taking the counter_led 
	andi temp2,mask_low_digit	;it mask out the low byte 4bits
					;mask out?
					;temp2 = 0x24 and we mask it out with mask_low_digit then => temp2 = 0x04
detect_0:
	cpi temp2,null			;temp2 == 0?
	brne detect_1			;if not equale then jump to the next detection
	ldi temp,led_7seg_zero
	out portb,temp			;out the bit-pattern to the portb for the 7segment-display
	rjmp led_7segment_end
detect_1:
	cpi temp2,one			;temp2 == 1?
	brne detect_2
	ldi temp,led_7seg_one		;load the value into the temp
	out portb,temp			;out the bit-pattern for number '1'
	rjmp led_7segment_end
detect_2:
	cpi temp2,two			;temp2 == 2?
	brne detect_3
	ldi temp,led_7seg_two		;out the bit-pattern for number '2'
	out portb,temp
	rjmp led_7segment_end		;jump to the end
detect_3:
	cpi temp2,three			;temp2 == 3?
	brne detect_4
	ldi temp,led_7seg_three
	out portb,temp			;out the bit-pattern for number '3'
	rjmp led_7segment_end		;jump to the end
detect_4:
	cpi temp2,four			;temp2 == 4?
	brne detect_5
	ldi temp,led_7seg_four
	out portb,temp			;out the bit-pattern for number '4'
	rjmp led_7segment_end		;jump to the end
detect_5:
	cpi temp2,five			;temp2 == 5?
	brne detect_6
	ldi temp,led_7seg_five
	out portb,temp			;out the bit-pattern for number '5'
	rjmp led_7segment_end		;jump to the end
detect_6:
	cpi temp2,six			;temp2 == 6?
	brne detect_7
	ldi temp,led_7seg_six
	out portb,temp			;out the bit-pattern for number '6'
	rjmp led_7segment_end		;jump to the end
detect_7:
	cpi temp2,seven			;temp2 == 7?
	brne detect_8
	ldi temp,led_7seg_seven
	out portb,temp			;out the bit-pattern for number '7'
	rjmp led_7segment_end		;jump to the end
detect_8:
	cpi temp2,eight			;temp2 == 8?
	brne detect_9
	ldi temp,led_7seg_eight
	out portb,temp			;out the bit-pattern for number '8'
	rjmp led_7segment_end		;jump to the end
detect_9:
	cpi temp2,nine			;temp2 == 1?
	brne detect_a
	ldi temp,led_7seg_nine
	out portb,temp			;out the bit-pattern for number '9'
	rjmp led_7segment_end		;jump to the end
detect_a:
	cpi temp2,char_a		;temp2 == a?
	brne detect_b
	ldi temp,led_7seg_a
	out portb,temp			;out the bit-pattern for char 'a'
	rjmp led_7segment_end		;jump to the end
detect_b:
	cpi temp2,char_b		;temp2 == b?
	brne detect_c
	ldi temp,led_7seg_b
	out portb,temp			;out the bit-pattern for char 'b'
	rjmp led_7segment_end		;jump to the end
detect_c:
	cpi temp2,char_c		;temp2 == c?
	brne detect_d
	ldi temp,led_7seg_c
	out portb,temp			;out the bit-pattern for char 'c'
	rjmp led_7segment_end		;jump to the end
detect_d:
	cpi temp2,char_d		;temp2 == d?
	brne detect_e
	ldi temp,led_7seg_d
	out portb,temp			;out the bit-pattern for char 'd'
	rjmp led_7segment_end		;jump to the end
detect_e:
	cpi temp2,char_e		;temp2 == e?
	brne detect_f
	ldi temp,led_7seg_e
	out portb,temp		;out the bit-pattern for char 'e'
	rjmp led_7segment_end		;jump to the end
detect_f:
	cpi temp2,char_f		;temp2 == f?
	brne led_7segment_end
	ldi temp,led_7seg_f
	out portb,temp			;out the bit-pattern for number '1'
	rjmp led_7segment_end		;jump to the end - you don't need to jump here the next command is the end of this subprogram3

led_7segment_end:
	ret
;----------------------------------------------------------------------------------------

























































































