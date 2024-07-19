/*
 * lib65816/debugger.c Release 1p1
 * Copyright (C) 2006 by Samuel A. Falvo II
 * See LICENSE for more details.
 *
 * 2007 Jan 5 saf2
 * Complete rewrite as the old "debugger" (really a tracer) was just rife
 * with bugs and utterly lacking in suitable features.
 */

#include <lib65816/config.h>

#ifdef DEBUG

#include <lib65816/cpu.h>
#include <stdio.h>

#include "cpumicro.h"

FILE *out = NULL;

/* 65816 debugger module */

char *mnemonics[256] = {
"BRK", "ORA", "COP", "ORA", "TSB", "ORA", "ASL", "ORA",
"PHP", "ORA", "ASL", "PHD", "TSB", "ORA", "ASL", "ORA",
"BPL", "ORA", "ORA", "ORA", "TRB", "ORA", "ASL", "ORA",
"CLC", "ORA", "INC", "TCS", "TRB", "ORA", "ASL", "ORA",
"JSR", "AND", "JSL", "AND", "BIT", "AND", "ROL", "AND",
"PLP", "AND", "ROL", "PLD", "BIT", "AND", "ROL", "AND",
"BMI", "AND", "AND", "AND", "BIT", "AND", "ROL", "AND",
"SEC", "AND", "DEC", "TSC", "BIT", "AND", "ROL", "AND",
"RTI", "EOR", "WDM", "EOR", "MVP", "EOR", "LSR", "EOR",
"PHA", "EOR", "LSR", "PHK", "JMP", "EOR", "LSR", "EOR",
"BVC", "EOR", "EOR", "EOR", "MVN", "EOR", "LSR", "EOR",
"CLI", "EOR", "PHY", "TCD", "JMP", "EOR", "LSR", "EOR",
"RTS", "ADC", "PER", "ADC", "STZ", "ADC", "ROR", "ADC",
"PLA", "ADC", "ROR", "RTL", "JMP", "ADC", "ROR", "ADC",
"BVS", "ADC", "ADC", "ADC", "STZ", "ADC", "ROR", "ADC",
"SEI", "ADC", "PLY", "TDC", "JMP", "ADC", "ROR", "ADC",
"BRA", "STA", "BRL", "STA", "STY", "STA", "STX", "STA",
"DEY", "BIT", "TXA", "PHB", "STY", "STA", "STX", "STA",
"BCC", "STA", "STA", "STA", "STY", "STA", "STX", "STA",
"TYA", "STA", "TXS", "TXY", "STZ", "STA", "STZ", "STA",
"LDY", "LDA", "LDX", "LDA", "LDY", "LDA", "LDX", "LDA",
"TAY", "LDA", "TAX", "PLB", "LDY", "LDA", "LDX", "LDA",
"BCS", "LDA", "LDA", "LDA", "LDY", "LDA", "LDX", "LDA",
"CLV", "LDA", "TSX", "TYX", "LDY", "LDA", "LDX", "LDA",
"CPY", "CMP", "REP", "CMP", "CPY", "CMP", "DEC", "CMP",
"INY", "CMP", "DEX", "WAI", "CPY", "CMP", "DEC", "CMP",
"BNE", "CMP", "CMP", "CMP", "PEI", "CMP", "DEC", "CMP",
"CLD", "CMP", "PHX", "STP", "JML", "CMP", "DEC", "CMP",
"CPX", "SBC", "SEP", "SBC", "CPX", "SBC", "INC", "SBC",
"INX", "SBC", "NOP", "XBA", "CPX", "SBC", "INC", "SBC",
"BEQ", "SBC", "SBC", "SBC", "PEA", "SBC", "INC", "SBC",
"SED", "SBC", "PLX", "XCE", "JSR", "SBC", "INC", "SBC"
};

enum modes {
    IMM8,
    IMM,
    IMMX,
    ACC,
    PCR,
    PCRL,
    IMPL,
    DP,
    DPX,
    DPY,
    DPI,
    DPIX,
    DPIY,
    DPIL,
    DPILY,
    ABS,
    ABSX,
    ABSY,
    ABSL,
    ABSLX,
    STK,
    STKIY,
    ABSI,
    ABSIX,
    BLK
};

int addrmodes[256] = {
    IMM8, DPIX, IMM8, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, ACC, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   DP, DPX, DPX, DPILY,
        IMPL, ABSY, ACC, IMPL,   ABS, ABSX, ABSX, ABSLX,
    ABS, DPIX, ABSL, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, ACC, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   DPX, DPX, DPX, DPILY,
        IMPL, ABSY, ACC, IMPL,   ABSX, ABSX, ABSX, ABSLX,

    IMPL, DPIX, IMM8, STK,   BLK, DP, DP, DPIL,
        IMPL, IMM, ACC, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   BLK, DPX, DPX, DPILY,
        IMPL, ABSY, IMPL, IMPL,   ABSL, ABSX, ABSX, ABSX,
    IMPL, DPIX, IMPL, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, ACC, IMPL,   ABSI, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   DPX, DPX, DPX, DPILY,
        IMPL, ABSY, IMPL, IMPL,   ABSIX, ABSX, ABSX, ABSLX,


    PCR, DPIX, PCRL, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, IMPL, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   DPX, DPX, DPX, DPILY,
        IMPL, ABSY, IMPL, IMPL,   ABS, ABSX, ABSX, ABSLX,
    IMMX, DPIX, IMMX, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, IMPL, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   DPX, DPX, DPY, DPILY,
        IMPL, ABSY, IMPL, IMPL,   ABSX, ABSX, ABSY, ABSLX,

    IMMX, DPIX, IMM8, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, IMPL, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   DPI, DPX, DPX, DPILY,
        IMPL, ABSY, IMPL, IMPL,   ABSI, ABSX, ABSX, ABSLX,
    IMMX, DPIX, IMM8, STK,   DP, DP, DP, DPIL,
        IMPL, IMM, IMPL, IMPL,   ABS, ABS, ABS, ABSL,
    PCR, DPIY, DPI, STKIY,   ABS, DPX, DPX, DPILY,
        IMPL, ABSY, IMPL, IMPL,   ABSIX, ABSX, ABSX, ABSLX
};

extern int cpu_irq;

void CPU_setDbgOutfile(FILE *f) {
	out = f;
}

void CPU_debug() {
	int	opcode;
	int	mode;
	int	operand;
    int ea;
    char operands[40];

	if (out == NULL) {
		out = stdout;
	}

	opcode = M_PEEK(PC.A);
	mode = addrmodes[opcode];
	fprintf(out, "A=%04X X=%04X Y=%04X S=%04X D=%04X B=%02X P=%02X (%c%c%c%c%c%c%c%c) E=%1d  ",
			(int) A.W, (int) X.W, (int) Y.W, (int) S.W, (int) D.W, (int) DB,
			(int) P,
			(F_getN?'N':'n'), (F_getV?'V':'v'), (F_getM?'M':'m'), (F_getX?'X':'x'),
			(F_getD?'D':'d'), (F_getI?'I':'i'), (F_getZ?'Z':'z'), (F_getC?'C':'c'),
			(int) E);
	fprintf(out, "%02X/%04X  %s ",(int) PC.B.PB,(int) PC.W.PC,mnemonics[opcode]);

	/* prevent error printing implied */
	operands[0] = 0;

	switch (mode) {
        case IMM8:
            sprintf( operands, "#$%02X", M_PEEK(PC.A+1) );
            break;

        case IMM:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            if( F_getM ) sprintf( operands, "#$%02X", (operand & 0xFF));
            else         sprintf( operands, "#$%04X", operand );
            break;

        case IMMX:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            if( F_getX ) sprintf( operands, "#$%02X", (operand & 0xFF));
            else         sprintf( operands, "#$%04X", operand );
            break;

        case ACC:
            sprintf( operands, "A" );
            break;

        case PCR:
            operand = M_PEEK(PC.A+1);
            sprintf( operands, "$%02X ($%02X%04X)", operand, PC.B.PB, PC.W.PC + operand + 2 - ((operand > 127) ? 256 : 0));
            break;

        case PCRL:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            sprintf( operands, "$%02X ($%02X%04X)", operand, PC.B.PB, PC.W.PC + operand + 3 - ((operand > 32767) ? 65536 : 0));
            break;

        case IMPL:
            //sprintf( operands, "" );
            break;

        case DP:
            operand = M_PEEK(PC.A+1);
            ea = D.W + operand;
            sprintf( operands, "$%02X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPX:
            operand = M_PEEK(PC.A+1);
            if( F_getX ) ea = D.W + operand + X.B.L;
            else         ea = D.W + operand + X.W;
            sprintf( operands, "$%02X,X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPY:
            operand = M_PEEK(PC.A+1);
            if( F_getX ) ea = D.W + operand + Y.B.L;
            else         ea = D.W + operand + Y.W;
            sprintf( operands, "$%02X,Y (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPI:
            operand = M_PEEK(PC.A+1);
            ea = D.W + operand;
            ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (DB<<16);
            sprintf( operands, "($%02X) (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPIX:
            operand = M_PEEK(PC.A+1);
            if( F_getX ) ea = D.W + operand + X.B.L;
            else         ea = D.W + operand + X.W;
            ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (DB<<16);
            sprintf( operands, "($%02X,X) (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPIY:
            operand = M_PEEK(PC.A+1);
            ea = D.W + operand;
            if( F_getX ) ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (DB<<16) + Y.B.L;
            else         ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (DB<<16) + Y.W;
            sprintf( operands, "($%02X),Y (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPIL:
            operand = M_PEEK(PC.A+1);
            ea = D.W + operand;
            ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (M_PEEK(ea+2)<<16);
            sprintf( operands, "[$%02X] (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case DPILY:
            operand = M_PEEK(PC.A+1);
            ea = D.W + operand;
            if( F_getX ) ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (M_PEEK(ea+2)<<16) + Y.B.L;
            else         ea = M_PEEK(ea) | (M_PEEK(ea+1)<<8) | (M_PEEK(ea+2)<<16) + Y.W;
            sprintf( operands, "[$%02X],Y (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABS:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            ea = operand + (DB<<16);
            sprintf( operands, "$%04X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABSX:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            if( F_getX ) ea = operand + (DB<<16) + X.B.L;
            else         ea = operand + (DB<<16) + X.W;
            sprintf( operands, "$%04X,X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABSY:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            if( F_getX ) ea = operand + (DB<<16) + Y.B.L;
            else         ea = operand + (DB<<16) + Y.W;
            sprintf( operands, "$%04X,Y (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABSL:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8) | (M_PEEK(PC.A+3)<<16);
            ea = operand;
            sprintf( operands, "$%06X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABSLX:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8) | (M_PEEK(PC.A+3)<<16);
            if( F_getX ) ea = operand + X.B.L;
            else         ea = operand + X.W;
            sprintf( operands, "$%06X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABSI:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            ea = M_PEEK(operand) + (M_PEEK(operand+1)<<8) + (DB<<16);
            sprintf( operands, "$%04X (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case ABSIX:
            operand = M_PEEK(PC.A+1) | (M_PEEK(PC.A+2)<<8);
            ea = operand | (PC.B.PB << 16);
            ea = M_PEEK(ea) + (M_PEEK(ea+1)<<8) + (PC.B.PB<<16);
            sprintf( operands, "($%04X,X) (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case STK:
            operand = M_PEEK(PC.A+1);
            ea = operand + S.W;
            sprintf( operands, "$%02X,S (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case STKIY:
            operand = M_PEEK(PC.A+1);
            ea = operand + S.W;
            if( F_getX ) ea = M_PEEK(ea) + (M_PEEK(ea+1)<<8) + (DB<<16) + Y.B.L;
            else         ea = M_PEEK(ea) + (M_PEEK(ea+1)<<8) + (DB<<16) + Y.W;

            sprintf( operands, "$%02X,S (@%06X %02X %02X %02X ...)",
                operand, ea, M_PEEK(ea), M_PEEK(ea+1), M_PEEK(ea+2) );
            break;

        case BLK:
            sprintf( operands, "$%02X, $%02X", M_PEEK(PC.A+2), M_PEEK(PC.A+1) );
            break;
	}
        fprintf( out, "%s\n", operands );
	fflush(out);
}


int CPU_log(char *buf, int maxlen) {

	int l = maxlen;
	int p = 0;
	int n;

	n = snprintf(buf+p, l, "A=%04X X=%04X Y=%04X S=%04X D=%04X B=%02X P=%02X (%c%c%c%c%c%c%c%c) E=%1d  ",
			(int) A.W, (int) X.W, (int) Y.W, (int) S.W, (int) D.W, (int) DB,
			(int) P,
			(F_getN?'N':'n'), (F_getV?'V':'v'), (F_getM?'M':'m'), (F_getX?'X':'x'),
			(F_getD?'D':'d'), (F_getI?'I':'i'), (F_getZ?'Z':'z'), (F_getC?'C':'c'),
			(int) E);
	l -= n;
	p += n;

	n = snprintf(buf+p, l, "%02X/%04X",(int) PC.B.PB,(int) PC.W.PC);
	l -= n;
	p += n;

	return p;
}


int CPU_dis(char *buf, int maxlen, int addr, unsigned char *stat, unsigned char (*peek)(int addr)) {
	int l = maxlen;
	int p = 0;
	int n, n2;

	int	opcode;
	int	mode;
	int	operand;
	int 	oplen;

	int xfl = *stat & 0x10;
	int mfl = *stat & 0x20;

	int pc = addr;
	int pcb = (pc >> 16) & 0xff; 
	int pcw = pc & 0xffff; 

	char operands[40];

	opcode = peek(pc);
	mode = addrmodes[opcode];
	n = snprintf(buf+p, l, "%02X/%04X  ", pcb, pcw);
	p += n;
	l -= n;

	n = 0;
	oplen = 0;
	operands[0] = 0;
	switch (mode) {
        case IMM8:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "#$%02X", operand );
	    oplen = 2;
            break;

        case IMM:
            if( mfl ) {
            	operand = peek(pc+1);
		n = snprintf(operands, 40, "#$%02X", operand);
	    	oplen = 2;
            } else {
            	operand = peek(pc+1) | (peek(pc+2)<<8);
		n = snprintf(operands, 40, "#$%04X", operand );
	    	oplen = 3;
	    }
            break;

        case IMMX:
            if( xfl ) {
            	operand = peek(pc+1);
		n = snprintf(operands, 40, "#$%02X", (operand & 0xFF));
	    	oplen = 2;
	    } else {
            	operand = peek(pc+1) | (peek(pc+2)<<8);
            	n = snprintf(operands, 40, "#$%04X", operand );
	    	oplen = 3;
	    }
            break;

        case ACC:
            n = snprintf(operands, 40, "A" );
	    oplen = 1;
            break;

        case PCR:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02X ($%02X%04X)", operand, pcb, pcw + operand + 2 - ((operand > 127) ? 256 : 0));
	    oplen = 2;
            break;

        case PCRL:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%02X ($%02X%04X)", operand, pcb, pcw + operand + 3 - ((operand > 32767) ? 65536 : 0));
	    oplen = 3;
            break;

        case IMPL:
            //sprintf( operands, "" );
	    oplen = 1;
            break;

        case DP:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02X",
                operand);
	    oplen = 2;
            break;

        case DPX:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02X,X",
                operand);
	    oplen = 2;
            break;

        case DPY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02X,Y",
                operand);
	    oplen = 2;
            break;

        case DPI:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "($%02X)",
                operand);
	    oplen = 2;
            break;

        case DPIX:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "($%02X,X)",
                operand);
	    oplen = 2;
            break;

        case DPIY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "($%02X),Y",
                operand);
	    oplen = 2;
            break;

        case DPIL:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "[$%02X]",
                operand);
	    oplen = 2;
            break;

        case DPILY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "[$%02X],Y",
                operand);
	    oplen = 2;
            break;

        case ABS:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04X",
                operand);
	    oplen = 3;
            break;

        case ABSX:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04X,X",
                operand);
	    oplen = 3;
            break;

        case ABSY:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04X,Y",
                operand);
	    oplen = 3;
            break;

        case ABSL:
            operand = peek(pc+1) | (peek(pc+2)<<8) | (peek(pc+3)<<16);
            n = snprintf(operands, 40, "$%06X",
                operand);
	    oplen = 4;
            break;

        case ABSLX:
            operand = peek(pc+1) | (peek(pc+2)<<8) | (peek(pc+3)<<16);
            n = snprintf(operands, 40, "$%06X",
                operand);
	    oplen = 4;
            break;

        case ABSI:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04X",
                operand);
	    oplen = 3;
            break;

        case ABSIX:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "($%04X,X)",
                operand);
	    oplen = 3;
            break;

        case STK:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02X,S",
                operand);
	    oplen = 2;
            break;

        case STKIY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02X,S",
                operand);
	    oplen = 2;
            break;

        case BLK:
            n = snprintf(operands, 40, "$%02X, $%02X", peek(pc+2), peek(pc+1) );
	    oplen = 3;
            break;
	}

	switch (oplen) {
	case 1:
	    n2 = snprintf(buf+p, l, " %02x           ", 
		peek(pc));
	    break;
	case 2:
	    n2 = snprintf(buf+p, l, " %02x %02x        ", 
		peek(pc), peek(pc+1));
	    break;
	case 3:
	    n2 = snprintf(buf+p, l, " %02x %02x %02x     ", 
		peek(pc), peek(pc+1), peek(pc+2));
	    break;
	case 4:
	    n2 = snprintf(buf+p, l, " %02x %02x %02x %02x  ", 
		peek(pc), peek(pc+1), peek(pc+2), peek(pc+3));
	    break;
	default:
	    n2 = 0;
	    break;
	}
	p += n2;
	l -= n2;

	n = snprintf(buf+p, l, "%s %s", mnemonics[opcode], operands);
	p += n;
	l -= n;

	if (opcode == 0xe2) {
		// SEP opcode
		*stat |= operand;
	}
	if (opcode == 0xc2) {
		// REP opcode
		*stat &= ~operand;
	}

	return oplen;
}


#endif
