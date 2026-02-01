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
#include <string.h>
#include <stdlib.h>

#include "cpumicro.h"


/* breakpoint handling */

#define BP_NUM (sizeof(bps)/sizeof(bps[0]))

#define BP_EXECUTE 1

struct bp {
  int flags;
  word32 addr;
};

static struct bp bps[128];


/* 65816 debugger module */

char *mnemonics_lc[256] = {
"brk", "ora", "cop", "ora", "tsb", "ora", "asl", "ora",
"php", "ora", "asl", "phd", "tsb", "ora", "asl", "ora",
"bpl", "ora", "ora", "ora", "trb", "ora", "asl", "ora",
"clc", "ora", "inc", "tcs", "trb", "ora", "asl", "ora",
"jsr", "and", "jsl", "and", "bit", "and", "rol", "and",
"plp", "and", "rol", "pld", "bit", "and", "rol", "and",
"bmi", "and", "and", "and", "bit", "and", "rol", "and",
"sec", "and", "dec", "tsc", "bit", "and", "rol", "and",
"rti", "eor", "wdm", "eor", "mvp", "eor", "lsr", "eor",
"pha", "eor", "lsr", "phk", "jmp", "eor", "lsr", "eor",
"bvc", "eor", "eor", "eor", "mvn", "eor", "lsr", "eor",
"cli", "eor", "phy", "tcd", "jmp", "eor", "lsr", "eor",
"rts", "adc", "per", "adc", "stz", "adc", "ror", "adc",
"pla", "adc", "ror", "rtl", "jmp", "adc", "ror", "adc",
"bvs", "adc", "adc", "adc", "stz", "adc", "ror", "adc",
"sei", "adc", "ply", "tdc", "jmp", "adc", "ror", "adc",
"bra", "sta", "brl", "sta", "sty", "sta", "stx", "sta",
"dey", "bit", "txa", "phb", "sty", "sta", "stx", "sta",
"bcc", "sta", "sta", "sta", "sty", "sta", "stx", "sta",
"tya", "sta", "txs", "txy", "stz", "sta", "stz", "sta",
"ldy", "lda", "ldx", "lda", "ldy", "lda", "ldx", "lda",
"tay", "lda", "tax", "plb", "ldy", "lda", "ldx", "lda",
"bcs", "lda", "lda", "lda", "ldy", "lda", "ldx", "lda",
"clv", "lda", "tsx", "tyx", "ldy", "lda", "ldx", "lda",
"cpy", "cmp", "rep", "cmp", "cpy", "cmp", "dec", "cmp",
"iny", "cmp", "dex", "wai", "cpy", "cmp", "dec", "cmp",
"bne", "cmp", "cmp", "cmp", "pei", "cmp", "dec", "cmp",
"cld", "cmp", "phx", "stp", "jml", "cmp", "dec", "cmp",
"cpx", "sbc", "sep", "sbc", "cpx", "sbc", "inc", "sbc",
"inx", "sbc", "nop", "xba", "cpx", "sbc", "inc", "sbc",
"beq", "sbc", "sbc", "sbc", "pea", "sbc", "inc", "sbc",
"sed", "sbc", "plx", "xce", "jsr", "sbc", "inc", "sbc"
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

extern int trace;
extern int no_io;

static FILE *outfile = NULL;

void CPU_setDbgOutfile(FILE *f) {
	outfile = f;
}

int CPU_disasm(int out) {
	int	opcode;
	int	mode;
	int	operand;
	int ea;
	char operands[40];
	int size;

	if (outfile == NULL) {
		outfile = stdout;
	}

	no_io = 1;
	opcode = M_READ(PC.A);
	mode = addrmodes[opcode];
	if(out == 1) fprintf(outfile, "A%04x X%04x Y%04x S%04x D%04x B%02x P%02x (%c%c%c%c%c%c%c%c) %c  ",
			(int) A.W, (int) X.W, (int) Y.W, (int) S.W, (int) D.W, (int) DB,
			(int) P,
			(F_getN?'N':'n'), (F_getV?'V':'v'), (F_getM?'M':'m'), (F_getX?'X':'x'),
			(F_getD?'D':'d'), (F_getI?'I':'i'), (F_getZ?'Z':'z'), (F_getC?'C':'c'),
			E?'E':'N');
	if(out) fprintf(outfile, "%02x.%04x  %s ",(int) PC.B.PB,(int) PC.W.PC,mnemonics_lc[opcode]);
	switch (mode) {
        case IMM8:
            sprintf( operands, "#$%02x", M_READ(PC.A+1) );
	    size = 2;
            break;

        case IMM:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            if( F_getM ) sprintf( operands, "#$%02x", (operand & 0xFF));
            else         sprintf( operands, "#$%04x", operand );
	    size = F_getM?2:3;
            break;

        case IMMX:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            if( F_getX ) sprintf( operands, "#$%02x", (operand & 0xFF));
            else         sprintf( operands, "#$%04x", operand );
	    size = F_getX?2:3;
            break;

        case ACC:
            sprintf( operands, "A" );
	    size = 1;
            break;

        case PCR:
            operand = M_READ(PC.A+1);
            sprintf( operands, "$%04x 		($%02x -> $%02x%04x)", PC.W.PC + operand + 2, operand, PC.B.PB, PC.W.PC + operand + 2);
	    size = 2;
            break;

        case PCRL:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            sprintf( operands, "$%04x 		($%04x -> $%02x%04x)", PC.W.PC + operand + 3, operand, PC.B.PB, PC.W.PC + operand + 3);
	    size = 3;
            break;

        case IMPL:
	    operands[0]=0;
	    size = 1;
            break;

        case DP:
            operand = M_READ(PC.A+1);
            ea = D.W + operand;
            sprintf( operands, "$%02x 		(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPX:
            operand = M_READ(PC.A+1);
            if( F_getX ) ea = D.W + operand + X.B.L;
            else         ea = D.W + operand + X.W;
            sprintf( operands, "$%02x,X 		(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPY:
            operand = M_READ(PC.A+1);
            if( F_getX ) ea = D.W + operand + Y.B.L;
            else         ea = D.W + operand + Y.W;
            sprintf( operands, "$%02x,Y 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPI:
            operand = M_READ(PC.A+1);
            ea = D.W + operand;
            ea = M_READ(ea) | (M_READ(ea+1)<<8) | (DB<<16);
            sprintf( operands, "($%02x) 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPIX:
            operand = M_READ(PC.A+1);
            if( F_getX ) ea = D.W + operand + X.B.L;
            else         ea = D.W + operand + X.W;
            ea = M_READ(ea) | (M_READ(ea+1)<<8) | (DB<<16);
            sprintf( operands, "($%02x,X) 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPIY:
            operand = M_READ(PC.A+1);
            ea = D.W + operand;
            if( F_getX ) ea = M_READ(ea) | (M_READ(ea+1)<<8) | (DB<<16) + Y.B.L;
            else         ea = M_READ(ea) | (M_READ(ea+1)<<8) | (DB<<16) + Y.W;
            sprintf( operands, "($%02x),Y 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPIL:
            operand = M_READ(PC.A+1);
            ea = D.W + operand;
            ea = M_READ(ea) | (M_READ(ea+1)<<8) | (M_READ(ea+2)<<16);
            sprintf( operands, "[$%02x] 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case DPILY:
            operand = M_READ(PC.A+1);
            ea = D.W + operand;
            if( F_getX ) ea = M_READ(ea) | (M_READ(ea+1)<<8) | (M_READ(ea+2)<<16) + Y.B.L;
            else         ea = M_READ(ea) | (M_READ(ea+1)<<8) | (M_READ(ea+2)<<16) + Y.W;
            sprintf( operands, "[$%02x],Y 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case ABS:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            ea = operand + (DB<<16);
            sprintf( operands, "$%04x 		(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 3;
            break;

        case ABSX:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            if( F_getX ) ea = operand + (DB<<16) + X.B.L;
            else         ea = operand + (DB<<16) + X.W;
            sprintf( operands, "$%04x,X 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 3;
            break;

        case ABSY:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            if( F_getX ) ea = operand + (DB<<16) + Y.B.L;
            else         ea = operand + (DB<<16) + Y.W;
            sprintf( operands, "$%04x,Y 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 3;
            break;

        case ABSL:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8) | (M_READ(PC.A+3)<<16);
            ea = operand;
            sprintf( operands, "$%06x	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 4;
            break;

        case ABSLX:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8) | (M_READ(PC.A+3)<<16);
            if( F_getX ) ea = operand + X.B.L;
            else         ea = operand + X.W;
            sprintf( operands, "$%06x 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 4;
            break;

        case ABSI:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            ea = M_READ(operand) + (M_READ(operand+1)<<8) + (DB<<16);
            sprintf( operands, "($%04x)		(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 3;
            break;

        case ABSIX:
            operand = M_READ(PC.A+1) | (M_READ(PC.A+2)<<8);
            ea = operand | (PC.B.PB << 16);
            ea = M_READ(ea) + (M_READ(ea+1)<<8) + (PC.B.PB<<16);
            sprintf( operands, "($%04x,X) 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 3;
            break;

        case STK:
            operand = M_READ(PC.A+1);
            ea = operand + S.W;
            sprintf( operands, "$%02x,S 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case STKIY:
            operand = M_READ(PC.A+1);
            ea = operand + S.W;
            if( F_getX ) ea = M_READ(ea) + (M_READ(ea+1)<<8) + (DB<<16) + Y.B.L;
            else         ea = M_READ(ea) + (M_READ(ea+1)<<8) + (DB<<16) + Y.W;

            sprintf( operands, "($%02x,S),Y 	(@%06x %02x %02x %02x ...)",
                operand, ea, M_READ(ea), M_READ(ea+1), M_READ(ea+2) );
	    size = 2;
            break;

        case BLK:
            sprintf( operands, "$%02x, $%02x", M_READ(PC.A+2), M_READ(PC.A+1) );
	    size = 3;
            break;
	}
	if(out) fprintf(outfile, "%s\n", operands );
	no_io=0;
	return size;
}



void CPU_debug(void)
{

  static char buf[32], rep[32];
  static word32 gaddr, daddr, m;
  word32 oldpc;
  static int run;
  int i, tmp;

  if(run){
    if(PC.A == gaddr){
      gaddr=-1;
      run=0;
    }

    for(i = 0; i<BP_NUM; i++)
      if(bps[i].flags == BP_EXECUTE && bps[i].addr == PC.A){
	printf("Breakpoint %d hit!\n", i);
	run = 0;
      }
  }
  if(run)
    return;

  CPU_disasm(1);

  while(1){
    if(trace){
      strcpy(buf,"s");
    }else{
      do {
	printf("> ");
	fflush(stdout);
      } while (!fgets(buf, 32, stdin));
    }

    if(!buf[0] || buf[0] == '\n')
      strcpy(buf, rep);
    else
      strcpy(rep, buf);

    switch (buf[0]){
    case 'g':  /* goto addr */
      if(sscanf(buf, "g %i", &tmp) == 1)
	gaddr = tmp;
      run = 1;
      break;
    case 't':
    case 's':  /* single step */
      //if(buf[1]=='*') log=1;
      //dd->run = TRUE;
      break;
    case 'n':  /* run to next instruction */
      gaddr = PC.A + CPU_disasm(0);
      run = 1;
      break;
    case 'r':
      printf("CPU reset!\n");
      CPU_reset();
      break;
    case 'q':
      exit(0);
      break;
    case 'd':
      /* d <addr> [n]: disassemble n instructions at addr
	 dh: disassemble here
	 d: disassemble next line
      */
      oldpc = PC.A;
      i = 10;
      if(sscanf(buf,"d %i %i", &tmp, &i) >= 1)
	daddr = tmp;
      else if(buf[1] == 'h')
	daddr = PC.A;
      printf("\n");
      while(i--){
	PC.A = daddr;
	daddr += CPU_disasm(2);
      }
      printf("\n");
      strcpy(rep, "d");
      PC.A = oldpc;
      continue;
    case 'm':   /* dump memory around addr */
      tmp = 64;
      if(sscanf(buf, "m %i %i", &i, &tmp) >= 1)
	daddr = i;
      m = daddr;
      daddr &= ~15;
      for(i = 0; i < tmp; i++){
	if((i & 15) == 0)
	  printf("\n%06lx:", (unsigned long)daddr);
	printf("%c%02x", daddr == m ? '>' : ' ', daddr >= 2 ? (int)MEM_readMem(daddr, 0, 0) : 0);
	daddr++;
      }
      printf("\n\n");
      strcpy(rep, "m");
      continue;
    case 'b':  /* set breakpoint at  addr */
      if(sscanf(buf, "b %i", &tmp) == 1){
	for(i = 0; i < BP_NUM; i++){
	  if(bps[i].flags == 0){
	    bps[i].flags = BP_EXECUTE;
	    bps[i].addr = tmp;
	    printf("\nbreakpoint %d set at 0x%lx\n\n", i, (unsigned long)tmp);
	    break;
	  }
	}
      }else{
	printf("\nBreakpoints:\n");
	for(i = 0; i<BP_NUM; i++){
	  if(bps[i].flags == BP_EXECUTE)
	    printf("%03d: 0x%06lx Execute\n", i, (unsigned long)bps[i].addr);
	}
	printf("\n");
      }
      continue;
    }
    strcpy(rep, buf);
    break;
  }
}


int CPU_log(char *buf, int maxlen) {

	int l = maxlen;
	int p = 0;
	int n;

	n = snprintf(buf+p, l, "A=%04x X=%04x Y=%04x S=%04x D=%04x B=%02x P=%02x (%c%c%c%c%c%c%c%c) E=%1d  ",
			(int) A.W, (int) X.W, (int) Y.W, (int) S.W, (int) D.W, (int) DB,
			(int) P,
			(F_getN?'N':'n'), (F_getV?'V':'v'), (F_getM?'M':'m'), (F_getX?'X':'x'),
			(F_getD?'D':'d'), (F_getI?'I':'i'), (F_getZ?'Z':'z'), (F_getC?'C':'c'),
			(int) E);
	l -= n;
	p += n;

	n = snprintf(buf+p, l, "%02x/%04x",(int) PC.B.PB,(int) PC.W.PC);
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
	n = snprintf(buf+p, l, "%02x/%04x  ", pcb, pcw);
	p += n;
	l -= n;

	n = 0;
	oplen = 0;
	operands[0] = 0;
	switch (mode) {
        case IMM8:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "#$%02x", operand );
	    oplen = 2;
            break;

        case IMM:
            if( mfl ) {
            	operand = peek(pc+1);
		n = snprintf(operands, 40, "#$%02x", operand);
	    	oplen = 2;
            } else {
            	operand = peek(pc+1) | (peek(pc+2)<<8);
		n = snprintf(operands, 40, "#$%04x", operand );
	    	oplen = 3;
	    }
            break;

        case IMMX:
            if( xfl ) {
            	operand = peek(pc+1);
		n = snprintf(operands, 40, "#$%02x", (operand & 0xFF));
		oplen = 2;
	    } else {
            	operand = peek(pc+1) | (peek(pc+2)<<8);
            	n = snprintf(operands, 40, "#$%04x", operand );
	    	oplen = 3;
	    }
            break;

        case ACC:
            n = snprintf(operands, 40, "A" );
	    oplen = 1;
            break;

        case PCR:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02x ($%02x%04x)", operand, pcb, pcw + operand + 2 - ((operand > 127) ? 256 : 0));
	    oplen = 2;
            break;

        case PCRL:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%02x ($%02x%04x)", operand, pcb, pcw + operand + 3 - ((operand > 32767) ? 65536 : 0));
	    oplen = 3;
            break;

        case IMPL:
            //sprintf( operands, "" );
	    oplen = 1;
            break;

        case DP:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02x",
                operand);
	    oplen = 2;
            break;

        case DPX:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02x,X",
                operand);
	    oplen = 2;
            break;

        case DPY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02x,Y",
                operand);
	    oplen = 2;
            break;

        case DPI:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "($%02x)",
                operand);
	    oplen = 2;
            break;

        case DPIX:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "($%02x,X)",
                operand);
	    oplen = 2;
            break;

        case DPIY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "($%02x),Y",
                operand);
	    oplen = 2;
            break;

        case DPIL:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "[$%02x]",
                operand);
	    oplen = 2;
            break;

        case DPILY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "[$%02x],Y",
                operand);
	    oplen = 2;
            break;

        case ABS:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04x",
                operand);
	    oplen = 3;
            break;

        case ABSX:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04x,X",
                operand);
	    oplen = 3;
            break;

        case ABSY:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04x,Y",
                operand);
	    oplen = 3;
            break;

        case ABSL:
            operand = peek(pc+1) | (peek(pc+2)<<8) | (peek(pc+3)<<16);
            n = snprintf(operands, 40, "$%06x",
                operand);
	    oplen = 4;
            break;

        case ABSLX:
            operand = peek(pc+1) | (peek(pc+2)<<8) | (peek(pc+3)<<16);
            n = snprintf(operands, 40, "$%06x",
                operand);
	    oplen = 4;
            break;

        case ABSI:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "$%04x",
                operand);
	    oplen = 3;
            break;

        case ABSIX:
            operand = peek(pc+1) | (peek(pc+2)<<8);
            n = snprintf(operands, 40, "($%04x,X)",
                operand);
	    oplen = 3;
            break;

        case STK:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02x,S",
                operand);
	    oplen = 2;
            break;

        case STKIY:
            operand = peek(pc+1);
            n = snprintf(operands, 40, "$%02x,S",
                operand);
	    oplen = 2;
            break;

        case BLK:
            n = snprintf(operands, 40, "$%02x, $%02x", peek(pc+2), peek(pc+1) );
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

	n = snprintf(buf+p, l, "%s %s", mnemonics_lc[opcode], operands);
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
