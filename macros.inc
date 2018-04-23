; multi-segment executable file template.

;macros
;Posibilita que el usuario teclee una cadena de caracteres
;E: LON indica el numero maximo de caracteres que se pueden teclear. 
;S: DIR que indica la direccion de memoria donde se va a almacenar lo que teclea el usuario   
pedir macro lon,dir
                        local fin_pedir,pedir1 
                        PUSH  AX 
                        PUSH  CX 
                        PUSH  DI 
 
                        mov   cx,3 
                        cld 
                        mov   al,'$' 
                        lea   di,dir 
                        rep   stosb ; incializa la direccion dir con $ 
                        mov   cx,lon 
                        lea   di,dir 
                pedir1: mov   ah,1 
                        int   21h 
                        cmp   al,13 
                        je    fin_pedir 
                        stosb 
                        loop  pedir1 
                    
             fin_pedir: POP   DI 
                        POP   CX  
                        POP   AX 
endm  
 
;Saca por pantalla una cadena de caracteres
;E: mem es la direccion de la variable que queremos sacar por pantalla 
write macro mem
                        PUSH  AX 
                        PUSH  DX 
                         
                        LEA   dx,mem 
                        mov   ah,9 
                        int   21h 
                         
                        POP   DX 
                        POP   AX 
endm 

;coloca el cursor en una determinada fila y columna de pantalla
;E: FILA y COL valores num?ricos que indican la posicion
CURSOR   MACRO  FILA,COL    
                        push  ax 
                        push  bx 
                        PUSH  dx 
                         
                        mov   dh,fila 
                        mov   dl,col 
                        mov   bh,0 
                        mov   ah,2 
                        int   10h 
                         
                        pop   dx 
                        pop   bx 
                        pop   ax 
endm 

;borra la pantalla por completo 
BORRAR MACRO             
                        push  ax 
                        PUSH  BX 
                        PUSH  CX 
                        PUSH  DX 
                         
                        MOV   AL,0 
                        mov   cx,0 
                        mov   dx,2479h 
                        MOV   Bh,7 
                        MOV   ah,6 
                        int   10h 

                        POP   DX 
                        POP   CX 
                        POP   BX 
                        POP   AX 
endm 

;Transforma de decimal a ASCII
;E: BINV valor num?rico
;S: ASC que apunta a la direcci?n de la variable ASCII donde se almacenar? el resultado  
DECASC  MACRO  BINV,ASC  
                        LOCAL DEC1,DEC2 
                        PUSH  AX 
                        PUSH  CX 
                        PUSH  DX 
                        PUSH  SI 
                        PUSH  DI 
                         
                        LEA   DI,ASC 
                        CLD 
                        MOV   CX,3 
                        MOV   AL,' ' 
                        REP   STOSB 
                        MOV   aH,0 
                        mov   AL,BINV 
                        LEA   SI,ASC[2] 
                        MOV   CX,10 
                  DEC1: CMP   AX,10 
                        JL    DEC2 
                        MOV   DX,0 
                        DIV   CX 
                        ADD   DL,30H 
                        MOV   [SI],DL 
                        DEC   SI 
                        JMP   DEC1 
                  DEC2: ADD   AL,30H 
                        MOV   [SI],AL 
                         
                        POP   DI 
                        POP   SI 
                        POP   DX 
                        POP   CX 
                        POP   AX 
ENDM 
 
;Transforma de ASCII a decimal
;E: ASC que apunta a la direcci?n de la variable ASCII donde se almacena la cadena  
;S: BINV valor num?rico
ASCdec  MACRO ASC,BINV 
                        LOCAL ASC1,ASC2,ASC11 
                        push  ax 
                        PUSH  BX 
                        PUSH  CX 
                        PUSH  SI 
                         
                        LEA   SI,ASC[2] 
                        MOV   CX,3 
                        MOV   BX,1 
                        MOV   BINV,0 
                  ASC1: MOV   AL,[SI] 
                        CMP   AL,'$' 
                        JE    asc11 
                        cmp   al,'0' 
                        jl    asc2 
                        cmp   al,'9' 
                        jg    asc2 
                        SUB   AL,30H 
                        mov   AH,0 
                        MUL   BL 
                        ADD   binv,aL 
                        mov   ax,10 
                        mul   bx 
                        mov   bx,ax 
                  ASC11:dec   si 
                        loop  asc1 
                         
                  asc2:  
                        POP   SI 
                        POP   CX 
                        POP   BX 
                        POP   AX 
ENDM 

;Dada una matriz, una fila, una columna y el ?numero de columnas de la matriz devuelve la posicion absoluta de esa celda en memoria 
;E: MAT, Matriz
;   FI, Fila
;   CO, Columna
;   DICO, Dimensi?n en columnas de MAT
;S: POSI, posici?n absoluta de memoria (16bits, DW) que ocupa la celda
POMATRIZ MACRO   MAT,FI,CO,DICO,POSI 
                        PUSH  AX 
                        PUSH  DI 
 
                        LEA   DI,MAT 
                        MOV   AX,0 
                        MOV   AL,FI 
                        MUL   DICO 
                        ADD   Al,CO 
                        ADD   AX,DI 
                        MOV   POSI,AX 
 
                        POP   DI 
                        POP   AX 
ENDM     