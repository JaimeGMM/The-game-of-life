; multi-segment executale file template.
include  'macros.inc'
data segment
    
    
    ;variables de coordenadas de pantalla
    
    posFilPantalla db 0
    posColPantalla db 0
   
    ;mensaje de principio de juego
    
    EJV db  ' @@@@  EL JUEGO DE LA VIDA @@@@    $'
    finjuego db 10,13, '@@@@ FIN DE LA PARTIDA @@@@  $'
   
    ;variables de la matriz colonia
    
    
    MnsjTmnioMatrizCol db 10,13,'Introduce el tamanio de la matriz colonia:  $'
    
    mnsj_error db 10,13,'Error: el numero debe estar entre 5 y 15    $'  ;mensje de error al introducir numero para el tamanio de la matriz colonia 
    mnsj_error_numcolonia db 10,13,'Error: el numero esta fuera de rango  $' ;mensaje de error al introducir el numero de elementos en colonia
    ColActual db 10,13, 'Colonia Actual:   $'
    
    
    posX db 0   ;variable que guarda la coordenada fila
    posY db 0   ;variable que guarda la coordenada columna
    
    col db 4 dup ('$')  ;vector que contiene la cifra introducida referida al num de elementos en la colonia
    numero_en_colonia db 0 ;numero en decimal del numero de elementos en la colonia         
    NIndividuoscolonia  db 0  ;numero de individuos en la colonia
    mnsj_num_inic_colonia db  10,13,'Introduce el numero de elementos en la colonia:  $' ;mensaje para introducir el numero de elementos en la colonia inicial
    Colonia db 225 dup ('$')  ;Matriz que contiene a la colonia    
    NumFil db 0 ;numero de filas de la matriz
    NumCol db 0 ;numero de columnas de la matriz
    vec_num_col db 4 dup  ('0') ;vector que contiene el numero de elementos en colonia
    tam_matriz db 4 dup ('$') ;vector que contiene la cifra introducida por usuario referida a tamanio de la matriz
    numero_matriz db 0 ;numero en decimal del tamanio de la matriz
    
    contOcup db 0 ;variable que me cuenta el numero de vecinos adyacentes de las coordenadas (i,j)
        
        
    
    ;variables de condiciones de supervivencia
    
    
    MnsjintroSurv db  10,13,'Introduce numero de reglas de supervivencia:   $' 
    MnsjerrorCondSurv db 10,13,'Error: numero introducido esta fuera de rango del numero de condiciones de supervivencia   $'
    MsjErrorIntroCondSurv db  10,13,'Error: numero introducido fuera de rango de numero de vecinos para supervivencia  $'
    reglaSurv db  10,13, 'Introduce el numero de vecinos para supervivencia:   $';mensaje con el numero de vecinos para supervivencia
    CSupervi    db 5 dup ('0')  ;vector condiciones supervivencia
    vectorNumCondSurv db 4 dup ('$')
    numCondSurv db 0  ;numero decimal del numero de condiciones de supervivencia 
    
    vectPosiblesVecinoSurv db 4 dup ('$') ;vector que contiene numero ascii de num de vecinos posibles para supervivencia 
    numPosibleVecinosSurv db 0 ;variable que guarda el decimal del num de posibles vecinos para supervivencia
    
    
    
    ;variables de condiciones de nacimiento
    
    CNacimiento db 5 dup ('0') ;vector condiciones nacimiento
    MnsjintroNac db  10,13,'Introduce numero de reglas de nacimiento:    $'
    vectorNumCondNac db 4 dup ('$')
    numCondNac db 0 ;numero decimal del numero de condiciones de nacimiento
    reglaNac db   10,13,'Introduce el numero de vecinos para nacimiento:  $'
    vectPosiblesVecinoNac db 4 dup ('$')
    numPosibleVecinosNac db 0 ;variable que guarda el decimal del num de posibles vecinos para nacimiento
    MnsjerrorCondNac db 10,13, 'Error: el numero introducido esta fuera de rango del numero de condiciones de nacimiento  $'
    MsjErrorIntroCondNac db 10,13,'Error: el numero introducido esta fuera de rango de numero de vecinos para nacimiento  $'
    
    
    
    ;variables referidas a la matriz SgteGen
    
    SgteGen db 225 dup ('$') ;matriz que guarda los estados intermedios de la colonia 
    SgteGenActual db 10,13,'Matriz SgteGeneracion actual:   $'
    
    saltocarro db 10,13,'$'                                                                  
    
    
    posXSgteGen db 0
    posYSgteGen db 0
    
    hayCambio db 0 ;si =0 no hay cambio ,si =1 hay cambio
    
    ;variables de Mgeneracion
    
    Mgeneracion db 240 dup ('$') ;
    
    mnsjParesCoord db 10,13, 'Introduce pares de coordenadas:  $' ;mensaje para introducir los pares de coordenadas de la matriz Mgeneracion
    mnsjErrorMgen db  10,13, 'Error: el numero esta fuera de rango.Vuelva a introducir desde el principio  $' ;mensaje de error de introducir pares de coordenadas de Mgeneracion  
    MsjMgen db 10,13, 'La Matriz Mgeneracion es:  $'
    
    vecfils db 0 ;vector que contiene la coordenada  fila de Mgeneracion                       
    veccols db 0 ;vector que contiene la coordenada columna de Mgeneracion
    cifrafila db 0 ;variable que contiene la coordenada fila de Mgeneracion
    cifracol  db 0 ;variable que contiene la coordenada columna de Mgeneracion
    vecfil db 4 dup ('$') ; vector que contiene la coordenada fila en ascii de Mgeneracion
    veccol db 4 dup ('$') ; vector que contiene la coordenada columna en ascii de Mgeneracion
    varfilMgen db 0 ; variable que guarda el decimal de la fila de Mgeneracion   
    varcolMgen db 0 ; variable que guarda el decimal de la columna de Mgeneracion
    
    
    NumFilMgen db 0 ;numero de filas de la matriz Mgeneracion
    NumColMgen db 0 ;numero de columnas de la matriz Mgeneracion
   
    posXMgen db 0 ;coordenada fila de Mgen
    posYMgen db 0 ;coordenada columna de Mgen
    
    ;variables de Mgeneracion (solo mostrar) 
    
    numMgenDec db 0 ;variable que guarda la coordenada en decimal que voy a mostrar
    vecnumMgenDec  db  4 dup ('$') ;vector que guarda la coordenada que voy a 
    
    ;variables referidas a MaxGen
    
    MsjPedirMaxGen db  10,13,'Introduce el numero maximo de generaciones a realizar:    $'
    MsjErrorMaxGen db  10,13,'Error: el numero introducido esta fuera de rango    $'
    vecNumGen db 4 dup ('$')
    numGen db 0
    MaxGen db 0 ;maximo numero de generaciones a realizar

    ;variable que me indica la posicion donde voy a insertar en la matriz
    Pos dw 0
    
    ;variable que me guarda el total de multiplicar fila por columna
    maxtamanio db 0                                                
    
    ;variable que guarda el el max de elementos de la colonia
    maxnumcolonia db 0
    
    CadGenPantalla db 4 dup ('$')
    NumGenPantalla db 0
    MensGenPantalla db 'Generacion Actual: $'




ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax                     
  
    
    
    ; add your code here
    
    CURSOR 1,15
    
    write EJV  
    write saltocarro
    write saltocarro 
    write saltocarro
     
    call PedirTamanioInicialTablero  
    call PedirColoniaInicial 
    call PedirCondicionesSuperv
    call PedirCondicionesNacimiento
    call PedirMaxGen
    
    write saltocarro
    write MsjMgen
    write saltocarro
    
    call MostrarMgeneracion
    
    
    call InicializarColonia
    
    
    
    ;limpiar pantalla y utilizar macros cusrsor para mostrar las dos matrices

    BucleGeneraciones:
    
    BORRAR
    CURSOR 0,0
    
    write MensGenPantalla
    decasc NumGenPantalla,CadGenPantalla
    write CadGenPantalla                    
                            
    write saltocarro
    
    mov hayCambio,0
    
    write ColActual
    write saltocarro
    
    call MostrarColonia
    
    call CalculaSgteGen
    
    write saltocarro
    
    write SgteGenActual
    
    write saltocarro
    write saltocarro
    
    call MostrarSgteGen
    
    
    call CalculaNuevaColonia
    
    cmp hayCambio,0
    
    je FinProgramaPrincipal
    
    dec MaxGen
    
    cmp MaxGen,0
    
    je FinProgramaPrincipal
    
    inc NumGenPantalla
    
    jmp BucleGeneraciones  
    
    FinProgramaPrincipal:
    
    write finjuego
    ; wait for any key....    
    mov ah, 1
    int 21h
   
    
    
    
mov ax, 4c00h ; exit to operating system.
    int 21h    
ends     
   
    PedirTamanioInicialTablero PROC
        
        push ax        
        push bx
        
        write  MnsjTmnioMatrizCol       
        pedir  2,tam_matriz 
        write saltocarro 
        
        
        filtrado_num:
         
         
         ASCdec tam_matriz,numero_matriz
         
         mov al,numero_matriz
         cmp al,5
         jb menserror
         cmp al,15
         ja menserror
         mov NumFil,al
         mov NumCol,al
         mov ah,0
         mov bl,al
        
         mov maxtamanio,al ;variable para controlar el max de elementos de la colonia
         mul bl    ;este mul es para guardar el numero maximo de casillas de la colonia
         mov maxnumcolonia,al
             
         jmp fin_tamanio_inicial
          
        menserror:
         
         write mnsj_error
         write saltocarro
         write MnsjTmnioMatrizCol
         pedir 2,tam_matriz 
         jmp filtrado_num
        
        fin_tamanio_inicial:
        
         pop bx
         pop ax
    
         ret
    
   PedirTamanioInicialTablero ENDP                                                                                

  
   
 
    ;procedimiento que pide el numero inicial de elementos  
    ;en colonia,los introduce en la variable Nindividuoscolonia 
    ;y rellena Mgeneracion
    PedirColoniaInicial PROC 
          
       push ax
       push cx
       push si
       push bx
          
       write mnsj_num_inic_colonia 
       pedir 3,col
       write saltocarro
                          
       ;se va a introducir el numero de elementos en  la variable Ncolonia
          
       filtrado_num_col:
         
          ASCdec col,numero_en_colonia
          mov al,numero_en_colonia
          cmp al,0
          jb menserror_klnia1
          cmp al,maxnumcolonia ;numero maximo de individuos en colonia (numero maximo de casillas de la matriz colonia)
          ja menserror_klnia1
          mov NIndividuoscolonia,al
          jmp pedircoordenadasMgen
   
        menserror_klnia1:
        
          write mnsj_error_numcolonia
          write mnsj_num_inic_colonia
          pedir 4,col
          write saltocarro
          jmp filtrado_num_col
               
               
        
        pedircoordenadasMgen:
   
          ;en esta etiqueta debe mostrar mensaje
          ;para que el usuario introduzca las
          ;coordenadas de fila y columna de cada
          ;individuo.Se guardara en MGeneracion
                
          mov ch,0
          mov cl,NIndividuoscolonia
          cmp cx,0
          Je finLLenarMgen 
          mov posX,0             
          mov posY,0
                
                    
          PedirPar:
                
            write mnsjParesCoord
            pedir 2,vecfil
            lea si,vecfil     
              
              filtroLetraFil:  ;filtro para que no acepte letra en la coordenada fila
                  
                cmp [si],'0'
                jb errorIntropar
                cmp [si],'9'
                ja errorIntropar
                inc si
                cmp [si],'$'
                jne filtroLetraFil  
            
            ASCdec vecfil,cifrafila
            write mnsjParesCoord
            pedir 2,veccol
            write saltocarro
            lea si,veccol
                
                filtroLetraCol:  ;filtro para que no acepte letra en la coordeanda columna
                
                cmp [si],'0'
                jb errorIntropar
                cmp [si],'9'
                ja errorIntropar
                inc si
                cmp [si],'$'
                jne filtroLetraCol  
                 
            ASCdec veccol,cifracol
            mov al,cifrafila
            cmp al,0
            jb  errorIntropar
            cmp al,NumFil
            jae errorIntropar  ;si es mayor o igual que el numero de filas
            mov al,cifracol
            cmp al,0
            jb  errorIntropar
            cmp al,NumCol
            jae errorIntropar  ;si es mayor o igual que el numero de columnas 
            mov posX,0
            POMATRIZ Mgeneracion,posX,posY,NIndividuoscolonia,Pos                                                 
            mov si,Pos
            mov bl,cifrafila
            mov [si],bl
            inc posX
            POMATRIZ Mgeneracion,posX,posY,NIndividuoscolonia,Pos
            mov si,Pos
            mov bl,cifracol
            mov [si],bl
            inc posY
                      
            loop  PedirPar
                        
               
            jmp FinLlenarMgen ;salto incond despues del llenado de la matriz
               
               
               
            errorIntropar:
               
                write mnsjErrorMgen
                write saltocarro 
                jmp pedircoordenadasMgen            
                 
                
                
            FinLlenarMgen:
            
                      
                      
                pop bx
                pop si
                pop cx
                pop ax
               
                ret
    
  
    PedirColoniaInicial  ENDP      
              
               
               
               
    ;este procedimiento inicializa todas las casillas de la matriz colonia a L
    ;E:la matriz colonia
    ;S: la matriz colonia con las casillas inicializadas a L
    InicializarColonia PROC           
               
               
          push ax
          push bx
          push cx
          push si
               
               
         ;aqui inicializo la colonia con todas las casillas a 'L'
               
          mov posX,0
          mov posY,0
               
               
          RecorrerFilIniciaCol:
               
               mov bl,NumFil
               cmp posX,bl
               je  FinInicializar
               mov posY,0
               
          RecorrerColIniciaCol:
                
               mov bl,NumCol
               cmp posY,bl
               je IncrementarFilIniciaCol
               POMATRIZ Colonia,posX,posY,NumCol,Pos
               mov si,Pos
               mov [si],'L'
               inc posY
               jmp RecorrerColIniciaCol
                    
               
               IncrementarFilIniciaCol:
               
                 inc posX
                 jmp RecorrerFilIniciaCol     
                    
               
              FinInicializar:
                          
                          
                          
               
          introCoord:
               
            mov posY,0 ;para poner fil y col a cero otra vez
            mov ch,0
            mov cl,NIndividuoscolonia
            cmp cx,0
            je  FinInicializaColonia
                   
                       
              LlenarMatColonia:
                   
                   
                 mov posX,0
                 POMATRIZ Mgeneracion,posX,posY,NIndividuoscolonia,Pos
                 mov si,Pos
                 mov bl,[si]
                 mov varfilMgen,bl
                 inc posX
                 POMATRIZ Mgeneracion,posX,posY,NIndividuoscolonia,Pos
                 mov si,Pos
                 mov bl,[si]
                 mov varcolMgen,bl
                 inc posY
                 POMATRIZ Colonia,varfilMgen,varcolMgen,NumCol,Pos
                 mov si,Pos
                 mov [si],'O'
                   
                   
                 loop  LlenarMatColonia
                         
                   
                   
                   
            FinInicializaColonia:
    
    
                 pop si
                 pop cx
                 pop bx
                 pop ax
    
                 ret
    
   InicializarColonia ENDP
                              
                  
                  
     
  
    ;procedimiento que pide las condiciones de spuervivencia y las introduce
    ;en el vector Csupervi
    PedirCondicionesSuperv PROC
        
               push ax
               push si
               push cx
               push bx
               
               write saltocarro
               lea si,CSupervi 
                
               condicionSurv:  ;introduce condiciones y ve si esta en el rango
               
                write MnsjintroSurv
                pedir 1,vectorNumCondSurv
                ASCdec vectorNumcondSurv,numCondSurv ;no me lo hace bien 
                mov al,numCondSurv
                cmp al,1
                jb errorIntroCondSurv
                cmp al,2
                ja errorIntroCondSurv
                mov bl,numCondSurv
                mov [si],bl
                inc si
                jmp PosiblesVecinSupervi
                
                
               errorIntroCondSurv:
               
                    write MnsjerrorCondSurv
                    
                    write saltocarro
                    
                    jmp condicionSurv
                
               
               PosiblesVecinSupervi:    ;introduce el numero de vecinos posibles de condicion
               
                     mov ch,0
                     mov cl,numCondSurv
                     cmp cx,0
                     je FinllenarVCsup
                        
         
                     condiSurv:
                        
                        write reglaSurv
                        pedir 1,vectPosiblesVecinoSurv
                        ASCdec vectPosiblesVecinoSurv,numPosibleVecinosSurv
                        mov al,numPosibleVecinosSurv
                        cmp al,2
                        jb errorNumPosibleSurv
                        cmp al,3
                        ja errorNumPosibleSurv
                        mov [si],al
                        inc si
                        
                     loop condiSurv   
 
                        jmp FinllenarVCsup ;salto a fin despues de llenar el vector de CSuperv
                        
                        
                     errorNumPosibleSurv:
                     
                        write MsjErrorIntroCondSurv
                        jmp PosiblesVecinSupervi
   
   
                     FinllenarVCsup:
                     
             
                        pop bx
                        pop cx
                        pop si
                        pop ax
                        
                        ret
   
   PedirCondicionesSuperv  ENDP
  
    
   ;Procedimiento que pide las condiciones de nacimiento
   ;y las introduce en el vector Cnacimiento   
   PedirCondicionesNacimiento PROC
         
               push ax
               push si
               push cx
               push bx
               
               write saltocarro 
               lea si,CNacimiento 
                
               condicionNac:  ;introduce condiciones y ve si esta en el rango
               
                
                write MnsjintroNac
                pedir 1,vectorNumCondNac
                ASCdec vectorNumCondNac,numCondNac
                mov al,numCondNac
                cmp al,1
                jb errorIntroCondNac
                cmp al,2
                ja errorIntroCondNac
                mov [si],al
                inc si
                jmp PosiblesVecinNac
                
                
               errorIntroCondNac:
               
                    write MnsjerrorCondNac
                    
                    write saltocarro
                    
                    jmp condicionNac
                
               
               PosiblesVecinNac:    ;introduce el numero de vecinos posibles de condicion de nacimiento
               
                     mov ch,0
                     mov cl,numCondNac
                     cmp cx,0
                     je FinllenarVCnaci
                        
         
                     condiNac:
                        
                        write reglaNac
                        pedir 1,vectPosiblesVecinoNac
                        ASCdec vectPosiblesVecinoNac,numPosibleVecinosNac
                        mov al,numPosibleVecinosNac
                        cmp al,2
                        jb errorNumPosibleNac
                        cmp al,3
                        ja errorNumPosibleNac
                        mov [si],al
                        inc si
                        
                        
                        loop condiNac
                                    
                                    
                        jmp FinllenarVCnaci   ;salto a fin despues de llenar el vector de CSuperv           
                        
                     
                     errorNumPosibleNac:
                     
                        write MsjErrorIntroCondNac
                        jmp condiNac
   
   
                     FinllenarVCnaci:
                     
                        pop bx
                        pop cx
                        pop si
                        pop ax
   
                        ret
   
  PedirCondicionesNacimiento ENDP
      
      
      ;procedimiento que pide el numero maximo de generaciones a realizar    
      ;y las guarda en MaxGen
      ;E:vecNumGen que guarda lo introducido por el usuario
      ;S: 
      PedirMaxGen PROC     
        
        
               push ax
               
               PMaxGen:
               
                    
                    write MsjPedirMaxGen
                    pedir 2,vecNumGen
                    ASCdec vecNumGen,numGen
                    mov al,numGen 
                    cmp al,0
                    jb ErrorMaxGen
                    cmp al,9 ;entre que rango esta el numero de generaciones que puedo pedir?
                    ja ErrorMaxGen
                    mov MaxGen,al
                    jmp finMaxGen
                    
   
               ErrorMaxGen:
               
                    write MsjErrorMaxGen
                    jmp pMaxGen
                    
   
               finMaxGen:
                    
                    pop ax
   
                    ret
  
  PedirMaxGen  ENDP   
         
 
 
   ;aqui van los procedimientos de mostrado
   
   
   ;Procedimiento que muetra la matriz Colonia
   ;E: La matriz Colonia
   MostrarColonia PROC  
                        
        
               push ax
               push bx
               push cx
               push si
               push dx
               
               ;aqui empieza el leido de cada casilla
               write saltocarro
               
               mov posX,0
               mov posY,0
               
               
               RecorrerFilMostrarCol:
                    
                    mov bl,NumFil
                    cmp posX,bl
                    je  FinMostrarColonia
                    mov posY,0
               
               RecorrerColMostrarColonia:
                
                    mov bl,NumCol
                    cmp posY,bl
                    je IncrementarFilaMostrarColonia
                    POMATRIZ Colonia,posX,posY,NumCol,Pos
                    mov si,Pos
                    mov bl,[si]
                    cmp bl,'L'
                    je muestraL
                    cmp bl,'O'
                    je muestraO
                    inc posY
                    jmp RecorrerColMostrarColonia
                    
               
               IncrementarFilaMostrarColonia:
               
                    inc posX
                    
                    write saltocarro
                    
                    jmp RecorrerFilMostrarCol     
                    
               
           
               muestraL:
                    
                    mov dl,95 ;muestra el vacio como guion bajo
                    mov ah,02h
                    int 21h
                    inc posY
                    jmp RecorrerColMostrarColonia 
           
               
               muestraO:
                     
                    ;muestra por pantalla una cara sonriente
                    mov dl,1  
                    mov ah,02h
                    int 21h
                    inc posY
                    jmp RecorrerColMostrarColonia
           
               
               
               
               
               FinMostrarColonia:
                   
                     pop dx
                     pop si
                     pop cx
                     pop bx
                     pop ax
     
                     ret
                           
  MostrarColonia ENDP
   
   
   
  MostrarSgteGen PROC  ;muestra la matriz SgteGen con el contenido que tenga dentro
               
               push ax
               push bx
               push cx
               push si
               push dx
               
               ;aqui empieza el leido de cada casilla
               
               mov posX,0
               mov posY,0
               
               
               RecorrerFilMostrarSgteGen:
               
                    mov bl,NumFil
                    cmp posX,bl
                    je  FinMostrarSgteGen
                    mov posY,0
               
               RecorrerColMostrarSgteGen:
                
                    mov bl,NumCol
                    cmp posY,bl
                    je IncrementarFilaMostrarSgteGen
                    POMATRIZ SgteGen,posX,posY,NumCol,Pos
                    mov si,Pos
                    mov dl,[si]
                    mov ah  ,02h
                    int 21h
                    inc posY
                    jmp RecorrerColMostrarSgteGen
                    
               
               IncrementarFilaMostrarSgteGen:
               
                    inc posX
                    
                    write saltocarro
                    
                    jmp RecorrerFilMostrarSgteGen     
                    
               
               FinMostrarSgteGen:
                   
                     pop dx
                     pop si
                     pop cx
                     pop bx
                     pop ax
     
                     ret
   
  MostrarSgteGen  ENDP
   
   
    
    ;procedimiento que obtiene el numero de filas y columnas  de la matriz Mgeneracion
    ;E: 
    ;S: NumFilMgen y NumColMgen que son el numero de filas y columnas de Mgeneracion
    ObtenerCoordsMgen PROC   
         
        push ax
        
        mov al,2
        mov NumFilMgen,al
        mov al,NIndividuoscolonia
        mov NumColMgen,al
        
        pop ax
   
        ret
   
  ObtenerCoordsMgen ENDP  
     
     
    
    ;Procedimiento que muestra por pantalla la matriz Mgeneracion
    ;E: La Matriz Mgeneracion
    MostrarMgeneracion PROC   
        
        push ax
        push bx             
        push cx        
        push si        
                
        
        write saltocarro
        call ObtenerCoordsMgen        
                
                
          mov posXMgen,0
          mov posYMgen,0
               
               
               RecorrerFilMostrarMgen:
               
                    mov bl,NumFilMgen
                    cmp posXMgen,bl
                    je  FinMostrarMgen
                    mov posYMgen,0
               
               RecorrerColMostrarMgen:
                
                    mov bl,NumColMgen
                    cmp posYMgen,bl
                    je IncrementarFilaMostrarMgen
                    POMATRIZ Mgeneracion,posXMgen,posYMgen,NumColMgen,Pos
                    mov si,Pos
                    mov bl,[si]
                    mov  numMgenDec,bl
                    DECASC numMgenDec,vecnumMgenDec
                    write vecnumMgenDec
                    inc posYMgen
                    jmp RecorrerColMostrarMgen
                    
               
               IncrementarFilaMostrarMgen:
               
                    inc posXMgen
                    
                    write saltocarro
                    
                    jmp RecorrerFilMostrarMgen     
                    
               
               
               
               FinMostrarMgen:
                           
                           
                    pop ax
                    pop bx
                    pop cx
                    pop si
    
                    ret
  
  MostrarMgeneracion  ENDP      
              
     
     
  ;calcula el numero de adyacentes para cada coordenada i,j de la matriz Colonia
  ;E: la coordenada de la matriz colonia
  ;S: el contador de elementos adyacentes a la coordenada de entrada  
  calcularvecinosColonia   PROC   
    
    
                    push ax
                    push bx
                    push si
                    
                   mov contOcup,0 
                   
                    
                   Movimiento1: ;decremento columna de la matriz colonia
                   
                        dec posY
                        cmp posY,0
                        jl  Movimiento2
                        POMATRIZ Colonia,posX,posY,NumCol,Pos 
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont1
                        
                       
                   Movimiento2:  ;decremento fila de la matriz colonia
                   
                        dec posX
                        cmp posX,0
                        jl  Movimiento3
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont2
                       
                   
                   
                   
                   Movimiento3: ;incremento columna de la matriz colonia
                   
                        
                        inc posY
                        mov bh,posY
                        cmp bh,NumCol
                        jae Movimiento4 ;si es mayor o igual al numero de columnas salta
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont3
                       
                   
                       
                   Movimiento4: ;incremento columna de la matriz colonia
                              
                        inc posY
                        mov bh,posY
                        cmp bh,NumCol
                        jae Movimiento5 ;si es mayor o igual al numero de columnas   
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont4
                       
                   
                   Movimiento5: ;decremento fila de la matriz de colonia
                   
                        inc posX
                        mov bh,posX
                        cmp bh,NumCol                                                  
                        jae  Movimiento6 ;mayor o igual 
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont5
                       
                                       
                    Movimiento6: ;decrementa fila de la matriz de colonia 
                    
                        inc posX
                        mov bh,posX
                        cmp bh,NumFil
                        jae Movimiento7
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont6               
                   
                   
                   Movimiento7: ;decremento columna de la matriz de colonia
                   
                        dec posY
                        cmp posY,0
                        jl  movimiento8
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont7               
                   
                   
                   Movimiento8: ;decremento columna de la matriz de colonia    
                      
                        dec posY
                        cmp posY,0
                        jl  finComprobar
                        POMATRIZ Colonia,posX,posY,NumCol,Pos
                        mov si,Pos
                        mov bl,[si]
                        cmp bl,'O'
                        je  incrementarCont8               
                   
                        
                        jmp finComprobar
                        
                   
                   incrementarCont1:
                       
                       
                        inc contOcup
                        jmp Movimiento2
                   
                   
                   incrementarCont2:
                       
                       
                        inc contOcup
                        jmp Movimiento3
                   
                   
                   incrementarCont3:
                       
                       
                        inc contOcup
                        jmp Movimiento4
                   
                           
                   incrementarCont4:
                       
                       
                        inc contOcup
                        jmp Movimiento5
                   
                                   
                           
                   incrementarCont5:
                       
                       
                        inc contOcup
                        jmp Movimiento6
                   
                            
                   incrementarCont6:
                   
                        inc contOcup
                        jmp Movimiento7  
                     
                     
                   incrementarCont7:
                   
                        inc contOcup
                        jmp Movimiento8
                  
                     
                   incrementarCont8:
                   
                        inc contOcup  
                        jmp finComprobar
                     
                     
                    
                   finComprobar: 
                    
                     pop si
                     pop bx
                     pop ax
                    
                
                     ret
    
   
  calcularvecinosColonia ENDP
  
      
  ;estos dos procesos a continuacion conforman el calcular nueva generacion 
      
   
   ;Procedimiento que calcula la matriz SgteGen
   ;E: parametro de entrada es la matriz Colonia
   ;S: la matriz SgteGen con los estados posibles
   CalculaSgteGen PROC    ;por corregir
    
    
             
               push ax
               push bx
               push cx
               push si
               
               
               ;aqui se analiza cada casilla de la Matriz colonia
              
               mov posXSgteGen,0
               mov posYSgteGen,0
               
               
               RecorrerFilCalculaSgteGen:
               
                    mov bl,NumFil
                    cmp posXSgteGen,bl
                    je  FinCalcularSgteGen
                    mov posYSgteGen,0
               
               RecorrerColCalculaSgteGen:
                
                    ;aqui guardo las coordenadas de la matriz para el calcularvecinos
                    mov ah,posXSgteGen
                    mov al,posYSgteGen
                    mov posX,ah
                    mov posY,al
                    
                    mov bl,NumCol
                    cmp posYSgteGen,bl
                    je  IncrementarFilCalculaSgteGen
                    call calcularvecinosColonia
                    
                    POMATRIZ Colonia,posXSgteGen,posYSgteGen,NumCol,Pos
                    mov si,Pos
                    mov bl,[si]
                    cmp bl,'L'        
                    je  saltoCondNac  ;si colonia esta a 'L',salto a condiciones nacimiento
                    cmp bl,'O'        
                    je  saltoCondSurv  ;si colonia esta a 'O' ,salto a condiciones de Surv
                    
               
               IncrementarFilCalculaSgteGen:
               
                    inc posXSgteGen
                    jmp RecorrerFilCalculaSgteGen
                         
                    
               
               
                saltoCondNac:    ;etiqueta para calcular si nace o se queda igual
                    
                    mov bh,contOcup  
                    lea si,CNacimiento
                    mov cl,[si]
                    mov ch,0
                     
                    comprobarCnac:
                     
                     inc si
                     mov bl,[si]
                     cmp bh,bl
                     je  llenarNac
                     
                     loop comprobarCnac
                    
                     ;si el numero de vecinos no coincide con la condiciones
                     ;de nacimiento,estando a 'L' la casilla de matriz colonia
                     ;en esta coordenada en la matriz SgteGeneracion
                     ;estara a 'L'
                     POMATRIZ SgteGen,posXSgteGen,posYSgteGen,NumCol,Pos
                     mov si,Pos
                     mov [si],'L'
                    
                     ;incremento columna y vuelvo a comprobar
                     inc posYSgteGen
                     jmp RecorrerColCalculaSgteGen 
                      
                     llenarNac:
                      
                         ;si el numero de vecinos coincide con las condiciones
                         ;de nacimiento,estando a 'L' la casilla de matriz colonia
                         ;pondra a 'N' en la casilla de mismas coordenadas
                         ;en la matriz SgteGen
                         POMATRIZ SgteGen,posXSgteGen,posYSgteGen,NumCol,Pos
                         mov si,Pos
                         mov [si],'N'
                         mov hayCambio,1
                         
                         inc posYSgteGen
                         jmp RecorrerColCalculaSgteGen
                         
                         
                
                
                saltoCondSurv:   ;etiqueta para calcular si muere o sobrevive
                    
                    mov bh,contOcup
                    lea si,CSupervi
                    mov cl,[si]
                    mov ch,0
                    
                    
                    comprobarCsurv:
                    
                    inc si
                    mov bl,[si] 
                    cmp bh,bl
                    je  llenarSurv
            
                    loop comprobarCsurv
                    
                    
                    ;si el numero de vecinos no coincide con la condiciones
                    ;de supervivencia,estando a 'O' la casilla de matriz colonia
                    ;en esta coordenada en la matriz SgteGeneracion
                    ;estara a 'M'
                    
                    POMATRIZ SgteGen,posXSgteGen,posYSgteGen,NumCol,Pos
                    mov si,Pos
                    mov [si],'M'
                    mov hayCambio,1
                    inc posYSgteGen
                    jmp RecorrerColCalculaSgteGen
                                  
                                  
                    llenarSurv:
                    
                         ;si el numero de vecinos coincide con las condiciones
                         ;de supervivencia,estando a 'O' la casilla de matriz colonia
                         ;pondra a 'O' en la casilla de mismas coordenadas
                         ;en la matriz SgteGen
                         POMATRIZ SgteGen,posXSgteGen,posYSgteGen,NumCol,Pos
                         mov si,Pos
                         mov [si],'O'
                         
                         inc posYSgteGen
                         jmp RecorrerColCalculaSgteGen
                              
                                  
            
               FinCalcularSgteGen:
               
                pop si
                pop cx
                pop bx
                pop ax
    
    
    
    
                     ret
    
  CalculaSgteGen  ENDP 
     
     
  
  
  ;calcula la nueva matriz colonia a partir de la matriz SgteGen
  ;E:parametro de entrada es la matriz SgteGeneracion
  ;S:la matriz Colonia con los nuevos estados 
  CalculaNuevaColonia PROC     
   
             
               push ax
               push bx
               push cx
               push si
               
               
               ;aqui se analiza cada casilla de la SgteGen
              
               mov posX,0
               mov posY,0
               
               
               RecorrerFilCalculaNuevaCol:
               
                    mov bl,NumFil
                    cmp posX,bl
                    je  FinCalcularNuevaCol
                    mov posY,0
               
               RecorrerColCalculaNuevaCol:
                
                    mov bl,NumCol
                    cmp posY,bl
                    je IncrementarFilCalculaNuevaCol
                    POMATRIZ SgteGen,posX,posY,NumCol,Pos
                    mov si,Pos
                    mov bl,[si]
                    cmp bl,'N'     ;si SgteGen esta a 'N',colonia se pone a 'O'
                    je  cambiarO
                    cmp bl,'M'     ; si SgteGen esta a 'M' ,colonia se pone a 'L'
                    je  cambiarL
                    cmp bl,'O'     ;si SgteGen esta a 'O',colonia se pone a 'O'
                    je  cambiarO
                    cmp bl,'L'     ;si SgteGen esta a 'L',colonia se pone a 'L'
                    je  cambiarL
                    
                    
               
               IncrementarFilCalculaNuevaCol:
               
                    inc posX
                    jmp RecorrerFilCalculaNuevaCol     
                    
               
               
                cambiarO:
                    
                    POMATRIZ Colonia,posX,posY,NumCol,Pos 
                    mov si,Pos
                    mov [si],'O'
                    inc posY
                    jmp RecorrerColCalculaNuevaCol 
                    
               
                cambiarL:
                
                    POMATRIZ Colonia,posX,posY,NumCol,Pos
                    mov si,Pos
                    mov [si],'L'
                    inc posY
                    jmp RecorrerColCalculaNuevaCol 
                     
           
               FinCalcularNuevaCol:
               
                pop si
                pop cx
                pop bx
                pop ax
    
                ret
    
   CalculaNuevaColonia  ENDP
 
 
   
   
   
     
end start ; set entry point and stop the assembler.
                                           