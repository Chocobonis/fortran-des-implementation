PROGRAM des
    IMPLICIT none
    ! ---- GET THE PLAIN TEXT FOR ENCRYPTION ------
    CHARACTER (len=128) :: STR2CIPH
    CHARACTER (len=128) :: PERMKEYS
    CHARACTER (len=128) :: ENCRTEXT
    CHARACTER (len=128) :: RGHT_EXP
    CHARACTER (len=128) :: XORED_ST
    CHARACTER (len=128) :: FINAL

    CHARACTER (len=128) :: ENCRYPTED

    CHARACTER (len=128) :: XORED_RH

    INTEGER :: CIPH_2XOR(128)
    INTEGER :: CIPH_1XOR(128)

    CHARACTER (len = 1) :: TEMP_CH1
    INTEGER :: TEMP_I
    INTEGER :: I

    INTEGER  :: PC1(56)
    INTEGER  :: PC2(48)

    ! --- THESE ARE THE PERMUTATION TABLES ---
    INTEGER  :: PERM_1(64)
    INTEGER  :: PERM_2(48)

    INTEGER  :: SUB_BOX(512)

    INTEGER  :: INV_PERM(64)
    INTEGER  :: TAB_PERM(32)

    CHARACTER (len=128) :: PERM_LN
    ! ----------------------------------------

    CHARACTER (len=28) :: HALF1_SBS 
    CHARACTER (len=28) :: HALF2_SBS

    CHARACTER (len=28) :: TMP_SHIFT
    CHARACTER (len=28) :: SHIFT_CMP
    INTEGER :: J

    PRINT *, 'BONIIS FORTRAN DES ENCRYPTION TOOL'
    PRINT *, '            BETA 1.0              '
    PRINT *, '---------------------------------------'

    PRINT *, 'Input your text to cipher: '
    PRINT *, '(text must be shorter than 128)'

    READ *, STR2CIPH

    PRINT *, 'Plain text: ', STR2CIPH

    ! ---- THIS IS THE KEY GENERATION FOR DES -----
    

    DATA PC1 /57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18, 10, 2, &
            59, 51, 43, 35, 27, 19, 11, 3, 60, 52, 44, 36, 63, 55, 47, 39, &
            31, 23, 15, 7, 62, 54, 46, 38, 30, 22, 14, 6, 61, 53, 45, 37, 29, &
            21, 13, 5, 28, 20, 12, 4/

    DATA PC2 /14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10, 23, 19, 12, 4, 26, 8, &
            16, 7, 27, 20, 13, 2, 41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, &
            48, 44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32/

    DATA PERM_1 /58,50,42,34,26,18,10,2, &
	60,52,44,36,28,20,12,4, &
	62,54,46,38,30,22,14,6, &
	64,56,48,40,32,24,16,8, &
	57,49,41,33,25,17,9,1, &
	59,51,43,35,27,19,11,3, &
	61,53,45,37,29,21,13,5, &
	63,55,47,39,31,23,15,7/

    DATA PERM_2 /32,1,2,3,4,5,4,5, &
	6,7,8,9,8,9,10,11, &
	12,13,12,13,14,15,16,17, &
	16,17,18,19,20,21,20,21, &
	22,23,24,25,24,25,26,27, &
	28,29,28,29,30,31,32,1 /

    DATA SUB_BOX /14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7, &
        0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8, &
        4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0, &
        15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13, &
        15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10, &
        3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5, &
        0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15, &
        13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9, &
        10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8, &
        13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1, &
        13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7, &
        1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12, &
        7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15, &
        13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9, &
        10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4, &
        3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14, &
        2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9, &
        14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6, &
        4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14, &
        11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3, &
        12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11, &
        10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8, &
        9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6, &
        4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13, &
        4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1, &
        13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6, &
        1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2, &
        6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12, &
        13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7, &
        1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2, &
        7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8, &
        2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11 /

    DATA TAB_PERM /16,7,20,21,29,12,28,17, &
	1,15,23,26,5,18,31,10, &
	2,8,24,14,32,27,3,9,&
	19,13,30,6,22,11,4,25 /

    DATA INV_PERM /40,8,48,16,56,24,64,32, & 
	39,7,47,15,55,23,63,31, &
	38,6,46,14,54,22,62,30, &
	37,5,45,13,53,21,61,29, &
	36,4,44,12,52,20,60,28, &
	35,3,43,11,51,19,59,27, &
	34,2,42,10,50,18,58,26, &
	33,1,41,9,49,17,57,25 /

    PERMKEYS = ''  ! WE MUST CLEAN UP ALL THE MEMORY
    HALF1_SBS = '' ! TO AVOID TRASH WHEN DOING OUR 
    HALF2_SBS = '' ! OPERATIONS

    DO I = 1, 56
        TEMP_I  = 0  ! TO PREVENT LEAKS
        TEMP_CH1=''  ! WE CLEANUP MEMORY
        TEMP_I = PC1(I)-1
        TEMP_CH1 = STR2CIPH(TEMP_I:TEMP_I)
        PERMKEYS = TRIM(PERMKEYS) // TRIM(TEMP_CH1) 
    END DO 

    PRINT *, 'Compression is: ', PERMKEYS
    
    DO I = 1, 56
        TEMP_CH1 = ''
        TEMP_CH1 = PERMKEYS(I:I)
        IF( I < 28) THEN
            HALF1_SBS = TRIM(HALF1_SBS) // TRIM(TEMP_CH1)      ! WE HAVE TO DIVIDE THE STRING IN 2
        ELSE                                                   ! AND THE HALF 1 IS THE LEFT
            HALF2_SBS = TRIM(HALF2_SBS) // TRIM(TEMP_CH1)      ! AND THE HALF 2 IS THE RIGHT
        END IF                                                 ! THESE WILL BE THE BASES FOR THE KEYS
    END DO
    
    PRINT *, 'The answer for the trims are: '
    PRINT *, 'Trim1: ', HALF1_SBS
    PRINT *, 'Trim2: ', HALF2_SBS

    PRINT *, 'Segment shifting for permutation 1:'
    DO I = 1, 2
        IF(I == 1 .OR. I == 2 .OR. I == 9 .OR. I == 16) THEN
            ! SHIFT ONCE THE HALVES
            TMP_SHIFT = ''
            SHIFT_CMP = ''
            DO J = 1, 28
                SHIFT_CMP = HALF1_SBS(1:1)
                TEMP_I = J+1
                TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(HALF1_SBS(TEMP_I:TEMP_I)) 
            END DO 
            TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(SHIFT_CMP)
            PRINT *, "Left segment shifted:", TMP_SHIFT
            HALF1_SBS = TMP_SHIFT

            TMP_SHIFT = ''
            SHIFT_CMP = ''
            DO J = 1, 28
                SHIFT_CMP = HALF2_SBS(1:1)
                TEMP_I = J+1
                TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(HALF2_SBS(TEMP_I:TEMP_I)) 
            END DO 
            TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(SHIFT_CMP)
            PRINT *, "Right segment shifted:", TMP_SHIFT
            HALF2_SBS = TMP_SHIFT
        ELSE
            ! SHIFT TWICE THE HALVES
            TMP_SHIFT = ''
            SHIFT_CMP = ''
            DO J = 1, 28
                SHIFT_CMP = HALF1_SBS(1:1)
                TEMP_I = J+2
                TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(HALF1_SBS(TEMP_I:TEMP_I)) 
            END DO 
            TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(SHIFT_CMP) 
            SHIFT_CMP = ''
            SHIFT_CMP = HALF1_SBS(2:2)
            TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(SHIFT_CMP)
            PRINT *, "Left twice segment shifted:", TMP_SHIFT
            HALF1_SBS = TMP_SHIFT

            TMP_SHIFT = ''
            SHIFT_CMP = ''
            DO J = 1, 28
                SHIFT_CMP = HALF2_SBS(1:1)
                TEMP_I = J+2
                TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(HALF2_SBS(TEMP_I:TEMP_I)) 
            END DO 
            TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(SHIFT_CMP) 
            SHIFT_CMP = ''
            SHIFT_CMP = HALF2_SBS(2:2)
            TMP_SHIFT = TRIM(TMP_SHIFT) // TRIM(SHIFT_CMP)
            PRINT *, "Right twice segment shifted:", TMP_SHIFT
            HALF2_SBS = TMP_SHIFT
        ENDIF
    END DO

    SHIFT_CMP =''
    SHIFT_CMP = TRIM(HALF1_SBS) // TRIM(HALF2_SBS)
    PERMKEYS = ''
    
    DO I = 1, 48
        TEMP_I  = 0  ! TO PREVENT LEAKS
        TEMP_CH1=''  ! WE CLEANUP MEMORY
        TEMP_I = PC2(I)-1
        TEMP_CH1 = SHIFT_CMP(TEMP_I:TEMP_I)
        PERMKEYS = TRIM(PERMKEYS) // TRIM(TEMP_CH1) 
    END DO 

    PRINT *, 'Final Key generated is: ', PERMKEYS

    ENCRTEXT = STR2CIPH
    PERMKEYS = ''    ! REUSED TO AVOID MESSY CODE
    DO I = 1, 64
        TEMP_I  = 0  ! TO PREVENT LEAKS
        TEMP_CH1=''  ! WE CLEANUP MEMORY
        TEMP_I = PERM_1(I)-1
        TEMP_CH1 = ENCRTEXT(TEMP_I:TEMP_I)
        PERMKEYS = TRIM(PERMKEYS) // TRIM(TEMP_CH1) 
    END DO

    PRINT *, 'First permutation process: ', PERMKEYS
    HALF1_SBS = ''
    HALF2_SBS = ''
    DO I = 1, 64
        TEMP_CH1 = ''
        TEMP_CH1 = PERMKEYS(I:I)
        IF( I < 32) THEN
            HALF1_SBS = TRIM(HALF1_SBS) // TRIM(TEMP_CH1)      ! WE HAVE TO DIVIDE THE STRING IN 2
        ELSE                                                   ! AND THE HALF 1 IS THE LEFT
            HALF2_SBS = TRIM(HALF2_SBS) // TRIM(TEMP_CH1)      ! AND THE HALF 2 IS THE RIGHT
        END IF                                                 ! THESE WILL BE THE BASES FOR THE KEYS
    END DO

    PRINT *, 'The answer for the trims are: '
    PRINT *, 'Trim1: ', HALF1_SBS
    PRINT *, 'Trim2: ', HALF2_SBS

    DO I = 1, 16
        DO J = 1, 48
            TEMP_I = 0  ! TO PREVENT LEAKS
            TEMP_CH1=''  ! WE CLEANUP MEMORY
            TEMP_I = PERM_2(J)-1
            TEMP_CH1 = HALF2_SBS(TEMP_I:TEMP_I)
            RGHT_EXP = TRIM(HALF2_SBS) // TRIM(TEMP_CH1) 
        END DO

        DO J = 1, 48
            TEMP_I = 0
            TEMP_I = ICHAR(RGHT_EXP(J:J))
            CIPH_2XOR(J) = TEMP_I

            TEMP_I = 0
            TEMP_I = ICHAR(PERMKEYS(J:J))
            CIPH_1XOR(J) = TEMP_I
        END DO

        DO J = 1, 64
            XORED_ST = TRIM(XORED_ST) // TRIM(CHAR(XOR(CIPH_2XOR(J), CIPH_1XOR(J))))
        END DO

        
        
        DO J = 1, 64
            TEMP_I  = 0  ! TO PREVENT LEAKS
            TEMP_CH1=''  ! WE CLEANUP MEMORY
            TEMP_I = SUB_BOX(J)-1
            TEMP_CH1 = XORED_ST(TEMP_I:TEMP_I)
            FINAL = TRIM(FINAL) // TRIM(TEMP_CH1) 
        END DO

        XORED_RH = XORED_ST

        DO J = 1, 48
            TEMP_I = 0  ! TO PREVENT LEAKS
            TEMP_CH1=''  ! WE CLEANUP MEMORY
            TEMP_I = PERM_2(J)-1
            TEMP_CH1 = HALF1_SBS(TEMP_I:TEMP_I)
            RGHT_EXP = TRIM(HALF1_SBS) // TRIM(TEMP_CH1) 
        END DO

        DO J = 1, 48
            TEMP_I = 0
            TEMP_I = ICHAR(RGHT_EXP(J:J))
            CIPH_2XOR(J) = TEMP_I

            TEMP_I = 0
            TEMP_I = ICHAR(PERMKEYS(J:J))
            CIPH_1XOR(J) = TEMP_I
        END DO

        DO J = 1, 64
            XORED_ST = TRIM(XORED_ST) // TRIM(CHAR(XOR(CIPH_2XOR(J), CIPH_1XOR(J))))
        END DO

    END DO

    FINAL = TRIM(XORED_RH) // TRIM(XORED_ST)

    DO I = 1, 64
        TEMP_I  = 0  ! TO PREVENT LEAKS
        TEMP_CH1=''  ! WE CLEANUP MEMORY
        TEMP_I = INV_PERM(I)-1
        TEMP_CH1 = FINAL(TEMP_I:TEMP_I)
        ENCRYPTED = TRIM(ENCRYPTED) // TRIM(TEMP_CH1) 
    END DO

    PRINT *, 'Key generated is: ', PERMKEYS
    PRINT *,'Encryped string is: ', ENCRYPTED
    PRINT *, 'Flat text is:', STR2CIPH
END PROGRAM des