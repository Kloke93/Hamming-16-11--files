;-------------------------------------------------------;
; Description: Hamming(16, 11) code tool
;              this code is a linear error-correcting code
; Name: Tomas Dal Farra
; Date: 31/08/2022
; Assembly 8086 in DOSBox-X 0.84.1
;-------------------------------------------------------;
IDEAL
MODEL small
STACK 100h
p186
DATASEG
;-------------------------------------------------------;
	; menu messages
	open_msg db 'Select the tool you want to work with:', 10, 13
	option1 db '1 - Hamming(16, 11) encode',  10, 13
	option2 db '2 - Hamming(16, 11) decode',  10, 13
	option3 db '3 - Hamming(16, 11) file encode', 10, 13
	option4 db '4 - Hamming(16, 11) file decode', 10, 13
	option5 db '5 - Exit', 10, 13
	; initial tool selection and tool prompts 
	choose_msg db 10, 13, 'Enter your choice (number): ', '$'
	choice_num db ?
	choice_path1 db 'Enter 11 bits to encode: ', '$'
	choice_path2 db 'Enter 16 bits to decode: ', '$'
	choice_path34 db 'Enter name of the file you want to work with: ', '$'
	choice_path4 db 'Enter name for new decoded file (example: exp.txt): ', '$'
	; to store bits
	to_encode_bits db 14d dup (?)
	to_decode_bits db 19d dup (?)
	decoded_bits db 11d dup (?), '$'
	encoded_bits db 16d dup (?), '$'
	decoded_bits16 db 16d dup (?), '$'
	; error messages
	err_choice db 'Not selected one of the options!!', 10, 13, '$'
	err_not11 db 'Not entered 11 bits!!', 10, 13, '$'
	err_not16 db 'Not entered 16 bits!!', 10, 13, '$'
	err_not_bit db 'Not every character is 0 or 1', 10, 13, '$'
	err_one_bit db 'One bit error in bit number ', '$'
	err_two_bit db 'There are at least two errors! (Imposible to correct)', 10, 13, '$'
	reminder db 'Reminder: bits in position 0 and powers of 2 are parity bits', 10, 13, 10, 13, '$'
	no_err db 'No error detected', 10, 13, '$'
	err_file_not_found db 'Error - This file can not be found', 10, 13, '$'
	err_file_too_many db 'Error - Too many open files', 10, 13, '$'
	err_file_no_perm db 'Error - Unauthorized access to file', 10, 13, '$' 
	err_invalid_handle db 'Invalid Handle (not able to close file)', 10, 13, '$'
	err_creating db 'Fatal Error - not able to create new file', 10, 13, '$'
	err_read db 'Error reading bytes from file', 10, 13, '$'
	err_write db 'Error writing bytes to file', 10, 13, '$'
	; file related
	file_name db 33d dup (0)
	file_handler dw ?
	word_buffer dw ?
	write_buffer dw ?
	new_file_name db 31 dup (0)
	new_file_handler dw ?
;-------------------------------------------------------;
CODESEG
;-------------------------------------------------------;
; NewLine
; Parameters: None
; Return: None
; Description: Prints a new line to the console
;-------------------------------------------------------;
proc NewLine
	pusha
	mov ah, 02h
	mov dl, 0Ah
	int 021h
	mov dl, 0Dh
	int 021h
	popa
	ret
endp NewLine
;-------------------------------------------------------;
; Initiate
; Parameters:
;		open_msg: Strings to open menu
; Return: None
; Description: Prints the opening 'menu'
;-------------------------------------------------------;
proc Initiate
	push ax
	push dx
	call NewLine
	mov ah, 09h
	lea dx, [open_msg]		; opening messages
	int 021h
	pop dx
	pop ax
	ret
endp Initiate
;-------------------------------------------------------;
; GetOption
; Parameters: 
;		err_choice: Error message to print when not one of the options is selected
;		choose_msg: Message to prompt again the user to select an option
; Return: to choice_num variable the option chosen
; Description: gets user's choice and checks if it's a valid input
;-------------------------------------------------------;
proc GetOption
	push ax
	push dx
get_option:
	mov ah, 01h
	int 021h
	cmp al, 031h 
	jb not_valid_option
	cmp al, 035h
	ja not_valid_option
valid_option:
	call NewLine
	sub al, 030h				; to store the number and not its ascii value
	mov [choice_num], al 
	jmp option_got
not_valid_option:
	call NewLine
	mov ah, 09h
	lea dx, [err_choice]		; warning about error
	int 021h
	lea dx, [choose_msg]		; prompt to choose again 
	int 021h
	jmp get_option
option_got:
	pop dx
	pop ax
	ret
endp GetOption
;-------------------------------------------------------;
; GetData
; Parameters:
;		GetData_input: space in memory to write input
;		GetData_amount: A number either 11 or 16 
;		GetData_err_amount: Error message for wrong amount of characters
;		GetData_promt_msg: The corresponding message to prompt the user
;		[err_not_bit]: Error message when character entered it's neither 1 nor 0
; Return: data entered by the user in to_encode_bits or to_decode_bits
; Description: Gets from the user the data (bits) required
;-------------------------------------------------------;
GetData_input equ [word ptr bp + 04h]
GetData_amount equ [byte ptr bp + 06h]
GetData_err_amount equ [word ptr bp + 08h]
GetData_promt_msg equ [word ptr bp + 0Ah]
proc GetData
	push bp
	mov bp, sp
	pusha
	; print prompt message
get_data:
	call NewLine
	mov ah, 09h
	mov dx, GetData_promt_msg
	int 021h
	; Get data from user
	mov dx, GetData_input
	mov bx, dx
	mov al, GetData_amount
	mov [byte ptr bx], al
	mov ah, 0Ah
	int 021h
	call NewLine
	dec al
	cmp [byte ptr bx + 1], al		; if entered the correct amount (of characters)
	je is_1or0							; jump to next checkup
	mov ah, 09h
	mov dx, GetData_err_amount
	int 021h
	jmp get_data
is_1or0:
	xor cx, cx
	mov cl, [byte ptr bx + 1]
	mov di, 0
loop_1_0:								; loop to check if all characters are either 1 or 0
	sub [byte ptr bx + di + 2], 030h
	je valid_data
	cmp [byte ptr bx + di + 2], 1		; sub intead of cmp (first) to get in ds 0s and 1s and not 030hs and 031hs
	je valid_data
	mov ah, 09h
	lea dx, [err_not_bit]
	int 21h
	jmp get_data
valid_data:
	inc di
	loop loop_1_0
	popa
	pop bp
	ret 8
endp GetData
;-------------------------------------------------------;
; AssignData	
; Parameters: 
;	[to_encode_bits]: array with all bits inteded to encode
; Return: [encoded_bits] with all the data bits
; Description: We assign all the data bits to their corresponding
;				space in memory
;-------------------------------------------------------;
parity_index equ [word ptr bp - 02h]	; local variable to indicate next parity bit index
proc AssignData
	push bp
	mov bp, sp
	sub sp, 2
	mov parity_index, 0100b		; first parity bit index to check is 4
	pusha
	lea si, [to_encode_bits + 1]
	lea bx, [encoded_bits]
	mov di, 2					; index 0, 1 and 2 are parity bits so we skip it
	xor cx, cx
	xor dx, dx
	mov cl, [byte ptr si]
	inc si
	; first of all we assign data bits to their place
assign_data:
	inc di
;-------------------------------------------------------;
is_parity_index:
	cmp di, parity_index 
	jne not_parity_index
	shl parity_index, 1
	jmp assign_data				; if this index corresponds with a parity bit index we try with next index
;-------------------------------------------------------;
not_parity_index:
	mov dl, [si]
	mov [byte ptr bx + di], dl
	inc si
	loop assign_data
	popa
	add sp, 2
	pop bp
	ret
endp AssignData
;-------------------------------------------------------;
; AssignParity
; Parameters:
;	[encoded_bits]: with all data bits we determine the parity bits
;	parity_group_amount: how many parity groups to check
; Return: Encoded bits in [encoded_bits]
; Description: We assign all parity bits to have the full encoded bits
;-------------------------------------------------------;
parity_group equ [word ptr bp - 02h]  ; to determine which group we are checking
parity_group_amount equ [word ptr bp + 04h]
proc AssignParity
	push bp
	mov bp, sp
	sub sp, 2
	mov parity_group, 0001b           ; first parity group (then 0010b, 0100b, etc)
	pusha
	lea bx, [encoded_bits]
	mov cx, parity_group_amount
	mov di, 1
	xor ax, ax
	mov [byte ptr bx], 0			; initialize to zero first bits
initiate_parity_bits:				; initialize to zero parity bits
	mov [byte ptr bx + di], 0
	shl di, 1
	inc ax
	cmp ax, parity_group_amount
	je def_parity
	jmp initiate_parity_bits
def_parity:
	xor di, di
	push cx
	mov ax, 1
	xor cx, cx
	mov cx, parity_group_amount
	shl ax, cl
	mov cx, ax
	xor ax, ax
	dec cx
def_in_group:
		inc di
		test parity_group, di		; tests if the current index is part of the group
		je skip_bit
		xor al, [byte ptr bx + di]
skip_bit:
		loop def_in_group
	pop cx
	mov di, parity_group
	mov [byte ptr bx + di], al
	shl parity_group, 1
	loop def_parity
	xor di, di
	mov cx, parity_group_amount
	mov ax, 1
	shl ax, cl
	mov cx, ax
	xor ax, ax
	dec cx
extended_bit:
	inc di
	xor al, [byte ptr bx + di]
	loop extended_bit
	mov [byte ptr bx], al			; defining all block parity
	popa
	add sp, 2
	pop bp
	ret 2
endp AssignParity
;-------------------------------------------------------;
; PrintCode
; Parameters: 
;	code_string_offset: offset of the code bits string
;	code_string_length: length of the code bits string
; Return: None
; Description: Turns in memory every bit to its ascii number 
; (either 030h or 031h) and then prints it for the user
;-------------------------------------------------------;
code_string_offset equ [word ptr bp + 04h]
code_string_length equ [word ptr bp + 06h]
proc PrintCode
	push bp
	mov bp, sp
	pusha
	mov dx, code_string_offset
	mov bx, code_string_offset
	mov cx, code_string_length
	xor di, di
convert_to_ascii:
	add [byte ptr bx + di], 030h
	inc di
	loop convert_to_ascii
	mov ah, 09h
	int 021h
	popa
	pop bp
	ret 4
endp PrintCode
;-------------------------------------------------------;
; GetSyndrome
; Parameters: 
;	offset_to_decode: offset of array with encoded bits
; Return: In ax the syndrome 
; Description: The syndrome is a vector that indicate which parity bits
;				were incorrect, but I will treat it as an index (gets syndrome)
;-------------------------------------------------------;
offset_to_decode equ [word ptr bp + 04h]
syndrome_index equ [word ptr bp - 02h]	; to store the return value temporary
proc GetSyndrome
	push bp
	mov bp, sp
	sub sp, 2
	pusha
	xor di, di
	xor cx, cx
	xor ax, ax
	mov bx, offset_to_decode
	mov cl, [byte ptr bx + 1]
check_one_pos:					; Gets all array elements = 1 and xors their index
	cmp [byte ptr bx + di + 2], 1
	jne skip_xor
	xor ax, di
skip_xor:
	inc di
	loop check_one_pos
	mov syndrome_index, ax
	popa
	mov ax, syndrome_index
	add sp, 2
	pop bp
	ret 2
endp GetSyndrome
;-------------------------------------------------------;
; CheckParity
; Parameters:
;	offset_to_decode: offset of array with encoded bits
; Return: in dx returns 0 if the parity of the whole block
;			is correct, or returns 1 if not
; Description: Uses bit number 0 of the code to check parity 
;				this way we can detect if a two bit error (not correct)
;-------------------------------------------------------;
; offset_to_decode equ [word ptr bp + 04h]
true_parity equ [word ptr bp - 02h]	; to store the return value temporary
proc CheckParity
	push bp
	mov bp, sp
	sub sp, 2
	pusha
	xor cx, cx
	xor dx, dx
	xor si, si
	mov bx, offset_to_decode
	mov cl, [byte ptr bx + 1]
check_parity:					; checks whole block parity with bit number 0
	xor dl, [byte ptr bx + si + 2]
	inc si
	loop check_parity
	mov true_parity, dx
	popa
	mov dx, true_parity
	add sp, 2
	pop bp
	ret 2
endp CheckParity
;-------------------------------------------------------;
; DecodeData
; Parameters: 
;	[to_decode_bits]: Array of bits to decode
;	[decoded_bits]: to bit return values there
; Return: in [decoded_bits] all data bits without correction
; Description: Decodes the data without correcting the errors
;-------------------------------------------------------;
;parity_index equ [word ptr bp - 02h]	;local variable to indicate next parity bit index
proc DecodeData
	push bp
	mov bp, sp
	sub sp, 2
	mov parity_index, 0100b			; first parity bit that need to be skipped
	pusha
	lea di, [decoded_bits]
	lea bx, [to_decode_bits]
	xor cx, cx
	mov si, 3
	mov cl, [byte ptr bx + 1]
	sub cl, 3
decoding:
	cmp si, parity_index
	jne do_decode
	shl parity_index, 1
	jmp skip_decode
do_decode:
	mov al, [byte ptr bx + si + 2]
	mov [byte ptr di], al
	inc di
skip_decode:
	inc si
	loop decoding
	popa
	add sp, 2
	pop bp
	ret
endp DecodeData
;-------------------------------------------------------;
; PrintNum
; Parameters:
;	hexa_num: the number we want to print
; Return: None
; Description: Prints a number in decimal system
; https://www.geeksforgeeks.org/8086-program-to-print-a-16-bit-decimal-number/
; that is the source code, I made little changes
;-------------------------------------------------------;
hexa_num equ [word ptr bp + 04h]
proc PrintNum
	push bp
	mov bp, sp
	pusha
	mov ax, hexa_num
	cmp ax, 0
	je exit_print
	xor cx, cx
	xor dx, dx
	mov bx, 10
push_digits:
	cmp ax, 0
	je print_num
	div bx
	push dx			; pushes last digit to stack (remiander)
	inc cx
	xor dx, dx
	jmp push_digits
print_num:
	pop dx			; pop the next digit to dx to print it
	add dx, 030h	; add 30h to get the ascii representation
	mov ah, 02h
	int 21h
	loop print_num
exit_print:
	popa
	pop bp
	ret 2
endp PrintNum
;-------------------------------------------------------;
; CorrectBit
; Parameters:
;	encoded_bit_index: where is the bit in [to_decode_bits]
;	[decoded_bits]: where the bit needs to be flipped
; Return: None
; Description: Flips the bit that had an error
; Note: if one parity bit has the error we don't flip any bit
;-------------------------------------------------------;
encoded_bit_index equ [word ptr bp + 04h]
;parity_index equ [word ptr bp - 02h]	;local variable to indicate next parity bit index
proc CorrectBit
	push bp
	mov bp, sp
	sub sp, 2
	mov parity_index, 0100b
	pusha
	lea bx, [decoded_bits]
	mov cx, 3
	mov ax, encoded_bit_index
	cmp ax, 3
	jbe completed_flip
search_closest_parity:					; when we know how many parity bits we passed
	cmp ax, parity_index				; we know how many bits to substract from
	je completed_flip					; from the original index
	jb flip_bit
	inc cx
	shl parity_index, 1
	jc flip_bit							; if there exists carry from the shift
	jmp search_closest_parity
flip_bit:
	sub ax, cx
	mov di, ax
	xor [byte ptr bx + di], 1
completed_flip:
	popa
	add sp, 2
	pop bp
	ret 2
endp CorrectBit
;-------------------------------------------------------;
; CorrerctError
; Parameters:
;	[err_one_bit], [err_two_bit], [no_err] and [reminder]: messages for user
;	[decoded_bits]: not corrected decoded bits
;	syndrome_bits: as the syndrome (in description as ax)
;	parity0_bits: as an indicator of correctness in the whole block parity (in description as dx)
; Return: None (prints messages)
; Description: there are multiple error situations:
;	1) no error (ax = 0 && dx = 0)
;	2) one bit error, this can be fix (ax = x && dx = 1)
;	3) two bit error, this only can be detected (ax != 0 && dx = 0)
;	4) bigger errors, hamming codes can't handle this and we assume they don't happen
; Also: this procedure prints some string as needed
;-------------------------------------------------------;
syndrome_bits equ [word ptr bp + 04h]	; push ax
parity_bit equ [word ptr bp + 06h]		; push dx
proc CorrectError
	push bp
	mov bp, sp
	pusha
	mov ah, 09h
	lea dx, [reminder]
	int 21h
	lea bx, [decoded_bits]
	cmp parity_bit, 0
	je correct_parity
	jmp one_bit_error		; if the parity is not correct we have a one bit error case
correct_parity:
	cmp syndrome_bits, 0
	jne two_bit_error
no_error:
	lea dx, [no_err]
	int 021h
	jmp completed_correction
two_bit_error:
	lea dx, [err_two_bit]
	int 021h
	jmp completed_correction
one_bit_error:
	push syndrome_bits
	call CorrectBit					; to correct the bit that had an error-correcting
	lea dx, [err_one_bit]
	int 021h
	push syndrome_bits
	call PrintNum
	call NewLine
completed_correction:
	popa
	pop bp
	ret 4
endp CorrectError
;-------------------------------------------------------;
; CorrectErrorFile
; Description: Same as CorrectError but without printing messages
;-------------------------------------------------------;
proc CorrectErrorFile
	push bp
	mov bp, sp
	pusha
	lea bx, [decoded_bits]
	cmp parity_bit, 0
	je completed_correctionn
	jmp one_bit_error		; if the parity is not correct we have a one bit error case
	push syndrome_bits
	call CorrectBit					; to correct the bit that had an error-correcting
completed_correctionn:
	popa
	pop bp
	ret 4
endp CorrectErrorFile
;-------------------------------------------------------;
; OpenFile
; Parameters: None
; Return: file handler to [file_handler]
; Description:  Prompts the user to select a file to encode
;				and opens it unless an error occurs 
;-------------------------------------------------------;
proc OpenFile
	pusha
open_file:
	xor di, di
	mov cx, 33d
	lea bx, [file_name]
initialize_file_name:
	mov [byte ptr bx + di], 0
	inc di
	loop initialize_file_name
	xor cx, cx
	xor di, di
	mov ah, 09h
	lea dx, [choice_path34]
	int 021h						; print prompt for file name
	mov ah, 0Ah
	lea dx, [file_name]
	mov bx, dx
	mov [byte ptr bx], 31d			; maximum numbers of characters for file name including enter
	int 021h						; get file name
	call NewLine
	mov cl, [byte ptr bx + 1]
	mov si, cx
	mov [byte ptr bx + si + 2], 0	; converting enter code to 0
	mov ah, 03Dh
	mov al, 0
	lea dx, [file_name + 2]
	int 021h						; open file to read only
	jnc	functional_file
	mov ah, 09h
	cmp ax, 15
	je error_too_many
	cmp ax, 12
	je error_unauthorized
error_not_found:					; ax = 2
	lea dx, [err_file_not_found]
	int 021h
	jmp open_file
error_too_many:
	lea dx, [err_file_too_many]
	int 021h
	jmp open_file
error_unauthorized:
	lea dx, [err_file_no_perm]
	int 021h
	jmp open_file
functional_file:
	mov [file_handler], ax			; saving file handler
	popa
	ret
endp OpenFile
;-------------------------------------------------------;
; CloseFile
; Parameters:
;		fhandler: handler of the file to close
; Return: None 
; Description: Closes an specific file
;-------------------------------------------------------;
fhandler equ [word ptr bp + 04h]
proc CloseFile
	push bp
	mov bp, sp
	pusha
	mov ah, 03Eh
	mov bx, fhandler
	int 021h
	jnc closed
invalid_handle:
	mov ah, 09h
	lea dx, [err_invalid_handle]
	int 021h
closed:
	popa
	pop bp
	ret 2
endp CloseFile
;-------------------------------------------------------;
; NewBinaryFile
; Parameters:
;		[file_name]: name of the original file
;		[new_file_name]: same name but with .bin format
; Return: None
; Description: Creates a new binary file to encode the
;				selected file
;-------------------------------------------------------;
proc NewBinaryFile
	pusha
	xor ax, ax
	xor si, si
	xor di, di
	mov cx, 31d
	lea bx, [new_file_name]
initialize_new_Bfile_name:
	mov [byte ptr bx + di], 0
	inc di
	loop initialize_new_Bfile_name
	xor cx, cx
	xor di, di
	lea bx, [file_name]
	lea di, [new_file_name]
	mov cl, [byte ptr bx + 1]
assign_name:
	mov al, [byte ptr bx + si + 2]
	cmp [byte ptr bx + si + 2], 02Eh
	jne copying_name
	mov dx, si							; saves where is the file format
copying_name:
	mov [di], al
	inc di
	inc si
	loop assign_name					; copying name to new file
	lea bx, [new_file_name]
	mov di, dx
	mov [byte ptr bx + di + 1], 'b'
	mov [byte ptr bx + di + 2], 'i'
	mov [byte ptr bx + di + 3], 'n'
	mov [byte ptr bx + di + 4], 0		; changes file format to .bin
	mov cx, 0
	mov ah, 03Ch
	lea dx, [new_file_name]
	int 021h							; creating new binary file
	jnc created_bin_file
	lea dx, [err_creating]
	mov sp, 100h						; get sp to point to 100h
	jmp exit							; fatal error occurred
created_bin_file:
	mov [new_file_handler], ax			; saving new file handler
	popa
	ret
endp NewBinaryFile
;-------------------------------------------------------;
; EncodeFile
; Parameters:  All parameters that inner procedures require 
; Return: Encoded binary file with all the code needed 
; Description: Encodes all the file to hamming code
; Also: Last bit according to protocol 
;-------------------------------------------------------;
bytes_to_work equ [word ptr bp - 02h]	; how many bytes we are writing or reading
bytes_even_odd equ [word ptr bp - 04h]  ; if amount of bytes written is even or odd (0 = even, 1 = odd)
proc EncodeFile
	push bp
	mov bp, sp
	sub sp, 4
	mov bytes_to_work, 2
	mov bytes_even_odd, 0
	pusha
	xor dx, dx
	xor di, di
	lea bx, [to_encode_bits + 1]
	mov [byte ptr bx], 0Bh
	
read_from_file:
	push offset word_buffer				; buffer_offset
	push bytes_to_work					; bytes_amount
	push [file_handler]					; fhandler
	call ReadFile
	push dx
	mov dx, [word_buffer]
	mov [byte ptr word_buffer], dh
	mov [byte ptr word_buffer + 1], dl	; change word_buffer to little endian
	pop dx
	cmp ax, 0							; check for full read
	je zero_left
	add dx, 010h
	cmp ax, 1
	jne continue_encode
	xor bytes_even_odd, 010h
	sub dx, 08h
continue_encode:
	mov cx, 0Bh
	sub cx, di
extract_encode:
	push offset word_buffer
	call ExtractBits
	
ready_to_encode:			; Checks if bits are ready to encode and write
	cmp di, 0Bh
	je encode_bytes
check_buffer_size:			; decides what to do according to how many bits there are in the buffer
	cmp dx, 0
	je read_from_file
	mov cx, dx
	cmp dx, 0Bh
	jb to_extract_encode
	mov cx, 0Bh
to_extract_encode:
	jmp extract_encode
	
encode_bytes:
	mov di, 0
	call AssignData
	push 4								; amount of parity bits
	call AssignParity
	push 16								; bits_amount
	push offset write_buffer			; bytes_storage
	push offset encoded_bits			; bits_offset
	call BitsToBytes
write_to_file:
	push offset write_buffer			; buffer_offset
	push bytes_to_work					; bytes_amount
	push [new_file_handler]				; fhandler
	call WriteFile
	cmp ax, 0
	je file_encoded						; if finished reading as jump to end
	jmp check_buffer_size				; return to check

zero_left:								; ax = 0
	xor si, si
	push si
	cmp di, 0
	je file_encoded
	pop si
	push di
	mov cx, 0Bh
	sub cx, di
make_zero_rest:
	mov [byte ptr bx + di + 1], 0
	inc di
	loop make_zero_rest
	pop di
	push ax
	xor si, si
	mov cx, di
convert_to_eleven:
	mov al, [byte ptr bx + di]
	mov [byte ptr bx + di], 0
	push bx
	sub bx, si
	mov [byte ptr bx + 0Bh], al
	pop bx
	dec di
	inc si
	loop convert_to_eleven				; to convert these bits to the correct format
	pop ax
	push si
	jmp encode_bytes
file_encoded:
	pop si
	add si, bytes_even_odd
	mov [word ptr write_buffer], si
	push offset write_buffer			; buffer_offset
	push 1								; bytes_amount
	push [new_file_handler]				; fhandler
	call WriteFile						; write how many bits are not filled
	popa
	add sp, 4
	pop bp
	ret
endp EncodeFile
;-------------------------------------------------------;
; ReadFile
; Parameters:
;	fhandler: handler of the file we want to read from
;	bytes_amount: how many bytes we want to read
;	buffer_offset: offset of the buffer to store the bytes read
; Return: In buffer_offset bytes read and in ax number of bytes read
; Description: Reads an amount of bytes from a file and saves 
;				the bytes
;-------------------------------------------------------;
; fhandler equ [word ptr bp + 04h]
bytes_amount equ [word ptr bp + 06h]
buffer_offset equ [word ptr bp + 08h]
pass_code equ [word ptr bp - 02h]		; pass to ax a value
proc ReadFile
	push bp
	mov bp, sp
	sub sp, 2
	pusha
	mov ah, 03Fh
	mov bx, fhandler
	mov dx, buffer_offset
	mov cx, bytes_amount
	int 021h						; read bytes
	jnc correct_read				; check if correct
	mov ah, 09h
	lea dx, [err_read]
	int 021h
correct_read:
	mov pass_code, ax
	popa
	mov ax, pass_code
	add sp, 2
	pop bp
	ret 6
endp ReadFile
;-------------------------------------------------------;
; WriteFile
; Parameters: 
;	fhandler: handler of the file we want to write to
;	bytes_amount: how many bytes we want to write
;	buffer_offset: offset of the buffer with the bytes to write
; Return: None
; Description: Writes some bytes to an specific file
;-------------------------------------------------------;
; fhandler equ [word ptr bp + 04h]
; bytes_amount equ [word ptr bp + 06h]
; buffer_offset equ [word ptr bp + 08h]
proc WriteFile
	push bp
	mov bp, sp
	pusha
	mov ah, 040h
	mov bx, fhandler
	mov dx, buffer_offset
	mov cx, bytes_amount
	int 021h
	jnc correct_write				; check if correct
	mov ah, 09h
	lea dx, [err_write]
	int 021h
correct_write:
	popa
	pop bp
	ret 6
endp WriteFile
;-------------------------------------------------------;
; BitsToBytes
; Parameters: 
;	bits_offset: offset of array with bits
;	bytes_storage: where to store the bits as a number
;	bits_amount: length of the array (how many bits)
; Return: In bytes_storage bits written in an array represented as a number
; Description: Uses an array that each element represents a bit being the first
;				element the MSB and stores this as a number
; Also:			(limit 16 bits)
;-------------------------------------------------------;
bits_offset equ [word ptr bp + 04h]
bytes_storage equ [word ptr bp + 06h]
bits_amount equ [word ptr bp + 08h]
proc BitsToBytes
	push bp
	mov bp, sp
	pusha
	xor cx, cx
	xor si, si
	xor dx, dx
	mov di, bytes_storage
	mov bx, bits_offset
	mov cx, bits_amount
	dec cx						; later compensated
	cmp cx, 15
	jbe in_domain				; is the array length within the procedure boundaries
	mov cx, 15
in_domain:
	add dl, [byte ptr bx + si]
	shl dx, 1
	inc si
	loop in_domain
	add dl, [byte ptr bx + si]	; one more repetion to skip the shifting and compensates sub
	mov bytes_storage, dx
	mov [byte ptr di], dh
	mov [byte ptr di + 1], dl
	popa
	pop bp
	ret 6
endp BitsToBytes
;-------------------------------------------------------;
; DecodeFile
; Parameters: All parameters that inner procedures require 
; Return: Decoded binary file with all the code needed 
; Description: Decodes all the file to hamming code
;-------------------------------------------------------;
; bytes_to_work equ [word ptr bp - 02h]	; how many bytes we are writing or reading
byte_remains equ [word ptr bp - 04h]	; if there is one more byte to decode
is_even_odd equ [word ptr bp - 06h]		; if there is an even or odd number of bytes
proc DecodeFile
	push bp
	mov bp, sp
	sub sp, 6
	mov bytes_to_work, 2
	mov byte_remains, 0
	mov is_even_odd, 010h
	pusha
	lea bx, [to_decode_bits + 1]
	mov [byte ptr bx], 10h
	xor dx, dx
	xor di, di
	xor si, si
	
read_to_decode:
	push offset word_buffer				; buffer_offset
	push bytes_to_work					; bytes_amount
	push [file_handler]					; fhandler
	call ReadFile
	push dx
	mov dx, [word_buffer]
	mov [byte ptr word_buffer], dh
	mov [byte ptr word_buffer + 1], dl	; change word_buffer to little endian
	pop dx
	cmp ax, 1
	je help_protocol
	add dx, 010h						; how many bits in buffer
	cmp di, 010h
	je convert_to_bytes

extract_decode:
	push di
	xor di, di
	mov cx, 10h
	push offset word_buffer
	call ExtractBits
	pop di

decode_bytes:
	push ax
	push dx
	xor ax, ax
	xor dx, dx
	push offset to_decode_bits
	call GetSyndrome
	push offset to_decode_bits
	call CheckParity
	call DecodeData
	push dx
	push ax
	call CorrectErrorFile
	pop dx
	pop ax
	
move_decoded:							; moves from decoded_bits to decoded_bits16
	push ax
	push bx
	lea si, [decoded_bits]
	lea bx, [decoded_bits16]
	mov cx, 0Bh
move_loop:
	cmp di, 10h
	je help_jump1
	mov al ,[byte ptr si]
	mov [byte ptr bx + di], al
	inc di
	inc si
	loop move_loop
help_jump1:
	pop bx
	pop ax
	push dx								; to avoid error in stack
help_jump2:
	pop dx
	cmp dx, 0
	jne extract_decode
	jmp read_to_decode
	
help_protocol:
	jmp protocol_byte

convert_to_bytes:
	push 010h
	push offset write_buffer			; bytes_storage
	push offset decoded_bits16			; bits_offset
	call BitsToBytes
write_decoded:
	xor di, di
	push offset write_buffer			; buffer_offset
	push bytes_to_work					; bytes_amount
	push [new_file_handler]				; fhandler
	call WriteFile
	cmp ax, 1
	jbe check_if_remains
	push dx
	lea dx, [decoded_bits + 0Bh]
	cmp si, dx
	je help_jump2						; je read_to_decode
	pop dx
	push ax
	push bx
	lea bx, [decoded_bits16]
	mov cx, 0Bh
	add cx, offset decoded_bits
	sub cx, si
	jmp move_loop
	
protocol_byte:
	push ax
	xor cx, cx
	mov cl, [byte ptr word_buffer + 1]
	and is_even_odd, cx
	and cx, 010Fh
	cmp cl, 0
	je fixed_protocol
	lea si, [decoded_bits + 0Ah]
	lea di, [decoded_bits16 + 0Fh]
	cmp is_even_odd, 0
	je move_protocol
	sub di, 08h
	mov bytes_to_work, 1
move_protocol:							; change bits according to protocol
	mov al, [byte ptr si]
	mov [byte ptr di], al
	dec si
	dec di
	loop move_protocol
	jmp fixed_protocol1
fixed_protocol:
	mov byte_remains, 1
fixed_protocol1:
	pop ax
	jmp convert_to_bytes
check_if_remains:						; there may be a byte not written
	cmp byte_remains, 1
	jne file_decoded
	push 08h
	push offset write_buffer			; bytes_storage
	push si								; bits_offset
	call BitsToBytes
	push offset write_buffer + 1		; buffer_offset
	push 1								; bytes_amount
	push [new_file_handler]				; fhandler
	call WriteFile
file_decoded:
	popa
	add sp, 6
	pop bp
	ret
endp DecodeFile
;-------------------------------------------------------;
; NewFile
; Parameters: None
; Return: in [new_file_name] the name of the new file and
;			in [new_file_handler] the new file handler
; Description: Creates a new file according to what the 
;				user needs
;-------------------------------------------------------;
proc NewFile
	pusha
	xor di, di
	mov cx, 31d
	lea bx, [new_file_name]
initialize_new_file_name:
	mov [byte ptr bx + di], 0
	inc di
	loop initialize_new_file_name
	xor cx, cx
	xor di, di
	mov ah, 09h
	lea dx, [choice_path4]
	int 021h						; print prompt for file name
	mov ah, 0Ah
	lea dx, [new_file_name]
	mov bx, dx
	mov [byte ptr bx], 29d			; maximum numbers of characters for file name including enter
	int 021h						; get file name
	mov cl, [byte ptr bx + 1]
	mov si, cx
	mov [byte ptr bx + si + 2], 0	; converting enter code to 0
	mov cx, 0
	mov ah, 03Ch
	lea dx, [new_file_name + 2]
	int 021h							; creating new file
	jnc created_file
	lea dx, [err_creating]
	mov sp, 100h						; get sp to point to 100h
	jmp exit							; fatal error occurred
created_file:
	mov [new_file_handler], ax			; saving new file handler
	popa
	ret
endp NewFile
;-------------------------------------------------------;
; ExtractBits
; Parameters: 
;	cx: how many bits we want to extract to buffer
;	extract_buffer_offset: offset to array where we take the bits from
;	bx: indicates where to store bits (to_encode_bits or to_decode_bits)
;	dx: how many bits are available to extract
; Return: All bits extracted in an array
; Description: Extracts from a byte a number of bits and stores them
;-------------------------------------------------------;
extract_buffer_offset equ [word ptr bp + 04h]
proc ExtractBits
	push bp
	mov bp, sp
	push si
	xor si, si
	mov si, extract_buffer_offset
extract_bits:
	cmp dx, 0
	je finish_extract_bits
	shl [word ptr si], 1
	mov [byte ptr bx + di + 1], 0		; if not 0 then we change it
	jnc next_bit
	mov [byte ptr bx + di + 1], 1
next_bit:
	inc di
	dec dx
	loop extract_bits
finish_extract_bits:
	pop si
	pop bp
	ret 2
endp ExtractBits
;-------------------------------------------------------;
start:
	mov ax, @data
	mov ds, ax
;-------------------------------------------------------;
display_menu:
	call Initiate
	call GetOption
	cmp [choice_num], 1
	je path_encode
	cmp [choice_num], 2
	je path_decode
	cmp [choice_num], 3
	je path_encode_file
	cmp [choice_num], 4
	je path_decode_file
	jmp paths_end
path_encode:						;Encoding path
	push offset choice_path1
	push offset err_not11
	push 0Ch					;We expect to get 12 charactets including enter
	push offset to_encode_bits
	call GetData
	call AssignData
	push 4
	call AssignParity
	push 16
	push offset encoded_bits
	call NewLine
	call PrintCode
	call NewLine
	jmp display_menu
path_decode:					;Decoding path
	push offset choice_path2
	push offset err_not16
	push 011h					;We expect to get 17 charactets including enter
	push offset to_decode_bits
	call GetData
	push offset to_decode_bits
	call GetSyndrome
	push offset to_decode_bits
	call CheckParity
	call DecodeData
	push dx
	push ax
	call NewLine
	call CorrectError
	call NewLine
	push 11
	push offset decoded_bits
	call PrintCode
	call NewLine
	jmp display_menu
path_encode_file:
	call OpenFile
	call NewBinaryFile
	call EncodeFile
	push [file_handler]
	call CloseFile
	push [new_file_handler]
	call CloseFile
	jmp display_menu
path_decode_file:
	call OpenFile
	call NewFile
	call NewLine
	call DecodeFile
	push [file_handler]
	call CloseFile
	push [new_file_handler]
	call CloseFile
	jmp display_menu
paths_end:
;-------------------------------------------------------;
exit:
	mov ax, 4c00h
	int 21h
END start
