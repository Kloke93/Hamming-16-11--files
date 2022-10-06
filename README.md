# Hamming-files
Application of hamming codes (16, 11) to encode and decode files (with an added byte protocol)

User Guide

Algorithm Explanation:

Hamming codes are error-correcting codes. In this implementation called (16, 11), or
(15, 11) extended, we have 5 parity bits for every 11 bits of data, meaning that when
sent or stored after encoding we send blocks of 16 bits, having an efficiency of 68.75%,
and giving the possibility of correcting one error for every block of 11 bits or detect two
errors in that block but not correct them (the two error detection attribute is given by the
extended parity bit).
There are different Hamming codes implementations as Hamming (7, 4) that means 4
bits of data 3 for parity or Hamming (256,247) (Also called extended (255, 247)) that
has 247 data bits and 9 parity bits.
*Hamming in parameters: Hamming (T, D), T is the total number of bits after encoding,
D represent how many data bits there are and T-D is the total amount of parity bits.
To encode a block of T bits we create groups that each parity bit will control, a parity bit
is equal to 0 if the sum of all bits in that group mod 2 is equal to 0. To distribute correctly
the parity bits, we may think about them in a list where the indexes are in binary, we but
all the parity bits in all indexes that are a power of 2 (for example 0100) and the index 0
is not used, unless we are working with an extended hamming code where we use this
first bit to indicate parity of the whole block. Each group of a parity bit is conformed by
all bits that shares the 1 bit with the parity bit (For example if T = 15: parity bit 0010
controls bits: 1010, 1110, 1011, 1111, 0110, 0011 and 0111).
To decode we check each parity bit and if there was an error in its parity, we know that
the index of the flipped bit must contain the parity index in it. Checking all parity bits, we
will identify exactly where is the error. If no bit was flipped also all parity bits will be
correctly marked.

How to use:

When we start running the program, we have four options to select:
1) Hamming (16, 11) encode
2) Hamming (16, 11) decode
3) Hamming (16, 11) file encode
4) Hamming (16, 11) file decode
5) Exit
Then we are asked to enter the corresponding number with the tool we want to use, I will
proceed to show the different tool paths.
1. Asks for 11 bits to encode and prints 16 bits encoded with Hamming Codes.

2. Asks for 16, prints a reminder to avoid confusions, indicates if there was an error,
in case there was one error it prints where was the error, however if there were
two errors it just warns that they are there, finally it prints the decoded bits.
3. Asks for a file to encode and uses the encoding algorithm to encode this entire file
4. Asks for a file to decode and a new file name to create one and write the decoded
data in it. Uses the decoding algorithm to decode the first file.
5. Terminates the program.
*File options have an extra byte that I called protocol byte; this byte uses its four least
significant bits to write how many bits while writing the encoded file were real bits from
the original file in the last byte, these is because usually we are required to insert some
0’s to have 16 bits in the last byte. The 3 most significant bits are not used, but the
remaining bit is used to indicate if number of block of 16 written was even or odd, (0 if
even and 1 if odd).

![HammingFlowChart](https://user-images.githubusercontent.com/88589936/194282038-70d2a539-671a-42cb-88f3-cb805c28cb8a.png)

This flow chart describes the functioning of the program, but it is not extremely detailed.

*The file encode/decode write read and extract in the program includes another process
too complex to add to this chart.

External Links:

https://en.wikipedia.org/wiki/Hamming_code Wikipedia about Hamming codes
Videos explanation of the algorithm:
1) https://youtu.be/X8jsijhllIA
2) https://youtu.be/b3NxrZOu_CE (relation of Hamming Codes to XOR)

https://www.geeksforgeeks.org/8086-program-to-print-a-16-bit-decimal-number/
that is the source code of the PrintNum procedure, though I made some changes
