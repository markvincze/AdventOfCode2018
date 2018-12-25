r0 = 0
r1 = 0
r2 = 0
r3 = 0
r4 = 10551361
r5 = 10550400

START1:
r2 = 1

START2:
if r4 == r1 * r2
    r0 += r1

r2 += 1

if r2 <= r4
    GOTO START2

r1 += 1

if r1 <= r4
    GOTO START1

END!

