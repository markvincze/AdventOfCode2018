r3 = 0;
r5 = r3 | 65536; // LABEL 6
r3 = 15028787;
r2 = r5 & 255; // LABEL 8

r3 = r3 + r2;
r3 = r3 & 16777215;
r3 = r3 * 65899;
r3 = r3 & 16777215;

if r5 < 256 {
    JUMP 28;
}
else {
    r2 = 0; // LABEL 17
    r4 = r2 + 1;
    r4 = r4 * 256;

    if r4 > r5 {
        JUMP 26;
    }
    else {
        r2 += 1;
        JUMP 18;
    }
}

r5 = r2; // LABEL 26
JUMP 8;

if r3 = r0 {  // LABEL 28
    HALT;
} else {
    JUMP 6;
}