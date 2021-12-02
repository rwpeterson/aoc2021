/forward/ {
    x += $2;
    x2 += $2;
    z2 += aim * $2;
}
/up/ {
    z -= $2;
    aim -= $2;
}
/down/ {
    z += $2;
    aim += $2;
}
END {
    print "x * z =", x * z;
    print "x2 * z2 =", x2 * z2;
}
